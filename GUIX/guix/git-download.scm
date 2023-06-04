;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:autoload   (git bindings)   (libgit2-init!)
  #:autoload   (git repository) (repository-open
                                 repository-close!
                                 repository-discover
                                 repository-head
                                 repository-working-directory)
  #:autoload   (git submodule)  (repository-submodules
                                 submodule-lookup
                                 submodule-path)
  #:autoload   (git commit)     (commit-lookup commit-tree)
  #:autoload   (git reference)  (reference-target)
  #:autoload   (git tree)       (tree-list)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (git-reference
            git-reference?
            git-reference-url
            git-reference-commit
            git-reference-recursive?

            git-fetch
            git-version
            git-file-name
            git-predicate))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific commit from a Git repository.
;;; The repository URL and commit hash are specified with a <git-reference>
;;; object.
;;;
;;; Code:

(define-record-type* <git-reference>
  git-reference make-git-reference
  git-reference?
  (url        git-reference-url)
  (commit     git-reference-commit)
  (recursive? git-reference-recursive?   ; whether to recurse into sub-modules
              (default #f)))

(define (git-package)
  "Return the default Git package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'git-minimal)))

(define* (git-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (git (git-package)))
  "Return a fixed-output derivation that fetches REF, a <git-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (define inputs
    `(("git" ,git)

      ;; When doing 'git clone --recursive', we need sed, grep, etc. to be
      ;; available so that 'git submodule' works.
      ,@(if (git-reference-recursive? ref)
            (standard-packages)

            ;; The 'swh-download' procedure requires tar and gzip.
            `(("gzip" ,(module-ref (resolve-interface '(gnu packages compression))
                                   'gzip))
              ("tar" ,(module-ref (resolve-interface '(gnu packages base))
                                  'tar))))))

  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define guile-lzlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-lzlib))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'guile-gnutls))

  (define glibc-locales
    ;; Note: pick the '-final' variant to avoid circular dependency on
    ;; i586-gnu, where 'glibc-utf8-locales' indirectly depends on Git.
    (module-ref (resolve-interface '(gnu packages commencement))
                'glibc-utf8-locales-final))

  (define modules
    (delete '(guix config)
            (source-module-closure '((guix build git)
                                     (guix build utils)
                                     (guix build download-nar)
                                     (guix swh)))))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json gnutls   ;for (guix swh)
                             guile-lzlib)
        #~(begin
            (use-modules (guix build git)
                         (guix build utils)
                         (guix build download-nar)
                         (guix swh)
                         (ice-9 match))

            (define recursive?
              (call-with-input-string (getenv "git recursive?") read))

            ;; Let Guile interpret file names as UTF-8, otherwise
            ;; 'delete-file-recursively' might fail to delete all of
            ;; '.git'--see <https://issues.guix.gnu.org/54893>.
            (setenv "GUIX_LOCPATH"
                    #+(file-append glibc-locales "/lib/locale"))
            (setlocale LC_ALL "en_US.utf8")

            ;; The 'git submodule' commands expects Coreutils, sed,
            ;; grep, etc. to be in $PATH.
            (set-path-environment-variable "PATH" '("bin")
                                           (match '#+inputs
                                             (((names dirs outputs ...) ...)
                                              dirs)))

            (setvbuf (current-output-port) 'line)
            (setvbuf (current-error-port) 'line)

            (or (git-fetch (getenv "git url") (getenv "git commit")
                           #$output
                           #:recursive? recursive?
                           #:git-command "git")
                (download-nar #$output)

                ;; As a last resort, attempt to download from Software Heritage.
                ;; Disable X.509 certificate verification to avoid depending
                ;; on nss-certs--we're authenticating the checkout anyway.
                ;; XXX: Currently recursive checkouts are not supported.
                (and (not recursive?)
                     (parameterize ((%verify-swh-certificate? #f))
                       (format (current-error-port)
                               "Trying to download from Software Heritage...~%")

                       (swh-download (getenv "git url") (getenv "git commit")
                                     #$output)
                       (when (file-exists?
                              (string-append #$output "/.gitattributes"))
                         ;; Perform CR/LF conversion and other changes
                         ;; specificied by '.gitattributes'.
                         (invoke "git" "-C" #$output "init")
                         (invoke "git" "-C" #$output "config" "--local"
                                 "user.email" "you@example.org")
                         (invoke "git" "-C" #$output "config" "--local"
                                 "user.name" "Your Name")
                         (invoke "git" "-C" #$output "add" ".")
                         (invoke "git" "-C" #$output "commit" "-am" "init")
                         (invoke "git" "-C" #$output "read-tree" "--empty")
                         (invoke "git" "-C" #$output "reset" "--hard")
                         (delete-file-recursively
                          (string-append #$output "/.git"))))))))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "git-checkout") build

                      ;; Use environment variables and a fixed script name so
                      ;; there's only one script in store for all the
                      ;; downloads.
                      #:script-name "git-download"
                      #:env-vars
                      `(("git url" . ,(git-reference-url ref))
                        ("git commit" . ,(git-reference-commit ref))
                        ("git recursive?" . ,(object->string
                                              (git-reference-recursive? ref))))
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")

                      #:system system
                      #:local-build? #t           ;don't offload repo cloning
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define (git-version version revision commit)
  "Return the version string for packages using git-download."
  ;; git-version is almost exclusively executed while modules are being loaded.
  ;; This makes any errors hide their backtrace. Avoid the mysterious error
  ;; "Value out of range 0 to N: 7" when the commit ID is too short, which
  ;; can happen, for example, when the user swapped the revision and commit
  ;; arguments by mistake.
  (when (< (string-length commit) 7)
    (raise
      (condition
        (&message (message "git-version: commit ID unexpectedly short")))))
  (string-append version "-" revision "." (string-take commit 7)))

(define (git-file-name name version)
  "Return the file-name for packages using git-download."
  (string-append name "-" version "-checkout"))


;;;
;;; 'git-predicate'.
;;;

(define* (git-file-list directory #:optional prefix #:key (recursive? #t))
  "Return the list of files checked in in the Git repository at DIRECTORY.
The result is similar to that of the 'git ls-files' command, except that it
also includes directories, not just regular files.

When RECURSIVE? is true, also list files in submodules, similar to the 'git
ls-files --recurse-submodules' command.  This is enabled by default.

The returned file names are relative to DIRECTORY, which is not necessarily
the root of the checkout.  If a PREFIX is provided, it is prepended to each
file name."
  (let* (;; 'repository-working-directory' always returns a trailing "/",
         ;; so add one here to ease the comparisons below.
         (directory  (string-append (canonicalize-path directory) "/"))
         (dot-git    (repository-discover directory))
         (repository (repository-open dot-git))
         (workdir    (repository-working-directory repository))
         (head       (repository-head repository))
         (oid        (reference-target head))
         (commit     (commit-lookup repository oid))
         (tree       (commit-tree commit))
         (files      (tree-list tree))
         (submodules (if recursive?
                         (map (lambda (name)
                                (submodule-path
                                 (submodule-lookup repository name)))
                              (repository-submodules repository))
                         '()))
         (relative      (and (not (string=? workdir directory))
                             (string-drop directory (string-length workdir))))
         (included?     (lambda (path)
                          (or (not relative)
                              (string-prefix? relative path))))
         (make-relative (lambda (path)
                          (if relative
                              (string-drop path (string-length relative))
                              path)))
         (add-prefix    (lambda (path)
                          (if prefix
                              (string-append prefix "/" path)
                              path)))
         (rectify       (compose add-prefix make-relative)))
    (repository-close! repository)
    (append
     (if (or relative prefix)
         (filter-map (lambda (file)
                       (and (included? file)
                            (rectify file)))
                     files)
         files)
     (append-map (lambda (submodule)
                   (if (included? submodule)
                       (git-file-list
                        (string-append workdir submodule)
                        (rectify submodule))
                       '()))
                 submodules))))

(define* (git-predicate directory #:key (recursive? #t))
  "Return a predicate that returns true if a file is part of the Git checkout
living at DIRECTORY.  If DIRECTORY does not lie within a Git checkout, and
upon Git errors, return #f instead of a predicate.

When RECURSIVE? is true, the predicate also returns true if a file is part of
any Git submodule under DIRECTORY.  This is enabled by default.

The returned predicate takes two arguments FILE and STAT where FILE is an
absolute file name and STAT is the result of 'lstat'."
  (libgit2-init!)
  (catch 'git-error
    (lambda ()
      (let* ((files  (git-file-list directory #:recursive? recursive?))
             (inodes (fold (lambda (file result)
                             (let* ((file (string-append directory "/" file))
                                    (stat (false-if-exception (lstat file))))
                               ;; Ignore FILE if it has been deleted.
                               (if stat
                                   (vhash-consv (stat:ino stat) (stat:dev stat)
                                                result)
                                   result)))
                           vlist-null
                           files)))
        (lambda (file stat)
          ;; Comparing file names is always tricky business so we rely on inode
          ;; numbers instead.
          (match (vhash-assv (stat:ino stat) inodes)
            ((_ . dev) (= dev (stat:dev stat)))
            (#f        #f)))))
    (const #f)))

;;; git-download.scm ends here
