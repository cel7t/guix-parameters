;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (guix android-repo-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (android-repo-reference
            android-repo-reference?
            android-repo-reference-manifest-url
            android-repo-reference-revision

            android-repo-fetch
            android-repo-version
            android-repo-file-name))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific commit from an Android repo
;;; repository.
;;; The repository's manifest (URL and revision) can be specified with a
;;; <android-repo-reference> object.
;;;
;;; Code:

(define-record-type* <android-repo-reference>
  android-repo-reference make-android-repo-reference
  android-repo-reference?
  (manifest-url        android-repo-reference-manifest-url)
  (manifest-revision   android-repo-reference-manifest-revision))

(define (git-repo-package)
  "Return the default git-repo package."
  (let ((distro (resolve-interface '(gnu packages android))))
    (module-ref distro 'git-repo)))

(define* (android-repo-fetch ref hash-algo hash
                             #:optional name
                             #:key (system (%current-system))
                             (guile (default-guile))
                             (git-repo (git-repo-package)))
  "Return a fixed-output derivation that fetches REF, an
<android-repo-reference> object.  The output is expected to have recursive
hash HASH of type HASH-ALGO (a symbol).  Use NAME as the file name, or a
generic name if unset."
  ;; TODO: Remove.
  (define inputs
    (standard-packages))

  (define zlib
    (module-ref (resolve-interface '(gnu packages compression)) 'zlib))

  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define guile-lzlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-lzlib))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'guile-gnutls))

  (define config.scm
    (scheme-file "config.scm"
                 #~(begin
                     (define-module (guix config)
                       #:export (%libz))

                     (define %libz
                       #+(file-append zlib "/lib/libz")))))

  (define modules
    (cons `((guix config) => ,config.scm)
          (delete '(guix config)
                  (source-module-closure '((guix build android-repo)
                                           (guix build utils)
                                           (guix build download-nar))))))

  (define build
    (with-imported-modules modules
      (with-extensions (list gnutls guile-json ;for (guix swh)
                             guile-lzlib)
        #~(begin
            (use-modules (guix build android-repo)
                         (guix build utils)
                         (guix build download-nar)
                         (ice-9 match))

            ;; The 'git submodule' commands expects Coreutils, sed,
            ;; grep, etc. to be in $PATH.
            (set-path-environment-variable "PATH" '("bin")
                                           (match '#+inputs
                                             (((names dirs outputs ...) ...)
                                              dirs)))

            (setvbuf (current-output-port) 'line)
            (setvbuf (current-error-port) 'line)

            (or (android-repo-fetch (getenv "android-repo manifest-url")
                                    (getenv "android-repo manifest-revision")
                                    #$output
                                    #:git-repo-command
                                    (string-append #+git-repo "/bin/repo"))
                (download-nar #$output))))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "android-repo-checkout") build

                      ;; Use environment variables and a fixed script name so
                      ;; there's only one script in store for all the
                      ;; downloads.
                      #:script-name "android-repo-download"
                      #:env-vars
                      `(("android-repo manifest-url" .
                         ,(android-repo-reference-manifest-url ref))
                        ("android-repo manifest-revision" .
                         ,(android-repo-reference-manifest-revision ref)))
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t           ;don't offload repo cloning
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define (android-repo-version version revision)
  "Return the version string for packages using android-repo-download."
  (string-append version "-" (string-join (string-split revision #\/) "_")))

(define (android-repo-file-name name version)
  "Return the file-name for packages using android-repo-download."
  (string-append name "-" version "-checkout"))


