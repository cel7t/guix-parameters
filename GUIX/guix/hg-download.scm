;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (hg-reference
            hg-reference?
            hg-reference-url
            hg-reference-changeset
            hg-predicate
            hg-fetch
            hg-version
            hg-file-name))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific changeset from a Mercurial
;;; repository.  The repository URL and changeset ID are specified with a
;;; <hg-reference> object.
;;;
;;; Code:

(define-record-type* <hg-reference>
  hg-reference make-hg-reference
  hg-reference?
  (url        hg-reference-url)
  (changeset  hg-reference-changeset))

(define (hg-package)
  "Return the default Mercurial package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'mercurial)))

(define* (hg-fetch ref hash-algo hash
                   #:optional name
                   #:key (system (%current-system)) (guile (default-guile))
                   (hg (hg-package)))
  "Return a fixed-output derivation that fetches REF, a <hg-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (define inputs
    ;; The 'swh-download' procedure requires tar and gzip.
    `(("gzip" ,(module-ref (resolve-interface '(gnu packages compression))
                           'gzip))
      ("tar" ,(module-ref (resolve-interface '(gnu packages base))
                          'tar))))

  (define guile-lzlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-lzlib))

  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'guile-gnutls))

  (define modules
    (delete '(guix config)
            (source-module-closure '((guix build hg)
                                     (guix build download-nar)
                                     (guix swh)))))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json gnutls ;for (guix swh)
                             guile-lzlib)
        #~(begin
            (use-modules (guix build hg)
                         (guix build utils) ;for `set-path-environment-variable'
                         (guix build download-nar)
                         (guix swh)
                         (ice-9 match))

            (set-path-environment-variable "PATH" '("bin")
                                           (match '#+inputs
                                             (((names dirs outputs ...) ...)
                                              dirs)))

            (setvbuf (current-output-port) 'line)
            (setvbuf (current-error-port) 'line)

            (or (hg-fetch '#$(hg-reference-url ref)
                          '#$(hg-reference-changeset ref)
                          #$output
                          #:hg-command (string-append #+hg "/bin/hg"))
                (download-nar #$output)
                ;; As a last resort, attempt to download from Software Heritage.
                ;; Disable X.509 certificate verification to avoid depending
                ;; on nss-certs--we're authenticating the checkout anyway.
                (parameterize ((%verify-swh-certificate? #f))
                  (format (current-error-port)
                          "Trying to download from Software Heritage...~%")
                  (swh-download #$(hg-reference-url ref)
                                #$(hg-reference-changeset ref)
                                #$output)))))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "hg-checkout") build
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t           ;don't offload repo cloning
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define (hg-version version revision changeset)
  "Return the version string for packages using hg-download."
  ;; hg-version is almost exclusively executed while modules are being loaded.
  ;; This makes any errors hide their backtrace. Avoid the mysterious error
  ;; "Value out of range 0 to N: 7" when the commit ID is too short, which
  ;; can happen, for example, when the user swapped the revision and commit
  ;; arguments by mistake.
  (when (< (string-length changeset) 7)
    (raise
     (condition
      (&message (message "hg-version: changeset ID unexpectedly short")))))
  (string-append version "-" revision "." (string-take changeset 7)))

(define (hg-file-name name version)
  "Return the file-name for packages using hg-download."
  (string-append name "-" version "-checkout"))

(define (hg-file-list directory)
  "Evaluates to a list of files contained in the repository at path
  @var{directory}"
  (let* ((port (open-input-pipe (format #f "hg files --repository ~s" directory)))
         (files (let loop ((files '()))
                  (let ((line (read-line port)))
                    (cond
                     ((eof-object? line) files)
                     (else
                      (loop (cons line files))))))))
    (close-pipe port)
    (map canonicalize-path files)))

(define (should-select? path-list candidate)
  "Returns #t in case that @var{candidate} is a file that is part of the given
@var{path-list}."
  (let ((canon-candidate (canonicalize-path candidate)))
    (let loop ((xs path-list))
      (cond
       ((null? xs)
        ;; Directories are not part of `hg files', but `local-file' will not
        ;; recurse if we don't return #t for directories.
        (equal? (array-ref (lstat candidate) 13) 'directory))
       ((string-contains candidate (car xs)) #t)
       (else (loop (cdr xs)))))))

(define (hg-predicate directory)
  "This procedure evaluates to a predicate that reports back whether a given
@var{file} - @var{stat} combination is part of the files tracked by
Mercurial."
  (let ((files (hg-file-list directory)))
    (lambda (file stat)
      (should-select? files file))))

;;; hg-download.scm ends here
