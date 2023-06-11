(define-module (DRAFTS test-packages)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table))

(define-public hello-parameterized
  (package
    (inherit hello)
    (properties
     `(,(parameter-spec
         ;; local -> optional by default
         ;; no other parameters to add to this example for now
         (local (list
                 (package-parameter
                 (name "nls!")
                 (transforms
                  (gnu-build-system . ((with-configure-flag . "hello=--disable-nls")))))
                 (package-parameter
                  (name "nls")
                  (default? #t)
                  (transforms
                   (gnu-build-system . ((with-configure-flag . "hello=--enable-nls")))))))
         ;; in the future we want to automatically create (one-of x x!) if both exist
         (one-of (list nls nls!)))))))
