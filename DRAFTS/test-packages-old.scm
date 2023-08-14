(define-module (DRAFTS test-packages)
  #:use-module (gnu packages base)
  #:use-module (guix parameters)
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
    (name "hello-parameterized")
    (properties
     `(,(parameter-spec
         ;; local -> optional by default
         ;; no other parameters to add to this example for now
         (local (list
                 (package-parameter
                  (name "nls!")
                  (transforms
                   '(((gnu-build-system) . ((with-configure-flag . "hello=--disable-nls"))))))))
         ;; in the future we want to automatically create (one-of x x!) if both exist
         (use-transforms '((nls! . #t))))))))

hello-parameterized

(define-public hello-no-nls
  (package
    (inherit hello)
    (name "hello-no-nls")
    (properties
     `(,(parameter-spec
         ;; local -> optional by default
         ;; no other parameters to add to this example for now
         (local (list
                 (package-parameter
                  (name "nls!")
                  (transforms
                   '(((gnu-build-system) . ((with-configure-flag . "hello=--disable-nls"))))))))
         ;; in the future we want to automatically create (one-of x x!) if both exist
         (use-transforms '((nls! . #t)))
         (defaults '(nls!)))))))

;; (define-public test-package
;;   (package
;;     (inherit some-package)
;;     (name "test-package-for-parameters")
;;     (properties
;;      `(,(parameter-spec
;;          (local (list "xyz" ; "str" -> (package-parameter (name "str"))
;;                       'abc ; 'sym -> (package-parameter (name 'sym))
;;                       (package-parameter
;;                        (name "uvw"))))
;;          (global (list global-parameter)) ; all global parameters need to be declared
;;          (defaults '(xyz)) ; we use syms to refer to parameters
;;          (one-of '((xyz abc)))
;;          (optional '(global-parameter)) ; all local parameters are optional by default
;;          (use-transforms (list ; transforms will only be used for these
;;                           ('uvw . ((with-configure-flag . "some-package=--some-flag")))
;;                           (global-parameter . #t))))) ; it uses its default transform
;;     (parameter/if (xyz abc)
;;                   (modify-inputs (package-inputs some-package)
;;                     (append libaaa))))))

;; PARSER:
;; 3 phases
;; 1) processing - process the spec and the input parameter list
;; 2) validation - validate the input parameter list against the spec
;; 3) application - apply the parameter list through procedures and macros

;; parameter-parser.scm has functions for validation
;; functions need to be adapted to data structures for processing
;; application needs to be written from scratch

;; application phase will be a bit troublesome
