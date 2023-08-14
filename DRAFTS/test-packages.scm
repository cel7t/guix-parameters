(define-module (test-packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xorg)
  #:use-module (guix parameters)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table))

(define git-st
  (package-with-parameters
  ;; pspec
  (parameter-spec
    (local
      (list
	(package-parameter
	  (name 'git)
	  (variants
	    (parameter-variant-match
	      (on #:transform (with-git-url #:package-name "https://github.com/cel7t/st")))))))
    (defaults '((git on))))
  (inherit st)
  (name "git-st")))

git-st
