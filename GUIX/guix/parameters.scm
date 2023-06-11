;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Sarthak Shah <shahsarthakw@gmail.com>
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

(define-module (guix parameters)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:export (package-parameter
            package-parameter?
            package-parameter-name
            package-parameter-property
            package-parameter-type
            package-parameter-description

            boolean
            optionally

            package-parameters
            lookup-package-parameter
            package-parameter-value
            set-package-parameter-value))

;;; Commentary:
;;;
;;; This module provides a way to express high-level "package parameters",
;;; which allow users to customize how packages are built.  Parameters are an
;;; interface that package developers define, where each parameter has a name
;;; and type.  The user interface then converts parameter values from string
;;; to Scheme values and records them in the package properties.
;;;
;;; Package parameters are discoverable; their description is
;;; internationalized.  The possible values of a parameter can be enumerated,
;;; and thus the Cartesian product of all possible parameter values for a
;;; package can be enumerated as well.
;;;
;;; Code:

;; Package parameter interface.
(define-record-type* <package-parameter> package-parameter
  make-package-parameter
  package-parameter?
  (name          package-parameter-name)
  (property      package-parameter-property (default (string->symbol name)))
  (type          package-parameter-type (default boolean))
  ;; the standard transforms; of the form (list ((build-system ...) transform))
  ;; sanitizer converts ((a b) t1 t2 t3) -> (a t*) (b t*) where t* is the composition of t1 t2 ...
  ;; this is then turned into a hash table s.t (hash-ref <tbl> build-system) returns t*
  ;; XXX: option to have common elements among multiple build systems (inherit of sorts)
  (transforms    package-parameter-transforms
                 (default (alist->hash-table '()))
                 (sanitize (lambda (val)
                             (cond
                              ((hash-table? val) val)
                              ((and (list? val)
                                    (list? (car val)))
                               (alist->hash-table
                                (apply append
                                       (map (lambda (x)
                                              (cons x
                                                (options->transformation (cdr val))))
                                            (car val)))))
                              (else (throw 'bad! val))))))
  ;; ONLY TO BE USED IN LOCAL DEFINITIONS
  ;; if set to #t, parameter is considered default
  (default? package-parameter-default? (default #f))
  (description   package-parameter-description))

;; Note that if a transform applies to all but a, b and c,
;; (case build-system
;;  ((a b c) error-out)
;;  (else do-something))
;; works

;; Type of a package parameter.
(define-record-type* <parameter-type> parameter-type
  make-parameter-type
  parameter-type?
  (name          parameter-type-name)              ;debugging purposes only!
  (string->value parameter-type-string->value)
  (value->string parameter-type-value->string)
  (universe      parameter-type-universe))

;; Here is how package parameter specs should be declared:
;; (parameter-spec
;;  (package-name "foo")
;;  (required-parameters (list a b c))
;;  (optional-parameters (list d e)))

;; thunked -> we can do stuff like (parameter-spec-optional-parameters ps) to get the optional parameters
(define-record-type* <parameter-spec> parameter-spec
  make-parameter-spec
  parameter-spec?
  this-parameter-spec
  (package-name package-name)
  ;; local-parameters: parameters specific to the package
  ;; XXX: extract the symbol from the parameter record thru a sanitizer
  (local    ps/local-parameters
    ;; keeping it as an alist as it will be useful to retrieve them for the UI
            (default '())
            (thunked))
  (required ps/required-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked)) 
  ;; XXX: automatically get local parameters
  (optional ps/optional-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked))
  ;; XXX: automatically create (x x!) if both are defined
  (one-of ps/one-of-parameters
            (default '())
            (thunked))
  (special ps/special-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked))
  (canonical-combinations ps/canonical-combinations
                          ;; XXX: here we'll run the parser that returns default values
                          ;;      also have a sanitizer that creates combinations from lists
                          (default '())
                          (thunked))
  (transforms ps/transforms
              ;; XXX: check if all the SPECIAL have been given transforms or not
              (default (alist->hash-table '()))
              ;; use the parameter transform sanitizer here
              (sanitize (lambda (val)
                          (cond ((hash-table? val) val)
                                ((list? val) (alist->hash-table val))
                                (else (throw 'bad! val)))))
              (thunked)))

;; for `one-of` we will still use a list to represent the tree
;; as a hash map will not benefit it

;; XXX: declare a MACRO that makes it possible to declare packages
;; with other parameters without transforms
;; One way could be to turn the package modifiers into transforms themselves
;; (package
;;   ...
;;    (p/if (a b) some-property
;;          something))
;; becomes
;; (package
;;   ...
;;   (parameters
;;    ...
;;    (special
;;     ((a b)
;;      transform-that-adds-something-to-some-property))))
;; or it could be a function with these parameters as arguments
;; 
;; another way would be to create 'parameteric variants'
;; this would be similar to package/inherit but here it won't create a new package
;; but rather a variant of the package with the specific properties defined
;; TEST: packages as procedures with non-dependency arguments

;; g23: Most parameters should be boolean
;; Might make sense to add a recursive type
(define boolean
  ;; The Boolean parameter type.
  (parameter-type (name 'boolean)
                  (universe '(#true #false))
                  (value->string
                   (match-lambda
                     (#f "false")
                     (#t "true")))
                  (string->value
                   (lambda (str)
                     (cond ((string-ci=? str "true")
                            #t)
                           ((string-ci=? str "false")
                            #f)
                           (else
                            (raise (condition
                                    (&message (message "wrong value"))))))))))

(define (package-parameters package)
  (or (assq-ref (package-properties package) 'parameters)
      '()))

(define (package-parameter-value package parameter)
  (assq-ref (package-properties package)
            (package-parameter-property parameter)))

(define (lookup-package-parameter package name)
  (find (lambda (parameter)
          (string=? (package-parameter-name parameter) name))
        (package-parameters package)))

(define (set-package-parameter-value package name value)
  (let ((parameter (lookup-package-parameter package name))
        (location  (package-field-location package 'properties)))
    (unless parameter
      (raise (apply make-compound-condition
                    (formatted-message
                     (G_ "~a: no such package parameter")
                     name)
                    (if location
                        (list (condition
                               (&error-location (location location))))
                        '()))))
    (let* ((property (package-parameter-property parameter))
           (type     (package-parameter-type parameter))
           (value    ((parameter-type-string->value type) value)))
      (package/inherit package
        (properties
         (alist-cons property value
                     (alist-delete property (package-properties package)
                                   eq?)))))))

;; (define-syntax-rule (optionally property exp)
;;   (if (assq-ref (package-properties this-package) property)
;;       (list exp)
;;       '()))

(define-syntax p/if
  (syntax-rules ()
    [(p/if property exp)
     (let ((properties (package-properties this-package)))
       (if (if (list? property)
               (member
                     #t
                     (map (lambda (x) (not (not (assq-ref properties x))))
                          property))
               (assq-ref properties property))
           (list exp)
           '()))]
    [(p/if property exp exp-else)
     (let ((properties (package-properties this-package)))
       (if (if (list? property)
               (member
                     #t
                     (map (lambda (x) (not (not (assq-ref properties x))))
                          property))
               (assq-ref properties property))
           (list exp)
           (list exp-else)))]))

(define-syntax p/if-all
  (syntax-rules ()
    [(p/if-all property exp)
     (let ((properties (package-properties this-package)))
       (if (if (list? property)
               (not (member
                     #f
                     (map (lambda (x) (not (not (assq-ref properties x))))
                          property)))
               (assq-ref properties property))
           (list exp)
           '()))]
    [(p/if-all property exp exp-else)
     (let ((properties (package-properties this-package)))
       (if (if (list? property)
               (not (member
                     #f
                     (map (lambda (x) (not (not (assq-ref properties x))))
                          property)))
               (assq-ref properties property))
           (list exp)
           (list exp-else)))]))

;; Test these macros without using packages:
;; (define (package-properties _) '((a . 1) (b . 2) (c . 3)))
;; (define this-package '())

;; (p/if '(a b e)
;;       (display "YES")
;;       (display "NO"))

;; (p/if-all '(a b e)
;;           (display "NO")
;;           (display "YES"))

;; p/match-any:
;; (p/match-any
;; ((a b) e1 e2 ..)
;; ((c) d1 d2 ..)
;; (else c1 c2 ...))

(define-syntax p/match-any
  (syntax-rules (all)
    [(_) '()]
    [(_ (all clauses ...)) (begin clauses ...)]
    [(_ ((parameters ...)) rest ...) (p/match-any rest ...)]
    [(_ ((parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (begin
         (and (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...)))
              (begin clauses ...))
         (p/match-any rest ...)))]))

;; (let ((SOME_ALIST_FOR_THIS_EXAMPLE '()))
;;   (p/match-any
;;    (('a 'd)
;;     (set! SOME_ALIST_FOR_THIS_EXAMPLE (append '(1) SOME_ALIST_FOR_THIS_EXAMPLE))
;;     (set! SOME_ALIST_FOR_THIS_EXAMPLE (append '(2) SOME_ALIST_FOR_THIS_EXAMPLE)))
;;    (('c))
;;    (('e)
;;     (set! SOME_ALIST_FOR_THIS_EXAMPLE (append '(3) SOME_ALIST_FOR_THIS_EXAMPLE)))
;;    (all
;;     (set! SOME_ALIST_FOR_THIS_EXAMPLE (append '(4) SOME_ALIST_FOR_THIS_EXAMPLE))))
;;   SOME_ALIST_FOR_THIS_EXAMPLE)

;; The answer to this should be '(4 2 1)
;; note that all is essentially useless, one can simply put the expression in all
;; outside the macro and it will work the same

(define-syntax p/match-all
  (syntax-rules (all)
    [(_) '()]
    [(_ (all clauses ...)) (begin clauses ...)]
    [(_ ((parameters ...)) rest ...) (p/match-all rest ...)]
    [(_ ((parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (begin
         (and (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...))))
              (begin clauses ...))
         (p/match-all rest ...)))]))

;; (p/match-all
;;  (('a 'b) (display "YES") (display "YES"))
;;  (('c 'd) (display "NO"))
;;  (all (display "ALL")))

(define-syntax p/match-case-any
  (syntax-rules (all)
    [(_) '()]
    [(_ (all clauses ...)) (begin clauses ...)]
    [(_ ((parameters ...)) rest ...) (p/match-case-any rest ...)]
    [(_ ((parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (if (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                           (list parameters ...)))
           (begin clauses ...)
           (p/match-case-any rest ...)))]))

;; should short-circuit at YESYES
;; (p/match-case
;;  (('a 'b 'e) (display "YES") (display "YES"))
;;  (('c 'd) (display "NO"))
;;  (all (display "ALL")))
   

;; p/match:
;; combine all and any into one
;; (p/match
;;  ((any a b) ...)
;;  ((all a b c) ...)
;;  (all ...))

(define-syntax p/match
  (syntax-rules (all any)
    [(_) '()]
    [(_ (all clauses ...) rest ...) (begin (begin clauses ...) (p/match rest ...))]
    [(_ ((predicate parameters ...)) rest ...) (p/match rest ...)]
    [(_ ((all parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (begin
         (and (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...))))
              (begin clauses ...))
         (p/match rest ...)))]
    [(_ ((any parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (begin
         (and (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...)))
              (begin clauses ...))
         (p/match rest ...)))]))

;; (p/match
;;  ((all 'a 'b) (display "YES"))
;;  (all (display "YES"))
;;  ((any 'c 'e) (display "YES"))
;;  ((all 'a 'o) (display "NO"))
;;  (all (display "ALL")))
 
(define-syntax p/match-case
  (syntax-rules (all any)
    [(_) '()]
    [(_ (all clauses ...) rest ...) (begin clauses ...)]
    [(_ ((predicate parameters ...)) rest ...) (p/match-case rest ...)]
    [(_ ((all parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (if (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                                (list parameters ...))))
           (begin clauses ...)
           (p/match-case rest ...)))]
    [(_ ((any parameters ...) clauses ...) rest ...)
     (let ((properties (package-properties this-package)))
       (if (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                           (list parameters ...)))
           (begin clauses ...)
           (p/match-case rest ...)))]))

;; (p/match-case
;;  ((all 'a 'f) (display "NO"))
;;  ;; (all (display "YES"))
;;  ;; ((any 'c 'e) (display "YES"))
;;  ;; ((all 'a 'b) (display "YES"))
;;  (all (display "ALL")))


;; Now before proceeding with writing a --with-parameter transform,
;; the following things need to be brought into order:
;; - global parameter definitions
;; - parameter spec and the properties field
;; we will be replacing the original patch's method of writing
;; all parameters in the properties field, and instead use this
;; parameter-spec record type
;; it is a data-structure similar to the `parameters` structure
;; declared at the start of DRAFTS/parameter-parser.scm
