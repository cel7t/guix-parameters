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
            parameter-type
            parameter-spec

            boolean
            package-parameter-spec
            parameter/if
            parameter/if-all
            parameter/match-any
            parameter/match-all
            parameter/match-case-any
            parameter/match
            parameter/match-case))

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
  (name          package-parameter-name
                 (sanitize (lambda (x)
                             (cond
                              ((string? x) (string->symbol x))
                              ((symbol? x) x)
                              (else (throw 'bad! x))))))
  (type          package-parameter-type (default boolean))
  ;; the standard transforms; of the form (list ((build-system ...) transform))
  ;; sanitizer converts ((a b) t1 t2 t3) -> (a t*) (b t*) where t* is the composition of t1 t2 ...
  ;; this is an alist, the parser will handle the special keyword `all` as applicable to all systems.
  (transforms    package-parameter-transforms
                 (default #f) ; no transforms by default
                 (sanitize (lambda (val)
                             (if (and (list? val)
                                      (list? (car val)))
                                 (apply append
                                        (map (lambda (x)
                                               (cons x
                                                     (options->transformation (cdr val))))
                                             (car val)))
                                 (throw 'bad! val)))))
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

;; thunked -> we can do stuff like (parameter-spec-optional-parameters ps) to get the optional parameters
(define-record-type* <parameter-spec> parameter-spec
  make-parameter-spec
  parameter-spec?
  this-parameter-spec
  ;; local-parameters: parameters specific to the package
  (local    ps/local
    ;; keeping it as an alist as it will be useful to retrieve them for the UI
    (default '())
    (sanitize (lambda (ls)
                (if (list? ls)
                    (map (lambda (val)
                           (cond ((package-parameter? val) val)
                                 ((string? val) (package-parameter (name val)))
                                 ((symbol? val) (package-parameter (name (symbol->string val))))
                                 (else (throw 'bad! val))))
                         ls)
                    (throw 'bad! val))))
    (thunked))
  (global ps/global ;; global parameters used must be declared
          (default '())
          (sanitizer (lambda (ls)
                       (map (lambda (val) ; they must be package parameters
                              (if (package-parameter? val)
                                  val
                                  (throw 'bad! val)))
                            ls)))
          (thunked))
  (defaults ps/defaults
    (default '())
    (thunked))
  (required ps/required
            (default '())
            (thunked)) 
  (optional ps/optional
            (default (map (lambda (x) (package-parameter-name x)) ps/local))
            ;; 6/13: removed the sanitizer as merging local and optional
            ;;       should be handled by the parser instead.
            (thunked))
  ;; XXX: automatically create (x x!) if both are defined
  ;; 6/12: this will be handled by the parser
  (one-of ps/one-of
          (default '())
          (thunked))
  (canonical ps/canonical-combinations
             (default ps/defaults)
             (thunked))
  (use-transforms ps/use-transforms ;; only use transforms for these
                  (default '())
                  (sanitize (lambda (ls)
                              (if (list? ls)
                                  (map (lambda (xc)
                                         (if (eqv? #t (cdr x))
                                             (cond
                                              ((package-parameter? (car x))
                                               (cons (package-parameter-name (car x))
                                                     (package-parameter-transforms (car x))))
                                              ((symbol? (car x))
                                               (cons (car x)
                                                     (find (lambda (g) (evq? (car x)
                                                                        (package-parameter-name g)))
                                                           ps/local)))
                                              ((string? (car x))
                                               (cons (string->symbol (car x))
                                                     (find (lambda (g) (evq? (string->symbol (car x))
                                                                        (package-parameter-name g)))
                                                           ps/local))))))
                                       ls)
                                  (throw 'bad! val))))
                  (thunked)))

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

(define (package-parameter-spec package)
  (or (assq-ref (package-properties package) 'parameter-spec)
      '()))

(define (ps/all-parameters pspec) ; for the UI
  ;; '(sym-a sym-b ...)
   (append
    (map (lambda (x) (package-parameter-name x))
         (ps/local pspec))
    (map (lambda (x) (package-parameter-name x))
         (ps/global pspec))
    (ps/defaults pspec)
    (ps/required pspec)
    (apply append (ps/one-of pspec))
    (ps/optional pspec)))

(define (ps/parameter-alist pspec))
  ;; '((sym-a . #t) (sym-b . #f) ...)
        
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

;; problem: given a symbol, we want to find the associated global parameter
;; solution:
;;   1. have all global parameters defined in one place
;;   2. define %global-package-parameters as a hash with sym keys
