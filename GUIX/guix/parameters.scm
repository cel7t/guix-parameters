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
  #:use-module (guix transformations)
  #:use-module (guix profiles)
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

            build-system/transform
            build-system/transform-match
            parameter-spec-property
            package-parameter-spec
            parameter-spec/all-parameters
            parameter-spec/base-parameter-alist
            parameter-spec/override-alist
            parameter-spec/validate-parameter-alist
            parameter-spec/resolve-parameter-alist
            %global-parameters
            define-global-parameter

            package-with-parameters
            parameter-spec/parameter-alist
            parameter/if
            parameter/if-all
            parameter/match-any
            parameter/match-all
            parameter/match-case-any
            parameter/match
            parameter/match-case
            parameter/modify-inputs))

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
                 (sanitize sanitize-string-or-symbol))
  (type          package-parameter-type (default boolean))
  ;; ;; the standard transforms; of the form (list ((build-system ...) transform))
  ;; ;; sanitizer converts ((a b) t1 t2 t3) -> (a t*) (b t*) where t* is the composition of t1 t2 ...
  ;; ;; this is an alist, the parser will handle the special keyword `all` as applicable to all systems.
  ;; (transforms    package-parameter-transforms
  ;;                (default (alist->hash-table '()))
  ;;                (sanitize sanitize-build-system-transforms))
  (morphisms     (default (alist->hash-table '()))
                 (sanitize sanitize-build-system-morphisms))

  ;; SCOPE FOR IMPROVEMENT:
  ;; another field for package-input-rewriting called 'rewrite'
  ;; consult Pjotr and Gabor about this

  ;; ONLY TO BE USED IN LOCAL DEFINITIONS
  ;; if set to #t, parameter is considered default
  ;; 6/15: just use parameter-spec/defaults
  ;; (default? package-parameter-default? (default #f))
  (description   package-parameter-description (default "")))

;; Type of a package parameter.
;; (define-record-type* <parameter-type> parameter-type
;;   make-parameter-type
;;   parameter-type?
;;   (name          parameter-type-name)              ;debugging purposes only!
;;   (string->value parameter-type-string->value)
;;   (value->string parameter-type-value->string)
;;   (universe      parameter-type-universe))

;; SANITIZERS

(define %global-parameters
  (alist->hash-table '()))

(define (sanitize-string-or-symbol x)
  (cond ((string? x) (string->symbol x))
        ((symbol? x) x)
        (else (throw 'bad! x))))

;; (define (sanitize-build-system-transforms ls)
(define (sanitize-build-system-morphisms ls)
  ;; ((a . t1 t2 ...) ((b c) t3 t4 ...))
  (cond ((hash-table? ls) ls)
        ((list? ls)
         (alist->hash-table ls))
        (else (throw 'bad! ls))))

(define-syntax lots-of-cons->alist
  (syntax-rules ()
    ((_ (a . b))
     (list (cons 'a b)))
    ((_ (a . b) rest ...)
     (cons (cons 'a b)
           (lots-of-cons->alist rest ...)))))

(define-syntax build-system/transform
  (syntax-rules (-> _)
    ((build-system/transform (x ...) -> y ...)
     (map (lambda (g)
            (cons g (lots-of-cons->alist y ...)))
          (list x ...)))
    ((build-system/transform _ -> y ...) ; for local parameter definitions
     (cons 'any ; matches any build system
      (lots-of-cons->alist y ...)))
    ((build-system/transform x -> y ...)
     (cons x (lots-of-cons->alist y ...)))))

;; Parameter Morphisms:
;; (parameter/morphism
;;  (sym + build-system -> morphisms ...))

;; alist->hash-table of the format
;; ((build-system . ((sym . ((transforms (a . b) ...) ...)) ...)) ...)

(define (return-list lst)
  (or (and (list? lst) lst)
      (list lst)))

(define-syntax parameter/morphism
  (syntax-rules (-> + _)
    [(%) '()]
    [(% _ -> morphisms ...)
     (cons 'any (cons 'any (parameter/parse-morphisms morphisms ...)))]
    [(% _ + _ -> morphisms ...)
     (cons 'any (cons 'any (parameter/parse-morphisms morphisms ...)))]
    [(% sym + _ -> morphisms ...)
     (let ((parsed-morphisms (parameter/parse-morphisms morphisms ...)))
       (cons 'any (map (lambda (g)
                         (cons g parsed-morphisms))
                       (return-list sym))))]
    [(% _ + b-system -> morphisms ...)
     (let ((parsed-morphisms (parameter/parse-morphisms morphisms ...)))
       (map (lambda (g) (cons g (cons 'any parsed-morphisms)))
            (return-list b-system)))]
    [(% sym + b-system -> morphisms ...)
     (let ((parsed-morphisms (parameter/parse-morphisms morphisms ...)))
       (map (lambda (g) (cons g (map (lambda (h) (cons h parsed-morphisms))
                                (return-list sym))))
            (return-list b-system)))]
    [(% sym -> morphisms ...)
     (let ((parsed-morphisms (parameter/parse-morphisms morphisms ...)))
       (cons 'any (map (lambda (g)
                         (cons g parsed-morphisms))
                       (return-list sym))))]))
    
;; (parameter/morphism '(1 2 3) + '(a b c) -> '(some morphisms))

;; look into more efficient ways to store this data

;; (define (parameter/parse-morphisms lst) lst) ; '(#:transform (a . b) ... #:rewrite (b . c) ... #:modify (c . d) ...)
           
(define-syntax build-system/transform-match
  (syntax-rules ()
    ((_ (x ...))
     (list
      (build-system/transform x ...)))
    ((_ (x ...) rest ...)
     (cons
      (build-system/transform x ...)
      (build-system/transform-match rest ...)))))

(define (local-sanitizer ls)
  (if (list? ls)
      (map (lambda (val)
             (cond ((package-parameter? val) val)
                   ((symbol? val) (package-parameter (name val)))
                   ((string? val) (package-parameter (name (string->symbol val))))
                   (else (throw 'bad! val))))
           ls)
      (throw 'bad! ls)))

(define (transform-sanitizer lv)
  (lambda (ls)
    (if (list? ls)
        (map (lambda (x)
               (if (eqv? #t (cdr x))
                   (cond
                    ((package-parameter? (car x))
                     (cons (package-parameter-name (car x))
                           (package-parameter-transforms (car x))))
                    ((symbol? (car x))
                     (cons (car x)
                           (package-parameter-transforms
                            (or
                             (find (lambda (g) (eqv? (car x)
                                                     (package-parameter-name g)))
                                   lv)
                             (hash-ref %global-parameters (car x))
                             (throw 'bad! (car x))))))
                    ((string? (car x))
                     (let ((y (string->symbol (car x))))
                       (cons y
                             (or
                              (find (lambda (g) (eqv? y
                                                      (package-parameter-name g)))
                                    lv)
                              (hash-ref %global-parameters y)
                              (throw 'bad! y))))))
                   x))
             ls)
        (throw 'bad! ls))))

;; thunked -> we can do stuff like (parameter-spec-optional-parameters ps) to get the optional parameters
(define-record-type* <parameter-spec> parameter-spec
  make-parameter-spec
  parameter-spec?
  this-parameter-spec
  ;; local-parameters: parameters specific to the package
  (local    parameter-spec/local
    ;; keeping it as an alist as it will be useful to retrieve them for the UI
    (default '())
    (sanitize local-sanitizer)
    (thunked))
  ;; 6/15: Pjotr recommended using a global hash table instead.
  ;;       See: (define-global-parameter), %global-parameters
  ;;       Lines commented out due to this will have an 'x615' next to them

  ;; (global parameter-spec/global ;; global parameters used must be declared
  ;;         (default '())
  ;;         (sanitizer (lambda (ls)
  ;;                      (map (lambda (val) ; they must be package parameters
  ;;                             (if (package-parameter? val)
  ;;                                 val
  ;;                                 (throw 'bad! val)))
  ;;                           ls)))
  ;;         (thunked))
  (defaults parameter-spec/defaults
    (default '())
    (thunked))
  (required parameter-spec/required
            (default '())
            (thunked))
  (optional parameter-spec/optional
            (default '()) ; 6/16: causing problems with parameter-spec/all-parameters
            ;; 6/13: removed the sanitizer as merging local and optional
            ;;       should be handled by the parser instead.
            (thunked))
  ;; XXX: automatically create (x x!) if both are defined
  ;; 6/12: this will be handled by the parser
  (one-of parameter-spec/one-of
          (default '())
          (thunked))
  ;; add dependencies
  ;; (dependencies (parameter/dependencies
  ;;                   (a b -> d e f)
  ;;  	               (c -> g h)))
  (dependencies parameter-spec/dependencies
              (default '())
              (thunked))
  (canonical parameter-spec/canonical-combinations
             (default parameter-spec/defaults)
             (thunked))
  (use-transforms parameter-spec/use-transforms ;; only use transforms for these
                  (default '())
                  (sanitize (transform-sanitizer (parameter-spec/local this-parameter-spec)))
                  (thunked))
  (parameter-alist parameter-spec/parameter-alist ;; this is ultimately what will be transformed by --with-parameters
                   ;; '((a . #t) (b . #f) ...)
                   (default (parameter-spec/base-parameter-alist this-parameter-spec)) ; if this doesn't work some tricks might be needed
                   (thunked)))

;; g23: Most parameters should be boolean
;; Might make sense to add a recursive type
;; (define boolean
;;   ;; The Boolean parameter type.
;;   (parameter-type (name 'boolean)
;;                   (universe '(#t #f))
;;                   (value->string
;;                    (match-lambda
;;                      (#f "off")
;;                      (#t "on")))
;;                   (string->value
;;                    (lambda (str)
;;                      (cond ((string-ci=? str "on")
;;                             #t)
;;                            ((string-ci=? str "off")
;;                             #f)
;;                            (else
;;                             (raise (condition
;;                                     (&message (message "wrong value"))))))))))

(define-syntax parameter-spec-property
  (syntax-rules ()
    [(parameter-spec-property body ...)
     (cons 'parameter-spec
           (parameter-spec body ...))]))

(define (transform-for-build-system parameter-transforms the-build-system)
  (or (hash-ref parameter-transforms the-build-system)
      (hash-ref parameter-transforms 'any)
      (throw 'bad! the-build-system)))


(define-syntax package-with-parameters
  (syntax-rules ()
    [(package-with-parameters body ...)
     (let ((the-package (package body ...)))
       ((options->transformation
         (apply append
                (let ((the-build-system (package-build-system the-package)))
                  (map (lambda (x)
                         (transform-for-build-system
                          (assq-ref (parameter-spec/use-transforms
                                     (package-parameter-spec the-package))
                                    (car x))
                          the-build-system))
                       (filter (lambda (x) (eqv? #t (cdr x)))
                               (parameter-spec/parameter-alist
                                (package-parameter-spec the-package)))))))
        the-package))]))

(define (package-parameter-spec package)
  (or (assq-ref (package-properties package) 'parameter-spec)
      '()))

(define (parameter-spec/all-parameters pspec) ; for the UI
  ;; '(sym-a sym-b ...)
  (delete-duplicates
   (append
    (map (lambda (x) (package-parameter-name x))
         (parameter-spec/local pspec))
    ;; x615
    ;; (map (lambda (x) (package-parameter-name x))
    ;;      (parameter-spec/global pspec))
    (parameter-spec/defaults pspec)
    (parameter-spec/required pspec)
    (apply append (parameter-spec/one-of pspec))
    (parameter-spec/optional pspec))))

(define (parameter-spec/base-parameter-alist pspec) ; returns base case
  ;; '((a . #t) (b . #f) ...)
  (let* ((default/on (delete-duplicates
                      (append
                       (map (lambda (x) (cons x #t))
                            (parameter-spec/required pspec))
                       (map (lambda (x) (cons x #t))
                            (parameter-spec/defaults pspec)))))
         (default/all (append
                       default/on
                       (map (lambda (x) (cons x #f))
                            (filter (lambda (x) (not (member (cons x #t)
                                                             default/on)))
                                    (parameter-spec/all-parameters pspec)))))
         (default/syms (map car default/all)))
    (if (not (eq? default/syms
                  (delete-duplicates default/syms)))
        (begin
          (throw 'bad! default/syms)
          '())
        default/all)))

(define (parameter-spec/override-alist pspec plist)
  ;; A: (INTERSECT PLIST PSPEC/ALL) + (DIFF PSPEC/BASE PLIST)
  ;; B: OFF[(DIFF PSPEC/ALL A)]
  ;; A + B
  (let* ((all-p (parameter-spec/all-parameters pspec))
         (plist/sym (map car plist))
         (override/a (append
                      (filter (lambda (x) (member (car x) all-p))
                              plist)
                      (filter (lambda (x) (not (member (car x) plist/sym)))
                              (parameter-spec/base-parameter-alist pspec))))
         (override/a-sym (map car override/a)))
    (append
     override/a
     (map (lambda (x) (cons x #f))
          (filter (lambda (x) (not (member x override/a-sym)))
                  all-p)))))

(define (parameter-spec/validate-parameter-alist pspec plist) ; #t or #f
  (define (validate/logic) ; defined as functions - want to call them individually
    (let ((PLH (alist->hash-table plist)))
      (fold (lambda (x y) (and x y)) #t
            (cons
             (equal? (parameter-spec/required pspec)
                     (filter (lambda (x) (hash-ref PLH x))
                             (parameter-spec/required pspec)))
             (append
              (map (lambda (ls)
                     (> 2 (length
                           (filter (lambda (x) (hash-ref PLH x))
                                   ls))))
                   (parameter-spec/one-of pspec)))))))
  (define (validate/duplicates)
    (define (validate/not-there? x lst)
      (if (not (member x lst))
          (if (> (length lst) 1)
              (validate/not-there? (car lst) (cdr lst))
              #t)
          #f))
    (let ((alist-p (map car plist)))
      (validate/not-there? (car alist-p) (cdr alist-p))))
  (define (validate/coverage)
    (let ((all-p (parameter-spec/all-parameters pspec))
          (alist-p (map car plist)))
      (fold (lambda (x y) (and x y)) #t
            (map (lambda (x) (not (not (member x alist-p))))
                 all-p))))
  (cond ((not (validate/logic))
         ;;; XXX: use raise/formatted-message instead of display
         (begin (display "There is a logic error in the given list")
                #f))
        ((not (validate/coverage))
         (begin (display "There is a coverage error: check the pipeline")
                #f))
        ((not (validate/duplicates))
         (begin (display "There are duplicates in the given list")
                #f))
        (else #t)))

(define (parameter-spec/resolve-parameter-alist pspec plist) ; checks if plist works
  ;; response: (#t . overriden-plist) if it works, (#f . (parameter-spec/parameter-alist pspec)) otherwise
  ;; parameter-alist -> base-parameter-alist by default, but can be overriden
  ;; this command can thus be chained

  ;; DEFAULT: pspec_ default -> valid?: (validate default) -> default: valid? default | '()
  ;; TRANSFORM -> PLIST
  ;; -> R-PLIST: (intersect plist all) - (common plist defaults) + (uncommon plist defaults)
  ;; -> valid?: (validate R-PLIST) -> pspec/parameter-alist: valid? R-PLIST pspec/parameter-alist
  (let ((olist (parameter-spec/override-alist pspec plist)))
    (if (parameter-spec/validate-parameter-alist pspec olist)
        (cons #t olist)
        (cons #f (parameter-spec/parameter-alist pspec)))))

;; %global-parameters: hash table containing global parameters ref'd by syms

(define-syntax define-global-parameter
  (syntax-rules ()
    [(define-global-parameter (parameter-definition ...))
     (let ((gp-val (parameter-definition ...)))
       (hash-set! %global-parameters
                  (package-parameter-name gp-val)
                  gp-val))]))

;; (define-global-parameter (package-parameter
;;                           (name "tests!")
;;                           (description "no tests")))
;; Works!

(define-syntax parameter/if
  (syntax-rules ()
    [(parameter/if property exp)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (if (list? property)
               (member
                #t
                (map (lambda (x) (not (not (assq-ref properties x))))
                     property))
               (assq-ref properties property))
            exp
           '()))]
    [(parameter/if property exp exp-else)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (if (list? property)
               (member
                #t
                (map (lambda (x) (not (not (assq-ref properties x))))
                     property))
               (assq-ref properties property))
           exp
           exp-else))]))

(define-syntax parameter/if-all
  (syntax-rules ()
    [(parameter/if-all property exp)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (if (list? property)
               (not (member
                     #f
                     (map (lambda (x) (not (not (assq-ref properties x))))
                          property)))
               (assq-ref properties property))
           exp
           '()))]
    [(parameter/if-all property exp exp-else)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (if (list? property)
               (not (member
                     #f
                     (map (lambda (x) (not (not (assq-ref properties x))))
                          property)))
               (assq-ref properties property))
           exp
           exp-else))]))

;; Test these macros without using packages:
;; (define (parameter-spec/parameter-alist _) '((a . 1) (b . 2) (c . 3)))
;; (define (package-parameter-spec _) #t)
;; (define this-package '())

;; (parameter/if '(a b e)
;;       "YES"
;;       "NO")

;; (parameter/if-all '(a b e)
;;           (display "NO")
;;           (display "YES"))

;; parameter/match-any:
;; (parameter/match-any
;; ((a b) e1 e2 ..)
;; ((c) d1 d2 ..)
;; (else c1 c2 ...))

(define-syntax parameter/match-any
  (syntax-rules (_)
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter/match-any rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...)))
              (begin clauses ...))
         (parameter/match-any rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (not (assq-ref properties parameter)))
              (begin clauses ...))
         (parameter/match-any rest ...)))]))

;; (let ((SOME_ALIST_FOR_THIS_EXAMPLE '()))
;;   (parameter/match-any
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

(define-syntax parameter/match-all
  (syntax-rules (_)
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter/match-all rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter/match-all rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (not (assq-ref properties parameter)))
              (begin clauses ...))
         (parameter/match-all rest ...)))]))

;; (parameter/match-all
;;  (('a 'b) (display "YES") (display "YES"))
;;  (('c 'd) (display "NO"))
;;  (all (display "ALL")))

(define-syntax parameter/match-case-all
  (syntax-rules ()
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter/match-case-any rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (and (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                           (list parameters ...))))
           (begin clauses ...)
           (parameter/match-case-any rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (and (not (not (assq-ref properties parameter)))
           (begin clauses ...)
           (parameter/match-case-any rest ...)))]))

;; should short-circuit at YESYES
;; (parameter/match-case
;;  (('a 'b 'e) (display "YES") (display "YES"))
;;  (('c 'd) (display "NO"))
;;  (all (display "ALL")))


;; parameter/match:
;; combine all and any into one
;; (parameter/match
;;  ((any a b) ...)
;;  ((all a b c) ...)
;;  (all ...))

(define-syntax parameter/match
  (syntax-rules (_ all)
    [(%) '()]
    [(% (_ clauses ...) rest ...) (begin (begin clauses ...) (parameter/match rest ...))]
    [(% (parameters) rest ...) (parameter/match rest ...)]
    [(% ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter/match rest ...)))]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...)))
              (begin clauses ...))
         (parameter/match rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (not (assq-ref properties parameter)))
              (begin clauses ...))
         (parameter/match rest ...)))]))

;; (parameter/match
;;  ((all 'a 'b) (display "YES"))
;;  (_ (display "YES"))
;;  (('c 'e) (display "YES"))
;;  ((all 'a 'o) (display "NO"))
;;  (_ (display "ALL")))

(define-syntax parameter/match-case
  (syntax-rules (all _)
    [(%) '()]
    [(% (_ clauses ...) rest ...) (begin clauses ...)]
    [(% (parameters) rest ...) (parameter/match-case rest ...)]
    [(% ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                                (list parameters ...))))
           (begin clauses ...)
           (parameter/match-case rest ...)))]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                           (list parameters ...)))
           (begin clauses ...)
           (parameter/match-case rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (not (not (assq-ref properties parameter)))
           (begin clauses ...)
           (parameter/match-case rest ...)))]))

;; (parameter/match-case
;;  ((all 'a 'f) (display "NO"))
;;  ;; (all (display "YES"))
;;  ;; ((any 'c 'e) (display "YES"))
;;  ;; ((all 'a 'b) (display "YES"))
;;  (all (display "ALL")))

(define-syntax parameter/modifier-if
  (syntax-rules (_ all delete prepend append replace)
    [(% _ exp exp2)
     exp]
    [(% (all parameters ...) exp exp2)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (member #t
                   (map (lambda (x) (not (not (assq-ref properties x))))
                        (list parameters ...)))
           exp
           exp2))]
    [(% (all parameter) exp exp2)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (assq-ref properties parameter))
       exp
       exp-else)]
    [(% parameter exp exp2)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (if (list? parameter)
               (member
                #t
                (map (lambda (x) (not (not (assq-ref properties x))))
                     parameter))
               (assq-ref properties parameter))
           exp
           exp2))]))

(define-syntax parameter/modify-inputs
  (syntax-rules (_ all delete prepend append replace)
    [(% inputs (parameter) clauses ...)
     (parameter/modify-inputs inputs clauses ...)]
    [(% inputs (parameter (delete name) rest ...) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (alist-delete name inputs)
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (delete names ...) rest ...) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (fold alist-delete inputs (list names ...))
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (prepend lst ...) rest ...) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (append (map add-input-label (list lst ...)) inputs)
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (append lst ...) rest ...) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (append inputs (map add-input-label (list lst ...)))
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (replace name replacement) rest ...) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (replace-input name replacement inputs)
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs)
     inputs]))

(define (give-me-a-symbol ex)
  (cond ((symbol? ex) ex)
        ((string? ex) (string->symbol ex))
        (else (throw 'bad! ex))))

(define-record-type* <parameter-type> parameter-type
  make-parameter-type
  parameter-type?
  this-parameter-type
  (name          parameter-type-name
                 (sanitize give-me-a-symbol))
  (universe      parameter-type-universe)
  (negation      parameter-type-negation
                 (default (car (parameter-type-universe this-parameter-type)))
                 (thunked))
  (description   parameter-type-description
                 (default "")))

;; (parameter-type-negation
;;  (parameter-type
;;   (name "ok")
;;   (universe '(not-ok ok))))

(define-syntax parameter/type
  (syntax-rules (_)
    [(% _ rest ...)
     (parameter/type (string-append (or (package-parameter-name this-package-parameter)
                                      "blank")
                                    "-type")
                     rest ...)]
    [(% t-name t-universe)
     (parameter-type
      (name t-name)
      (universe t-universe))]
    [(% t-name t-universe t-negation)
     (parameter-type
      (name t-name)
      (universe t-universe)
      (negation t-negation))]
    [(% t-name t-universe t-negation t-description)
     (parameter-type
      (name t-name)
      (universe t-universe)
      (negation t-negation)
      (description t-description))]))

;; (parameter-type-negation (parameter/type _ '(1 2 3)))
;; 
;; (define (package-parameter-name _) #f)
;; (define this-package-parameter #f)
(define boolean
  (parameter-type
   (name 'boolean)
   (universe '(off on))
   (description "Boolean Parameter Type")))
