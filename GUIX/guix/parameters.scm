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
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix records)
  #:use-module (guix transformations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  ;? -> remove?
  #:export (parameter-type;
            package-parameter;
            parameter-spec;
            boolean-parameter-type;

            parameter-variant;
            parameter-variant-match;
            parameter-spec-property;?
            package-parameter-spec;
            all-spec-parameters;
            base-parameter-alist;
            parameter-process-list;
            package-override-plist;?
            parameter-spec-validate;
            package-resolve-parameter-list;
            %global-parameters;
            define-global-parameter;

            package-with-parameters;
            parameterize-package;
            apply-variants;
            parameter-spec-parameter-alist;
            parameter-if;
            parameter-if-all;?
            parameter-match-any;?
            parameter-match-all;?
            parameter-match-case-all;?
            parameter-match;
            parameter-match-case;
            parameter-modify-inputs;
            ))

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

(define (give-me-a-symbol ex)
  "Take a string or symbol EX and return a symbol."
  (cond ((symbol? ex) ex)
        ((string? ex) (string->symbol ex))
        (else (raise (formatted-message
                      (G_ "Not a symbol or a string: ~s")
                      ex)))))

(define-record-type* <parameter-type> parameter-type
  make-parameter-type
  parameter-type?
  this-parameter-type
  (name          parameter-type-name
                 (sanitize give-me-a-symbol))
  (accepted-values      parameter-type-accepted-values)
  (negation      parameter-type-negation
                 (default (car (parameter-type-accepted-values this-parameter-type)))
                 (thunked))
  (default       parameter-type-default
    (default (if (not (parameter-type-negation this-parameter-type))
                 (car (parameter-type-accepted-values this-parameter-type))
                 (cadr (parameter-type-accepted-values this-parameter-type))))
    (thunked))
  (description   parameter-type-description
                 (default "")))

(define boolean-parameter-type
  (parameter-type
   (name 'boolean)
   (accepted-values '(off on))
   (description "Boolean Parameter Type")))

;; Package parameter interface.
(define-record-type* <package-parameter> package-parameter
  make-package-parameter
  package-parameter?
  (name         package-parameter-name
                (sanitize give-me-a-symbol))
  (type         package-parameter-type
                (default boolean-parameter-type))
  (variants     package-parameter-variants
                (default '())
                (sanitize sanitize-parametric-variants))
  (dependencies package-parameter-dependencies
                (default '())
                (sanitize dependency-sanitizer)
                (thunked))
  (predicate    package-parameter-predicate
                (default #f))
  (description  package-parameter-description (default "")))

(define %global-parameters
  (alist->hash-table '()))

;; SANITIZERS

(define (sanitize-parametric-variants ls)
  "Raise an error if LS is not a list."
  (cond ((list? ls) ls)
        (else (raise (formatted-message
                      (G_ "Not a list: ~s")
                      ls)))))

;; % USEFUL HELPER FUNCTIONS %

(define (return-list lst)
  "Take a value LST, return LST if it a list and (list LST) otherwise."
  (if (list? lst)
      lst
      (list lst)))

(define (append-everything . things)
  "Take a number of THINGS, and append them all."
  (apply append
         (map return-list things)))

(define (get-parameter-sym psym)
  "If the argument is a cons cell, return the CAR otherwise return the argument."
  (match psym
    [(a . b) a]
    [a a]))

(define* (merge-same-car lst #:optional (carry '()))
  "Merge the cells of LST with the same value in their CAR."
  (define (assq-append alist key cont)
    (if (equal? (caar alist) key)
        (cons (cons key (append (cdar alist) cont))
              (cdr alist))
        (cons (car alist) (assq-append (cdr alist) key cont))))
  (cond ((null? lst) carry)
        ((null? (filter (lambda (y) (equal? (caar lst)
                                          (car y)))
                        carry))
         (merge-same-car (cdr lst) (cons (car lst) carry)))
        (else
         (merge-same-car (cdr lst)
                         (assq-append carry (caar lst) (cdar lst))))))

(define-syntax parameter-variant
  (syntax-rules ()
    [(%) '()]
    [(% psym variants ...)
     (let ((parsed-variants
            (parse-kw-list '(variants ...))))
       (map (cut cons <>
                 parsed-variants)
       (return-list 'psym)))]))

(define* (parse-kw-list kw-lst)
  "Parses a list of keywords, KW-LST and returns an alist."
  (define (list-till-kw lst)
    (receive (a b)
        (break keyword? lst)
      (cons a b)))
  (define* (break-keywords lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) '())
          ((keyword? (car lst))
           (let ((next-lst (list-till-kw (cdr lst))))
             (cons (cons (keyword->symbol (car lst))
                         (car next-lst))
                   (break-keywords (cdr next-lst)))))
          (else (raise (formatted-message
                      (G_ "Error trying to break keywords at ~s")
                      lst)))))
  (merge-same-car (break-keywords kw-lst)))

;; The lock here is used to signal when merge-same-car is to be used
;; having a :lock means merge-same-car has been used further up the tree
;; note that :lock is not a keyword but a symbol, as we are using keywords elsewhere
(define-syntax parameter-variant-match
  (syntax-rules (:lock)
    ((% :lock (x ...))
     (return-list
      (parameter-variant x ...)))
    ((% :lock (x ...) rest ...)
     (append
      (return-list (parameter-variant x ...))
      (parameter-variant-match :lock rest ...)))
    ((% rest ...)
     (map
      (lambda (ls) (cons (car ls)
                    (merge-same-car (cdr ls))))
      (merge-same-car
       (parameter-variant-match :lock rest ...))))))

(define (local-sanitizer ls)
  "Sanitize a list of local parameters, LS."
  (if (list? ls)
      (map (lambda (val)
             (cond ((package-parameter? val) val)
                   ((symbol? val) (package-parameter (name val)))
                   ((string? val) (package-parameter (name (string->symbol val))))
                   (else (raise (formatted-message
                                 (G_ "Not a parameter, symbol or string: ~s")
                                 val)))))
           ls)
      (raise (formatted-message
                      (G_ "Spec's local field is not a list: ~s")
                      ls))))

(define* (variant-sanitizer lv)
  "Sanitize a list of variants."
  ;; #:yes -> use default variant
  ;; #:no -> don't use variant
  ;; #:special -> use variant in cdr
  (define (sym->parameter psym)
    "Take a symbol PSYM and return the corresponding parameter."
    (or (find (lambda (g) (eqv? psym
                           (package-parameter-name g)))
              lv)
        (hash-ref %global-parameters psym)
        (raise (formatted-message
                                 (G_ "sym->parameter: not a symbol: ~s")
                                 psym))))
  (define-macro (assq-ov! asslst key val)
    `(set! ,asslst
       (assq-set! ,asslst ,key ,val)))
  (lambda (ls)
    (let ((triad (parse-kw-list ls)))
      (if (find (lambda (g) (not (or (eqv? (car g) 'yes)
                                (eqv? (car g) 'no)
                                (eqv? (car g) 'special))))
                triad)
          (raise (formatted-message
                      (G_ "Invalid keyword in use-variants: ~s")
                      (car g))))
      (let ((vars-lst '()))
        (map
         (match-lambda
           [('yes rest ...)
            (map
             (lambda (p)
               (if (not (symbol? p))
                   (raise (formatted-message
                      (G_ "Not a symbol: ~s")
                      p))
                   (assq-ov! vars-lst p #:yes)))
             rest)]
           [('no rest ...)
            (map
             (lambda (p)
               (if (not (symbol? p))
                   (raise (formatted-message
                      (G_ "Not a symbol: ~s")
                      p))
                   (assq-ov! vars-lst p #:no)))
             rest)]
           [('special rest ...)
            (map
             (lambda (x)
               (assq-ov! vars-lst
                         (car x)
                         (cdr x)))
             rest)]
           [_ (error "wrongly formatted use-variant!")])
         triad)
        (map
         (lambda (x)
           (match (assq-ref vars-lst (package-parameter-name x))
             [#f (assq-ov! vars-lst
                            (package-parameter-name x)
                            (package-parameter-variants x))]
             [#:yes (assq-ov! vars-lst
                            (package-parameter-name x)
                            (package-parameter-variants x))]
             [#:no #f] ; do nothing
             [varn (assq-ov! vars-lst
                            (package-parameter-name x)
                            varn)]))
         lv)
        vars-lst))))

(define (dependency-sanitizer deps)
  "Sanitize the dependency-list of a package-parameter."
  (unless (eqv? deps '())
    (if (not (list? deps))
        (raise (formatted-message
                (G_ "Dependencies not a list: ~s")
                deps)))
    (if (keyword? (car deps))
      (if (match (car deps)
                 [#:package #t]
                 [#:parameter #t]
                 [_ #f])
             (parse-kw-list deps)
        (raise (formatted-message
                (G_ "Bad dependency keyword: ~s")
                (car deps))))
      (dependency-sanitizer (cons #:parameter deps)))))

(define-record-type* <parameter-spec> parameter-spec
  make-parameter-spec
  parameter-spec?
  this-parameter-spec
  (local    parameter-spec-local
    (default '())
    (sanitize local-sanitizer)
    (thunked))
  (defaults parameter-spec-defaults
    (default '())
    (thunked))
  (required parameter-spec-required
            (default '())
            (thunked))
  (optional parameter-spec-optional
            (default '())
            (thunked))
  (one-of parameter-spec-one-of
          (default '())
          (thunked))
  (combinations-with-substitutes
   parameter-spec-combinations-with-substitutes
             (default parameter-spec-defaults)
             (thunked))
  (use-variants parameter-spec-use-variants
                  (default '())
                  (sanitize (variant-sanitizer
                              (parameter-spec-local this-parameter-spec)))
                  (thunked))
  (parameter-alist parameter-spec-parameter-alist
                   (default (base-parameter-alist this-parameter-spec))
                   (thunked)))

(define-syntax parameter-spec-property
  (syntax-rules ()
    [(parameter-spec-property body ...)
     (cons 'parameter-spec
           (parameter-spec body ...))]))

;; this fn will be applied to applicable variants
  ;; varlst -> [(<psym cons> . options) (<psym cons> . options) ...]
    ;; inner function
    ;; PKG: package record
    ;; VARS: [(psym val) (OPTION . (option args) ...) (OPTION-2 ...) ...]
(define (apply-variants pkg vars)
  "Apply a list of variants, VARS to the given package PKG."
  (define (exact-sub v)
    (if (list? v)
        (map exact-sub v)
        (match v
          [#:package-name
           (package-name pkg)]
          [#:package
           pkg]
          [#:parameter-value
           (cdar vars)]
          [x x])))
  ;; substitute keywords - transforms
  (define* (sub-kw-t in #:optional (ret '()))
    (if (null? in)
        (match (reverse ret)
               [(a . rest)
                (cons a (string-join rest "="))])
        (sub-kw-t
         (cdr in)
         (cons
          (exact-sub (car in))
          ret))))
  ;; substitute keywords
  (define* (sub-kw in #:optional (ret '()))
    (if (null? in)
        (reverse ret)
        (sub-kw
         (cdr in)
         (cons
          (exact-sub (car in))
          ret))))

  (cond [(null? (cdr vars))
         pkg] ; ((psym val))
        [(null? (cdadr vars)) ; ((psym val) (option))
         (apply-variants pkg (cons (car vars) (cddr vars)))]
        [#t
         (match (caadr vars) ; ((psym . val) . (<option> optargs) ...)
           ('build-system
            ;; halt execution if it does not match
            (if
             (member (package-build-system the-package)
                     (cdadr vars)) ; will be a list of build systems
             (apply-variants pkg (cons (car vars)
                                       (cddr vars)))
             pkg))
           ('transform
            (apply-variants
             ((options->transformation
              (map sub-kw-t (return-list (cdadr vars))))
              pkg)
             (cons (car vars)
                   (cddr vars))))
           ('lambda
               (apply-variants
                ;; eval should normally be avoided
                ;; but `lambda` as is defined evaluates
                ;; code after substituting in keywords
                (primitive-eval (sub-kw (cadadr vars)))
                (cons (car vars)
                      (cddr vars)))))]))

(define-syntax package-with-parameters
  (syntax-rules ()
    [(% spec body ...)
     (let* [(the-package-0 (package body ...))
           (the-package (package
                         (inherit the-package-0)
                         (properties
                           (cons (cons 'parameter-spec
                                     spec)
                                 (package-properties the-package-0)))))]
       (parameterize-package the-package
                             (parameter-spec-parameter-alist spec)
                             #:force-parameterization? #t))]))

(define* (parameterize-package the-initial-package
                               the-initial-list
                               #:key (force-parameterization? #f))
  "Evaluates THE-INITIAL-PACKAGE with the parameter-list THE-INITIAL-LIST."
  (define-macro (assq-ov! asslst key val)
    `(set! ,asslst
       (assq-set! ,asslst ,key ,val)))

  (define smoothen
    (match-lambda
      [(a . #:off)
       (cons a
             (parameter-type-negation
              (package-parameter-type (parameter-spec-get-parameter spec a))))]
      [(a . #:default)
       (cons a
             (parameter-type-default
              (package-parameter-type (parameter-spec-get-parameter spec a))))]
      [cell cell]))

  (let* [(the-initial-spec 
          (package-parameter-spec the-initial-package))
         (the-original-parameter-list
          (package-parameter-alist the-initial-package))
         (the-parameter-list
          (package-resolve-parameter-list the-initial-package
                                          the-initial-list))]
    ;; exit and return the same package if no impactful changes
    ;; XXX: make it more sophisticated, only measure parameters that change things
    (if (and (not force-parameterization?)
             (null? (filter (lambda (x)
                              (not (eqv? (assq-ref the-original-parameter-list
                                                   (car x))
                                         (cdr x))))
                            the-parameter-list)))
             the-initial-package
        (let* [(the-spec ; this value gets called very often
                (parameter-spec
                 (inherit the-initial-spec)
                 (parameter-alist
                  the-parameter-list)))
               (the-package
                (package
                  (inherit the-initial-package)
                  (properties (assq-set! (package-properties the-initial-package)
                                         'parameter-spec
                                         the-spec))))
               (the-variants
                ;; first get list of normal variants (local, etc)
                ;; then match over use-variants
                ;; if cdr #:yes, check the-parameter-list for val
                ;; if cdr #:no, purge from prev list
                ;; if cdr #:special, /replace/ value
                (let ((var-lst (parameter-spec-use-variants the-spec)))
                  (map (lambda (x)
                         (set! var-lst
                           (assq-set! var-lst
                                      (car x)
                                      (package-parameter-variants
                                       (parameter-spec-get-parameter the-spec (car x))))))
                       (filter (lambda (x)
                                 (match (package-parameter-predicate
                                         (parameter-spec-get-parameter
                                          the-spec
                                          (car x)))
                                   [#f #f]
                                   [#t #t]
                                   [fn (fn the-package)]))
                               (filter
                                (lambda (x)
                                  (not (assq-ref var-lst (car x)))) ; not in the variant-lst?
                                the-parameter-list)))
                  (map
                   (lambda (x)
                     (match (cdr x)
                       [#:yes (assq-ov! var-lst
                                        (car x)
                                        (package-parameter-variants
                                         (parameter-spec-get-parameter the-spec (car x))))]
                       [#:no (set! var-lst
                               (assq-remove! var-lst
                                             (car x)))]
                       [_ #f]))
                   var-lst)

                  var-lst))
               (applicable-variants
                (map (lambda (y)
                       (cons (cons (car y)
                                   (assq-ref the-parameter-list (car y)))
                             (apply append
                                    (map (lambda (x)
                                           (return-list (cdr x)))
                                         (cdr y)))))
                     ;; does it have values?
                     (filter (lambda (x) (not (null? (cdr x))))
                             (map ;; get list of applicable values
                              (lambda (x)
                                (let ((absv (assq-ref the-parameter-list (car x)))
                                      ;; if absv is -ve, only -ve values allowed
                                      ;; if absv is +ve, only +ve and _ allowed
                                      (negv (parameter-type-negation
                                             (package-parameter-type
                                              (parameter-spec-get-parameter the-spec (car x)))))
                                      (defv (parameter-type-default
                                             (package-parameter-type
                                              (parameter-spec-get-parameter the-spec (car x))))))
                                  (cons (car x)
                                        (filter
                                         (lambda (ls)
                                           (match (car ls)
                                             ['_ (not (eqv? absv negv))]
                                             [#:off (eqv? absv negv)]
                                             [#:default (eqv? absv defv)]
                                             [oth (eqv? absv oth)]))
                                         (cdr x)))))
                              (filter (lambda (x) assq-ref the-parameter-list (car x))
                                      the-variants)))))]
          (fold (lambda (vlst pack)
                  (apply-variants pack vlst))
                the-package
                applicable-variants)))))

(define (package-parameter-spec package)
  "Takes a package PACKAGE and returns its parameter-spec."
  (or (assq-ref (package-properties package) 'parameter-spec)
      (parameter-spec))) ; returns empty spec

(define (package-parameter-alist package)
  "Takes a package PACKAGE and returns its parameter-list."
  (parameter-spec-parameter-alist
   (package-parameter-spec package)))

;;; PROCESSING PIPELINE

;; Convention:
;;   Works on Parameters? -> parameter-spec/fun
;;   Works on Parameter-Spec? -> parameter-spec/fun
(define (parameter-spec-get-parameter pspec pcons)
  "Takes a parameter cell PCONS and returns the corresponding package-parameter."
  (let ((psym (get-parameter-sym pcons)))
  (or (find (lambda (x)
               (eqv? psym
                     (package-parameter-name x)))
             (parameter-spec-local pspec))
      (hash-ref %global-parameters psym)
      (raise (formatted-message
                (G_ "Parameter not found: ~s")
                psym)))))

(define (parameter-spec-negation-supported? pspec x)
  "Is negation supported for the given parameter X?"
  (let ((negv
         (parameter-type-negation (package-parameter-type (parameter-spec-get-parameter pspec x)))))
    (if negv
        negv
        '_)))

(define (get-spec-deps pspec psym)
  "Get the dependencies of the corresponding parameter to a given parameter symbol, PSYM."
  (let ([p (parameter-spec-get-parameter pspec psym)])
    (return-list
     (assq-ref (package-parameter-dependencies p)
               'parameter))))

;; 1. Fetching

(define (base-parameter-alist pspec) ; returns base case
  "Returns the BASE-PARAMETER-ALIST for a given parameter-spec PSPEC."
  ;; '((a . psym) (b . #f) ...)
  (let* ((v1 (parameter-process-list ; returns funneled list
              (append-everything
               (parameter-spec-defaults pspec)
               (parameter-spec-required pspec))))
         (v2 (parameter-process-list
              (append-everything
               (apply append
                      ;; XXX: change to a filter-map
                      (filter (cut car <>)
                              (map (cut get-spec-deps pspec <>)
                                   (return-list v1))))
               v1))))
    ;; funnel will signal duplication err
    ;; check if base case is valid
    (parameter-spec-validate pspec v2)
    v2))

;; 2. Processing

;; IMPORTANT CHANGE: Symbolic Negation no longer supported (psym!)
(define (parameter-process-list lst)
  "Processes and formats a list of parameters, LST."
  (define (return-cell p)
    (match p
      [(a b) (cons a b)]
      [(a . b) p]
      [a (cons a '_)]))
  (define (funnel plst)
    ;; first we will get a list indexed by keys
    (define (group-val carry lst)
      (if (null-list? lst)
          carry
          (let ((v (assq-ref carry (caar lst))))
            (group-val
             (assq-set! carry (caar lst)
                        (if v
                            (cons (cdar lst) v)
                            ;; We want a list in cdr
                            (cons (cdar lst) '())))
             (cdr lst)))))
    (define (figure-out psym p)
      (or (and (< (length p) 3)
               (or (and (eq? (length p) 1) (car p))
                   (and (member '_ p)
                        (car (delq '_ p)))))
          (raise (formatted-message
                (G_ "Too many values for a single parameter: ~s with ~s")
                psym p))))
    (map (lambda (x) (cons (car x)
                      (figure-out (car x) ; for the error message
                       (delete-duplicates (cdr x)))))
         (group-val '() plst)))
  (funnel (map
               return-cell
               lst)))

;; 3. Overriding

(define (all-spec-parameters pspec) ; for the UI
  "Returns all the parameters in a parameter-spec, PSPEC."
  ;; '(sym-a sym-b ...)
  (delete-duplicates
   (map get-parameter-sym ; we do not care about the values
        (append-everything ; works same as before
         (map package-parameter-name
              (parameter-spec-local pspec))
         (parameter-spec-defaults pspec)
         (parameter-spec-required pspec)
         ;; We are NOT pulling dependencies at this phase
         ;; They will not be influenced by the user parameter alist
         (filter (lambda (x) (not (eqv? x '_)))
                 (apply append (parameter-spec-one-of pspec)))
         (parameter-spec-optional pspec)))))

;; Now we compare it against the PLIST
;; NOTE: This is the only instance where GLOBAL PARAMETERS may be used
;;       Since referring to the package is not possible, we pass it instead of pspec
(define (package-override-plist pkg plist)
  "Takes a package PKG and parameter-list PLIST and overrides PLIST according to the package."
  (let* ((pspec (package-parameter-spec pkg))
         (all-p (all-spec-parameters pspec))
         (filtered-plist (filter (lambda (x) (or (member (car x) all-p)
                                            (and (hash-ref %global-parameters (car x))
                                                 ((package-parameter-predicate
                                                   (hash-ref %global-parameters (car x)))
                                                  pkg))))
                                 (parameter-process-list plist)))
         (filtered-car (map car filtered-plist))
         (remaining-p (filter (lambda (x) (not (member x filtered-car)))
                              all-p)))
    (append-everything filtered-plist
                       (map (lambda (x) (if (parameter-spec-negation-supported? pspec x)
                                       (cons x #:off)
                                       (cons x '_)))
                            remaining-p))))

;; 4. Funneling

(define (override-spec-multi-match pspec plst)
  "Overrides various keyword values in the parameter-list PLST."
  (map
    (match-lambda
      [(a . '_)
       (cons a
             (cadr (parameter-type-accepted-values (package-parameter-type (parameter-spec-get-parameter pspec a)))))]
      [(a . #:off)
       (cons a
             (parameter-type-negation (package-parameter-type (parameter-spec-get-parameter pspec a))))]
      [(a . #:default)
       (cons a
             (parameter-type-default (package-parameter-type (parameter-spec-get-parameter pspec a))))]
      [cell cell])
    plst))

;; 5. Validation

(define (parameter-spec-validate pspec plst)
  "Validates a parameter-list PLST against the parameter-spec PSPEC."
  (define (process-multi-list lst)
    (apply append
           (map (lambda (x)
                  (parameter-process-list (list x)))
                (filter (lambda (x) (not (eqv? x '_)))
                        lst))))

  ;; We want all tests to run
  (let ((works? #t))

    (define (m+eqv? new-val orig-val)
      (or (and (eqv? orig-val '_)
               (not (eqv? new-val #:off)))
          (eqv? orig-val new-val)))

    (define (throw+f sym vals)
      (raise (formatted-message
              (G_ "Parameter Validation Error: ~a with values ~s~%")
              sym vals))
      (set! works? #f))

    ;; first we check duplication
    ;; a bit unnecessary
    (define (validate/duplication)
      (let ((symlst (map car plst)))
        (unless (eqv? symlst (delete-duplicates symlst))
          (throw+f "Duplicates" plst))))

    ;; logic checking checks for:
    ;;   - presence of required parameters
    ;;   - 'one-of' conflicts
    ;;   - dependency satisfaction
    (define (validate/logic)
      (map ; required
       (lambda (x)
         (unless
             (let ((new-val (assq-ref plst (car x))))
               (m+eqv? (if (eqv?
                             new-val
                             (parameter-spec-negation-supported?
                               pspec
                               (car x)))
                         #:off new-val)
                       (cdr x)))
           (throw+f "Unsatisfied Requirements" x)))
       (parameter-process-list ; cannot have duplicates here!
        (parameter-spec-required pspec)))
      (map ; one-of
       (lambda (ls)
         (unless
             (let ((satisfied (count
                                (lambda (x)
                                  (let ((new-val (assq-ref plst (car x))))
                                    (m+eqv?
                                      (if
                                        (eqv? new-val
                                              (parameter-spec-negation-supported?
                                                pspec
                                                (car x)))
                                        #:off new-val)
                                      (cdr x))))
                                (process-multi-list ls)))) ; duplicates could happen!
               (or (= satisfied 1)
                   (and (= satisfied 0)
                        (eqv? (car ls) '_))))
           (throw+f "Unsatisfied One-Of" ls)))
       (parameter-spec-one-of pspec))

      (unless
        (null?
          (let ((symlst (map car plst)))
          (filter (lambda (x)
                    (let ((deps (package-parameter-dependencies
                                  (parameter-spec-get-parameter pspec
                                                                x))))
                      (not (assq-ref deps 'package))))
                  symlst)))
        (warning
          (G_ "Package Dependencies are not supported!~%")))

      (unless (not (member #f
                      (return-list
                       (map (lambda (x)
                              (let ((deps (package-parameter-dependencies
                                           (parameter-spec-get-parameter pspec x))))
                                (if deps
                                    (not
                                      (member
                                        #f
                                        (map
                                          (lambda (dep)
                                            ;; 0. restructure d to a proper cell
                                            (let ((d (car
                                                       (parameter-process-list
                                                         (return-list dep)))))
                                              ;; 1. assq-ref
                                              (m+eqv?
                                                (assq-ref plst (car d))
                                                (cdr d))))
                                          (return-list
                                            ;;; XXX: check for packages
                                            ;; not doable in the current state as the validator
                                            ;; does not take the entire package as an argument
                                            ;; the validator will have to be heavily modified
                                            (assq-ref deps 'parameter)))))
                                    #t)))
                            ;; filter to check if parameter is not its negation
                            (filter (lambda (x)
                                      (not (eqv? (cdr x)
                                                 (parameter-spec-negation-supported?
                                                  pspec
                                                  (car x)))))
                                    plst)))))
                      (throw+f "Bad dependencies!" plst)))

    (validate/duplication)

    (validate/logic)

    works?))

;; need pkg instead of pspec for override-spec
(define (package-resolve-parameter-list pkg plst)
  "Resolves a parameter-list PLST against the package PKG."
  (let* ([pspec (package-parameter-spec pkg)]
         [proper-plst (override-spec-multi-match
                      pspec
                      (package-override-plist
                       pkg
                       (parameter-process-list plst)))])
    (if (parameter-spec-validate pspec proper-plst)
        proper-plst
        (base-parameter-alist pspec))))

;; %global-parameters: hash table containing global parameters ref'd by syms

(define-syntax define-global-parameter
  (syntax-rules ()
    [(define-global-parameter parameter-definition)
     (let ((gp-val parameter-definition))
       (hash-set! %global-parameters
                  (package-parameter-name gp-val)
                  gp-val))]))

(define-syntax parameter-inside?
  (syntax-rules ()
    [(% p plst)
     (not
      (eqv? (or (assq-ref plst p)
                (error "Parameter not found!"))
            (parameter-type-negation
             (package-parameter-type
              (parameter-spec-get-parameter
               (package-parameter-spec this-package)
               p)))))]))

(define-syntax parameter-if
  (syntax-rules ()
    [(parameter-if property exp)
     (let ((properties
            (parameter-spec-parameter-alist
             (package-parameter-spec this-package))))
       (if (member
            #t
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property))))
           exp
           '()))]
    [(parameter-if property exp exp-else)
     (let ((properties
            (parameter-spec-parameter-alist
             (package-parameter-spec this-package))))
       (if (member
            #t
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property))))
           exp
           exp-else))]))

(define-syntax parameter-if-all
  (syntax-rules ()
    [(parameter-if-all property exp)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (not (member
                     #f
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property)))))
           exp
           '()))]
    [(parameter-if-all property exp exp-else)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (not (member
                     #f
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property)))))
           exp
           exp-else))]))

(define-syntax parameter-match-any
  (syntax-rules (_)
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter-match-any rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (cut parameter-inside? <> properties)
                              (list parameters ...)))
              (begin clauses ...))
         (parameter-match-any rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (parameter-inside? parameter properties)
              (begin clauses ...))
         (parameter-match-any rest ...)))]))

(define-syntax parameter-match-all
  (syntax-rules (_)
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter-match-all rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (cut parameter-inside? <> properties)
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter-match-all rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (parameter-inside? parameter properties)
              (begin clauses ...))
         (parameter-match-all rest ...)))]))

(define-syntax parameter-match-case-all
  (syntax-rules ()
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter-match-case-any rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (and (not (member #f (map (cut parameter-inside? <> properties)
                                 (list parameters ...))))
            (begin clauses ...)
            (parameter-match-case-any rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (and (parameter-inside? parameter properties)
            (begin clauses ...)
            (parameter-match-case-any rest ...)))]))

(define-syntax parameter-match
  (syntax-rules (_ all)
    [(%) '()]
    [(% (_ clauses ...) rest ...) (begin (begin clauses ...) (parameter-match rest ...))]
    [(% (parameters) rest ...) (parameter-match rest ...)]
    [(% ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (cut parameter-inside? <> properties)
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter-match rest ...)))]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (cut parameter-inside? <> properties)
                              (list parameters ...)))
              (begin clauses ...))
         (parameter-match rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (parameter-inside? parameter properties)
              (begin clauses ...))
         (parameter-match rest ...)))]))

(define-syntax parameter-match-case
  (syntax-rules (all _)
    [(%) '()]
    [(% (_ clauses ...) rest ...) (begin clauses ...)]
    [(% (parameters) rest ...) (parameter-match-case rest ...)]
    [(% ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (not (member #f (map (cut parameter-inside? <> properties)
                                (list parameters ...))))
           (begin clauses ...)
           (parameter-match-case rest ...)))]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (member #t (map (cut parameter-inside? <> properties)
                           (list parameters ...)))
           (begin clauses ...)
           (parameter-match-case rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (parameter-inside? parameter properties)
           (begin clauses ...)
           (parameter-match-case rest ...)))]))

(define-syntax parameter-modifier-if
  (syntax-rules (_ all delete prepend append replace)
    [(% _ exp exp2)
     exp]
    [(% (all parameters ...) exp exp2)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (member #t
                   (map (cut parameter-inside? <> properties)
                        (list parameters ...)))
           exp
           exp2))]
    [(% parameter exp exp2)
     (let ((properties (parameter-spec-parameter-alist (package-parameter-spec this-package))))
       (if (member
                #t
                (map (cut parameter-inside? <> properties)
                     (return-list parameter)))
           exp
           exp2))]))

(define-syntax parameter-modify-inputs
  (syntax-rules (_ all delete prepend append replace)
    [(% inputs (parameter) clauses ...)
     (parameter-modify-inputs inputs clauses ...)]
    [(% inputs (parameter (delete name) rest ...) clauses ...)
     (parameter-modify-inputs
      (parameter-modifier-if
       parameter
       (alist-delete name inputs)
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (delete names ...) rest ...) clauses ...)
     (parameter-modify-inputs
      (parameter-modifier-if
       parameter
       (fold alist-delete inputs (list names ...))
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (prepend lst ...) rest ...) clauses ...)
     (parameter-modify-inputs
      (parameter-modifier-if
       parameter
       (append (map add-input-label (list lst ...)) inputs)
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (append lst ...) rest ...) clauses ...)
     (parameter-modify-inputs
      (parameter-modifier-if
       parameter
       (append inputs (map add-input-label (list lst ...)))
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs (parameter (replace name replacement) rest ...) clauses ...)
     (parameter-modify-inputs
      (parameter-modifier-if
       parameter
       (replace-input name replacement inputs)
       inputs)
      (parameter rest ...)
      clauses ...)]
    [(% inputs)
     inputs]))

