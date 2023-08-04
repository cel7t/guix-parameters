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
  #:export (package-parameter
            parameter-type
            parameter-spec
            boolean-parameter-type

            parameter-variant
            parameter-variant-match
            parameter-spec-property
            package-parameter-spec
            all-spec-parameters
            base-parameter-alist
            parameter-spec-override-plist
            parameter-spec-validate
            spec-resolve-list
            %global-parameters
            define-global-parameter

            package-with-parameters
            parameter-spec-parameter-alist
            parameter-if
            parameter-if-all
            parameter-match-any
            parameter-match-all
            parameter-match-case-any
            parameter-match
            parameter-match-case
            parameter-modify-inputs))

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
  (cond ((symbol? ex) ex)
        ((string? ex) (string->symbol ex))
        (else (throw 'bad! ex))))

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
  (dependencies package-parameter-dependencies ; 7/14
                (default '())
                (thunked))
  (description  package-parameter-description (default "")))

;; TODO: Find a cleaner way to manage global parameters
(define %global-parameters
  (alist->hash-table '()))

;; SANITIZERS

;; (define (sanitize-package-parameter-name x)
;;   (cond ((string? x)
;;          (if (string= (string-take-right x 1) "!")
;;              (throw "Negation in parameter name!" x) ; we cannot have negation in parameter name!
;;              (string->symbol x)))
;;         ((symbol? x)
;;          (if (string= (string-take-right (symbol->string x) 1) "!")
;;              (throw "Negation in parameter name!" x) ; we cannot have negation in parameter name!
;;              x))
;;         (else (throw 'bad! x))))

;; (sanitize-package-parameter-name 'x!)

;; (define (sanitize-build-system-transforms ls)
(define (sanitize-parametric-variants ls)
  ;; ((a . t1 t2 ...) ((b c) t3 t4 ...))
  (cond ((list? ls) ls)
        (else (throw 'bad! ls))))

;; (define-syntax lots-of-cons->alist
;;   (syntax-rules ()
;;     ((_ (a . b))
;;      (list (cons 'a b)))
;;     ((_ (a . b) rest ...)
;;      (cons (cons 'a b)
;;            (lots-of-cons->alist rest ...)))))

;; (define-syntax build-system/transform
;;   (syntax-rules (-> _)
;;     ((build-system/transform (x ...) -> y ...)
;;      (map (lambda (g)
;;             (cons g (lots-of-cons->alist y ...)))
;;           (list x ...)))
;;     ((build-system/transform _ -> y ...) ; for local parameter definitions
;;      (cons 'any ; matches any build system
;;       (lots-of-cons->alist y ...)))
;;     ((build-system/transform x -> y ...)
;;      (cons x (lots-of-cons->alist y ...)))))

;; Parameter Variants:
;; (parameter-variant
;;  (sym + build-system -> morphism-list))

;; alist->hash-table of the format
;; ((build-system . ((sym . ((transforms (a . b) ...) ...)) ...)) ...)

;; % USEFUL HELPER FUNCTIONS %

(define (return-list lst)
  (if (list? lst)
      lst
      (list lst)))

(define (append-everything . things)
  (apply append
         (map return-list things)))

(define (get-parameter-sym psym)
  (match psym
    [(a . b) a]
    [a a]))

(define* (merge-same-car lst #:optional (carry '()))
  ;; Takes an ALIST and merges entries with the same CAR
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

;; (define-syntax parameter-variant
;;   (syntax-rules (-> + _)
;;     [(%) '()]
;;     [(% _ -> variants ...)
;;      (cons 'any (cons 'any (parameter/parse-variants '(variants ...))))]
;;     [(% _ + _ -> variants ...)
;;      (cons 'any (cons 'any (parameter/parse-variants '(variants ...))))]
;;     [(% sym + _ -> variants ...)
;;      (let ((parsed-variants (parameter/parse-variants '(variants ...))))
;;        (cons 'any (map (lambda (g)
;;                          (cons g parsed-variants))
;;                        (return-list 'sym))))]
;;     [(% _ + b-system -> variants ...)
;;      (let ((parsed-variants (parameter/parse-variants '(variants ...))))
;;        (map (lambda (g) (cons g (cons 'any parsed-variants)))
;;             (return-list 'b-system)))]
;;     [(% sym + b-system -> variants ...)
;;      (let ((parsed-variants (parameter/parse-variants '(variants ...))))
;;        (map (lambda (g) (cons g (map (lambda (h) (cons h parsed-variants))
;;                                      (return-list 'sym))))
;;             (return-list 'b-system)))]
;;     [(% sym -> variants ...)
;;      (let ((parsed-variants (parameter/parse-variants '(variants ...))))
;;        (cons 'any (map (lambda (g)
;;                          (cons g parsed-variants))
;;                        (return-list 'sym))))]))

;; (parameter-variant (! _ 3) + (a b c) -> #:transform m1 #:rewrite m2 m3 #:modify c3)

;; NEW SYNTAX
;; (parameter-morphism (param ...) #:_ _ #:_ _ ...)

;; BUILD-SYSTEM is just a part of CDR now

(define-syntax parameter-variant
  (syntax-rules ()
    [(%) '()]
    [(% psym variants ...)
     (let ((parsed-variants
            (parse-kw-list '(variants ...))))
       (map (cut cons <>
                 parsed-variants)
       (return-list 'psym)))]))

;; if we want to use break, (use-modules (srfi srfi-1) (ice-9 receive))
;;
;; (define (list-till-kw lst)
;;   (receive (a b)
;;       (break keyword? lst)
;;     (cons a b)))
;;
;; (list-till-kw '(a b #:c d e))

;; (define* (list-till-kw lst #:optional (carry '()))
;;     (cond ((null? lst) (cons (reverse carry) '()))
;;           ((and (not (null? (cdr lst)))
;;                 (keyword? (car lst)))
;;            (cons (reverse carry) lst))
;;           (else (list-till-kw (cdr lst) (cons (car lst) carry)))))

(define* (parse-kw-list kw-lst)
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
          (else (throw 'bad! lst))))
  (merge-same-car (break-keywords kw-lst)))

;; (define-syntax build-system/transform-match
;;   (syntax-rules ()
;;     ((_ (x ...))
;;      (list
;;       (build-system/transform x ...)))
;;     ((_ (x ...) rest ...)
;;      (cons
;;       (build-system/transform x ...)
;;       (build-system/transform-match rest ...)))))


;; (parse-kw-list '(#:transform a (b c) #:rewrite d #:transform h))

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

;; (use-modules (ice-9 pretty-print))
;; (pretty-print
;; (parameter-variant-match
;;  ((a b c) #:build-system (d e f) #:transform (x _) y #:rewrite z)
;;  (g #:transform u)))

(define (local-sanitizer ls)
  (if (list? ls)
      (map (lambda (val)
             (cond ((package-parameter? val) val)
                   ((symbol? val) (package-parameter (name val)))
                   ((string? val) (package-parameter (name (string->symbol val))))
                   (else (throw 'bad! val))))
           ls)
      (throw 'bad! ls)))

;; (use-modules (ice-9 match))
;; (define (morphism-sanitizer lv) ; ((a^ m) ((b sym) m2) c ((d sym1 sym2 ...) m3) ...)
;;   (define (default-morphism? psym) ; check if parameter is given as parameter^
;;     ;; TAKE SPECIAL CARE:
;;     ;;   As we are treating ^ as a special character,
;;     ;;   it will trim it away from the parameter symbol.
;;     ;;   DO NOT USE IT AT THE END OF THE PARAMETER!
;;     (or (and (string=? (string-take-right (symbol->string psym) 1) "^")
;;              (string->symbol (string-drop-right (symbol->string psym) 1)))
;;         (and (string=? (string-take-right (symbol->string psym) 2) "^!")
;;              (string->symbol (string-append (string-drop-right (symbol->string psym) 2)
;;                                             "!")))))
;;   (define (default-morphism-list psym)
;;     (or (find (lambda (g) (eqv? psym
;;                                 (package-parameter-name g)))
;;               lv)
;;         (hash-ref %global-parameters psym)
;;         (throw 'bad! psym)))
;;   (lambda (ls)
;;     (map
;;      (match-lambda
;;        [psym
;;         ;; default morphism for psym
;;         (list
;;          (cons psym
;;                (default-morphism-list psym)))]
;;        [((psym vals ...) m)
;;         ;; assign morphism to psym at vals
;;         (let ((variants (if (keyword? (car m))
;;                              (parameter/parse-variants m)
;;                              m)))
;;           (map (lambda (x) (cons x variants))
;;                (return-list vals)))]
;;        [((? default-morphism? psym) sym)
;;         ;; get default morphism at sym
;;         (let ((csym (default-morphism? psym)))
;;           (list
;;            (cons (cons csym sym)
;;                  (default-morphism-list csym))))]
;;        [(psym m)
;;         ;; morphism for psym
;;         (let ((variants (if (keyword? (car m))
;;                              (parameter/parse-variants m)
;;                              m)))
;;           (list
;;            (cons psym variants)))]
;;        [x
;;         (throw 'bad! x)])
;;      ls)))

(define* (variant-sanitizer lv)
  ;; #:yes -> use default variant
  ;; #:no -> don't use variant
  ;; #:special -> use variant in cdr
  (define (sym->parameter psym)
    (or (find (lambda (g) (eqv? psym
                           (package-parameter-name g)))
              lv)
        (hash-ref %global-parameters psym)
        (throw 'bad! psym)))
  (lambda (ls)
    (let ((triad (parse-kw-list ls)))
      (if (find (lambda (g) (not (or (eqv? (car g) 'yes)
                                (eqv? (car g) 'no)
                                (eqv? (car g) 'special))))
                triad)
          (error "invalid keyword in use-variant"))
      (let ((local-no '())
            (variant-lst '()))
        (set! variant-lst
          (apply append
                 (map
                  (match-lambda
                    [(#:yes rest ...)
                     (return-list
                      (map (lambda (y)
                             (cons y
                                   (package-parameter-variants
                                    (sym->parameter y))))
                           rest))]
                    [(#:no rest ...)
                     (and (set! local-no
                            (append
                             (return-list
                              (filter (lambda (g)
                                        (member (package-parameter-name g)
                                                rest))
                                      lv))
                             local-no))
                          '())]
                    [(#:special rest ...)
                     (return-list
                      (map
                       (lambda (x)
                         (cons (car x)
                               (sanitize-parameteric-variants (cdr x))))
                       rest))]
                    [_ (error "wrongly formatted use-variant!")])
                  triad)))
        (append variant-lst
                (return-list
                 (map (lambda (z)
                        (cons (package-parameter-name z)
                              (package-parameter-variants z)))
                      (filter (lambda (g)
                                (not (member g
                                             local-no)))
                              lv))))))))

;; parameter-dependency
;; now a sanitizer
;; '(#:parameter a b ... #:package c d ...)
;; '(a b c) -> parameter

;; XXX: check for keyword validity i.e only #:package and #:parameter
;;      make list-of-list
(define (dependency-sanitizer deps)
  (if (not (list? deps)) (throw 'bad! deps))
  (if (keyword? (car deps))
      (parse-kw-list deps)
      (dependency-sanitizer (cons #:parameter deps))))

;; (define-syntax parameter/dependency
;;   (lambda (defn)
;;     (syntax-case defn (->)
;;       [(% p-lst -> rest ...)
;;        (syntax
;;         (let ((morphism-list (return-list '(rest ...))))
;;           (map
;;            (lambda (x)
;;              (cons x
;;                    (parameter/parse-variants (if (keyword? (car morphism-list))
;;                                                   morphism-list
;;                                                   (cons #:parameters morphism-list)))))
;;            (return-list 'p-lst))))])))
;;
;; ;; (parameter/dependency (a b) -> #:parameters a b #:packages d)
;; ;; (parameter/dependency (a (b yyy)) -> m n o)
;;
;; (define-syntax parameter/dependency-match
;;   (syntax-rules (:lock _ ->)
;;     ((% :lock (x ...))
;;      (parameter/dependency x ...))
;;     ((% :lock (x ...) rest ...)
;;      (append
;;       (parameter/dependency x ...)
;;       (parameter/dependency-match :lock rest ...)))
;;     ((% rest ...)
;;      (merge-same-car
;;       (parameter/dependency-match :lock rest ...)))))

;; (parameter/dependency-match
;;  (a -> k)
;;  ((a b) -> #:parameters a b #:packages d)
;;  ((a (b yyy)) -> m n o))

;; thunked -> we can do stuff like (parameter-spec-optional-parameters ps) to get the optional parameters
(define-record-type* <parameter-spec> parameter-spec
  make-parameter-spec
  parameter-spec?
  this-parameter-spec
  ;; local-parameters: parameters specific to the package
  (local    parameter-spec-local
    ;; keeping it as an alist as it will be useful to retrieve them for the UI
    (default '())
    (sanitize local-sanitizer) ; morphism-update: all good!
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
  (defaults parameter-spec-defaults ; '(a b c d ...) -> '(a (b sym) (c sym2) e! ...)
    (default '())
    (thunked))
  (required parameter-spec-required
            (default '())
            (thunked))
  (optional parameter-spec-optional
            (default '()) ; 6/16: causing problems with all-spec-parameters
            ;; 6/13: removed the sanitizer as merging local and optional
            ;;       should be handled by the parser instead.
            (thunked))
  ;; XXX: automatically create (x x!) if both are defined
  ;; 6/12: this will be handled by the parser
  (one-of parameter-spec-one-of
          (default '())
          (thunked))
  ;; add dependencies
  ;; (dependencies (parameter/dependencies
  ;;                   (a b -> d e f)
  ;;  	               (c -> g h)))
  (dependencies parameter-spec-dependencies
                (default '())
                (sanitize dependency-sanitizer)
                (thunked))
  ;; 7/14 : Moved to the package-parameter record
  (combinations-with-substitutes
   parameter-spec-combinations-with-substitutes
             (default parameter-spec-defaults)
             (thunked))
  ;; (use-transforms parameter-spec/use-transforms ;; only use transforms for these
  ;;                 (default '())
  ;;                 (sanitize (transform-sanitizer (parameter-spec/local this-parameter-spec)))
  ;;                 (thunked))
  (use-variants parameter-spec-use-variants ;; only use variants for these
                  (default '())
                  (sanitize (variant-sanitizer parameter-spec-local))
                  (thunked))
  (parameter-alist parameter-spec-parameter-alist ;; this is ultimately what will be transformed by --with-parameters
                   ;; '((a . #t) (b . #f) ...)
                   (default (base-parameter-alist this-parameter-spec)) ; if this doesn't work some tricks might be needed
                   (thunked)))

;; g23: Most parameters should be boolean
;; Might make sense to add a recursive type
;; (define boolean-parameter-type
;;   ;; The Boolean parameter type.
;;   (parameter-type (name 'boolean)
;;                   (accepted-values '(#t #f))
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

;; XXX: Redo with new update in mind
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

;;; PROCESSING PIPELINE

;; Convention:
;;   Works on Parameters? -> parameter-spec/fun
;;   Works on Parameter-Spec? -> parameter-spec/fun
(define (parameter-spec-get-parameter pspec psym)
  (or (find (lambda (x)
               (eqv? psym
                     (package-parameter-name x)))
             (parameter-spec-local pspec))
      (hash-ref %global-parameters psym)
      (throw "Parameter not found: " psym)))

(define (parameter-spec-negation-supported? pspec x)
  (let ((negv
         (parameter-type-negation (package-parameter-type (parameter-spec-get-parameter pspec x)))))
    (if negv
        negv
        '_)))

;; (define (get-spec-deps pspec lst)
;;   (apply
;;    append
;;    (map
;;     (lambda (x)
;;       (cons x
;;             (assq-ref
;;              (assq-ref (parameter-spec-dependencies pspec) x)
;;              'parameters)))
;;     lst)))
(define (get-spec-deps psym)
  (let ([p (parameter-spec-get-parameter pspec psym)])
    (return-list
     (assq-ref (package-parameter-dependencies p)
               'parameter))))

;; 1. Fetching

(define (base-parameter-alist pspec) ; returns base case
  ;; '((a . psym) (b . #f) ...)
  (let* ((v1 (parameter-process-list ; returns funneled list
              (append-everything
                    (parameter-spec-defaults pspec)
                    (parameter-spec-required pspec))))
         (v2 (parameter-process-list
              (append-everything
               (apply append
                      (map (cut get-spec-deps pspec <>)
                           (return-list v1)))
               v1))))
         ;; funnel will signal duplication err
    v2))

;; 2. Processing

;; IMPORTANT CHANGE: Symbolic Negation no longer supported (psym!)
(define (parameter-process-list lst)
  ;; (define (unexclaim p) ; step 1
  ;;   (define (negated-sym? p)
  ;;     (string=? (string-take-right (symbol->string (car (return-list p))) 1) "!"))
  ;;   (match p
  ;;     ;; Signal error if p! is in a cell
  ;;     [((? negated-sym? a) . b) (throw 'negation-in-cell! p)]
  ;;     [(a . b) p] ; normal cells are OK
  ;;     [(? negated-sym? a) (cons (string->symbol
  ;;                                (string-drop-right (symbol->string a) 1))
  ;;                               #:off)]
  ;;     [_ p]))
  (define (return-cell p) ; step 2 + 3
    (match p
      [(a b) (cons a b)]
      [(a . b) p]
      [a (cons a '_)]))
  ;; (define (desugarize p) ; step 4
  ;;   (match p
  ;;     ;; [(a . '_) (cons a '_)]
  ;;     ;; [(a . '!) (cons a #:off)]
  ;;     [_ p]))
  (define (funnel plst) ; step 5
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
    (define (figure-out p)
      (or (and (< (length p) 3)
               (or (and (eq? (length p) 1) (car p))
                   (and (member '_ p)
                        (car (delq '_ p)))))
          (throw 'too-many-elements! p)))
    (map (lambda (x) (cons (car x)
                      (figure-out
                       (delete-duplicates (cdr x)))))
         (group-val '() plst)))
  (funnel (map ;; (lambda (x) (desugarize (return-cell (unexclaim x))))
               return-cell
               lst)))

;; 3. Overriding

;; This will get us all the parameters
(define (all-spec-parameters pspec) ; for the UI
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
(define (parameter-spec-override-plist pspec plist)
  ;; (display "OVERRIDE")(newline)
  (let* ((all-p (all-spec-parameters pspec))
         (filtered-plist (filter (lambda (x) (member (car x) all-p))
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
  ;; (display "MULTIMATCH")(newline)
  (map
    (match-lambda
      [(a . '_) ;; TODO: iterate through these!
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
  ;; (display "VALIDATING: ") (display plst) (newline)
  (define (process-multi-list lst)
    (apply append
           (map (lambda (x)
                  ;; (display x) (display ": ")
                  ;; (display (parameter-process-list (list x))) (newline)
                  (parameter-process-list (list x)))
                (filter (lambda (x) ;; (display x) (newline)
                           (not (eqv? x '_)))
                        lst))))

  ;; We want all tests to run
  (let ((works? #t))

    (define (m+eqv? new-val orig-val)
      ;; (display "VALS: ") (display new-val)
      ;; (display " ") (display orig-val) (newline)
      (or (and (eqv? orig-val '_)
               (not (eqv? new-val #:off)))
          (eqv? orig-val new-val)))

    (define (throw+f sym vals)
      (display "Error: ")
      (display sym)
      (display " with values ")
      (display vals)
      (newline)
      (set! works? #f))

    ;; first we check duplication
    ;; a bit unnecessary
    (define (validate/duplication)
      (let ((symlst (map car plst)))
        (unless (eqv? symlst (delete-duplicates symlst))
          (throw+f 'duplicates plst))))

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
           (throw+f 'unsatisfied-requirement x)))
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
           (throw+f 'one-of-unsatisfied ls)))
       (parameter-spec-one-of pspec))
      ;; XXX: Needs a per-parameter rewrite
      ;; (map ; dependencies
      ;;  (lambda (x)
      ;;    (let ([deplst (parameter-process-list
      ;;                   (assq-ref
      ;;                    (assq-ref (parameter-spec-dependencies pspec) x)
      ;;                    'parameters))])
      ;;      (map
      ;;       (lambda (y)
      ;;         (unless (m+eqv? (assq-ref plst (car y))
      ;;                         (cdr y))
      ;;           (throw+f 'dependency-unsatisfied y)))
      ;;       deplst)))
      ;;  plst)
      )

    (validate/duplication)
    (validate/logic)
 ;;   (display "DOES IT WORK? ") (display works?) (newline)
    works?))

(define (spec-resolve-list pspec plst)
  (let ([proper-plst (override-spec-multi-match
                      pspec
                      (parameter-spec-override-plist
                       pspec
                       (parameter-process-list plst)))])
   ;; (display "TRIALS OVER?")(newline)
    (if (parameter-spec-validate pspec proper-plst)
        proper-plst
        (base-parameter-alist pspec))))

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
            (parameter-spec/parameter-alist
             (package-parameter-spec this-package))))
       (if (member
            #t
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property))))
           exp
           '()))]
    [(parameter-if property exp exp-else)
     (let ((properties
            (parameter-spec/parameter-alist
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
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (not (member
                     #f
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property)))))
           exp
           '()))]
    [(parameter-if-all property exp exp-else)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (not (member
                     #f
            (map (cut parameter-inside? <> properties)
                 (parameter-process-list (return-list property)))))
           exp
           exp-else))]))

;; Test these macros without using packages:
;; (define (parameter-spec/parameter-alist _)
;;   (list (cons 'a 1)
;;         (cons 'b 2)
;;         (cons 'c 3)))
;; (define (package-parameter-spec _) #t)
;; (define this-package '())

;; (parameter-if '(a (b 3))
;;       "YES"
;;       "NO")

;; (parameter-if-all '(a (b 3))
;;           "NO"
;;           "YES)

;; parameter-match-any:
;; (parameter-match-any
;; ((a b) e1 e2 ..)
;; ((c) d1 d2 ..)
;; (else c1 c2 ...))

(define-syntax parameter-match-any
  (syntax-rules (_)
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter-match-any rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (cut parameter-inside? <> properties)
                              (list parameters ...)))
              (begin clauses ...))
         (parameter-match-any rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (parameter-inside? parameter properties)
              (begin clauses ...))
         (parameter-match-any rest ...)))]))

;; (let ((SOME_ALIST_FOR_THIS_EXAMPLE '()))
;;   (parameter-match-any
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

(define-syntax parameter-match-all
  (syntax-rules (_)
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter-match-all rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (cut parameter-inside? <> properties)
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter-match-all rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (parameter-inside? parameter properties)
              (begin clauses ...))
         (parameter-match-all rest ...)))]))

;; (parameter-match-all
;;  (('a 'b) (display "YES") (display "YES"))
;;  (('c 'd) (display "NO"))
;;  (all (display "ALL")))

(define-syntax parameter-match-case-all
  (syntax-rules ()
    [(%) '()]
    [(% (_ clauses ...)) (begin clauses ...)]
    [(% ((parameters ...)) rest ...) (parameter-match-case-any rest ...)]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (and (not (member #f (map (cut parameter-inside? <> properties)
                                 (list parameters ...))))
            (begin clauses ...)
            (parameter-match-case-any rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (and (parameter-inside? parameter properties)
            (begin clauses ...)
            (parameter-match-case-any rest ...)))]))

;; should short-circuit at YESYES
;; (parameter-match-case
;;  (('a 'b 'e) (display "YES") (display "YES"))
;;  (('c 'd) (display "NO"))
;;  (all (display "ALL")))


;; parameter-match:
;; combine all and any into one
;; (parameter-match
;;  ((any a b) ...)
;;  ((all a b c) ...)
;;  (all ...))

(define-syntax parameter-match
  (syntax-rules (_ all)
    [(%) '()]
    [(% (_ clauses ...) rest ...) (begin (begin clauses ...) (parameter-match rest ...))]
    [(% (parameters) rest ...) (parameter-match rest ...)]
    [(% ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (cut parameter-inside? <> properties)
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter-match rest ...)))]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (cut parameter-inside? <> properties)
                              (list parameters ...)))
              (begin clauses ...))
         (parameter-match rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (parameter-inside? parameter properties)
              (begin clauses ...))
         (parameter-match rest ...)))]))

;; (parameter-match
;;  ((all 'a 'b) (display "YES"))
;;  (_ (display "YES"))
;;  (('c 'e) (display "YES"))
;;  ((all 'a 'o) (display "NO"))
;;  (_ (display "ALL")))

(define-syntax parameter-match-case
  (syntax-rules (all _)
    [(%) '()]
    [(% (_ clauses ...) rest ...) (begin clauses ...)]
    [(% (parameters) rest ...) (parameter-match-case rest ...)]
    [(% ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (not (member #f (map (cut parameter-inside? <> properties)
                                (list parameters ...))))
           (begin clauses ...)
           (parameter-match-case rest ...)))]
    [(% ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (member #t (map (cut parameter-inside? <> properties)
                           (list parameters ...)))
           (begin clauses ...)
           (parameter-match-case rest ...)))]
    [(% (parameter clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (parameter-inside? parameter properties)
           (begin clauses ...)
           (parameter-match-case rest ...)))]))

;; (parameter-match-case
;;  ((all 'a 'f) (display "NO"))
;;  ;; (all (display "YES"))
;;  ;; ((any 'c 'e) (display "YES"))
;;  ;; ((all 'a 'b) (display "YES"))
;;  (all (display "ALL")))

(define-syntax parameter-modifier-if
  (syntax-rules (_ all delete prepend append replace)
    [(% _ exp exp2)
     exp]
    [(% (all parameters ...) exp exp2)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (member #t
                   (map (cut parameter-inside? <> properties)
                        (list parameters ...)))
           exp
           exp2))]
  ;;  [(% (all parameter) exp exp2) ; unnecessary
  ;;   (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
  ;;     (if (parameter-inside? parameter properties)
  ;;         exp
  ;;         exp-else))]
    [(% parameter exp exp2)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
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

;; (parameter-type-negation
;;  (parameter-type
;;   (name "ok")
;;   (accepted-values '(not-ok ok))))

;; (define-syntax parameter/type
;;   (syntax-rules (_)
;;     [(% _ rest ...)
;;      (parameter/type (string-append (or (package-parameter-name this-package-parameter)
;;                                         "%blank")
;;                                     "-type")
;;                      rest ...)]
;;     [(% t-name t-accepted-values)
;;      (parameter-type
;;       (name t-name)
;;       (accepted-values t-accepted-values))]
;;     [(% t-name t-accepted-values t-negation)
;;      (parameter-type
;;       (name t-name)
;;       (accepted-values t-accepted-values)
;;       (negation t-negation))]
;;     [(% t-name t-accepted-values t-negation t-description)
;;      (parameter-type
;;       (name t-name)
;;       (accepted-values t-accepted-values)
;;       (negation t-negation)
;;       (description t-description))]))

;; (parameter-type-negation (parameter/type _ '(1 2 3)))
;;
;; (define (package-parameter-name _) #f)
;; (define this-package-parameter #f)
