;; Drafts for enumerated and negated parameters

;; idea for parameter defintions:
;; type:
;;   universe must have a minimum of two values
;;   first value is always "OFF", and the rest are assigned positive indices
;;   indices and corresponding symbols are interchangeable

;; method for implementation:
;; make the parser type-conscious, with a separate parser for non-booleans
;; this is a BAD idea; parameters will be a mix of booleans and non-booleans

;; define separate functions for verifying parameter values
;; add functionality to 'require' etc parameters with values
;; end result: parameter-alist should return values other than #t and #f

;; package-rewriting and input modification can be added in as fields within parameters
;; we need a generic name for them, as they cover more than transforms
;; package morphisms?

(package-parameter-type
 (name 'oflag-type)
 (description "Parameter-type for GCC Oflags")
 (universe '(-O0 -O1 -O2 -O3 -Os -Ofast -Og -Oz)) ; must be symbols
 (negation '-O0)) ; negating a parameter with this type returns -O0
;; if negation is set to #f, negation is disallowed
;; by default the first value in the universe is picked
;; (hence this was unnecessary)

(parameter/type -O0 -O1 -O2 -O3 -Os -Ofast -Og -Oz) ; quick, anonymous definition

(package-parameter
 (name 'oflag)
 (type 'oflag-type) ; to avoid confusion with package-parameter-type (type definition)
 (morphisms
  (parameter/morphisms
   (transform -> (some-transform . (string-append ^name "=something")) (some-other-transform . ^name)) ; ^name -> filled in as package name
   (modify-inputs + _ + gnu-build-system -> (append something) (delete something-else)) ; _ -> match any enumeration
   (rewrite + O2 -> (replace-this with-this)))
 (description
  "Parameter Describing GCC Oflags"))

;; another important todo is adding a method to access the package name from within morphisms
;; for example, if we want a generic --without-tests we need to supply the package name as an argument
;; new morphism macro could take care of that, but it would make the most sense to use the parser for this

;; new match function will default to checking if anything at all is non-negated
;; but if a cons cell is encountered it will match against the cdr

(parameter/match
 ((oflag . -O2) '(#:make-flag "-O2"))
 (oflag '(#:tests? #f))) ; -O2 will trigger this too

;; negation
;; I'll write a common parameter value-seeking procedure
;; this procedure will check if a parameter ends with !, and if it does it will treat it differently

(require '(tests! tests)) ;; -> error

;; now there are three situations
;; 1) both p and p! are defined
;;    in this situation a simple (one-of (p p!) ...) is enough
;; 2) p is defined but p! is not
;;   if p! is being used, do nothing- take it as the absence of p
;; 3) p! is defined but p is not
;;   same as above, but with roles reversed

;; so in practice, for enumerated types p! is shorthand for (p [value at index 0])

;; checking for negation:
(and (string=? (string-take-right (symbol->string 'p!) 1) "!")
     (not (package-parameter-type-negation (package-parameter-type PARAMETER-NAME))))

;; if a type is non-boolean, we want the UI to show all enumerations as well

(use-modules (ice-9 regex))
(match:prefix (if-return
               (string-match "!" "psi")
               (string-match "!" "!")))

(define-syntax if-return
  (syntax-rules ()
    [(% expr)
     (if expr expr)]
    [(% expr els)
     (if expr expr els)]))

(define (give-me-a-symbol ex)
  (cond ((symbol? ex) ex)
        ((string? ex) (string->symbol ex))
        (else (throw 'bad! ex))))

(define-record-type* <parameter-type> parameter-type
  make-parameter-type
  parameter-type?
  (name          parameter-type-name
                 (sanitizer give-me-a-symbol))
  (universe      parameter-type-universe)
  (negation      parameter-type-negation
                 (default (car universe)))
  (description   parameter-type-description))

;; (define-syntax p-test
;;   (syntax-rules (+)
;;     [(% a ... + c ...)
;;      (+ a ... (p-test c ...))]))
    
;; multiple ... only work with ()
;; this is because ... is a postfix operator

(define* (parameter/parse-morphisms lst #:optional (soup '()))
  (cond ((eqv? (car lst) (symbol->keyword 'transform)) "ok")))

(keyword-like-symbol->keyword ':transform)
(parameter/parse-morphisms '(:transform))

(use-modules (srfi srfi-1))

(find-tail keyword? '(#:a b c #:d e f))

(span keyword? '(#:a b c #:d e f))
(break keyword? '(b c d e f))

(car '(1))

(cdr '(1))

(define* (parameter/parse-morphisms kw-lst)
  (define* (list-till-kw lst #:optional (carry '()))
    (cond ((null? lst) (cons (reverse carry) '()))
          ((and (not (null? (cdr lst)))
                (keyword? (car lst)))
           (cons (reverse carry) lst))
          (else (list-till-kw (cdr lst) (cons (car lst) carry)))))
  (define* (break-keywords lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) '())
          ((keyword? (car lst))
           (let ((next-lst (list-till-kw (cdr lst))))
             (cons (cons (keyword->symbol (car lst))
                         (car next-lst))
                   (break-keywords (cdr next-lst)))))
          (else (throw 'bad! lst))))
  (break-keywords kw-lst))

(break-keywords '(#:a b c #:d e f))
(break keyword? '(b c #:d e f))
(list-till-kw '(b c #:d e f))
(parameter/parse-morphisms '(#:a b c #:d e f))

(values 1 2)
      
(define-syntax syntax-case-test
  (lambda (x)
    (syntax-case x ()
      ((_ p rest ...)
       (syntax
        (when (keyword? p)
           rest ...))))))

(syntax-case-test "not ok" "ok")
(syntax-case-test #:ok "ok")

(use-modules (ice-9 match))

(match '(3 3)
  [((? even? a) b)
   (display (cons a b))]
  [(a ...)
   (display a)]
  [_
   (display "fail")])

(use-modules (srfi srfi-1))

(pair? (cons 1 2))
