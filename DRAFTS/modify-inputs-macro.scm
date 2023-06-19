(define-syntax parameter/modify-inputs
  (syntax-rules (all) ; any: default behavior
    [(_) '()]
    [(_ (all clauses ...) rest ...) (begin (begin clauses ...) (parameter/match rest ...))]
    [(_ ((anything ...)) rest ...) (parameter/match rest ...)]
    [(_ ((parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (member #t (map (lambda (x) (not (not (assq-ref properties x))))
                              (list parameters ...)))
              (begin clauses ...))
         (parameter/match rest ...)))]
    [(_ ((all parameters ...) clauses ...) rest ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (begin
         (and (not (member #f (map (lambda (x) (not (not (assq-ref properties x))))
                                   (list parameters ...))))
              (begin clauses ...))
         (parameter/match rest ...)))]))

;; (parameter/modify-inputs (package-inputs "a")
;;                          (x (append z))
;;                          ((y u) (append t) (delete c))
;;                          (_ (something x)))

(define-syntax listify
  (syntax-rules (_)
    [(listify _ rest ...)
     (display (listify rest ...))]
    [(listify x y rest ...)
     (cons (cons x y) (listify rest ...))]
    [(listify x ...)
     (list x ...)]))

(listify _ 2 3 4 5)

(cons 1 '((2 3) (4 5)))

(define-syntax lots-of-pairs->alist
  (syntax-rules ()
    ((_ (a b))
     `((,a ,b)))
    ((_ (a b) rest ...)
     `((,a ,b)
           ,@(lots-of-pairs->alist rest ...)))))

(append (lots-of-pairs->alist (1 2) (3 4)) '((5 6)))

(define-syntax parameter/modify-inputs
  (syntax-rules (for and _)
    [(parameter/modify-inputs for package-name rest ...)
     `(modify-inputs (package package-name)
       ,@(parameter/modify-inputs rest ...))]
    [(parameter/modify-inputs (_ cells ...) rest ...)
     (append
      (lots-of-pairs->alist cells ...)
      (parameter/modify-inputs rest ...))]
    [(parameter/modify-inputs (parameter cells ...) rest ...)
     (append
      (parameter/if parameter (lots-of-pairs->alist cells ...) '())
      (parameter/modify-inputs rest ...))]
  ;  [(parameter/modify-inputs ((parameters ...) cells ...) rest ...)
  ;   (append
  ;    (parameter/if (list parameters ...) (lots-of-pairs->alist cells ...) '())
  ;    (parameter/modify-inputs rest ...))]
    [(parameter/modify-inputs ((and parameters ...) cells ...) rest ...)
     (append
      (parameter/if-all (list parameters ...) (lots-of-pairs->alist cells ...) '())
      (parameter/modify-inputs rest ...))]
    [(parameter/modify-inputs)
     '()]))

(define-syntax ppp3
  (syntax-rules (traverse)
    
    [(ppp3 some ... (traverse b rest ...))
     (ppp3 some ... b (traverse rest ...))]
    [(ppp3 some ... (traverse))
     (+ some ...)]
    [(ppp3 a rest ...)
     (ppp3 a (traverse rest ...))]))

(ppp3 1 2 3)

(when #t (display 'yes) (+ 1 2))

(define-syntax parameter/when
  (syntax-rules ()
    [(parameter/when property expr ...)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (when (if (list? property)
                 (member #t
                  (map (lambda (x) (not (not (assq-ref properties x))))
                       property))
                 (assq-ref properties property))
         expr ...))]))

(define-syntax some/when
  (syntax-rules ()
    [(some/when prop expr ...)
     (when (assq-ref '((1 2) (3 4)) prop)
       expr ...)]))

(list (some/when 1 (cons 1 2) (cons 3 4)))

(define-syntax parameter/modify-inputs
  (syntax-rules (traverse and _ delete prepend append replace)
    [(% inputs some ... (traverse (_ (a b)) rest ...))
     (parameter/modify-inputs inputs some ...
                              (a b)
                              (traverse rest ...))]
    [(% inputs some ... (traverse (_ (a b) cells ...) rest ...))
     (parameter/modify-inputs inputs some ...
                              (a b) ; XXX: optimize
                              (traverse (_ cells ...) rest ...))]
    [(% inputs some ... (traverse ((and parameters ...) (a b)) rest ...))
     (parameter/modify-inputs inputs some ...
                              (parameter/if-all (list parameters ...) (a b))
                              (traverse rest ...))]
    [(% inputs some ... (traverse ((and parameters ...) (a b) cells ...) rest ...))
     (parameter/modify-inputs inputs some ...
                              (parameter/if-all (list parameters ...) (a b))
                              (traverse ((and parameters ...) cells ...) rest ...))]
    [(% inputs some ... (traverse (parameter (a b)) rest ...))
     (parameter/modify-inputs inputs some ...
                              (parameter/if parameter (a b))
                              (traverse rest ...))]
    [(% inputs some ... (traverse (parameter (a b) cells ...) rest ...))
     (parameter/modify-inputs inputs some ...
                              (parameter/if parameter (a b)) ; XXX: optimize
                              (traverse (parameter cells ...) rest ...))]
    [(% inputs some ... (traverse)) ; break out
     (modify-inputs inputs some ...)]
    [(% inputs rest ...) ; should be LAST in macro list
     (parameter/modify-inputs inputs (traverse rest ...))]))

(define-syntax parameter/if
  (syntax-rules ()
    [(parameter/if property exp)
       (if #t
           exp
           '())]
    [(parameter/if property exp exp-else)
       (if #t
           exp
           exp-else)]))

(defne (package pname)
  pname)

(define-syntax modify-inputs
  (syntax-rules ()
    [
    (_ rest ...)
    (begin
      (display (list rest ...))
      ;(display rest ...)
      (newline))]))

(parameter/modify-inputs "NICE PACKAGE"
                         ('a (1 2) (3 4))
                         (('b 'c) (4 5) (6 7))
                         (_ (8 9)))

(lots-of-pairs->alist (1 2) (3 4) (5 6))

(define-syntax modify-inputs
  (syntax-rules (_ and delete prepend append replace)
    ((% inputs (delete name) clauses ...)
     (modify-inputs (alist-delete name inputs)
                    clauses ...))
    ((% inputs (delete names ...) clauses ...)
     (modify-inputs (fold alist-delete inputs (list names ...))
                    clauses ...))
    ((% inputs (prepend lst ...) clauses ...)
     (modify-inputs (append (map add-input-label (list lst ...)) inputs)
                    clauses ...))
    ((% inputs (append lst ...) clauses ...)
     (modify-inputs (append inputs (map add-input-label (list lst ...)))
                    clauses ...))
    ((% inputs (replace name replacement) clauses ...)
     (modify-inputs (replace-input name replacement inputs)
                    clauses ...))
    ((% inputs)
     inputs)))

(define-syntax parameter/modify-inputs
  (syntax-rules (_ and delete prepend append replace)
    [(% inputs (parameter (delete name)) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (alist-delete name inputs)
       inputs)
      clauses ...)]
    [(% inputs (parameter (delete names ...)) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (fold alist-delete inputs (list names ...))
       inputs)
      clauses ...)]                        
    [(% inputs (parameter (prepend lst ...)) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (append (map add-input-label (list lst ...)) inputs)
       inputs)
      clauses ...)]                                                 
    [(% inputs (parameter (append lst ...)) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (append inputs (map add-input-label (list lst ...)))
       inputs)
      clauses ...)]                                                
    [(% inputs (parameter (replace name replacement)) clauses ...)
     (parameter/modify-inputs
      (parameter/modifier-if
       parameter
       (replace-input name replacement inputs)
       inputs)
      clauses ...)]
    [(% inputs)
     inputs]))

(define-syntax parameter/modifier-if
  (syntax-rules (_ and delete prepend append replace)
    [(% _ exp exp2)
     exp]
    [(% (and parameters ...) exp exp2)
     (let ((properties (parameter-spec/parameter-alist (package-parameter-spec this-package))))
       (if (member #t
                   (map (lambda (x) (not (not (assq-ref properties x))))
                        (list parameters ...)))
           exp
           exp2))]
    [(% (and parameter) exp exp2)
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
