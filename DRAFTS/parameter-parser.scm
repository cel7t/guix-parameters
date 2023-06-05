(define-module (draft parameters)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; IMPORTANT:
;; functions will be renamed to more sensible names
;; s/p\//parameters-/g

(define %parameters ; example parameters of a package "zoo"
  '(parameters
    (required ant beaver) ; these are REQUIRED to build the package
    (required-off zebra) ; these are REQUIRED TO BE OFF, useful for forcing errors
    (optional (cat dog) elephant) ; these are optional, cat and dog are default
    (one-of cat flamingo gorilla) ; only one of these is allowed, cat is default
    (one-of none hippo impala) ; only one of these is allowed, none are default
    (special (on flamingo) ; flamingo, if ON, has a special transform
             (off cat hippo)))) ; cat and hippo have one when OFF

(define (p/default p-list)
  (delete-duplicates 
   (apply
    append
    (map
     (lambda (ls)
       (case (car ls)
         ((required)
          (cdr ls))
         ((optional)
          (if (list? (cadr ls))
              (cadr ls)
              '()))
         ((one-of)
          (if (not (eq? (cadr ls) 'none))
              (list (cadr ls))
              '()))
         ((special required-off)
          '())
         (else (error "Invalid parameter specification: " (car ls)))))
     (cdr p-list)))))

(define (p/default-off p-list)
  (delete-duplicates 
   (apply
    append
    (map
     (lambda (ls)
       (case (car ls)
         ((required-off)
          (cdr ls))
         ((optional)
          (if (list? (cadr ls))
              (cddr ls)
              (cdr ls)))
         ((one-of)
          (cddr ls))
         ((special required)
          '())
         (else (error "Invalid parameter specification: " (car ls)))))
     (cdr p-list)))))

(define (p/total p-list)
  (lset-union
   eqv?
   (p/default p-list)
   (p/default-off p-list)))

(p/default %parameters)
(p/default-off %parameters)
(p/total %parameters)

;; We need custom boolean operators as they cannot short-circuit

(define (p-and . args)
  (= 0
     (count (lambda (x) (eqv? #f x))
            args)))

(define (p-none . args)
  (= 0
     (count (lambda (x) (eqv? #t x))
            args)))

(define (p-xor . args)
  (= 1
     (count (lambda (x) (eqv? #t x))
            args)))

(p-and #t #t)

(define (p/resolve p-spec parameter-list)
  (apply
   p-and
   (let ((p-list (cons 'none parameter-list)))
     (map
      (lambda (ls)
        (apply
         (case (car ls)
           ((required) p-and)
           ((required-off) p-none)
           ((optional) (lambda _ #t))
           ((one-of) p-xor)
           ((special) (lambda _ #t))
           (else (error "Invalid parameter specification: " (car ls))))
         (map
          (lambda (sym)
            (not (not (member sym p-list))))
          (cdr ls))))
      (cdr p-spec)))))

(p/resolve %parameters (p/default %parameters)) ; sanity check

;; Code for getting the OS' parameter specification

;; defined "parameter" in the OS record
;; use method to get it
;; write similar parser for it

(define %default-os-parameters
  '((on ant cat mouse)
    (off cow horse elephant)))

(define (p/read-os-parameters) ; dummy method
  %default-os-parameters)

;; OS parameters method needs to create (on) and (off)
;; regardless of the existence of on/off parameters

;; Expected format for OS parameters:
;; '((on x y) (off z) (on d e f a) (off u l t))
;; run two filters to collect all on/off respectively

(define (p/os-parameters)
  (let ((user-p (p/read-os-parameters)))
    (list
     (cons 'on
           (apply append
                  (map (lambda (ls) (cdr ls))
                       (filter (lambda (ls)
                                 (eqv? (car ls) 'on))
                               user-p))))
     (cons 'off
           (apply append
                  (map (lambda (ls) (cdr ls))
                       (filter (lambda (ls)
                                 (eqv? (car ls) 'off))
                               user-p)))))))

(define (p/read-os-parameters)
  '((on ant cat) (off cow) (on mouse) (off horse elephant)))

(p/os-parameters) ; works!

(define (p/read-os-parameters)
  '((on ant cat mouse))) ; only on parameters

(p/os-parameters) ; still generates (off)

;; should we actually override %base-parameters
;; if a in os' (on) is in user's (off)?

(define (p/os-parameters-overriding)
  (let ((user-p (p/read-os-parameters)))
    (define (recurse-on p off-lst)
      (filter (lambda (x) (not (member x off-lst)))
              p))
    (define (recurse-off p on-lst)
      (filter (lambda (x) (not (member x on-lst)))
              p))
    (define (recurse-over-p p on-lst off-lst)
      (if (null? p)
          (list (cons 'on on-lst)
                (cons 'off off-lst))
          (if (eqv? 'on (caar p))
              (recurse-over-p
               (cdr p)
               (append (recurse-on (cdar p) off-lst)
                       on-lst)
               off-lst)
              (recurse-over-p
               (cdr p)
               on-lst
               (append (recurse-on (cdar p) on-lst)
                       off-lst)))))
    (recurse-over-p user-p '() '())))

(define (p/read-os-parameters)
  ;; horse is off in %base-parameters but on in user parameters
  '((on ant cat horse) (off cow) (on mouse) (off horse elephant)))

(p/os-parameters-overriding) ; works!

(define (p/get-parameters _) ; dummy method
  %parameters)

;; parameters = default + (common os-on available) - (common os-off available)
(define (p/package-parameters package)
  (let* ((os-on (cdar (p/os-parameters)))
         (os-off (cdadr (p/os-parameters)))
         (p-pkg (p/get-parameters package))
         (all (p/total p-pkg)))
    (lset-difference
     eqv?
     (lset-union
      eqv?
      (p/default p-pkg) ; default parameters
      (lset-intersection
       eqv?
       all ; all available parameters
       os-on)) ; os' on parameters
     (lset-intersection
      eqv?
      all
      os-off)))) ; os' off parameters

(p/package-parameters %parameters)

;; Now, we want to check if the OS configuration resolves
(define (p/applicable-parameters pkg)
  (let ((user-p (p/package-parameters pkg))
        (pkg-p (p/get-parameters pkg)))
    (if (p/resolve pkg-p user-p) ; if user-p resolve
        user-p ; return user-p
        (and ; XXX: print package name 
         (display "User parameters do not resolve. Using default parameters for package")
         (p/default pkg-p))))) ; uses default parameters

(p/applicable-parameters %parameters)

(define (p/applicable-parameters-off package)
  (lset-difference
   eqv?
   (p/total package)
   (p/applicable-parameters package)))

(p/applicable-parameters-off %parameters)

;; Functions for checking what parameters require special transforms

(define (p/special-transforms pkg ps/e ps/d)
  (let* ((p-spec (p/get-parameters pkg))
         (special (last (cdr p-spec)))) ; "special" must be the last list
    (if (eqv? (car special) 'special)
        (filter (lambda (ls) (> (length ls) 1)) ; remove "(on)" and "(off)"
                (map (lambda (ls)
                       (if (eqv? (car ls) 'on)
                           (cons 'on
                                 (lset-intersection eqv?
                                                    ps/e
                                                    (cdr ls)))
                           (cons 'off
                                 (lset-intersection eqv?
                                                    ps/d
                                                    (cdr ls)))))
                     (cdr special)))
        '())))

(p/special-transforms %parameters
                      (p/applicable-parameters %parameters)
                      (p/applicable-parameters-off %parameters))

;; Now we will have to modify the package record itself to hold 'special transforms'
;; These are transforms that do not match the standard parameter transforms for the build system

;; Ex. disabled `tests` is usually done by the without-tests transform
;; But maybe a package needs an extra transform to be done for it

;; Might also need to look into declaring parameters as _necessarily_ recursive for #2 and #3

;; We also need to modify Ludo's parameter record type to accept these instead
;; And the new package record needs to take the parameters as a list

;; After simple parameters are done, CFLAGS/CXXFLAGS may be implemented as parameters
;; For these, letting package-specific rules override OS rules makes more sense
;; As parameter syms can be anything, having ex. -ffast-math itself as a sym works
;; Implementing these will be otherwise very easy as with-configure-flag can be used
;; WISHLIST: Add sublist for CFLAGS (gcc flags)

;; 6/4: s/enabled/on/g s/disabled/off/g
;; on/off are more succint and nicer
;; added 'required-off'; could be useful for packages that cannot be built on systems
;; with a particular parameter enabled (ex. x86-only package on ARM system)
;; created os-parameters and os-parameters-overriding!
