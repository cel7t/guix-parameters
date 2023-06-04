(define-module (draft parameters)
	#:use-module (srfi srfi-1)
	#:use-module (ice-9 match))

;; IMPORTANT:
;; functions will be renamed to more sensible names
;; s/p\//parameters-/g

(define %parameters ; example parameters of a package "zoo"
	'(parameters
		(required ant beaver) ; these are REQUIRED to build the package
		(optional (cat dog) elephant) ; these are optional, cat and dog are default
		(one-of cat flamingo gorilla) ; only one of these is allowed, cat is default
		(one-of none hippo impala) ; only one of these is allowed, none are default
		(special (enabled flamingo) ; flamingo, if enabled, has a special transform
						 (disabled cat hippo)))) ; cat and hippo have one when disabled

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
				 ((special)
					'())
				 (else (error "Invalid parameter specification: " (car ls)))))
		 (cdr p-list)))))

(define (p/default-disabled p-list)
	(delete-duplicates 
	 (apply
		append
		(map
		 (lambda (ls)
			 (case (car ls)
				 ((required)
					'())
				 ((optional)
					(if (list? (cadr ls))
							(cddr ls)
							(cdr ls)))
				 ((one-of)
					(cddr ls))
				 ((special)
					'())
				 (else (error "Invalid parameter specification: " (car ls)))))
		 (cdr p-list)))))

(define (p/total p-list)
	(lset-union
	 eqv?
	 (p/default p-list)
	 (p/default-disabled p-list)))

(p/default %parameters)
(p/default-disabled %parameters)
(p/total %parameters)

;; We need custom boolean operators as they cannot short-circuit

(define (p-and . args)
	(= 0
		 (count (lambda (x) (eqv? #f x))
						args)))

(define (p-xor . args)
	(= 1
		 (count (lambda (x) (eqv? #t x))
										args)))

(define (p/resolve p-spec parameter-list)
	(apply
	 p-and
	(let ((p-list (cons 'none parameter-list)))
	 (map
		 (lambda (ls)
			 (apply
				(case (car ls)
					((required) p-and)
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
	'((enabled ant cat mouse)
		(disabled cow horse elephant)))

(define (p/os-parameters) ; dummy method
	%default-os-parameters)

(define (p/get-parameters _) ; dummy method
	%parameters)

;; parameters = default + (common os-enabled available) - (common os-disabled available)
(define (p/package-parameters package)
	(let* ((os-enabled (cdar (p/os-parameters)))
				 (os-disabled (cdadr (p/os-parameters)))
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
			 os-enabled)) ; os' enabled parameters
		 (lset-intersection
			eqv?
			all
			os-disabled)))) ; os' disabled parameters

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

(define (p/applicable-parameters-disabled package)
	(lset-difference
	 eqv?
	 (p/total package)
	 (p/applicable-parameters package)))

(p/applicable-parameters-disabled %parameters)

;; Functions for checking what parameters require special transforms

(define (p/special-transforms pkg ps/e ps/d)
	(let* ((p-spec (p/get-parameters pkg))
				 (special (last (cdr p-spec)))) ; "special" must be the last list
		(if (eqv? (car special) 'special)
				(filter (lambda (ls) (> (length ls) 1)) ; remove "(enabled)" and "(disabled)"
						 (map (lambda (ls)
										(if (eqv? (car ls) 'enabled)
												(cons 'enabled
															(lset-intersection eqv?
																								 ps/e
																								 (cdr ls)))
												(cons 'disabled
															(lset-intersection eqv?
																								 ps/d
																								 (cdr ls)))))
									(cdr special)))
				'())))

(p/special-transforms %parameters
											(p/applicable-parameters %parameters)
											(p/applicable-parameters-disabled %parameters))

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
