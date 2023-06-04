;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2013-2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
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

(define-module (guix scripts lint)
  #:use-module (guix packages)
  #:use-module (guix lint)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (guix-lint
            run-checkers))

(define (emit-warnings warnings)
  ;; Emit a warning about PACKAGE, printing the location of FIELD if it is
  ;; given, the location of PACKAGE otherwise, the full name of PACKAGE and the
  ;; provided MESSAGE.
  (for-each
   (lambda (lint-warning)
     (let* ((package (lint-warning-package lint-warning))
            (name    (package-name package))
            (version (package-version package))
            (loc     (lint-warning-location lint-warning))
            (message (lint-warning-message lint-warning)))
       (parameterize
           ((guix-warning-port (current-output-port)))
         (info loc (G_ "~a@~a: ~a~%")
               name version message))))
   warnings))

(define* (run-checkers package checkers #:key store)
  "Run the given CHECKERS on PACKAGE."
  (let ((tty? (isatty? (current-error-port))))
    (for-each (lambda (checker)
                (when tty?
                  (format (current-error-port) "checking ~a@~a [~a]...\x1b[K\r"
                          (package-name package) (package-version package)
                          (lint-checker-name checker))
                  (force-output (current-error-port)))
                (emit-warnings
                 (if (lint-checker-requires-store? checker)
                     ((lint-checker-check checker) package #:store store)
                     ((lint-checker-check checker) package))))
              checkers)
    (when tty?
      (format (current-error-port) "\x1b[K")
      (force-output (current-error-port)))))

(define (list-checkers-and-exit checkers)
  ;; Print information about all available checkers and exit.
  (format #t (G_ "Available checkers:~%"))
  (for-each (lambda (checker)
              (format #t "- ~a: ~a~%"
                      (lint-checker-name checker)
                      (G_ (lint-checker-description checker))))
            checkers)
  (exit 0))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  '())

(define (show-help)
  (display (G_ "Usage: guix lint [OPTION]... [PACKAGE]...
Run a set of checkers on the specified package; if none is specified,
run the checkers on all packages.\n"))
  (display (G_ "
  -c, --checkers=CHECKER1,CHECKER2...
                         only run the specified checkers"))
   (display (G_ "
  -x, --exclude=CHECKER1,CHECKER2...
                         exclude the specified checkers"))
  (display (G_ "
  -n, --no-network       only run checkers that do not access the network"))
  (display (G_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))

  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -l, --list-checkers    display the list of available lint checkers"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (option-checker short-long)
  ;; Factorize the creation of the two options -c/--checkers and -x/--exclude,
  ;; see %options.  The parameter SHORT-LONG is the list containing the short
  ;; and long name.  The alist uses the long name as symbol.
  (option short-long #t #f
          (lambda (opt name arg result)
            (let ((names (map string->symbol (string-split arg #\,)))
                  (checker-names (map lint-checker-name %all-checkers))
                  (option-name (string->symbol (match short-long
                                                 ((short long) long)))))
              (for-each (lambda (c)
                          (unless (memq c checker-names)
                            (leave (G_ "~a: invalid checker~%") c)))
                        names)
              (alist-cons option-name
                          (filter (lambda (checker)
                                    (member (lint-checker-name checker)
                                            names))
                                  %all-checkers)
                          result)))))

(define %options
  ;; Specification of the command-line options.
  ;; TODO: add some options:
  ;; * --certainty=[low,medium,high]: only run checkers that have at least this
  ;;                                  'certainty'.
  (list (option-checker '(#\c "checkers"))
        (option-checker '(#\x "exclude"))
        (option '(#\n "no-network") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'no-network? #t result)))
        (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\l "list-checkers") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'list? #t result)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))

        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix lint")))))


;;;
;;; Entry Point
;;;

(define-command (guix-lint . args)
  (category packaging)
  (synopsis "validate package definitions")

  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                             (('argument . spec)
                              (specification->package spec))
                             (('expression . exp)
                              (read/eval-package-expression exp))
                             (_ #f))
                           (reverse opts)))
         (no-checkers (or (assoc-ref opts 'exclude) '()))
         (the-checkers (filter (lambda (checker)
                                 (not (member checker no-checkers)))
                               (or (assoc-ref opts 'checkers) %all-checkers)))
         (checkers
          (if (assoc-ref opts 'no-network?)
              (filter (lambda (checker)
                        (member checker %local-checkers))
                      the-checkers)
              the-checkers)))

    (when (assoc-ref opts 'list?)
      (list-checkers-and-exit checkers))

    (with-error-handling
      (let ((any-lint-checker-requires-store?
             (any lint-checker-requires-store? checkers)))

        (define (call-maybe-with-store proc)
          (if any-lint-checker-requires-store?
              (with-store store
                (proc store))
              (proc #f)))

        (call-maybe-with-store
         (lambda (store)
           (cond
            ((null? args)
             (fold-packages (lambda (p r) (run-checkers p checkers
                                                        #:store store)) '()))
            (else
             (for-each (lambda (package)
                         (run-checkers package checkers
                                       #:store store))
                       args)))))))))
