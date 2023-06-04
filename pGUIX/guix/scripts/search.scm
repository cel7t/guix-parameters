;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix scripts search)
  #:use-module (guix ui)
  #:use-module (guix scripts package)
  #:use-module ((guix scripts build)
                #:select (%standard-build-options))
  #:use-module (guix scripts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (guix-search))

(define (show-help)
  (display (G_ "Usage: guix search [OPTION] REGEXPS...
Search for packages matching REGEXPS."))
  (display (G_"
This is an alias for 'guix package -s'.\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix search")))

        (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)))

(define-command (guix-search . args)
  (synopsis "search for packages")

  (define (handle-argument arg result)
    ;; Treat all non-option arguments as regexps.
    (cons `(query search ,(or arg ""))
          result))

  (define opts
    (parse-command-line args %options (list (list))
                        #:build-options? #f
                        #:argument-handler handle-argument))

  (unless (assoc-ref opts 'query)
    (leave (G_ "missing arguments: no regular expressions to search for~%")))

  (guix-package* opts))
