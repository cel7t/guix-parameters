;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix scripts upgrade)
  #:use-module (guix ui)
  #:use-module (guix scripts package)
  #:use-module (guix scripts build)
  #:use-module (guix transformations)
  #:use-module (guix scripts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-upgrade))

(define (show-help)
  (display (G_ "Usage: guix upgrade [OPTION] [REGEXP]
Upgrade packages that match REGEXP.
This is an alias for 'guix package -u'.\n"))
  (display (G_ "
  -p, --profile=PROFILE  use PROFILE instead of the user's default profile"))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
      --do-not-upgrade[=REGEXP] do not upgrade any packages matching REGEXP"))
  (newline)
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix upgrade")))

         ;; Preserve some of the 'guix package' options.
         (append (filter (lambda (option)
                           (any (cut member <> (option-names option))
                                '("profile" "dry-run" "verbosity" "do-not-upgrade")))
                         %package-options)

                 %transformation-options
                 %standard-build-options)))

(define-command (guix-upgrade . args)
  (synopsis "upgrade packages to their latest version")

  (define (handle-argument arg result arg-handler)
    ;; Treat non-option arguments as upgrade regexps.
    (values (alist-cons 'upgrade arg
                        (delete '(upgrade . #f) result))
            arg-handler))

  (define opts
    (parse-command-line args %options
                        (list `((upgrade . #f)
                                ,@%package-default-options)
                              #f)
                        #:argument-handler handle-argument))

  (guix-package* opts))
