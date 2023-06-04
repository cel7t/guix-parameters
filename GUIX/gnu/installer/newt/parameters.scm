;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt parameters)
  #:use-module (gnu installer proxy)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (guix build syscalls)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (newt)
  #:export (run-parameters-page))

(define (run-proxy-page)
  (define proxy
    (run-input-page (G_ "Please enter the HTTP proxy URL. If you enter an \
empty string, proxy usage will be disabled.")
                    (G_ "HTTP proxy configuration")
                    #:allow-empty-input? #t))
  (if (string=? proxy "")
      (clear-http-proxy)
      (set-http-proxy proxy)))

(define (run-parameters-page keyboard-layout-selection)
  "Run a parameters page allowing to change the keyboard layout"
  (let* ((items
          (list
           (cons (G_ "Change keyboard layout") keyboard-layout-selection)
           (cons (G_ "Configure HTTP proxy") run-proxy-page)
           (cons (G_ "Reboot") reboot)))
         (result
          (run-listbox-selection-page
           #:info-text (G_ "Please choose one of the following parameters or \
press ‘Back’ to go back to the installation process.")
           #:title (G_ "Installation parameters")
           #:listbox-items items
           #:listbox-item->text car
           #:sort-listbox-items? #f
           #:listbox-height 6
           #:button-text (G_ "Back"))))
    (match result
      ((_ . proc)
       (proc))
      (_ #f))))
