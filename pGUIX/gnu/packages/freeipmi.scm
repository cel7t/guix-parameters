;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019–2022 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages freeipmi)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gnupg))

(define-public freeipmi
  (package
    (name "freeipmi")
    (version "1.6.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freeipmi/freeipmi-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0zbszq9nn60vqm2gmwy6hmcz9yqb3lk064ib7l89q65n07ja3r7w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           ,@(if (%current-target-system)
                               ;; We cannot check for these devices
                               ;; when cross compiling.
                               `("ac_cv_file__dev_random=yes"
                                 "ac_cv_file__dev_urandom=yes")
                               '()))))
    (inputs
     (list libgcrypt))
    (home-page "https://www.gnu.org/software/freeipmi/")
    (synopsis "Platform management, including sensor and power monitoring")
    (description
     "GNU FreeIPMI is a collection of in-band and out-of-band IPMI software
in accordance with the IPMI v1.5/2.0 specification.  These programs provide a
set of interfaces for platform management.  Common functionality includes
sensor monitoring, system event monitoring, power control and
serial-over-LAN.")
    (license gpl3+)))
