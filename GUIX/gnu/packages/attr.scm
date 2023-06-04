;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012, 2013, 2016, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages attr)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages hurd)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public attr
  (package
    (name "attr")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/attr/attr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1y6sibbkrcjygv8naadnsg6xmsqwfh6cwrqk01l0v2i5kfacdqds"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ,@(if (hurd-target?)
               `((add-before 'configure 'skip-linux-syscalls
                   (lambda _
                     ;; Starting from 2.5.1, libattr includes Linux-specific
                     ;; calls to syscall(2).  Comment them out for GNU/Hurd
                     ;; and instead use the glibc-provided wrappers.
                     (substitute* "Makefile.in"
                       (("libattr/syscalls\\.c") "")
                       (("\tlibattr/la-syscalls\\.lo") "")
                       (("-Wl,[[:graph:]]+/libattr\\.lds") "")))))
               '())
         (replace 'check
           (lambda* (#:key target #:allow-other-keys)
             ;; Use the right shell.
             (substitute* "test/run"
               (("/bin/sh")
                (which "sh")))

             ;; When building natively, run the tests.
             ;;
             ;; Note that we use system* and unconditionally return #t here
             ;; to ignore the test result, because the tests will fail when
             ;; the build is performed on a file system without support for
             ;; extended attributes, and we wish to allow Guix to be built
             ;; on such systems.
             (unless target
               (system* "make" "tests" "-C" "test")))))))
    (inputs
     ;; Perl is needed to run tests; remove it from cross builds.
     (if (%current-target-system)
         '()
         `(("perl" ,perl))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))

    (home-page "https://savannah.nongnu.org/projects/attr/")
    (synopsis "Library and tools for manipulating extended attributes")
    (description
     "Portable library and tools for manipulating extended attributes.")
    (license lgpl2.1+)))
