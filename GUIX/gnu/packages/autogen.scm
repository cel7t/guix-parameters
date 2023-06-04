;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2023 Kaelyn Takata <kaelyn.alexi@protonmail.com>
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

(define-module (gnu packages autogen)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile))

(define-public autogen
  (package
    (name "autogen")
    (version "5.18.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/autogen/rel" version
                           "/autogen-" version ".tar.xz"))
       (sha256
        (base32 "16mlbdys8q4ckxlvxyhwkdnh1ay9f6g0cyp1kylkpalgnik398gq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Address '-Werror=format-overflow' error.
           (substitute* "getdefs/getdefs.c"
             (("def_bf\\[[[:space:]]*MAXNAMELEN[[:space:]]*\\]")
              "def_bf[MAXNAMELEN + 10]"))
           ;; Address '-Werror=format-truncation' error on i686.
           (substitute* "autoopts/usage.c"
             (("vfmt\\[sizeof\\(vfmtfmt\\)\\]")
              "vfmt[sizeof(vfmtfmt) + 6]"))))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config which))
    (inputs (list guile-3.0 perl))          ; for doc generator mdoc
    (arguments
     '(#:configure-flags
       ;; XXX Needed to build 5.18.16.  ./configure fails without it:
       ;; “Something went wrong bootstrapping makefile fragments for
       ;;  automatic dependency tracking.  Try re-running configure with […]”
       (list "--disable-dependency-tracking")

       ;; XXX: Parallel tests may cause an indefinite hang with GNU Make 4.3.
       #:parallel-tests? #f

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'support-guile-3.0
           ;; Upstream bug:
           ;; https://sourceforge.net/p/autogen/bugs/196/
           ;;
           ;; Supported in Debian and openSUSE:
           ;; https://salsa.debian.org/debian/autogen/-/blob/master/debian/patches/40_suse_04-guile-version.patch
           (lambda _
             (substitute* "agen5/guile-iface.h"
               (("#elif GUILE_VERSION < 203000") "#elif GUILE_VERSION < 301000"))
             (substitute* "configure"
               (("2.2 2.0 1.8") "3.0 2.2 2.0 1.8"))))
         (add-after 'unpack 'use-numeric-ids-in-tarball
           ;; Pass arguments to tar to generate tarball with consistent uid
           ;; and gid to ensure reproducible build
           (lambda _
             (substitute* "pkg/libopts/mklibsrc.sh"
               (("--sort=name --format=gnu")
                "--sort=name --format=gnu --owner=0 --group=0 --numeric-owner"))))
         (add-before 'build 'set-man-page-date
           ;; Avoid embedding the current date for reproducible builds
           (lambda _
             (setenv "MAN_PAGE_DATE" "2012-04-18")))
         (add-before 'patch-source-shebangs 'patch-test-scripts
           (lambda _
             (let ((sh (which "sh")))
               (substitute*
                   (append (find-files "agen5/test" "\\.test$")
                           (find-files "autoopts/test" "\\.(test|in)$"))
                 (("/bin/sh") sh))
               #t))))))
    (home-page "https://www.gnu.org/software/autogen/")
    (synopsis "Automated program generator")
    (description
     "AutoGen is a program to ease the maintenance of programs that contain
large amounts of repetitive text.  It automates the construction of these
sections of the code, simplifying the task of keeping the text in sync.  It
also includes an add-on package called AutoOpts, which is specialized for the
maintenance and documentation of program options.")
    (license gpl3+)))
