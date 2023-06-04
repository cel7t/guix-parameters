;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages iso-codes)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define-public iso-codes/official
  ;; This package variant is intended for ‘external’ use, such as users running
  ;; ‘guix install’, where any deviation from ISO gospel might be harmful.
  (package
    (name "iso-codes")
    (version "4.5.0")
    (home-page "https://salsa.debian.org/iso-codes-team/iso-codes")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url home-page)
                   (commit (string-append "iso-codes-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1q6x9c5x4x0x4q11iygldsmxdyzhz1mb4n8im76glwsgqsqyjs80"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal perl python-wrapper))
    (synopsis "Various ISO standards")
    (description
     "This package provides lists of various ISO standards (e.g. country,
language, language scripts, and currency names) in one place, rather
than repeated in many programs throughout the system.

Currently there are lists of languages and countries embedded in
several different programs, which leads to dozens of lists of
200 languages, translated into more than 30 languages... not
very efficient.

With this package, we create a single \"gettext domain\" for every
supported ISO standard which contains the translations of
that domain.  It is easy for a programmer to re-use those
translations instead of maintaining their own translation
infrastructure.  Moreover, the programmer does not need to follow
changes in the ISO standard and will not work with outdated
information.")
    (license license:gpl2+)))           ; some bits use the lgpl2

(define-public iso-codes
  ;; This package should be used universally within Guix, e.g., as an input to
  ;; other Guix packages or in the Guix System installer's country selector.
  (hidden-package
   (package
     (inherit iso-codes/official)
     (source
      (origin
        (inherit (package-source iso-codes/official))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (substitute* (find-files "." "\\.po$")
              (("#.*Name for TWN,.*") "")
              (("^msgid \"Taiwan, .*") "# Guix doesn't use "))
            (substitute* "data/iso_3166-1.json"
              (("(Taiwan), [^\"]*" _ name) name))))))
     (synopsis "Various ISO standards as used by GNU@tie{}Guix"))))

(define-public python-iso639
  (package
    (name "python-iso639")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iso-639" version))
       (sha256
        (base32
         "0jffmh4m20q8j27xb2fqbnlghjj0cx8pgsbzqisdg65qh2wd976w"))))
    (build-system python-build-system)
    (home-page "https://github.com/noumar/iso639")
    (synopsis "Python library for ISO 639 standard")
    (description "This package provides a Python library for ISO 639 standard
that is concerned with representation of names for languages and language
groups.")
    (license license:agpl3+)))

(define-public python-iso3166
  (package
    (name "python-iso3166")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iso3166" version))
       (sha256
        (base32
         "068p94gavc8gbmp5a2kw8hi5l551wfzbpmp6z7ll8sx6vnw53mgw"))))
    (build-system python-build-system)
    (home-page "https://github.com/deactivated/python-iso3166")
    (synopsis "Self-contained ISO 3166-1 country definitions")
    (description "This package provides the ISO 3166-1 country definitions.")
    (license license:expat)))
