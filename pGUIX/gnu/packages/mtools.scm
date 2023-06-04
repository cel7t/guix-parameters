;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2015, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages mtools)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public mtools
  (package
    (name "mtools")
    (version "4.0.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mtools/mtools-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "05fg5i8da5jdym3cq2939j7n3fqw4cz2riy1yci6pbw29pgdzgv4"))
              (patches
               (search-patches "mtools-mformat-uninitialized.patch"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/mtools/")
    (synopsis "Access MS-DOS disks without mounting")
    (description
     "GNU Mtools is a set of utilities for accessing MS-DOS disks from a GNU
or Unix system.  It supports long file names and multiple disk formats.  It
also supports some FAT-specific features such as volume labels and
FAT-specific file attributes.")
    (license gpl3+)))
