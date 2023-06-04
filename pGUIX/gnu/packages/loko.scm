;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
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

(define-module (gnu packages loko)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages chez))

(define-public loko-scheme
  (package
    (name "loko-scheme")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/weinholt/loko")
             (commit (string-append "v" version))))
       (sha256
        (base32 "12xp82z91qkp9q8lfp46s4sda8qgs472jic3js1kbykn4jzy7399"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; r7rs tests are a work in progress as of 0.7.0.
      #:tests? #f
      #:strip-binaries? #f
      #:make-flags
      #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'akku-fixes
            (lambda _
              (delete-file "Akku.lock")
              (substitute* "Akku.manifest"
                (("\\(depends.*") "(depends)"))
              (invoke "akku" "install")
              (let ((dest "./.akku/lib/")
                    (source "/share/guile/site/3.0/"))
                (for-each
                 (lambda (name prefix)
                   ;; Symlink the scheme libraries so that Akku can find them
                   (symlink (string-append prefix source name)
                            (string-append dest name)))
                 '("struct" "laesare" "pfds" "machine-code")
                 (list #$(this-package-native-input "guile-struct-pack")
                       #$(this-package-native-input "guile-laesare")
                       #$(this-package-native-input "guile-pfds")
                       #$(this-package-native-input "guile-machine-code"))))
              (substitute* ".akku/env"
                (("/bin/sh") (which "sh")))
              #t)))))
    (native-inputs
     (list akku
           (chez-scheme-for-system)
           guile-struct-pack
           guile-laesare
           guile-pfds
           guile-machine-code))
    (home-page "https://scheme.fail")
    (synopsis "Implementation of the algorithmic language Scheme")
    (description
     "Loko Scheme is intended to be a platform for application and operating
system development.  It is written purely in Scheme and some assembler
(i.e. no C code at the bottom).  Both the R6RS and the R7RS standards are
supported.")
    (license license:agpl3+)))
