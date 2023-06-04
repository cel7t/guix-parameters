;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages popt)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages texinfo))

(define-public argtable
  (package
    (name "argtable")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/argtable/argtable/"
                    "argtable-" version "/argtable"
                    (string-join (string-split version #\.) "-")
                    ".tar.gz"))
             (sha256
              (base32
               "1gyxf4bh9jp5gb3l6g5qy90zzcf3vcpk0irgwbv1lc6mrskyhxwg"))))
    (build-system gnu-build-system)
    (home-page "https://argtable.sourceforge.net/")
    (synopsis "Command line option parsing library")
    (description
     "Argtable is an ANSI C library for parsing GNU style command line
options.  It enables a program's command line syntax to be defined in the
source code as an array of argtable structs.  The command line is then parsed
according to that specification and the resulting values are returned in those
same structs where they are accessible to the main program.  Both tagged (-v,
--verbose, --foo=bar) and untagged arguments are supported, as are multiple
instances of each argument.  Syntax error handling is automatic and the library
also provides the means for generating a textual description of the command
line syntax.")
    (license lgpl2.0+)))

(define-public popt
  (package
    (name "popt")
    (version "1.18")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://ftp.rpm.org/popt/releases"
                                 "/popt-1.x/popt-" version ".tar.gz"))
             (sha256
              (base32
               "1lf5zlj5rbg6s4bww7hbhpca97prgprnarx978vcwa0bl81vqnai"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-test
           (lambda _
             (substitute* "tests/test-poptrc.in"
               (("/bin/echo") (which "echo")))
             (substitute* "tests/testit.sh"   ;don't expect old libtool names
               (("lt-test1") "test1"))
             #t)))))
    (home-page "https://rpm5.org/files/popt/")
    (synopsis "Command line option parsing library")
    (description
     "This is the popt(3) command line option parsing library.  While it is
similar to getopt(3), it contains a number of enhancements, including:

  - popt is fully reentrant;

  - popt can parse arbitrary argv[] style arrays while getopt(3) makes this
    quite difficult;

  - popt allows users to alias command line arguments;

  - popt provides convenience functions for parsing strings into argv[] style
    arrays.")
    (license x11)))

(define-public gflags
  (package
    (name "gflags")
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gflags/gflags")
             (commit (string-append "v" version))))
       (sha256
        (base32 "147i3md3nxkjlrccqg4mq1kyzc7yrhvqv5902iibc7znkvzdvlp0"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DBUILD_TESTING=ON")))
    (home-page "https://gflags.github.io/gflags/")
    (synopsis "C++ library for command-line argument parsing")
    (description
     "Gflags is a C++ library to parse command-line flags.  It differs from
other such libraries in that command-line flag definitions can be scattered
around the source code, and not just listed in one place such as @code{main}.
This means that a single source-code file will define and use flags that are
meaningful to that file.  Any application that links in that file will get the
flags, and the gflags library will automatically handle that flag
appropriately.")
    (license bsd-3)))

(define-public gengetopt
  (package
    (name "gengetopt")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gengetopt/gengetopt-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1b44fn0apsgawyqa4alx2qj5hls334mhbszxsy6rfr0q074swhdr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f             ; not supported
       #:parallel-tests? #f))           ; likewise
    (native-inputs
     (list texinfo))
    (synopsis "Create parsers for command line options")
    (description
     "GNU Gengetopt is a program to generate a C/C++ function for parsing
command-line options using the getopt_long function found in GNU
libc, removing some of the tedium of this task for large programs
that accept many options.  The options parsed by the generated
function may be in both short (e.g., \"-h\") and long (\"--help\")
formats, as specified by the GNU coding standards.  Additionally, the
output of the standard options \"--help\" and \"--version\" is generated
automatically.")
    (home-page "https://www.gnu.org/software/gengetopt/gengetopt.html")
    (license gpl3+)))
