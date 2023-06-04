;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2020 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2022 Zhu Zihao  <all_but_last@163.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer  <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages pretty-print)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages swig))

(define-public a2ps
  (package
    (name "a2ps")
    (version "4.15.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/a2ps/a2ps-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mvd41xvcy8vk91nndzifasq600kzlswl1379bhnpn49pa23y1ja"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove timestamp from the installed 'README' file.
               #~(begin
                   (substitute* "etc/README.in"
                     (("@date@")
                      "1st of some month, sometime after 1970"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-failing-tests
            (lambda _
              (substitute* (list "tests/Makefile.am" "tests/Makefile.in")
                (("(encoding|prolog-2)\\.tst") ""))))
          (add-before 'build 'patch-scripts
            (lambda _
              (substitute*
                  '("afm/make_fonts_map.sh"
                    "tests/defs"
                    "tests/backup.tst"
                    "tests/styles.tst")
                (("/bin/rm") (which "rm")))))
          (add-before 'check 'patch-test-files
            ;; Alternatively, we could unpatch the shebangs in tst files.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("tests/ps-ref/includeres.ps"
                             "tests/gps-ref/includeres.ps")
                (("/usr/local/bin/perl")
                 (search-input-file inputs "/bin/perl")))
              ;; Some of the reference postscript contain a 'version 3'
              ;; string that in inconsistent with the source text in the
              ;; tstfiles directory.  Erroneous search-and-replace?
              (substitute* '("tests/ps-ref/InsertBlock.ps"
                             "tests/gps-ref/InsertBlock.ps"
                             "tests/ps-ref/bookie.ps"
                             "tests/gps-ref/bookie.ps")
                (("version 3") "version 2"))
              (substitute* '("tests/ps-ref/psmandup.ps"
                             "tests/gps-ref/psmandup.ps")
                (("#! */bin/sh")
                 (string-append "#!" (which "sh")))))))))
    (native-inputs
     (list gperf groff perl pkg-config))
    (inputs
     (list file gv libgc libpaper psutils))
    (home-page "https://www.gnu.org/software/a2ps/")
    (synopsis "Any file to PostScript, including pretty-printing")
    (description
     "GNU a2ps converts almost anything to a PostScript file, ready for
printing.  It accomplishes this by being able to delegate files to external
handlers, such as Groff and Gzip.  It handles as many steps as is necessary to
produce a pretty-printed file.  It also includes some extra abilities for
special cases, such as pretty-printing @samp{-help} output.")
    (license gpl3+)))

(define-public trueprint
  (package
    (name "trueprint")
    (version "5.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/trueprint/trueprint-"
                          version ".tar.gz"))
      (sha256
       (base32
        "13rkc0fga10xyf56yy9dnq95zndnfadkhxflnp24skszj21y8jqh"))))
    (build-system gnu-build-system)
    (arguments
     ;; Must define DIFF_CMD for tests to pass
     '(#:configure-flags '("CPPFLAGS=-DDIFF_CMD=\\\"diff\\\"")))
    (home-page "https://www.gnu.org/software/trueprint/")
    (synopsis "Pretty-print C sources and other plain text to PostScript")
    (description
     "GNU Trueprint translates C source code files as PostScript files.
In addition to the basic source code output, it can also perform diff-marking,
indentation counting, function and file indices and more.")
    (license gpl2)))

(define-public enscript
  (package
    (name "enscript")
    (version "1.6.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/enscript/enscript-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1fy0ymvzrrvs889zanxcaxjfcxarm2d3k43c9frmbl1ld7dblmkd"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/enscript/")
    (synopsis "Generating PostScript, including pretty-printing")
    (description
     "GNU Enscript is a program to convert ASCII text files to PostScript,
HTML or RTF formats, to be stored in files or sent immediately to a printer.
It also includes the capability to perform syntax highlighting for several
different programming languages.")
    (license gpl3+)))

(define-public fmt
  (package
    (name "fmt")
    (version "9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fmtlib/fmt/releases/download/"
                           version "/fmt-" version ".zip"))
       (sha256
        (base32 "15n9yi6xzzs7g9rm87kg8y5yhl2zrqj3bjr845saa63f6swlrsyc"))))
    (build-system cmake-build-system)
    (arguments '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs (list unzip))
    (home-page "https://fmt.dev")
    (synopsis "Small and fast C++ formatting library")
    (description "@code{fmt} (formerly @code{cppformat}) is a formatting
library for C++.  It can be used as a safe alternative to @code{printf} or as
a fast alternative to @code{IOStreams}.")
    ;; The library is bsd-2, but documentation and tests include other licenses.
    (license (list bsd-2 bsd-3 psfl))))

(define-public fmt-8
  (package
    (inherit fmt)
    (version "8.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fmtlib/fmt/releases/download/"
                           version "/fmt-" version ".zip"))
       (sha256
        (base32 "0p8f82ijqa57sk72hjf0qviv1wwinmns0p87wiv2v8fvisnqnxr3"))))))

(define-public fmt-8.0
  (package
    (inherit fmt)
    (version "8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fmtlib/fmt/releases/download/"
                           version "/fmt-" version ".zip"))
       (sha256
        (base32 "1gqmsk4r93x65cqs8w7zhfiv70w5fv8279nrblggqm4mmdpaa9x6"))))))

(define-public fmt-7
  (package
    (inherit fmt)
    (version "7.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fmtlib/fmt/releases/download/"
                           version "/fmt-" version ".zip"))
       (sha256
        (base32 "17sc10hfg087z0s774lnn05wwy3bfzmcv7j448p92pr0s02cb62x"))))))

(define-public fmt-6
  (package
    (inherit fmt)
    (version "6.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fmtlib/fmt/releases/download/"
                           version "/fmt-" version ".zip"))
       (sha256
        (base32 "1s1hxaby5byb07rgmrk4a0q11fxhz7b42khch7sp2qx974y0yrb3"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; TODO: posix-mock-test segfaults
       #:configure-flags
       '("-DBUILD_SHARED_LIBS=ON"
         "-DCMAKE_CXX_COMPILER=clang++"
         "-DCMAKE_CXX_FLAGS=-stdlib=libc++"
         "-DCMAKE_EXE_LINKER_FLAGS=-lc++abi")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (cons (search-input-directory inputs "/include/c++/v1")
                              ;; Hide GCC's C++ headers so that they do not interfere with
                              ;; the Clang headers.
                              (delete (string-append gcc "/include/c++")
                                      (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                    #\:)))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))))))))
    (properties `((hidden? . #true)))
    (native-inputs
     (list unzip))
    (inputs
     `(("libcxx" ,libcxx+libcxxabi-6)
       ("libcxxabi" ,libcxxabi-6)
       ("clang" ,clang-6)))))

(define-public source-highlight
  (package
    (name "source-highlight")
    (version "3.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/src-highlite/source-highlight-"
                           version ".tar.gz"))
       (patches (search-patches "source-highlight-gcc-compat.patch"))
       (sha256
        (base32
         "148w47k3zswbxvhg83z38ifi85f9dqcpg7icvvw1cm6bg21x4zrs"))))
    (build-system gnu-build-system)
    ;; The ctags that comes with emacs does not support the --excmd options,
    ;; so can't be used
    (inputs
     (list boost))
    (native-inputs
     (list bison flex))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-boost=" (assoc-ref %build-inputs "boost")))
           #:parallel-tests? #f         ;There appear to be race conditions
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'rename-lesspipe-to-lesspipe.sh.in
                 (lambda _
                   (substitute* "src/src-hilite-lesspipe.sh.in"
                     (("lesspipe") "lesspipe.sh"))))
               (add-before 'build 'skip-doc-directory
                 (lambda _
                   (substitute* "Makefile"
                     (("^SUBDIRS = (.*) doc(.*)$" _ before after)
                      (string-append "SUBDIRS = " before
                                     " " after "\n")))))
               (add-before 'check 'patch-test-files
                 (lambda _
                   ;; Unpatch shebangs in test input so that source-highlight
                   ;; is still able to infer input language
                   (substitute* '("tests/test.sh"
                                  "tests/test2.sh"
                                  "tests/test.tcl")
                     (((string-append "#! *" (which "sh"))) "#!/bin/sh"))
                   ;; Initial patching unrecoverably removes whitespace, so
                   ;; remove it also in the comparison output.
                   (substitute* '("tests/test.sh.html"
                                  "tests/test2.sh.html"
                                  "tests/test.tcl.html")
                     (("#! */bin/sh") "#!/bin/sh")))))))
    (home-page "https://www.gnu.org/software/src-highlite/")
    (synopsis "Produce a document with syntax highlighting from a source file")
    (description
     "GNU source-highlight reads in a source code file and produces an output
file in which the keywords are highlighted in different colors to designate
their syntactic role.  It supports over 150 different languages and it can
output to 8 different formats, including HTML, LaTeX and ODF.  It can also
output to ANSI color escape sequences, so that highlighted source code can be
seen in a terminal.")
    (license gpl3+)
    (properties '((ftp-directory . "/gnu/src-highlite")))))

(define-public highlight
  (package
    (name "highlight")
    (version "3.62")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.andre-simon.de/zip/highlight-"
                           version ".tar.bz2"))
       (sha256
        (base32 "088di7qxd6b2r22qljllhnly0r9a0lfnwnfqswjn23s09awjbl6p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (let ((confdir (string-append %output "/share/highlight/config/")))
         (list (string-append "PREFIX=" %output)
               (string-append "HL_CONFIG_DIR=" confdir)
               (string-append "conf_dir=" confdir)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'fix-search-for-lua
           (lambda _
             (substitute* "src/makefile"
               (("(LUA_PKG_NAME=).*" _ assignment)
                (string-append assignment "lua-" ,(version-major+minor
                                                   (package-version lua))
                               "\n")))
             (substitute* "extras/swig/makefile"
               (("lua") (string-append "lua-" ,(version-major+minor
                                                (package-version lua)))))
             #t))
         (add-after 'install 'install-perl-bindings
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((perldir (string-append (assoc-ref outputs "out")
                                            "/lib/perl5/site_perl/"
                                            ,(package-version perl)))
                    (autodir (string-append perldir "/auto/highlight")))
               (with-directory-excursion "extras/swig"
                 (invoke "make" "perl")
                 (invoke "perl" "-I" "." "testmod.pl")
                 (install-file "highlight.pm" perldir)
                 (install-file "highlight.so" autodir))
               #t))))))
    (inputs
     (list lua boost perl))
    (native-inputs
     (list pkg-config swig))
    (home-page "http://www.andre-simon.de/doku/highlight/en/highlight.php")
    (synopsis "Convert code to documents with syntax highlighting")
    (description "Highlight converts source code to HTML, XHTML, RTF, LaTeX,
TeX, SVG, BBCode and terminal escape sequences with colored syntax
highlighting.  Language definitions and color themes are customizable.")
    (license gpl3+)))
