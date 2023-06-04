;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2018, 2020-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages parallel)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix utils) #:select (target-64bit?))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freeipmi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public parallel
  (package
    (name "parallel")
    (version "20230522")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/parallel/parallel-"
                          version ".tar.bz2"))
      (sha256
       (base32 "00j2nqy808wd43hgalgry15v4ga1wrfq8s5r1cv18fq20mv6px0s"))
      (snippet
       '(begin
          (use-modules (guix build utils))
          ;; Delete pre-generated manpages and documents.
          ;; TODO: generate rst files.
          ;; parallel_cheat_bw.pdf uses libreoffice to be generated.
          (rename-file "src/parallel_cheat_bw.pdf"
                       "src/parallel_cheat_bw.pdf-keep")
          (for-each delete-file (find-files "src" "\\.(1|7|html|pdf)$"))
          (rename-file "src/parallel_cheat_bw.pdf-keep"
                       "src/parallel_cheat_bw.pdf")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bin-sh
           (lambda _
             (for-each
              (lambda (file)
                (substitute* file
                  ;; Patch hard coded '/bin/sh' in the line ending in:
                  ;; $Global::shell = $ENV{'PARALLEL_SHELL'} ||
                  ;;  parent_shell($$) || $ENV{'SHELL'} || "/bin/sh";
                  (("/bin/sh\\\";\n$") (string-append (which "sh") "\";\n"))))
              (list "src/parallel" "src/sem"))))
         (add-before 'install 'add-install-to-path
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (assoc-ref outputs "out") "/bin"))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/parallel")
                 `("PATH" ":" prefix
                   ,(map (lambda (input)
                           (string-append (assoc-ref inputs input) "/bin"))
                         '("perl"
                           "procps")))))))
         (add-after 'wrap-program 'post-install-test
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke (string-append
                      (assoc-ref outputs "out") "/bin/parallel")
                     "echo"
                     ":::" "1" "2" "3"))))))
    (native-inputs
     (list perl pod2pdf))
    (inputs
     (list bash-minimal perl procps))
    (home-page "https://www.gnu.org/software/parallel/")
    (synopsis "Build and execute command lines in parallel")
    (description
     "GNU Parallel is a tool for executing shell jobs in parallel using one
or more computers.  Jobs can consist of single commands or of scripts
and they are executed on lists of files, hosts, users or other items.")
    (license license:gpl3+)))

(define-public xe
  (package
    (name "xe")
    (version "0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leahneukirchen/xe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04jr8f6jcijr0bsmn8ajm0aj35qh9my3xjsaq64h8lwg5bpyn29x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (synopsis "Execute a command for every argument")
    (description
     "The xe utility constructs command lines from specified arguments,
combining some of the best features of xargs(1) and apply(1).  Parallel
execution is also possible.")
    (home-page "https://github.com/leahneukirchen/xe")
    (license license:public-domain)))

(define-public xjobs
  (package
    (name "xjobs")
    (version "20200726")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.maier-komor.de/xjobs/xjobs-"
                    version ".tgz"))
              (sha256
               (base32
                "0ay6gn43pnm7r1jamwgpycl67bjg5n87ncl27jb01w2x6x70z0i3"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ;; No tests
    (native-inputs
     (list flex which))
    (home-page "https://www.maier-komor.de/xjobs.html")
    (properties `((release-monitoring-url . ,home-page)))
    (synopsis
     "Parallel execution of jobs with several useful options")
    (description
     "xjobs reads job descriptions line by line and executes them in
parallel.  It limits the number of parallel executing jobs and starts new jobs
when jobs finish.")
    (license license:gpl2+)))

(define-public slurm
  (package
    (name "slurm")
    (version "22.05.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0f3hhlki8g7slllsnyj1qikbsvr62i0hig85lcdcfnmsagzlhbyi"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; According to
                  ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg00534.html>
                  ;; there are non-free bits under contribs/, though it's not
                  ;; clear which ones.  libpmi is clearly free (it used to be
                  ;; under src/api/) and so is pmi2 (lax non-copyleft
                  ;; license), so remove all of contribs/ except pmi and pmi2.
                  (substitute* "configure.ac"
                    (("^[[:space:]]+contribs/(.*)$" all directory)
                     (if (string-prefix? "pmi" directory)
                         all
                         "")))

                  (rename-file "contribs/pmi" "tmp-pmi")
                  (rename-file "contribs/pmi2" "tmp-pmi2")
                  (delete-file-recursively "contribs")
                  (mkdir "contribs")
                  (rename-file "tmp-pmi" "contribs/pmi")
                  (rename-file "tmp-pmi2" "contribs/pmi2")))))
    ;; FIXME: More optional inputs could be added,
    ;; in particular mysql and gtk+.
    (inputs (list freeipmi
                  `(,hwloc-2 "lib")
                  json-c
                  linux-pam
                  munge
                  numactl
                  readline))
    (native-inputs
     (list autoconf expect perl pkg-config python-wrapper))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-pam" "--sysconfdir=/etc/slurm"
                   "--disable-static"
                   (string-append "--with-freeipmi=" #$(this-package-input "freeipmi"))
                   (string-append "--with-hwloc="
                                  (ungexp (this-package-input "hwloc") "lib"))
                   (string-append "--with-json=" #$(this-package-input "json-c"))
                   (string-append "--with-munge=" #$(this-package-input "munge"))

                   ;; 32-bit support is marked as deprecated and needs to be
                   ;; explicitly enabled.
                   #$@(if (target-64bit?) '() '("--enable-deprecated")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-plugin-linker-flags
                 (lambda _
                   (substitute* (find-files "src/plugins/" "Makefile.in")
                     (("_la_LDFLAGS = ")
                      "_la_LDFLAGS = ../../../api/libslurm.la "))))
               (add-after 'patch-plugin-linker-flags 'autoconf
                 (lambda _ (invoke "autoconf")))  ;configure.ac was patched
               (add-after 'install 'install-libpmi
                 (lambda _
                   ;; Open MPI expects libpmi to be provided by Slurm so install it.
                   (invoke "make" "install" "-C" "contribs/pmi")

                   ;; Others expect pmi2.
                   (invoke "make" "install" "-C" "contribs/pmi2"))))))
    (home-page "https://slurm.schedmd.com/")
    (synopsis "Workload manager for cluster computing")
    (description
     "SLURM is a fault-tolerant and highly scalable cluster management and job
scheduling system for large and small clusters.  It allocates access to
resources (computer nodes) to users for some duration of time, provides a
framework for starting, executing, and monitoring work (typically a parallel
job) on a set of allocated nodes, and arbitrates contention for resources
by managing a queue of pending work.")
    (license (list license:bsd-2      ; src/common/log.[ch], src/common/uthash
                   license:expat      ; slurm/pmi.h
                   license:isc        ; src/common/strlcpy.c
                   license:lgpl2.1+   ; hilbert.[ch], src/common/slurm_time.h
                   license:zlib       ; src/common/strnatcmp.c
                   (license:non-copyleft    ;contribs/pmi2, Argonne Natl. Lab.
                    "https://github.com/SchedMD/slurm/blob/master/contribs/pmi2/COPYRIGHT")
                   license:gpl2+))))   ; the rest, often with OpenSSL exception

;; The SLURM client/daemon protocol and file format changes from time to time
;; in incompatible ways, as noted in
;; <https://slurm.schedmd.com/troubleshoot.html#network>.  Thus, keep older
;; releases here.  See also <https://issues.guix.gnu.org/44387>.
;; As noted in the link, YY.MM is the release scheme, and the 'maintenance'
;; digit does not introduce incompatibilities.

(define-public slurm-21.08
  (package
    (inherit slurm)
    (version "21.08.8")
    (source (origin
              (inherit (package-source slurm))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1sjln54idc9rhg8f2nvm38sgs6fncncyzslas8ixy65pqz2hphbf"))))))

(define-public slurm-20.11
  (package
    (inherit slurm)
    (version "20.11.9")
    (source (origin
              (inherit (package-source slurm))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0xq2d6dm285y541dyg1h66z7svsisrq8c81ag0f601xz1cn3mq9m"))))))

(define-public slurm-20.02
  (package
    (inherit slurm)
    (version "20.02.6-1")
    (source (origin
              (inherit (package-source slurm))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0qj4blfymrd2ry2qmb58l3jbr4jwygc3adcfw7my27rippcijlyc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments slurm)
       ((#:configure-flags flags ''())
        #~(append '("CFLAGS=-O2 -g -fcommon" "LDFLAGS=-fcommon")
                  #$flags))))))

(define-public slurm-19.05
  (package
    (inherit slurm-20.02)
    (version "19.05.8")
    (source (origin
              (inherit (package-source slurm))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "10c9j4a9a6d4ibpf75006mn03p8xgpaprc247x2idakysjf2fw43"))))))

;; Same as Debian 10
(define-public slurm-18.08
  (package
    (inherit slurm-20.02)
    (version "18.08.9")
    (source
      (origin
        (inherit (package-source slurm))
        (uri (string-append
               "https://download.schedmd.com/slurm/slurm-"
               version ".tar.bz2"))
        (sha256
         (base32
          "1bgrpz75m7l4xhirsd0fvnkzlkrl8v2qpmjcz60barc5qm2kn457"))))))

(define-public slurm-drmaa
  (package
    (name "slurm-drmaa")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/natefoo/slurm-drmaa/releases/download/"
                    version "/slurm-drmaa-" version ".tar.gz"))
              (sha256
               (base32
                "1fn3p4wjj0sgvx0isy3hiwi35vhxa2n2ksq5cn9sq2hg7yyb2phl"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; The tests require "bats".
    (inputs
     (list slurm))
    (native-inputs
     (list which))
    (home-page "https://github.com/natefoo/slurm-drmaa")
    (synopsis "Distributed resource management application API for SLURM")
    (description
     "PSNC DRMAA for Simple Linux Utility for Resource Management (SLURM) is
an implementation of Open Grid Forum DRMAA 1.0 (Distributed Resource
Management Application API) specification for submission and control of jobs
to SLURM.  Using DRMAA, grid applications builders, portal developers and ISVs
can use the same high-level API to link their software with different
cluster/resource management systems.")
    (license license:gpl3+)))

(define-public python-slurm-magic
  (let ((commit "73dd1a2b85799f7dae4b3f1cd9027536eff0c4d7")
        (revision "0"))
    (package
      (name "python-slurm-magic")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/NERSC/slurm-magic")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "19pp2vs0wm8mx0arz9n6lw9wgyv70w9wyi4y6b91qc5j3bz5igfs"))
                (file-name (git-file-name name version))))
      (build-system python-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-slurm-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; The '_execute' method tries to exec 'salloc'
                        ;; etc. from $PATH.  Record the absolute file name
                        ;; instead.
                        (let ((slurm (assoc-ref inputs "slurm")))
                          (substitute* "slurm_magic.py"
                            (("name = (.*)$" _ value)
                             (string-append "name = \""
                                            slurm "/bin/\" + "
                                            value "\n")))
                          #t))))))
      (inputs
       (list slurm))
      (propagated-inputs
       (list python-ipython python-pandas))
      (synopsis "Control the SLURM batch scheduler from Jupyter Notebook")
      (description
       "This package implements Jupyter/IPython
@uref{http://ipython.readthedocs.io/en/stable/interactive/magics.html, magic
commands} for interacting with the SLURM workload manager.  SLURM magic simply
wraps command-line executables and the commands themselves should look like
their command-line counterparts.  Commands are spawned via @code{subprocess}
and output captured in the notebook.  Whatever arguments are accepted by a
SLURM command line executable are also accepted by the corresponding magic
command---e.g., @code{%salloc}, @code{%sbatch}, etc.")
      (license license:bsd-3))))

(define-public pthreadpool
  ;; This repository has only one tag, 0.1, which is older than what users
  ;; such as XNNPACK expect.
  (let ((commit "1787867f6183f056420e532eec640cba25efafea")
        (version "0.1")
        (revision "1"))
    (package
      (name "pthreadpool")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/pthreadpool")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02hdvxfn5krw8zivkgjx3b4rk9p02yr4mpdjlp75lsv6z1xf5yrx"))
                (patches (search-patches "pthreadpool-system-libraries.patch"))))
      (build-system cmake-build-system)
      (arguments '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
      (inputs
       (list googletest googlebenchmark fxdiv))
      (synopsis "Efficient thread pool implementation")
      (description
       "The pthreadpool library implements an efficient and portable thread
pool, similar to those implemented by OpenMP run-time support libraries for
constructs such as @code{#pragma omp parallel for}, with additional
features.")
      (license license:bsd-2))))

(define-public cpuinfo
  ;; There's currently no tag on this repo.
  (let ((version "0.0")
        (revision "2")
        (commit "53298db833c5c5a1598639e9b47cc1a602bbac26"))
    (package
      (name "cpuinfo")
      (version (git-version version revision commit))
      (home-page "https://github.com/pytorch/cpuinfo")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01kfgxya2w32dz9bd3qm3i2d6nffw0qfyql11rxl7d3g830brj5k"))
                (patches (search-patches "cpuinfo-system-libraries.patch"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags '(list "-DBUILD_SHARED_LIBS=ON")
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'skip-bad-test
             (lambda _
               (substitute* "test/init.cc"
                 (("TEST\\(CORE, known_uarch\\) \\{" m)
                  (string-append m "\
GTEST_SKIP() << \"See https://github.com/pytorch/cpuinfo/issues/132\";"))))))))
      (inputs
       (list googletest googlebenchmark))
      (synopsis "C/C++ library to obtain information about the CPU")
      (description
       "The cpuinfo library provides a C/C++ and a command-line interface to
obtain information about the CPU being used: supported instruction set,
processor name, cache information, and topology information.")
      (license license:bsd-2))))

(define-public clog
  (package
    (inherit cpuinfo) ;distributed with cpuinfo but not built by it
    (name "clog")
    (source (origin
              (inherit (package-source cpuinfo))
              (patches (search-patches "clog-fix-shared-build.patch"))))
    (arguments
     (list #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "deps/clog"))))))
    (native-inputs (list googletest))
    (inputs '())
    (synopsis "C-style logging library based on printf")
    (description
     "This package provides a C-style library for logging errors,
warnings, information notes, and debug information.  Its features are:
@itemize
@item printf-style interface for formatting variadic parameters.
@item Separate functions for logging errors, warnings, information notes, and
debug information.
@item Independent logging settings for different modules.
@item Logging to logcat on Android and stderr/stdout on other platforms.
@item Compatible with C99 and C++.
@item Covered with unit tests.
@end itemize")))

(define-public psimd
  ;; There is currently no tag in this repo.
  (let ((commit "072586a71b55b7f8c584153d223e95687148a900")
        (version "0.0")
        (revision "1"))
    (package
      (name "psimd")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/Psimd")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16mslhvqs0gpqbg7kkq566a8gkn58cgjpqca8ljj9qcv5mk9apwm"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f))                  ;there are no tests
      (synopsis "Portable 128-bit SIMD intrinsics")
      (description
       "This header-only C++ library provides a portable interface to
single-instruction multiple-data (SIMD) intrinsics.")
      (license license:expat))))
