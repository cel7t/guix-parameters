;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages logging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls))

(define-public log4cpp
  (package
    (name "log4cpp")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/log4cpp/log4cpp-"
                                  (version-major+minor version) ".x%20%28new%29"
                                  "/log4cpp-" (version-major+minor version)
                                  "/log4cpp-" version ".tar.gz"))
              (sha256
               (base32
                "07gmr3jyaf2239n9sp6h7hwdz1pv7b7aka8n06gmr2fnlmaymfrc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-call-stime
           (lambda _
             ;; Patch out use of 'stime' which was removed from glibc 2.31.
             ;; The test would not work in the build container anyway.
             (substitute* "tests/testDailyRollingFileAppender.cpp"
               (("if \\(stime\\(&now\\) == -1\\)")
                "if (1)"))
             #t)))))
    (synopsis "Log library for C++")
    (description
     "Log4cpp is library of C++ classes for flexible logging to files, syslog,
IDSA and other destinations.  It is modeled after the Log4j Java library,
staying as close to their API as is reasonable.")
    (home-page "https://log4cpp.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public glog
  (package
    (name "glog")
    (version "0.5.0")
    (home-page "https://github.com/google/glog")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (sha256
               (base32
                "17014q25c99qyis6l3fwxidw6222bb269fdlr74gn7pzmzg4lvg3"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list perl ;for tests
           autoconf automake libtool))
    (synopsis "C++ logging library")
    (description
     "Google glog is a library that implements application-level logging.
This library provides logging APIs based on C++-style streams and various
helper macros.  You can log a message by simply streaming things to log at a
particular severity level.  It allows logging to be controlled from the
command line.")
    (license license:bsd-3)))

;; This is the legacy version of the tailon package.  The new version, written
;; in Go in available here: https://github.com/gvalkov/tailon.
(define-public tailon
  (package
    (name "tailon")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "0xkmrivzilsc9wqr8ms67v7399gxnh7pv5687k4rdpdgz4309fwc"))))
    (build-system python-build-system)
    (native-inputs
     (list python-tox python-wheel))
    (inputs
     (list python-pyyaml-5 python-sockjs-tornado python-tornado-http-auth
           python-tornado python-deepmerge))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-commands.py
           (lambda args
             (substitute* "tailon/commands.py"
               (("self\\.first_in_path\\('grep'\\)")
                (string-append"'" (which "grep") "'"))
               (("self\\.first_in_path\\('gawk', 'awk'\\)")
                (string-append"'" (which "gawk") "'"))
               (("self\\.first_in_path\\('gsed', 'sed'\\)")
                (string-append"'" (which "sed") "'"))
               (("self\\.first_in_path\\('gtail', 'tail'\\)")
                (string-append"'" (which "tail") "'")))))
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ((",<5.0.0") "")))))))
    (home-page "https://tailon.readthedocs.io/")
    (synopsis
     "Webapp for looking at and searching through log files")
    (description
     "Tailon provides a web interface around the tail, grep, awk and sed
commands, displaying the results via a web interface.")
    (license license:bsd-3)))

(define-public multitail
  (package
    (name "multitail")
    (version "6.5.2")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/halturin/multitail")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "17hg5qpangyx4m7hp2x4h56mp6w3wsaslg1il39qcpwsffh1rihc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "SYSCONFDIR=$(PREFIX)/etc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-build
           ;; With some luck, you might be able to remove this when updating…
           (lambda _
             (substitute* "Makefile"
               ((" \\*\\.txt") "")
               ((".*CONFIG_DIR.*") "")
               (("^install: .*" match)
                (string-append match
                               "\t$(INSTALL_DIR) $(DESTDIR)$(SYSCONFDIR)\n")))
             (substitute* "version"
               (("(VERSION=).*" _ assign)
                (string-append assign ,version)))))
         (add-after 'unpack 'patch-curses-headers
           (lambda _
             (substitute* "mt.h"
               (("ncursesw/") ""))))
         (delete 'configure))           ; no configure script
       #:tests? #f)) ; no test suite (make check just runs cppcheck)
    (inputs (list ncurses))
    (home-page "https://vanheusden.com/multitail/")
    (synopsis "Monitor multiple log files")
    (description
     "MultiTail can monitor, color, filter, and merge log files and command
output in multiple windows in a terminal.")
    (license license:gpl2+)))

(define-public spdlog
  (package
    (name "spdlog")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gabime/spdlog")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i3a1cqrg1sz0w50g7zz9x73rf838igqri12q8ijh4rzpq0qq3ch"))))
    (build-system cmake-build-system)
    ;; TODO run benchmark. Currently not possible, as adding
    ;; (gnu packages benchmark) forms a dependency cycle
    (arguments
     '(#:configure-flags
       (list "-DSPDLOG_BUILD_BENCH=OFF"
             "-DSPDLOG_BUILD_SHARED=ON"
             "-DSPDLOG_BUILD_TESTS=ON")))
    (home-page "https://github.com/gabime/spdlog")
    (synopsis "Fast C++ logging library")
    (description "Spdlog is a very fast header-only/compiled C++ logging
library.")
    ;; spdlog is under Expat license, but the bundled fmt library in
    ;; "include/spdlog/fmt/bundled" is under BSD 2 clause license.
    (license (list license:expat license:bsd-2))))

(define-public spdlog-1.10
  (package
    (inherit spdlog)
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gabime/spdlog")
             (commit (string-append "v" version))))
       (file-name (git-file-name "spdlog" version))
       (sha256
        (base32 "02xz017ba9fssm1rp1fcfld7h79awbr6fqai9dxaqp02akp3davk"))))))

(define-public rsyslog
  (package
    (name "rsyslog")
    (version "8.2204.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/rsyslog.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bsd1n3n4hvlkwf4g85g3fg37mnvkdmxsfdmg273gcachhyl5hbx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))
      #:configure-flags
      ;; Rsyslog comes with a plethora of optional modules.  We enable most of
      ;; them for a full-featured build.
      '(list "--enable-kmsg"
             "--enable-liblogging_stdlog"
             "--enable-unlimited_select"
             "--enable-usertools"

             ;; Input plugins
             "--enable-imbatchreport"
             "--enable-imczmq"
             "--enable-imdiag"          ;for full tests
             "--enable-imdocker"
             "--enable-imfile"
             "--enable-imkafka"
             "--enable-improg"
             "--enable-impstats"
             "--enable-imptcp"
             "--enable-imtuxedoulog"

             ;; Output plugins
             "--enable-clickhouse"
             "--enable-elasticsearch"
             "--enable-mail"
             "--enable-omczmq"
             "--enable-omfile_hardened"
             "--enable-omhttp"
             "--enable-omhttpfs"
             "--enable-omkafka"
             "--enable-omprog"
             "--enable-omruleset"
             "--enable-omstdout"
             "--enable-omtcl"
             "--enable-omudpspoof"
             "--enable-omuxsock"

             ;; Parser Modules
             "--enable-pmaixforwardedfrom"
             "--enable-pmciscoios"
             "--enable-pmcisconames"
             "--enable-pmdb2diag"
             "--enable-pmlastmsg"
             "--enable-pmnormalize"
             "--enable-pmnull"
             "--enable-pmpanngfw"
             "--enable-pmsnare"

             ;; Message Modification Modules
             "--enable-mmanon"
             "--enable-mmaudit"
             "--enable-mmcount"
             "--enable-mmdarwin"
             "--enable-mmdblookup"
             "--enable-mmfields"
             "--enable-mmjsonparse"
             "--enable-mmkubernetes"
             "--enable-mmnormalize"
             "--enable-mmpstrucdata"
             "--enable-mmrfc5424addhmac"
             "--enable-mmrm1stspace"
             "--enable-mmsequence"
             "--enable-mmsnmptrapd"
             "--enable-mmtaghostname"
             "--enable-mmutf8fix"

             ;; Database Support
             "--enable-libdbi"
             "--enable-mysql"
             "--enable-pgsql"

             ;; Protocol Support
             "--enable-openssl"
             "--enable-gnutls"
             "--enable-gssapi-krb5"
             "--enable-snmp"

             ;; Function modules
             "--enable-fmhash_xxhash"

             ;; Needed to build rscryutil.1.gz.
             "--enable-generate-man-pages")))
    (native-inputs
     (list autoconf
           automake
           bison
           flex
           libtool
           pkg-config
           python-docutils))            ; rst2man for man pages
    (inputs
     (list curl
           cyrus-sasl
           czmq
           gnutls
           libdbi
           libestr
           libfastjson
           libgcrypt
           liblogging
           liblognorm
           libmaxminddb
           libnet
           librdkafka
           lz4
           (list mariadb "dev")
           (list mariadb "lib")
           mit-krb5
           net-snmp
           openssl
           postgresql
           tcl
           (list util-linux "lib")
           zeromq
           zlib))
    (home-page "https://www.rsyslog.com/")
    (synopsis "RSYSLOG is a flexible and fast system for log processing")
    (description
     "Rsyslog offers high-performance, great security features and a modular
design.  While it started as a regular syslogd, rsyslog has evolved into a
kind of swiss army knife of logging, being able to accept inputs from a wide
variety of sources, transform them, and output the results to diverse
destinations.")
    ;; Most of the source code is licensed under the LGPL3+ with many source
    ;; files licensed under the terms of the ASL2.0.  Some modules are
    ;; licensed under GPL3+.
    (license (list license:lgpl3+
                   license:gpl3+
                   license:asl2.0))))
