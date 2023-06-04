;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2020, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Arthur Margerit <ruhtra.mar@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Saku Laesvuori <saku@laesvuori.fi>
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

(define-module (gnu packages glib)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:use-module ((srfi srfi-1) #:hide (zip))

  ;; Export variables up-front to allow circular dependency with the 'xorg'
  ;; module.
  #:export (dbus
            glib
            gobject-introspection
            dbus-glib
            intltool
            itstool
            libsigc++
            glibmm
            telepathy-glib
            perl-net-dbus
            perl-net-dbus-glib))

(define dbus
  (package
    (name "dbus")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dbus.freedesktop.org/releases/dbus/dbus-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1m7bibavml4gx9d67j403l0kzd1a4z8lhrpxb2as3q4nfpiwrmyc"))
              (patches (search-patches "dbus-helper-search-path.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list
        ;; Install the system bus socket under /var.
        "--localstatedir=/var"

        ;; Install the session bus socket under /tmp.
        "--with-session-socket-dir=/tmp"

        ;; Build shared libraries only.
        "--disable-static"

        ;; Use /etc/dbus-1 for system-wide config.
        ;; Look for configuration file under
        ;; /etc/dbus-1.  This is notably required by
        ;; 'dbus-daemon-launch-helper', which looks for
        ;; the 'system.conf' file in that place,
        ;; regardless of what '--config-file' was
        ;; passed to 'dbus-daemon' on the command line;
        ;; see <https://bugs.freedesktop.org/show_bug.cgi?id=92458>.
        "--sysconfdir=/etc")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
                  (lambda _
                    ;; Don't try to create /var and /etc.
                    (invoke "make"
                            "localstatedir=/tmp/dummy"
                            "sysconfdir=/tmp/dummy"
                            "install"))))))
    (native-inputs
     (list pkg-config
           ;; Dependencies to generate the doc.
           docbook-xml-4.4
           docbook-xsl
           doxygen
           xmlto
           libxml2 ;for XML_CATALOG_FILES
           libxslt
           yelp-tools))
    (inputs
     (list expat
           ;; Add a dependency on libx11 so that 'dbus-launch' has support for
           ;; '--autolaunch'.
           libx11))
    (outputs '("out" "doc"))            ;22 MiB of HTML doc
    (home-page "https://www.freedesktop.org/wiki/Software/dbus/")
    (synopsis "Message bus for inter-process communication (IPC)")
    (description
     "D-Bus is a message bus system, a simple way for applications to
talk to one another.  In addition to interprocess communication, D-Bus
helps coordinate process lifecycle; it makes it simple and reliable to
code a \"single instance\" application or daemon, and to launch
applications and daemons on demand when their services are needed.

D-Bus supplies both a system daemon (for events such as \"new hardware
device added\" or \"printer queue changed\") and a
per-user-login-session daemon (for general IPC needs among user
applications).  Also, the message bus is built on top of a general
one-to-one message passing framework, which can be used by any two apps
to communicate directly (without going through the message bus
daemon).  Currently the communicating applications are on one computer,
or through unencrypted TCP/IP suitable for use behind a firewall with
shared NFS home directories.")
    (license license:gpl2+)))                     ; or Academic Free License 2.1

;;; This variant is used for the Jami service: it provides an entry point to
;;; further customize the configuration of the D-Bus instance run by the
;;; jami-dbus-session service.
(define-public dbus-for-jami
  (hidden-package
   (package/inherit dbus
     (name "dbus-for-jami")
     (arguments
      (substitute-keyword-arguments (package-arguments dbus)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'customize-config
              (lambda _
                (substitute* "bus/session.conf.in"
                  (("@SYSCONFDIR_FROM_PKGDATADIR@/dbus-1/session-local.conf")
                   "/var/run/jami/session-local.conf")))))))))))

;;; The reason this is not enabled in the regular dbus package is because it
;;; impacts the performance of D-Bus (including its library) as a whole, even
;;; when the DBUS_VERBOSE environment variable is not set.
(define-public dbus-verbose
  (package/inherit dbus
    (name "dbus-verbose")
    (arguments (substitute-keyword-arguments (package-arguments dbus)
                 ((#:configure-flags flags '())
                  `(cons "--enable-verbose-mode" ,flags))))
    (synopsis "D-Bus with verbose mode enabled for debugging")
    (description "This variant D-Bus package is built with verbose mode, which
eases debugging of D-Bus services by printing various debug information when
the @code{DBUS_VERBOSE} environment variable is set to @samp{1}.  For more
information, refer to the @samp{dbus-daemon(1)} man page.")))

(define glib
  (package
    (name "glib")
    (version "2.72.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/"
                       name "/" (string-take version 4) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1w25sf2wxkkah2p2w189q58mza3zv8z1fh2q1m82sldq4kva4faa"))
       (patches
        (search-patches "glib-appinfo-watch.patch"
                        "glib-skip-failing-test.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "tests/spawn-test.c"
             (("/bin/sh") "sh"))))))
    (build-system meson-build-system)
    (outputs '("out"                    ;libraries, locales, etc
               "static"                 ;static libraries
               "bin"                    ;executables; depends on Python
               "debug"))
    (arguments
     (list
      #:disallowed-references
      (cons tzdata-for-tests
            ;; Verify glib-mkenums, gtester, ... use the cross-compiled
            ;; python.
            (if (%current-target-system)
                (map (cut gexp-input <> #:native? #t)
                     `(,(this-package-native-input "python")
                       ,(this-package-native-input "python-wrapper")))
                '()))
      #:configure-flags #~(list "--default-library=both"
                                "-Dman=false"
                                "-Dselinux=disabled"
                                (string-append "--bindir="
                                               #$output:bin "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Needed to pass the test phase on slower ARM and i686 machines.
          (add-after 'unpack 'increase-test-timeout
            (lambda _
              (substitute* "meson.build"
                (("(test_timeout.*) = ([[:digit:]]+)" all first second)
                 (string-append first " = " second "0")))))
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "gio/tests/meson.build"
                ((".*'testfilemonitor'.*") ;marked as flaky
                 ""))
              (with-directory-excursion "glib/tests"
                (substitute* '("unix.c" "utils.c")
                  (("[ \t]*g_test_add_func.*;") "")))
              (with-directory-excursion "gio/tests"
                (substitute* '("contenttype.c" "gdbus-address-get-session.c"
                               "gdbus-peer.c" "appinfo.c" "desktop-app-info.c")
                  (("[ \t]*g_test_add_func.*;") "")))

              #$@(if (target-x86-32?)
                     ;; Comment out parts of timer.c that fail on i686 due to
                     ;; excess precision when building with GCC 10:
                     ;; <https://gitlab.gnome.org/GNOME/glib/-/issues/820>.
                     '((substitute* "glib/tests/timer.c"
                         (("^  g_assert_cmpuint \\(micros.*" all)
                          (string-append "//" all "\n"))
                         (("^  g_assert_cmpfloat \\(elapsed, ==.*" all)
                          (string-append "//" all "\n"))))
                     '())))
          ;; Python references are not being patched in patch-phase of build,
          ;; despite using python-wrapper as input. So we patch them manually.
          ;;
          ;; These python scripts are both used during build and installed,
          ;; so at first, use a python from 'native-inputs', not 'inputs'. When
          ;; cross-compiling, the 'patch-shebangs' phase will replace
          ;; the native python with a python from 'inputs'.
          (add-after 'unpack 'patch-python-references
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (substitute* '("gio/gdbus-2.0/codegen/gdbus-codegen.in"
                             "glib/gtester-report.in"
                             "gobject/glib-genmarshal.in"
                             "gobject/glib-mkenums.in")
                (("@PYTHON@")
                 (search-input-file (or native-inputs inputs)
                                    (string-append
                                     "/bin/python"
                                     #$(version-major+minor
                                        (package-version python))))))))
          (add-before 'check 'pre-check
            (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
              ;; For tests/gdatetime.c.
              (setenv "TZDIR"
                      (search-input-directory (or native-inputs inputs)
                                              "share/zoneinfo"))
              ;; Some tests want write access there.
              (setenv "HOME" (getcwd))
              (setenv "XDG_CACHE_HOME" (getcwd))))
          (add-after 'install 'move-static-libraries
            (lambda _
              (mkdir-p (string-append #$output:static "/lib"))
              (for-each (lambda (a)
                          (rename-file a (string-append #$output:static "/lib/"
                                                        (basename a))))
                        (find-files #$output "\\.a$"))))
          (add-after 'install 'patch-pkg-config-files
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Do not refer to "bindir", which points to "${prefix}/bin".
              ;; We don't patch "bindir" to point to "$bin/bin", because that
              ;; would create a reference cycle between the "out" and "bin"
              ;; outputs.
              (substitute*
                  (list (search-input-file outputs "lib/pkgconfig/gio-2.0.pc")
                        (search-input-file outputs "lib/pkgconfig/glib-2.0.pc"))
                (("^bindir=.*")
                 "")
                (("=\\$\\{bindir\\}/")
                 "=")))))))
    (native-inputs
     (list dbus
           gettext-minimal
           m4                           ;for installing m4 macros
           perl                         ;needed by GIO tests
           pkg-config
           python                       ;for 'patch-python-references
           python-wrapper
           tzdata-for-tests))           ;for tests/gdatetime.c
    (inputs
     (list ;; "python", "python-wrapper" and "bash-minimal"
      ;; are for the 'patch-shebangs' phase, to make
      ;; sure the installed scripts end up with a correct shebang
      ;; when cross-compiling.
      bash-minimal
      python
      python-wrapper))
    (propagated-inputs
     (list libffi             ;in the Requires.private field of gobject-2.0.pc
           pcre               ;in the Requires.private field of glib-2.0.pc
           `(,util-linux "lib")  ;for libmount
           zlib))                ;in the Requires.private field of glib-2.0.pc
    (native-search-paths
     ;; This variable is not really "owned" by GLib, but several related
     ;; packages refer to it: gobject-introspection's tools use it as a search
     ;; path for .gir files, and it's also a search path for schemas produced
     ;; by 'glib-compile-schemas'.
     (list
      (search-path-specification
       (variable "XDG_DATA_DIRS")
       (files '("share")))
      ;; To load extra gio modules from glib-networking, etc.
      (search-path-specification
       (variable "GIO_EXTRA_MODULES")
       (files '("lib/gio/modules")))))
    (search-paths native-search-paths)
    (synopsis "Low-level core library for GNOME projects")
    (description "GLib provides the core application building blocks for
libraries and applications written in C.  It provides the core object system
used in GNOME, the main loop implementation, and a large set of utility
functions for strings and common data structures.")
    (home-page "https://wiki.gnome.org/Projects/GLib")
    (license license:lgpl2.1+)
    (properties '((hidden? . #t)))))

(define-public glib-next
  (package
    (inherit glib)
    (name "glib")
    (version "2.73.3")
    (source
     (origin
       (inherit (package-source glib))
       (uri
        (string-append "mirror://gnome/sources/"
                       name "/" (string-take version 4) "/"
                       name "-" version ".tar.xz"))
       (snippet
        '(substitute* "glib/tests/spawn-test.c"
           (("/bin/sh") "sh")))
       (sha256
        (base32 "1bgfch7zj1pq4rkqcibfky1470ijljyrx5pn5s5v9mk72s22n6nz"))))
    (arguments
     (substitute-keyword-arguments (package-arguments glib)
       ((#:test-options test-options ''())
        ;; Skip flaky or slow tests.
        `(cons* "--no-suite=slow" "--no-suite=flaky" ,test-options))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'disable-failing-tests
              (lambda _
                (with-directory-excursion "glib/tests"
                  (substitute* '("unix.c" "utils.c")
                    (("[ \t]*g_test_add_func.*;") "")))
                ;; The "glib:gio / file" test fails with the error "No
                ;; application is registered as handling this file" (see:
                ;; https://gitlab.gnome.org/GNOME/glib/-/issues/2742).
                (with-directory-excursion "gio/tests"
                  (substitute* '("appinfo.c"
                                 "contenttype.c"
                                 "desktop-app-info.c"
                                 "file.c"
                                 "gdbus-address-get-session.c"
                                 "gdbus-peer.c")
                    (("[ \t]*g_test_add_func.*;") "")))

                #$@(if (target-x86-32?)
                       ;; Comment out parts of timer.c that fail on i686 due to
                       ;; excess precision when building with GCC 10:
                       ;; <https://gitlab.gnome.org/GNOME/glib/-/issues/820>.
                       '((substitute* "glib/tests/timer.c"
                           (("^  g_assert_cmpuint \\(micros.*" all)
                            (string-append "//" all "\n"))
                           (("^  g_assert_cmpfloat \\(elapsed, ==.*" all)
                            (string-append "//" all "\n"))))
                       '())))))))
    (native-inputs
     (modify-inputs (package-native-inputs glib)
       (append desktop-file-utils)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs glib)
       (replace "pcre" pcre2)))))

(define-public glib-with-documentation
  ;; glib's doc must be built in a separate package since it requires gtk-doc,
  ;; which in turn depends on glib.
  (package/inherit glib
    (properties (alist-delete 'hidden? (package-properties glib)))
    (outputs (cons "doc" (package-outputs glib))) ; 20 MiB of GTK-Doc reference
    (native-inputs
     (modify-inputs (package-native-inputs glib)
       (prepend docbook-xml-4.2
                docbook-xml
                docbook-xsl
                gtk-doc
                libxml2
                libxslt)))
    (arguments
     (substitute-keyword-arguments (package-arguments glib)
       ((#:configure-flags flags ''())
        #~(cons "-Dgtk_doc=true"
                (delete "-Dman=false" #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'move-doc
              (lambda _
                (let ((html "/share/gtk-doc"))
                  (mkdir-p (string-append #$output:doc "/share"))
                  (rename-file
                   (string-append #$output html)
                   (string-append #$output:doc html)))))))))))

(define (python-extension-suffix python triplet)
  "Determine the suffix for C extensions for PYTHON when compiled
for TRIPLET."
  ;; python uses strings like 'x86_64-linux-gnu' instead of
  ;; 'x86_64-unknown-linux-gnu'.
  (define normalised-system
    (string-replace-substring triplet "-unknown-" "-"))
  (define major.minor (version-major+minor (package-version python)))
  (define majorminor (string-delete #\. major.minor))
  (string-append
    ;; If guix' python package used "--with-pydebug", a #\d would
    ;; need to be added, likewise "--with-pymalloc" and "--with-wide-unicode"
    ;; would require a #\m and #\u, see cpython's configure.ac.
    ".cpython-" majorminor "-" normalised-system
    (if (target-mingw? triplet)
        ".dll"
        ".so")))

(define (correct-library-name-phase python name)
  "Return a G-exp evaluating to a phase renaming the python extension NAME
from what Meson thinks its name should be to what python expects its name
to be.  NAME must not include the platform-specific suffix.  This can only
be used when cross-compiling."
  #~(lambda _
      (define name #$name)
      (define native-suffix
        #$(python-extension-suffix python
                                   (nix-system->gnu-triplet (%current-system))))
      (define target-suffix
        #$(python-extension-suffix python (%current-target-system)))
      (define native-name
        (string-append name native-suffix))
      (define target-name
        (string-append name target-suffix))
      (rename-file native-name target-name)))

(define gobject-introspection
  (package
    (name "gobject-introspection")
    (version "1.72.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/"
                   "gobject-introspection/" (version-major+minor version)
                   "/gobject-introspection-" version ".tar.xz"))
             (sha256
              (base32 "1g5aps3b20ck96ahy7fjl4nhp9nabkd9rlqd0s1qzn3111cqxzh2"))
             (patches (search-patches
                       "gobject-introspection-cc.patch"
                       "gobject-introspection-girepository.patch"
                       "gobject-introspection-absolute-shlib-path.patch"))))
    (build-system meson-build-system)
    (arguments
     `(,@(if (%current-target-system)
             `(#:configure-flags
               '("-Dgi_cross_use_prebuilt_gi=true"
                 ;; Building introspection data requires running binaries
                 ;; for ‘host’ on ‘build’, so don't do that.
                 ;;
                 ;; TODO: it would be nice to have introspection data anyways
                 ;; as discussed here: https://issues.guix.gnu.org/50201#60.
                 "-Dbuild_introspection_data=false"))
             '())
       #:phases
       ,#~
       (modify-phases %standard-phases
         #$@(if (%current-target-system)
                ;; 'typelibs' is undefined.
                `((add-after 'unpack 'set-typelibs
                    (lambda _
                      (substitute* "meson.build"
                        (("\\bsources: typelibs\\b")
                         "sources: []")))))
                '())
         (add-after 'unpack 'do-not-use-/usr/bin/env
           (lambda _
             (substitute* "tools/g-ir-tool-template.in"
               (("#!@PYTHON_CMD@")
                (string-append "#!" (which "python3"))))))
         #$@(if (%current-target-system)
               ;; Meson gives python extensions an incorrect name, see
               ;; <https://github.com/mesonbuild/meson/issues/7049>.
                #~((add-after 'install 'rename-library
                     #$(correct-library-name-phase
                         (this-package-input "python")
                         #~(string-append #$output
                                          "/lib/gobject-introspection/giscanner"
                                          "/_giscanner"))))
                #~()))))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("python" ,python)
       ("zlib" ,zlib)))
    (propagated-inputs
     (list glib
           ;; In practice, GIR users will need libffi when using
           ;; gobject-introspection.
           libffi))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GI_TYPELIB_PATH")
       (files '("lib/girepository-1.0")))))
    (search-paths native-search-paths)
    (synopsis "GObject introspection tools and libraries")
    (description "GObject introspection is a middleware layer between
C libraries (using GObject) and language bindings.  The C library can be scanned
at compile time and generate metadata files, in addition to the actual native
C library.  Then language bindings can read this metadata and automatically
provide bindings to call into the C library.")
    (home-page "https://wiki.gnome.org/Projects/GObjectIntrospection")
    (license
     (list
      ;; For library.
      license:lgpl2.0+
      ;; For tools.
      license:gpl2+))))

(define-public gobject-introspection-next
  (package
    (inherit gobject-introspection)
    (name "gobject-introspection")
    (version "1.73.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/"
                                  "gobject-introspection/" (version-major+minor version)
                                  "/gobject-introspection-" version ".tar.xz"))
              (sha256
               (base32 "1gkbx32as3v2286w7k3j24fwhkxj6brr49881m2zavxamfwxdm34"))
              (patches (search-patches
                        "gobject-introspection-cc-1.72.patch"
                        "gobject-introspection-girepository.patch"
                        "gobject-introspection-absolute-shlib-path-1.72.patch"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs gobject-introspection)
       (replace "glib" glib-next)))))

(define intltool
  (package
    (name "intltool")
    (version "0.51.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/intltool/trunk/"
                                 version "/+download/intltool-"
                                 version ".tar.gz"))
             (patches (search-patches "intltool-perl-compatibility.patch"))
             (sha256
              (base32
               "1karx4sb7bnm2j67q0q74hspkfn6lqprpy5r99vkn5bb36a4viv7"))))
    (build-system gnu-build-system)
    (inputs
     (list file))
    (propagated-inputs
     `(;; Propagate gettext because users expect it to be there, and so does
       ;; the `intltool-update' script.
       ("gettext" ,gettext-minimal)

       ("perl-xml-parser" ,perl-xml-parser)
       ("perl" ,perl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file (assoc-ref inputs "file")))
               (substitute* "intltool-update.in"
                 (("`file") (string-append "`" file "/bin/file")))
               #t))))))
    (home-page "https://launchpad.net/intltool/+download")
    (synopsis "Tools to centralise translations of different file formats")
    (description
     "Intltool is a set of tools to centralise translations of many different
file formats using GNU gettext-compatible PO files.

The intltool collection can be used to do these things:

    Extract translatable strings from various source files (.xml.in,
    glade, .desktop.in, .server.in, .oaf.in).

    Collect the extracted strings together with messages from traditional
    source files (.c, .h) in po/$(PACKAGE).pot.

    Merge back the translations from .po files into .xml, .desktop and
    oaf files.  This merge step will happen at build resp. installation time.")
    (license license:gpl2+)))

(define itstool
  (package
    (name "itstool")
    (version "2.0.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://files.itstool.org/itstool/itstool-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1jl7gsr7aclb9nvqazr039m86y7f7ivfhl2pixcrbfqjkb97r6kb"))))
    (build-system gnu-build-system)
    (inputs
     (list libxml2 python-libxml2 python))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/itstool")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))))))))
    (home-page "https://itstool.org")
    (synopsis "Tool to translate XML documents with PO files")
    (description
     "ITS Tool allows you to translate your XML documents with PO files, using
rules from the W3C Internationalization Tag Set (ITS) to determine what to
translate and how to separate it into PO file messages.

PO files are the standard translation format for GNU and other Unix-like
systems.  They present translatable information as discrete messages, allowing
each message to be translated independently.  In contrast to whole-page
translation, translating with a message-based format like PO means you can
easily track changes to the source document down to the paragraph.  When new
strings are added or existing strings are modified, you only need to update the
corresponding messages.

ITS Tool is designed to make XML documents translatable through PO files by
applying standard ITS rules, as well as extension rules specific to ITS Tool.
ITS also provides an industry standard way for authors to override translation
information in their documents, such as whether a particular element should be
translated.")
    (license license:gpl3+)))

(define dbus-glib
  (package
    (name "dbus-glib")
    (version "0.110")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://dbus.freedesktop.org/releases/dbus-glib/dbus-glib-"
                             version ".tar.gz"))
             (sha256
              (base32
               "09g8swvc95bk1z6j8sw463p2v0dqmgm2zjfndf7i8sbcyq67dr3w"))))
    (build-system gnu-build-system)
    (arguments
     (if (%current-target-system)
         `(#:configure-flags
           ;; Run a native 'dbus-binding-tool' instead of a cross-compiled
           ;; 'dbus-binding-tool' when cross-compiling.
           ,#~(list
               (string-append
                "--with-dbus-binding-tool="
                #+(file-append this-package "/bin/dbus-binding-tool"))))
         '()))
    (propagated-inputs ; according to dbus-glib-1.pc
     (list dbus glib))
    (inputs
     (list expat))
    (native-inputs
     (list `(,glib "bin") pkg-config))
    (home-page "https://dbus.freedesktop.org/doc/dbus-glib/")
    (synopsis "D-Bus GLib bindings")
    (description
     "GLib bindings for D-Bus.  The package is obsolete and superseded
by GDBus included in Glib.")
    (license license:gpl2)))                     ; or Academic Free License 2.1

(define-public libaccounts-glib
  (package
    (name "libaccounts-glib")
    (version "1.25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/accounts-sso/libaccounts-glib")
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19rhk9f97m736d5ia26vfcbjp5kgi454558yhf9mrwm4iw5d9pk4"))))
    (build-system meson-build-system)
    (native-inputs (list dbus
                         `(,glib "bin")
                         gobject-introspection
                         gtk-doc
                         pkg-config
                         vala))
    (inputs (list check python python-pygobject))
    (propagated-inputs (list glib libxml2 sqlite))
    (arguments
     (list #:tests? #f                  ;one test fails.
           #:imported-modules `((guix build python-build-system)
                                ,@%meson-build-system-modules)
           #:modules '(((guix build python-build-system)
                        #:select (python-version))
                       (guix build meson-build-system)
                       (guix build utils))
           ;; don't try installing to python store path.
           #:configure-flags
           #~(list (string-append "-Dpy-overrides-dir="
                                  #$output "/lib/python"
                                  (python-version #$(this-package-input
                                                     "python"))
                                  "/site-packages/gi/overrides"))
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "dbus-run-session" "--" "meson" "test"
                                      "--print-errorlogs")))))))
    (home-page "https://accounts-sso.gitlab.io/")
    (synopsis "Accounts SSO (Single Sign-On) management library for GLib
applications")
    (description
     "Accounts SSO is a framework for application developers who
wish to acquire, use and store web account details and credentials.  It
handles the authentication process of an account and securely stores the
credentials and service-specific settings.")
    (license license:lgpl2.1+)))

(define libsigc++
  (package
    (name "libsigc++")
    (version "3.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsigc++/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1kn57b039lg20182lnchl1ys27vf34brn43f895cal8nc7sdq3mp"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags #~(list "-Dbuild-documentation=true")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'move-doc
                          (lambda _
                            (mkdir-p (string-append #$output:doc "/share"))
                            (rename-file
                             (string-append #$output "/share/doc")
                             (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list docbook-xml-4.1.2
           docbook-xsl
           graphviz
           doxygen
           m4
           mm-common
           perl
           pkg-config
           libxml2
           libxslt))
    (inputs (list boost))
    (home-page "https://libsigcplusplus.github.io/libsigcplusplus/")
    (synopsis "Type-safe callback system for standard C++")
    (description
     "Libsigc++ implements a type-safe callback system for standard C++.  It
allows you to define signals and to connect those signals to any callback
function, either global or a member function, regardless of whether it is
static or virtual.  It also contains adaptor classes for connection of
dissimilar callbacks and has an ease of use unmatched by other C++ callback
libraries.")
    (license license:lgpl3+)))

(define-public libsigc++-2
  (package
    (inherit libsigc++)
    (name "libsigc++")
    (version "2.9.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libsigc++/"
                       (version-major+minor version)
                       "/libsigc++-" version ".tar.xz"))
       (sha256
        (base32 "0zq963d0sss82q62fdfjs7l9iwbdch51albck18cb631ml0v7y8b"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'move-doc
                 (lambda _
                   (mkdir-p (string-append #$output:doc "/share"))
                   (rename-file
                    (string-append #$output "/share/doc")
                    (string-append #$output:doc "/share/doc")))))))))

(define glibmm
  (package
    (name "glibmm")
    (version "2.72.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glibmm/"
                                  (version-major+minor version)
                                  "/glibmm-" version ".tar.xz"))
              (sha256
               (base32
                "1n2w2pcpbxjbsxynmar3i5ibr7src6gnrdxb9nn57p5miai4jxia"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:configure-flags #~(list "-Dbuild-documentation=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "tests/meson.build"
                ;; This test uses /etc/fstab as an example file to read from;
                ;; disable it.
                (("[ \t]*.*giomm_simple.*$") "")
                ;; This test does a DNS lookup, and then expects to be able to
                ;; open a TLS session; just skip it.
                (("[ \t]*.*giomm_tls_client.*$") ""))))
          (add-after 'install 'move-doc
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (rename-file
               (string-append #$output "/share/doc")
               (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list graphviz
           doxygen
           `(,glib "bin")
           m4
           mm-common
           perl
           pkg-config
           libxslt))
    (propagated-inputs
     (list libsigc++ glib))
    (home-page "https://gtkmm.org/")
    (synopsis "C++ interface to the GLib library")
    (description
     "Glibmm provides a C++ programming interface to the part of GLib that are
useful for C++.")
    (license license:lgpl2.1+)))

(define-public glibmm-next
  (package
   (inherit glibmm)
   (version "2.76.0")
   (name "glibmm")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/glibmm/"
                                (version-major+minor version)
                                "/glibmm-" version ".tar.xz"))
            (sha256
             (base32
              "1cia8vrpwzn8zwalws42mga5hi965840m5s8dvfzv55xx86dhdw6"))))
   (propagated-inputs
    (modify-inputs (package-propagated-inputs glibmm)
      (replace "glib" glib-next)))))

 (define-public glibmm-2.64
   (package
    (inherit glibmm)
    (name "glibmm")
    (version "2.64.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/glibmm/"
                       (version-major+minor version)
                       "/glibmm-" version ".tar.xz"))
       (sha256
        (base32 "11m37sbx0i18cl17d0fkq0bik4bbzlb5n8kcl651jhci5ipci3sh"))))
     (propagated-inputs
      (modify-inputs (package-propagated-inputs glibmm)
        (replace "libsigc++" libsigc++-2)))))

(define-public python-pygobject
  (package
    (name "python-pygobject")
    (version "3.42.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "0my95gjnps093inzznbipkhf25cffbc32v9is2fq8wvh59g6ks5d"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; We disable these tests in a snippet so that they are inherited
           ;; by the Python 2 variant which is built differently.
           (with-directory-excursion "tests"
             ;; FIXME: These tests require Gdk and/or Gtk 4.
             (for-each delete-file
                       '("test_atoms.py" "test_overrides_gtk.py"))
             #t)))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; The default 90 seconds can be too low on slower machines.
               (invoke "meson" "test" "--timeout-multiplier" "5")))))))
    (native-inputs
     `(("glib-bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)
       ("python-wrapper" ,python-wrapper))) ; For patching shebangs
    (inputs
     (list python python-pycairo gobject-introspection))
    (propagated-inputs
     ;; pygobject-3.0.pc refers to all these.
     (list glib libffi))
    ;; For finding typelib files, since gobject-introscpetion isn't propagated.
    (native-search-paths (package-native-search-paths gobject-introspection))
    (home-page "https://live.gnome.org/PyGObject")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (properties
     '((upstream-name . "pygobject")))
    (license license:lgpl2.1+)))

(define-public perl-glib
  (package
    (name "perl-glib")
    (version "1.3293")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XA/XAOC/Glib-"
                    version ".tar.gz"))
              (sha256
               (base32
                "005m3inz12xcsd5sr056cm1kbhmxsx2ly88ifbdv6p6cwz0s05kk"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list glib))
    (home-page "https://metacpan.org/release/Glib")
    (synopsis "Perl wrappers for the GLib utility and Object libraries")
    (description "This module provides perl access to GLib and GLib's GObject
libraries.  GLib is a portability and utility library; GObject provides a
generic type system with inheritance and a powerful signal system.  Together
these libraries are used as the foundation for many of the libraries that make
up the Gnome environment, and are used in many unrelated projects.")
    (license license:lgpl2.1+)))

(define-public perl-glib-object-introspection
  (package
    (name "perl-glib-object-introspection")
    (version "0.049")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/"
                           "Glib-Object-Introspection-" version ".tar.gz"))
       (sha256
        (base32 "0mxg6pz8qfyipw0ypr54alij0c4adzg94f62702b2a6hkp5jhij6"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list gobject-introspection perl-cairo-gobject perl-glib))
    (home-page "https://metacpan.org/dist/Glib-Object-Introspection")
    (synopsis "Dynamically create Perl language bindings")
    (description "Glib::Object::Introspection uses the gobject-introspection and
libffi projects to dynamically create Perl bindings for a wide variety of
libraries.  Examples include gtk+, webkit, libsoup and many more.")
    (license license:lgpl2.1+)))

(define telepathy-glib
  (package
    (name "telepathy-glib")
    (version "0.24.2")
    (source
     (origin
      (method url-fetch)
       (uri
        (string-append
         "https://telepathy.freedesktop.org/releases/telepathy-glib/"
         "telepathy-glib-" version ".tar.gz"))
       (sha256
        (base32
         "1w3kja8j3gz2apal79bi3hq44xk5g78aphrqbw983l6df7bp98xh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-vala-bindings")

       ;; '../tools/glib-*.py' generate files but the target dependencies are
       ;; (presumably) not fully specified in the makefile, leading to
       ;; parallel build errors like:
       ;;
       ;;   EOFError: EOF read where object expected
       ;;   make[2]: *** [Makefile:1906: _gen/register-dbus-glib-marshallers-body.h] Error 1
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; None of the tests below are able to find the org.gtk.vfs.Daemon
             ;; service file provided by gvfs.
             (substitute* "tests/dbus/Makefile.in"
               (("test-contacts\\$\\(EXEEXT\\)") "")
               (("test-file-transfer-channel\\$\\(EXEEXT\\)") "")
               (("test-stream-tube\\$\\(EXEEXT\\)") ""))
             #t)))))
    (native-inputs
     `(("glib" ,glib "bin") ; uses glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; There are all in the Requires.private field of telepathy-glib.pc.
     (list dbus dbus-glib glib))
    (home-page "https://telepathy.freedesktop.org/wiki/")
    (synopsis "GLib Real-time communications framework over D-Bus")
    (description "Telepathy is a flexible, modular communications framework
that enables real-time communication over D-Bus via pluggable protocol
backends.  Telepathy is a communications service that can be accessed by
many applications simultaneously.

This package provides the library for GLib applications.")
    (license license:lgpl2.1+)))

(define-public dbus-c++
  (package
    (name "dbus-c++")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "mirror://sourceforge/dbus-cplusplus/dbus-c%2B%2B/"
                version "/libdbus-c%2B%2B-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "dbus-c++-gcc-compat.patch"
                                       "dbus-c++-threading-mutex.patch"))
              (sha256
               (base32
                "0qafmy2i6dzx4n1dqp6pygyy6gjljnb7hwjcj2z11c1wgclsq4dw"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list dbus))                      ;mentioned in the pkg-config file
    (inputs
     (list efl expat glib libunwind))
    (native-inputs
     (list pkg-config))
    (arguments
     `(;; The 'configure' machinery fails to detect that it needs -lpthread.
       #:configure-flags (list "LDFLAGS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-missing-header
           (lambda _
             (substitute* "include/dbus-c++/eventloop-integration.h"
               (("#include <errno.h>")
                "#include <errno.h>\n#include <unistd.h>"))
             #t)))))
    (synopsis "D-Bus API for C++")
    (description "This package provides D-Bus client API bindings for the C++
programming language.  It also provides the @command{dbusxx-xml2cpp} and
@command{dbusxx-introspect} commands.")
    (home-page "https://sourceforge.net/projects/dbus-cplusplus/")
    (license license:lgpl2.1+)))

(define-public dbus-cxx
  (package
    (name "dbus-cxx")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/dbus-cxx/dbus-cxx/"
                                  version "/dbus-cxx-" version ".tar.gz"))
              (sha256
               (base32
                "1acsgpkd9v7b9jdc79ijmh9dbdfrzgkwkaff518i3zpk7y6g5mzw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_TESTS=ON"
                           "-DENABLE_TOOLS=ON"
                           "-DENABLE_GLIBMM=ON")))
    (inputs (list dbus
                  libsigc++
                  glibmm
                  python
                  popt
                  expat))
    (native-inputs (list pkg-config m4))
    (synopsis "C++ wrapper for dbus")
    (description "Dbus-cxx is a C++ wrapper for dbus.\n
It exposes the C API to allow direct manipulation and
relies on sigc++ to provide an Oriented Object interface.\n
This package provide 2 utils:
@enumerate
@item @command{dbus-cxx-xml2cpp} to generate proxy and adapter
@item @command{dbus-cxx-introspect} to introspect a dbus interface
@end enumerate

Some codes examples can be find at:
@url{https://dbus-cxx.github.io/examples.html}")
    (home-page "https://dbus-cxx.github.io/")
    (license license:gpl3)))

(define-public appstream-glib
  (package
    (name "appstream-glib")
    (version "0.7.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.freedesktop.org/~hughsient/"
                                  "appstream-glib/releases/"
                                  "appstream-glib-" version ".tar.xz"))
              (sha256
               (base32
                "00j0kkgf224nzmrha72g8pd72mymhph7vaisj35i4ffy7cpd47na"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gsettings" ,gsettings-desktop-schemas) ; for ‘org.gnome.system.proxy’
       ("glib:bin" ,glib "bin")         ; for glib-compile-resources
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("gcab" ,gcab) ; for .pc file
       ("gdk-pixbuf" ,gdk-pixbuf) ; for .pc file
       ("libuuid" ,util-linux "lib"))) ; for .pc file
    (inputs
     `(("glib" ,glib)
       ("gperf" ,gperf)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libsoup" ,libsoup-minimal-2)))
    (arguments
     `(#:configure-flags
       (list "-Ddep11=false"
             "-Dintrospection=false"    ; avoid g-ir-scanner dependency
             "-Drpm=false"
             "-Dstemmer=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "libappstream-glib/as-self-test.c"
               (("g_test_add_func.*as_test_store_local_appdata_func);") ""))
             #t))
         (add-before 'check 'set-home
           (lambda _
             ;; Some tests want write access there.
             (setenv "HOME" "/tmp"))))))
    (home-page "https://github.com/hughsie/appstream-glib")
    (synopsis "Library for reading and writing AppStream metadata")
    (description "This library provides objects and helper methods to help
reading and writing @uref{https://www.freedesktop.org/wiki/Distributions/AppStream,AppStream}
metadata.")
    (license license:lgpl2.1+)))

(define perl-net-dbus
  (package
    (name "perl-net-dbus")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANBERR/Net-DBus-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1g0w8i5scmh7kfy9mmvv8q326627qf38z26mvczmn8x1yjgar8g7"))))
    (build-system perl-build-system)
    (native-inputs
     (list pkg-config perl-test-pod perl-test-pod-coverage))
    (inputs
     (list dbus))
    (propagated-inputs
     (list perl-xml-twig))
    (home-page "https://metacpan.org/release/Net-DBus")
    (synopsis "Extension for the DBus bindings")
    (description "@code{Net::DBus} provides a Perl XS API to the DBus
inter-application messaging system.  The Perl API covers the core base level
of the DBus APIs, not concerning itself yet with the GLib or QT wrappers.")
    (license license:perl-license)))

(define perl-net-dbus-glib
  (package
    (name "perl-net-dbus-glib")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DA/DANBERR/"
                           "Net-DBus-GLib-" version ".tar.gz"))
       (sha256
        (base32
         "1z4mbv8z0rad604xahijpg5szzi8qak07hbahh230z4jf96fkxvj"))))
    (build-system perl-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list dbus-glib))
    (home-page "https://metacpan.org/release/Net-DBus-GLib")
    (synopsis "Perl extension for the DBus GLib bindings")
    (description "This package provides an extension to the @code{Net::DBus}
module allowing integration with the GLib mainloop.  To integrate with the
main loop, simply get a connection to the bus via the methods in
@code{Net::DBus::GLib} rather than the usual @code{Net::DBus} module.  Every
other API remains the same.")
    (license license:gpl2+)))

(define-public template-glib
  (package
    (name "template-glib")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1z9xkin5fyfh071ma9y045jcw83hgx33dfbjraw6cxk0qdmfysr1"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-D" "enable_gtk_doc=true")))
    (inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("glib:bin" ,glib "bin") ;; For glib-mkenums
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://gitlab.gnome.org/GNOME/template-glib")
    (synopsis "Library for template expansion")
    (description
     "Template-GLib is a library to help you generate text based on a template and
user defined state.  Template-GLib does not use a language runtime, so it is
safe to use from any GObject-Introspectable language.

Template-GLib allows you to access properties on GObjects as well as call
simple methods via GObject-Introspection.")
    (license license:lgpl2.1+)))

(define-public xdg-dbus-proxy
  (package
    (name "xdg-dbus-proxy")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/flatpak/xdg-dbus-proxy"
                                  "/releases/download/" version
                                  "/xdg-dbus-proxy-" version ".tar.xz"))
              (sha256
               (base32
                "03sj1h0c2l08xa8phw013fnxr4fgav7l2mkjhzf9xk3dykwxcj8p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           ;; For tests.
           dbus
           ;; These are required to build the manual.
           docbook-xml-4.3
           docbook-xsl
           libxml2
           libxslt))
    (inputs
     (list glib))
    (home-page "https://github.com/flatpak/xdg-dbus-proxy")
    (synopsis "D-Bus connection proxy")
    (description
     "xdg-dbus-proxy is a filtering proxy for D-Bus connections.  It can be
used to create D-Bus sockets inside a Linux container that forwards requests
to the host system, optionally with filters applied.")
    (license license:lgpl2.1+)))

(define-public dbus-test-runner
  (package
    (name "dbus-test-runner")
    (version "19.04.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://launchpad.net/dbus-test-runner/"
                    (version-major+minor version) "/" version
                    "/+download/dbus-test-runner-" version ".tar.gz"))
              (sha256
               (base32
                "0xnbay58xn0hav208mdsg8dd176w57dcpw1q2k0g5fh9v7xk4nk4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-test-paths
           ;; add missing space
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("#!/bin/bash") (string-append "#!" (which "bash"))))
             (substitute* "tests/Makefile.in"
               (("/bin/sh") (which "sh"))
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("echo cat") (string-append "echo " (which "cat")))
               (("/bin/true") (which "true")))
             #t)))))
    (inputs
     (list gtk+ glib dbus-glib))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ;; following used for tests
       ("python" ,python)
       ("python-dbusmock" ,python-dbusmock)
       ("xvfb" ,xorg-server-for-tests)))
    (home-page "https://launchpad.net/dbus-test-runner")
    (synopsis "Run a executables under a new DBus session for testing")
    (description "A small little utility to run a couple of executables under a
new DBus session for testing.")
    (license license:gpl3)))
