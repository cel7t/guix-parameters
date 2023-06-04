;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015–2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages webkit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public libwpe
  (package
    (name "libwpe")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://wpewebkit.org/releases/libwpe-"
                       version ".tar.xz"))
       (sha256
        (base32 "13618imck69w7fbmljlh62j4gnlspb9zfqzv9hlkck3bi8icmvp8"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     (list mesa))
    (propagated-inputs
     (list libxkbcommon))
    (synopsis "General-purpose library for WPE")
    (description "LibWPE is general-purpose library specifically developed for
the WPE-flavored port of WebKit.")
    (home-page "https://wpewebkit.org/")
    (license license:bsd-2)))

(define-public wpebackend-fdo
  (package
    (name "wpebackend-fdo")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wpewebkit.org/releases/"
                                  "wpebackend-fdo-" version ".tar.xz"))
              (sha256
               (base32
                "1b3l02dns1yxw3rq18cv00qan4hp95mxw5b3ssc0fh93ap0wjfb2"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libepoxy libwpe mesa wayland))
    (home-page "https://wpewebkit.org/")
    (synopsis "Wayland WPE backend")
    (description
     "This package provides a backend implementation for the WPE WebKit
engine that uses Wayland for graphics output.")
    (license license:bsd-2)))

(define-public webkitgtk
  (package
    (name "webkitgtk")                  ; webkit2gtk4
    (version "2.40.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.webkitgtk.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32 "0070fy5crf7kngy49wz5bqwvp8z9rmnq2cm6wxp41nllv5q8i2cn"))
              (patches (search-patches
                        "webkitgtk-adjust-bubblewrap-paths.patch"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc" "debug"))
    (arguments
     (list
      ;; The release archive doesn't include the resources/sources needed to
      ;; run the various regression tests.
      #:tests? #f
      ;; When building using the default RelWithDebInfo build type, the final
      ;; binaries require 20 GiB of memory to link (even with ld.gold or lld)
      ;; and produce 4.6 GiB of debug symbols.
      #:build-type "Release"
      #:configure-flags
      #~(list "-DPORT=GTK"
              ;; GTKDOC will be removed upstream soon in favor of
              ;; gi-docgen; it is normally disabled because the
              ;; doc is rather expensive to build.
              "-DENABLE_GTKDOC=ON"
              ;; The minibrowser, not built by default, is a good
              ;; tool to validate the good operation of
              ;; webkitgtk.
              "-DENABLE_MINIBROWSER=ON"
              ;; The default lib installation prefix is lib64.
              (string-append "-DLIB_INSTALL_DIR=" #$output "/lib")
              ;; XXX: WebKitGTK makes use of elogind's systemd-compatible
              ;; headers, which are under the include/elogind prefix.  The WTF
              ;; component doesn't propagate the Journald header correctly
              ;; detected (stubs from elogind), hence the following hack (see:
              ;; https://bugs.webkit.org/show_bug.cgi?id=254495).
              (string-append "-DCMAKE_CXX_FLAGS=-I"
                             (search-input-directory
                              %build-inputs "include/elogind")))
      ;; The build may fail with -j1 (see:
      ;; https://bugs.webkit.org/show_bug.cgi?id=195251).
      #:make-flags #~(list "-j" (number->string (max 2 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-CC
            (lambda _
              ;; Some Perl scripts check for the CC environment variable, else
              ;; use /usr/bin/gcc.
              (setenv "CC" #$(cc-for-target))))
          (add-after 'unpack 'configure-bubblewrap-store-directory
            (lambda _
              ;; This phase works in tandem with
              ;; webkitgtk-adjust-bubblewrap-paths.patch and avoids hard
              ;; coding /gnu/store, for users with other prefixes.
              (let ((store-directory (%store-directory)))
                (substitute*
                    "Source/WebKit/UIProcess/Launcher/glib/BubblewrapLauncher.cpp"
                  (("@storedir@") store-directory)))))
          (add-after 'unpack 'do-not-disable-new-dtags
            ;; Ensure the linker uses new dynamic tags as this is what Guix
            ;; uses and validates in the validate-runpath phase.
            (lambda _
              (substitute* "Source/cmake/OptionsCommon.cmake"
                (("if \\(LD_SUPPORTS_DISABLE_NEW_DTAGS\\)")
                 "if (FALSE)"))))
          (add-after 'unpack 'embed-absolute-wpebackend-reference
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((wpebackend-fdo (assoc-ref inputs "wpebackend-fdo")))
                (substitute* "Source/WebKit/UIProcess/glib/WebProcessPoolGLib.cpp"
                  (("libWPEBackend-fdo-[\\.0-9]+\\.so" all)
                   (search-input-file inputs (string-append "lib/" all)))))))
          #$@(if (target-x86-64?)
                 '()
                 '((add-after 'unpack 'disable-sse2
                     (lambda _
                       (substitute* "Source/cmake/WebKitCompilerFlags.cmake"
                         (("WTF_CPU_X86 AND NOT CMAKE_CROSSCOMPILING")
                          "FALSE"))))))
          (add-after 'install 'move-doc-files
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((doc (assoc-ref outputs "doc")))
                (mkdir-p (string-append doc "/share"))
                (rename-file (string-append #$output "/share/gtk-doc")
                             (string-append doc "/share/gtk-doc"))))))))
    (native-inputs
     (list bison
           gettext-minimal
           `(,glib "bin")               ;for glib-mkenums, etc.
           gobject-introspection
           gperf
           perl
           pkg-config
           python-wrapper
           gi-docgen
           ruby
           unifdef))
    (propagated-inputs
     (list gtk+ libsoup))
    (inputs
     (list at-spi2-core
           bubblewrap
           elogind
           enchant
           geoclue
           gst-plugins-base
           gst-plugins-bad-minimal
           harfbuzz
           hyphen
           icu4c
           lcms
           libavif
           libgcrypt
           libgudev
           libjpeg-turbo
           libmanette
           libnotify
           libpng
           libseccomp
           libsecret
           libtasn1
           libwebp
           libwpe
           libxcomposite
           libxml2
           libxslt
           libxt
           mesa
           openjpeg
           sqlite
           woff2
           wpebackend-fdo
           xdg-dbus-proxy))
    (properties '((timeout . 144000)))  ; 40 hours, most notably for aarch64
    (home-page "https://www.webkitgtk.org/")
    (synopsis "Web content engine for GTK+")
    (description "WebKitGTK+ is a full-featured port of the WebKit rendering engine,
suitable for projects requiring any kind of web integration, from hybrid
HTML/CSS applications to full-fledged web browsers.  WebKitGTK+ video playing
capabilities can be extended through the use of GStreamer plugins (not
propagated by default) such as @code{gst-plugins-good} and
@code{gst-plugins-bad}.")
    ;; WebKit's JavaScriptCore and WebCore components are available under
    ;; the GNU LGPL, while the rest is available under a BSD-style license.
    (license (list license:lgpl2.0
                   license:lgpl2.1+
                   license:bsd-2
                   license:bsd-3))))

(define-public webkitgtk-next
  (package
    (inherit webkitgtk)
    (name "webkitgtk-next")             ; webkit2gtk5
    (arguments
     (substitute-keyword-arguments (package-arguments webkitgtk)
       ((#:configure-flags flags)
        #~(cons* "-DENABLE_INTROSPECTION=ON"
                 "-DUSE_GTK4=ON"
                 (delete "-DENABLE_GTKDOC=ON" #$flags)))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs webkitgtk)
       (replace "gtk+" gtk)))
    (inputs
     (modify-inputs (package-inputs webkitgtk)
       (delete "libnotify")))))

;;; Required by e.g. emacs-next-pgtk, emacs-xwidgets, and some other GNOME
;;; packages for webkit2gtk-4.0.  See also the upstream tracker for libsoup 3:
;;; https://gitlab.gnome.org/GNOME/libsoup/-/issues/218.
(define-public webkitgtk-with-libsoup2
  (package/inherit webkitgtk
    (name "webkitgtk-with-libsoup2")
    (arguments (substitute-keyword-arguments (package-arguments webkitgtk)
                 ((#:configure-flags flags)
                  #~(cons "-DUSE_SOUP2=ON" #$flags))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs webkitgtk)
       (replace "libsoup" libsoup-minimal-2)))))

(define-public wpewebkit
  (package
    (inherit webkitgtk)
    (name "wpewebkit")
    (version "2.40.0")
    (source (origin
              (inherit (package-source webkitgtk))
              (uri (string-append "https://wpewebkit.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32 "1dl663nbm011sx099x9gdhk3aj119yn5rxp77jmnhdv1l77jpv58"))))
    (arguments
     (substitute-keyword-arguments (package-arguments webkitgtk)
       ((#:configure-flags flags)
        #~(cons "-DPORT=WPE"
                (delete "-DPORT=GTK" #$flags)))))
    (synopsis "WebKit port optimized for embedded devices")
    (description "WPE WebKit allows embedders to create simple and performant
systems based on Web platform technologies.  It is designed with hardware
acceleration in mind, leveraging common 3D graphics APIs for best performance.")
    (home-page "https://wpewebkit.org/")
    (properties '((cpe-name . "wpe_webkit")))))
