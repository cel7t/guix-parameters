;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2022 dan <i@dan.games>
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

(define-module (gnu packages fcitx5)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public xcb-imdkit
  (package
    (name "xcb-imdkit")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5/xcb-imdkit/xcb-imdkit-"
             version ".tar.xz"))
       (sha256
        (base32 "1k03ix0r385lf56pyw653h3f2inkl774n9fh1wkb2m4jv4kcsgrp"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled uthash.
           (delete-file-recursively "uthash")
           #t))))
    (build-system cmake-build-system)
    (inputs
     (list uthash libxcb xcb-util xcb-util-keysyms))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (home-page "https://github.com/fcitx/xcb-imdkit")
    (synopsis "Input method development support for XCB")
    (description "Xcb-imdkit is an implementation of xim protocol in XCB,
comparing with the implementation of IMDkit with Xlib, and xim inside Xlib, it
has less memory foot print, better performance, and safer on malformed
client.")
    (license license:lgpl2.1)))

(define-public fcitx5
  (package
    (name "fcitx5")
    (version "5.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5/fcitx5/fcitx5-"
             version "_dict.tar.xz"))
       (sha256
        (base32 "1w26v0a8qizv8flpkmg04b42kwym6ca1a1chng7j2j1xizjhiw0p"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCLDR_DIR="
                            (assoc-ref %build-inputs "unicode-cldr-common")
                            "/share/unicode/cldr"))))
    (inputs
     `(("cairo" ,cairo)
       ("cairo-xcb" ,cairo-xcb)
       ("dbus" ,dbus)
       ("enchant" ,enchant)
       ("expat" ,expat)
       ("fmt" ,fmt)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("iso-codes" ,iso-codes)
       ("json-c" ,json-c)
       ("libevent" ,libevent)
       ("libpthread-stubs" ,libpthread-stubs)
       ("libuuid" ,util-linux "lib")
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxfixes" ,libxfixes)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxkbfile" ,libxkbfile)
       ("pango" ,pango)
       ("unicode-cldr-common" ,unicode-cldr-common)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("xcb-imdkit" ,xcb-imdkit)
       ("xcb-util" ,xcb-util)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)
       ("xkeyboard-config" ,xkeyboard-config)))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "FCITX_ADDON_DIRS")
            (files '("lib/fcitx5")))))
    (home-page "https://github.com/fcitx/fcitx5")
    (synopsis "Input method framework")
    (description "Fcitx 5 is a generic input method framework.")
    (license license:lgpl2.1+)))

(define-public fcitx5-lua
  (package
    (name "fcitx5-lua")
    (version "5.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5/fcitx5-lua/fcitx5-lua-"
             version ".tar.xz"))
       (sha256
        (base32 "13vh6i7pap3h9jrjri3cfi7pcjwhlkw9g24ibfh0aykdhd8d7p99"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DUSE_DLOPEN=OFF")))
    (inputs
     (list fcitx5 lua gettext-minimal libpthread-stubs))
    (native-inputs
     (list extra-cmake-modules))
    (home-page "https://github.com/fcitx/fcitx5-lua")
    (synopsis "Lua support for Fcitx 5")
    (description "Fcitx5-lua allows writing Fcitx5 extension in Lua.")
    (license license:lgpl2.1+)))

(define-public libime
  (package
    (name "libime")
    (version "1.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5/libime/libime-"
                           version "_dict.tar.xz"))
       (sha256
        (base32 "00d191nghqkcxky5i2j5v8105ckhfh0bdjbkk8swyndjp35bn54f"))))
    (build-system cmake-build-system)
    (inputs
     (list fcitx5 boost))
    (native-inputs
     (list extra-cmake-modules python))             ;needed to run test
    (home-page "https://github.com/fcitx/libime")
    (synopsis "Library for implementing generic input methods")
    (description "Libime is a library for implementing various input method
editors.")
    (license license:lgpl2.1+)))

(define-public fcitx5-gtk
  (package
    (name "fcitx5-gtk")
    (version "5.0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5"
                           "/fcitx5-gtk/fcitx5-gtk-"
                           version ".tar.xz"))
       (sha256
        (base32 "0s08j8y7bxn9hjz9xkza2fal24r9az2p7js7v86nbjmvy7klq4md"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;No test
      #:configure-flags
      #~(list (string-append "-DGOBJECT_INTROSPECTION_GIRDIR="
                             #$output "/share/gir-1.0")
              (string-append "-DGOBJECT_INTROSPECTION_TYPELIBDIR="
                             #$output "/lib/girepository-1.0")
              "-DENABLE_GTK4_IM_MODULE=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-install-prefix
            (lambda _
              ;; Take care of different versions of GTK because this package
              ;; provides IM module for GTK application to use input method.
              (define (split-immodule gtk-version output)
                (substitute* (string-append gtk-version "/CMakeLists.txt")
                  (("\\$\\{CMAKE_INSTALL_LIBDIR\\}")
                   (string-append output "/lib"))))

              (let ((gtk2 #$output:gtk2)
                    (gtk3 #$output:gtk3))
                (for-each split-immodule
                          '("gtk2" "gtk3")
                          (list gtk2 gtk3))))))))
    (inputs
     (list fcitx5
           fmt
           libx11
           libxkbcommon
           gettext-minimal
           gobject-introspection
           gtk+-2
           gtk+
           glib))
    (native-inputs
     (list extra-cmake-modules pkg-config
           `(,glib "bin")))           ;for glib-genmarshal
    (outputs '("out" "gtk2" "gtk3"))
    (home-page "https://github.com/fcitx/fcitx5-gtk")
    (synopsis "GLib-based D-Bus client and GTK IM module for Fcitx 5")
    (description "Fcitx5-gtk provides the following functionality in the
corresponding output:

@table @code
@item out
GLib-based D-Bus client of Fcitx5.
@item gtk2
IM module for GTK+2 applications.
@item gtk3
IM module for GTK+3 applications.
@end table")
    (license license:lgpl2.1+)))

;; XXX: This package is separated from fcitx5-gtk for following reasons.
;; 1. GTK4 has a lot more dependencies, some of which maybe unavailable on
;;    platforms other than x86_64. See <https://issues.guix.gnu.org/53648>.
;; 2. GTK4 now propagates pango@1.50, it will conflict with GTK3 and GTK2
;;    (propagates pango@1.48) if they're all in the inputs of same package.
;;    See <https://issues.guix.gnu.org/54261>.
(define-public fcitx5-gtk4
  (package
    (inherit fcitx5-gtk)
    (name "fcitx5-gtk4")
    (arguments
     (list
      #:tests? #f                       ;No test
      #:configure-flags
      #~(list (string-append "-DCMAKE_CXX_FLAGS=-I"
                             #$(this-package-input "fcitx5-gtk")
                             "/include/Fcitx5/GClient"
                             " -I" #$(this-package-input "glib")
                             "/include/gio-unix-2.0")
              "-DENABLE_GTK2_IM_MODULE=OFF"
              "-DENABLE_GTK3_IM_MODULE=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-fcitxtheme-path
            (lambda _
              (substitute* "gtk4/gtk4inputwindow.cpp"
                (("<gtk3/fcitxtheme.h>")
                 "\"fcitxtheme.h\""))))
          (add-before 'configure 'fix-gclient
            (lambda* (#:key inputs #:allow-other-keys)
              (define gclient
                (search-input-file inputs "lib/libFcitx5GClient.so"))
              ;; Force cmake search libFcitx5GClient.so in library search
              ;; path instead of compiling again.
              (substitute* "gtk4/CMakeLists.txt"
                (("Fcitx5::GClient")
                 gclient))))
          (add-before 'build 'enter-gtk4-subdirectory
            (lambda _
              (chdir "gtk4")))
          (add-after 'install 'leave-gtk4-subdirectory
            (lambda _
              (chdir ".."))))))
    (inputs
     (modify-inputs (package-inputs fcitx5-gtk)
       (delete "gtk+")
       (prepend fcitx5-gtk gtk)))
    (outputs '("out"))
    (synopsis "GTK4 IM module for Fcitx 5")
    (description "Fcitx5-gtk4 provides IM module for GTK4 applications.")))

(define-public fcitx5-qt
  (package
    (name "fcitx5-qt")
    (version "5.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5"
                           "/fcitx5-qt/fcitx5-qt-"
                           version ".tar.xz"))
       (sha256
        (base32 "1x4iks052jn6dcwd1lv1i9cpwh3l3qnnd3phryfsg4kn4yhyfs22"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCMAKE_INSTALL_QT5PLUGINDIR="
                            %output "/lib/qt5/plugins")
             "-DENABLE_QT4=Off")))
    (inputs
     `(("fcitx5" ,fcitx5)
       ("libxcb" ,libxcb)
       ("libxkbcommon" ,libxkbcommon)
       ("qtbase" ,qtbase-5)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     (list extra-cmake-modules))
    (home-page "https://github.com/fcitx/fcitx5-qt")
    (synopsis "Qt library and IM module for Fcitx 5")
    (description "Fcitx5-qt provides Qt library for development and IM module
for Qt based application.")
    (license (list license:lgpl2.1+
                   ;; Files under qt4(Fcitx5Qt4DBusAddons), qt5/dbusaddons
                   ;; and qt5/platforminputcontext.
                   license:bsd-3))))

(define-public fcitx5-anthy
  (package
    (name "fcitx5-anthy")
    (version "5.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5"
                           "/fcitx5-anthy/fcitx5-anthy-"
                           version ".tar.xz"))
       (sha256
        (base32 "1dx7ajygdy244lwyv3fv14b4wdg3csppxhk8p0g2zwjfvxq4v1qa"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;; no tests
    (inputs (list fcitx5 anthy gettext-minimal fmt))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (home-page "https://github.com/fcitx/fcitx5-anthy")
    (synopsis "Anthy Japanese language input for Fcitx 5")
    (description "Fcitx5-Anthy provides Japanese input support to Fcitx5 using
the Anthy input method.")
    (license license:gpl2+)))

(define-public fcitx5-chinese-addons
  (package
    (name "fcitx5-chinese-addons")
    (version "5.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.fcitx-im.org/fcitx5"
                           "/fcitx5-chinese-addons/fcitx5-chinese-addons-"
                           version "_dict.tar.xz"))
       (sha256
        (base32 "0vilq49q02hrim0dg4z3rm6p3ma8v4smh0r5q5rrj6bkyf1apivx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DUSE_WEBKIT=off")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'split-outputs
           ;; Build with GUI supports requires Qt and increase package closure
           ;; by 800M on x86_64, so place it under another output.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "gui/pinyindictmanager/CMakeLists.txt"
               (("\\$\\{CMAKE_INSTALL_LIBDIR\\}" _)
                (string-append (assoc-ref outputs "gui") "/lib"))))))))
    (inputs
     `(("fcitx5" ,fcitx5)
       ("fcitx5-lua" ,fcitx5-lua)
       ("boost" ,boost)
       ("libime",libime)
       ("curl" ,curl)
       ("gettext" ,gettext-minimal)
       ("fmt" ,fmt)
       ("libpthread-stubs" ,libpthread-stubs)
       ("opencc" ,opencc)
       ("qtbase" ,qtbase-5)
       ("fcitx5-qt" ,fcitx5-qt)
       ("qtdeclarative-5" ,qtdeclarative-5)
       ("qtwebchannel-5" ,qtwebchannel-5)
       ("qtwebengine-5" ,qtwebengine-5)))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (outputs '("out" "gui"))
    (home-page "https://github.com/fcitx/fcitx5-chinese-addons")
    (synopsis "Chinese related addons for Fcitx 5")
    (description "Fcitx5-chinese-addons provides Chinese related addons,
including input methods previous bundled inside Fcitx 4:

@itemize
@item Bingchan
@item Cangjie
@item Erbi
@item Pinyin
@item Shuangpin
@item Wanfeng
@item Wubi
@item Wubi Pinyin
@item Ziranma
@end itemize\n")
    (license (list license:lgpl2.1+
                   license:gpl2+
                   ;; im/pinyin/emoji.txt
                   license:unicode))))

(define-public fcitx5-configtool
  (package
    (name "fcitx5-configtool")
    (version "5.0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.fcitx-im.org/fcitx5"
             "/fcitx5-configtool/fcitx5-configtool-" version ".tar.xz"))
       (sha256
        (base32 "1mc6axvkv2i396v7hyqllpiv5rdnf8vvqg9bkb66ljni4dnikdpq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; KDE is currently not working on Guix, KCM supports doesn't make sense.
       '("-DENABLE_KCM=Off")))
    (inputs
     `(("fcitx5" ,fcitx5)
       ("fcitx5-qt" ,fcitx5-qt)
       ("qtbase" ,qtbase-5)
       ("qtx11extras" ,qtx11extras)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("libx11" ,libx11)
       ("xkeyboard-config" ,xkeyboard-config)
       ("libxkbfile" ,libxkbfile)
       ("gettext" ,gettext-minimal)
       ("iso-codes" ,iso-codes)))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (home-page "https://github.com/fcitx/fcitx5-configtool")
    (synopsis "Graphical configuration tool for Fcitx 5")
    (description "Fcitx5-configtool is a graphical configuration tool
to manage different input methods in Fcitx 5.")
    (license license:gpl2+)))

(define-public fcitx5-material-color-theme
  (package
    (name "fcitx5-material-color-theme")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hosxy/Fcitx5-Material-Color")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0drdypjf1njl7flkb5d581vchwlp4gaqyws3cp0v874wkwh4gllb"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (srfi srfi-26))

             (let* ((out (assoc-ref outputs "out"))
                    (assets-dir (string-append
                                 out "/share/fcitx5-material-color-theme"))
                    (themes-prefix (string-append out "/share/fcitx5/themes")))

               (define (install-theme-variant variant target)
                 (let ((dir (string-append themes-prefix "/" target))
                       (conf (string-append "theme-" variant ".conf")))
                   (format #t "install: Installing color variant \"~a\" to ~a~%"
                           variant dir)
                   (substitute* conf
                     (("^Name=.*")
                      (string-append "Name=" target "\n")))
                   (mkdir-p dir)
                   (copy-file conf (string-append dir "/theme.conf"))
                   (symlink (string-append assets-dir "/arrow.png")
                            (string-append dir "/arrow.png"))
                   (symlink (string-append assets-dir "/radio.png")
                            (string-append dir "/radio.png"))))

               (mkdir-p assets-dir)
               (install-file "arrow.png" assets-dir)
               (install-file "radio.png" assets-dir)
               (for-each
                (lambda (x)
                  (install-theme-variant
                   x (string-append "Material-Color-" (string-capitalize x))))
                '("black" "blue" "brown" "indigo"
                  "orange" "pink" "red" "teal"))

               (install-theme-variant
                "deepPurple" "Material-Color-DeepPurple")
               (install-theme-variant
                "sakuraPink" "Material-Color-SakuraPink")))))))
    (home-page "https://github.com/hosxy/Fcitx5-Material-Color")
    (synopsis "Material Design for Fcitx 5")
    (description "Fcitx5-material-color-theme is a Material Design theme
for Fcitx 5 with following color variants:

@itemize
@item Black
@item Blue
@item Brown
@item Indigo
@item Orange
@item Pink
@item Red
@item teal
@item DeepPurple
@end itemize\n")
    (license license:asl2.0)))

(define-public fcitx5-rime
  (package
    (name "fcitx5-rime")
    (version "5.0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.fcitx-im.org/fcitx5"
                                  "/fcitx5-rime/fcitx5-rime-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0d3qikdcm55p7qjfgiidx1a0zij3sv4s6j7vxxmxwyynjl5xjxjh"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ;no tests
       #:configure-flags (list (string-append "-DRIME_DATA_DIR="
                                              (assoc-ref %build-inputs
                                                         "rime-data")
                                              "/share/rime-data"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-source
                    (lambda _
                      (substitute* "data/CMakeLists.txt"
                        (("DESTINATION....RIME_DATA_DIR..")
                         "DESTINATION \"${CMAKE_INSTALL_DATADIR}/rime-data\""))
                      #t)))))
    (inputs (list fcitx5 librime rime-data))
    (native-inputs (list gettext-minimal extra-cmake-modules pkg-config))
    (home-page "https://github.com/fcitx/fcitx5-rime")
    (synopsis "Rime Input Method Engine for Fcitx 5")
    (description
     "@dfn{fcitx5-rime} provides the Rime input method engine for fcitx5.
Rime is a lightweight, extensible input method engine supporting various input
schemas including glyph-based input methods, romanization-based input methods
as well as those for Chinese dialects.  It has the ability to compose phrases
and sentences intelligently and provide very accurate traditional Chinese
output.")
    (license license:lgpl2.1+)))
