;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 L  p R n  d n <guix@lprndn.info>
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

(define-module (gnu packages arcan)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public arcan
  (let ((commit "b4dd1fbd1938492ff4b269189d3c8524be7450a9")
        (revision "1"))
    (package
      (name "arcan")
      (version (git-version "0.5.5.2" revision commit))
      (source (origin
                (method git-fetch)
                (file-name (git-file-name name version))
                (uri (git-reference
                      (url "https://github.com/letoram/arcan")
                      (commit commit)))
                (sha256
                 (base32 "1pd0avlzc2rig1hd37zbhc7r2s6fjzdhshfg9l9cfzibl7caclyw"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DCMAKE_C_FLAGS=-fcommon"
                             "-DVIDEO_PLATFORM=egl-dri" "-DBUILTIN_LUA=off"
                             "-DSTATIC_OPENAL=off""-DENABLE_LWA=on"
                             "-DSTATIC_SQLITE3=off" "-DSTATIC_FREETYPE=off"
                             "-DSHMIF_TUI_ACCEL=on")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-cmake-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/platform/cmake/modules/FindGBMKMS.cmake"
                 (("/usr/local/include/libdrm")
                  (search-input-directory inputs "include/libdrm")))
               (substitute* "src/platform/cmake/modules/FindAPR.cmake"
                 (("/usr/local/apr/include/apr-1")
                  (search-input-directory inputs "include/apr-1")))
               #t))
           ;; Normally, it tries to fetch patched openal with git
           ;; but copying files manually in the right place seems to work too.
           (add-after 'unpack 'prepare-static-openal
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((arcan-openal (assoc-ref inputs "arcan-openal")))
                 (copy-recursively arcan-openal "external/git/openal"))
               #t))
           (add-after 'prepare-static-openal 'generate-man
             (lambda _
               (with-directory-excursion "doc"
                 (invoke "ruby" "docgen.rb" "mangen"))
               #t))
           (add-before 'configure 'chdir
             (lambda _
               (chdir "src")
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/arcan")
                   `("ARCAN_RESOURCEPATH" ":" suffix
                     (,(string-append out "/share/arcan/resources")))
                   `("ARCAN_STATEBASEPATH" ":" =
                     ("$HOME/.arcan/resources/savestates"))
                   `("ARCAN_STATEPATH" ":" =
                     ("$HOME/.arcan/resources/savestates"))
                   `("ARCAN_BINPATH" ":" =
                     (,(string-append out "/bin/arcan_frameserver")))))
               #t)))
         #:tests? #f))
      (native-search-paths
       (list (search-path-specification
              (variable "ARCAN_APPLBASEPATH")
              (separator #f)
              (files '("share/arcan/appl")))
             (search-path-specification
              (variable "ARCAN_SCRIPTPATH")
              (separator #f)
              (files '("share/arcan/scripts")))))
      (inputs
       `(("apr" ,apr)
         ("ffmpeg" ,ffmpeg-4)
         ("freetype" ,freetype)
         ("glib" ,glib)
         ("glu" ,glu)
         ("harfbuzz" ,harfbuzz)
         ("libdrm" ,libdrm)
         ("libusb" ,libusb)
         ("libxkbcommon" ,libxkbcommon)
         ("lua" ,luajit)
         ("lzip" ,lzip)
         ("openal" ,openal)
         ("pcre" ,pcre)
         ("sqlite" ,sqlite)
         ("tesseract-ocr" ,tesseract-ocr)
         ("leptonica" ,leptonica)
         ("vlc" ,vlc)
         ;;  To build arcan_lwa, we need a patched version of openal.
         ;; https://github.com/letoram/arcan/wiki/packaging
         ("arcan-openal" ,(origin
                            (method git-fetch)
                            (file-name "arcan-openal-0.5.4")
                            (uri (git-reference
                                  (url "https://github.com/letoram/openal")
                                  (commit "1c7302c580964fee9ee9e1d89ff56d24f934bdef")))
                            (sha256
                             (base32
                              "0dcxcnqjkyyqdr2yk84mprvkncy5g172kfs6vc4zrkklsbkr8yi2"))))))
      (native-inputs
       (list pkg-config ruby))               ; For documentation and testing
      (home-page "https://arcan-fe.com")
      (synopsis "Display server, multimedia framework and game engine (egl-dri)")
      (description "Arcan is a development framework for creating virtually
anything from user interfaces for specialized embedded applications
all the way to full-blown desktop environments.  At its heart lies a multimedia
engine programmable using Lua.")
      ;; https://github.com/letoram/arcan/blob/master/COPYING
      (license (list license:gpl2+
                     license:lgpl2.0
                     license:lgpl2.0+
                     license:public-domain
                     license:bsd-3)))))

(define-public arcan-sdl
  (package
    (inherit arcan)
    (name "arcan-sdl")
    (inputs
     (modify-inputs (package-inputs arcan)
       (delete "libdrm")
       (prepend sdl)))
    (arguments
     `(,@(ensure-keyword-arguments
          (package-arguments arcan)
          '(#:configure-flags
            '("-DCMAKE_C_FLAGS=-fcommon"
              "-DVIDEO_PLATFORM=sdl" "-DBUILTIN_LUA=off"
              "-DSTATIC_OPENAL=off" "-DDISABLE_JIT=off"
              "-DENABLE_LWA=on" "-DSTATIC_SQLITE3=off"
              "-DSTATIC_FREETYPE=off" "-DSHMIF_TUI_ACCEL=on")))))
    (synopsis "Combined display server, multimedia framework and game engine (SDL)")))

(define-public xarcan
  (let ((commit "8e6ee029388326cfe5cddeffe482eb3702e9b7f3")
        (revision "1" ))
    (package
      (name "xarcan")
      (version (git-version "0.5.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (file-name (git-file-name name version))
         (uri (git-reference
               (url "https://github.com/letoram/xarcan")
               (commit commit)))
         (sha256
          (base32 "0zng7cs6733mnf0p6g5wv02981f2sf567n56csax6cmzb8fpamym"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         `("--enable-kdrive" "--enable-xarcan"
           "--disable-xorg" "--disable-xwayland"
           "--disable-xnest" "--disable-xvfb"
           "--enable-glamor" "--enable-glx"
           "--disable-int10-module" "--enable-ipv6"
           "--enable-record" "--without-systemd-daemon"
           "--enable-xcsecurity" "--disable-static"
           ,(string-append "--with-xkb-path="
                           (assoc-ref %build-inputs "xkeyboard-config")
                           "/share/X11/xkb")
           ,(string-append "--with-xkb-bin-directory="
                           (assoc-ref %build-inputs "xkbcomp")
                           "/bin")
           ,(string-append "--with-xkb-output="
                           "/tmp"))))   ; FIXME: Copied from xorg
      (native-inputs
       (list pkg-config autoconf automake libtool util-macros))
      (inputs
       `(("arcan" ,arcan)
         ("font-util" ,font-util)
         ("libdrm" ,libdrm)
         ("libepoxy" ,libepoxy)
         ("libkbfile" ,libxkbfile)
         ("libressl" ,libressl)
         ("libx11" ,libx11)
         ("libxfont2" ,libxfont2)
         ("mesa" ,mesa)
         ("pixman" ,pixman)
         ("xkeyboard-config" ,xkeyboard-config)
         ("xkbcomp" ,xkbcomp)
         ("xorgproto" ,xorgproto)
         ("xtrans" ,xtrans)))
      (home-page "https://arcan-fe.com")
      (synopsis "Patched Xserver that bridges connections to Arcan")
      (description "Patched Xserver with a KDrive backend that uses the arcan-shmif
 to map Xlib/Xcb/X clients to a running arcan instance.  It allows running an X session
as a window under Arcan.")
      (license license:expat))))

(define-public arcan-wayland
  (package
    (inherit arcan)
    (name "arcan-wayland")
    (native-inputs
     (list pkg-config))
    (inputs
     (list arcan
           libseccomp
           libxkbcommon
           mesa
           wayland
           wayland-protocols))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src/tools/waybridge")
             #t))
         (add-after 'unpack 'fix-cmake-find-shmif
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/platform/cmake/modules/Findarcan_shmif.cmake"
               (("/usr/local") (assoc-ref inputs "arcan")))
             #t)))))
    (synopsis "Wayland protocol service for Arcan")
    (description "Arcan-wayland (waybridge) bridges Wayland connections
with an Arcan connection point.  It allows Wayland compatible clients
to connect and render using Arcan.")
    (license license:bsd-3)))
