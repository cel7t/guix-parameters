;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015, 2016, 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2016 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2020 Nikita <nikita@n0.is>
;;; Copyright © 2016 doncatnip <gnopap@gmail.com>
;;; Copyright © 2016 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Kyle Andrews <kyle.c.andrews@gmail.com>
;;; Copyright © 2019 Ingo Ruhnke <grumbel@gmail.com>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2019 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019 Noodles! <nnoodle@chiru.no>
;;; Copyright © 2019, 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Boris A. Dekshteyn <harlequin78@gmail.com>
;;; Copyright © 2020 Marcin Karpezo <sirmacik@wioo.waw.pl>
;;; Copyright © 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2022, 2023 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020 Niklas Eklund <niklas.eklund@posteo.net>
;;; Copyright © 2020 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 lasnesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021, 2023 jgart <jgart@dismail.de>
;;; Copyright © 2021 Disseminate Dissent <disseminatedissent@protonmail.com>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2022 Pier-Hugues Pellerin <ph@heykimo.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Elais Player <elais@fastmail.com>
;;; Copyright © 2022, 2023 Trevor Richards <trev@trevdev.ca>
;;; Copyright © 2022 Fredrik Salomonsson <plattfot@posteo.net>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2023 Jonathan Brielamier <jonathan.brielmaier@web.de>
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

(define-module (gnu packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public bspwm
  (package
    (name "bspwm")
    (version "0.9.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/bspwm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qlv7b4c2mmjfd65y100d11x8iqyg5f6lfiws3cgmpjidhdygnxc"))))
    (build-system gnu-build-system)
    (inputs
     (list libxcb
           libxinerama
           sxhkd
           xcb-util
           xcb-util-keysyms
           xcb-util-wm))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure script
       #:tests? #f                      ; no check target
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" %output))))
    (home-page "https://github.com/baskerville/bspwm")
    (synopsis "Tiling window manager based on binary space partitioning")
    (description "bspwm is a tiling window manager that represents windows as
the leaves of a full binary tree.")
    (license license:bsd-2)))

(define-public herbstluftwm
  (package
    (name "herbstluftwm")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://herbstluftwm.org/tarballs/herbstluftwm-"
                           version ".tar.gz"))
       (sha256
        (base32 "01c1f5041bblg8d7p12jkynd57xi1frxy61qsrdcxgp5144n1m5j"))
       (file-name (string-append "herbstluftwm-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (inputs
     (list dzen
           dmenu
           glib
           glibmm
           xterm
           xsetroot
           libx11
           libxcursor
           libxext
           libxfixes
           libxinerama
           libxrandr
           libxft))
    (native-inputs
     (list asciidoc pkg-config python))
    (arguments
     '(#:tests? #f
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "-DCC=gcc"
               (string-append "-DCMAKE_INSTALL_SYSCONF_PREFIX=" out "/etc")
               (string-append "-DBASHCOMPLETIONDIR=" out "/etc/bash_completion.d")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'link-libxcursor
           (lambda _
             ;; libX11 will dlopen libXcursor to load cursors.
             (setenv "LDFLAGS" "-lXcursor")))
         (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (call-with-output-file
                   (string-append xsessions "/herbstluftwm.desktop")
                 (lambda (port)
                   (format port "~
                     [Desktop Entry]~@
                     Name=herbstluftwm~@
                     Comment=Manual tiling window manager~@
                     Exec=~a/bin/herbstluftwm~@
                     Type=XSession~%" out)))
               #t))))))
    (synopsis "Tiling window manager for X11")
    (description "herbstluftwm is a manual tiling window manager for X11 using
Xlib and GLib.  Its main features are:

@itemize
@item
The layout is based on splitting frames into subframes which can be split
again or can be filled with windows (similar to i3 or musca).

@item
Tags (or workspaces or virtual desktops or …) can be added/removed at runtime.
Each tag contains an own layout.

@item
Exactly one tag is viewed on each monitor.  The tags are monitor independent
(similar to Xmonad).

@item
It is configured at runtime via IPC calls from @command{herbstclient}.  So the
configuration file is just a script which is run on startup (similar to wmii
or musca).

@end itemize")
    (home-page "https://herbstluftwm.org")
    (license license:bsd-2)))

(define-public i3status
  (package
    (name "i3status")
    (version "2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://i3wm.org/i3status/i3status-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0929chhvyq9hg4scpcz8r9zn3s9jvbg6a86k3wqa77qg85rh4kaw"))
              (snippet
               ;; Delete the pregenerated man page, to be rebuilt from source.
               '(delete-file "man/i3status.1"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           '(list "-Dmans=True")
           #:tests? #f))                ; no test suite
    (inputs
     (list alsa-lib
           libconfuse
           libnl
           yajl
           pulseaudio))
    (native-inputs
     (list asciidoc
           perl
           pkg-config
           docbook-xsl libxml2          ; for XML_CATALOG_FILES
           xmlto))
    (home-page "https://i3wm.org/i3status/")
    (synopsis "Status bar for i3bar, dzen2, xmobar or similar programs")
    (description "i3status is a small program for generating a status bar for
i3bar, dzen2, xmobar or similar programs.  It is designed to be very efficient
by issuing a very small number of system calls, as one generally wants to
update such a status line every second.  This ensures that even under high
load, your status bar is updated correctly.  Also, it saves a bit of energy by
not hogging your CPU as much as spawning the corresponding amount of shell
commands would.")
    (license license:bsd-3)))

(define-public i3-wm
  (package
    (name "i3-wm")
    (version "4.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://i3wm.org/downloads/i3-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0jrya4rhh46sivlmqaqc4n9abpp1yn1ajhi616gn75cxwl8rjqr8"))))
    (build-system meson-build-system)
    (arguments
     `(;; The test suite requires the unpackaged Xephyr X server.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-session-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (i3 (string-append out "/bin/i3"))
                    (i3-with-shmlog (string-append out "/bin/i3-with-shmlog")))
               (substitute* (string-append out "/share/xsessions/i3.desktop")
                 (("Exec=i3") (string-append "Exec=" i3)))
               (substitute* (string-append out "/share/xsessions/i3-with-shmlog.desktop")
                 (("Exec=i3-with-shmlog") (string-append "Exec=" i3-with-shmlog)))
               #t))))))
    (inputs
     (list libxcb
           xcb-util
           xcb-util-cursor
           xcb-util-keysyms
           xcb-util-wm
           xcb-util-xrm
           libxkbcommon
           libev
           yajl
           xmlto
           perl-pod-simple
           libx11
           pcre2
           startup-notification
           pango
           cairo))
    (native-inputs
     (list which
           perl
           pkg-config
           asciidoc
           ;; For building the documentation.
           libxml2
           docbook-xsl))
    (home-page "https://i3wm.org/")
    (synopsis "Tiling window manager")
    (description "i3 is a tiling X11 window manager that dynamically manages
tiled, stacked, and tabbed window layouts.

i3 primarily targets advanced users.  Windows are managed manually and organised
inside containers, which can be split vertically or horizontally, and optionally
resized.

i3 uses a plain-text configuration file, and can be extended and controlled from
many programming languages.")
    (properties
     `((upstream-name . "i3")
       (release-monitoring-url . "https://i3wm.org/downloads")))
    (license license:bsd-3)))

(define-public i3-gaps
  (deprecated-package "i3-gaps" i3-wm))

(define-public i3lock
  (package
    (name "i3lock")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://i3wm.org/i3lock/i3lock-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "02szjsaz7rqrdkd0r2nwgwa85c4hwfrcskxw7ryk695kmjcfhzv3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo
           libev
           linux-pam
           libxcb
           libxkbcommon
           xcb-util
           xcb-util-image
           xcb-util-xrm))
    (home-page "https://i3wm.org/i3lock/")
    (synopsis "Lightweight screen locker")
    (description
     "i3lock is a simple X11 screen locker developed alongside the i3 project.
Despite the name it should work with any X11 window manager.")
    (license license:bsd-3)))

(define-public i3lock-blur
  (package
    (name "i3lock-blur")
    (version "2.10")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/karulont/i3lock-blur")
                (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bd5nrlga5g1sz1f64gnc3dqy8yfrr4q1ss59krymbpxa1hhf55c"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config automake autoconf))
    (inputs
     (list cairo
           mesa
           libev
           linux-pam
           libxcb
           libxkbcommon
           xcb-util
           xcb-util-image
           xcb-util-xrm))
    (home-page "https://github.com/karulont/i3lock-blur")
    (synopsis "Lightweight screen locker with transparent blurring background")
    (description
     "Simple X11 screen locker with transparent blurring background developed
alongside the i3 project.  Despite the name it should work with any X11 window
manager.")
    (license license:expat)))

(define-public i3blocks
  (package
    (name "i3blocks")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vivien/i3blocks")
                    (commit version)))
              (sha256
               (base32
                "0v8mwnm8qzpv6xnqvrk43s4b9iyld4naqzbaxk4ldq1qkhai0wsv"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://github.com/vivien/i3blocks")
    (synopsis "Minimalist scheduler for status bar scripts")
    (description "i3blocks executes your command lines and generates a
status line from their output.  The generated line is meant to be displayed by
the i3 window manager through its i3bar component, as an alternative to
i3status.")
    (license license:gpl3+)))

(define-public perl-anyevent-i3
  (package
    (name "perl-anyevent-i3")
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MS/MSTPLBG/"
                                  "AnyEvent-I3-" version ".tar.gz"))
              (sha256
               (base32
                "0qvd9bq16jyy7v3ma82qcnvz9j503bw0mh7h55gkjf7ir62ck0jk"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-anyevent perl-json-xs))
    (home-page "https://metacpan.org/release/AnyEvent-I3")
    (synopsis
     "Communicate with the i3 window manager through perl")
    (description
     "This module connects to the i3 window manager using the UNIX socket
based IPC interface it provides (if enabled in the configuration file).
You can then subscribe to events or send messages and receive their replies.")
    ;; Can be used with either license.
    (license (list license:gpl3+ license:perl-license))))

(define-public python-i3-py
  (package
    (name "python-i3-py")
    (version "0.6.5")
    (source
     (origin
       ;; The latest release is not tagged in Git nor has an entry in PyPi,
       ;; but there is still a clear commit for it, and it's been the last one
       ;; for years.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziberna/i3-py")
             (commit "27f88a616e9ecc340e7d041d3d00782f8a1964c1")))
       (sha256
        (base32
         "1nm719dc2xqlll7vj4c4m7mpjb27lpn3bg3c66gajvnrz2x1nmxs"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests yet
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-doc
                    ;; Copy readme file to documentation directory.
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((doc (string-append (assoc-ref outputs "out")
                                                "/share/doc/" ,name)))
                        (install-file "README.md" doc)
                        ;; Avoid unspecified return value.
                        #t))))))
    (propagated-inputs
     (list i3-wm))
    (home-page "https://github.com/ziberna/i3-py")
    (synopsis "Python interface to the i3 window manager")
    (description "This package allows you to interact from a Python program
with the i3 window manager via its IPC socket.  It can send commands and other
kinds of messages to i3, select the affected containers, filter results and
subscribe to events.")
    (license license:gpl3+)))

(define-public qtile
  (package
    (name "qtile")
    (version "0.18.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "qtile" version))
        (sha256
          (base32 "14hb26xkza7brvkd4276j60mxd3zsas72ih6y0cq3j060izm1865"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Tests require Xvfb and writable temp/cache space
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libqtile/pangocffi.py"
               (("^gobject = ffi.dlopen.*")
                 (string-append "gobject = ffi.dlopen(\""
                  (assoc-ref inputs "glib") "/lib/libgobject-2.0.so.0\")\n"))
                (("^pango = ffi.dlopen.*")
                 (string-append "pango = ffi.dlopen(\""
                  (assoc-ref inputs "pango") "/lib/libpango-1.0.so.0\")\n"))
                (("^pangocairo = ffi.dlopen.*")
                 (string-append "pangocairo = ffi.dlopen(\""
                  (assoc-ref inputs "pango") "/lib/libpangocairo-1.0.so.0\")\n")))))
       (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions"))
                    (qtile (string-append out "/bin/qtile start")))
               (mkdir-p xsessions)
               (copy-file "resources/qtile.desktop" (string-append xsessions "/qtile.desktop"))
               (substitute* (string-append xsessions "/qtile.desktop")
                 (("qtile start") qtile))))))))
    (inputs
      (list glib pango pulseaudio))
    (propagated-inputs
      (list python-cairocffi
            python-cffi
            python-dateutil
            python-dbus-next
            python-iwlib
            python-keyring
            python-mpd2
            python-pyxdg
            python-xcffib))
    (native-inputs
      (list pkg-config
            python-flake8
            python-pep8-naming
            python-psutil
            python-pytest-cov
            python-setuptools-scm))
    (home-page "http://qtile.org")
    (synopsis "Hackable tiling window manager written and configured in Python")
    (description "Qtile is simple, small, and extensible.  It's easy to write
your own layouts, widgets, and built-in commands.")
    (license license:expat)))

(define-public quickswitch-i3
  (let ((commit "ed692b1e8f43b95bd907ced26238ce8ccb2ed28f")
        (revision "1")) ; Guix package revision
    (package
      (name "quickswitch-i3")
      (version (string-append "2.2-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         ;; The latest commit is a few years old and just a couple commits
         ;; after the last tagged release, so we use that latest commit
         ;; instead of the release.
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/proxypoke/quickswitch-for-i3")
               (commit commit)))
         (sha256
          (base32
           "0447077sama80jcdg5p64zjsvafmz5rbdrirhm1adcdjhkh6iqc5"))
         (patches (search-patches "quickswitch-fix-dmenu-check.patch"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; no tests yet
         #:phases (modify-phases %standard-phases
                    (add-after 'install 'install-doc
                      ;; Copy readme file to documentation directory.
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((doc (string-append (assoc-ref outputs "out")
                                                  "/share/doc/" ,name)))
                          (install-file "README.rst" doc)
                          ;; Avoid unspecified return value.
                          #t))))))
      (inputs
       (list python-i3-py dmenu))
      (home-page "https://github.com/proxypoke/quickswitch-for-i3")
      (synopsis "Quickly change to and locate windows in the i3 window manager")
      (description
       "This utility for the i3 window manager allows you to quickly switch to
and locate windows on all your workspaces, using an interactive dmenu
prompt.")
      (license license:wtfpl2))))

(define-public i3lock-color
  (package
    (name "i3lock-color")
    (version "2.13.c.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Raymo111/i3lock-color")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lnyh8spbf1ar4xan5v7q8i2i51aq1i60kzbfkn9w3wa0jzf9f3d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests included
    (inputs
     (list cairo
           libev
           libjpeg-turbo
           libxcb
           libxkbcommon
           linux-pam
           xcb-util
           xcb-util-image
           xcb-util-xrm))
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://github.com/Raymo111/i3lock-color")
    (synopsis "Screen locker with color configuration support")
    (description
     "i3lock-color is a simpler X11 screen locker derived from i3lock.
Features include:

@enumerate
@item forking process, the locked screen is preserved when you suspend to RAM;
@item specify background color or image to be displayed in the lock screen;
@item many additional color options.
@end enumerate")
    (license license:bsd-3)))

(define-public i3lock-fancy
  (package
    (name "i3lock-fancy")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/meskarune/i3lock-fancy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11g2kkim33ra38d1m897sq1ifajw17iyw9mr7sg1q8i2ibl4lfsi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (icons (string-append out "/share/i3lock-fancy/icons/"))
                    (wmctrl (search-input-file inputs "/bin/wmctrl"))
                    (mconvert (search-input-file inputs "/bin/convert"))
                    (mimport (search-input-file inputs "/bin/import"))
                    (awk (search-input-file inputs "/bin/gawk")))

               (substitute* "lock"
                 (("\\$\\(command -V wmctrl\\)") wmctrl)
                 (("convert") mconvert)
                 (("shot=\\(import") (string-append "shot=\(" mimport))
                 (("awk -F") (string-append awk " -F"))
                 ((" awk") awk)
                 (("\\$scriptpath/icons/") icons))
               #t)))
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (icons (string-append out "/share/i3lock-fancy/icons/")))

               (install-file "lock" bin)
               (rename-file (string-append bin "/lock")
                            (string-append bin "/i3lock-fancy"))
               (copy-recursively "icons" icons)
               #t))))))
    (inputs
     (list imagemagick wmctrl i3lock gawk))
    (home-page "https://github.com/meskarune/i3lock-fancy")
    (synopsis "Screen locker with screenshot function")
    (description
     "@code{i3lock-fancy} is a Bash script that takes a screenshot of
the desktop, blurs the background and adds a lock icon and text.
It requires @code{i3lock-color} or @code{i3lock} and can optionally
be passed any screenshot util like @code{scrot}.
This screen locker can be used with any window manager or
desktop environment.")
    (license license:expat)))

(define-public icewm
  (package
    (name "icewm")
    (version "3.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ice-wm/icewm/releases/download/"
                    version "/icewm-" version ".tar.lz"))
              (sha256
               (base32
                "124w00yq35mj859nf321bpy36ij7a0kblcg95axxahz4l6fhxfxf"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list fontconfig
                  fribidi
                  glib                  ;for icewm-menu-fdo
                  imlib2
                  libice
                  libjpeg-turbo
                  libsm
                  libxcomposite
                  libxdamage
                  libxext
                  libxfixes
                  libxft
                  libxinerama
                  libxpm
                  libxrandr
                  libxrender
                  libx11
                  lzip
                  perl))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-gmo-files
                 ;; gmo files are generated from .po files
                 ;; so remove them before build to make sure
                 ;; they are re-generated if needed
                 (lambda _
                   (for-each delete-file
                             (find-files "po" "\\.gmo$"))))
               (add-after 'unpack 'skip-failing-test
                 ;; strtest.cc tests failing due to $HOME and /etc setup
                 ;; difference under guix
                 (lambda _
                   (substitute* "src/Makefile.in"
                     (("TESTS = strtest\\$\\(EXEEXT\\)")
                      "TESTS = ")))))))
    (home-page "https://ice-wm.org/")
    (synopsis "Window manager for the X Window System")
    (description
     "IceWM is a window manager for the X Window System.  The goal of IceWM is
speed, simplicity, and not getting in the user’s way.  It comes with a taskbar
with pager, global and per-window keybindings and a dynamic menu system.
Application windows can be managed by keyboard and mouse.  Windows can be
iconified to the taskbar, to the tray, to the desktop or be made hidden.  They
are controllable by a quick switch window (Alt+Tab) and in a window list.  A
handful of configurable focus models are menu-selectable.  Setups with
multiple monitors are supported by RandR and Xinerama.  IceWM is very
configurable, themeable and well documented.  It includes an optional external
background wallpaper manager with transparency support, a simple session
manager and a system tray.")
    (license license:lgpl2.0)))

(define-public xmonad
  (package
    (name "xmonad")
    (version "0.17.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "xmonad" version))
              (sha256
               (base32
                "1apqwyqmc51gamfgsvlanzqqig9qvjss89ibcamhnha1gs1k4jl8"))
              (patches (search-patches "xmonad-dynamic-linking.patch"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xmonad")))
    (inputs (list ghc-x11 ghc-data-default-class ghc-setlocale))
    (native-inputs (list ghc-quickcheck ghc-quickcheck-classes))
    (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-xsession
             (lambda _
               (let ((xsessions (string-append #$output "/share/xsessions")))
                 (mkdir-p xsessions)
                 (call-with-output-file (string-append xsessions
                                                       "/xmonad.desktop")
                  (lambda (port)
                    (format port "~
                     [Desktop Entry]~@
                     Name=~a~@
                     Comment=xmonad window manager~@
                     Exec=~a/bin/xmonad~@
                     Type=Application~%" #$name #$output)))))))
       #:cabal-revision '("2"
                          "1rgwrnyb7kijzl2mqm8ks2nydh37q5vkbg4400rg9n6x13w2r9b3")))
    (home-page "http://xmonad.org")
    (synopsis "Tiling window manager")
    (description
     "Xmonad is a tiling window manager for X.  Windows are arranged
automatically to tile the screen without gaps or overlap, maximising screen
use.  All features of the window manager are accessible from the keyboard: a
mouse is strictly optional.  Xmonad is written and extensible in Haskell.
Custom layout algorithms, and other extensions, may be written by the user in
config files.  Layouts are applied dynamically, and different layouts may be
used on each workspace.  Xinerama is fully supported, allowing windows to be
tiled on several screens.")
    (license license:bsd-3)))

(define-public xmobar
  (package
    (name "xmobar")
    (version "0.46")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "xmobar" version))
              (sha256
               (base32
                "0glpiq7c0qwfcxnc2flgzj7afm5m1a9ghzwwcq7f8q27m21kddrd"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xmobar")))
    (native-inputs
     (list ghc-hspec hspec-discover))
    (inputs
     (list ghc-alsa-core
           ghc-alsa-mixer
           ghc-dbus
           ghc-hinotify
           ghc-http
           ghc-http-conduit
           ghc-http-types
           ghc-iwlib
           ghc-libmpd
           ghc-netlink
           ghc-old-locale
           ghc-parsec-numbers
           ghc-regex-compat
           ghc-temporary
           ghc-timezone-olson
           ghc-x11
           ghc-x11-xft
           ghc-cairo
           ghc-pango
           libxpm))
    (arguments
     `(#:configure-flags (list "--flags=all_extensions")
       ;; Haddock documentation is for the library.
       #:haddock? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'register 'remove-libraries
             (lambda* (#:key outputs #:allow-other-keys)
               (delete-file-recursively (string-append (assoc-ref outputs "out") "/lib"))))
         (add-before 'build 'patch-test-shebang
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "test/Xmobar/Plugins/Monitors/AlsaSpec.hs"
               (("/bin/bash") (which "bash"))))))))
    (home-page "https://xmobar.org")
    (synopsis "Minimalistic text based status bar")
    (description
     "@code{xmobar} is a lightweight, text-based, status bar written in
Haskell.  It was originally designed to be used together with Xmonad, but it
is also usable with any other window manager.  While xmobar is written in
Haskell, no knowledge of the language is required to install and use it.")
    (license license:bsd-3)))

(define-public yeganesh
  (package
    (name "yeganesh")
    (version "2.4")
    (source
     (origin

       (method url-fetch)
       (uri (string-append "http://dmwit.com/yeganesh/yeganesh-" version ".tar.gz"))
       (sha256
        (base32 "04djfyjab3c5y9z9x8zd0xcx0jyy35zq7cl9ddr4ppf6k5ky6iky"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-strict ghc-xdg-basedir))
    (home-page "http://dmwit.com/yeganesh/")
    (synopsis "Small wrapper around dmenu")
    (description "@code{yeganesh} is a small wrapper around demnu.  Like
dmenu, it accepts input on stdin and writes the chosen result on stdout.
Unlike dmenu, it mangles the input before it presents its choices.  In
particular, it displays commonly-chosen options before uncommon ones.")
    (license license:bsd-3)))

(define-public ghc-xmonad-contrib
  (package
    (name "ghc-xmonad-contrib")
    (version "0.17.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "xmonad-contrib" version))
              (sha256
               (base32
                "0lwj8xkyaw6h0rv3lz2jdqrwzz7yghfmnhpndygkb3wgyhvq6dxb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xmonad-contrib")))
    (inputs (list ghc-random ghc-x11 xmonad ghc-utf8-string ghc-x11-xft))
    (native-inputs (list ghc-quickcheck ghc-hspec))
    (arguments
     `(#:cabal-revision ("1"
                         "0dc9nbn0kaw98rgpi1rq8np601zjhdr1y0ydg6yb82wwaqawql6z")))
    (home-page "https://xmonad.org/")
    (synopsis "Third party extensions for xmonad")
    (description
     "Third party tiling algorithms, configurations, and scripts to Xmonad, a
tiling window manager for X.")
    (license license:bsd-3)))

(define-public evilwm
  (package
    (name "evilwm")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.6809.org.uk/evilwm/dl/evilwm-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jry36qkg2l02v37zvzszxvxm2d8c62z25gks5gdqqjl9ifbpv1j"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxrandr))
    (arguments
     `(#:modules ((srfi srfi-26)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:make-flags (let ((inputs (map (cut assoc-ref %build-inputs <>)
                                       '("libx11" "libxext" "libxrandr")))
                          (join (lambda (proc strs)
                                  (string-join (map proc strs) " ")))
                          (dash-I (cut string-append "-I" <> "/include"))
                          (dash-L (cut string-append "-L" <> "/lib")))
                      `("desktopfilesdir=$(prefix)/share/xsessions"
                        ,(string-append "prefix=" (assoc-ref %outputs "out"))
                        ,(string-append "CPPFLAGS=" (join dash-I inputs))
                        ,(string-append "LDFLAGS=" (join dash-L inputs))))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)))) ;no configure script
    (home-page "https://www.6809.org.uk/evilwm/")
    (synopsis "Minimalist window manager for the X Window System")
    (description
     "evilwm is a minimalist window manager based on aewm, extended to feature
many keyboard controls with repositioning and maximize toggles, solid window
drags, snap-to-border support, and virtual desktops.")
    (license (license:x11-style "file:///README"))))

(define-public fluxbox
  (package
    (name "fluxbox")
    (version "1.3.7")
    (synopsis "Small and fast window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fluxbox/fluxbox/"
                                  version "/fluxbox-" version ".tar.xz"))
              (sha256
               (base32
                "1h1f70y40qd225dqx937vzb4k2cz219agm1zvnjxakn5jkz7b37w"))
              (patches
               (search-patches "fluxbox-1.3.7-no-dynamic-cursor.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CPPFLAGS=-U__TIME__") ;ugly, but for reproducibility
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'force-bootstrap
           (lambda _
             (delete-file "configure")))
         (add-after 'install 'install-vim-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (syntax (string-append out "/share/vim/vimfiles/syntax")))
               (copy-recursively "3rd/vim/vim/syntax" syntax)
               #t)))
         (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (call-with-output-file
                 (string-append xsessions "/fluxbox.desktop")
                 (lambda (port)
                   (format port "~
                     [Desktop Entry]~@
                     Name=~a~@
                     Comment=~a~@
                     Exec=~a/bin/startfluxbox~@
                     Type=Application~%" ,name ,synopsis out)))
               #t))))))
    (native-inputs
     (list autoconf automake gnu-gettext pkg-config))
    (inputs
     (list freetype
           fribidi
           imlib2
           libx11
           libxcursor
           libxext
           libxft
           libxinerama
           libxpm
           libxrandr
           libxrender))
    (description "Fluxbox is a window manager.  It is light on resources
and easy to handle yet full of features to make an easy and fast desktop
experience.")
    (home-page "http://fluxbox.org/")
    (license license:expat)))

(define-public fbautostart
  (package
    (name "fbautostart")
    (version "2.718281828")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/paultag/fbautostart.git")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "13h6j5khi5axqhflzhayzgvyhxylmk5vsgin235ji440mzd516gz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (synopsis "XDG autostarter for Fluxbox window manager")
    (description "This package provides an autostarter complaint with
the XDG Autostart specification.")
    (home-page "https://github.com/paultag/fbautostart")
    (license license:expat)))

(define-public fnott
  (package
    (name "fnott")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/dnkl/fnott")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00zg03nz79kqcsnwmm22friawhvl05f93yxpvqmy5wvggx9hrlz8"))))
    (build-system meson-build-system)
    (arguments `(#:build-type "release"))
    (native-inputs
     (list pkg-config
           wayland-protocols
           tllist
           scdoc))
    (inputs
     (list wlroots wayland fcft dbus libpng))
    (home-page "https://codeberg.org/dnkl/fnott")
    (synopsis "Keyboard driven and lightweight Wayland notification daemon")
    (description "Fnott is a keyboard driven and lightweight notification daemon
for wlroots-based Wayland compositors.")
    (license license:expat)))

(define-public awesome
  (package
    (name "awesome")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/awesomeWM/awesome-releases/raw/master/"
             "awesome-" version ".tar.xz"))
       (sha256
        (base32 "0lqpw401mkkmp9wgbvrmm45bqq2j9357l4irwdqv6l1305pls9kq"))
       (modules '((guix build utils)
                  (srfi srfi-19)))
       (snippet
        '(begin
           ;; Remove non-reproducible timestamp and use the date of
           ;; the source file instead.
           (substitute* "common/version.c"
             (("__DATE__ \" \" __TIME__")
              (date->string
               (time-utc->date
                (make-time time-utc 0 (stat:mtime (stat "awesome.c"))))
               "\"~c\"")))
           #t))
       (patches
        (search-patches "awesome-reproducible-png.patch"
                        "awesome-4.3-fno-common.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list asciidoc
           docbook-xsl
           doxygen
           gperf
           imagemagick
           libxml2 ;for XML_CATALOG_FILES
           lua-ldoc
           pkg-config
           xmlto))
    (inputs
     (list cairo
           dbus
           gdk-pixbuf
           glib
           gobject-introspection
           imlib2
           libev
           libxcb
           libxcursor
           libxdg-basedir
           libxkbcommon
           lua
           lua-lgi
           pango
           startup-notification
           xcb-util
           xcb-util-cursor
           xcb-util-image
           xcb-util-keysyms
           xcb-util-renderutil
           xcb-util-xrm
           xcb-util-wm))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 match))
       ;; Let compression happen in our 'compress-documentation' phase
       ;; so that '--no-name' is used, which removes timestamps from
       ;; gzip output.
       #:configure-flags
       '("-DCOMPRESS_MANPAGES=off")
       ;; Building awesome in its source directory is no longer
       ;; supported.
       #:out-of-source? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/awful/completion.lua"
               (("/usr/bin/env")
                ""))
             ;; The build process needs to load Cairo dynamically.
             (let* ((cairo (string-append (assoc-ref inputs "cairo") "/lib"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (lua-dependencies
                     (filter (match-lambda
                               ((label . _) (string-prefix? "lua-" label)))
                             inputs))
                    (lua-path
                     (string-join
                      (map (match-lambda
                             ((_ . dir)
                              (string-append
                               dir "/share/lua/" lua-version "/?.lua;"
                               dir "/share/lua/" lua-version "/?/?.lua")))
                           lua-dependencies)
                      ";"))
                    (lua-cpath
                     (string-join
                      (map (match-lambda
                             ((_ . dir)
                              (string-append
                               dir "/lib/lua/" lua-version "/?.so;"
                               dir "/lib/lua/" lua-version "/?/?.so")))
                           lua-dependencies)
                      ";")))
               (setenv "LD_LIBRARY_PATH" cairo)
               (setenv "LUA_PATH" (string-append "?.lua;" lua-path))
               (setenv "LUA_CPATH" lua-cpath)
               (setenv "HOME" (getcwd))
               (setenv "XDG_CACHE_HOME" (getcwd)))))
         (replace 'check
           (lambda _
             ;; There aren't any tests, so just make sure the binary
             ;; gets built and can be run successfully.
             (invoke "../build/awesome" "-v")))
         (add-after 'install 'patch-session-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (awesome (string-append out "/bin/awesome")))
               (substitute* (string-append out "/share/xsessions/awesome.desktop")
                 (("Exec=awesome") (string-append "Exec=" awesome)))
               #t)))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((awesome (assoc-ref outputs "out"))
                    (cairo (string-append (assoc-ref inputs "cairo") "/lib"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (lua-lgi (assoc-ref inputs "lua-lgi")))
               (wrap-program (string-append awesome "/bin/awesome")
                 `("LUA_PATH" ";" suffix
                   (,(format #f "~a/share/lua/~a/?.lua" lua-lgi lua-version)))
                 `("LUA_CPATH" ";" suffix
                   (,(format #f "~a/lib/lua/~a/?.so" lua-lgi lua-version)))
                 `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
                 `("LD_LIBRARY_PATH" suffix (,cairo)))
               #t))))))
    (home-page "https://awesomewm.org/")
    (synopsis "Highly configurable window manager")
    (description
     "Awesome has been designed as a framework window manager.  It is fast, small,
dynamic and extensible using the Lua programming language.")
    (license license:gpl2+)))

(define-public menumaker
  (package
    (name "menumaker")
    (version "0.99.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/menumaker/"
                           "menumaker-" version ".tar.gz"))
       (sha256
        (base32 "0nnnc1awvhq5pplvclap3ha61g9209bca6zqgpsm1f53fq75vs8i"))))
    (build-system gnu-build-system)
    (inputs
     (list python))
    (synopsis "Heuristics-driven menu generator")
    (description
     "MenuMaker is a menu generation utility for a number of X window
managers and desktop environments.  It is capable of finding lots of
installed programs and generating a root menu consistent across all
supported window managers, so one will get (almost) the same menu in
all of them.  Currently supported window managers include:

@enumerate
@item BlackBox
@item Deskmenu
@item FluxBox
@item IceWM
@item OpenBox
@item PekWM
@item WindowMaker
@item XFCE
@end enumerate\n")
    (home-page "https://menumaker.sourceforge.net/")
    (license license:bsd-2)))

(define-public keybinder
  (package
    (name "keybinder")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/kupferlauncher/keybinder"
                           "/releases/download/" name "-3.0-v" version "/"
                           name "-3.0-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0830ihwnalw59pp1xzrp37dn58n8vwb8zasnm4a1h81v3x7dxqz6"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+ gobject-introspection))
    (native-inputs
     (list gtk-doc pkg-config))
    (synopsis "Library for registering global keyboard shortcuts, Gtk3 version")
    (description
     "Keybinder is a library for registering global keyboard shortcuts.
Keybinder works with GTK-based applications using the X Window System.")
    (home-page "https://github.com/kupferlauncher/keybinder")
    (license license:x11)))

(define-public keybinder-3.0
  (deprecated-package "keybinder-3.0" keybinder))

(define-public spectrwm
  (package
    (name "spectrwm")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/conformal/spectrwm")
             (commit
              (string-append "SPECTRWM_"
                             (string-join (string-split version #\.) "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dfqy5f0s1nv6rqkz9lj006vypmp4rwxd5vczfk3ndzqgnh19kw6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (let ((pkg-config (lambda (flag)
                                        (string-append
                                         "$(shell pkg-config " flag " "
                                         "xft fontconfig x11 libpng)"))))
                      (list
                       "CC=gcc"
                       (string-append "PREFIX=" %output)
                       (string-append "INCS=-I. " (pkg-config "--cflags"))
                       (string-append "LIBS=" (pkg-config "--libs") " -lm")))
       #:tests? #f                      ;no test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-dir
           (lambda _
             (chdir "linux") #t))
         (add-after 'change-dir 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               (("-g") ""))))
         (delete 'configure))))         ;no 'configure' exists
    (inputs
     `(("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("libx11" ,libx11)
       ("libxcursor" ,libxcursor)
       ("libxrandr" ,libxrandr)
       ("libxtst" ,libxtst)
       ("libxft" ,libxft)
       ("xcb-util" ,xcb-util)
       ("xcb-util-wm" ,xcb-util-wm)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (native-inputs
     (list libxt pkg-config))
    (synopsis "Minimalistic automatic tiling window manager")
    (description
     "Spectrwm is a small dynamic tiling and reparenting window manager for X11.
It is inspired by Xmonad and dwm.  Its major features include:

@itemize
@item Navigation anywhere on all screens with either the keyboard or mouse
@item Customizable status bar
@item Restartable without losing state
@item Quick launch menu
@item Many screen layouts possible with a few simple key strokes
@item Move/resize floating windows
@item Extended Window Manager Hints (@dfn{EWMH}) support
@item Configurable tiling
@item Adjustable tile gap allows for a true one pixel border
@item Customizable colors and border width
@end itemize\n")
    (home-page "https://github.com/conformal/spectrwm")
    (license license:isc)))

(define-public cwm
  (package
    (name "cwm")
    (version "7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://leahneukirchen.org/releases/cwm-"
                           version ".tar.gz"))
       (sha256
        (base32 "145xjwam11194w2irsvs4z0xgn0jdijxfmx67gqd1n0j8g5wan2a"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'build 'install-xsession
            (lambda _
              ;; Add a .desktop file to xsessions.
              (let ((xsessions (string-append #$output "/share/xsessions")))
                (mkdir-p xsessions)
                (make-desktop-entry-file
                 (string-append xsessions "/cwm.desktop")
                 #:name "cwm"
                 #:exec (string-append #$output "/bin/cwm")
                 #:try-exec (string-append #$output "/bin/cwm")
                 #:comment '((#f "OpenBSD Calm Window Manager fork")))))))))
    (native-inputs
     (list bison pkg-config))
    (inputs
     (list libxrandr libxft libxinerama))
    (home-page "https://github.com/leahneukirchen/cwm")
    (synopsis "OpenBSD fork of the Calm Window Manager")
    (description "Cwm is a stacking window manager for X11.  It is an OpenBSD
project derived from the original Calm Window Manager.")
    (license license:isc)))

(define-public dwl
  (package
    (name "dwl")
    (version "0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/djpohly/dwl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pj0h3zd2f60hxpavpmgzid1sj7hf9m5cgclbackljqq4gpwlvir"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure
    (native-inputs
     (list pkg-config))
    (inputs
     (list wlroots))
    (home-page "https://github.com/djpohly/dwl")
    (synopsis "Dynamic window manager for Wayland")
    (description
     "@command{dwl} is a compact, hackable compositor for Wayland based on
wlroots.  It is intended to fill the same space in the Wayland world that dwm
does in X11, primarily in terms of philosophy, and secondarily in terms of
functionality.  Like dwm, dwl is easy to understand and hack on, due to a
limited size and a few external dependencies.  It is configurable via
@file{config.h}.")
    ;;             LICENSE       LICENSE.dwm   LICENSE.tinywl
    (license (list license:gpl3+ license:expat license:cc0))))

(define-public nitrogen
  (package
    (name "nitrogen")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://github.com/l3ib/nitrogen/"
                                  "releases/download/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zc3fl1mbhq0iyndy4ysmy8vv5c7xwf54rbgamzfhfvsgdq160pl"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2 gtkmm-2 glib glibmm))
    (native-inputs
     (list pkg-config))
    (home-page "http://projects.l3ib.org/nitrogen/")
    (synopsis "Background browser and setter for X windows")
    (description
     "This package is a background browser and setter for X windows.  It's
features are:

@itemize
@item Multihead and Xinerama aware
@item Recall mode to used via startup script
@item Uses freedesktop.org standard for thumbnails
@item Can set GNOME background
@item Command lie set modes for script use
@item Inotify monitoring of browse directory
@item Lazy loading of thumbnails - conserves memory
@item \"Automatic\" set mode - determines best mode to set an image based on
its size
@item Display preview images in a tiled icon layout
@end itemize")
    (license license:gpl2+)))

(define-public polybar
  (package
    (name "polybar")
    (version "3.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/polybar/polybar/releases/"
                           "download/" version "/polybar-" version ".tar.gz"))
       (sha256
        (base32 "19azx5dpfyfh0pv4q2fcrf4p7a0pc5d13m7lnv3qy8376mbmhmzj"))))
    (build-system cmake-build-system)
    (arguments
     ;; Test is disabled because it requires downloading googletest from the
     ;; Internet.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               ;; Make polybar find its default configuration file in the
               ;; store.
               (add-after 'unpack 'patch-config-path
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("/etc") (string-append #$output "/etc")))
                   (substitute* "src/utils/file.cpp"
                     (("\"/etc\"") (string-append "\"" #$output "/etc\""))))))))
    (inputs
     (list alsa-lib
           cairo
           i3-wm
           jsoncpp
           libmpdclient
           libnl
           libuv
           libxcb
           pulseaudio
           xcb-proto
           xcb-util
           xcb-util-cursor
           xcb-util-image
           xcb-util-wm
           xcb-util-xrm))
    (native-inputs
     (list pkg-config
           python-sphinx ; for the manual
           python))      ; xcb-proto depends on python 3
    (home-page "https://polybar.github.io/")
    (synopsis "Fast and easy-to-use status bar")
    (description "Polybar aims to help users build beautiful and highly
customizable status bars for their desktop environment.  It has built-in
functionality to display information about the most commonly used services.")
    (license license:expat)))

(define-public wlroots
  (package
    (name "wlroots")
    (version "0.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11kcica9waj1a1xgbi12gif9z5z0b4xzycbcgawbgdmj77pws8sk"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xwayland/server.c"
               (("Xwayland") (string-append (assoc-ref inputs
                                                       "xorg-server-xwayland")
                                            "/bin/Xwayland")))
             #t))
         (add-before 'configure 'fix-meson-file
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "backend/drm/meson.build"
               (("/usr/share/hwdata/pnp.ids")
                (string-append (assoc-ref inputs "hwdata")
                               "/share/hwdata/pnp.ids"))))))))
    (propagated-inputs
     (list ;; As required by wlroots.pc.
           eudev
           libinput-minimal
           libxkbcommon
           mesa
           pixman
           libseat
           wayland
           wayland-protocols
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))
    (native-inputs
     (list
       `(,hwdata "pnp")
       pkg-config))
    (home-page "https://github.com/swaywm/wlroots")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))  ; MIT license

(define-public sway
  (package
    (name "sway")
    (version "1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/sway")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17dqr2lkmcv2ssp7vky27zw599i77whpb1aqh1s6kl8a8vkrz6mg"))))
    (build-system meson-build-system)
    (arguments
     `(;; elogind is propagated by wlroots -> libseat
       ;; and would otherwise shadow basu.
       #:configure-flags '("-Dsd-bus-provider=basu")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode path to swaybg.
             (substitute* "sway/config.c"
               (("strdup..swaybg..")
                (string-append "strdup(\"" (assoc-ref inputs "swaybg")
                               "/bin/swaybg\")")))
             ;; Hardcode path to scdoc.
             (substitute* "meson.build"
               (("scdoc.get_pkgconfig_variable..scdoc..")
                (string-append "'" (assoc-ref inputs "scdoc")
                               "/bin/scdoc'")))
             #t)))))
    (inputs (list basu
                  cairo
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput-minimal
                  libxkbcommon
                  pango
                  pcre2
                  swaybg
                  wayland
                  wlroots))
    (native-inputs
     (list linux-pam mesa pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Wayland compositor compatible with i3")
    (description "Sway is a i3-compatible Wayland compositor.")
    (license license:expat)))

(define-public swayidle
  (package
    (name "swayidle")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swayidle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06iq12p4438d6bv3jlqsf01wjaxrzlnj1bnicn41kad563aq41xl"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dlogind-provider=elogind")))
    (inputs (list elogind wayland))
    (native-inputs (list pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/swayidle")
    (synopsis "Idle management daemon for Wayland compositors")
    (description "Swayidle is a idle management daemon for Wayland compositors.")
    (license license:expat))) ; MIT license

(define-public swaylock
  (package
    (name "swaylock")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swaylock")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03jrjwlwxkcyd6m9a1bbwapasnz7b7aws7h0y6jigjm4m478phv6"))))
    (build-system meson-build-system)
    (inputs (list cairo gdk-pixbuf libxkbcommon linux-pam wayland))
    (native-inputs (list pango pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Screen locking utility for Wayland compositors")
    (description "Swaylock is a screen locking utility for Wayland compositors.")
    (license license:expat))) ; MIT license

(define-public swaylock-effects
  (package
    (inherit swaylock)
    (name "swaylock-effects")
    (version "1.6.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirutka/swaylock-effects")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j7dxn66xqlf6iv2arqzz7mxlh7nf85anvpyf30d2frcidarda9h"))))
    (arguments
     (list #:configure-flags #~'("-Dsse=false")))
    (synopsis "Screen locking utility for Wayland compositors with effects")
    (description "@code{Swaylock-effects} is a fork of swaylock with additional
features, such as the ability to take a screenshot as the background image,
display a clock or apply image manipulation techniques to the background image.")
    (home-page "https://github.com/jirutka/swaylock-effects")))

(define-public swaybg
  (package
    (name "swaybg")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swaybg")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lmqz5bmig90gq2m7lwf02d2g7z4hzf8fhqz78c8vk92c6p4xwbc"))))
    (build-system meson-build-system)
    (inputs (list cairo gdk-pixbuf wayland))
    (native-inputs (list pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Screen wallpaper utility for Wayland compositors")
    (description "Swaybg is a wallpaper utility for Wayland compositors.")
    (license license:expat))) ; MIT license


(define-public swaynotificationcenter
  (package
    (name "swaynotificationcenter")
    (version "0.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ErikReider/SwayNotificationCenter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1xvr5m5sqznr3dd512i5pk0d56v7n0ywdcy6rnz85vbf2k7b6kj5"))))
    (build-system meson-build-system)
    (arguments (list #:configure-flags #~(list "-Dsystemd-service=false")))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           pkg-config
           python-minimal
           scdoc
           vala))
    (inputs
     (list json-glib
           glib
           gtk+
           gtk-layer-shell
           libhandy
           wayland-protocols))
    (synopsis "Notification daemon with a graphical interface")
    (description
     "This package provides a notification daemon for the Sway Wayland
compository, supporting the following featuers:

@itemize
@item Keyboard shortcuts
@item Notification body markup with image support
@item A panel to view previous notifications
@item Show album art for notifications like Spotify
@item Do not disturb
@item Click notification to execute default action
@item Show alternative notification actions
@item Customization through a CSS file
@item Trackpad/mouse gesture to close notification
@item The same features as any other basic notification daemon
@item Basic configuration through a JSON config file
@item Hot-reload config through swaync-client
@end itemize")
    (home-page "https://github.com/ErikReider/SwayNotificationCenter")
    (license license:expat)))

(define-public waybar
  (package
    (name "waybar")
    (version "0.9.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Alexays/Waybar")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11yia2fs5a05jlbrdhxm26c2sgmbj3iwsk3bsqcvjvv3mlsrhxkf"))))
    (build-system meson-build-system)
    (inputs (list date
                  fmt
                  gtk-layer-shell
                  gtkmm-3
                  jsoncpp
                  libdbusmenu
                  libinput-minimal
                  libmpdclient
                  libnl
                  libxml2
                  playerctl
                  pulseaudio
                  spdlog
                  wayland))
    (native-inputs
     (list `(,glib "bin") pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/Alexays/Waybar")
    (synopsis "Wayland bar for Sway and Wlroots based compositors")
    (description "Waybar is a highly customisable Wayland bar for Sway and
Wlroots based compositors.")
    (license license:expat))) ; MIT license

(define-public waybar-cpu-histogram
  (package
    (name "waybar-cpu-histogram")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~plattfot/waybar-cpu-histogram")
             (commit (string-append version))))
       (sha256
        (base32
         "001pyf1jdmf2478plnggd7dkfi688qwi89db2jwfp4zza3640na6"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list jsoncpp
           fmt))
    (synopsis "CPU histogram for waybar")
    (description
     "Custom module for waybar to show CPU usage as a histogram.  A compact way
to see how many cores are active, compared to having a bar for each
core/thread.")
    (home-page "https://github.com/plattfot/cpu-histogram/")
    (license license:expat)))

(define-public waybar-experimental
  (let ((base waybar))
    (package/inherit base
      (name "waybar-experimental")
      (arguments
       (list #:configure-flags #~(list "-Dexperimental=true")))
      (synopsis "Waybar with experimental features"))))

(define-public wlr-randr
  (package
    (name "wlr-randr")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/wlr-randr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cj24fb6s7n8nphvhrp8ldrivjdcrjw64i5v9rsfb6z80q4qg548"))))
    (build-system meson-build-system)
    (inputs (list wayland))
    (native-inputs (list pkg-config))
    (home-page "https://sr.ht/~emersion/wlr-randr")
    (synopsis "Utility to manage Wayland compositor outputs")
    (description "wlr-randr is a utility to manage outputs of a Wayland compositor.")
    (license license:expat))) ; MIT license

(define-public mako
  (package
    (name "mako")
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/mako")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vpar1a7zafkd2plmyaackgba6fyg35s9zzyxmj8j7v2q5zxirgz"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-makoctl
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "makoctl"
                     (("^BUSCTL=.*$")
                      (string-append
                       "BUSCTL="
                       (search-input-file inputs "bin/basuctl")
                       "\n"))
                     (("jq ")
                      (string-append
                       (search-input-file inputs "bin/jq")
                       " "))))))))
    (inputs (list basu cairo gdk-pixbuf jq pango wayland))
    (native-inputs (list pkg-config scdoc wayland-protocols))
    (home-page "https://wayland.emersion.fr/mako")
    (synopsis "Lightweight Wayland notification daemon")
    (description "Mako is a lightweight notification daemon for Wayland
compositors that support the layer-shell protocol.")
    (license license:expat))) ; MIT license

(define-public kanshi
  (package
    (name "kanshi")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~emersion/kanshi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "195v6lfh9w88nas6pca0v644nvrc645sramj78gzgqdm7nm20rvq"))))
    (build-system meson-build-system)
    (inputs (list wayland))
    (native-inputs (list pkg-config scdoc))
    (home-page "https://wayland.emersion.fr/kanshi")
    (synopsis "Hotswappable output profiles for Wayland")
    (description "Kanshi allows you to define output profiles that are
automatically enabled and disabled on hotplug.  Kanshi can be used with
Wayland compositors supporting the wlr-output-management protocol.")
    (license license:expat))) ; MIT license

(define-public stumpwm
  (package
    (name "stumpwm")
    (version "22.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stumpwm/stumpwm")
             (commit version)))
       (file-name (git-file-name "stumpwm" version))
       (sha256
        (base32 "1wxgddmkgmpml44a3m6bd8y529b13jz14apxxipmij10wzpgay6d"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list sbcl-fiasco
           texinfo

           ;; To build the manual.
           autoconf
           automake))
    (inputs
     (list sbcl-alexandria
           sbcl-cl-ppcre
           sbcl-clx))
    (outputs '("out" "lib"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* "stumpwm-tests.asd"
                (("\"ALL-TESTS\"")
                 "\"RUN-PACKAGE-TESTS\" :package"))))
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append (assoc-ref outputs "out") "/bin/stumpwm")
               outputs
               #:entry-program '((stumpwm:stumpwm) 0))))
          (add-after 'build-program 'create-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (xsessions (string-append out "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/stumpwm.desktop")
                  (lambda (file)
                    (format file
                       "[Desktop Entry]~@
                        Name=stumpwm~@
                        Comment=The Stump Window Manager~@
                        Exec=~a/bin/stumpwm~@
                        TryExec=~@*~a/bin/stumpwm~@
                        Icon=~@
                        Type=Application~%"
                       out))))))
          (add-after 'install 'install-manual
            (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (info (string-append out "/share/info")))
                (invoke "./autogen.sh")
                (invoke "sh" "./configure" "SHELL=sh")
                (apply invoke "make" "stumpwm.info" make-flags)
                (install-file "stumpwm.info" info)))))))
    (synopsis "Window manager written in Common Lisp")
    (description
     "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
    (home-page "https://github.com/stumpwm/stumpwm")
    (license license:gpl2+)
    (properties `((cl-source-variant . ,(delay cl-stumpwm))))))

(define-public cl-stumpwm
  (package
    (inherit (sbcl-package->cl-source-package stumpwm))
    (name "cl-stumpwm")))

(define-public stumpwm+slynk
  (package
    (inherit stumpwm)
    (name "stumpwm-with-slynk")
    (outputs '("out"))
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("slynk" ,sbcl-slynk)))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 (setenv "HOME" "/tmp")
                 (build-program program outputs
                                #:entry-program '((stumpwm:stumpwm) 0)
                                #:dependencies '("stumpwm" "slynk")
                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm" "slynk")))
                 #t)))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'cleanup)))))))

(define stumpwm-contrib
  (let ((commit "4613a956add7a17986a3b26c341229466cd13f1d")
        (revision "5"))
    (package
      (name "stumpwm-contrib")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm-contrib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1g8h2vd5qsmaiz6ixlx9ykrv6a08izmkf0js18fvljvznpyhsznz"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("stumpwm" ,stumpwm "lib")))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "StumpWM extra modules")
      (description "This package provides extra modules for StumpWM.")
      (license (list license:gpl2+ license:gpl3+ license:bsd-2)))))

(define-public stumpish
  (package
    (inherit stumpwm-contrib)
    (name "stumpish")
    (inputs
     (list bash rlwrap xprop))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         (chdir "util/stumpish")
         (substitute* "stumpish"
           (("rlwrap") (search-input-file %build-inputs "/bin/rlwrap"))
           (("xprop") (search-input-file %build-inputs "/bin/xprop"))
           (("/bin/sh") (search-input-file %build-inputs "/bin/bash")))
         (install-file "stumpish" (string-append %output "/bin")))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "StumpWM interactive shell")
    (description "This package provides a StumpWM interactive shell.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-pamixer
  (let ((commit "aa820533c80ea1af5a0e107cea25eaf34e69dc24")
        (revision "1"))
    (package
      (name "sbcl-stumpwm-pamixer")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Junker/stumpwm-pamixer")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0djcrr16bx40l7b60d4j507vk5l42fdgmjpgrnk86z1ba8wlqim8"))))
      (inputs (list pamixer `(,stumpwm "lib")))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list #:asd-systems ''("pamixer")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-pamixer
                   (lambda _
                     (substitute* "pamixer.lisp"
                       (("\"pamixer \"")
                        (string-append "\""
                                       #$(this-package-input "pamixer")
                                       "/bin/pamixer \""))))))))
      (home-page "https://github.com/Junker/stumpwm-pamixer")
      (synopsis "StumpWM Pamixer Module")
      (description
       "This package provides a minimalistic Pulseaudio volume and microphone
control module for StumpWM.")
      (license license:gpl3))))

(define-public sbcl-stumpwm-ttf-fonts
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-ttf-fonts")
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("clx-truetype" ,sbcl-clx-truetype)))
    (arguments
     '(#:asd-systems '("ttf-fonts")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/ttf-fonts") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Implementation of TTF font rendering for Lisp")
    (description "This package provides a Lisp implementation of TTF font
rendering.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-pass
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-pass")
    (arguments
     '(#:asd-systems '("pass")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/pass") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Integrate @code{pass} with StumpWM")
    (description "This package provides an interface which integrates
password-store into StumpWM.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-globalwindows
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-globalwindows")
    (arguments
     '(#:asd-systems '("globalwindows")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/globalwindows") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Manipulate all windows in the current X session")
    (description "This package provides a StumpWM module to manipulate all
windows in the current X session.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-swm-gaps
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-swm-gaps")
    (arguments
     '(#:asd-systems '("swm-gaps")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/swm-gaps") #t)))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Gaps between windows for StumpWM")
    (description "This package provides a StumpWM module which adds gaps
between windows.")
    (license (list license:gpl2+ license:gpl3+ license:bsd-2))))

(define-public sbcl-stumpwm-net
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-net")
    (arguments
     '(#:asd-systems '("net")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "modeline/net") #t)))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/net")
    (synopsis "Modeline support for network connectivity")
    (description "Modeline support for network connectivity.")
    (supported-systems
     (filter (lambda (a) (string-contains a "linux")) %supported-systems))
    (license license:gpl3+)))

(define-public sbcl-stumpwm-wifi
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-wifi")
    (arguments
     '(#:asd-systems '("wifi")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "modeline/wifi") #t)))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/wifi")
    (synopsis "Modeline support for wifi connectivity")
    (description "Modeline support for wifi connectivity.")
    (supported-systems
     (filter (lambda (a) (string-contains a "linux")) %supported-systems))
    (license license:gpl3+)))

(define-public sbcl-stumpwm-stumptray
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-stumptray")
    (arguments
     '(#:asd-systems '("stumptray")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "modeline/stumptray") #t)))))
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("xembed" ,sbcl-clx-xembed)
       ("alexandria" ,sbcl-alexandria)))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/stumptray")
    (synopsis "Modeline support for stumptray connectivity")
    (description "Modeline support for stumptray connectivity.")
    (supported-systems
     (filter (lambda (a) (string-contains a "linux")) %supported-systems))
    (license license:gpl3+)))

(define-public sbcl-stumpwm-kbd-layouts
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-kbd-layouts")
    (arguments
     '(#:asd-systems '("kbd-layouts")
       #:asd-operation "compile-system"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir (lambda _ (chdir "util/kbd-layouts") #t)))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/kbd-layouts")
    (synopsis "Keyboard layout switcher for StumpWM")
    (description "Keyboard layout switcher for StumpWM")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-numpad-layouts
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-numpad-layouts")
    (arguments
     '(#:asd-systems '("numpad-layouts")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/numpad-layouts"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/numpad-layouts")
    (synopsis "Keyboard numpad layouts for StumpWM")
    (description "This is a module for handling different keyboards numpad
layouts in StumpWM.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-cpu
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-cpu")
    (arguments
     '(#:asd-systems '("cpu")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "modeline/cpu"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/cpu")
    (synopsis "Modeline support for CPU info")
    (description "Modeline support for CPU info.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-disk
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-disk")
    (arguments
     '(#:asd-systems '("disk")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "modeline/disk") #t)))))
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("cl-diskspace" ,sbcl-cl-diskspace)
       ("cl-mount-info" ,sbcl-cl-mount-info)))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "StumpWM modeline support to show disk usage")
    (description "StumpWM modeline support to show disk usage")
    (license (list license:gpl2+ license:gpl3+))))

(define-public sbcl-stumpwm-mem
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-mem")
    (arguments
     '(#:asd-systems '("mem")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "modeline/mem"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/mem")
    (synopsis "Modeline support for memory info")
    (description "Modeline support for memory info.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-winner-mode
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-winner-mode")
    (arguments
     '(#:asd-systems '("winner-mode")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/winner-mode"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/winner-mode")
    (synopsis "Emacs' winner-mode for StumpWM")
    (description "This module provides a winner-mode for StumpWM similar to the
one in Emacs.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-screenshot
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-screenshot")
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("zpng" ,sbcl-zpng)))
    (arguments
     '(#:asd-systems '("screenshot")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/screenshot")))
         (add-after 'chdir 'fix-build
           (lambda _
             (substitute* "screenshot.asd"
               (("#:zpng")
                "#:stumpwm #:zpng")))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/screenshot")
    (synopsis "Screenshots for StumpWM")
    (description "This StumpWM module can take screenshots and store them as
PNG files.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-hostname
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-hostname")
    (arguments
     '(#:asd-systems '("hostname")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "modeline/hostname"))))))
    (home-page
     "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/hostname")
    (synopsis "Put hostname in the StumpWM modeline")
    (description "This StumpWM module puts the hostname in the StumpWM
modeline.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-notify
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-notify")
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-bordeaux-threads
           sbcl-dbus
           sbcl-xml-emitter
           (list stumpwm "lib")))
    (arguments
     '(#:asd-systems '("notify")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "util/notify"))))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib")
    (synopsis "Notifications server for StumpWM")
    (description "This module implements org.freedesktop.Notifications
interface[fn:dbus-spec].  It shows notifications using stumpwm:message
by default.")
    (license license:gpl3+)))

(define-public sbcl-stumpwm-battery-portable
  (package
    (inherit stumpwm-contrib)
    (name "sbcl-stumpwm-battery-portable")
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-cl-ppcre
           (list stumpwm "lib")))
    (arguments
     '(#:asd-systems '("battery-portable")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "modeline/battery-portable"))))))
    (synopsis "Battery level indicator for StumpWM")
    (description "This module provides a battery level indicator for the
modeline.  It can be displayed in the modeline with %B.")
    (license (list license:expat license:gpl3+))))

(define-public lemonbar
  (package
    (name "lemonbar")
    (version "1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LemonBoy/bar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sm1lxxf0y2n87nvc8mz6i6mzb32f4qab80ppb28ibrwfir6jsln"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:make-flags
      #~(list #$(string-append "CC=" (cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (inputs
     (list libxcb))
    (native-inputs
     (list perl))
    (home-page "https://github.com/LemonBoy/bar")
    (synopsis "Featherweight status bar")
    (description
     "@code{lemonbar} (formerly known as @code{bar}) is a lightweight
bar entirely based on XCB.  Provides full UTF-8 support, basic
formatting, RandR and Xinerama support and EWMH compliance without
wasting your precious memory.")
    (license license:x11)))

(define-public lemonbar-xft
  ;; Upstream v2.0 tag is several years behind HEAD
  (let ((commit "481e12363e2a0fe0ddd2176a8e003392be90ed02"))
    (package
      (inherit lemonbar)
      (name "lemonbar-xft")
      (version (string-append "2.0." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/drscream/lemonbar-xft")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0588g37h18lv50h7w8vfbwhvc3iajh7sdr53848spaif99nh3mh4"))))
      (inputs
       (modify-inputs (package-inputs lemonbar)
         (prepend freetype libxft libx11)))
      (arguments
       (substitute-keyword-arguments (package-arguments lemonbar)
         ((#:make-flags make-flags)
          #~(#$@make-flags
             (format #f "CFLAGS=~a -DVERSION='~s'"
                     (string-append
                      "-I" #$(this-package-input "freetype")
                      "/include/freetype2")
                     #$version)))))
      (home-page "https://github.com/drscream/lemonbar-xft")
      (synopsis
       (string-append
        (package-synopsis lemonbar)
        " with fontconfig support"))
      (description
       (string-append
        (package-description lemonbar)
        "This is a fork of the @code{lemonbar} package that adds fontconfig
support, for easier unicode usage.")))))

(define-public xclickroot
  (package
    (name "xclickroot")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phillbush/xclickroot")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wnsfxvh4v02r2jjyh2n6jfkbj2dna2hlm6anl4b36lwji749k2k"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11))
    (arguments
     `(#:tests? #f ;no test suite
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/phillbush/xclickroot")
    (synopsis "Run a command when a mouse button is pressed on the root window")
    (description "@code{xclickroot} runs a command every time a given mouse
button is pressed on the root window.")
    (license license:public-domain)))

(define-public xinitrc-xsession
  (let ((commit "cbfc77a1ccaf07b7d8a35f4d8007c7102f365374")
        (revision "0"))
    (package
      (name "xinitrc-xsession")
      (version (git-version "1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://aur.archlinux.org/xinit-xsession.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12nv3qyjhy2l9mcb547f414d8bj79mhdhsra0g8x7x71b1xxl15b"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'patch-xsession-file
              (lambda _
                (let* ((xinitrc-desktop
                        (string-append #$output "/share/xsessions/xinitrc.desktop"))
                       (xinitrc-helper
                        (string-append #$output "/bin/xinitrcsession-helper")))
                  (substitute* xinitrc-desktop
                    (("Exec=xinitrcsession-helper")
                     (string-append "Exec=" xinitrc-helper)))))))
        #:install-plan
        #~(list '("xinitrcsession-helper" "bin/")
                '("xinitrc.desktop" "share/xsessions/"))))
      (home-page "https://aur.archlinux.org/packages/xinit-xsession/")
      (synopsis "Use ~/.xinitrc as an xsession from your display manager")
      (description
       "Xinitrc-xsession allows @code{~/.xinitrc} to be run as a session from
your display manager.  Make @code{~/.xinitrc} executable and use this package
in your system configuration have this xsession available to your display
manager.")
      (license license:gpl3))))

(define-public xmenu
  (package
    (name "xmenu")
    (version "4.5.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phillbush/xmenu")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qgxkrv9jnnnf3px7zh0paf8xsr4bcpf0f2nq9zy012m214223hs"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxinerama libxft freetype imlib2))
    (arguments
     `(#:tests? #f ;no test suite
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output)
             (string-append "CFLAGS="
                            "-I" (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/phillbush/xmenu")
    (synopsis "Menu utility for X")
    (description "@code{xmenu} receives a menu specification in stdin, shows
a menu for the user to select one of the options, and outputs the option
selected to stdout.  It can be controlled both via mouse and via keyboard.")
    (license license:public-domain)))

(define-public idesk
  (package
    (name "idesk")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/idesk/idesk/idesk-" version
             "/idesk-" version ".tar.bz2"))
       (sha256
        (base32
         "1lxk2yvgysxwl514zc82lwr1dwc8cd62slgr5lzdhjbdrxfymdyl"))
       (modules '((guix build utils)
                  (ice-9 format)))
       (snippet
        '(let* ((file     "src/DesktopConfig.cpp")
                (template (string-append file ".XXXXXX"))
                (out      (mkstemp! template))
                (st       (stat file))
                (mode     (stat:mode st)))
           (call-with-ascii-input-file file
             (lambda (p)
               (format out "~{~a~%~}" '("#include <unistd.h>"
                                        "#include <sys/stat.h>"
                                        "#include <sys/types.h>"))
               (dump-port p out)
               (close out)
               (chmod template mode)
               (rename-file template file)
               #t))))))
    (build-system gnu-build-system)
    (inputs
     (list libx11
           libxft
           libxpm
           libpng
           freetype
           imlib2-1.7
           sed))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:tests? #f)) ;no test suite
    (home-page "https://sourceforge.net/projects/idesk/")
    (synopsis "Add icons on X desktop and set background image for wallpaper")
    (description "Idesk is program that draws desktop icons.  Each icon will
execute a shell command on a configurable action.  The icons can be moved on
the desktop by dragging them, and the icons will remember their positions on
start-up.")
    (license license:bsd-3)))

(define-public xnotify
  (package
    (name "xnotify")
    (version "0.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phillbush/xnotify")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jxms4md2mwfjgm2pgg3vakpp33800jbn9hnl0j4jyfc9f1ckbsv"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxft libxinerama imlib2))
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output)
             (string-append "CFLAGS="
                            "-I" (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))
       #:tests? #f ;no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/phillbush/xnotify")
    (synopsis "Displays a notification on the screen")
    (description "XNotify receives a notification specification in stdin and
shows a notification for the user on the screen.")
    (license license:expat)))

(define-public cagebreak
  (package
    (name "cagebreak")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/project-repo/cagebreak")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09ky8wili3ym8qi4dasckdcdcvn4g6ak08dg0yccnwmnlwxiyps6"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dxwayland=true")
      ;; XXX: Running cagebreak tests need more tools, such as: clang-format,
      ;; shellcheck, git, gnupg ...
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-data-dir
            (lambda _
              (substitute* '("cagebreak.c" "meson.build")
                (("/etc/") (string-append #$output "/etc/"))
                (("/usr/share/") (string-append #$output "/usr/share/"))))))))
    (native-inputs (list pandoc pkg-config))
    (inputs (list libevdev pango wlroots))
    (home-page "https://github.com/project-repo/cagebreak")
    (synopsis "Tiling wayland compositor inspired by ratpoison")
    (description
     "@command{cagebreak} is a slim, keyboard-controlled, tiling compositor
for wayland conceptually based on the X11 window manager
@command{ratpoison}.")
    (license license:expat)))

(define-public libucl
  (package
    (name "libucl")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vstakhov/libucl/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j8npymjhcnzbwhx1wggr88148cga921438flf1sn7mw1b9dr55f"))))
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ;; no tests
    (home-page "https://github.com/vstakhov/libucl")
    (synopsis "Universal configuration language (UCL) parser")
     (description "libucl implements a configuration language that is easy to
read and write, and compatible with JSON.")
    (license license:bsd-2)))

(define-public labwc
  (package
    (name "labwc")
    (version "0.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/labwc/labwc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zbgj8r8ppvqnz2imh6f825f2lvsqpiqfa0r5g5r4nsvadiipivp"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config gettext-minimal scdoc))
    (inputs
     (list cairo
           glib
           libxcb
           libxml2
           pango
           wlroots))
    (home-page "https://labwc.github.io")
    (synopsis "Window-stacking compositor for Wayland")
    (description
     "Labwc is lightweight and independent with a focus on simply stacking
windows well and rendering some window decorations, it is inspired by Openbox.
It takes a no-bling/frills approach and says no to features such as icons
(except window buttons), animations, decorative gradients and any other
options not required to reasonably render common themes.  It relies on clients
for panels, screenshots, wallpapers and so on to create a full desktop
environment.

Labwc tries to stay in keeping with wlroots and sway in terms of general
approach and coding style.

Labwc has no reliance on any particular Desktop Environment, Desktop Shell or
session.  Nor does it depend on any UI toolkits such as Qt or GTK.")
    (license license:gpl2)))

(define-public hikari
  (package
    (name "hikari")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hikari.acmelabs.space/releases/"
                           "hikari-" version ".tar.gz"))
       (sha256
        (base32 "1qjd9dhpmv75idf6jjzwff8wgliad4d0af8pih9526p14vrnyws0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list bmake pkg-config wayland-protocols))
    (inputs
     (list cairo
           libinput-minimal
           libucl
           libxkbcommon
           linux-pam
           pango
           wayland
           wlroots))
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "PREFIX=" (assoc-ref %outputs "out"))
        (string-append "CC=" ,(cc-for-target))
        "WITH_XWAYLAND=YES"
        "WITH_SCREENCOPY=YES"
        "WITH_LAYERSHELL=YES"
        "WITH_VIRTUAL_INPUT=YES")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (apply invoke "bmake" make-flags)))
         (replace 'install
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (apply invoke "bmake" "install" make-flags))))))
    (home-page "https://hikari.acmelabs.space/")
    (synopsis "Stacking Wayland compositor with tiling capabilities")
    (description
     "Hikari is a stacking Wayland compositor with additional tiling
capabilities.  It is heavily inspired by the Calm Window manager(cwm).")
    (license license:bsd-2)))

(define-public jwm
  (package
    (name "jwm")
    (version "2.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/joewing/jwm/releases/download/"
                    "v" version "/jwm-" version ".tar.xz"))
              (sha256
               (base32
                "1av7r9sp26r5l74zvwdmyyyzav29mw5bafihp7y33vsjqkh4wfzf"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f   ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-xsession
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (xsessions (string-append out "/share/xsessions")))
                (mkdir-p xsessions)
                (call-with-output-file
                    (string-append xsessions "/jwm.desktop")
                  (lambda (port)
                    (format port "~
                     [Desktop Entry]~@
                     Name=jwm~@
                     Comment=Joe's Window Manager~@
                     Exec=~a/bin/jwm~@
                     Type=XSession~%" out)))))))))
    (native-inputs (list pkg-config))
    (inputs
     (list cairo
           libjpeg-turbo
           libpng
           librsvg
           libxext
           libxinerama
           libxmu
           libxpm
           libxrender
           libxt
           pango))
    (home-page "http://joewing.net/projects/jwm")
    (synopsis "Joe's Window Manager")
    (description
     "JWM is a light-weight window manager for the X11 Window System.  it is
written in C and uses only Xlib at a minimum.  Because of its small footprint,
it makes a good window manager for older computers and less powerful systems,
such as the Raspberry Pi, though it is perfectly capable of running on modern
systems.")
    (license license:expat)))

(define-public mjwm
  (package
    (name "mjwm")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chiku/mjwm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lgfp2xidhvmbj4zqvzz9g8zwbn6mz0pgacc57b43ha523vamsjq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f   ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-subcategory.h
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "include/subcategory.h"
                ;; icon name should be application-other instead of
                ;; application-others.
                (("applications-others") "applications-other")))))))
    (home-page "https://github.com/chiku/mjwm")
    (synopsis "Create menu for JWM")
    (description
     "MJWM can create JWM's menu from (freedesktop) desktop files and the
generated file can be include in the rootmenu section of your jwm config
file.")
    (license license:gpl2+)))

(define-public devour
  (package
    (name "devour")
    (version "12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/salman-abedin/devour")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qq5l6d0fn8azg7sj7a4m2jsmhlpswl5793clcxs1p34vy4wb2lp"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11))
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))           ;no configure script
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "BIN_DIR=" %output "/bin"))))
    (home-page "https://github.com/salman-abedin/devour")
    (synopsis "X11 window swallower")
    (description
     "@command{devour} hides your current window before launching an external
program and unhides it after quitting.")
    (license license:gpl2)))

(define-public trayer-srg
  (package
    (name "trayer-srg")
    (version "1.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sargon/trayer-srg")
             (commit (string-append "trayer-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1mvhwaqa9bng9wh3jg3b7y8gl7nprbydmhg963xg0r076jyzv0cg"))))
    (native-inputs
     (list libxmu pkg-config))
    (inputs
     (list libx11 gdk-pixbuf gtk+-2))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "CC=" ,(cc-for-target))
               (string-append "PREFIX=" %output)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (apply invoke "./configure" configure-flags))))))
    (home-page "https://github.com/sargon/trayer-srg")
    (synopsis "Minimal GTK based system tray")
    (description
     "@command{trayer} is small program designed to provide systray
functionality present in GNOME/KDE desktop environments for window managers
which do not support it.")
    (license license:expat)))

(define-public wlogout
  (package
    (name "wlogout")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ArtsyMacaw/wlogout")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1swhzkqkzli59c89pvrakfvicd00x7ga860c3x2pbb4y3xziqfvi"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config scdoc))
    (inputs
     (list gtk-layer-shell gtk+))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack  'patch-source-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "main.c"
                 (("/usr/share") (string-append out "/share"))
                 (("/etc") (string-append out "/etc"))))
             #t)))))
    (home-page "https://github.com/ArtsyMacaw/wlogout")
    (synopsis "Logout menu for Wayland")
    (description "wlogout is a logout menu for Wayland environments.")
    (license license:expat)))

(define-public berry
  (package
    (name "berry")
    (version "0.1.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/jlervin/berry")
          (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ygqzgi7ncc6whvwdifds2cq9cllq9fhiqnigx859hbdnf453hn4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:make-flags
       ,#~(list (string-append "CC=" #$(cc-for-target))
                (string-append "prefix=" #$output)
                (string-append "CFLAGS="
                               "-I" (assoc-ref %build-inputs "freetype")
                               "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (xsessions (string-append output "/share/xsessions")))
               (mkdir-p xsessions)
               (with-output-to-file (string-append xsessions "/berry.desktop")
                 (lambda _
                   (format #t
                    "\
[Desktop Entry]~@
Name=berry~@
Comment=Berry Window Manager~@
Exec=~a/bin/berry~@
TryExec=~@*~a/bin/berry~@
Icon=~@
Type=Application~%"
                    output)))))))))
    (native-inputs
     (list pkg-config))
    (inputs
      (list freetype
            fontconfig
            libxext
            libx11
            libxft
            libxinerama))
    (home-page "https://berrywm.org/")
    (synopsis "Healthy, byte-sized window manager")
    (description
     "@code{berry} is a healthy, bite-sized window manager written in C using XLib.")
    (license license:expat)))

(define-public avizo
  (package
    (name "avizo")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/misterdanb/avizo")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ddv5ssxfjbzhqskbbhi9qj1yqkraiv3r8svfmp9s5nnfpid8aba"))))
    (build-system meson-build-system)
    (inputs (list gtk+))
    (native-inputs
     (list vala
           `(,glib "bin")
           gobject-introspection
           gtk-layer-shell
           pkg-config))
    (home-page "https://github.com/misterdanb/avizo")
    (synopsis "Notification daemon for Sway")
    (description
     "Avizo is a simple notification daemon for Sway, mainly intended to be
used for multimedia keys.")
    (license license:gpl3+)))

(define-public grimshot
  (package
    (inherit sway)
    (name "grimshot")
    (source (origin
              (inherit (package-source sway))
              (snippet #~(delete-file "contrib/grimshot.1"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~`(("grimshot" "bin/")
                              ("grimshot.1" "share/man/man1/"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "contrib")))
                        (add-after 'chdir 'patch-script-dependencies
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "grimshot"
                              (("\\b(date|grim|jq|notify-send|slurp|swaymsg|wl-copy)\\b"
                                _ binary)
                               (search-input-file
                                inputs (string-append "bin/" binary))))))
                        (add-after 'patch-script-dependencies 'build-man-page
                          (lambda _
                            (with-input-from-file "grimshot.1.scd"
                              (lambda _
                                (with-output-to-file "grimshot.1"
                                  (lambda _
                                    (invoke "scdoc"))))))))))
    (native-inputs (list scdoc))
    (inputs (list coreutils
                  grim
                  jq
                  libnotify
                  slurp
                  sway
                  wl-clipboard))
    (synopsis "Screenshot utility for the Sway window manager")
    (description "Grimshot is a screenshot utility for @code{sway}.  It provides
an interface over @code{grim}, @code{slurp} and @code{jq}, and supports storing
the screenshot either directly to the clipboard using @code{wl-copy} or to a
file.")))

(define-public wld
  (let ((commit "6586736176ef50a88025abae835e29a7ca980126")
        (revision "1"))
    (package
      (name "wld")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/michaelforney/wld")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qkd3q8p1s72x688g83fkcarrz2h20904rpd8z44ql6ksgrj9bp3"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                              ; no tests
         #:make-flags (list (string-append "CC=" ,(cc-for-target))
                            (string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (inputs (list fontconfig
                    libdrm
                    pixman
                    wayland))
      (propagated-inputs (list fontconfig pixman))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/michaelforney/wld")
      (synopsis "Primitive drawing library for Wayland")
      (description "wld is a drawing library that targets Wayland.")
      (license license:expat))))

(define-public swc
  (let ((commit "a7b615567f83d9e48d585251015048c441ca0239")
        (revision "1"))
    (package
      (name "swc")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/michaelforney/swc")
                      (commit commit)))
                (sha256
                 (base32
                  "19rpbwpi81pm92fkhsmbx7pzagpah5m9ih5h5k3m8dy6r8ihdh35"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;no tests
         #:make-flags (list (string-append "CC="
                                           ,(cc-for-target))
                            (string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (inputs (list libdrm
                    libinput
                    libxcb
                    libxkbcommon
                    wayland
                    wayland-protocols
                    wld
                    xcb-util-wm))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/michaelforney/swc")
      (synopsis "Library for making a simple Wayland compositor")
      (description
       "swc is a small Wayland compositor implemented as a library.

It has been designed primarily with tiling window managers in mind.  Additionally,
notable features include:
@itemize
@item Easy to follow code base
@item XWayland support
@item Can place borders around windows
@end itemize")
      (license license:expat))))

(define-public velox
  (let ((commit "fcc041265539befd907a64ee3a536cb2489ffb99")
        (revision "1"))
    (package
      (name "velox")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/michaelforney/velox")
                      (commit commit)))
                (sha256
                 (base32
                  "0d11bmag5zwmas3rf1b7x5hjla7wpxq70nr86dz3x9r7cal04mym"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;no tests
         #:make-flags (list (string-append "CC="
                                           ,(cc-for-target))
                            (string-append "PREFIX=" %output))
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (inputs (list libinput libxkbcommon wayland wld))
      (propagated-inputs (list swc))
      (native-inputs (list pkg-config))
      (home-page "https://github.com/michaelforney/velox")
      (synopsis "Simple window manager based on swc")
      (description "velox is a simple window manager for Wayland based on swc.
It is inspired by dwm and xmonad.")
      (license license:expat))))
