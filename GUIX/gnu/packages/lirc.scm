;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages lirc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public lirc
  (package
    (name "lirc")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lirc/LIRC/" version
                                  "/lirc-" version ".tar.bz2"))
              (sha256
               (base32
                "0ai27l6hxfgkwvkqa3fy1b1gqzw2y10md030y5ig4748fj1fqi1x"))
              (patches (search-patches "lirc-localstatedir.patch"
                                       "lirc-reproducible-build.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--localstatedir=/var"
         ;; "configure" script fails to enable "devinput" driver as it
         ;; checks for "/dev/input" directory (which is not available),
         ;; so enable it explicitly.
         "--enable-devinput")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-kernel-sniffing
           (lambda _
             ;; Correct the faulty assumption that systemd support should be
             ;; hard-wired when a build host's /proc/version contains "Ubuntu".
             (substitute* "configure"
               (("kernelversion=.*") "kernelversion=irrelevant\n"))))
         (add-after 'unpack 'patch-lirc-make-devinput
           (lambda* (#:key inputs #:allow-other-keys)
             ;; 'lirc-make-devinput' script assumes that linux headers
             ;; are placed in "/usr/...".
             (let ((headers (assoc-ref inputs "kernel-headers")))
               (substitute* "tools/lirc-make-devinput"
                 (("/usr/include") (string-append headers "/include"))))))
         (add-after 'unpack 'patch-doc/Makefile.in
           (lambda _
             ;; Lirc wants to install several images and a useless html page
             ;; to "$(localstatedir)/lib/lirc/".  This makes 'install' phase
             ;; fail as localstatedir is "/var", so do not install these
             ;; files there (the same images are installed in
             ;; "share/doc/lirc/images/" anyway).
             (substitute* "doc/Makefile.in"
               (("^vardocs_DATA =.*") "vardocs_DATA =\n")
               (("^varimage_DATA =.*") "varimage_DATA =\n"))))
         (add-after 'unpack 'omit-pip-sourceball
           ;; ‘make install’ invokes ’setup.py sdist’, which has no known (to
           ;; nckx) way to enforce mtimes.  The utility of this is questionable,
           ;; IMO: let's disable it entirely & listen for complaints, if any.
           (lambda _
             (substitute* "Makefile.in"
               (("(PYTHON_TARBALL.*=).*" _ tarball=)
                (string-append tarball= "\n")))))
         (add-before 'configure 'build-reproducibly
           (lambda _
             (setenv "LIRC_IRDB_CACHE_ID" "build time"))))))
    (native-inputs
     (list pkg-config libxslt))
    (inputs
     `(("libx11" ,libx11)
       ("libusb-compat" ,libusb-compat)
       ("alsa-lib" ,alsa-lib)
       ("python" ,python)))
    (home-page "https://www.lirc.org/")
    (synopsis "Linux Infrared Remote Control")
    (description
     "LIRC allows computers to send and receive IR signals of many commonly
used remote controls.  The most important part of LIRC is the @code{lircd}
daemon that decodes IR signals received by the device drivers.  The second
daemon program @code{lircmd} translates IR signals to mouse movements.
The user space applications allow you to control your computer with a remote
control: you can send X events to applications, start programs and much more
on just one button press.")
    (license license:gpl2+)))

(define-public python-lirc
  (let ((commit "c28708bbeb6e02d85f13dd7e0b24e8e86abc215b")
        (revision "2"))
    (package
      (name "python-lirc")
      (version (git-version "1.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tompreston/python-lirc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13s9zqyfh871ls1aha47rhmk13b4mcyfckcn2sw70bvc26832gk6"))))
      (build-system python-build-system)
      (inputs
       (list lirc))
      (native-inputs
       (list python-cython))
      (arguments
       `(#:tests? #f         ; the only tests that exist are interactive
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'build-from-cython-files
             (lambda _ (invoke "make" "py3"))))))
      (home-page "https://github.com/tompreston/python-lirc")
      (synopsis "Python bindings for LIRC")
      (description "@code{lirc} is a Python module which provides LIRC bindings.")
      (license license:gpl3))))
