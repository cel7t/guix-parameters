;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages mc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public mc
  (package
    (name "mc")
    (version "4.8.29")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://ftp.osuosl.org/pub/midnightcommander/mc-"
                          version ".tar.xz"))
      (sha256
       (base32 "1m0aai4paxpcf3lw1dd94zyxc7wd0ysmfwpibg50q62q9yws7n01"))))
    (build-system gnu-build-system)
    (native-inputs (list perl pkg-config))
    (inputs (list aspell
                  check
                  glib
                  gpm
                  libssh2
                  ncurses
                  unzip))
    (arguments
     `(#:configure-flags
       '("--with-screen=ncurses" "--enable-aspell")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-FHS-file-names
           (lambda _
             ;; Patch files to refer to executables in the store or $PATH.
             (substitute* "misc/mcedit.menu.in"
               (("#! /bin/sh") (string-append "#!" (which "sh")))
               (("/bin/bash") (which "bash")))
             (substitute* "misc/ext.d/misc.sh.in"
               (("/bin/cat") "cat"))
             (substitute* (list "lib/utilunix.c"
                                "src/usermenu.c"
                                "src/vfs/fish/fish.c"
                                "tests/src/vfs/extfs/helpers-list/Makefile.in")
               (("/bin/sh") (which "sh")))
             (substitute* "src/filemanager/ext.c"
               (("/bin/rm") "rm")
               (("/bin/sh") (which "sh")))

             ;; There are other /bin/<shell>s hard-coded in this file, but they
             ;; are never tried after bash (mc's first choice) is found.
             (substitute* "lib/shell.c"
               (("/bin/bash") (which "bash")))
             #t))
         (add-before 'check 'fix-tests
           (lambda _
             ;; Don't expect a UID or GID of ‘0’ in the build environment.
             (with-directory-excursion "tests/src/vfs/extfs/helpers-list/data"
               (substitute* (list "rpm.custom.output"
                                  "rpm.glib.output")
                 (("      0        0") "<<uid>>  <<gid>>")))
             ;; XXX ERROR:mc_realpath.c:99:realpath_test: assertion failed
             ;; (resolved_path == data->expected_string): ("" == "/usr/bin")
             (substitute* "tests/lib/mc_realpath.c"
               (("/usr/bin") "/")
               (("usr/bin") "/"))
             #t)))))
    (home-page "https://www.midnight-commander.org")
    (properties
      `((release-monitoring-url . "https://ftp.osuosl.org/pub/midnightcommander/")))
    (synopsis "Graphical file manager")
    (description
     "GNU Midnight Commander is a command-line file manager laid out in a
common two-pane format.  In addition to standard file management tasks such as
copying and moving, Midnight Commander also supports viewing the contents of
RPM package files and other archives and managing files on other computers via
FTP or FISH.  It also includes a powerful text editor for opening text
files.")
    (license gpl3+)))
