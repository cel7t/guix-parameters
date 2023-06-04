;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2022 Fatima Toothpaste <fatimatoothpaste@disroot.org>
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

(define-module (gnu packages nicotine)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages xorg))

(define-public nicotine+
  (package
    (name "nicotine+")
    (version "3.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Nicotine-Plus/nicotine-plus")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "1x08z5lvkdl62dkc11vrsackgzsh1vr9vp3vgsgfzjyrvlsybmfw"))
              (modules '((guix build utils)))
              ;; Remove test that relies on network access.
              (snippet '(delete-file-recursively "test/integration"))))
    (build-system python-build-system)
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules
       ((guix build utils)
        (guix build python-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append
                          (assoc-ref outputs "out")
                          "/bin/nicotine"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))))
         (add-after 'wrap-program 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'glib-or-gtk-wrap 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "xvfb-run" "python" "-m" "unittest")))))))
    (inputs
     (list bash-minimal gspell gtk+ python-pygobject libappindicator python-pytaglib))
    (native-inputs
     (list gettext-minimal xvfb-run))
    (home-page "https://nicotine-plus.org/")
    (synopsis "Graphical client for Soulseek")
    (description
     "Nicotine+ is a graphical client for the Soulseek peer-to-peer
file sharing network.  It is an attempt to keep Nicotine working with
the latest libraries, kill bugs, keep current with the Soulseek protocol,
and add some new features that users want and/or need.")
    (license license:gpl3+)))
