;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix build linux-module-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:export (%standard-phases
            linux-module-build))

;; Commentary:
;;
;; Builder-side code of linux-module build.
;;
;; Code:

;; Similar to make-linux-libre's "configure" phase.
(define* (configure #:key inputs target arch #:allow-other-keys)
  (setenv "KCONFIG_NOTIMESTAMP" "1")
  (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))

  (setenv "ARCH" arch)
  (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))

  (when target
    ;; TODO? (setenv "EXTRA_VERSION" ,extra-version)
    ;; TODO? kernel ".config".
    (setenv "CROSS_COMPILE" (string-append target "-"))
    (format #t "`CROSS_COMPILE' set to `~a'~%"
            (getenv "CROSS_COMPILE"))))

(define* (build #:key (make-flags '()) (parallel-build? #t)
                (source-directory ".")
                inputs
                #:allow-other-keys)
  (apply invoke "make" "-C"
         (string-append (assoc-ref inputs "linux-module-builder")
                        "/lib/modules/build")
         (string-append "M=" (canonicalize-path source-directory))
         `(,@(if parallel-build?
                 `("-j" ,(number->string (parallel-job-count)))
                 '())
           ,@make-flags)))

;; Similar to the "modules_install" part of make-linux-libre.
(define* (install #:key (make-flags '()) (parallel-build? #t)
                  (source-directory ".")
                  inputs native-inputs outputs
                  #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (moddir (string-append out "/lib/modules")))
    ;; Install kernel modules
    (mkdir-p moddir)
    (apply invoke "make" "-C"
            (string-append (assoc-ref inputs "linux-module-builder")
                           "/lib/modules/build")
            (string-append "M=" (canonicalize-path source-directory))
            ;; Disable depmod because the Guix system's module directory
            ;; is an union of potentially multiple packages.  It is not
            ;; possible to use depmod to usefully calculate a dependency
            ;; graph while building only one of those packages.
            "DEPMOD=true"
            (string-append "MODULE_DIR=" moddir)
            (string-append "INSTALL_PATH=" out)
            (string-append "INSTALL_MOD_PATH=" out)
            "INSTALL_MOD_STRIP=1"
            "modules_install"
         `(,@(if parallel-build?
                 `("-j" ,(number->string (parallel-job-count)))
                 '())
           ,@make-flags))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'configure configure)
    (replace 'build build)
    (replace 'install install)))

(define* (linux-module-build #:key inputs
                             (phases %standard-phases)
                             #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order, with a Linux
kernel in attendance."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))

;;; linux-module-build-system.scm ends here
