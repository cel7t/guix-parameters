;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix build python-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            add-installed-pythonpath
            site-packages
            python-version
            python-build))

;; Commentary:
;;
;; Builder-side code of the standard Python package build procedure.
;;
;;
;; Backgound about the Python installation methods
;;
;; In Python there are different ways to install packages: distutils,
;; setuptools, easy_install and pip.  All of these are sharing the file
;; setup.py, introduced with distutils in Python 2.0. The setup.py file can be
;; considered as a kind of Makefile accepting targets (or commands) like
;; "build" and "install".  As of autumn 2016 the recommended way to install
;; Python packages is using pip.
;;
;; For both distutils and setuptools, running "python setup.py install" is the
;; way to install Python packages.  With distutils the "install" command
;; basically copies all packages into <prefix>/lib/pythonX.Y/site-packages.
;;
;; Some time later "setuptools" was established to enhance distutils.  To use
;; setuptools, the developer imports setuptools in setup.py.  When importing
;; setuptools, the original "install" command gets overwritten by setuptools'
;; "install" command.
;;
;; The command-line tools easy_install and pip are both capable of finding and
;; downloading the package source from PyPI (the Python Package Index).  Both
;; of them import setuptools and execute the "setup.py" file under their
;; control.  Thus the "setup.py" behaves as if the developer had imported
;; setuptools within setup.py - even if it is still using only distutils.
;;
;; Setuptools' "install" command (to be more precise: the "easy_install"
;; command which is called by "install") will put the path of the currently
;; installed version of each package and it's dependencies (as declared in
;; setup.py) into an "easy-install.pth" file.  In Guix each packages gets its
;; own "site-packages" directory and thus an "easy-install.pth" of its own.
;; To avoid conflicts, the python build system renames the file to
;; <packagename>.pth in the phase rename-pth-file.  To ensure that Python will
;; process the .pth file, easy_install also creates a basic "site.py" in each
;; "site-packages" directory.  The file is the same for all packages, thus
;; there is no need to rename it.  For more information about .pth files and
;; the site module, please refere to
;; https://docs.python.org/3/library/site.html.
;;
;; The .pth files contain the file-system paths (pointing to the store) of all
;; dependencies.  So the dependency is hidden in the .pth file but is not
;; visible in the file-system.  Now if packages A and B both required packages
;; P, but in different versions, Guix will not detect this when installing
;; both A and B to a profile. (For details and example see
;; https://lists.gnu.org/archive/html/guix-devel/2016-10/msg01233.html.)
;;
;; Pip behaves a bit different then easy_install: it always executes
;; "setup.py" with the option "--single-version-externally-managed" set.  This
;; makes setuptools' "install" command run the original "install" command
;; instead of the "easy_install" command, so no .pth file (and no site.py)
;; will be created.  The "site-packages" directory only contains the package
;; and the related .egg-info directory.
;;
;; This is exactly what we need for Guix and this is what we mimic in the
;; install phase below.
;;
;; As a draw back, the magic of the .pth file of linking to the other required
;; packages is gone and these packages have now to be declared as
;; "propagated-inputs".
;;
;; Note: Importing setuptools also adds two sub-commands: "install_egg_info"
;; and "install_scripts".  These sub-commands are executed even if
;; "--single-version-externally-managed" is set, thus the .egg-info directory
;; and the scripts defined in entry-points will always be created.


(define setuptools-shim
  ;; Run setup.py with "setuptools" being imported, which will patch
  ;; "distutils". This is needed for packages using "distutils" instead of
  ;; "setuptools" since the former does not understand the
  ;; "--single-version-externally-managed" flag.
  ;; Python code taken from pip 9.0.1 pip/utils/setuptools_build.py
  (string-append
   "import setuptools, tokenize;__file__='setup.py';"
   "f=getattr(tokenize, 'open', open)(__file__);"
   "code=f.read().replace('\\r\\n', '\\n');"
   "f.close();"
   "exec(compile(code, __file__, 'exec'))"))

(define (call-setuppy command params use-setuptools?)
  (if (file-exists? "setup.py")
      (begin
         (format #t "running \"python setup.py\" with command ~s and parameters ~s~%"
                command params)
         (if use-setuptools?
             (apply invoke "python" "-c" setuptools-shim
                    command params)
             (apply invoke "python" "./setup.py" command params)))
      (error "no setup.py found")))

(define* (sanity-check #:key tests? inputs outputs #:allow-other-keys)
  "Ensure packages depending on this package via setuptools work properly,
their advertised endpoints work and their top level modules are importable
without errors."
  (let ((sanity-check.py (assoc-ref inputs "sanity-check.py")))
    ;; Make sure the working directory is empty (i.e. no Python modules in it)
    (with-directory-excursion "/tmp"
      (invoke "python" sanity-check.py (site-packages inputs outputs)))))

(define* (build #:key use-setuptools? #:allow-other-keys)
  "Build a given Python package."
  (call-setuppy "build" '() use-setuptools?)
  #t)

(define* (check #:key tests? test-target use-setuptools? #:allow-other-keys)
  "Run the test suite of a given Python package."
  (if tests?
      ;; Running `setup.py test` creates an additional .egg-info directory in
      ;; build/lib in some cases, e.g. if the source is in a sub-directory
      ;; (given with `package_dir`). This will by copied to the output, too,
      ;; so we need to remove.
      (let ((before (find-files "build" "\\.egg-info$" #:directories? #t)))
        (call-setuppy test-target '() use-setuptools?)
        (let* ((after (find-files "build" "\\.egg-info$" #:directories? #t))
               (inter (lset-difference string=? after before)))
          (for-each delete-file-recursively inter)))
      (format #t "test suite not run~%"))
  #t)

(define (python-version python)
  (let* ((version     (last (string-split python #\-)))
         (components  (string-split version #\.))
         (major+minor (take components 2)))
    (string-join major+minor ".")))

(define (python-output outputs)
  "Return the path of the python output, if there is one, or fall-back to out."
  (or (assoc-ref outputs "python")
      (assoc-ref outputs "out")))

(define (site-packages inputs outputs)
  "Return the path of the current output's Python site-package."
  (let ((out (python-output outputs))
        (python (assoc-ref inputs "python")))
    (string-append out "/lib/python" (python-version python) "/site-packages")))

(define (add-installed-pythonpath inputs outputs)
  "Prepend the site-package of OUTPUT to GUIX_PYTHONPATH.  This is useful when
running checks after installing the package."
  (setenv "GUIX_PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                           (getenv "GUIX_PYTHONPATH"))))

(define* (add-install-to-pythonpath #:key inputs outputs #:allow-other-keys)
  "A phase that just wraps the 'add-installed-pythonpath' procedure."
  (add-installed-pythonpath inputs outputs))

(define* (add-install-to-path #:key outputs #:allow-other-keys)
  "Adding Python scripts to PATH is also often useful in tests."
  (setenv "PATH" (string-append (assoc-ref outputs "out")
                                "/bin:"
                                (getenv "PATH"))))

(define* (install #:key inputs outputs (configure-flags '()) use-setuptools?
                  #:allow-other-keys)
  "Install a given Python package."
  (let* ((out (python-output outputs))
         (python (assoc-ref inputs "python"))
         (major-minor (map string->number
                           (take (string-split (python-version python) #\.) 2)))
         (<3.7? (match major-minor
                   ((major minor)
                    (or (< major 3) (and (= major 3) (< minor 7))))))
         (params (append (list (string-append "--prefix=" out)
                               "--no-compile")
                         (if use-setuptools?
                             ;; distutils does not accept these flags
                             (list "--single-version-externally-managed"
                                   "--root=/")
                             '())
                         configure-flags)))
    (call-setuppy "install" params use-setuptools?)
    ;; Rather than produce potentially non-reproducible .pyc files on Pythons
    ;; older than 3.7, whose 'compileall' module lacks the
    ;; '--invalidation-mode' option, do not generate any.
    (unless <3.7?
      (invoke "python" "-m" "compileall" "--invalidation-mode=unchecked-hash"
              out))))

(define* (wrap #:key inputs outputs #:allow-other-keys)
  (define (list-of-files dir)
    (find-files dir (lambda (file stat)
                      (and (eq? 'regular (stat:type stat))
                           (not (wrapped-program? file))))))

  (define bindirs
    (append-map (match-lambda
                  ((_ . dir)
                   (list (string-append dir "/bin")
                         (string-append dir "/sbin"))))
                outputs))

  ;; Do not require "bash" to be present in the package inputs
  ;; even when there is nothing to wrap.
  ;; Also, calculate (sh) only once to prevent some I/O.
  (define %sh (delay (search-input-file inputs "bin/bash")))
  (define (sh) (force %sh))

  (let* ((var `("GUIX_PYTHONPATH" prefix
                ,(search-path-as-string->list
                  (or (getenv "GUIX_PYTHONPATH") "")))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-program <> #:sh (sh) var)
                            files)))
              bindirs)))

(define* (rename-pth-file #:key name inputs outputs #:allow-other-keys)
  "Rename easy-install.pth to NAME.pth to avoid conflicts between packages
installed with setuptools."
  ;; Even if the "easy-install.pth" is not longer created, we kept this phase.
  ;; There still may be packages creating an "easy-install.pth" manually for
  ;; some good reason.
  (let* ((site-packages (site-packages inputs outputs))
         (easy-install-pth (string-append site-packages "/easy-install.pth"))
         (new-pth (string-append site-packages "/" name ".pth")))
    (when (file-exists? easy-install-pth)
      (rename-file easy-install-pth new-pth))))

(define* (ensure-no-mtimes-pre-1980 #:rest _)
  "Ensure that there are no mtimes before 1980-01-02 in the source tree."
  ;; Rationale: patch-and-repack creates tarballs with timestamps at the POSIX
  ;; epoch, 1970-01-01 UTC.  This causes problems with Python packages,
  ;; because Python eggs are ZIP files, and the ZIP format does not support
  ;; timestamps before 1980.
  (let ((early-1980 315619200))  ; 1980-01-02 UTC
    (ftw "." (lambda (file stat flag)
               (unless (<= early-1980 (stat:mtime stat))
                 (utime file early-1980 early-1980))
               #t))))

(define* (enable-bytecode-determinism #:rest _)
  "Improve determinism of pyc files."
  ;; Use deterministic hashes for strings, bytes, and datetime objects.
  (setenv "PYTHONHASHSEED" "0")
  ;; Prevent Python from creating .pyc files when loading modules (such as
  ;; when running a test suite).
  (setenv "PYTHONDONTWRITEBYTECODE" "1"))

(define* (ensure-no-cythonized-files #:rest _)
  "Check the source code for @code{.c} files which may have been pre-generated
by Cython."
  (for-each
    (lambda (file)
      (let ((generated-file
              (string-append (string-drop-right file 3) "c")))
        (when (file-exists? generated-file)
          (format #t "Possible Cythonized file found: ~a~%" generated-file))))
    (find-files "." "\\.pyx$")))

(define %standard-phases
  ;; The build phase only builds C extensions and copies the Python sources,
  ;; while the install phase copies then byte-compiles the sources to the
  ;; prefix directory.  The check phase is moved after the installation phase
  ;; to ease testing the built package.
  (modify-phases gnu:%standard-phases
    (add-after 'unpack 'ensure-no-mtimes-pre-1980 ensure-no-mtimes-pre-1980)
    (add-after 'ensure-no-mtimes-pre-1980 'enable-bytecode-determinism
      enable-bytecode-determinism)
    (add-after 'enable-bytecode-determinism 'ensure-no-cythonized-files
      ensure-no-cythonized-files)
    (delete 'bootstrap)
    (delete 'configure)                 ;not needed
    (replace 'build build)
    (delete 'check)                     ;moved after the install phase
    (replace 'install install)
    (add-after 'install 'add-install-to-pythonpath add-install-to-pythonpath)
    (add-after 'add-install-to-pythonpath 'add-install-to-path
      add-install-to-path)
    (add-after 'add-install-to-path 'wrap wrap)
    (add-after 'wrap 'check check)
    (add-after 'check 'sanity-check sanity-check)
    (add-before 'strip 'rename-pth-file rename-pth-file)))

(define* (python-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Python package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; python-build-system.scm ends here
