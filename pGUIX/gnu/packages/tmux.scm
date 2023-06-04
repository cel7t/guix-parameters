;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
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

(define-module (gnu packages tmux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages sphinx))

(define-public tmux
  (package
    (name "tmux")
    (version "3.3a")
    (source (origin
             (method url-fetch)
             (uri (string-append
                    "https://github.com/tmux/tmux/releases/download/"
                    version "/tmux-" version ".tar.gz"))
             (sha256
              (base32
               "0gzrrm6imhcp3sr5vw8g71x9n40bbdidwvcdyk2741xx8dw39zg4"))))
    (build-system gnu-build-system)
    (inputs
     (list libevent ncurses))
    (home-page "https://github.com/tmux/tmux/wiki")
    (synopsis "Terminal multiplexer")
    (description
     "tmux is a terminal multiplexer: it enables a number of terminals (or
windows), each running a separate program, to be created, accessed, and
controlled from a single screen.  tmux may be detached from a screen and
continue running in the background, then later reattached.")
    (license license:isc)))

(define-public tmux-themepack
  (package
    (name "tmux-themepack")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jimeh/tmux-themepack")
                    (commit version)))
              (sha256
               (base32
                "00dmd16ngyag3n46rbnl9vy82ih6g0y02yfwkid32a1c8vdbvb3z"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                    ; no test suite
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (string-append
                                   (assoc-ref outputs "out")
                                   "/share/" ,name)))
                        (copy-recursively "powerline" (string-append out "/powerline"))
                        (for-each (lambda (file) (copy-file file (string-append out "/" file)))
                                  '("basic.tmuxtheme"
                                    "default.tmuxtheme"
                                    "themepack.tmux"))))))))
    (home-page "https://github.com/jimeh/tmux-themepack")
    (synopsis "Collection of themes for Tmux")
    (description
     "This package provides several themes for Tmux, the terminal multiplexer.")
    (license license:wtfpl2)))

(define-public tmuxifier
  (package
    (name "tmuxifier")
    (version "0.13.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jimeh/tmuxifier")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1b6a1cw2mnml84k5vhbcp58kvp94xlnlpp4kwdhqw4jrzfgcjfzd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out    (assoc-ref %outputs "out"))
                             (bindir (string-append out "/bin"))
                             (share  (string-append out "/share/" ,name)))
                        (install-file "bin/tmuxifier" bindir)
                        (substitute* (string-append bindir "/tmuxifier")
                          (("set -e")
                           (string-append "TMUXIFIER=" share "\nset -e")))
                        (for-each (lambda (init-script)
                                    (install-file init-script (string-append
                                                               share "/init")))
                                  '("init.sh" "init.tcsh" "init.fish"))
                        (for-each (lambda (dir)
                                    (copy-recursively dir (string-append
                                                           share "/" dir)))
                                  '("completion" "lib" "libexec"
                                    "templates"))))))))
    (home-page "https://github.com/jimeh/tmuxifier")
    (synopsis "Powerful session, window & pane management for Tmux")
    (description "Tmuxifier allows you to easily create, edit, and load
@code{layout} files, which are simple shell scripts where you use the tmux
command and helper commands provided by tmuxifier to manage Tmux sessions and
windows.")
    (license license:expat)))

(define-public python-libtmux
  (package
    (name "python-libtmux")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       ;; PyPI source tarball does not include tests.
       (uri (git-reference
             (url "https://github.com/tmux-python/libtmux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "068vy92f2668vrjvd3laqvxd48cmna66f2msdmwk2hm9qxklgf51"))))
    (build-system python-build-system)
    (propagated-inputs
     (list procps))             ;tests need top
    (native-inputs
     (list python-pytest tmux))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Fix <https://github.com/tmux-python/libtmux/issues/265>.
             (setenv "LANG" "en_US.utf8")
             ;; Skip tests that I suspect fail because of a change
             ;; in behavior in tmux 3 from tmux 2
             ;; https://github.com/tmux-python/libtmux/issues/281
             (invoke "pytest" "-vv" "-k"
                     (string-append "not test_show_option_unknown "
                                    "and not test_show_window_option_unknown"))
             #t)))))
    (home-page "https://github.com/tmux-python/libtmux")
    (synopsis "Python API for tmux")
    (description "Libtmux is the tool behind @command{tmuxp}, a tmux workspace
manager in Python.  It creates object mappings to traverse, inspect and interact
with live tmux sessions.")
    (license license:expat)))

(define-public python-daemux
  (package
    (name "python-daemux")
    (version "0.1.0")
    (source
     ;; We fetch from the Git repo because there are no tests in the PyPI
     ;; archive.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edouardklein/daemux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cb8v552f2hkwz6d3hwsmrz3gd28jikga3lcc3r1zlw8ra7804ph"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (mkdir-p "tmptmux")
                      (setenv "TMUX_TMPDIR" (string-append (getcwd) "/tmptmux"))
                      (invoke "tmux" "new-session" "-d")
                      (invoke "make" "test"))))))
    (propagated-inputs
     (list python-libtmux))
    (native-inputs
     (list python-coverage python-sphinx tmux))
    (home-page "https://github.com/edouardklein/daemux")
    (synopsis "Start, stop, restart and check daemons via tmux")
    (description
     "Daemux lets you run daemons in a @command{tmux} pane.  Users can launch
long-running background tasks, and check these tasks' health by hand, relaunch
them, etc., by attaching to the corresponding pane in tmux.")
    (license license:agpl3+)))

(define-public tmux-xpanes
  (package
    (name "tmux-xpanes")
    (version "4.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/greymd/tmux-xpanes")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09fmnn1q76r1l4cv7clmfr3j9cjmd053kq238d0qj2i486948ivv"))))
    (build-system trivial-build-system)
    (inputs
     (list bash))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append (assoc-ref %build-inputs "bash") "/bin"))
         (copy-recursively (assoc-ref %build-inputs "source") ".")
         (substitute* "bin/xpanes"
           (("/bin/bash") (which "bash")))
         (install-file "bin/xpanes" (string-append %output "/bin"))
         (install-file "man/xpanes.1" (string-append %output "/man/man1"))
         #t)))
    (home-page "https://github.com/greymd/tmux-xpanes")
    (synopsis "Tmux based terminal divider")
    (description "This package provides tmux-based terminal divider.

@code{xpanes} or @code{tmux-xpanes} (alias of @code{xpanes}) commands have
following features:

@itemize
@item Split tmux window into multiple panes.
@item Build command lines & execute them on the panes.
@item Runnable from outside of tmux session.
@item Runnable from inside of tmux session.
@item Record operation log.
@item Flexible layout arrangement for panes.
@item Display pane title on each pane.
@item Generate command lines from standard input (Pipe mode).
@end itemize")
    (license license:expat)))

(define-public tmux-plugin-resurrect
  (let ((commit "a2ddfb96b94bb64a7a2e3f5fa2a7c57dce8ad579")
        (revision "0"))
    (package
      (name "tmux-plugin-resurrect")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tmux-plugins/tmux-resurrect/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1gc8z99na1d4scn2kq4alwyn43h3r7ykz9bkhcypjh8iri6dsl0c"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let ((out (string-append %output
                                               "/share/tmux-plugins/resurrect/")))
                       (mkdir-p out)
                       (copy-recursively (assoc-ref %build-inputs "source") out)))))
      (synopsis "Restore tmux environment after system restart")
      (description
       "This plugin goes to great lengths to save and restore all the details
from your tmux environment.  Here's what's been taken care of:

@itemize
@item all sessions, windows, panes and their order
@item current working directory for each pane
@item exact pane layouts within windows (even when zoomed)
@item active and alternative session
@item active and alternative window for each session
@item windows with focus
@item active pane for each window
@item \"grouped sessions\" (useful feature when using tmux with multiple monitors)
@item programs running within a pane! More details in the restoring programs doc.
@end itemize

Optional:

@itemize
@item restoring vim and neovim sessions
@item restoring pane contents
@end itemize")
      (home-page "https://github.com/tmux-plugins/tmux-resurrect/")
      (license license:expat))))

(define-public tmux-plugin-mem-cpu-load
  (package
    (name "tmux-plugin-mem-cpu-load")
    (version "3.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thewtex/tmux-mem-cpu-load")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "03bax7g9jlsci44ccs50drh617ya3fzvlplwyvxfyb7mgmh85r72"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (synopsis "CPU, RAM, and load monitor for use with tmux")
    (description "This package provides a lightweight program for system
monitoring in the status line of tmux.

The memory monitor displays the used and available memory.

The CPU usage monitor outputs a percent CPU usage over all processors.  It
also displays a textual bar graph of the current percent usage.

The system load average is also displayed.")
    (home-page "https://github.com/thewtex/tmux-mem-cpu-load")
    (license license:asl2.0)))
