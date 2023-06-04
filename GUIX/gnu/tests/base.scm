;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (gnu tests base)
  #:use-module (gnu tests)
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:autoload   (gnu system image) (system-image)
  #:use-module (gnu system shadow)
  #:use-module (gnu system nss)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services networking)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages virtualization)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((srfi srfi-1) #:hide (partition))
  #:use-module (ice-9 match)
  #:export (run-basic-test
            %test-basic-os
            %test-linux-libre-5.15
            %test-linux-libre-5.10
            %test-linux-libre-5.4
            %test-linux-libre-4.19
            %test-linux-libre-4.14
            %test-halt
            %test-root-unmount
            %test-cleanup
            %test-mcron
            %test-nss-mdns))

(define %simple-os
  (simple-operating-system))


(define* (run-basic-test os command #:optional (name "basic")
                         #:key
                         initialization
                         root-password
                         desktop?)
  "Return a derivation called NAME that tests basic features of the OS started
using COMMAND, a gexp that evaluates to a list of strings.  Compare some
properties of running system to what's declared in OS, an <operating-system>.

When INITIALIZATION is true, it must be a one-argument procedure that is
passed a gexp denoting the marionette, and it must return gexp that is
inserted before the first test.  This is used to introduce an extra
initialization step, such as entering a LUKS passphrase.

When ROOT-PASSWORD is true, enter it as the root password when logging in.
Otherwise assume that there is no password for root."
  (define special-files
    (service-value
     (fold-services (operating-system-services os)
                    #:target-type special-files-service-type)))

  (define guix&co
    (match (package-transitive-propagated-inputs guix)
      (((labels packages) ...)
       (cons guix packages))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-1)
                       (srfi srfi-19)
                       (srfi srfi-26)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette #$command))

          (test-runner-current (system-test-runner #$output))
          (test-begin "basic")

          #$(and initialization
                 (initialization #~marionette))

          (test-assert "uname"
            (match (marionette-eval '(uname) marionette)
              (#("Linux" host-name version _ architecture)
               (and (string=? host-name
                              #$(operating-system-host-name os))
                    (string-prefix? #$(package-version
                                       (operating-system-kernel os))
                                    version)
                    (string-prefix? architecture %host-type)))))

          ;; Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         #f))))
             marionette))

          (test-eq "stdin is /dev/null"
            'eof
            ;; Make sure services can no longer read from stdin once the
            ;; system has booted.
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (start-service 'user-processes)
                ((@@ (gnu services herd) eval-there)
                 '(let ((result (read (current-input-port))))
                    (if (eof-object? result)
                        'eof
                        result))))
             marionette))

          (test-assert "shell and user commands"
            ;; Is everything in $PATH?
            (zero? (marionette-eval '(system "
. /etc/profile
set -e -x
guix --version
ls --version
grep --version
info --version")
                                    marionette)))

          (test-equal "special files"
            '#$special-files
            (marionette-eval
             '(begin
                (use-modules (ice-9 match))

                (map (match-lambda
                       ((file target)
                        (list file (readlink file))))
                     '#$special-files))
             marionette))

          (test-assert "accounts"
            (let ((users (marionette-eval '(begin
                                             (use-modules (ice-9 match))
                                             (let loop ((result '()))
                                               (match (getpw)
                                                 (#f (reverse result))
                                                 (x  (loop (cons x result))))))
                                          marionette)))
              (lset= equal?
                     (map (lambda (user)
                            (list (passwd:name user)
                                  (passwd:dir user)))
                          users)
                     (list
                      #$@(map (lambda (account)
                                `(list ,(user-account-name account)
                                       ,(user-account-home-directory account)))
                              (operating-system-user-accounts os))))))

          (test-assert "shepherd services"
            (let ((services (marionette-eval
                             '(begin
                                (use-modules (gnu services herd))

                                (map (compose car live-service-provision)
                                     (current-services)))
                             marionette)))
              (lset= eq?
                     (pk 'services services)
                     '(root #$@(operating-system-shepherd-service-names os)))))

          (test-equal "libc honors /etc/localtime"
            -7200          ;CEST = GMT+2
            ;; Assume OS is configured to have a CEST timezone.
            (let* ((sept-2021 (time-second
                               (date->time-utc
                                (make-date 0 0 00 12 01 09 2021 7200)))))
              (marionette-eval
               `(tm:gmtoff (localtime ,sept-2021))
               marionette)))

          (test-equal "/var/log/messages is not world-readable"
            #o640                                ;<https://bugs.gnu.org/40405>
            (begin
              (wait-for-file "/var/log/messages" marionette
                             #:read 'get-u8)
              (marionette-eval '(stat:perms (lstat "/var/log/messages"))
                               marionette)))

          (test-assert "homes"
            (let ((homes
                   '#$(map user-account-home-directory
                           (filter user-account-create-home-directory?
                                   (operating-system-user-accounts os)))))
              (marionette-eval
               `(begin
                  (use-modules (gnu services herd) (srfi srfi-1))

                  ;; Home directories are supposed to exist once 'user-homes'
                  ;; has been started.
                  (start-service 'user-homes)

                  (every (lambda (home)
                           (and (file-exists? home)
                                (file-is-directory? home)))
                         ',homes))
               marionette)))

          (test-assert "skeletons in home directories"
            (let ((users+homes
                   '#$(filter-map (lambda (account)
                                    (and (user-account-create-home-directory?
                                          account)
                                         (not (user-account-system? account))
                                         (list (user-account-name account)
                                               (user-account-home-directory
                                                account))))
                                  (operating-system-user-accounts os))))
              (marionette-eval
               `(begin
                  (use-modules (guix build utils) (srfi srfi-1)
                               (ice-9 ftw) (ice-9 match))

                  (every (match-lambda
                           ((user home)
                            ;; Make sure HOME has all the skeletons...
                            (and (null? (lset-difference string=?
                                                         (scandir "/etc/skel/")
                                                         (scandir home)))

                                 ;; ... and that everything is user-owned.
                                 (let* ((pw  (getpwnam user))
                                        (uid (passwd:uid pw))
                                        (gid (passwd:gid pw))
                                        (st  (lstat home)))
                                   (define (user-owned? file)
                                     (= uid (stat:uid (lstat file))))

                                   (and (= uid (stat:uid st))
                                        (eq? 'directory (stat:type st))
                                        (every user-owned?
                                               (find-files home
                                                           #:directories? #t)))))))
                         ',users+homes))
               marionette)))

          (test-equal "permissions on /root"
            #o700
            (let ((root-home #$(any (lambda (account)
                                      (and (zero? (user-account-uid account))
                                           (user-account-home-directory
                                            account)))
                                    (operating-system-user-accounts os))))
              (stat:perms (marionette-eval `(stat ,root-home) marionette))))

          (test-equal "ownership and permissions of /var/empty"
            '(0 0 #o555)
            (let ((st (marionette-eval `(stat "/var/empty") marionette)))
              (list (stat:uid st) (stat:gid st)
                    (stat:perms st))))

          (test-equal "no extra home directories"
            '()

            ;; Make sure the home directories that are not supposed to be
            ;; created are indeed not created.
            (let ((nonexistent
                   '#$(filter-map (lambda (user)
                                    (and (not
                                          (user-account-create-home-directory?
                                           user))
                                         (user-account-home-directory user)))
                                  (operating-system-user-accounts os))))
              (marionette-eval
               `(begin
                  (use-modules (srfi srfi-1))

                  ;; Note: Do not flag "/var/empty".
                  (filter file-exists?
                          ',(remove (cut string-prefix? "/var/" <>)
                                    nonexistent)))
               marionette)))

          (test-equal "login on tty1"
            "root\n"
            (begin
              ;; XXX: On desktop, GDM3 will switch to TTY7. If this happens
              ;; after we switched to TTY1, we won't be able to login. Make
              ;; sure to wait long enough before switching to TTY1.
              (when #$desktop?
                (sleep 30))

              (marionette-control "sendkey ctrl-alt-f1" marionette)
              ;; Wait for the 'term-tty1' service to be running (using
              ;; 'start-service' is the simplest and most reliable way to do
              ;; that.)
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'term-tty1))
               marionette)

              ;; Now we can type.
              (let ((password #$root-password))
                (if password
                    (begin
                      (marionette-type "root\n" marionette)
                      (wait-for-screen-text marionette
                                            (lambda (text)
                                              (string-contains text "Password"))
                                            #:ocr
                                            #$(file-append ocrad "/bin/ocrad"))
                      (marionette-type (string-append password "\n\n")
                                       marionette))
                    (marionette-type "root\n\n" marionette)))
              (marionette-type "id -un > logged-in\n" marionette)

              ;; It can take a while before the shell commands are executed.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (wait-for-file "/root/logged-in" marionette
                             #:read 'get-string-all
                             #:timeout 30)))

          (test-equal "getlogin on tty1"
            "\"root\""
            (begin
              ;; Assume we logged in in the previous test and type.
              (marionette-type "guile -c '(write (getlogin))' > /root/login-id.tmp\n"
                               marionette)
              (marionette-type "mv /root/login-id{.tmp,}\n"
                               marionette)

              ;; It can take a while before the shell commands are executed.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (wait-for-file "/root/login-id" marionette
                             #:read 'get-string-all
                             #:timeout 30)))

          ;; There should be one utmpx entry for the user logged in on tty1.
          (test-equal "utmpx entry"
            '(("root" "tty1" #f))
            (marionette-eval
             '(begin
                (use-modules (guix build syscalls)
                             (srfi srfi-1))

                (filter-map (lambda (entry)
                              (and (equal? (login-type USER_PROCESS)
                                           (utmpx-login-type entry))
                                   (list (utmpx-user entry) (utmpx-line entry)
                                         (utmpx-host entry))))
                            (utmpx-entries)))
             marionette))

          ;; Likewise for /var/log/wtmp (used by 'last').
          (test-assert "wtmp entry"
            (match (marionette-eval
                    '(begin
                       (use-modules (guix build syscalls)
                                    (srfi srfi-1))

                       (define (entry->list entry)
                         (list (utmpx-user entry) (utmpx-line entry)
                               (utmpx-host entry) (utmpx-login-type entry)))

                       (call-with-input-file "/var/log/wtmp"
                         (lambda (port)
                           (let loop ((result '()))
                             (if (eof-object? (peek-char port))
                                 (map entry->list (reverse result))
                                 (loop (cons (read-utmpx port) result)))))))
                    marionette)
              (((users lines hosts types) ..1)
               (every (lambda (type)
                        (eqv? type (login-type LOGIN_PROCESS)))
                      types))))

          (test-assert "host name resolution"
            (match (marionette-eval
                    '(begin
                       ;; Wait for nscd or our requests go through it.
                       (use-modules (gnu services herd))
                       (start-service 'nscd)

                       (list (getaddrinfo "localhost")
                             (getaddrinfo #$(operating-system-host-name os))))
                    marionette)
              ((((? vector?) ..1) ((? vector?) ..1))
               #t)
              (x
               (pk 'failure x #f))))

          (test-assert "nscd configuration action"
            (marionette-eval '(with-shepherd-action 'nscd ('configuration)
                                                    results
                                (file-exists? (car results)))
                             marionette))

          (test-equal "nscd invalidate action"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'nscd ('invalidate "hosts")
                                                    result
                                                    result)
                             marionette))

          ;; FIXME: The 'invalidate' action can't reliably obtain the exit
          ;; code of 'nscd' so skip this test.
          (test-skip 1)
          (test-equal "nscd invalidate action, wrong table"
            '(#f)                                 ;one value, #f
            (marionette-eval '(with-shepherd-action 'nscd ('invalidate "xyz")
                                                    result
                                                    result)
                             marionette))

          (test-equal "host not found"
            #f
            (marionette-eval
             '(false-if-exception (getaddrinfo "does-not-exist"))
             marionette))

          (test-equal "locale"
            "en_US.utf8"
            (marionette-eval '(let ((before (setlocale LC_ALL "en_US.utf8")))
                                (setlocale LC_ALL before))
                             marionette))

          (test-eq "/run/current-system is a GC root"
            'success!
            (marionette-eval '(begin
                                ;; Make sure the (guix …) modules are found.
                                (eval-when (expand load eval)
                                  (set! %load-path
                                    (append (map (lambda (package)
                                                   (string-append package
                                                                  "/share/guile/site/"
                                                                  (effective-version)))
                                                 '#$guix&co)
                                            %load-path)))

                                (use-modules (srfi srfi-34) (guix store))

                                (let ((system (readlink "/run/current-system")))
                                  (guard (c ((store-protocol-error? c)
                                             (and (file-exists? system)
                                                  'success!)))
                                    (with-store store
                                      (delete-paths store (list system))
                                      #f))))
                             marionette))

          ;; This symlink is currently unused, but better have it point to the
          ;; right place.  See
          ;; <https://lists.gnu.org/archive/html/guix-devel/2016-08/msg01641.html>.
          (test-equal "/var/guix/gcroots/profiles is a valid symlink"
            "/var/guix/profiles"
            (marionette-eval '(readlink "/var/guix/gcroots/profiles")
                             marionette))

          (test-equal "guix-daemon set-http-proxy action"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'guix-daemon
                                  ('set-http-proxy "http://localhost:8118")
                                  result
                                result)
                             marionette))

          (test-equal "guix-daemon set-http-proxy action, clear"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'guix-daemon
                                  ('set-http-proxy)
                                  result
                                result)
                             marionette))

          (test-assert "screendump"
            (begin
              (let ((capture
                     (string-append #$output "/tty1.ppm")))
                (marionette-control
                 (string-append "screendump " capture) marionette)
                (file-exists? capture))))

          (test-assert "screen text"
            (wait-for-screen-text
             marionette
             (lambda (text)
               ;; Check whether the welcome message and shell prompt are
               ;; displayed.  Note: OCR confuses "y" and "V" for instance, so
               ;; we cannot reliably match the whole text.
               (and (string-contains text "This is the GNU")
                    (string-contains text
                                     (string-append
                                      "root@"
                                      #$(operating-system-host-name os)))))
             #:ocr #$(file-append ocrad "/bin/ocrad")))

          (test-end))))

  (gexp->derivation name test))

(define* (test-basic-os #:optional (kernel linux-libre))
  (system-test
   (name (if (eq? kernel linux-libre)
             "basic"
             (string-append (package-name kernel) "-"
                            (version-major+minor (package-version kernel)))))
   (description
    "Instrument %SIMPLE-OS, run it in a VM, and run a series of basic
functionality tests, using the given KERNEL.")
   (value
    (let* ((os  (marionette-operating-system
                 (operating-system
                   (inherit %simple-os)
                   (kernel kernel))
                 #:imported-modules '((gnu services herd)
                                      (guix combinators))))
           (vm  (virtual-machine os)))
      ;; XXX: Add call to 'virtualized-operating-system' to get the exact same
      ;; set of services as the OS produced by
      ;; 'system-qemu-image/shared-store-script'.
      (run-basic-test (virtualized-operating-system os '())
                      #~(list #$vm)
                      name)))))

(define %test-basic-os
  (test-basic-os))

;; Ensure the LTS kernels are up to snuff, too.
(define %test-linux-libre-5.15
  (test-basic-os linux-libre-5.15))

(define %test-linux-libre-5.10
  (test-basic-os linux-libre-5.10))

(define %test-linux-libre-5.4
  (test-basic-os linux-libre-5.4))

(define %test-linux-libre-4.19
  (test-basic-os linux-libre-4.19))

(define %test-linux-libre-4.14
  (test-basic-os linux-libre-4.14))


;;;
;;; Halt.
;;;

(define (run-halt-test vm)
  ;; As reported in <http://bugs.gnu.org/26931>, running tmux would previously
  ;; lead the 'stop' method of 'user-processes' to an infinite loop, with the
  ;; tmux server process as a zombie that remains in the list of processes.
  ;; This test reproduces this scenario.
  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette))

          (define marionette
            (make-marionette '(#$vm)))

          (define ocrad
            #$(file-append ocrad "/bin/ocrad"))

          ;; Wait for tty1 and log in.
          (marionette-eval '(begin
                              (use-modules (gnu services herd))
                              (start-service 'term-tty1))
                           marionette)
          (marionette-type "root\n" marionette)

          ;; Start tmux and wait for it to be ready.
          (marionette-type "tmux new-session 'echo 1 > /ready; bash'\n"
                           marionette)
          (wait-for-file "/ready" marionette)

          ;; Make sure to stop the test after a while.
          (sigaction SIGALRM (lambda _
                               (format (current-error-port)
                                       "FAIL: Time is up, but VM still running.\n")
                               (primitive-exit 1)))
          (alarm 10)

          ;; Get debugging info.
          (marionette-eval '(current-output-port
                             (open-file "/dev/console" "w0"))
                           marionette)
          (marionette-eval '(system* #$(file-append procps "/bin/ps")
                                     "-eo" "pid,ppid,stat,comm")
                           marionette)

          ;; See if 'halt' actually works.
          (marionette-eval '(system* "/run/current-system/profile/sbin/halt")
                           marionette)

          ;; If we reach this line, that means the VM was properly stopped in
          ;; a timely fashion.
          (alarm 0)
          (call-with-output-file #$output
            (lambda (port)
              (display "success!" port))))))

  (gexp->derivation "halt" test))

(define %test-halt
  (system-test
   (name "halt")
   (description
    "Use the 'halt' command and make sure it succeeds and does not get stuck
in a loop.  See <http://bugs.gnu.org/26931>.")
   (value
    (let ((os (marionette-operating-system
               (operating-system
                 (inherit %simple-os)
                 (packages (cons tmux %base-packages)))
               #:imported-modules '((gnu services herd)
                                    (guix combinators)))))
      (run-halt-test (virtual-machine os))))))


;;;
;;; Root cleanly unmounted.
;;;

(define (run-root-unmount-test os)
  (define test-image
    (image (operating-system os)
           (format 'compressed-qcow2)
           (volatile-root? #f)
           (shared-store? #f)
           (partition-table-type 'mbr)
           (partitions
            (list (partition
                   (size 'guess)
                   (offset (* 512 2048))          ;leave room for GRUB
                   (flags '(boot))
                   (label "root-under-test")))))) ;max 16 characters!

  (define observer-os
    (marionette-operating-system
     %simple-os
     #:imported-modules
     (source-module-closure '((guix build syscalls)
                              (gnu build file-systems)))))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)
                              (guix build utils)))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build utils)
                       (srfi srfi-64)
                       (ice-9 ftw))

          (define image
            "/tmp/writable-image.qcow2")

          (define (test-system-marionette)
            ;; Return a marionette on a system where we'll run 'halt'.
            (invoke #$(file-append qemu-minimal "/bin/qemu-img")
                    "create" "-f" "qcow2" image "3G"
                    "-b" #$(system-image test-image) "-F" "qcow2")
            (make-marionette
             `(,(string-append #$qemu-minimal "/bin/" (qemu-command))
               ,@(if (file-exists? "/dev/kvm")
                     '("-enable-kvm")
                     '())
               "-no-reboot"
               "-m" "1024"                        ;memory size, in MiB
               "-drive" ,(format #f "file=~a,if=virtio" image))))

          (define witness-size
            ;; Size of the /witness file.
            (* 20 (expt 2 20)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "root-unmount")

          (let ((marionette (test-system-marionette)))
            (test-assert "file created"
              (marionette-eval `(begin
                                  (use-modules (guix build utils))
                                  (call-with-output-file "/witness"
                                    (lambda (port)
                                      (call-with-input-file "/dev/random"
                                        (lambda (input)
                                          (dump-port input port
                                                     ,witness-size))))))
                               marionette))

            ;; Halt the system.
            (marionette-eval '(system* "/run/current-system/profile/sbin/halt")
                             marionette)

            (display "waiting for marionette to complete...")
            (force-output)
            (false-if-exception (waitpid (marionette-pid marionette)))
            (display " done\n")
            (force-output))

          ;; Remove the sockets used by the marionette above to avoid
          ;; EADDRINUSE.
          (for-each delete-file
                    (find-files "/tmp" (lambda (file stat)
                                         (eq? (stat:type stat) 'socket))))

          ;; Now boot another system and check whether the root file system of
          ;; the first one was cleanly unmounted.

          (let ((observer
                 (make-marionette (list #$(virtual-machine observer-os)
                                        "-drive"
                                        (format #f "file=~a,if=virtio" image)))))
            (test-assert "partitions"
              (marionette-eval '(begin
                                  (use-modules (gnu build file-systems))
                                  (disk-partitions))
                               observer))

            (test-assert "partition found"
              (marionette-eval '(find-partition-by-label "root-under-test")
                               observer))

            (test-assert "root file system is clean"
              (marionette-eval '(cleanly-unmounted-ext2?
                                 (find-partition-by-label "root-under-test"))
                               observer))

            (test-equal "root file system contains /witness"
              witness-size
              (let ((files (marionette-eval
                            '(begin
                               (use-modules (guix build syscalls)
                                            (ice-9 ftw))
                               (mount (find-partition-by-label "root-under-test")
                                      "/mnt" "ext4" MS_RDONLY)
                               (scandir "/mnt"))
                            observer)))
                (if (member "witness" files)
                    (marionette-eval '(stat:size (stat "/mnt/witness"))
                                     observer)
                    files))))

          (test-end))))

  (gexp->derivation "root-unmount" test))

(define %test-root-unmount
  (system-test
   (name "root-unmount")
   (description
    "Make sure the root file system is cleanly unmounted when the system is
halted.")
   (value
    (let ((os (marionette-operating-system %simple-os)))
      (run-root-unmount-test os)))))


;;;
;;; Cleanup of /tmp, /var/run, etc.
;;;

(define %cleanup-os
  (simple-operating-system
   (simple-service 'dirty-things
                   boot-service-type
                   (let ((script (plain-file
                                  "create-utf8-file.sh"
                                  (string-append
                                   "echo $0: dirtying /tmp...\n"
                                   "set -e; set -x\n"
                                   "touch /witness\n"
                                   "exec touch /tmp/λαμβδα"))))
                     (with-imported-modules '((guix build utils))
                       #~(begin
                           (setenv "PATH"
                                   #$(file-append coreutils "/bin"))
                           (invoke #$(file-append bash "/bin/sh")
                                   #$script)))))))

(define (run-cleanup-test name)
  (define os
    (marionette-operating-system %cleanup-os
                                 #:imported-modules '((gnu services herd)
                                                      (guix combinators))))
  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "cleanup")

          (test-assert "dirty service worked"
            (marionette-eval '(file-exists? "/witness") marionette))

          (test-equal "/tmp cleaned up"
            '("." "..")
            (marionette-eval '(begin
                                (use-modules (ice-9 ftw))
                                (scandir "/tmp"))
                             marionette))

          (test-end))))

  (gexp->derivation "cleanup" test))

(define %test-cleanup
  ;; See <https://bugs.gnu.org/26353>.
  (system-test
   (name "cleanup")
   (description "Make sure the 'cleanup' service can remove files with
non-ASCII names from /tmp.")
   (value (run-cleanup-test name))))


;;;
;;; Mcron.
;;;

(define %mcron-os
  ;; System with an mcron service, with one mcron job for "root" and one mcron
  ;; job for an unprivileged user.
  (let ((job1 #~(job '(next-second '(0 5 10 15 20 25 30 35 40 45 50 55))
                     (lambda ()
                       (unless (file-exists? "witness")
                        (call-with-output-file "witness"
                          (lambda (port)
                            (display (list (getuid) (getgid)) port)))))))
        (job2 #~(job next-second-from
                     (lambda ()
                       (call-with-output-file "witness"
                         (lambda (port)
                           (display (list (getuid) (getgid)) port))))
                     #:user "alice"))
        (job3 #~(job next-second-from             ;to test $PATH
                     "touch witness-touch")))
    (simple-operating-system
     (service mcron-service-type
              (mcron-configuration (jobs (list job1 job2 job3)))))))

(define (run-mcron-test name)
  (define os
    (marionette-operating-system
     %mcron-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "mcron")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'mcron))
             marionette))

          ;; Make sure root's mcron job runs, has its cwd set to "/root", and
          ;; runs with the right UID/GID.
          (test-equal "root's job"
            '(0 0)
            (wait-for-file "/root/witness" marionette))

          ;; Likewise for Alice's job.  We cannot know what its GID is since
          ;; it's chosen by 'groupadd', but it's strictly positive.
          (test-assert "alice's job"
            (match (wait-for-file "/home/alice/witness" marionette)
              ((1000 gid)
               (>= gid 100))))

          ;; Last, the job that uses a command; allows us to test whether
          ;; $PATH is sane.
          (test-equal "root's job with command"
            ""
            (wait-for-file "/root/witness-touch" marionette
                           #:read '(@ (ice-9 rdelim) read-string)))

          ;; Make sure the 'schedule' action is accepted.
          (test-equal "schedule action"
            '(#t)                                 ;one value, #t
            (marionette-eval '(with-shepherd-action 'mcron ('schedule) result
                                result)
                             marionette))

          (test-end))))

  (gexp->derivation name test))

(define %test-mcron
  (system-test
   (name "mcron")
   (description "Make sure the mcron service works as advertised.")
   (value (run-mcron-test name))))


;;;
;;; Avahi and NSS-mDNS.
;;;

(define %avahi-os
  (operating-system
    (inherit %simple-os)
    (name-service-switch %mdns-host-lookup-nss)
    (services (cons* (service avahi-service-type
                              (avahi-configuration (debug? #t)))
                     (service dbus-root-service-type)
                     (service dhcp-client-service-type) ;needed for multicast

                     ;; Enable heavyweight debugging output.
                     (modify-services (operating-system-user-services
                                       %simple-os)
                       (nscd-service-type config
                                          => (nscd-configuration
                                              (inherit config)
                                              (debug-level 3)
                                              (log-file "/dev/console")))
                       (syslog-service-type config
                                            =>
                                            (syslog-configuration
                                             (inherit config)
                                             (config-file
                                              (plain-file
                                               "syslog.conf"
                                               "*.* /dev/console\n")))))))))

(define (run-nss-mdns-test)
  ;; Test resolution of '.local' names via libc.  Start the marionette service
  ;; *after* nscd.  Failing to do that, libc will try to connect to nscd,
  ;; fail, then never try again (see '__nss_not_use_nscd_hosts' in libc),
  ;; leading to '.local' resolution failures.
  (define os
    (marionette-operating-system
     %avahi-os
     #:requirements '(nscd)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define mdns-host-name
    (string-append (operating-system-host-name os)
                   ".local"))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-1)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (mkdir #$output)
          (chdir #$output)

          (test-runner-current (system-test-runner))
          (test-begin "avahi")

          (test-assert "nscd PID file is created"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nscd))
             marionette))

          (test-assert "nscd is listening on its socket"
            (marionette-eval
             ;; XXX: Work around a race condition in nscd: nscd creates its
             ;; PID file before it is listening on its socket.
             '(let ((sock (socket PF_UNIX SOCK_STREAM 0)))
                (let try ()
                  (catch 'system-error
                    (lambda ()
                      (connect sock AF_UNIX "/var/run/nscd/socket")
                      (close-port sock)
                      (format #t "nscd is ready~%")
                      #t)
                    (lambda args
                      (format #t "waiting for nscd...~%")
                      (usleep 500000)
                      (try)))))
             marionette))

          (test-assert "avahi is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'avahi-daemon))
             marionette))

          (test-assert "network is up"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'networking))
             marionette))

          (test-equal "avahi-resolve-host-name"
            0
            (marionette-eval
             '(system*
               "/run/current-system/profile/bin/avahi-resolve-host-name"
               "-v" #$mdns-host-name)
             marionette))

          (test-equal "avahi-browse"
            0
            (marionette-eval
             '(system* "/run/current-system/profile/bin/avahi-browse" "-avt")
             marionette))

          (test-assert "getaddrinfo .local"
            ;; Wait for the 'avahi-daemon' service and perform a resolution.
            (match (marionette-eval
                    '(getaddrinfo #$mdns-host-name)
                    marionette)
              (((? vector? addrinfos) ..1)
               (pk 'getaddrinfo addrinfos)
               (and (any (lambda (ai)
                           (= AF_INET (addrinfo:fam ai)))
                         addrinfos)
                    (any (lambda (ai)
                           (= AF_INET6 (addrinfo:fam ai)))
                         addrinfos)))))

          (test-assert "gethostbyname .local"
            (match (pk 'gethostbyname
                       (marionette-eval '(gethostbyname #$mdns-host-name)
                                        marionette))
              ((? vector? result)
               (and (string=? (hostent:name result) #$mdns-host-name)
                    (= (hostent:addrtype result) AF_INET)))))


          (test-end))))

  (gexp->derivation "nss-mdns" test))

(define %test-nss-mdns
  (system-test
   (name "nss-mdns")
   (description
    "Test Avahi's multicast-DNS implementation, and in particular, test its
glibc name service switch (NSS) module.")
   (value (run-nss-mdns-test))))
