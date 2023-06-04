;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu build marionette)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:export (marionette?
            marionette-pid
            make-marionette
            marionette-eval
            wait-for-file
            wait-for-tcp-port
            wait-for-unix-socket
            marionette-control
            wait-for-screen-text
            %qwerty-us-keystrokes
            marionette-type

            system-test-runner
            qemu-command))

;;; Commentary:
;;;
;;; Instrumentation tools for QEMU virtual machines (VMs).  A "marionette" is
;;; essentially a VM (a QEMU instance) with its monitor connected to a
;;; Unix-domain socket, and with a REPL inside the guest listening on a
;;; virtual console, which is itself connected to the host via a Unix-domain
;;; socket--these are the marionette's strings, connecting it to the almighty
;;; puppeteer.
;;;
;;; Code:

(define-record-type <marionette>
  (marionette command pid monitor repl)
  marionette?
  (command    marionette-command)                 ;list of strings
  (pid        marionette-pid)                     ;integer
  (monitor    marionette-monitor)                 ;port
  (repl       %marionette-repl))                  ;promise of a port

(define-syntax-rule (marionette-repl marionette)
  (force (%marionette-repl marionette)))

(define* (wait-for-monitor-prompt port #:key (quiet? #t))
  "Read from PORT until we have seen all of QEMU's monitor prompt.  When
QUIET? is false, the monitor's output is written to the current output port."
  (define full-prompt
    (string->list "(qemu) "))

  (let loop ((prompt full-prompt)
             (matches '())
             (prefix  '()))
    (match prompt
      (()
       ;; It's useful to set QUIET? so we don't display the echo of our own
       ;; commands.
       (unless quiet?
         (for-each (lambda (line)
                     (format #t "qemu monitor: ~a~%" line))
                   (string-tokenize (list->string (reverse prefix))
                                    (char-set-complement (char-set #\newline))))))
      ((chr rest ...)
       (let ((read (read-char port)))
         (cond ((eqv? read chr)
                (loop rest (cons read matches) prefix))
               ((eof-object? read)
                (error "EOF while waiting for QEMU monitor prompt"
                       (list->string (reverse prefix))))
               (else
                (loop full-prompt
                      '()
                      (cons read (append matches prefix))))))))))

(define* (make-marionette command
                          #:key (socket-directory "/tmp") (timeout 20))
  "Return a QEMU marionette--i.e., a virtual machine with open connections to the
QEMU monitor and to the guest's backdoor REPL."
  (define (file->sockaddr file)
    (make-socket-address AF_UNIX
                         (string-append socket-directory "/" file)))

  (define extra-options
    (list "-nographic"
          "-monitor" (string-append "unix:" socket-directory "/monitor")
          "-chardev" (string-append "socket,id=repl,path=" socket-directory
                                    "/repl")
          "-chardev" (string-append "socket,id=qga,server=on,wait=off,path="
                                    socket-directory "/qemu-ga")

          ;; See
          ;; <http://www.linux-kvm.org/page/VMchannel_Requirements#Invocation>.
          "-device" "virtio-serial"
          "-device" "virtserialport,chardev=repl,name=org.gnu.guix.port.0"
          "-device" "virtserialport,chardev=qga,name=org.qemu.guest_agent.0"))

  (define (accept* port)
    (match (select (list port) '() (list port) timeout)
      (((port) () ())
       (accept port))
      (_
       (error "timeout in 'accept'" port))))

  (let ((monitor (socket AF_UNIX SOCK_STREAM 0))
        (repl    (socket AF_UNIX SOCK_STREAM 0)))
    (bind monitor (file->sockaddr "monitor"))
    (listen monitor 1)
    (bind repl (file->sockaddr "repl"))
    (listen repl 1)

    (match (primitive-fork)
      (0
       (catch #t
         (lambda ()
           (close monitor)
           (close repl)
           (match command
             ((program . args)
              (apply execl program program
                     (append args extra-options)))))
         (lambda (key . args)
           (print-exception (current-error-port)
                            (stack-ref (make-stack #t) 1)
                            key args)
           (primitive-exit 1))))
      (pid
       (format #t "QEMU runs as PID ~a~%" pid)

       (match (accept* monitor)
         ((monitor-conn . _)
          (display "connected to QEMU's monitor\n")
          (close-port monitor)
          (wait-for-monitor-prompt monitor-conn)
          (display "read QEMU monitor prompt\n")

          (marionette (append command extra-options) pid
                      monitor-conn

                      ;; The following 'accept' call connects immediately, but
                      ;; we don't know whether the guest has connected until
                      ;; we actually receive the 'ready' message.
                      (match (accept* repl)
                        ((repl-conn . addr)
                         (display "connected to guest REPL\n")
                         (close-port repl)
                         ;; Delay reception of the 'ready' message so that the
                         ;; caller can already send monitor commands.
                         (delay
                           (match (read repl-conn)
                             ('ready
                              (display "marionette is ready\n")
                              repl-conn))))))))))))

(define (marionette-eval exp marionette)
  "Evaluate EXP in MARIONETTE's backdoor REPL.  Return the result."
  (match marionette
    (($ <marionette> command pid monitor (= force repl))
     (write exp repl)
     (newline repl)
     (with-exception-handler
         (lambda (exn)
           (simple-format
            (current-error-port)
            "error reading marionette response: ~A
  remaining response: ~A\n"
            exn
            (get-line repl))
           (raise-exception exn))
       (lambda ()
         (read repl))
       #:unwind? #t))))

(define* (wait-for-file file marionette
                        #:key (timeout 10) (read 'read))
  "Wait until FILE exists in MARIONETTE; READ its content and return it.  If
FILE has not shown up after TIMEOUT seconds, raise an error."
  (match (marionette-eval
          `(let loop ((i ,timeout))
             (cond ((file-exists? ,file)
                    (cons 'success
                          (let ((content
                                 (call-with-input-file ,file ,read)))
                            (if (eof-object? content)
                                ;; #<eof> can't be read, so convert to the
                                ;; empty string
                                ""
                                content))))
                   ((> i 0)
                    (sleep 1)
                    (loop (- i 1)))
                   (else
                    'failure)))
          marionette)
    (('success . result)
     result)
    ('failure
     (error "file didn't show up" file))))

(define* (wait-for-tcp-port port marionette
                            #:key
                            (timeout 20)
                            (address `(make-socket-address AF_INET
                                                           INADDR_LOOPBACK
                                                           ,port)))
  "Wait for up to TIMEOUT seconds for PORT to accept connections in
MARIONETTE.  ADDRESS must be an expression that returns a socket address,
typically a call to 'make-socket-address'.  Raise an error on failure."
  ;; Note: The 'connect' loop has to run within the guest because, when we
  ;; forward ports to the host, connecting to the host never raises
  ;; ECONNREFUSED.
  (match (marionette-eval
          `(let* ((address ,address)
                  (sock (socket (sockaddr:fam address) SOCK_STREAM 0)))
             (let loop ((i 0))
               (catch 'system-error
                 (lambda ()
                   (connect sock address)
                   (close-port sock)
                   'success)
                 (lambda args
                   (if (< i ,timeout)
                       (begin
                         (sleep 1)
                         (loop (+ 1 i)))
                       (list 'failure address))))))
          marionette)
    ('success #t)
    (('failure address)
     (error "nobody's listening on port"
            (list (inet-ntop (sockaddr:fam address) (sockaddr:addr address))
                  (sockaddr:port address))))))

(define* (wait-for-unix-socket file-name marionette
                                #:key (timeout 20))
  "Wait for up to TIMEOUT seconds for FILE-NAME, a Unix domain socket, to
accept connections in MARIONETTE.  Raise an error on failure."
  (match (marionette-eval
          `(begin
             (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
               (let loop ((i 0))
                 (catch 'system-error
                   (lambda ()
                     (connect sock AF_UNIX ,file-name)
                     (close-port sock)
                     'success)
                   (lambda args
                     (if (< i ,timeout)
                         (begin
                           (sleep 1)
                           (loop (+ 1 i)))
                         'failure))))))
          marionette)
    ('success #t)
    ('failure
     (error "nobody's listening on unix domain socket" file-name))))

(define (marionette-control command marionette)
  "Run COMMAND in the QEMU monitor of MARIONETTE.  COMMAND is a string such as
\"sendkey ctrl-alt-f1\" or \"screendump foo.ppm\" (info \"(QEMU) QEMU
Monitor\")."
  (match marionette
    (($ <marionette> _ _ monitor)
     (display command monitor)
     (newline monitor)
     ;; The "quit" command terminates QEMU immediately, with no output.
     (unless (string=? command "quit") (wait-for-monitor-prompt monitor)))))

(define* (invoke-ocrad-ocr image #:key (ocrad "ocrad"))
  "Invoke the OCRAD command on image, and return the recognized text."
  (let* ((pipe (open-pipe* OPEN_READ ocrad "-i" "-s" "10" image))
         (text (get-string-all pipe)))
    (unless (zero? (close-pipe pipe))
      (error "'ocrad' failed" ocrad))
    text))

(define* (invoke-tesseract-ocr image #:key (tesseract "tesseract"))
  "Invoke the TESSERACT command on IMAGE, and return the recognized text."
  (let* ((output-basename (tmpnam))
         (output-basename* (string-append output-basename ".txt")))
    (dynamic-wind
      (const #t)
      (lambda ()
        (let ((exit-val (status:exit-val
                         (system* tesseract image output-basename))))
          (unless (zero? exit-val)
            (error "'tesseract' failed" tesseract))
          (call-with-input-file output-basename* get-string-all)))
      (lambda ()
        (false-if-exception (delete-file output-basename))
        (false-if-exception (delete-file output-basename*))))))

(define* (marionette-screen-text marionette #:key (ocr "ocrad"))
  "Take a screenshot of MARIONETTE, perform optical character
recognition (OCR), and return the text read from the screen as a string, along
the screen dump image used.  Do this by invoking OCR, which should be the file
name of GNU Ocrad's@command{ocrad} or Tesseract OCR's @command{tesseract}
command.  The screen dump image returned as the second value should be deleted
if it is not needed."
  (define image (string-append (tmpnam) ".ppm"))
  ;; Use the QEMU Monitor to save an image of the screen to the host.
  (marionette-control (string-append "screendump " image) marionette)
  ;; Process it via the OCR.
  (cond
   ((string-contains ocr "ocrad")
    (values (invoke-ocrad-ocr image #:ocrad ocr) image))
   ((string-contains ocr "tesseract")
    (values (invoke-tesseract-ocr image #:tesseract ocr) image))
   (else (error "unsupported ocr command"))))

(define* (wait-for-screen-text marionette predicate
                               #:key
                               (ocr "ocrad")
                               (timeout 30)
                               pre-action
                               post-action)
  "Wait for TIMEOUT seconds or until the screen text on MARIONETTE matches
PREDICATE, whichever comes first.  Raise an error when TIMEOUT is exceeded.
The error contains the recognized text along the preserved file name of the
screen dump, which is relative to the current working directory.  If
PRE-ACTION is provided, it should be a thunk to call before each OCR attempt.
Likewise for POST-ACTION, except it runs at the end of a successful OCR."
  (define start
    (car (gettimeofday)))

  (define end
    (+ start timeout))

  (let loop ((last-text #f)
             (last-screendump #f))
    (if (> (car (gettimeofday)) end)
        (let ((screendump-backup (string-drop last-screendump 5)))
          ;; Move the file from /tmp/fileXXXXXX.pmm to the current working
          ;; directory, so that it is preserved in the test derivation output.
          (copy-file last-screendump screendump-backup)
          (delete-file last-screendump)
          (error "'wait-for-screen-text' timeout"
                 'ocr-text: last-text
                 'screendump: screendump-backup))
        (let* ((_ (and (procedure? pre-action) (pre-action)))
               (text screendump (marionette-screen-text marionette #:ocr ocr))
               (_ (and (procedure? post-action) (post-action)))
               (result (predicate text)))
          (cond (result
                 (delete-file screendump)
                 result)
                (else
                 (sleep 1)
                 (loop text screendump)))))))

(define %qwerty-us-keystrokes
  ;; Maps "special" characters to their keystrokes.
  '((#\newline . "ret")
    (#\space . "spc")
    (#\- . "minus")
    (#\+ . "shift-equal")
    (#\* . "shift-8")
    (#\= . "equal")
    (#\? . "shift-slash")
    (#\[ . "bracket_left")
    (#\] . "bracket_right")
    (#\{ . "shift-bracket_left")
    (#\} . "shift-bracket_right")
    (#\( . "shift-9")
    (#\) . "shift-0")
    (#\/ . "slash")
    (#\< . "shift-comma")
    (#\> . "shift-dot")
    (#\. . "dot")
    (#\, . "comma")
    (#\: . "shift-semicolon")
    (#\; . "semicolon")
    (#\' . "apostrophe")
    (#\! . "shift-1")
    (#\" . "shift-apostrophe")
    (#\` . "grave_accent")
    (#\bs . "backspace")
    (#\tab . "tab")))

(define (character->keystroke chr keystrokes)
  "Return the keystroke for CHR according to the keyboard layout defined by
KEYSTROKES."
  (if (char-set-contains? char-set:upper-case chr)
      (string-append "shift-" (string (char-downcase chr)))
      (or (assoc-ref keystrokes chr)
          (string chr))))

(define* (string->keystroke-commands str
                                     #:optional
                                     (keystrokes
                                      %qwerty-us-keystrokes))
  "Return a list of QEMU monitor commands to send the keystrokes corresponding
to STR.  KEYSTROKES is an alist specifying a mapping from characters to
keystrokes."
  (string-fold-right (lambda (chr result)
                       (cons (string-append
                              "sendkey "
                              (character->keystroke chr keystrokes))
                             result))
                     '()
                     str))

(define* (marionette-type str marionette
                          #:key (keystrokes %qwerty-us-keystrokes))
  "Type STR on MARIONETTE's keyboard, using the KEYSTROKES alist to map characters
to actual keystrokes."
  (for-each (cut marionette-control <> marionette)
            (string->keystroke-commands str keystrokes)))


;;;
;;; Test helper.
;;;

(define* (system-test-runner #:optional log-directory)
  "Return a SRFI-64 test runner that calls 'exit' upon 'test-end'.  When
LOG-DIRECTORY is specified, create log file within it."
  (let ((runner  (test-runner-simple)))
    ;; Log to a file under LOG-DIRECTORY.
    (test-runner-on-group-begin! runner
      (let ((on-begin (test-runner-on-group-begin runner)))
        (lambda (runner suite-name count)
          (when log-directory
            (catch 'system-error
              (lambda ()
                (mkdir log-directory))
              (lambda args
                (unless (= (system-error-errno args) EEXIST)
                  (apply throw args))))
            (set! test-log-to-file
                  (string-append log-directory "/" suite-name ".log")))
          (on-begin runner suite-name count))))

    ;; The default behavior on 'test-end' is to only write a line if the test
    ;; failed.  Arrange to also write a line on success.
    (test-runner-on-test-end! runner
      (let ((on-end (test-runner-on-test-end runner)))
        (lambda (runner)
          (let* ((kind      (test-result-ref runner 'result-kind))
                 (results   (test-result-alist runner))
                 (test-name (assq-ref results 'test-name)))
            (unless (memq kind '(fail xpass))
              (format (current-output-port) "~a: ~a~%"
                      (string-upcase (symbol->string kind))
                      test-name)))

          (on-end runner))))

    ;; On 'test-end', display test results and exit with zero if and only if
    ;; there were no test failures.
    (test-runner-on-final! runner
      (lambda (runner)
        (let ((success? (= (test-runner-fail-count runner) 0)))
          (test-on-final-simple runner)

          (when (not success?)
            (let* ((log-port (test-runner-aux-value runner))
                   (log-file (port-filename log-port)))
              (format (current-error-port)
                      "\nTests failed, dumping log file '~a'.\n\n"
                      log-file)

              ;; At this point LOG-PORT is not closed yet; flush it.
              (force-output log-port)

              ;; Brute force to avoid dependency on (guix build utils) for
              ;; 'dump-port'.
              (let ((content (call-with-input-file log-file
                               get-bytevector-all)))
                (put-bytevector (current-error-port) content))))

          (exit success?))))
    runner))

(define* (qemu-command #:optional (system %host-type))
  "Return the default name of the QEMU command for SYSTEM."
  (let ((cpu (substring system 0
                        (string-index system #\-))))
    (string-append "qemu-system-"
                   (cond
                    ((string-match "^i[3456]86$" cpu) "i386")
                    ((string-match "armhf" cpu) "arm")
                    (else cpu)))))

;;; marionette.scm ends here
