;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 threads)
             (srfi srfi-1)
             (guix build compile)
             (guix build utils))

(define host (getenv "host"))
(define srcdir (getenv "srcdir"))

(define (relative-file file)
  (if (string-prefix? (string-append srcdir "/") file)
      (string-drop file (+ 1 (string-length srcdir)))
      file))

(define (file-mtime<? f1 f2)
  (< (stat:mtime (stat f1))
     (stat:mtime (stat f2))))

(define (scm->go file)
  (let* ((relative (relative-file file))
         (without-extension (string-drop-right relative 4)))
    (string-append without-extension ".go")))

(define (file-needs-compilation? file)
  (let ((go (scm->go file)))
    (or (not (file-exists? go))
        (file-mtime<? go file))))

(define* (parallel-job-count #:optional (flags (getenv "MAKEFLAGS")))
  "Return the number of parallel jobs as determined by FLAGS, the flags passed
to 'make'."
  (match flags
    (#f (current-processor-count))
    (flags
     (let ((initial-flags (string-tokenize flags)))
       (let loop ((flags initial-flags))
         (match flags
           (()
            ;; Note: GNU make prior to version 4.2 would hide "-j" flags from
            ;; $MAKEFLAGS.  Thus, check for a "--jobserver" flag here and
            ;; assume we're using all cores if specified.
            (if (any (lambda (flag)
                       (string-prefix? "--jobserver" flag))
                     initial-flags)
                (current-processor-count)         ;GNU make < 4.2
                1))                               ;sequential make
           (("-j" (= string->number count) _ ...)
            (if (integer? count)
                count
                (current-processor-count)))
           ((head tail ...)
            (if (string-prefix? "-j" head)
                (match (string-drop head 2)
                  (""
                   (current-processor-count))
                  ((= string->number count)
                   (if (integer? count)
                       count
                       (current-processor-count))))
                (loop tail)))))))))

(define (parallel-job-count*)
  ;; XXX: Work around memory requirements not sustainable on i686 above '-j4'
  ;; or so: <https://bugs.gnu.org/40522>.
  (let ((count (parallel-job-count)))
    (if (string-prefix? "i686" %host-type)
        (min count 4)
        count)))

(define (% completed total)
  "Return the completion percentage of COMPLETED over TOTAL as an integer."
  (inexact->exact (round (* 100. (/ completed total)))))

;; Install a SIGINT handler to give unwind handlers in 'compile-file' an
;; opportunity to run upon SIGINT and to remove temporary output files.
(sigaction SIGINT
  (lambda args
    (exit 1)))

(match (command-line)
  ((_ "--total" (= string->number grand-total)
      "--completed" (= string->number processed)
      . files)
   ;; GRAND-TOTAL is the total number of .scm files in the project; PROCESSED
   ;; is the total number of .scm files already compiled in previous
   ;; invocations of this script.
   (catch #t
     (lambda ()
       (let* ((to-build  (filter file-needs-compilation? files))
              (processed (+ processed
                            (- (length files) (length to-build)))))
         (compile-files srcdir (getcwd) to-build
                        #:workers (parallel-job-count*)
                        #:host host
                        #:report-load (lambda (file total completed)
                                        (when file
                                          (format #t "[~3d%] LOAD     ~a~%"
                                                  (% (+ 1 completed
                                                          (* 2 processed))
                                                     (* 2 grand-total))
                                                  file)
                                          (force-output)))
                        #:report-compilation (lambda (file total completed)
                                               (when file
                                                 (format #t "[~3d%] GUILEC   ~a~%"
                                                         (% (+ total completed 1
                                                                     (* 2 processed))
                                                            (* 2 grand-total))
                                                         (scm->go file))
                                                 (force-output))))))
     (lambda _
       (primitive-exit 1))
     (lambda args
       ;; Try to report the error in an intelligible way.
       (let* ((stack   (make-stack #t))
              (frame   (if (> (stack-length stack) 1)
                           (stack-ref stack 1)    ;skip the 'throw' frame
                           (stack-ref stack 0)))
              (ui      (false-if-exception
                        (resolve-module '(guix ui))))
              (report  (and ui
                            (false-if-exception
                             (module-ref ui 'report-load-error)))))
         (if report
             (report (or (and=> (current-load-port) port-filename) "?.scm")
                     args frame)
             (begin
               (print-exception (current-error-port) frame
                                (car args) (cdr args))
               (display-backtrace stack (current-error-port)))))))))
