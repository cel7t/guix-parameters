;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018-2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-inferior)
  #:use-module (guix tests)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages sqlite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim))

(define %top-srcdir
  (dirname (search-path %load-path "guix.scm")))

(define %top-builddir
  (dirname (search-path %load-compiled-path "guix.go")))

(define %store
  (open-connection-for-tests))

(define (manifest-entry->list entry)
  (list (manifest-entry-name entry)
        (manifest-entry-version entry)
        (manifest-entry-output entry)
        (manifest-entry-search-paths entry)
        (map manifest-entry->list (manifest-entry-dependencies entry))))


(test-begin "inferior")

(test-equal "open-inferior"
  '(42 #t)
  (let ((inferior (open-inferior %top-builddir
                                 #:command "scripts/guix")))
    (and (inferior? inferior)
         (let ((a (inferior-eval '(apply * '(6 7)) inferior))
               (b (inferior-eval '(@ (gnu packages base) coreutils)
                                 inferior)))
           (close-inferior inferior)
           (list a (inferior-object? b))))))

(test-equal "close-inferior"
  '((hello) (world))
  (let* ((inferior1 (open-inferior %top-builddir #:command "scripts/guix"))
         (lst1      (inferior-eval '(list 'hello) inferior1))
         (inferior2 (open-inferior %top-builddir #:command "scripts/guix"))
         (lst2      (inferior-eval '(list 'world) inferior2)))
    ;; This call succeeds if and only if INFERIOR2 does not also hold a file
    ;; descriptor to the socketpair beneath INFERIOR1; otherwise it blocks.
    ;; See <https://issues.guix.gnu.org/55441#10>.
    (close-inferior inferior1)

    (close-inferior inferior2)
    (list lst1 lst2)))

(test-equal "&inferior-exception"
  '(a b c d)
  (let ((inferior (open-inferior %top-builddir
                                 #:command "scripts/guix")))
    (guard (c ((inferior-exception? c)
               (close-inferior inferior)
               (and (eq? inferior (inferior-exception-inferior c))
                    (match (inferior-exception-stack c)
                      (((_ (files lines columns)) ..1)
                       (member "guix/repl.scm" files)))
                    (inferior-exception-arguments c))))
      (inferior-eval '(throw 'a 'b 'c 'd) inferior)
      'badness)))

(test-equal "&inferior-exception, legacy mode"
  '(a b c d)
  ;; Omit #:command to open an inferior in "legacy" mode, where Guile runs
  ;; directly.
  (let ((inferior (open-inferior %top-builddir)))
    (guard (c ((inferior-exception? c)
               (close-inferior inferior)
               (and (eq? inferior (inferior-exception-inferior c))
                    (inferior-exception-arguments c))))
      (inferior-eval '(throw 'a 'b 'c 'd) inferior)
      'badness)))

(test-equal "inferior-packages"
  (take (sort (fold-packages (lambda (package lst)
                               (cons (list (package-name package)
                                           (package-version package)
                                           (package-home-page package)
                                           (package-location package))
                                     lst))
                             '())
              (lambda (x y)
                (string<? (car x) (car y))))
        10)
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (packages (inferior-packages inferior)))
    (and (every string? (map inferior-package-synopsis packages))
         (let ()
           (define result
             (take (sort (map (lambda (package)
                                (list (inferior-package-name package)
                                      (inferior-package-version package)
                                      (inferior-package-home-page package)
                                      (inferior-package-location package)))
                              packages)
                         (lambda (x y)
                           (string<? (car x) (car y))))
                   10))
           (close-inferior inferior)
           result))))

(test-equal "inferior-available-packages"
  (take (sort (fold-available-packages
               (lambda* (name version result
                              #:key supported? deprecated?
                              #:allow-other-keys)
                 (if (and supported? (not deprecated?))
                     (alist-cons name version result)
                     result))
               '())
              (lambda (x y)
                (string<? (car x) (car y))))
        10)
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (packages (inferior-available-packages inferior)))
    (close-inferior inferior)
    (take (sort packages (lambda (x y)
                           (string<? (car x) (car y))))
          10)))

(test-equal "lookup-inferior-packages"
  (let ((->list (lambda (package)
                  (list (package-name package)
                        (package-version package)
                        (package-location package)))))
    (list (map ->list (find-packages-by-name "guile" #f))
          (map ->list (find-packages-by-name "guile" "2.2"))))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (->list   (lambda (package)
                     (list (inferior-package-name package)
                           (inferior-package-version package)
                           (inferior-package-location package))))
         (lst1     (map ->list
                        (lookup-inferior-packages inferior "guile")))
         (lst2     (map ->list
                        (lookup-inferior-packages inferior
                                                  "guile" "2.2"))))
    (close-inferior inferior)
    (list lst1 lst2)))

(test-assert "lookup-inferior-packages and eq?-ness"
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (lst1     (lookup-inferior-packages inferior "guile"))
         (lst2     (lookup-inferior-packages inferior "guile")))
    (close-inferior inferior)
    (every eq? lst1 lst2)))

(test-equal "inferior-package-inputs"
  (let ((->list (match-lambda
                  ((label (? package? package) . rest)
                   `(,label
                     (package ,(package-name package)
                              ,(package-version package)
                              ,(package-location package))
                     ,@rest)))))
    (list (map ->list (package-inputs guile-3.0-latest))
          (map ->list (package-native-inputs guile-3.0-latest))
          (map ->list (package-propagated-inputs guile-3.0-latest))))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (guile    (first (lookup-inferior-packages inferior "guile")))
         (->list   (match-lambda
                     ((label (? inferior-package? package) . rest)
                      `(,label
                        (package ,(inferior-package-name package)
                                 ,(inferior-package-version package)
                                 ,(inferior-package-location package))
                        ,@rest))))
         (result   (list (map ->list (inferior-package-inputs guile))
                         (map ->list
                              (inferior-package-native-inputs guile))
                         (map ->list
                              (inferior-package-propagated-inputs
                               guile)))))
    (close-inferior inferior)
    result))

(test-equal "inferior-package-search-paths"
  (package-native-search-paths guile-3.0)
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (guile    (first (lookup-inferior-packages inferior "guile")))
         (result   (inferior-package-native-search-paths guile)))
    (close-inferior inferior)
    result))

(test-equal "inferior-eval-with-store"
  (add-text-to-store %store "foo" "Hello, world!")
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix")))
    (inferior-eval-with-store inferior %store
                              '(lambda (store)
                                 (add-text-to-store store "foo"
                                                    "Hello, world!")))))

(test-assert "inferior-eval-with-store, &store-protocol-error"
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix")))
    (guard (c ((store-protocol-error? c)
               (string-contains (store-protocol-error-message c)
                                "invalid character")))
      (inferior-eval-with-store inferior %store
                                '(lambda (store)
                                   (add-text-to-store store "we|rd/?!@"
                                                      "uh uh")))
      #f)))

(test-equal "inferior-eval-with-store, exception"
  '(the-answer = 42)
  (let ((inferior (open-inferior %top-builddir
                                 #:command "scripts/guix")))
    (guard (c ((inferior-exception? c)
               (close-inferior inferior)
               (inferior-exception-arguments c)))
      (inferior-eval-with-store inferior %store
                                '(lambda (store)
                                   (throw 'the-answer '= 42))))))

(test-equal "inferior-eval-with-store, not a procedure"
  'wrong-type-arg
  (let ((inferior (open-inferior %top-builddir
                                 #:command "scripts/guix")))
    (guard (c ((inferior-exception? c)
               (close-inferior inferior)
               (car (inferior-exception-arguments c))))
     (inferior-eval-with-store inferior %store '(+ 1 2)))))

(test-equal "inferior-package-derivation"
  (map derivation-file-name
       (list (package-derivation %store %bootstrap-guile "x86_64-linux")
             (package-derivation %store %bootstrap-guile "armhf-linux")))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (packages (inferior-packages inferior))
         (guile    (find (lambda (package)
                           (string=? (package-name %bootstrap-guile)
                                     (inferior-package-name package)))
                         packages)))
    (map derivation-file-name
         (list (inferior-package-derivation %store guile "x86_64-linux")
               (inferior-package-derivation %store guile "armhf-linux")))))

(unless (package-replacement sqlite)
  (test-skip 1))

(test-equal "inferior-package-replacement"
  (package-derivation %store
                      (package-replacement sqlite)
                      "x86_64-linux")
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (packages (inferior-packages inferior)))
    (match (lookup-inferior-packages inferior
                                     (package-name sqlite)
                                     (package-version sqlite))
      ((inferior-sqlite rest ...)
       (inferior-package-derivation %store
                                    (inferior-package-replacement
                                     inferior-sqlite)
                                    "x86_64-linux")))))

(test-equal "inferior-package->manifest-entry"
  (manifest-entry->list (package->manifest-entry
                         (first (find-best-packages-by-name "guile" #f))))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (guile    (first (lookup-inferior-packages inferior "guile")))
         (entry    (inferior-package->manifest-entry guile)))
    (close-inferior inferior)
    (manifest-entry->list entry)))

(test-equal "packages->manifest"
  (map manifest-entry->list
       (manifest-entries (packages->manifest
                          (find-best-packages-by-name "guile" #f))))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (guile    (first (lookup-inferior-packages inferior "guile")))
         (manifest (packages->manifest (list guile))))
    (close-inferior inferior)
    (map manifest-entry->list (manifest-entries manifest))))

(test-equal "#:error-port stderr"
  42
  ;; There's a special case in open-bidirectional-pipe for
  ;; (current-error-port) being stderr, so this test just checks that
  ;; open-inferior doesn't raise an exception
  (let ((inferior (open-inferior %top-builddir
                                 #:command "scripts/guix"
                                 #:error-port (current-error-port))))
    (and (inferior? inferior)
         (inferior-eval '(display "test" (current-error-port)) inferior)
         (let ((result (inferior-eval '(apply * '(6 7)) inferior)))
           (close-inferior inferior)
           result))))

(test-equal "#:error-port pipe"
  "42"
  (match (pipe)
    ((port-to-read-from . port-to-write-to)

     (setvbuf port-to-read-from 'line)
     (setvbuf port-to-write-to 'line)

     (let ((inferior (open-inferior %top-builddir
                                    #:command "scripts/guix"
                                    #:error-port port-to-write-to)))
       (and (inferior? inferior)
            (begin
              (inferior-eval '(display "42\n" (current-error-port)) inferior)

              (let loop ((line (read-line port-to-read-from)))
                (if (string=? line "42")
                    (begin
                      (close-inferior inferior)
                      line)
                    (loop (read-line port-to-read-from))))))))))

(test-end "inferior")
