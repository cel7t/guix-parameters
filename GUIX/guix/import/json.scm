;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix import json)
  #:use-module (json)
  #:use-module (guix http-client)
  #:use-module (guix import utils)
  #:use-module (guix import print)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:export (json-fetch
            json->code
            json->scheme-file))

(define* (json-fetch url
                     #:key
                     (http-fetch http-fetch)
                     (timeout 10)
                     ;; Note: many websites returns 403 if we omit a
                     ;; 'User-Agent' header.
                     (headers `((user-agent . "GNU Guile")
                                (Accept . "application/json"))))
  "Return a representation of the JSON resource URL (a list or hash table), or
#f if URL returns 403 or 404.  HEADERS is a list of HTTP headers to pass in
the query.  HTTP-FETCH is called to perform the request: for example, to
enable caching, supply 'http-fetch/cached'."
  (guard (c ((and (http-get-error? c)
                  (let ((error (http-get-error-code c)))
                    (or (= 403 error)
                        (= 404 error))))
             #f))
    (let* ((port   (http-fetch url #:timeout timeout #:headers headers))
           (result (json->scm port)))
      (close-port port)
      result)))

(define (json->code file-name)
  "Read FILE-NAME containing one ore more JSON package definitions and return
a list of S-expressions, or return #F when the JSON is invalid."
  (catch 'json-invalid
    (lambda ()
      (let ((json (json-string->scm
                   (with-input-from-file file-name read-string))))
        (match json
          (#(packages ...)
           ;; To allow definitions to refer to one another, collect references
           ;; to local definitions and tell alist->package to ignore them.
           (second
            (memq #:result
                  (fold
                   (lambda (pkg names+result)
                     (match names+result
                       ((#:names names #:result result)
                        (list #:names
                              (cons (assoc-ref pkg "name") names)
                              #:result
                              (append result
                                      (list
                                       (package->code (alist->package pkg names))
                                       (string->symbol (assoc-ref pkg "name"))))))))
                        (list #:names '()
                              #:result '())
                        packages))))
          (package
            (list (package->code (alist->package json))
                  (string->symbol (assoc-ref json "name")))))))
    (const #f)))

(define (json->scheme-file file)
  "Convert the FILE containing a JSON package definition to a Scheme
representation and return the new file name (or #F on error)."
  (and-let* ((sexprs (json->code file))
             (file* (let* ((tempdir (or (getenv "TMPDIR") "/tmp"))
                           (template (string-append tempdir "/guix-XXXXXX"))
                           (port     (mkstemp! template)))
                      (close-port port)
                      template)))
    (call-with-output-file file*
      (lambda (port)
        (write '(use-modules (gnu)
                             (guix)
                             ((guix licenses) #:prefix license:))
               port)
        (for-each (cut write <> port) sexprs)))
    file*))
