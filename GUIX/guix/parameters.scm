;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Sarthak Shah <shahsarthakw@gmail.com>
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

(define-module (guix parameters)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:export (package-parameter
            package-parameter?
            package-parameter-name
            package-parameter-property
            package-parameter-type
            package-parameter-description

            boolean
            optionally

            package-parameters
            lookup-package-parameter
            package-parameter-value
            set-package-parameter-value))

;;; Commentary:
;;;
;;; This module provides a way to express high-level "package parameters",
;;; which allow users to customize how packages are built.  Parameters are an
;;; interface that package developers define, where each parameter has a name
;;; and type.  The user interface then converts parameter values from string
;;; to Scheme values and records them in the package properties.
;;;
;;; Package parameters are discoverable; their description is
;;; internationalized.  The possible values of a parameter can be enumerated,
;;; and thus the Cartesian product of all possible parameter values for a
;;; package can be enumerated as well.
;;;
;;; Code:

;; Package parameter interface.
(define-record-type* <package-parameter> package-parameter
  make-package-parameter
  package-parameter?
  (name          package-parameter-name)
  (property      package-parameter-property (default (string->symbol name)))
  (type          package-parameter-type)
  ;; the standard transforms; of the form (list ((build-system) transform))
  ;; parses it like a (case x) statement with x being the build system
  ;; macro converts ((a b) t) -> (a t) (b t) which is then turned into a hash table
  ;; XXX: make an easier way to chain transforms instead of using lambdas
  ;; XXX: write something to throw an error for invalid build systems
  (transforms    package-parameter-transforms (default (alist->hash-table '())))
  (description   package-parameter-description))
  
;; Note that if a transform applies to all but a, b and c,
;; (case build-system
;;  ((a b c) error-out)
;;  (else do-something))
;; works

;; Type of a package parameter.
(define-record-type* <parameter-type> parameter-type
  make-parameter-type
  parameter-type?
  (name          parameter-type-name)              ;debugging purposes only!
  (string->value parameter-type-string->value)
  (value->string parameter-type-value->string)
  (universe      parameter-type-universe))

;; Here is how package parameter specs should be declared:
(parameter-spec
 (package-name "foo")
 (required-parameters "a b c")
 (optional-parameters "d e")

(define-record-type* <parameter-spec> parameter-spec
  make-parameter-spec
  parameter-spec?
  this-parameter-spec
  (package-name package-name)
  ;; local-parameters: parameters specific to the package
  ;; these will have their definitions next to them
  ;; '((libzoo . (package-parameter ...)) ...)
  (local    ps/local-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked))
  (required ps/required-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked)) 
  (optional ps/optional-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked))
  (one-of ps/one-of-parameters
            (default '())
            (thunked))
  (special ps/special-parameters
            (default (alist->hash-table '()))
            (sanitize (lambda (val)
                        (cond ((hash-table? val) val)
                              ((list? val) (alist->hash-table val))
                              (else (throw 'bad! val)))))
            (thunked))
  (transforms ps/transforms
              ;; add checking for whether all LOCAL and
              ;; SPECIAL have been given transforms or not
              (default (alist->hash-table '()))
              (sanitize (lambda (val)
                          (cond ((hash-table? val) val)
                                ((list? val) (alist->hash-table val))
                                (else (throw 'bad! val)))))
              (thunked)))

;; for `one-of` we will still use a list to represent the tree
;; as a hash map will not benefit it

;; g23: Most parameters should be boolean
;; Might make sense to add a recursive type
(define boolean
  ;; The Boolean parameter type.
  (parameter-type (name 'boolean)
                  (universe '(#true #false))
                  (value->string
                   (match-lambda
                     (#f "false")
                     (#t "true")))
                  (string->value
                   (lambda (str)
                     (cond ((string-ci=? str "true")
                            #t)
                           ((string-ci=? str "false")
                            #f)
                           (else
                            (raise (condition
                                    (&message (message "wrong value"))))))))))

(define (package-parameters package)
  (or (assq-ref (package-properties package) 'parameters)
      '()))

(define (package-parameter-value package parameter)
  (assq-ref (package-properties package)
            (package-parameter-property parameter)))

(define (lookup-package-parameter package name)
  (find (lambda (parameter)
          (string=? (package-parameter-name parameter) name))
        (package-parameters package)))

(define (set-package-parameter-value package name value)
  (let ((parameter (lookup-package-parameter package name))
        (location  (package-field-location package 'properties)))
    (unless parameter
      (raise (apply make-compound-condition
                    (formatted-message
                     (G_ "~a: no such package parameter")
                     name)
                    (if location
                        (list (condition
                               (&error-location (location location))))
                        '()))))
    (let* ((property (package-parameter-property parameter))
           (type     (package-parameter-type parameter))
           (value    ((parameter-type-string->value type) value)))
      (package/inherit package
        (properties
         (alist-cons property value
                     (alist-delete property (package-properties package)
                                   eq?)))))))

(define-syntax-rule (optionally property exp)
  (if (assq-ref (package-properties this-package) property)
      (list exp)
      '()))

;; g23: Change the parameter and package record to contain our parameters

;; Our parameter record should have the following:
;; - Parameter type
;; - Build systems it works with
;; - Transforms in the style of (case x) where x is the build system
