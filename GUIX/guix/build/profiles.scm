;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017-2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build profiles)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (guix search-paths)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:re-export (symlink-relative)                  ;for convenience
  #:export (ensure-writable-directory
            build-profile))

;;; Commentary:
;;;
;;; Build a user profile (essentially the union of all the installed packages)
;;; with its associated meta-data.
;;;
;;; Code:

(define (abstract-profile profile)
  "Return a procedure that replaces PROFILE in VALUE with a reference to the
'GUIX_PROFILE' environment variable.  This allows users to specify what the
user-friendly name of the profile is, for instance ~/.guix-profile rather than
/gnu/store/...-profile."
  (let ((replacement (string-append "${GUIX_PROFILE:-" profile "}"))
        (crop        (cute string-drop <> (string-length profile))))
    (match-lambda
      ((search-path . value)
       (match (search-path-specification-separator search-path)
         (#f
          (cons search-path
                (string-append replacement (crop value))))
         ((? string? separator)
          (let ((items (string-tokenize* value separator)))
            (cons search-path
                  (string-join (map (lambda (str)
                                      (string-append replacement (crop str)))
                                    items)
                               separator)))))))))

(define (write-environment-variable-definition port)
  "Write the given environment variable definition to PORT."
  (match-lambda
    ((search-path . value)
     (display (search-path-definition search-path value #:kind 'prefix)
              port)
     (newline port))))

(define (build-etc/profile output search-paths)
  "Build the 'OUTPUT/etc/profile' shell file containing environment variable
definitions for all the SEARCH-PATHS."
  (define file
    (string-append output "/etc/profile"))

  (mkdir-p (dirname file))
  (when (file-exists? file)
    (delete-file file))

  (call-with-output-file file
    (lambda (port)
      ;; The use of $GUIX_PROFILE described below is not great.  Another
      ;; option would have been to use "$1" and have users run:
      ;;
      ;;   source ~/.guix-profile/etc/profile ~/.guix-profile
      ;;
      ;; However, when 'source' is used with no arguments, $1 refers to the
      ;; first positional parameter of the calling script, so we cannot rely
      ;; on it.
      (display "\
# Source this file to define all the relevant environment variables in Bash
# for this profile.  You may want to define the 'GUIX_PROFILE' environment
# variable to point to the \"visible\" name of the profile, like this:
#
#  GUIX_PROFILE=/path/to/profile ; \\
#  source /path/to/profile/etc/profile
#
# When GUIX_PROFILE is undefined, the various environment variables refer
# to this specific profile generation.
\n" port)
      (let ((variables (evaluate-search-paths search-paths
                                              (list output))))
        (for-each (write-environment-variable-definition port)
                  (map (abstract-profile output) variables))))))

(define* (ensure-writable-directory directory
                                    #:key (symlink symlink))
  "Ensure DIRECTORY exists and is writable.  If DIRECTORY is currently a
symlink (to a read-only directory in the store), then delete the symlink and
instead make DIRECTORY a \"real\" directory containing symlinks."
  (define (absolute? file)
    (string-prefix? "/" file))

  (define (unsymlink link)
    (let* ((target (match (readlink link)
                     ((? absolute? target)
                      target)
                     ((? string? relative)
                      (string-append (dirname link) "/" relative))))
           ;; TARGET might itself be a symlink, so append "/" to make sure
           ;; 'scandir' enters it.
           (files  (scandir (string-append target "/")
                            (negate (cut member <> '("." ".."))))))
      (delete-file link)
      (mkdir link)
      (for-each (lambda (file)
                  (symlink (string-append target "/" file)
                           (string-append link "/" file)))
                files)))

  (catch 'system-error
    (lambda ()
      (mkdir directory))
    (lambda args
      (let ((errno (system-error-errno args)))
        (if (= errno EEXIST)
            (let ((stat (lstat directory)))
              (case (stat:type stat)
                ((symlink)
                 ;; "Unsymlink" DIRECTORY so that it is writable.
                 (unsymlink directory))
                ((directory)
                 #t)
                (else
                 (error "cannot mkdir because a same-named file exists"
                        directory))))
            (apply throw args))))))

(define (manifest-sexp->inputs+search-paths manifest)
  "Parse MANIFEST, an sexp as produced by 'manifest->gexp', and return two
values: the list of store items of its manifest entries, and the list of
search path specifications."
  (define-syntax let-fields
    (syntax-rules ()
      ;; Bind the fields NAME of LST to same-named variables in the lexical
      ;; scope of BODY.
      ((_ lst (name rest ...) body ...)
       (let ((name (match (assq 'name lst)
                     ((_ value) value)
                     (#f '()))))
         (let-fields lst (rest ...) body ...)))
      ((_ lst () body ...)
       (begin body ...))))

  (match manifest                            ;this must match 'manifest->gexp'
    (('manifest ('version (or 3 4))
                ('packages (entries ...)))
     (let loop ((entries entries)
                (inputs '())
                (search-paths '()))
       (match entries
         (((name version output item fields ...) . rest)
          (let ((paths search-paths))
            (let-fields fields (propagated-inputs search-paths)
              (loop (append rest propagated-inputs) ;breadth-first traversal
                    (cons item inputs)
                    (append search-paths paths)))))
         ((('repeated name version item) . rest)
          (loop rest inputs search-paths))
         (()
          (values (reverse inputs)
                  (delete-duplicates
                   (cons* $PATH
                          $GUIX_EXTENSIONS_PATH
                          (map sexp->search-path-specification
                               (reverse search-paths)))))))))))

(define* (build-profile output manifest
                        #:key (extra-inputs '()) (symlink symlink))
  "Build a user profile from MANIFEST, an sexp, and EXTRA-INPUTS, a list of
store items, in directory OUTPUT, using SYMLINK to create symlinks.  Create
OUTPUT/etc/profile with Bash definitions for all the variables listed in the
search paths of MANIFEST's entries."
  (define manifest-file
    (string-append output "/manifest"))

  (let-values (((inputs search-paths)
                (manifest-sexp->inputs+search-paths manifest)))

    ;; Make the symlinks.
    (union-build output (append extra-inputs inputs)
                 #:symlink symlink
                 #:log-port (%make-void-port "w"))

    ;; If one of the INPUTS provides a '/manifest' file, delete it.  That can
    ;; happen if MANIFEST contains something such as a Guix instance, which is
    ;; ultimately built as a profile.
    (when (file-exists? manifest-file)
      (delete-file manifest-file))

    ;; Store meta-data.
    (call-with-output-file manifest-file
      (lambda (p)
        (display "\
;; This file was automatically generated and is for internal use only.
;; It cannot be passed to the '--manifest' option.
;; Run 'guix package --export-manifest' if you want to export a file
;; suitable for '--manifest'.\n\n"
                 p)
        (pretty-print manifest p)))

    ;; Make sure we can write to 'OUTPUT/etc'.  'union-build' above could have
    ;; made 'etc' a symlink to a read-only sub-directory in the store so we
    ;; need to work around that.
    (ensure-writable-directory (string-append output "/etc")
                               #:symlink symlink)

    ;; Write 'OUTPUT/etc/profile'.
    (build-etc/profile output search-paths)))

;;; Local Variables:
;;; eval: (put 'let-fields 'scheme-indent-function 2)
;;; End:

;;; profile.scm ends here
