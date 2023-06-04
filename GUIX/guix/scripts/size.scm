;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix scripts size)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix combinators)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:export (profile?
            profile-file
            profile-self-size
            profile-closure-size
            store-profile

            guix-size))

;; Size profile of a store item.
(define-record-type <profile>
  (profile file self-size closure-size)
  profile?
  (file          profile-file)                 ;store item
  (self-size     profile-self-size)            ;size in bytes
  (closure-size  profile-closure-size))        ;size of dependencies in bytes

(define substitutable-path-info*
  (store-lift substitutable-path-info))

(define (file-size item)
  "Return the size in bytes of ITEM, resorting to information from substitutes
if ITEM is not in the store."
  (mlet %store-monad ((info (query-path-info* item)))
    (if info
        (return (path-info-nar-size info))
        (mlet %store-monad ((info (substitutable-path-info* (list item))))
          (match info
            ((info)
             ;; The nar size is an approximation, but a good one.
             (return (substitutable-nar-size info)))
            (()
             (leave (G_ "no available substitute information for '~a'~%")
                    item)))))))

(define profile-closure<?
  (match-lambda*
    ((($ <profile> name1 self1 total1)
      ($ <profile> name2 self2 total2))
     (< total1 total2))))

(define profile-self<?
  (match-lambda*
    ((($ <profile> name1 self1 total1)
      ($ <profile> name2 self2 total2))
     (< self1 self2))))

(define* (display-profile profile #:optional (port (current-output-port))
                          #:key (profile<? profile-closure<?))
  "Display PROFILE, a list of PROFILE objects, to PORT.  Sort entries
according to PROFILE<?."
  (define MiB (expt 2 20))

  (format port "~64a ~8a ~a\n"
          (G_ "store item") (G_ "total") (G_ "self"))
  (let ((whole (reduce + 0 (map profile-self-size profile))))
    (for-each (match-lambda
                (($ <profile> name self total)
                 (format port "~64a  ~6,1f  ~6,1f ~5,1f%\n"
                         name (/ total MiB) (/ self MiB)
                         (* 100. (/ self whole 1.)))))
              (sort profile (negate profile<?)))
    (format port (G_ "total: ~,1f MiB~%") (/ whole MiB 1.))))

(define display-profile*
  (lift display-profile %store-monad))

(define (substitutable-requisites store items)
  "Return the list of requisites of ITEMS based on information available in
substitutes."
  (let loop ((items  items)
             (result '()))
    (match items
      (()
       (delete-duplicates result))
      (items
       (let ((info (substitutable-path-info store
                                            (delete-duplicates items))))
         (loop (remove (lambda (item)             ;XXX: complexity
                         (member item result))
                       (append-map substitutable-references info))
               (append (append-map substitutable-references info)
                       result)))))))

(define (requisites* items)
  "Return as a monadic value the requisites of ITEMS, based either on the
information available in the local store or using information about
substitutes."
  (lambda (store)
    (let-values (((local missing)
                  (partition (cut valid-path? store <>) items)))
      (values (delete-duplicates
               (append (requisites store local)
                       (substitutable-requisites store missing)))
              store))))

(define (store-profile items)
  "Return as a monadic value a list of <profile> objects representing the
profile of ITEMS and their requisites."
  (mlet* %store-monad ((refs  (>>= (requisites* items)
                                   (lambda (refs)
                                     (return (delete-duplicates
                                              (append items refs))))))
                       (sizes (mapm %store-monad
                                    (lambda (item)
                                      (>>= (file-size item)
                                           (lambda (size)
                                             (return (cons item size)))))
                                    refs)))
    (define size-table
      (fold (lambda (pair result)
              (match pair
                ((item . size)
                 (vhash-cons item size result))))
            vlist-null sizes))

    (define (dependency-size item)
      (mlet %store-monad ((deps (requisites* (list item))))
        (foldm %store-monad
               (lambda (item total)
                 (return (+ (match (vhash-assoc item size-table)
                              ((_ . size) size))
                            total)))
               0
               (delete-duplicates (cons item deps)))))

    (mapm %store-monad
          (match-lambda
            ((item . size)
             (mlet %store-monad ((dependencies (dependency-size item)))
               (return (profile item size dependencies)))))
          sizes)))

(define* (ensure-store-item spec-or-item)
  "Return a store file name.  If SPEC-OR-ITEM is a store file name, return it
as is.  Otherwise, assume SPEC-OR-ITEM is a package output specification such
as \"guile:debug\" or \"gcc-4.8\" and return its store file name."
  (with-monad %store-monad
    (if (store-path? spec-or-item)
        (return spec-or-item)
        (let-values (((package output)
                      (specification->package+output spec-or-item)))
          (mlet %store-monad ((drv (package->derivation package)))
            ;; Note: we don't try building DRV like 'guix archive' does
            ;; because we don't have to since we can instead rely on
            ;; substitute meta-data.
            (return (derivation->output-path drv output)))))))


;;;
;;; Charts.
;;;

;; Autoload Guile-Charting.
;; XXX: Use this hack instead of #:autoload to avoid compilation errors.
;; See <http://bugs.gnu.org/12202>.
(module-autoload! (current-module)
                  '(charting) '(make-page-map))

(define (profile->page-map profiles file)
  "Write a 'page map' chart of PROFILES, a list of <profile> objects, to FILE,
the name of a PNG file."
  (define (strip name)
    (string-drop name (+ (string-length (%store-prefix)) 28)))

  (define data
    (fold2 (lambda (profile result offset)
             (match profile
               (($ <profile> name self)
                (let ((self (inexact->exact
                             (round (/ self (expt 2. 10))))))
                  (values `((,(strip name) ,offset . ,self)
                            ,@result)
                          (+ offset self))))))
           '()
           0
           (sort profiles
                 (match-lambda*
                   ((($ <profile> name1 self1 total1)
                     ($ <profile> name2 self2 total2))
                    (> total1 total2))))))

  ;; TRANSLATORS: This is the title of a graph, meaning that the graph
  ;; represents a profile of the store (the "store" being the place where
  ;; packages are stored.)
  (make-page-map (G_ "store profile") data
                 #:write-to-png file))


;;;
;;; Options.
;;;

(define (show-help)
  (display (G_ "Usage: guix size [OPTION]... PACKAGE|STORE-ITEM
Report the size of the PACKAGE or STORE-ITEM, with its dependencies.\n"))
  (display (G_ "
      --substitute-urls=URLS
                         fetch substitute from URLS if they are authorized"))
  ;; TRANSLATORS: "closure" and "self" must not be translated.
  (display (G_ "
      --sort=KEY         sort according to KEY--\"closure\" or \"self\""))
  (display (G_ "
  -m, --map-file=FILE    write to FILE a graphical map of disk usage"))
  (newline)
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-native-build-options-help)
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '("substitute-urls") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'substitute-urls
                                     (string-tokenize arg)
                                     (alist-delete 'substitute-urls result))
                         rest)))
        (option '("sort") #t #f
                (lambda (opt name arg result . rest)
                  (match arg
                    ("closure"
                     (alist-cons 'profile<? profile-closure<? result))
                    ("self"
                     (alist-cons 'profile<? profile-self<? result))
                    (_
                     (leave (G_ "~a: invalid sorting key~%") arg)))))
        (option '(#\m "map-file") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'map-file arg result)))
        (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix size")))
        %standard-native-build-options))

(define %default-options
  `((system . ,(%current-system))
    (profile<? . ,profile-self<?)))


;;;
;;; Entry point.
;;;

(define-command (guix-size . args)
  (category packaging)
  (synopsis "profile the on-disk size of packages")

  (with-error-handling
    (let* ((opts     (parse-command-line args %options (list %default-options)
                                         #:build-options? #f))
           (files    (filter-map (match-lambda
                                   (('argument . file) file)
                                   (_ #f))
                                 opts))
           (profile<? (assoc-ref opts 'profile<?))
           (map-file (assoc-ref opts 'map-file))
           (system   (assoc-ref opts 'system))
           (urls     (assoc-ref opts 'substitute-urls)))
      (match files
        (()
         (leave (G_ "missing store item argument\n")))
        ((files ..1)
         (leave-on-EPIPE
          ;; Turn off grafts because (1) substitute servers do not serve grafted
          ;; packages, and (2) they do not make any difference on the
          ;; resulting size.
          (parameterize ((%graft? #f))
            (with-store store
              (set-build-options store
                                 #:use-substitutes? #t
                                 #:substitute-urls urls)

              (run-with-store store
                (mlet* %store-monad ((items   (mapm %store-monad
                                                    ensure-store-item files))
                                     (profile (store-profile items)))
                  (if map-file
                      (begin
                        (profile->page-map profile map-file)
                        (return #t))
                      (display-profile* profile (current-output-port)
                                        #:profile<? profile<?)))
                #:system system)))))))))
