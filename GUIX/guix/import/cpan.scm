;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import cpan)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((ice-9 popen) #:select (open-pipe* close-pipe))
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (json)
  #:use-module (gcrypt hash)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store url-fetch))
  #:use-module ((guix import utils) #:select (factorize-uri))
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix derivations)
  #:export (cpan->guix-package
            metacpan-url->mirror-url
            %cpan-updater

            %metacpan-base-url))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of a CPAN
;;; module, using meta-data from metacpan.org.
;;;
;;; Code:

(define %metacpan-base-url
  ;; Base URL of the MetaCPAN API.
  (make-parameter "https://fastapi.metacpan.org/v1/"))

;; Dependency of a "release".
(define-json-mapping <cpan-dependency> make-cpan-dependency cpan-dependency?
  json->cpan-dependency
  (relationship cpan-dependency-relationship "relationship"
                string->symbol)                   ;requires | suggests
  (phase        cpan-dependency-phase "phase"
                string->symbol)          ;develop | configure | test | runtime
  (module       cpan-dependency-module)           ;string
  (version      cpan-dependency-version))         ;string

;; Release as returned by <https://fastapi.metacpan.org/v1/release/PKG>.
(define-json-mapping <cpan-release> make-cpan-release cpan-release?
  json->cpan-release
  (license      cpan-release-license)
  (author       cpan-release-author)
  (version      cpan-release-version "version"
                (match-lambda
                  ((? number? version)
                   ;; Version is sometimes not quoted in the module json, so
                   ;; it gets imported into Guile as a number, so convert it
                   ;; to a string (example: "X11-Protocol-Other").
                   (number->string version))
                  ((? string? version)
                   ;; Sometimes we get a "v" prefix.  Strip it.
                   (if (string-prefix? "v" version)
                       (string-drop version 1)
                       version))))
  (module       cpan-release-module "main_module") ;e.g., "Test::Script"
  (distribution cpan-release-distribution)         ;e.g., "Test-Script"
  (download-url cpan-release-download-url "download_url")
  (abstract     cpan-release-abstract "abstract")
  (home-page    cpan-release-home-page "resources"
                (match-lambda
                  (#f #f)
                  ((? unspecified?) #f)
                  ((lst ...) (assoc-ref lst "homepage"))))
  (dependencies cpan-release-dependencies "dependency"
                (lambda (vector)
                  (map json->cpan-dependency (vector->list vector)))))

(define string->license
  (match-lambda
   ;; List of valid values from https://metacpan.org/pod/CPAN::Meta::Spec.
   ;; Some licenses are excluded based on their absense from (guix licenses).
   ("agpl_3" 'agpl3)
   ;; apache_1_1
   ("apache_2_0" 'asl2.0)
   ;; artistic_1
   ("artistic_2" 'artistic2.0)
   ("bsd" 'bsd-3)
   ("freebsd" 'bsd-2)
   ;; gfdl_1_2
   ("gfdl_1_3" 'fdl1.3+)
   ("gpl_1" 'gpl1)
   ("gpl_2" 'gpl2)
   ("gpl_3" 'gpl3)
   ("lgpl_2_1" 'lgpl2.1)
   ("lgpl_3_0" 'lgpl3)
   ("mit" 'x11)
   ;; mozilla_1_0
   ("mozilla_1_1" 'mpl1.1)
   ("openssl" 'openssl)
   ("perl_5" 'perl-license)   ;GPL1+ and Artistic 1
   ("qpl_1_0" 'qpl)
   ;; ssleay
   ;; sun
   ("zlib" 'zlib)
   (#(x) (string->license x))
   (#(lst ...) `(list ,@(map string->license lst)))
   (_ #f)))

(define (module->name module)
  "Transform a 'module' name into a 'release' name"
  (regexp-substitute/global #f "::" module 'pre "-" 'post))

(define (module->dist-name module)
  "Return the base distribution module for a given module.  E.g. the 'ok'
module is distributed with 'Test::Simple', so (module->dist-name \"ok\") would
return \"Test-Simple\""
  (assoc-ref (json-fetch (string-append
                          (%metacpan-base-url) "/module/"
                          module
                          "?fields=distribution"))
             "distribution"))

(define (package->upstream-name package)
  "Return the CPAN name of PACKAGE."
  (let* ((properties (package-properties package))
         (upstream-name (and=> properties
                               (cut assoc-ref <> 'upstream-name))))
    (or upstream-name
        (match (package-source package)
          ((? origin? origin)
           (match (origin-uri origin)
             ((or (? string? url) (url _ ...))
              (match (string-match "([^/]*)-v?[0-9\\.]+" url)
                (#f #f)
                (m (match:substring m 1))))
             (_ #f)))
          (_ #f)))))

(define (cpan-fetch name)
  "Return a <cpan-release> record for Perl module MODULE,
or #f on failure.  MODULE should be the distribution name, such as
\"Test-Script\" for the \"Test::Script\" module."
  ;; This API always returns the latest release of the module.
  (and=> (json-fetch (string-append (%metacpan-base-url) "/release/"
                                    name))
         json->cpan-release))

(define (cpan-home name)
  (string-append "https://metacpan.org/release/" name))

(define (metacpan-url->mirror-url url)
  "Replace 'https://cpan.metacpan.org' in URL with 'mirror://cpan'."
  (regexp-substitute/global #f "http[s]?://cpan.metacpan.org"
                            url
                            'pre "mirror://cpan" 'post))

(define cpan-source-url
  (compose metacpan-url->mirror-url cpan-release-download-url))

(define (perl-package)
  "Return the 'perl' package.  This is a lazy reference so that we don't
depend on (gnu packages perl)."
  (module-ref (resolve-interface '(gnu packages perl)) 'perl))

(define %corelist
  (delay
    (let* ((perl (with-store store
                   (derivation->output-path
                    (package-derivation store (perl-package)))))
           (core (string-append perl "/bin/corelist")))
      (and (access? core X_OK)
           core))))

(define core-module?
  (let ((rx (make-regexp
             (string-append "released with perl v?([0-9\\.]*)"
                            "(.*and removed from v?([0-9\\.]*))?"))))
    (lambda (name)
      (define perl-version
        (package-version (perl-package)))

      (define (version-between? lower version upper)
        (and (version>=? version lower)
             (or (not upper)
                 (version>? upper version))))
      (and (force %corelist)
           (parameterize ((current-error-port (%make-void-port "w")))
             (let* ((corelist (open-pipe* OPEN_READ (force %corelist) name)))
               (let loop ()
                 (let ((line (read-line corelist)))
                   (if (eof-object? line)
                       (begin (close-pipe corelist) #f)
                       (or (and=> (regexp-exec rx line)
                                  (lambda (m)
                                    (let ((first (match:substring m 1))
                                          (last  (match:substring m 3)))
                                      (version-between?
                                       first perl-version last))))
                           (loop)))))))))))

(define (cpan-name->downstream-name name)
  "Return the Guix package name corresponding to NAME."
  (if (string-prefix? "perl-" name)
      (string-downcase name)
      (string-append "perl-" (string-downcase name))))

(define (cran-dependency->upstream-input dependency)
  "Return the <upstream-input> corresponding to DEPENDENCY, or #f if
DEPENDENCY denotes an implicit or otherwise unnecessary dependency."
  (match (cpan-dependency-module dependency)
    ("perl" #f)                                   ;implicit dependency
    (module
     (let ((type (match (cpan-dependency-phase dependency)
                   ((or 'configure 'build 'test)
                    ;; "runtime" may also be needed here.  See
                    ;; https://metacpan.org/pod/CPAN::Meta::Spec#Phases,
                    ;; which says they are required during
                    ;; building.  We have not yet had a need for
                    ;; cross-compiled Perl modules, however, so
                    ;; we leave it out.
                    'native)
                   ('runtime
                    'propagated)
                   (_
                    #f))))
       (and type
            (not (core-module? module))           ;expensive call!
            (upstream-input
             (name (module->dist-name module))
             (downstream-name (cpan-name->downstream-name name))
             (type type)))))))

(define (cpan-module-inputs release)
  "Return the list of <upstream-input> for dependencies of RELEASE, a
<cpan-release>."
  (define (upstream-input<? a b)
    (string<? (upstream-input-downstream-name a)
              (upstream-input-downstream-name b)))

  (sort (delete-duplicates
         (filter-map cran-dependency->upstream-input
                     (cpan-release-dependencies release)))
        upstream-input<?))

(define (cpan-module->sexp release)
  "Return the 'package' s-expression for a CPAN module from the release data
in RELEASE, a <cpan-release> record."
  (define name
    (cpan-release-distribution release))

  (define version (cpan-release-version release))
  (define source-url (cpan-source-url release))

  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       `((,input-type (list ,@(map (compose string->symbol
                                            upstream-input-downstream-name)
                                   inputs)))))))

  (let ((tarball (with-store store
                   (download-to-store store source-url)))
        (inputs (cpan-module-inputs release)))
    `(package
       (name ,(cpan-name->downstream-name name))
       (version ,version)
       (source (origin
                 (method url-fetch)
                 (uri (string-append ,@(factorize-uri source-url version)))
                 (sha256
                  (base32
                   ,(bytevector->nix-base32-string (file-sha256 tarball))))))
       (build-system perl-build-system)
       ,@(maybe-inputs 'native-inputs
                       (filter (upstream-input-type-predicate 'native)
                               inputs))
       ,@(maybe-inputs 'propagated-inputs
                       (filter (upstream-input-type-predicate 'propagated)
                               inputs))
       (home-page ,(cpan-home name))
       (synopsis ,(cpan-release-abstract release))
       (description fill-in-yourself!)
       (license ,(string->license (cpan-release-license release))))))

(define (cpan->guix-package module-name)
  "Fetch the metadata for PACKAGE-NAME from metacpan.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (let ((release (cpan-fetch (module->name module-name))))
    (and=> release cpan-module->sexp)))

(define cpan-package?
  (let ((cpan-rx (make-regexp (string-append "("
                                             "mirror://cpan" "|"
                                             "https?://www.cpan.org" "|"
                                             "https?://cpan.metacpan.org"
                                             ")"))))
    (url-predicate (cut regexp-exec cpan-rx <>))))

(define* (latest-release package #:key (version #f))
  "Return an <upstream-source> for the latest release of PACKAGE."
  (when version
    (raise
     (formatted-message
      (G_ "~a updater doesn't support updating to a specific version, sorry.")
      "cpan")))
  (match (cpan-fetch (package->upstream-name package))
    (#f #f)
    (release
     (let ((core-inputs
            (match (package-direct-inputs package)
              (((_ inputs _ ...) ...)
               (filter-map (match-lambda
                             ((and (? package?)
                                   (? cpan-package?)
                                   (= package->upstream-name
                                      (? core-module? name)))
                              name)
                             (else #f))
                           inputs)))))
       ;; Warn about inputs that are part of perl's core
       (unless (null? core-inputs)
         (for-each (lambda (module)
                     (warning (G_ "input '~a' of ~a is in Perl core~%")
                              module (package-name package)))
                   core-inputs)))
     (let ((version (cpan-release-version release))
           (url     (cpan-source-url release)))
       (upstream-source
        (package (package-name package))
        (version version)
        (urls (list url))
        (inputs (cpan-module-inputs release)))))))

(define %cpan-updater
  (upstream-updater
   (name 'cpan)
   (description "Updater for CPAN packages")
   (pred cpan-package?)
   (import latest-release)))
