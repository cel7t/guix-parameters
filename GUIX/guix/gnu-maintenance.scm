;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix gnu-maintenance)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module ((guix http-client) #:hide (open-socket-for-uri))
  ;; not required in many cases, so autoloaded to reduce start-up costs.
  #:autoload   (guix download) (%mirrors)
  #:use-module (guix ftp-client)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix memoization)
  #:use-module (guix records)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:autoload   (guix import utils) (false-if-networking-error)
  #:autoload   (zlib) (call-with-gzip-input-port)
  #:autoload   (htmlprag) (html->sxml)            ;from Guile-Lib
  #:export (gnu-package-name
            gnu-package-mundane-name
            gnu-package-copyright-holder
            gnu-package-savannah
            gnu-package-fsd
            gnu-package-language
            gnu-package-logo
            gnu-package-doc-category
            gnu-package-doc-summary
            gnu-package-doc-description
            gnu-package-doc-urls
            gnu-package-download-url

            official-gnu-packages
            find-package
            gnu-package?

            uri-mirror-rewrite

            release-file?
            releases
            import-release
            gnu-release-archive-types
            gnu-package-name->name+version

            %gnu-updater
            %gnu-ftp-updater
            %savannah-updater
            %sourceforge-updater
            %xorg-updater
            %kernel.org-updater
            %generic-html-updater))

;;; Commentary:
;;;
;;; Code for dealing with the maintenance of GNU packages, such as
;;; auto-updates.
;;;
;;; Code:


;;;
;;; List of GNU packages.
;;;

(define %gnumaint-base-url
  "https://web.cvs.savannah.gnu.org/viewvc/*checkout*/www/www/prep/gnumaint/")

(define %package-list-url
  (string->uri
   (string-append %gnumaint-base-url "rec/gnupackages.rec")))

(define %package-description-url
  ;; This file contains package descriptions in recutils format.
  ;; See <https://lists.gnu.org/archive/html/guix-devel/2013-10/msg00071.html>
  ;; and <https://lists.gnu.org/archive/html/guix-devel/2018-06/msg00362.html>.
  (string->uri
   (string-append %gnumaint-base-url "rec/pkgblurbs.rec")))

(define-record-type* <gnu-package-descriptor>
  gnu-package-descriptor
  make-gnu-package-descriptor

  gnu-package-descriptor?

  (name             gnu-package-name)
  (mundane-name     gnu-package-mundane-name)
  (copyright-holder gnu-package-copyright-holder)
  (savannah         gnu-package-savannah)
  (fsd              gnu-package-fsd)
  (language         gnu-package-language)         ; list of strings
  (logo             gnu-package-logo)
  (doc-category     gnu-package-doc-category)
  (doc-summary      gnu-package-doc-summary)
  (doc-description  gnu-package-doc-description)  ; taken from 'pkgdescr.txt'
  (doc-urls         gnu-package-doc-urls)         ; list of strings
  (download-url     gnu-package-download-url))

(define* (official-gnu-packages
          #:optional (fetch http-fetch/cached))
  "Return a list of records, which are GNU packages.  Use FETCH,
to fetch the list of GNU packages over HTTP."
  (define (read-records port)
    ;; Return a list of alists.  Each alist contains fields of a GNU
    ;; package.
    (let loop ((alist  (recutils->alist port))
               (result '()))
      (if (null? alist)
          (reverse result)
          (loop (recutils->alist port)

                ;; Ignore things like "%rec" (info "(recutils) Record
                ;; Descriptors").
                (if (assoc-ref alist "package")
                    (cons alist result)
                    result)))))

  (define official-description
    (let ((db (read-records (fetch %package-description-url #:text? #t))))
      (lambda (name)
        ;; Return the description found upstream for package NAME, or #f.
        (and=> (find (lambda (alist)
                       (equal? name (assoc-ref alist "package")))
                     db)
               (lambda (record)
                 (let ((field (assoc-ref record "blurb")))
                   ;; The upstream description file uses "redirect PACKAGE" as
                   ;; a blurb in cases where the description of the two
                   ;; packages should be considered the same (e.g., GTK+ has
                   ;; "redirect gnome".)  This is usually not acceptable for
                   ;; us because we prefer to have distinct descriptions in
                   ;; such cases.  Thus, ignore the 'blurb' field when that
                   ;; happens.
                   (and field
                        (not (string-prefix? "redirect " field))
                        field)))))))

  (map (lambda (alist)
         (let ((name (assoc-ref alist "package")))
           (alist->record `(("description" . ,(official-description name))
                            ,@alist)
                          make-gnu-package-descriptor
                          (list "package" "mundane_name" "copyright_holder"
                                "savannah" "fsd" "language" "logo"
                                "doc_category" "doc_summary" "description"
                                "doc_url"
                                "download_url")
                          '("doc_url" "language"))))
       (let* ((port (fetch %package-list-url #:text? #t))
              (lst  (read-records port)))
         (close-port port)
         lst)))

(define (find-package name)
  "Find GNU package called NAME and return it.  Return #f if it was not
found."
  (find (lambda (package)
          (string=? name (gnu-package-name package)))
        (official-gnu-packages)))

(define gnu-package?
  (let ((official-gnu-packages (memoize official-gnu-packages)))
    (mlambdaq (package)
      "Return true if PACKAGE is a GNU package.  This procedure may access the
network to check in GNU's database."
      (define (mirror-type url)
        (let ((uri (string->uri url)))
          (and (eq? (uri-scheme uri) 'mirror)
               (cond
                ((member (uri-host uri)
                         '("gnu" "gnupg" "gcc" "gnome"))
                 ;; Definitely GNU.
                 'gnu)
                ((equal? (uri-host uri) "cran")
                 ;; Possibly GNU: mirror://cran could be either GNU R itself
                 ;; or a non-GNU package.
                 #f)
                (else
                 ;; Definitely non-GNU.
                 'non-gnu)))))

      (define (gnu-home-page? package)
        (letrec-syntax ((>> (syntax-rules ()
                              ((_ value proc)
                               (and=> value proc))
                              ((_ value proc rest ...)
                               (and=> value
                                      (lambda (next)
                                        (>> (proc next) rest ...)))))))
          (>> package package-home-page
              string->uri uri-host
              (lambda (host)
                (member host '("www.gnu.org" "gnu.org"))))))

      (or (gnu-home-page? package)
          (match (package-source package)
            ((? origin? origin)
             (let ((url  (origin-uri origin))
                   (name (package-upstream-name package)))
               (case (and (string? url) (mirror-type url))
                 ((gnu) #t)
                 ((non-gnu) #f)
                 (else
                  (and (member name (map gnu-package-name (official-gnu-packages)))
                       #t)))))
            (_ #f))))))


;;;
;;; Latest FTP release.
;;;

(define (ftp-server/directory package)
  "Return the FTP server and directory where PACKAGE's tarball are stored."
  (let ((name (package-upstream-name package)))
    (values (or (assoc-ref (package-properties package) 'ftp-server)
                "ftp.gnu.org")
            (or (assoc-ref (package-properties package) 'ftp-directory)
                (string-append "/gnu/" name)))))

(define %tarball-rx
  ;; The .zip extensions is notably used for freefont-ttf.
  ;; The "-src" pattern is for "TeXmacs-1.0.7.9-src.tar.gz".
  ;; The "-gnu[0-9]" pattern is for "icecat-38.4.0-gnu1.tar.bz2".
  ;; Accept underscores as in "PKG_1.2.tar.gz" for some non-GNU packages.
  ;; Accept 'v' or 'V' prefix as in 'PKG-v2.3.tgz'.
  (make-regexp "^([^.]+)[-_][vV]?([0-9]|[^-])+(-(src|[sS]ource|gnu[0-9]))?\\.(tar\\.|tgz|zip$)"))

(define %alpha-tarball-rx
  (make-regexp "^.*-.*[0-9](-|~|\\.)?(alpha|beta|rc|RC|cvs|svn|git)-?[0-9\\.]*\\.tar\\."))

(define (release-file? project file)
  "Return #f if FILE is not a release tarball of PROJECT, otherwise return
true."
  (and (not (member (file-extension file)
                    '("sig" "sign" "asc"
                      "md5sum" "sha1sum" "sha256sum")))
       (and=> (regexp-exec %tarball-rx file)
              (lambda (match)
                ;; Filter out unrelated files, like `guile-www-1.1.1'.
                ;; Case-insensitive for things like "TeXmacs" vs. "texmacs".
                ;; The "-src" suffix is for "freefont-src-20120503.tar.gz".
                (and=> (match:substring match 1)
                       (lambda (name)
                         (or (string-ci=? name project)
                             (string-ci=? name
                                          (string-append project
                                                         "-src")))))))
       (not (regexp-exec %alpha-tarball-rx file))
       (let ((s (tarball-sans-extension file)))
         (regexp-exec %package-name-rx s))))

(define (tarball->version tarball)
  "Return the version TARBALL corresponds to.  TARBALL is a file name like
\"coreutils-8.23.tar.xz\"."
  (let-values (((name version)
                (gnu-package-name->name+version
                 (tarball-sans-extension tarball))))
    version))

(define* (releases project
                   #:key
                   (server "ftp.gnu.org")
                   (directory (string-append "/gnu/" project)))
  "Return the list of <upstream-release> of PROJECT as a list of release
name/directory pairs."
  ;; TODO: Parse something like fencepost.gnu.org:/gd/gnuorg/packages-ftp.
  (define conn (ftp-open server))

  (let loop ((directories (list directory))
             (result      '()))
    (match directories
      (()
       (ftp-close conn)
       (coalesce-sources result))
      ((directory rest ...)
       (let* ((files   (ftp-list conn directory))
              (subdirs (filter-map (match-lambda
                                     ((name 'directory . _) name)
                                     (_ #f))
                                   files)))
         (define (file->url file)
           (string-append "ftp://" server directory "/" file))

         (define (file->source file)
           (let ((url (file->url file)))
             (upstream-source
              (package project)
              (version (tarball->version file))
              (urls (list url))
              (signature-urls (list (string-append url ".sig"))))))

         (loop (append (map (cut string-append directory "/" <>)
                            subdirs)
                       rest)
               (append
                ;; Filter out signatures, deltas, and files which
                ;; are potentially not releases of PROJECT--e.g.,
                ;; in /gnu/guile, filter out guile-oops and
                ;; guile-www; in mit-scheme, filter out binaries.
                (filter-map (match-lambda
                              ((file 'file . _)
                               (and (release-file? project file)
                                    (file->source file)))
                              (_ #f))
                            files)
                result)))))))

(define* (import-ftp-release project
                             #:key
                             (version #f)
                             (server "ftp.gnu.org")
                             (directory (string-append "/gnu/" project))
                             (file->signature (cut string-append <> ".sig")))
  "Return an <upstream-source> for the latest release of PROJECT on SERVER
under DIRECTORY, or #f. Optionally include a VERSION string to fetch a specific version.

Use FTP-OPEN and FTP-CLOSE to open (resp. close) FTP connections; this can be
useful to reuse connections.

FILE->SIGNATURE must be a procedure; it is passed a source file URL and must
return the corresponding signature URL, or #f it signatures are unavailable."
  (define (latest a b)
    (if (version>? a b) a b))

  (define (latest-release a b)
    (if (version>? (upstream-source-version a) (upstream-source-version b))
        a b))

  (define patch-directory-name?
    ;; Return #t for patch directory names such as 'bash-4.2-patches'.
    (cut string-suffix? "patches" <>))

  (define conn (ftp-open server #:timeout 5))

  (define (file->url directory file)
    (string-append "ftp://" server directory "/" file))

  (define (file->source directory file)
    (let ((url (file->url directory file)))
      (upstream-source
       (package project)
       (version (tarball->version file))
       ;; uri-mirror-rewrite: Don't turn nice mirror:// URIs into ftp://
       ;; URLs during "guix refresh -u".
       (urls (list (uri-mirror-rewrite url)))
       (signature-urls (match (file->signature url)
                         (#f #f)
                         (sig (list (uri-mirror-rewrite sig))))))))

  (let loop ((directory directory)
             (result    #f))
    (let* ((entries (catch 'ftp-error
                      (lambda _ (ftp-list conn directory))
                      (const '())))

           ;; Filter out things like /gnupg/patches.  Filter out "w32"
           ;; directories as found on ftp.gnutls.org.
           (subdirs (filter-map (match-lambda
                                  (((? patch-directory-name? dir)
                                    'directory . _)
                                   #f)
                                  (("w32" 'directory . _)
                                   #f)
                                  (("unstable" 'directory . _)
                                   ;; As seen at ftp.gnupg.org/gcrypt/pinentry.
                                   #f)
                                  ((directory 'directory . _)
                                   directory)
                                  (_ #f))
                                entries))

           ;; Whether or not SUBDIRS is empty, compute the latest releases
           ;; for the current directory.  This is necessary for packages
           ;; such as 'sharutils' that have a sub-directory that contains
           ;; only an older release.
           (releases (filter-map (match-lambda
                                   ((file 'file . _)
                                    (and (release-file? project file)
                                         (file->source directory file)))
                                   (_ #f))
                                 entries)))

      ;; Assume that SUBDIRS correspond to versions, and jump into the
      ;; one with the highest version number.
      (let* ((release  (if version
                           (find (lambda (upstream)
                                   (string=? (upstream-source-version upstream) version))
                                 (coalesce-sources releases))
                           (reduce latest-release #f
                                   (coalesce-sources releases))))
             (result   (if (and result release)
                           (latest-release release result)
                           (or release result)))
             (target   (reduce latest #f subdirs)))
        (if target
            (loop (string-append directory "/" target)
                  result)
            (begin
              (ftp-close conn)
              result))))))

(define* (import-release package
                         #:key
                         (version #f)
                         (server "ftp.gnu.org")
                         (directory (string-append "/gnu/" package)))
  "Return the <upstream-source> for the latest version of PACKAGE or #f.
PACKAGE must be the canonical name of a GNU package. Optionally include a
VERSION string to fetch a specific version."
  (import-ftp-release package
                      #:version version
                      #:server server
                      #:directory directory))

(define-syntax-rule (false-if-ftp-error exp)
  "Return #f if an FTP error is raise while evaluating EXP; return the result
of EXP otherwise."
  (catch 'ftp-error
    (lambda ()
      exp)
    (lambda (key port . rest)
      (if (ftp-connection? port)
          (ftp-close port)
          (close-port port))
      #f)))

(define* (import-release* package #:key (version #f))
  "Like 'import-release', but (1) take a <package> object, and (2) ignore FTP
errors that might occur when PACKAGE is not actually a GNU package, or not
hosted on ftp.gnu.org, or not under that name (this is the case for
\"emacs-auctex\", for instance.)"
  (let-values (((server directory)
                (ftp-server/directory package)))
    (false-if-ftp-error (import-release (package-upstream-name package)
                                        #:version version
                                        #:server server
                                        #:directory directory))))


;;;
;;; Latest HTTP release.
;;;

(define (html-links sxml)
  "Return the list of links found in SXML, the SXML tree of an HTML page."
  (let loop ((sxml sxml)
             (links '()))
    (match sxml
      (('a ('@ attributes ...) body ...)
       (match (assq 'href attributes)
         (#f          (fold loop links body))
         (('href url) (fold loop (cons url links) body))))
      ((tag ('@ _ ...) body ...)
       (fold loop links body))
      ((tag body ...)
       (fold loop links body))
      (_
       links))))

(define* (import-html-release package
                              #:key
                              (version #f)
                              (base-url "https://kernel.org/pub")
                              (directory (string-append "/" package))
                              file->signature)
  "Return an <upstream-source> for the latest release of PACKAGE (a string) on
SERVER under DIRECTORY, or #f. Optionally include a VERSION string to fetch a
specific version.

BASE-URL should be the URL of an HTML page, typically a directory listing as
found on 'https://kernel.org/pub'.

When FILE->SIGNATURE is omitted or #f, guess the detached signature file name,
if any.  Otherwise, FILE->SIGNATURE must be a procedure; it is passed a source
file URL and must return the corresponding signature URL, or #f it signatures
are unavailable."
  (let* ((uri   (string->uri (if (string-null? directory)
                                 base-url
                                 (string-append base-url directory "/"))))
         (port  (http-fetch/cached uri #:ttl 3600))
         (sxml  (html->sxml port))
         (links (delete-duplicates (html-links sxml))))
    (define (file->signature/guess url)
      (let ((base (basename url)))
        (any (lambda (link)
               (any (lambda (extension)
                      (and (string=? (string-append base extension)
                                     (basename link))
                           (string-append url extension)))
                    '(".asc" ".sig" ".sign")))
             links)))

    (define (url->release url)
      (let* ((base (basename url))
             (base-url (string-append base-url directory))
             (url  (cond ((and=> (string->uri url) uri-scheme) ;full URL?
                          url)
                         ;; full URL, except for URI scheme.  Reuse the URI
                         ;; scheme of the document that contains the link.
                         ((string-prefix? "//" url)
                          (string-append
                           (symbol->string (uri-scheme (string->uri base-url)))
                           ":" url))
                         ((string-prefix? "/" url) ;absolute path?
                          (let ((uri (string->uri base-url)))
                            (uri->string
                             (build-uri (uri-scheme uri)
                                        #:host (uri-host uri)
                                        #:port (uri-port uri)
                                        #:path url))))

                         ;; URL is a relative path and BASE-URL may or may not
                         ;; end in slash.
                         ((string-suffix? "/" base-url)
                          (string-append base-url url))
                         (else
                          ;; If DIRECTORY is non-empty, assume BASE-URL
                          ;; denotes a directory; otherwise, assume BASE-URL
                          ;; denotes a file within a directory, and that URL
                          ;; is relative to that directory.
                          (string-append (if (string-null? directory)
                                             (dirname base-url)
                                             base-url)
                                         "/" url)))))
        (and (release-file? package base)
             (let ((version (tarball->version base)))
               (upstream-source
                (package package)
                (version version)
                ;; uri-mirror-rewrite: Don't turn nice mirror:// URIs into ftp://
                ;; URLs during "guix refresh -u".
                (urls (list (uri-mirror-rewrite url)))
                (signature-urls
                 (and=> ((or file->signature file->signature/guess) url)
                        (lambda (url) (list (uri-mirror-rewrite url))))))))))

    (define candidates
      (filter-map url->release links))

    (close-port port)
    (match candidates
      (() #f)
      ((first . _)
       (if version
           ;; find matching release version and return it
           (find (lambda (upstream)
                   (string=? (upstream-source-version upstream) version))
                 (coalesce-sources candidates))
           ;; Select the most recent release and return it.
           (reduce (lambda (r1 r2)
                         (if (version>? (upstream-source-version r1)
                                        (upstream-source-version r2))
                             r1 r2))
                       first
                       (coalesce-sources candidates)))))))


;;;
;;; Updaters.
;;;

(define %gnu-file-list-uri
  ;; URI of the file list for ftp.gnu.org.
  (string->uri "https://ftp.gnu.org/find.txt.gz"))

(define ftp.gnu.org-files
  (mlambda ()
    "Return the list of files available at ftp.gnu.org."

    ;; XXX: Memoize the whole procedure to work around the fact that
    ;; 'http-fetch/cached' caches the gzipped version.

    (define (trim-leading-components str)
      ;; Trim the leading ".", if any, in "./gnu/foo".
      (string-trim str (char-set #\.)))

    (define (string->lines str)
      (string-tokenize str (char-set-complement (char-set #\newline))))

    ;; Since https://ftp.gnu.org honors 'If-Modified-Since', the hard-coded
    ;; TTL can be relatively short.
    (let ((port (http-fetch/cached %gnu-file-list-uri #:ttl (* 15 60))))
      (map trim-leading-components
           (call-with-gzip-input-port port
             (compose string->lines get-string-all))))))

(define* (import-gnu-release package #:key (version #f))
  "Return the latest release of PACKAGE, a GNU package available via
ftp.gnu.org. Optionally include a VERSION string to fetch a specific version.

This method does not rely on FTP access at all; instead, it browses the file
list available from %GNU-FILE-LIST-URI over HTTP(S)."
  (define archive-type
    (package-archive-type package))

  (define (better-tarball? tarball1 tarball2)
    (string=? (file-extension tarball1) archive-type))

  (define (find-latest-tarball-version tarballs)
    (fold (lambda (file1 file2)
            (if (and file2
                     (version>? (tarball-sans-extension (basename file2))
                                (tarball-sans-extension (basename file1))))
                file2
                file1))
          #f
          tarballs))

  (let-values (((server directory)
                (ftp-server/directory package))
               ((name)
                (package-upstream-name package)))
    (let* ((files    (ftp.gnu.org-files))
           ;; select tarballs for this package
           (relevant (filter (lambda (file)
                               (and (string-prefix? "/gnu" file)
                                    (string-contains file directory)
                                    (release-file? name (basename file))))
                             files))
           ;; find latest version
           (version (or version
                        (and (not (null? relevant))
                             (tarball->version
                              (find-latest-tarball-version relevant)))))
           ;; find tarballs matching this version
           (tarballs (filter (lambda (file)
                               (string=? version (tarball->version file)))
                             relevant)))
    (match tarballs
           (() #f)
           (_
            (upstream-source
             (package name)
             (version version)
             (urls (map (lambda (file)
                          (string-append "mirror://gnu/"
                                         (string-drop file
                                                      (string-length "/gnu/"))))
                       ;; Sort so that the tarball with the same compression
                       ;; format as currently used in PACKAGE comes first.
                       (sort tarballs better-tarball?)))
             (signature-urls (map (cut string-append <> ".sig") urls))))))))

(define %package-name-rx
  ;; Regexp for a package name, e.g., "foo-X.Y".  Since TeXmacs uses
  ;; "TeXmacs-X.Y-src", the `-src' suffix is allowed.
  (make-regexp "^(.*)[-_][vV]?(([0-9]|\\.)+)(-src|\\.src|\\.orig)?"))

(define (gnu-package-name->name+version name+version)
  "Return the package name and version number extracted from NAME+VERSION."
  (let ((match (regexp-exec %package-name-rx name+version)))
    (if (not match)
        (values name+version #f)
        (values (match:substring match 1) (match:substring match 2)))))

(define gnome-package?
  (url-prefix-predicate "mirror://gnome/"))

(define (pure-gnu-package? package)
  "Return true if PACKAGE is a non-Emacs and non-GNOME GNU package.  This
excludes AucTeX, for instance, whose releases are now uploaded to
elpa.gnu.org, GNU Radio, which has releases at www.gnuradio.org, and all the
GNOME packages; EMMS is included though, because its releases are on gnu.org."
  (and (or (not (string-prefix? "emacs-" (package-name package)))
           (gnu-hosted? package))
       (not (gnome-package? package))
       (not (string-prefix? "gnuradio" (package-name package)))
       (gnu-package? package)))

(define gnu-hosted?
  (url-prefix-predicate "mirror://gnu/"))

(define (uri-mirror-rewrite uri)
  "Rewrite URI to a mirror:// URI if possible, or return URI unmodified."
  (if (string-prefix? "mirror://" uri)
      uri                            ;nothing to do, it's already a mirror URI
      (let loop ((mirrors %mirrors))
        (match mirrors
          (()
           uri)
          (((mirror-id mirror-urls ...) rest ...)
           (match (find (cut string-prefix? <> uri) mirror-urls)
             (#f
              (loop rest))
             (prefix
              (format #f "mirror://~a/~a"
                      mirror-id
                      (string-drop uri (string-length prefix))))))))))

(define %savannah-base
  ;; One of the Savannah mirrors listed at
  ;; <https://download.savannah.gnu.org/mirmon/savannah/> that serves valid
  ;; HTML (unlike <https://download.savannah.nongnu.org/releases>.)
  "https://de.freedif.org/savannah/")

(define* (import-savannah-release package #:key (version #f))
  "Return the latest release of PACKAGE. Optionally include a VERSION string
to fetch a specific version."
  (let* ((uri       (string->uri
                     (match (origin-uri (package-source package))
                       ((? string? uri) uri)
                       ((uri mirrors ...) uri))))
         (package   (package-upstream-name package))
         (directory (dirname (uri-path uri))))
    ;; Note: We use the default 'file->signature', which adds ".sig", ".asc",
    ;; or whichever detached signature naming scheme PACKAGE uses.
    (import-html-release package
                         #:version version
                         #:base-url %savannah-base
                         #:directory directory)))

(define* (latest-sourceforge-release package #:key (version #f))
  "Return the latest release of PACKAGE. Optionally include a VERSION string
to fetch a specific version."
  (define (uri-append uri extension)
    ;; Return URI with EXTENSION appended.
    (build-uri (uri-scheme uri)
               #:host (uri-host uri)
               #:path (string-append (uri-path uri) extension)))

  (define (valid-uri? uri port)
    ;; Return true if URI is reachable.
    (false-if-exception
     (case (response-code (http-head uri #:port port #:keep-alive? #t))
       ((200 302) #t)
       (else #f))))

  (when version
    (error
     (formatted-message
      (G_ "Updating to a specific version is not yet implemented for ~a, sorry.")
      "sourceforge")))

  (let* ((name     (package-upstream-name package))
         (base     (string-append "https://sourceforge.net/projects/"
                                  name "/files"))
         (url      (string-append base "/latest/download"))
         (uri      (string->uri url))
         (port     (false-if-exception (open-socket-for-uri uri)))
         (response (and port
                        (http-head uri #:port port #:keep-alive? #t))))
    (dynamic-wind
      (const #t)
      (lambda ()
        (and response
             (= 302 (response-code response))
             (response-location response)
             (match (string-tokenize (uri-path (response-location response))
                                     (char-set-complement (char-set #\/)))
               ((_ components ...)
                (let* ((path (string-join components "/"))
                       (url  (string-append "mirror://sourceforge/" path)))
                  (and (release-file? name (basename path))

                       ;; Take the heavy-handed approach of probing 3 additional
                       ;; URLs.  XXX: Would be nicer if this could be avoided.
                       (let* ((loc (response-location response))
                              (sig (any (lambda (extension)
                                          (let ((uri (uri-append loc extension)))
                                            (and (valid-uri? uri port)
                                                 (string-append url extension))))
                                        '(".asc" ".sig" ".sign"))))
                         (upstream-source
                          (package name)
                          (version (tarball->version (basename path)))
                          (urls (list url))
                          (signature-urls (and sig (list sig)))))))))))
      (lambda ()
        (when port
          (close-port port))))))

(define* (import-xorg-release package #:key (version #f))
  "Return the latest release of PACKAGE.  Optionally include a VERSION string
to fetch a specific version."
  (let ((uri (string->uri (origin-uri (package-source package)))))
    (false-if-ftp-error
     (import-ftp-release
      (package-name package)
      #:version version
      #:server "ftp.freedesktop.org"
      #:directory
      (string-append "/pub/xorg/" (dirname (uri-path uri)))))))

(define* (import-kernel.org-release package #:key (version #f))
  "Return the latest release of PACKAGE, the name of a kernel.org package.
Optionally include a VERSION string to fetch a specific version."
  (define %kernel.org-base
    ;; This URL and sub-directories thereof are nginx-generated directory
    ;; listings suitable for 'import-html-release'.
    "https://mirrors.edge.kernel.org/pub")

  (define (file->signature file)
    (string-append (file-sans-extension file) ".sign"))

  (let* ((uri       (string->uri
                     (match (origin-uri (package-source package))
                       ((? string? uri) uri)
                       ((uri mirrors ...) uri))))
         (package   (package-upstream-name package))
         (directory (dirname (uri-path uri))))
    (import-html-release package
                         #:version version
                         #:base-url %kernel.org-base
                         #:directory directory
                         #:file->signature file->signature)))

(define html-updatable-package?
  ;; Return true if the given package may be handled by the generic HTML
  ;; updater.
  (let ((hosting-sites '("github.com" "github.io" "gitlab.com"
                         "notabug.org" "sr.ht" "gitlab.inria.fr"
                         "ftp.gnu.org" "download.savannah.gnu.org"
                         "pypi.org" "crates.io" "rubygems.org"
                         "bioconductor.org")))
    (define http-url?
      (url-predicate (lambda (url)
                       (match (string->uri url)
                         (#f #f)
                         (uri
                          (let ((scheme (uri-scheme uri))
                                (host   (uri-host uri)))
                            (and (memq scheme '(http https))
                                 (not (member host hosting-sites)))))))))

    (lambda (package)
      (or (assoc-ref (package-properties package) 'release-monitoring-url)
          (http-url? package)))))

(define* (import-html-updatable-release package #:key (version #f))
  "Return the latest release of PACKAGE.  Do that by crawling the HTML page of
the directory containing its source tarball.  Optionally include a VERSION
string to fetch a specific version."
  (let* ((uri       (string->uri
                     (match (origin-uri (package-source package))
                       ((? string? url) url)
                       ((url _ ...) url))))
         (custom    (assoc-ref (package-properties package)
                               'release-monitoring-url))
         (base      (or custom
                        (string-append (symbol->string (uri-scheme uri))
                                       "://" (uri-host uri))))
         (directory (if custom
                        ""
                        (dirname (uri-path uri))))
         (package   (package-upstream-name package)))
    (false-if-networking-error
     (import-html-release package
                          #:version version
                          #:base-url base
                          #:directory directory))))

(define %gnu-updater
  ;; This is for everything at ftp.gnu.org.
  (upstream-updater
   (name 'gnu)
   (description "Updater for GNU packages")
   (pred (lambda (package)
           (false-if-networking-error (gnu-hosted? package))))
   (import import-gnu-release)))

(define %gnu-ftp-updater
  ;; This is for GNU packages taken from alternate locations, such as
  ;; alpha.gnu.org, ftp.gnupg.org, etc.  It is obsolescent.
  (upstream-updater
   (name 'gnu-ftp)
   (description "Updater for GNU packages only available via FTP")
   (pred (lambda (package)
           (false-if-networking-error
            (and (not (gnu-hosted? package))
                 (pure-gnu-package? package)))))
   (import import-release*)))

(define %savannah-updater
  (upstream-updater
   (name 'savannah)
   (description "Updater for packages hosted on savannah.gnu.org")
   (pred (url-prefix-predicate "mirror://savannah/"))
   (import import-savannah-release)))

(define %sourceforge-updater
  (upstream-updater
   (name 'sourceforge)
   (description "Updater for packages hosted on sourceforge.net")
   (pred (url-prefix-predicate "mirror://sourceforge/"))
   (import latest-sourceforge-release)))

(define %xorg-updater
  (upstream-updater
   (name 'xorg)
   (description "Updater for X.org packages")
   (pred (url-prefix-predicate "mirror://xorg/"))
   (import import-xorg-release)))

(define %kernel.org-updater
  (upstream-updater
   (name 'kernel.org)
   (description "Updater for packages hosted on kernel.org")
   (pred (url-prefix-predicate "mirror://kernel.org/"))
   (import import-kernel.org-release)))

(define %generic-html-updater
  (upstream-updater
   (name 'generic-html)
   (description "Updater that crawls HTML pages.")
   (pred html-updatable-package?)
   (import import-html-updatable-release)))

;;; gnu-maintenance.scm ends here
