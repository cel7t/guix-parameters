;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Timothy Sample <samplet@ngyro.com>
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

(define-module (guix build download)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module ((web client) #:hide (open-socket-for-uri))
  #:use-module (web response)
  #:use-module (guix base64)
  #:use-module (guix ftp-client)
  #:use-module (guix build utils)
  #:use-module (guix progress)
  #:use-module (guix memoization)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:autoload   (ice-9 ftw) (scandir)
  #:autoload   (guix base16) (bytevector->base16-string)
  #:autoload   (guix swh) (swh-download-directory %verify-swh-certificate?)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (open-socket-for-uri
            open-connection-for-uri
            http-fetch
            %x509-certificate-directory
            close-connection
            resolve-uri-reference
            maybe-expand-mirrors
            url-fetch
            byte-count->string
            uri-abbreviation
            nar-uri-abbreviation
            store-path-abbreviation))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over HTTP or FTP (builder-side code).
;;;
;;; Code:

(define %http-receive-buffer-size
  ;; Size of the HTTP receive buffer.
  65536)

(define* (ellipsis #:optional (port (current-output-port)))
  "Make a rough guess at whether Unicode's HORIZONTAL ELLIPSIS can be written
in PORT's encoding, and return either that or ASCII dots."
  (if (equal? (port-encoding port) "UTF-8")
      "…"
      "..."))

(define* (store-path-abbreviation store-path #:optional (prefix-length 6))
  "If STORE-PATH is the file name of a store entry, return an abbreviation of
STORE-PATH for display, showing PREFIX-LENGTH characters of the hash.
Otherwise return STORE-PATH."
  (if (string-prefix? (%store-directory) store-path)
      (let ((base (basename store-path)))
        (string-append (string-take base prefix-length)
                       (ellipsis)
                       (string-drop base 32)))
      store-path))

(define* (uri-abbreviation uri #:optional (max-length 42))
  "If URI's string representation is larger than MAX-LENGTH, return an
abbreviation of URI showing the scheme, host, and basename of the file."
  (define uri-as-string
    (uri->string uri))

  (define (elide-path)
    (let* ((path   (uri-path uri))
           (base   (basename path))
           (prefix (string-append (symbol->string (uri-scheme uri)) "://"

                                  ;; `file' URIs have no host part.
                                  (or (uri-host uri) "")

                                  (string-append "/" (ellipsis) "/"))))
      (if (> (+ (string-length prefix) (string-length base)) max-length)
          (string-append prefix (ellipsis)
                         (string-drop base (quotient (string-length base) 2)))
          (string-append prefix base))))

  (if (> (string-length uri-as-string) max-length)
      (let ((short (elide-path)))
        (if (< (string-length short) (string-length uri-as-string))
            short
            uri-as-string))
      uri-as-string))

(define (nar-uri-abbreviation uri)
  "Abbreviate URI, which is assumed to be the URI of a nar as served by Hydra
and 'guix publish', something like
\"http://example.org/nar/1ldrllwbna0aw5z8kpci4fsvbd2w8cw4-texlive-bin-2015\"."
  (let* ((uri  (if (string? uri) (string->uri uri) uri))
         (path (basename (uri-path uri))))
    (if (and (> (string-length path) 33)
             (char=? (string-ref path 32) #\-))
        (string-drop path 33)
        path)))

(define* (ftp-fetch uri file #:key timeout print-build-trace?)
  "Fetch data from URI and write it to FILE.  Return FILE on success.  Bail
out if the connection could not be established in less than TIMEOUT seconds."
  (let* ((conn (match (and=> (uri-userinfo uri)
                             (cut string-split <> #\:))
                 (((? string? user))
                  (ftp-open (uri-host uri) #:timeout timeout
                                           #:username user))
                 (((? string? user) (? string? pass))
                  (ftp-open (uri-host uri) #:timeout timeout
                                           #:username user
                                           #:password pass))
                 (_ (ftp-open (uri-host uri) #:timeout timeout))))
         (size (false-if-exception (ftp-size conn (uri-path uri))))
         (in   (ftp-retr conn (basename (uri-path uri))
                         (dirname (uri-path uri))
                         #:timeout timeout)))
    (call-with-output-file file
      (lambda (out)
        (dump-port* in out
                    #:buffer-size %http-receive-buffer-size
                    #:reporter
                    (if print-build-trace?
                        (progress-reporter/trace
                         file (uri->string uri) size)
                        (progress-reporter/file
                         (uri-abbreviation uri) size)))))

    (ftp-close conn)
    (unless print-build-trace?
      (newline))
    file))

;; Autoload GnuTLS so that this module can be used even when GnuTLS is
;; not available.  At compile time, this yields "possibly unbound
;; variable" warnings, but these are OK: we know that the variables will
;; be bound if we need them, because (guix download) adds GnuTLS as an
;; input in that case.

(define (load-gnutls)
  ;; XXX: Use this hack instead of #:autoload to avoid compilation errors.
  ;; See <http://bugs.gnu.org/12202>.
  (module-use! (resolve-module '(guix build download))
               (resolve-interface '(gnutls)))
  (set! load-gnutls (const #t)))

(define %x509-certificate-directory
  ;; The directory where X.509 authority PEM certificates are stored.
  (make-parameter (or (getenv "GUIX_TLS_CERTIFICATE_DIRECTORY")
                      (getenv "SSL_CERT_DIR")     ;like OpenSSL
                      "/etc/ssl/certs")))

(define (set-certificate-credentials-x509-trust-file!* cred file format)
  "Like 'set-certificate-credentials-x509-trust-file!', but without the file
name decoding bug described at
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26948#17>."
  (let ((data (call-with-input-file file get-bytevector-all)))
    (set-certificate-credentials-x509-trust-data! cred data format)))

(define make-credentials-with-ca-trust-files
  (mlambda (directory)
    "Return certificate credentials with X.509 authority certificates read from
DIRECTORY.  Those authority certificates are checked when
'peer-certificate-status' is later called."
    ;; Memoize the result to avoid scanning all the certificates every time a
    ;; connection is made.
    (let ((cred  (make-certificate-credentials))
          (files (match (scandir directory (cut string-suffix? ".pem" <>))
                   ((or #f ())
                    ;; Some distros provide nothing but bundles (*.crt) under
                    ;; /etc/ssl/certs, so look for them.
                    (or (scandir directory (cut string-suffix? ".crt" <>))
                        '()))
                   (pem pem))))
      (for-each (lambda (file)
                  (let ((file (string-append directory "/" file)))
                    ;; Protect against dangling symlinks.
                    (when (file-exists? file)
                      (set-certificate-credentials-x509-trust-file!*
                       cred file
                       x509-certificate-format/pem))))
                files)
      cred)))

(define (peer-certificate session)
  "Return the certificate of the remote peer in SESSION."
  (match (session-peer-certificate-chain session)
    ((first _ ...)
     (import-x509-certificate first x509-certificate-format/der))))

(define (assert-valid-server-certificate session server)
  "Return #t if the certificate of the remote peer for SESSION is a valid
certificate for SERVER, where SERVER is the expected host name of peer."
  (define cert
    (peer-certificate session))

  ;; First check whether the server's certificate matches SERVER.
  (unless (x509-certificate-matches-hostname? cert server)
    (throw 'tls-certificate-error 'host-mismatch cert server))

  ;; Second check its validity and reachability from the set of authority
  ;; certificates loaded via 'set-certificate-credentials-x509-trust-file!'.
  (match (peer-certificate-status session)
    (()                                           ;certificate is valid
     #t)
    ((statuses ...)
     (throw 'tls-certificate-error 'invalid-certificate cert server
            statuses))))

(define (print-tls-certificate-error port key args default-printer)
  "Print the TLS certificate error represented by ARGS in an intelligible
way."
  (match args
    (('host-mismatch cert server)
     (format port
             "X.509 server certificate for '~a' does not match: ~a~%"
             server (x509-certificate-dn cert)))
    (('invalid-certificate cert server statuses)
     (format port
             "X.509 certificate of '~a' could not be verified:~%~{  ~a~%~}"
             server
             (map certificate-status->string statuses)))))

(set-exception-printer! 'tls-certificate-error
                        print-tls-certificate-error)

(define (wrap-record-port-for-gnutls<3.7.7 record port)
  "Return a port that wraps RECORD to ensure that closing it also closes PORT,
the actual socket port, and its file descriptor.  Make sure it does not
introduce extra buffering (custom ports are buffered by default as of Guile
3.0.5).

This wrapper is unnecessary with GnuTLS >= 3.7.7, which can automatically
close SESSION's file descriptor when RECORD is closed."
  (define (read! bv start count)
    (define read
      (catch 'gnutls-error
        (lambda ()
          (get-bytevector-n! record bv start count))
        (lambda (key err proc . rest)
          ;; When responding to "Connection: close" requests, some servers
          ;; close the connection abruptly after sending the response body,
          ;; without doing a proper TLS connection termination.  Treat it as
          ;; EOF.  This is fixed in GnuTLS 3.7.7.
          (if (eq? err error/premature-termination)
              the-eof-object
              (apply throw key err proc rest)))))

    (if (eof-object? read)
        0
        read))
  (define (write! bv start count)
    (put-bytevector record bv start count)
    (force-output record)
    count)
  (define (get-position)
    (port-position record))
  (define (set-position! new-position)
    (set-port-position! record new-position))
  (define (close)
    (unless (port-closed? port)
      (close-port port))
    (unless (port-closed? record)
      (close-port record)))

  (define (unbuffered port)
    (setvbuf port 'none)
    port)

  (unbuffered
   (make-custom-binary-input/output-port "gnutls wrapped port" read! write!
                                         get-position set-position!
                                         close)))

(define* (tls-wrap port server #:key (verify-certificate? #t))
  "Return PORT wrapped in a TLS connection to SERVER.  SERVER must be a DNS
host name without trailing dot."
  (define (log level str)
    (format (current-error-port)
            "gnutls: [~a|~a] ~a" (getpid) level str))

  (load-gnutls)
  (let ((session  (make-session connection-end/client))
        (ca-certs (%x509-certificate-directory)))

    ;; Some servers such as 'cloud.github.com' require the client to support
    ;; the 'SERVER NAME' extension.  However, 'set-session-server-name!' is
    ;; not available in older GnuTLS releases.  See
    ;; <http://bugs.gnu.org/18526> for details.
    (if (module-defined? (resolve-interface '(gnutls))
                         'set-session-server-name!)
        (set-session-server-name! session server-name-type/dns server)
        (format (current-error-port)
                "warning: TLS 'SERVER NAME' extension not supported~%"))

    (set-session-transport-fd! session (fileno port))
    (set-session-default-priority! session)

    ;; The "%COMPAT" bit allows us to work around firewall issues (info
    ;; "(gnutls) Priority Strings"); see <http://bugs.gnu.org/23311>.
    ;; Explicitly disable SSLv3, which is insecure:
    ;; <https://tools.ietf.org/html/rfc7568>.
    (set-session-priorities! session "NORMAL:%COMPAT:-VERS-SSL3.0")

    (set-session-credentials! session
                              (if (and verify-certificate? ca-certs)
                                  (make-credentials-with-ca-trust-files
                                   ca-certs)
                                  (make-certificate-credentials)))

    ;; Uncomment the following lines in case of debugging emergency.
    ;;(set-log-level! 10)
    ;;(set-log-procedure! log)

    (let loop ((retries 5))
      (catch 'gnutls-error
        (lambda ()
          (handshake session))
        (lambda (key err proc . rest)
          (cond ((eq? err error/warning-alert-received)
                 ;; Like Wget, do no stop upon non-fatal alerts such as
                 ;; 'alert-description/unrecognized-name'.
                 (format (current-error-port)
                         "warning: TLS warning alert received: ~a~%"
                         (alert-description->string (alert-get session)))
                 (handshake session))
                (else
                 (if (or (fatal-error? err) (zero? retries))
                     (apply throw key err proc rest)
                     (begin
                       ;; We got 'error/again' or similar; try again.
                       (format (current-error-port)
                               "warning: TLS non-fatal error: ~a~%"
                               (error->string err))
                       (loop (- retries 1)))))))))

    ;; Verify the server's certificate if needed.
    (when verify-certificate?
      (catch 'tls-certificate-error
        (lambda ()
          (assert-valid-server-certificate session server))
        (lambda args
          (close-port port)
          (apply throw args))))

    (let ((record (session-record-port session)))
      (setvbuf record 'block)
      (if (module-defined? (resolve-interface '(gnutls))
                           'set-session-record-port-close!) ;GnuTLS >= 3.7.7
          (let ((close-wrapped-port (lambda (_) (close-port port))))
            (set-session-record-port-close! record close-wrapped-port)
            record)
          (wrap-record-port-for-gnutls<3.7.7 record port)))))

(define (ensure-uri uri-or-string)                ;XXX: copied from (web http)
  (cond
   ((string? uri-or-string) (string->uri uri-or-string))
   ((uri? uri-or-string) uri-or-string)
   (else (error "Invalid URI" uri-or-string))))

(define* (open-socket-for-uri uri-or-string #:key timeout)
  "Return an open input/output port for a connection to URI.  When TIMEOUT is
not #f, it must be a (possibly inexact) number denoting the maximum duration
in seconds to wait for the connection to complete; passed TIMEOUT, an
ETIMEDOUT error is raised."
  ;; Includes a fix for <http://bugs.gnu.org/15368> which affects Guile's
  ;; 'open-socket-for-uri' up to 2.0.11 included, uses 'connect*' instead
  ;; of 'connect', and uses AI_ADDRCONFIG.

  (define http-proxy (current-http-proxy))
  (define uri (ensure-uri (or http-proxy uri-or-string)))
  (define addresses
    (let ((port (uri-port uri)))
      (delete-duplicates
       (getaddrinfo (uri-host uri)
                    (cond (port => number->string)
                          (else (symbol->string (uri-scheme uri))))
                    (if (number? port)
                        (logior AI_ADDRCONFIG AI_NUMERICSERV)
                        AI_ADDRCONFIG))
       (lambda (ai1 ai2)
         (equal? (addrinfo:addr ai1) (addrinfo:addr ai2))))))

  (let loop ((addresses addresses))
    (let* ((ai (car addresses))
           (s  (with-fluids ((%default-port-encoding #f))
                 ;; Restrict ourselves to TCP.
                 (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
      (catch 'system-error
        (lambda ()
          (connect* s (addrinfo:addr ai) timeout)

          ;; Buffer input and output on this port.
          (setvbuf s 'block)
          ;; If we're using a proxy, make a note of that.
          (when http-proxy (set-http-proxy-port?! s #t))
          s)
        (lambda args
          ;; Connection failed, so try one of the other addresses.
          (close s)
          (if (null? (cdr addresses))
              (apply throw args)
              (loop (cdr addresses))))))))

(define (setup-http-tunnel port uri)
  "Establish over PORT an HTTP tunnel to the destination server of URI."
  (define target
    (string-append (uri-host uri) ":"
                   (number->string
                    (or (uri-port uri)
                        (match (uri-scheme uri)
                          ('http 80)
                          ('https 443))))))
  (format port "CONNECT ~a HTTP/1.1\r\n" target)
  (format port "Host: ~a\r\n\r\n" target)
  (force-output port)
  (read-response port))

(define* (open-connection-for-uri uri
                                  #:key
                                  timeout
                                  (verify-certificate? #t))
  "Like 'open-socket-for-uri', but also handle HTTPS connections.  When
VERIFY-CERTIFICATE? is true, verify HTTPS server certificates."
  ;; Note: Guile 2.2.0's (web client) has a same-named export that's actually
  ;; undefined.  See Guile commit 011669af3b428e5626f7bbf66b11d57d9768c047.

  (define https?
    (eq? 'https (uri-scheme uri)))

  (define https-proxy (let ((proxy (getenv "https_proxy")))
                        (and (not (equal? proxy ""))
                             proxy)))

  (let-syntax ((with-https-proxy
                (syntax-rules ()
                  ((_ exp)
                   ;; For HTTPS URIs, honor 'https_proxy', not 'http_proxy'.
                   (let ((thunk (lambda () exp)))
                     (if (and https?
                              (module-variable
                               (resolve-interface '(web client))
                               'current-http-proxy))
                         (parameterize ((current-http-proxy https-proxy))
                           (thunk))
                         (thunk)))))))
    (with-https-proxy
     (let ((s (open-socket-for-uri uri #:timeout timeout)))
       ;; Buffer input and output on this port.
       (setvbuf s 'block %http-receive-buffer-size)

       (when (and https? https-proxy)
         (setup-http-tunnel s uri))

       (if https?
           (tls-wrap s (uri-host uri)
                     #:verify-certificate? verify-certificate?)
           s)))))

(define (close-connection port)                   ;deprecated
  (unless (port-closed? port)
    (close-port port)))

;; XXX: This is an awful hack to make sure the (set-port-encoding! p
;; "ISO-8859-1") call in `read-response' passes, even during bootstrap
;; where iconv is not available.
(module-define! (resolve-module '(web response))
                'set-port-encoding!
                (lambda (p e) #f))

(define (resolve-uri-reference ref base)
  "Resolve the URI reference REF, interpreted relative to the BASE URI, into a
target URI, according to the algorithm specified in RFC 3986 section 5.2.2.
Return the resulting target URI."

  (define (merge-paths base-path rel-path)
    (let* ((base-components (string-split base-path #\/))
           (base-directory-components (match base-components
                                        ((components ... last) components)
                                        (() '())))
           (base-directory (string-join base-directory-components "/")))
      (string-append base-directory "/" rel-path)))

  (define (remove-dot-segments path)
    (let loop ((in
                ;; Drop leading "." and ".." components from a relative path.
                ;; (absolute paths will start with a "" component)
                (drop-while (match-lambda
                              ((or "." "..") #t)
                              (_ #f))
                            (string-split path #\/)))
               (out '()))
      (match in
        (("." . rest)
         (loop rest out))
        ((".." . rest)
         (match out
           ((or () (""))
            (error "remove-dot-segments: too many '..' components" path))
           (_
            (loop rest (cdr out)))))
        ((component . rest)
         (loop rest (cons component out)))
        (()
         (string-join (reverse out) "/")))))

  (cond ((or (uri-scheme ref)
             (uri-host   ref))
         (build-uri (or (uri-scheme ref)
                        (uri-scheme base))
                    #:userinfo (uri-userinfo ref)
                    #:host     (uri-host     ref)
                    #:port     (uri-port     ref)
                    #:path     (remove-dot-segments (uri-path ref))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))
        ((string-null? (uri-path ref))
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments (uri-path base))
                    #:query    (or (uri-query ref)
                                   (uri-query base))
                    #:fragment (uri-fragment ref)))
        (else
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments
                                (if (string-prefix? "/" (uri-path ref))
                                    (uri-path ref)
                                    (merge-paths (uri-path base)
                                                 (uri-path ref))))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))))

(define* (http-fetch uri #:key timeout (verify-certificate? #t))
  "Return an input port containing the data at URI, and the expected number of
bytes available or #f.  When TIMEOUT is true, bail out if the connection could
not be established in less than TIMEOUT seconds.  When VERIFY-CERTIFICATE? is
true, verify HTTPS certificates; otherwise simply ignore them."

  (define headers
    `(;; Some web sites, such as http://dist.schmorp.de, would block you if
      ;; there's no 'User-Agent' header, presumably on the assumption that
      ;; you're a spammer.  So work around that.
      (User-Agent . "GNU Guile")

      ;; Some servers, such as https://alioth.debian.org, return "406 Not
      ;; Acceptable" when not explicitly told that everything is accepted.
      (Accept . "*/*")

      ;; Basic authentication, if needed.
      ,@(match (uri-userinfo uri)
          ((? string? str)
           `((Authorization . ,(string-append "Basic "
                                              (base64-encode
                                               (string->utf8 str))))))
          (_ '()))))

  (let*-values (((connection)
                 (open-connection-for-uri uri
                                          #:timeout timeout
                                          #:verify-certificate?
                                          verify-certificate?))
                ((resp port)
                 (http-get uri #:port connection #:decode-body? #f
                           #:streaming? #t
                           #:headers headers))
                ((code)
                 (response-code resp)))
    (case code
      ((200)                                      ; OK
       (values port (response-content-length resp)))
      ((301                                       ; moved permanently
        302                                       ; found (redirection)
        303                                       ; see other
        307                                       ; temporary redirection
        308)                                      ; permanent redirection
       (let ((uri (resolve-uri-reference (response-location resp) uri)))
         (format #t "following redirection to `~a'...~%"
                 (uri->string uri))
         (close connection)
         (http-fetch uri
                     #:timeout timeout
                     #:verify-certificate? verify-certificate?)))
      (else
       (error "download failed" (uri->string uri)
              code (response-reason-phrase resp))))))


(define-syntax-rule (false-if-exception* body ...)
  "Like `false-if-exception', but print the exception on the error port."
  (catch #t
    (lambda ()
      body ...)
    (lambda (key . args)
      #f)
    (lambda (key . args)
      (print-exception (current-error-port) #f key args))))

(define (uri-vicinity dir file)
  "Concatenate DIR, slash, and FILE, keeping only one slash in between.
This is required by some HTTP servers."
  (string-append (string-trim-right dir #\/) "/"
                 (string-trim file #\/)))

(define (maybe-expand-mirrors uri mirrors)
  "If URI uses the 'mirror' scheme, expand it according to the MIRRORS alist.
Return a list of URIs."
  (case (uri-scheme uri)
    ((mirror)
     (let ((kind (string->symbol (uri-host uri)))
           (path (uri-path uri)))
       (match (assoc-ref mirrors kind)
         ((mirrors ..1)
          (map (compose string->uri (cut uri-vicinity <> path))
               mirrors))
         (_
          (error "unsupported URL mirror kind" kind uri)))))
    (else
     (list uri))))

(define* (disarchive-fetch/any uris file
                               #:key (timeout 10) (verify-certificate? #t))
  "Fetch a Disarchive specification from any of URIS, assemble it,
and write the output to FILE."
  (define (fetch-specification uris)
    (any (lambda (uri)
           (false-if-exception*
            (let-values (((port size) (http-fetch uri
                                                  #:verify-certificate?
                                                  verify-certificate?
                                                  #:timeout timeout)))
              (format #t "Retrieving Disarchive spec from ~a ...~%"
                      (uri->string uri))
              (let ((specification (read port)))
                (close-port port)
                specification))))
         uris))

  (define (resolve addresses output)
    (any (match-lambda
           (('swhid swhid)
            (match (string-split swhid #\:)
              (("swh" "1" "dir" id)
               (format #t "Downloading ~a from Software Heritage...~%" file)
               (false-if-exception*
                (swh-download-directory id output)))
              (_ #f)))
           (_ #f))
         addresses))

  (format #t "Trying to use Disarchive to assemble ~a...~%" file)
  (match (and=> (resolve-module '(disarchive) #:ensure #f)
                (lambda (disarchive)
                  (cons (module-ref disarchive '%disarchive-log-port)
                        (module-ref disarchive 'disarchive-assemble))))
    (#f (format #t "could not load Disarchive~%")
        #f)
    ((%disarchive-log-port . disarchive-assemble)
     (match (fetch-specification uris)
       (#f (format #t "could not find its Disarchive specification~%")
           #f)
       (spec (parameterize ((%disarchive-log-port (current-output-port))
                            (%verify-swh-certificate? verify-certificate?))
               (false-if-exception*
                (disarchive-assemble spec file #:resolver resolve))))))))

(define (internet-archive-uri uri)
  "Return a URI corresponding to an Internet Archive backup of URI, or #f if
URI does not denote a Web URI."
  (and (memq (uri-scheme uri) '(http https))
       (let* ((now  (time-utc->date (current-time time-utc)))
              (date (date->string now "~Y~m~d~H~M~S")))
         ;; Note: the date in the URL can be anything and web.archive.org
         ;; automatically redirects to the closest date.
         (build-uri 'https #:host "web.archive.org"
                    #:path (string-append "/web/" date "/"
                                          (uri->string uri))))))

(define* (url-fetch url file
                    #:key
                    (timeout 10) (verify-certificate? #t)
                    (mirrors '()) (content-addressed-mirrors '())
                    (disarchive-mirrors '())
                    (hashes '())
                    print-build-trace?)
  "Fetch FILE from URL; URL may be either a single string, or a list of
string denoting alternate URLs for FILE.  Return #f on failure, and FILE
on success.

When MIRRORS is defined, it must be an alist of mirrors; it is used to resolve
'mirror://' URIs.

HASHES must be a list of algorithm/hash pairs, where each algorithm is a
symbol such as 'sha256 and each hash is a bytevector.
CONTENT-ADDRESSED-MIRRORS must be a list of procedures that, given a hash
algorithm and a hash, return a URL where the specified data can be retrieved
or #f.

When VERIFY-CERTIFICATE? is true, validate HTTPS server certificates;
otherwise simply ignore them."
  (define uri
    (append-map (cut maybe-expand-mirrors <> mirrors)
                (match url
                  ((_ ...) (map string->uri url))
                  (_       (list (string->uri url))))))

  (define (fetch uri file)
    (format #t "~%Starting download of ~a~%From ~a...~%"
            file (uri->string uri))
    (case (uri-scheme uri)
      ((http https)
       (false-if-exception*
        (let-values (((port size)
                      (http-fetch uri
                                  #:verify-certificate? verify-certificate?
                                  #:timeout timeout)))
          (call-with-output-file file
            (lambda (output)
              (dump-port* port output
                          #:buffer-size %http-receive-buffer-size
                          #:reporter (if print-build-trace?
                                         (progress-reporter/trace
                                          file (uri->string uri) size)
                                         (progress-reporter/file
                                          (uri-abbreviation uri) size)))
              (newline)))
          (close-port port)
          file)))
      ((ftp)
       (false-if-exception* (ftp-fetch uri file
                                       #:timeout timeout
                                       #:print-build-trace?
                                       print-build-trace?)))
      (else
       (format #t "skipping URI with unsupported scheme: ~s~%"
               uri)
       #f)))

  (define content-addressed-uris
    (append-map (lambda (make-url)
                  (filter-map (match-lambda
                                ((hash-algo . hash)
                                 (let ((file (strip-store-file-name file)))
                                   (string->uri (make-url file hash-algo hash)))))
                              hashes))
                content-addressed-mirrors))

  (define disarchive-uris
    (append-map (lambda (mirror)
                  (let ((make-url (match mirror
                                    ((? string?)
                                     (lambda (hash-algo hash)
                                       (string-append
                                        mirror
                                        (symbol->string hash-algo) "/"
                                        (bytevector->base16-string hash))))
                                    ((? procedure?)
                                     mirror))))
                    (map (match-lambda
                           ((hash-algo . hash)
                            (string->uri (make-url hash-algo hash))))
                         hashes)))
                disarchive-mirrors))

  ;; Make this unbuffered so 'progress-report/file' works as expected.  'line
  ;; means '\n', not '\r', so it's not appropriate here.
  (setvbuf (current-output-port) 'none)

  (setvbuf (current-error-port) 'line)

  (let try ((uri (append uri content-addressed-uris
                   (match uri
                     ((first . _)
                      (or (and=> (internet-archive-uri first) list)
                          '()))
                     (() '())))))
    (match uri
      ((uri tail ...)
       (or (fetch uri file)
           (try tail)))
      (()
       ;; If we are looking for a software archive, one last thing we
       ;; can try is to use Disarchive to assemble it.
       (or (disarchive-fetch/any disarchive-uris file
                                 #:verify-certificate? verify-certificate?
                                 #:timeout timeout)
           (begin
             (format (current-error-port) "failed to download ~s from ~s~%"
                     file url)
             ;; Remove FILE in case we made an incomplete download, for
             ;; example due to ENOSPC.
             (catch 'system-error
               (lambda ()
                 (delete-file file))
               (const #f))
             #f))))))

;;; download.scm ends here
