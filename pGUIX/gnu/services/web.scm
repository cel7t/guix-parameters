;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017, 2018, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019, 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2022 Simen Endsjø <simendsjo@gmail.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services getmail)
  #:use-module (gnu services mail)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages web)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages php)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages rust-apps)
  #:autoload   (guix i18n) (G_)
  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module ((guix store) #:select (text-file))
  #:use-module ((guix utils) #:select (version-major))
  #:use-module ((guix packages) #:select (package-version))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (httpd-configuration
            httpd-configuration?
            httpd-configuration-package
            httpd-configuration-pid-file
            httpd-configuration-config

            httpd-virtualhost
            httpd-virtualhost?
            httpd-virtualhost-addresses-and-ports
            httpd-virtualhost-contents

            httpd-config-file
            httpd-config-file?
            httpd-config-file-modules
            httpd-config-file-server-root
            httpd-config-file-server-name
            httpd-config-file-listen
            httpd-config-file-pid-file
            httpd-config-file-error-log
            httpd-config-file-user
            httpd-config-file-group

            httpd-module
            httpd-module?
            %default-httpd-modules

            httpd-service-type

            nginx-configuration
            nginx-configuration?
            nginx-configuration-nginx
            nginx-configuration-shepherd-requirement
            nginx-configuration-log-directory
            nginx-configuration-log-level
            nginx-configuration-run-directory
            nginx-configuration-server-blocks
            nginx-configuration-upstream-blocks
            nginx-configuration-server-names-hash-bucket-size
            nginx-configuration-server-names-hash-bucket-max-size
            nginx-configuration-modules
            nginx-configuration-global-directives
            nginx-configuration-extra-content
            nginx-configuration-file

            nginx-server-configuration
            nginx-server-configuration?
            nginx-server-configuration-listen
            nginx-server-configuration-server-name
            nginx-server-configuration-root
            nginx-server-configuration-locations
            nginx-server-configuration-index
            nginx-server-configuration-ssl-certificate
            nginx-server-configuration-ssl-certificate-key
            nginx-server-configuration-server-tokens?
            nginx-server-configuration-raw-content

            nginx-upstream-configuration
            nginx-upstream-configuration?
            nginx-upstream-configuration-name
            nginx-upstream-configuration-servers
            nginx-upstream-configuration-extra-content

            nginx-location-configuration
            nginx-location-configuration?
            nginx-location-configuration-uri
            nginx-location-configuration-body

            nginx-named-location-configuration
            nginx-named-location-configuration?
            nginx-named-location-configuration-name
            nginx-named-location-configuration-body

            nginx-service
            nginx-service-type

            fcgiwrap-configuration
            fcgiwrap-configuration?
            fcgiwrap-service-type

            php-fpm-configuration
            make-php-fpm-configuration
            php-fpm-configuration?
            php-fpm-configuration-php
            php-fpm-configuration-socket
            php-fpm-configuration-user
            php-fpm-configuration-group
            php-fpm-configuration-socket-user
            php-fpm-configuration-socket-group
            php-fpm-configuration-pid-file
            php-fpm-configuration-log-file
            php-fpm-configuration-process-manager
            php-fpm-configuration-display-errors
            php-fpm-configuration-timezone
            php-fpm-configuration-workers-log-file
            php-fpm-configuration-file
            php-fpm-configuration-php-ini-file

            php-fpm-dynamic-process-manager-configuration
            make-php-fpm-dynamic-process-manager-configuration
            php-fpm-dynamic-process-manager-configuration?
            php-fpm-dynamic-process-manager-configuration-max-children
            php-fpm-dynamic-process-manager-configuration-start-servers
            php-fpm-dynamic-process-manager-configuration-min-spare-servers
            php-fpm-dynamic-process-manager-configuration-max-spare-servers

            php-fpm-static-process-manager-configuration
            make-php-fpm-static-process-manager-configuration
            php-fpm-static-process-manager-configuration?
            php-fpm-static-process-manager-configuration-max-children

            php-fpm-on-demand-process-manager-configuration
            make-php-fpm-on-demand-process-manager-configuration
            php-fpm-on-demand-process-manager-configuration?
            php-fpm-on-demand-process-manager-configuration-max-children
            php-fpm-on-demand-process-manager-configuration-process-idle-timeout

            php-fpm-service-type
            nginx-php-location

            cat-avatar-generator-service

            hpcguix-web-configuration
            hpcguix-web-configuration?
            hpcguix-web-service-type

            tailon-configuration-file
            tailon-configuration-file?
            tailon-configuration-file-files
            tailon-configuration-file-bind
            tailon-configuration-file-relative-root
            tailon-configuration-file-allow-transfers?
            tailon-configuration-file-follow-names?
            tailon-configuration-file-tail-lines
            tailon-configuration-file-allowed-commands
            tailon-configuration-file-debug?
            tailon-configuration-file-http-auth
            tailon-configuration-file-users

            tailon-configuration
            tailon-configuration?
            tailon-configuration-config-file
            tailon-configuration-package

            tailon-service-type

            anonip-configuration
            anonip-configuration?
            anonip-configuration-anonip
            anonip-configuration-input
            anonip-configuration-output
            anonip-configuration-skip-private?
            anonip-configuration-column
            anonip-configuration-replacement
            anonip-configuration-ipv4mask
            anonip-configuration-ipv6mask
            anonip-configuration-increment
            anonip-configuration-delimiter
            anonip-configuration-regex
            anonip-service-type

            varnish-configuration
            varnish-configuration?
            varnish-configuration-package
            varnish-configuration-name
            varnish-configuration-backend
            varnish-configuration-vcl
            varnish-configuration-listen
            varnish-configuration-storage
            varnish-configuration-parameters
            varnish-configuration-extra-options

            varnish-service-type

            patchwork-database-configuration
            patchwork-database-configuration?
            patchwork-database-configuration-engine
            patchwork-database-configuration-name
            patchwork-database-configuration-user
            patchwork-database-configuration-password
            patchwork-database-configuration-host
            patchwork-database-configuration-port

            patchwork-settings-module
            patchwork-settings-module?
            patchwork-settings-module-database-configuration
            patchwork-settings-module-secret-key
            patchwork-settings-module-allowed-hosts
            patchwork-settings-module-default-from-email
            patchwork-settings-module-static-url
            patchwork-settings-module-admins
            patchwork-settings-module-debug?
            patchwork-settings-module-enable-rest-api?
            patchwork-settings-module-enable-xmlrpc?
            patchwork-settings-module-force-https-links?
            patchwork-settings-module-extra-settings

            patchwork-configuration
            patchwork-configuration?
            patchwork-configuration-patchwork
            patchwork-configuration-settings-module
            patchwork-configuration-domain

            patchwork-virtualhost
            patchwork-service-type

            mumi-configuration
            mumi-configuration?
            mumi-configuration-mumi
            mumi-configuration-mailer?
            mumi-configuration-sender
            mumi-configuration-smtp

            mumi-service-type

            gmnisrv-configuration
            gmnisrv-configuration?
            gmnisrv-configuration-package
            gmnisrv-configuration-config-file

            gmnisrv-service-type

            agate-configuration
            agate-configuration?
            agate-configuration-package
            agate-configuration-content
            agate-configuration-cert
            agate-configuration-key
            agate-configuration-addr
            agate-configuration-hostname
            agate-configuration-lang
            agate-configuration-silent
            agate-configuration-serve-secret
            agate-configuration-log-ip
            agate-configuration-user
            agate-configuration-group
            agate-configuration-log-file

            agate-service-type))

;;; Commentary:
;;;
;;; Web services.
;;;
;;; Code:

(define-record-type* <httpd-module>
  httpd-module make-httpd-module
  httpd-module?
  (name httpd-load-module-name)
  (file httpd-load-module-file))

;; Default modules for the httpd-service-type, taken from etc/httpd/httpd.conf
;; file in the httpd package.
(define %default-httpd-modules
  (map (match-lambda
         ((name file)
          (httpd-module
           (name name)
           (file file))))
       '(("authn_file_module" "modules/mod_authn_file.so")
         ("authn_core_module" "modules/mod_authn_core.so")
         ("authz_host_module" "modules/mod_authz_host.so")
         ("authz_groupfile_module" "modules/mod_authz_groupfile.so")
         ("authz_user_module" "modules/mod_authz_user.so")
         ("authz_core_module" "modules/mod_authz_core.so")
         ("access_compat_module" "modules/mod_access_compat.so")
         ("auth_basic_module" "modules/mod_auth_basic.so")
         ("reqtimeout_module" "modules/mod_reqtimeout.so")
         ("filter_module" "modules/mod_filter.so")
         ("mime_module" "modules/mod_mime.so")
         ("log_config_module" "modules/mod_log_config.so")
         ("env_module" "modules/mod_env.so")
         ("headers_module" "modules/mod_headers.so")
         ("setenvif_module" "modules/mod_setenvif.so")
         ("version_module" "modules/mod_version.so")
         ("unixd_module" "modules/mod_unixd.so")
         ("status_module" "modules/mod_status.so")
         ("autoindex_module" "modules/mod_autoindex.so")
         ("dir_module" "modules/mod_dir.so")
         ("alias_module" "modules/mod_alias.so"))))

(define-record-type* <httpd-config-file>
  httpd-config-file make-httpd-config-file
  httpd-config-file?
  (modules        httpd-config-file-modules
                  (default %default-httpd-modules))
  (server-root    httpd-config-file-server-root
                  (default httpd))
  (server-name    httpd-config-file-server-name
                  (default #f))
  (document-root  httpd-config-file-document-root
                  (default "/srv/http"))
  (listen         httpd-config-file-listen
                  (default '("80")))
  (pid-file       httpd-config-file-pid-file
                  (default "/var/run/httpd"))
  (error-log      httpd-config-file-error-log
                  (default "/var/log/httpd/error_log"))
  (user           httpd-config-file-user
                  (default "httpd"))
  (group          httpd-config-file-group
                  (default "httpd"))
  (extra-config   httpd-config-file-extra-config
                  (default
                    (list "TypesConfig etc/httpd/mime.types"))))

(define-gexp-compiler (httpd-config-file-compiler
                       (file <httpd-config-file>) system target)
  (match file
    (($ <httpd-config-file> load-modules server-root server-name
                                   document-root listen pid-file error-log
                                   user group extra-config)
     (gexp->derivation
      "httpd.conf"
      #~(call-with-output-file (ungexp output "out")
          (lambda (port)
            (display
             (string-append
              (ungexp-splicing
               `(,@(append-map
                    (match-lambda
                      (($ <httpd-module> name module)
                       `("LoadModule " ,name " " ,module "\n")))
                    load-modules)
                 ,@`("ServerRoot " ,server-root "\n")
                 ,@(if server-name
                       `("ServerName " ,server-name "\n")
                       '())
                 ,@`("DocumentRoot " ,document-root "\n")
                 ,@(append-map
                    (lambda (listen-value)
                      `("Listen " ,listen-value "\n"))
                    listen)
                 ,@(if pid-file
                       `("Pidfile " ,pid-file "\n")
                       '())
                 ,@(if error-log
                       `("ErrorLog " ,error-log "\n")
                       '())
                 ,@(if user
                       `("User " ,user "\n")
                       '())
                 ,@(if group
                       `("Group " ,group "\n")
                       '())
                 "\n\n"
                 ,@extra-config)))
             port)))
      #:local-build? #t))))

(define-record-type <httpd-virtualhost>
  (httpd-virtualhost addresses-and-ports contents)
  httpd-virtualhost?
  (addresses-and-ports httpd-virtualhost-addresses-and-ports)
  (contents            httpd-virtualhost-contents))

(define-record-type* <httpd-configuration>
  httpd-configuration make-httpd-configuration
  httpd-configuration?
  (package  httpd-configuration-package
            (default httpd))
  (pid-file httpd-configuration-pid-file
            (default "/var/run/httpd"))
  (config   httpd-configuration-config
            (default (httpd-config-file))))

(define %httpd-accounts
  (list (user-group (name "httpd") (system? #t))
        (user-account
         (name "httpd")
         (group "httpd")
         (system? #t)
         (comment "Apache HTTPD server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define httpd-shepherd-services
  (match-lambda
    (($ <httpd-configuration> package pid-file config)
     (list (shepherd-service
            (provision '(httpd))
            (documentation "The Apache HTTP Server")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      `(#$(file-append package "/bin/httpd")
                        #$@(if config
                               (list "-f" config)
                               '()))
                      #:pid-file #$pid-file))
            (stop #~(make-kill-destructor)))))))

(define httpd-activation
  (match-lambda
    (($ <httpd-configuration> package pid-file config)
     (match-record
      config
      <httpd-config-file>
      (error-log document-root)
      #~(begin
          (use-modules (guix build utils))

          (mkdir-p #$(dirname error-log))
          (mkdir-p #$document-root))))))

(define (httpd-process-extensions original-config extension-configs)
  (let ((config (httpd-configuration-config
                 original-config)))
    (if (httpd-config-file? config)
        (httpd-configuration
         (inherit original-config)
         (config
          (httpd-config-file
           (inherit config)
           (extra-config
            (append (httpd-config-file-extra-config config)
                    (append-map
                     (match-lambda
                       (($ <httpd-virtualhost>
                           addresses-and-ports
                           contents)
                        `(,(string-append
                            "\n<VirtualHost " addresses-and-ports ">\n")
                          ,@contents
                          "\n</VirtualHost>\n"))
                       ((? string? x)
                        `("\n" ,x "\n"))
                       ((? list? x)
                        `("\n" ,@x "\n")))
                     extension-configs)))))))))

(define httpd-service-type
  (service-type (name 'httpd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          httpd-shepherd-services)
                       (service-extension activation-service-type
                                          httpd-activation)
                       (service-extension account-service-type
                                          (const %httpd-accounts))))
                (compose concatenate)
                (extend httpd-process-extensions)
                (default-value
                  (httpd-configuration))
                (description "Run the Apache httpd Web server.")))

(define-record-type* <nginx-server-configuration>
  nginx-server-configuration make-nginx-server-configuration
  nginx-server-configuration?
  (listen              nginx-server-configuration-listen
                       (default '("80" "443 ssl")))
  (server-name         nginx-server-configuration-server-name
                       (default (list 'default)))
  (root                nginx-server-configuration-root
                       (default "/srv/http"))
  (locations           nginx-server-configuration-locations
                       (default '()))
  (index               nginx-server-configuration-index
                       (default (list "index.html")))
  (try-files           nginx-server-configuration-try-files
                       (default '()))
  (ssl-certificate     nginx-server-configuration-ssl-certificate
                       (default #f))
  (ssl-certificate-key nginx-server-configuration-ssl-certificate-key
                       (default #f))
  (server-tokens?      nginx-server-configuration-server-tokens?
                       (default #f))
  (raw-content         nginx-server-configuration-raw-content
                       (default '())))

(define-record-type* <nginx-upstream-configuration>
  nginx-upstream-configuration make-nginx-upstream-configuration
  nginx-upstream-configuration?
  (name                nginx-upstream-configuration-name)
  (servers             nginx-upstream-configuration-servers)
  (extra-content       nginx-upstream-configuration-extra-content
                       (default '())))

(define-record-type* <nginx-location-configuration>
  nginx-location-configuration make-nginx-location-configuration
  nginx-location-configuration?
  (uri                 nginx-location-configuration-uri
                       (default #f))
  (body                nginx-location-configuration-body))

(define-record-type* <nginx-named-location-configuration>
  nginx-named-location-configuration make-nginx-named-location-configuration
  nginx-named-location-configuration?
  (name                nginx-named-location-configuration-name
                       (default #f))
  (body                nginx-named-location-configuration-body))

(define-record-type* <nginx-configuration>
  nginx-configuration make-nginx-configuration
  nginx-configuration?
  (nginx         nginx-configuration-nginx          ;file-like
                 (default nginx))
  (shepherd-requirement nginx-configuration-shepherd-requirement
                        (default '()))              ;list of symbols
  (log-directory nginx-configuration-log-directory  ;string
                 (default "/var/log/nginx"))
  (log-level     nginx-configuration-log-level
                 (sanitize assert-valid-log-level)
                 (default 'error))
  (run-directory nginx-configuration-run-directory  ;string
                 (default "/var/run/nginx"))
  (server-blocks nginx-configuration-server-blocks
                 (default '()))          ;list of <nginx-server-configuration>
  (upstream-blocks nginx-configuration-upstream-blocks
                   (default '()))      ;list of <nginx-upstream-configuration>
  (server-names-hash-bucket-size nginx-configuration-server-names-hash-bucket-size
                                 (default #f))
  (server-names-hash-bucket-max-size nginx-configuration-server-names-hash-bucket-max-size
                                     (default #f))
  (modules nginx-configuration-modules (default '()))
  (global-directives nginx-configuration-global-directives
                     (default '((events . ()))))
  (lua-package-path nginx-lua-package-path ;list of file-like
                    (default #f))
  (lua-package-cpath nginx-lua-package-cpath ;list of file-like
                     (default #f))
  (extra-content nginx-configuration-extra-content
                 (default ""))
  (file          nginx-configuration-file         ;#f | string | file-like
                 (default #f)))

(define (assert-valid-log-level level)
  "Ensure @var{level} is one of @code{'debug}, @code{'info}, @code{'notice},
@code{'warn}, @code{'error}, @code{'crit}, @code{'alert}, or @code{'emerg}."
  (unless (memq level '(debug info notice warn error crit alert emerg))
    (raise
     (formatted-message (G_ "unknown log level '~a'~%") level)))
  level)

(define (config-domain-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of domain names."
 (map (match-lambda
        ('default "_ ")
        ((? string? str) (list str " ")))
      names))

(define (config-index-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of index files."
 (map (match-lambda
        ((? string? str) (list str " ")))
      names))

(define (emit-load-module module)
  (list "load_module " module ";\n"))

(define emit-global-directive
  (match-lambda
    ((key . (? list? alist))
     (format #f "~a { ~{~a~}}~%" key (map emit-global-directive alist)))
    ((key . value)
     (format #f "~a ~a;~%" key value))))

(define emit-nginx-location-config
  (match-lambda
    (($ <nginx-location-configuration> uri body)
     (list
      "      location " uri " {\n"
      (map (lambda (x) (list "        " x "\n")) body)
      "      }\n"))
    (($ <nginx-named-location-configuration> name body)
     (list
      "      location @" name " {\n"
      (map (lambda (x) (list "        " x "\n")) body)
      "      }\n"))))

(define (emit-nginx-server-config server)
  (let ((listen (nginx-server-configuration-listen server))
        (server-name (nginx-server-configuration-server-name server))
        (ssl-certificate (nginx-server-configuration-ssl-certificate server))
        (ssl-certificate-key
         (nginx-server-configuration-ssl-certificate-key server))
        (root (nginx-server-configuration-root server))
        (index (nginx-server-configuration-index server))
        (try-files (nginx-server-configuration-try-files server))
        (server-tokens? (nginx-server-configuration-server-tokens? server))
        (locations (nginx-server-configuration-locations server))
        (raw-content (nginx-server-configuration-raw-content server)))
    (define-syntax-parameter <> (syntax-rules ()))
    (define-syntax-rule (and/l x tail ...)
      (let ((x* x))
        (if x*
            (syntax-parameterize ((<> (identifier-syntax x*)))
              (list tail ...))
            '())))
    (list
     "    server {\n"
     (map (lambda (directive) (list "      listen " directive ";\n")) listen)
     "      server_name " (config-domain-strings server-name) ";\n"
     (and/l ssl-certificate     "      ssl_certificate " <> ";\n")
     (and/l ssl-certificate-key "      ssl_certificate_key " <> ";\n")
     (if (not (equal? "" root))
         (list "      root " root ";\n")
         "")
     (if (not (null? index))
         (list "      index " (config-index-strings index) ";\n")
         "")
     (if (not (nil? try-files))
         (and/l (config-index-strings try-files) "      try_files " <> ";\n")
         "")
     "      server_tokens " (if server-tokens? "on" "off") ";\n"
     "\n"
     (map emit-nginx-location-config locations)
     "\n"
     (map (lambda (x) (list "      " x "\n")) raw-content)
     "    }\n")))

(define (emit-nginx-upstream-config upstream)
  (list
   "    upstream " (nginx-upstream-configuration-name upstream) " {\n"
   (map (lambda (server)
          (simple-format #f "      server ~A;\n" server))
        (nginx-upstream-configuration-servers upstream))
   (let ((extra-content
          (nginx-upstream-configuration-extra-content upstream)))
     (if (and extra-content (not (null? extra-content)))
         (cons
          "\n"
          (map (lambda (line)
                 (simple-format #f "      ~A\n" line))
               (flatten extra-content)))
         '()))
   "    }\n"))

(define (flatten . lst)
  "Return a list that recursively concatenates all sub-lists of LST."
  (define (flatten1 head out)
    (if (list? head)
        (fold-right flatten1 out head)
        (cons head out)))
  (fold-right flatten1 '() lst))

(define (default-nginx-config config)
  (match-record config
                <nginx-configuration>
                (nginx log-directory run-directory
                 log-level
                 server-blocks upstream-blocks
                 server-names-hash-bucket-size
                 server-names-hash-bucket-max-size
                 modules
                 global-directives
                 lua-package-path
                 lua-package-cpath
                 extra-content)
   (apply mixed-text-file "nginx.conf"
          (flatten
           "user nginx nginx;\n"
           "pid " run-directory "/pid;\n"
           "error_log " log-directory "/error.log " (symbol->string log-level) ";\n"
           (map emit-load-module modules)
           (map emit-global-directive global-directives)
           "http {\n"
           "    client_body_temp_path " run-directory "/client_body_temp;\n"
           "    proxy_temp_path " run-directory "/proxy_temp;\n"
           "    fastcgi_temp_path " run-directory "/fastcgi_temp;\n"
           "    uwsgi_temp_path " run-directory "/uwsgi_temp;\n"
           "    scgi_temp_path " run-directory "/scgi_temp;\n"
           "    access_log " log-directory "/access.log;\n"
           "    include " nginx "/share/nginx/conf/mime.types;\n"
           (if lua-package-path
               #~(format #f "    lua_package_path ~s;~%"
                         (string-join (map (lambda (path)
                                             (string-append path "/lib/?.lua"))
                                           '#$lua-package-path)
                                      ";"))
               "")
           (if lua-package-cpath
               #~(format #f "    lua_package_cpath ~s;~%"
                         (string-join (map (lambda (cpath)
                                             (string-append cpath "/lib/lua/?.lua"))
                                           '#$lua-package-cpath)
                                      ";"))
               "")
           (if server-names-hash-bucket-size
               (string-append
                 "    server_names_hash_bucket_size "
                 (number->string server-names-hash-bucket-size)
                 ";\n")
               "")
           (if server-names-hash-bucket-max-size
               (string-append
                "    server_names_hash_bucket_max_size "
                (number->string server-names-hash-bucket-max-size)
                ";\n")
               "")
           "\n"
           (map emit-nginx-upstream-config upstream-blocks)
           (map emit-nginx-server-config server-blocks)
           extra-content
           "\n}\n"))))

(define %nginx-accounts
  (list (user-group (name "nginx") (system? #t))
        (user-account
         (name "nginx")
         (group "nginx")
         (system? #t)
         (comment "nginx server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (nginx-activation config)
  (match-record config
                <nginx-configuration>
                (nginx log-directory run-directory file)
   #~(begin
       (use-modules (guix build utils))

       (format #t "creating nginx log directory '~a'~%" #$log-directory)
       (mkdir-p #$log-directory)
       (format #t "creating nginx run directory '~a'~%" #$run-directory)
       (mkdir-p #$run-directory)
       (format #t "creating nginx temp directories '~a/{client_body,proxy,fastcgi,uwsgi,scgi}_temp'~%" #$run-directory)
       (mkdir-p (string-append #$run-directory "/client_body_temp"))
       (mkdir-p (string-append #$run-directory "/proxy_temp"))
       (mkdir-p (string-append #$run-directory "/fastcgi_temp"))
       (mkdir-p (string-append #$run-directory "/uwsgi_temp"))
       (mkdir-p (string-append #$run-directory "/scgi_temp"))
       ;; Start-up logs. Once configuration is loaded, nginx switches to
       ;; log-directory.
       (mkdir-p (string-append #$run-directory "/logs"))
       ;; Check configuration file syntax.
       (system* (string-append #$nginx "/sbin/nginx")
                "-c" #$(or file
                           (default-nginx-config config))
                "-p" #$run-directory
                "-t"))))

(define (nginx-shepherd-service config)
  (match-record config
                <nginx-configuration>
                (nginx file run-directory shepherd-requirement)
   (let* ((nginx-binary (file-append nginx "/sbin/nginx"))
          (pid-file (in-vicinity run-directory "pid"))
          (config-file (or file (default-nginx-config config)))
          (nginx-action
           (lambda args
             #~(lambda _
                 (invoke #$nginx-binary "-c" #$config-file #$@args)
                 (match '#$args
                   (("-s" . _) #f)
                   (_
                    ;; When FILE is true, we cannot be sure that PID-FILE will
                    ;; be created, so assume it won't show up.  When FILE is
                    ;; false, read PID-FILE.
                    #$(if file
                          #~#t
                          #~(read-pid-file #$pid-file))))))))

     (list (shepherd-service
            (provision '(nginx))
            (documentation "Run the nginx daemon.")
            (requirement `(user-processes loopback ,@shepherd-requirement))
            (modules `((ice-9 match)
                       ,@%default-modules))
            (start (nginx-action "-p" run-directory))
            (stop (nginx-action "-s" "stop"))
            (actions
              (list
               (shepherd-configuration-action config-file)
               (shepherd-action
                 (name 'reload)
                 (documentation "Reload nginx configuration file and restart worker processes.
This has the effect of killing old worker processes and starting new ones, using
the same configuration file.  It is useful for situations where the same nginx
configuration file can point to different things after a reload, such as
renewed TLS certificates, or @code{include}d files.")
                 (procedure (nginx-action "-s" "reload")))
               (shepherd-action
                (name 'reopen)
                (documentation "Re-open log files.")
                (procedure (nginx-action "-s" "reopen"))))))))))

(define nginx-service-type
  (service-type (name 'nginx)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          nginx-shepherd-service)
                       (service-extension activation-service-type
                                          nginx-activation)
                       (service-extension account-service-type
                                          (const %nginx-accounts))))
                (compose concatenate)
                (extend (lambda (config servers)
                          (nginx-configuration
                            (inherit config)
                            (server-blocks
                              (append (nginx-configuration-server-blocks config)
                              servers)))))
                (default-value (nginx-configuration))
                (description "Run the nginx Web server.")))

(define-record-type* <fcgiwrap-configuration> fcgiwrap-configuration
  make-fcgiwrap-configuration
  fcgiwrap-configuration?
  (package       fcgiwrap-configuration-package ;file-like
                 (default fcgiwrap))
  (socket        fcgiwrap-configuration-socket
                 (default "tcp:127.0.0.1:9000"))
  (user          fcgiwrap-configuration-user
                 (default "fcgiwrap"))
  (group         fcgiwrap-configuration-group
                 (default "fcgiwrap")))

(define fcgiwrap-accounts
  (match-lambda
    (($ <fcgiwrap-configuration> package socket user group)
     (filter identity
             (list
              (and (equal? group "fcgiwrap")
                   (user-group
                    (name "fcgiwrap")
                    (system? #t)))
              (and (equal? user "fcgiwrap")
                   (user-account
                    (name "fcgiwrap")
                    (group group)
                    (system? #t)
                    (comment "Fcgiwrap Daemon")
                    (home-directory "/var/empty")
                    (shell (file-append shadow "/sbin/nologin")))))))))

(define fcgiwrap-shepherd-service
  (match-lambda
    (($ <fcgiwrap-configuration> package socket user group)
     (list (shepherd-service
            (provision '(fcgiwrap))
            (documentation "Run the fcgiwrap daemon.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append package "/sbin/fcgiwrap")
                        "-s" #$socket)
                      #:user #$user #:group #$group
                      #:log-file "/var/log/fcgiwrap.log"))
            (stop #~(make-kill-destructor)))))))

(define fcgiwrap-activation
  (match-lambda
    (($ <fcgiwrap-configuration> package socket user group)
     #~(begin
         ;; When listening on a unix socket, create a parent directory for the
         ;; socket with the correct permissions.
         (when (string-prefix? "unix:" #$socket)
           (let ((run-directory
                  (dirname (substring #$socket (string-length "unix:")))))
             (mkdir-p run-directory)
             (chown run-directory
                    (passwd:uid (getpw #$user))
                    (group:gid (getgr #$group)))))))))

(define fcgiwrap-service-type
  (service-type (name 'fcgiwrap)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          fcgiwrap-shepherd-service)
                       (service-extension account-service-type
                                          fcgiwrap-accounts)
                       (service-extension activation-service-type
                                          fcgiwrap-activation)))
                (default-value (fcgiwrap-configuration))
                (description "Run FastCGI, an interface between the front-end
and the back-end of a Web service.")))

(define-record-type* <php-fpm-configuration> php-fpm-configuration
  make-php-fpm-configuration
  php-fpm-configuration?
  (php              php-fpm-configuration-php ;file-like
                    (default php))
  (socket           php-fpm-configuration-socket
                    (default (string-append "/var/run/php"
                                            (version-major (package-version php))
                                            "-fpm.sock")))
  (user             php-fpm-configuration-user
                    (default "php-fpm"))
  (group            php-fpm-configuration-group
                    (default "php-fpm"))
  (socket-user      php-fpm-configuration-socket-user
                    (default "php-fpm"))
  (socket-group     php-fpm-configuration-socket-group
                    (default "nginx"))
  (pid-file         php-fpm-configuration-pid-file
                    (default (string-append "/var/run/php"
                                            (version-major (package-version php))
                                            "-fpm.pid")))
  (log-file         php-fpm-configuration-log-file
                    (default (string-append "/var/log/php"
                                            (version-major (package-version php))
                                            "-fpm.log")))
  (process-manager  php-fpm-configuration-process-manager
                    (default (php-fpm-dynamic-process-manager-configuration)))
  (display-errors   php-fpm-configuration-display-errors
                    (default #f))
  (timezone         php-fpm-configuration-timezone
                    (default #f))
  (workers-log-file php-fpm-configuration-workers-log-file
                    (default (string-append "/var/log/php"
                                            (version-major (package-version php))
                                            "-fpm.www.log")))
  (file             php-fpm-configuration-file ;#f | file-like
                    (default #f))
  (php-ini-file     php-fpm-configuration-php-ini-file ;#f | file-like
                    (default #f)))

(define-record-type* <php-fpm-dynamic-process-manager-configuration>
  php-fpm-dynamic-process-manager-configuration
  make-php-fpm-dynamic-process-manager-configuration
  php-fpm-dynamic-process-manager-configuration?
  (max-children         php-fpm-dynamic-process-manager-configuration-max-children
                        (default 5))
  (start-servers        php-fpm-dynamic-process-manager-configuration-start-servers
                        (default 2))
  (min-spare-servers    php-fpm-dynamic-process-manager-configuration-min-spare-servers
                        (default 1))
  (max-spare-servers    php-fpm-dynamic-process-manager-configuration-max-spare-servers
                        (default 3)))

(define-record-type* <php-fpm-static-process-manager-configuration>
  php-fpm-static-process-manager-configuration
  make-php-fpm-static-process-manager-configuration
  php-fpm-static-process-manager-configuration?
  (max-children         php-fpm-static-process-manager-configuration-max-children
                        (default 5)))

(define-record-type* <php-fpm-on-demand-process-manager-configuration>
  php-fpm-on-demand-process-manager-configuration
  make-php-fpm-on-demand-process-manager-configuration
  php-fpm-on-demand-process-manager-configuration?
  (max-children         php-fpm-on-demand-process-manager-configuration-max-children
                        (default 5))
  (process-idle-timeout php-fpm-on-demand-process-manager-configuration-process-idle-timeout
                        (default 10)))

(define php-fpm-accounts
  (match-lambda
    (($ <php-fpm-configuration> php socket user group socket-user socket-group)
     `(,@(if (equal? group "php-fpm")
             '()
             (list (user-group (name "php-fpm") (system? #t))))
       ,(user-group
         (name group)
         (system? #t))
       ,(user-account
         (name user)
         (group group)
         (supplementary-groups '("php-fpm"))
         (system? #t)
         (comment "php-fpm daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))))

(define (default-php-fpm-config socket user group socket-user socket-group
          pid-file log-file pm display-errors timezone workers-log-file)
  (apply mixed-text-file "php-fpm.conf"
         (flatten
          "[global]\n"
          "pid =" pid-file "\n"
          "error_log =" log-file "\n"
          "[www]\n"
          "user =" user "\n"
          "group =" group "\n"
          "listen =" socket "\n"
          "listen.owner =" socket-user "\n"
          "listen.group =" socket-group "\n"

          (if timezone
              (string-append "php_admin_value[date.timezone] = \"" timezone "\"\n")
              "")

          (match pm
            (($ <php-fpm-dynamic-process-manager-configuration>
                pm.max-children
                pm.start-servers
                pm.min-spare-servers
                pm.max-spare-servers)
             (list
              "pm = dynamic\n"
              "pm.max_children =" (number->string pm.max-children) "\n"
              "pm.start_servers =" (number->string pm.start-servers) "\n"
              "pm.min_spare_servers =" (number->string pm.min-spare-servers) "\n"
              "pm.max_spare_servers =" (number->string pm.max-spare-servers) "\n"))

            (($ <php-fpm-static-process-manager-configuration>
                pm.max-children)
             (list
              "pm = static\n"
              "pm.max_children =" (number->string pm.max-children) "\n"))

            (($ <php-fpm-on-demand-process-manager-configuration>
                pm.max-children
                pm.process-idle-timeout)
             (list
              "pm = ondemand\n"
              "pm.max_children =" (number->string pm.max-children) "\n"
              "pm.process_idle_timeout =" (number->string pm.process-idle-timeout) "s\n")))


          "php_flag[display_errors] = " (if display-errors "on" "off") "\n"

          (if workers-log-file
              (list "catch_workers_output = yes\n"
                    "php_admin_value[error_log] =" workers-log-file "\n"
                    "php_admin_flag[log_errors] = on\n")
              (list "catch_workers_output = no\n")))))

(define php-fpm-shepherd-service
  (match-lambda
    (($ <php-fpm-configuration> php socket user group socket-user socket-group
                                pid-file log-file pm display-errors
                                timezone workers-log-file file php-ini-file)
     (list (shepherd-service
            (provision '(php-fpm))
            (documentation "Run the php-fpm daemon.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append php "/sbin/php-fpm")
                        "--fpm-config"
                        #$(or file
                              (default-php-fpm-config socket user group
                                socket-user socket-group pid-file log-file
                                pm display-errors timezone workers-log-file))
                        #$@(if php-ini-file
                               `("-c" ,php-ini-file)
                               '()))
                      #:pid-file #$pid-file))
            (stop #~(make-kill-destructor)))))))

(define (php-fpm-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((user (getpwnam #$(php-fpm-configuration-user config)))
             (touch (lambda (file-name)
                      (call-with-output-file file-name (const #t))))
             (workers-log-file
              #$(php-fpm-configuration-workers-log-file config))
             (init-log-file
              (lambda (file-name)
                (when workers-log-file
                  (when (not (file-exists? file-name))
                    (touch file-name))
                  (chown file-name (passwd:uid user) (passwd:gid user))
                  (chmod file-name #o660)))))
        (init-log-file #$(php-fpm-configuration-log-file config))
        (init-log-file workers-log-file))))


(define php-fpm-service-type
  (service-type
   (name 'php-fpm)
   (description
    "Run @command{php-fpm} to provide a fastcgi socket for calling php through
a webserver.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             php-fpm-shepherd-service)
          (service-extension activation-service-type
                             php-fpm-activation)
          (service-extension account-service-type
                             php-fpm-accounts)))
   (default-value (php-fpm-configuration))))

(define* (nginx-php-location
          #:key
          (nginx-package nginx)
          (socket (string-append "/var/run/php"
                                 (version-major (package-version php))
                                 "-fpm.sock")))
  "Return a nginx-location-configuration that makes nginx run .php files."
  (nginx-location-configuration
   (uri "~ \\.php$")
   (body (list
          "fastcgi_split_path_info ^(.+\\.php)(/.+)$;"
          (string-append "fastcgi_pass unix:" socket ";")
          "fastcgi_index index.php;"
          (list "include " nginx-package "/share/nginx/conf/fastcgi.conf;")))))

(define* (cat-avatar-generator-service
          #:key
          (cache-dir "/var/cache/cat-avatar-generator")
          (package cat-avatar-generator)
          (configuration (nginx-server-configuration)))
  (simple-service
    'cat-http-server nginx-service-type
    (list (nginx-server-configuration
            (inherit configuration)
            (locations
              (cons
                (let ((base (nginx-php-location)))
                  (nginx-location-configuration
                    (inherit base)
                    (body (list (string-append "fastcgi_param CACHE_DIR \""
                                               cache-dir "\";")
                                (nginx-location-configuration-body base)))))
                (nginx-server-configuration-locations configuration)))
            (root #~(string-append #$package
                                   "/share/web/cat-avatar-generator"))))))


(define-record-type* <hpcguix-web-configuration>
  hpcguix-web-configuration make-hpcguix-web-configuration
  hpcguix-web-configuration?

  (package  hpcguix-web-package (default hpcguix-web)) ;file-like

  (specs    hpcguix-web-configuration-specs (default #f)) ;#f | gexp
  (address  hpcguix-web-configuration-address (default "127.0.0.1"))
  (port     hpcguix-web-configuration-port (default 5000)))

(define %hpcguix-web-accounts
  (list (user-group
         (name "hpcguix-web")
         (system? #t))
        (user-account
         (name "hpcguix-web")
         (group "hpcguix-web")
         (system? #t)
         (comment "hpcguix-web")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %hpcguix-web-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 ftw))

        (let ((home-dir "/var/cache/guix/web")
              (user (getpwnam "hpcguix-web")))
          (mkdir-p home-dir)
          (chown home-dir (passwd:uid user) (passwd:gid user))
          (chmod home-dir #o755)

          ;; Remove stale 'packages.json.lock' file (and other lock files, if
          ;; any) since that would prevent 'packages.json' from being updated.
          (for-each (lambda (lock)
                      (delete-file (string-append home-dir "/" lock)))
                    (scandir home-dir
                             (lambda (file)
                               (string-suffix? ".lock" file))))))))

(define %hpcguix-web-log-file
  "/var/log/hpcguix-web.log")

(define %hpcguix-web-log-rotations
  (list (log-rotation
         (files (list %hpcguix-web-log-file))
         (frequency 'weekly))))

(define (hpcguix-web-shepherd-service config)
  (let ((specs       (hpcguix-web-configuration-specs config))
        (hpcguix-web (hpcguix-web-package config)))
    (with-imported-modules (source-module-closure
                            '((gnu build shepherd)))
      (shepherd-service
       (documentation "hpcguix-web daemon")
       (provision     '(hpcguix-web))
       (requirement   '(networking))
       (start #~(make-forkexec-constructor
                 (list #$(file-append hpcguix-web "/bin/hpcguix-web")
                       (string-append "--listen="
                                      #$(hpcguix-web-configuration-address
                                         config))
                       "-p"
                       #$(number->string
                          (hpcguix-web-configuration-port config))
                       #$@(if specs
                              #~((string-append "--config="
                                                #$(scheme-file
                                                   "hpcguix-web.scm" specs)))
                              #~()))
                 #:user "hpcguix-web"
                 #:group "hpcguix-web"
                 #:environment-variables
                 (list "XDG_CACHE_HOME=/var/cache/guix/web"
                       "SSL_CERT_DIR=/etc/ssl/certs")
                 #:log-file #$%hpcguix-web-log-file))
       (stop #~(make-kill-destructor))))))

(define hpcguix-web-service-type
  (service-type
   (name 'hpcguix-web)
   (description "Run the hpcguix-web server.")
   (extensions
    (list (service-extension account-service-type
                             (const %hpcguix-web-accounts))
          (service-extension activation-service-type
                             (const %hpcguix-web-activation))
          (service-extension rottlog-service-type
                             (const %hpcguix-web-log-rotations))
          (service-extension shepherd-root-service-type
                             (compose list hpcguix-web-shepherd-service))))
   (default-value (hpcguix-web-configuration))))


;;;
;;; Tailon
;;;

(define-record-type* <tailon-configuration-file>
  tailon-configuration-file make-tailon-configuration-file
  tailon-configuration-file?
  (files                   tailon-configuration-file-files
                           (default '("/var/log")))
  (bind                    tailon-configuration-file-bind
                           (default "localhost:8080"))
  (relative-root           tailon-configuration-file-relative-root
                           (default #f))
  (allow-transfers?        tailon-configuration-file-allow-transfers?
                           (default #t))
  (follow-names?           tailon-configuration-file-follow-names?
                           (default #t))
  (tail-lines              tailon-configuration-file-tail-lines
                           (default 200))
  (allowed-commands        tailon-configuration-file-allowed-commands
                           (default '("tail" "grep" "awk")))
  (debug?                  tailon-configuration-file-debug?
                           (default #f))
  (wrap-lines              tailon-configuration-file-wrap-lines
                           (default #t))
  (http-auth               tailon-configuration-file-http-auth
                           (default #f))
  (users                   tailon-configuration-file-users
                           (default #f)))

(define (tailon-configuration-files-string files)
  (string-append
   "\n"
   (string-join
    (map
     (lambda (x)
       (string-append
        "  - "
        (cond
         ((string? x)
          (simple-format #f "'~A'" x))
         ((list? x)
          (string-join
           (cons (simple-format #f "'~A':" (car x))
                 (map
                  (lambda (x) (simple-format #f "      - '~A'" x))
                  (cdr x)))
           "\n"))
         (else (error x)))))
     files)
    "\n")))

(define-gexp-compiler (tailon-configuration-file-compiler
                       (file <tailon-configuration-file>) system target)
  (match file
    (($ <tailon-configuration-file> files bind relative-root
                                    allow-transfers? follow-names?
                                    tail-lines allowed-commands debug?
                                    wrap-lines http-auth users)
     (text-file
      "tailon-config.yaml"
      (string-concatenate
       (filter-map
        (match-lambda
         ((key . #f) #f)
         ((key . value) (string-append key ": " value "\n")))

        `(("files" . ,(tailon-configuration-files-string files))
          ("bind" . ,bind)
          ("relative-root" . ,relative-root)
          ("allow-transfers" . ,(if allow-transfers? "true" "false"))
          ("follow-names" . ,(if follow-names? "true" "false"))
          ("tail-lines" . ,(number->string tail-lines))
          ("commands" . ,(string-append "["
                                        (string-join allowed-commands ", ")
                                        "]"))
          ("debug" . ,(if debug? "true" #f))
          ("wrap-lines" . ,(if wrap-lines "true" "false"))
          ("http-auth" . ,http-auth)
          ("users" . ,(if users
                          (string-concatenate
                           (cons "\n"
                                 (map (match-lambda
                                       ((user . pass)
                                        (string-append
                                         "  " user ":" pass)))
                                      users)))
                          #f)))))))))

(define-record-type* <tailon-configuration>
  tailon-configuration make-tailon-configuration
  tailon-configuration?
  (config-file tailon-configuration-config-file
               (default (tailon-configuration-file)))
  (package tailon-configuration-package
           (default tailon)))

(define tailon-shepherd-service
  (match-lambda
    (($ <tailon-configuration> config-file package)
     (list (shepherd-service
            (provision '(tailon))
            (documentation "Run the tailon daemon.")
            (start #~(make-forkexec-constructor
                      `(,(string-append #$package "/bin/tailon")
                        "-c" ,#$config-file)
                      #:user "tailon"
                      #:group "tailon"))
            (stop #~(make-kill-destructor)))))))

(define %tailon-accounts
  (list (user-group (name "tailon") (system? #t))
        (user-account
         (name "tailon")
         (group "tailon")
         (system? #t)
         (comment "tailon")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define tailon-service-type
  (service-type
   (name 'tailon)
   (description
    "Run Tailon, a Web application for monitoring, viewing, and searching log
files.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailon-shepherd-service)
          (service-extension account-service-type
                             (const %tailon-accounts))))
   (compose concatenate)
   (extend (lambda (parameter files)
             (tailon-configuration
              (inherit parameter)
              (config-file
               (let ((old-config-file
                      (tailon-configuration-config-file parameter)))
                 (tailon-configuration-file
                  (inherit old-config-file)
                  (files (append (tailon-configuration-file-files old-config-file)
                                 files))))))))
   (default-value (tailon-configuration))))



;;;
;;; Log anonymization
;;;

(define-record-type* <anonip-configuration>
  anonip-configuration make-anonip-configuration
  anonip-configuration?
  (anonip            anonip-configuration-anonip       ;file-like
                     (default anonip))
  (input             anonip-configuration-input)       ;string
  (output            anonip-configuration-output)      ;string
  (skip-private?     anonip-configuration-skip-private? ;boolean
                     (default #f))
  (column            anonip-configuration-column       ;number
                     (default #f))
  (replacement       anonip-configuration-replacement  ;string
                     (default #f))
  (ipv4mask          anonip-configuration-ipv4mask     ;number
                     (default #f))
  (ipv6mask          anonip-configuration-ipv6mask     ;number
                     (default #f))
  (increment         anonip-configuration-increment    ;number
                     (default #f))
  (delimiter         anonip-configuration-delimiter    ;string
                     (default #f))
  (regex             anonip-configuration-regex        ;string
                     (default #f)))

(define (anonip-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (for-each
         (lambda (directory)
           (mkdir-p directory)
           (chmod directory #o755))
         (list (dirname #$(anonip-configuration-input config))
               (dirname #$(anonip-configuration-output config)))))))

(define (anonip-shepherd-service config)
  (let ((input (anonip-configuration-input config))
        (output (anonip-configuration-output config))
        (optional
         (lambda (accessor option)
           (or (and=> (accessor config)
                      (lambda (value)
                        (list
                         (format #false "~a=~a"
                                 option value))))
               (list)))))
    (list
     (shepherd-service
      (provision
       (list (symbol-append 'anonip- (string->symbol output))))
      (requirement '(user-processes))
      (documentation
       "Anonimyze the given log file location with anonip.")
      (start
       #~(lambda ()
           (define (spawn)
             (fork+exec-command
              (append
               (list #$(file-append (anonip-configuration-anonip config)
                                    "/bin/anonip")
                     (string-append "--input=" #$input)
                     (string-append "--output=" #$output))
               (if #$(anonip-configuration-skip-private? config)
                   '("--skip-private") (list))
               '#$(optional anonip-configuration-column "--column")
               '#$(optional anonip-configuration-ipv4mask "--ipv4mask")
               '#$(optional anonip-configuration-ipv6mask "--ipv6mask")
               '#$(optional anonip-configuration-increment "--increment")
               '#$(optional anonip-configuration-replacement
                            "--replacement")
               '#$(optional anonip-configuration-delimiter "--delimiter")
               '#$(optional anonip-configuration-regex "--regex"))
              ;; Run in a UTF-8 locale
              #:environment-variables
              (list (string-append "GUIX_LOCPATH=" #$glibc-utf8-locales
                                   "/lib/locale")
                    "LC_ALL=en_US.utf8")))

           (let ((stat (stat #$input #f)))
             (cond ((not stat)
                    (mknod #$input 'fifo #o600 0)
                    (spawn))
                   ((eq? 'fifo (stat:type stat))
                    (spawn))
                   (else
                    (format #t "'~a' is not a FIFO; bailing out~%"
                            #$input)
                    #f)))))
      (stop #~(make-kill-destructor))))))

(define anonip-service-type
  (service-type
   (name 'anonip)
   (extensions
    (list (service-extension shepherd-root-service-type
                             anonip-shepherd-service)
          (service-extension activation-service-type
                             anonip-activation)))
   (description
    "Provide web server log anonymization with @command{anonip}.")))


;;;
;;; Varnish
;;;

(define-record-type* <varnish-configuration>
  varnish-configuration make-varnish-configuration
  varnish-configuration?
  (package             varnish-configuration-package          ;file-like
                       (default varnish))
  (name                varnish-configuration-name             ;string
                       (default "default"))
  (backend             varnish-configuration-backend          ;string
                       (default "localhost:8080"))
  (vcl                 varnish-configuration-vcl              ;#f | <file-like>
                       (default #f))
  (listen              varnish-configuration-listen           ;list of strings
                       (default '("localhost:80")))
  (storage             varnish-configuration-storage          ;list of strings
                       (default '("malloc,128m")))
  (parameters          varnish-configuration-parameters       ;list of string pairs
                       (default '()))
  (extra-options       varnish-configuration-extra-options    ;list of strings
                       (default '())))

(define %varnish-accounts
  (list (user-group
         (name "varnish")
         (system? #t))
        (user-account
         (name "varnish")
         (group "varnish")
         (system? #t)
         (comment "Varnish Cache User")
         (home-directory "/var/varnish")
         (shell (file-append shadow "/sbin/nologin")))))

(define varnish-shepherd-service
  (match-lambda
    (($ <varnish-configuration> package name backend vcl listen storage
                                parameters extra-options)
     (list (shepherd-service
            (provision (list (symbol-append 'varnish- (string->symbol name))))
            (documentation (string-append "The Varnish Web Accelerator"
                                          " (" name ")"))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append package "/sbin/varnishd")
                            "-n" #$name
                            #$@(if vcl
                                   #~("-f" #$vcl)
                                   #~("-b" #$backend))
                            #$@(append-map (lambda (a) (list "-a" a)) listen)
                            #$@(append-map (lambda (s) (list "-s" s)) storage)
                            #$@(append-map (lambda (p)
                                             (list "-p" (format #f "~a=~a"
                                                                (car p) (cdr p))))
                                           parameters)
                            #$@extra-options)
                      ;; Varnish will drop privileges to the "varnish" user when
                      ;; it exists.  Not passing #:user here allows the service
                      ;; to bind to ports < 1024.
                      #:pid-file (if (string-prefix? "/" #$name)
                                     (string-append #$name "/_.pid")
                                     (string-append "/var/varnish/" #$name "/_.pid"))))
            (stop #~(make-kill-destructor)))))))

(define varnish-service-type
  (service-type
   (name 'varnish)
   (description "Run the Varnish cache server.")
   (extensions
    (list (service-extension account-service-type
                             (const %varnish-accounts))
          (service-extension shepherd-root-service-type
                             varnish-shepherd-service)))
   (default-value
     (varnish-configuration))))


;;;
;;; Patchwork
;;;

(define-record-type* <patchwork-database-configuration>
  patchwork-database-configuration make-patchwork-database-configuration
  patchwork-database-configuration?
  (engine          patchwork-database-configuration-engine
                   (default "django.db.backends.postgresql_psycopg2"))
  (name            patchwork-database-configuration-name
                   (default "patchwork"))
  (user            patchwork-database-configuration-user
                   (default "httpd"))
  (password        patchwork-database-configuration-password
                   (default ""))
  (host            patchwork-database-configuration-host
                   (default ""))
  (port            patchwork-database-configuration-port
                   (default "")))

(define-record-type* <patchwork-settings-module>
  patchwork-settings-module make-patchwork-settings-module
  patchwork-settings-module?
  (database-configuration    patchwork-settings-module-database-configuration
                             (default (patchwork-database-configuration)))
  (secret-key-file           patchwork-settings-module-secret-key-file
                             (default "/etc/patchwork/django-secret-key"))
  (allowed-hosts             patchwork-settings-module-allowed-hosts)
  (default-from-email        patchwork-settings-module-default-from-email)
  (static-url                patchwork-settings-module-static-url
                             (default "/static/"))
  (admins                    patchwork-settings-module-admins
                             (default '()))
  (debug?                    patchwork-settings-module-debug?
                             (default #f))
  (enable-rest-api?          patchwork-settings-module-enable-rest-api?
                             (default #t))
  (enable-xmlrpc?            patchwork-settings-module-enable-xmlrpc?
                             (default #t))
  (force-https-links?        patchwork-settings-module-force-https-links?
                             (default #t))
  (extra-settings            patchwork-settings-module-extra-settings
                             (default "")))

(define-record-type* <patchwork-configuration>
  patchwork-configuration make-patchwork-configuration
  patchwork-configuration?
  (patchwork                patchwork-configuration-patchwork
                            (default patchwork))
  (domain                   patchwork-configuration-domain)
  (settings-module          patchwork-configuration-settings-module)
  (static-path              patchwork-configuration-static-url
                            (default "/static/"))
  (getmail-retriever-config getmail-retriever-config))

;; Django uses a Python module for configuration, so this compiler generates a
;; Python module from the configuration record.
(define-gexp-compiler (patchwork-settings-module-compiler
                       (file <patchwork-settings-module>) system target)
  (match file
    (($ <patchwork-settings-module> database-configuration secret-key-file
                                    allowed-hosts default-from-email
                                    static-url admins debug? enable-rest-api?
                                    enable-xmlrpc? force-https-links?
                                    extra-configuration)
     (gexp->derivation
      "patchwork-settings"
      (with-imported-modules '((guix build utils))
        #~(let ((output #$output))
            (define (create-__init__.py filename)
              (call-with-output-file filename
                (lambda (port) (display "" port))))

            (use-modules (guix build utils)
                         (srfi srfi-1))

            (mkdir-p (string-append output "/guix/patchwork"))
            (create-__init__.py
             (string-append output "/guix/__init__.py"))
            (create-__init__.py
             (string-append output "/guix/patchwork/__init__.py"))

            (call-with-output-file
                (string-append output "/guix/patchwork/settings.py")
              (lambda (port)
                (display
                 (string-append "from patchwork.settings.base import *

# Configuration from Guix
with open('" #$secret-key-file "') as f:
    SECRET_KEY = f.read().strip()

ALLOWED_HOSTS = [
" #$(string-concatenate
     (map (lambda (allowed-host)
            (string-append "  '" allowed-host "'\n"))
          allowed-hosts))
"]

DEFAULT_AUTO_FIELD = 'django.db.models.AutoField'

DEFAULT_FROM_EMAIL = '" #$default-from-email "'
SERVER_EMAIL = DEFAULT_FROM_EMAIL
NOTIFICATION_FROM_EMAIL = DEFAULT_FROM_EMAIL

ADMINS = [
" #$(string-concatenate
     (map (match-lambda
            ((name email-address)
             (string-append
              "('" name "','" email-address "'),")))
          admins))
"]

DEBUG = " #$(if debug? "True" "False") "

ENABLE_REST_API = " #$(if enable-rest-api? "True" "False") "
ENABLE_XMLRPC = " #$(if enable-xmlrpc? "True" "False") "

FORCE_HTTPS_LINKS = " #$(if force-https-links? "True" "False") "

DATABASES = {
    'default': {
" #$(match database-configuration
      (($ <patchwork-database-configuration>
          engine name user password host port)
       (string-append
        "        'ENGINE': '" engine "',\n"
        "        'NAME': '" name "',\n"
        "        'USER': '" user "',\n"
        "        'PASSWORD': '" password "',\n"
        "        'HOST': '" host "',\n"
        "        'PORT': '" port "',\n"))) "
    },
}

" #$(if debug?
        #~(string-append "STATIC_ROOT = '"
                         #$(file-append patchwork "/share/patchwork/htdocs")
                         "'")
        #~(string-append "STATIC_URL = '" #$static-url "'")) "

STATICFILES_STORAGE = (
  'django.contrib.staticfiles.storage.StaticFilesStorage'
)

# Guix Extra Configuration
" #$extra-configuration "
") port)))
            #t))
      #:local-build? #t))))

(define patchwork-virtualhost
  (match-lambda
    (($ <patchwork-configuration> patchwork domain
                                  settings-module static-path
                                  getmail-retriever-config)
     (define wsgi.py
       (file-append patchwork
                    (string-append
                     "/lib/python"
                     (version-major+minor
                      (package-version python))
                     "/site-packages/patchwork/wsgi.py")))

     (httpd-virtualhost
      "*:8080"
      `("ServerAdmin admin@example.com`
ServerName " ,domain "

LogFormat \"%v %h %l %u %t \\\"%r\\\" %>s %b \\\"%{Referer}i\\\" \\\"%{User-Agent}i\\\"\" customformat
LogLevel info
CustomLog \"/var/log/httpd/" ,domain "-access_log\" customformat

ErrorLog /var/log/httpd/error.log

WSGIScriptAlias / " ,wsgi.py "
WSGIDaemonProcess " ,(package-name patchwork) " user=httpd group=httpd processes=1 threads=2 display-name=%{GROUP} lang='en_US.UTF-8' locale='en_US.UTF-8' python-path=" ,settings-module "
WSGIProcessGroup " ,(package-name patchwork) "
WSGIPassAuthorization On

<Files " ,wsgi.py ">
  Require all granted
</Files>

" ,@(if static-path
        `("Alias " ,static-path " " ,patchwork "/share/patchwork/htdocs/")
        '())
"
<Directory \"/srv/http/" ,domain "/\">
    AllowOverride None
    Options MultiViews Indexes SymlinksIfOwnerMatch IncludesNoExec
    Require method GET POST OPTIONS
</Directory>")))))

(define (patchwork-httpd-configuration patchwork-configuration)
  (list "WSGISocketPrefix /var/run/mod_wsgi"
        (list "LoadModule wsgi_module "
              (file-append mod-wsgi "/modules/mod_wsgi.so"))
        (patchwork-virtualhost patchwork-configuration)))

(define (patchwork-django-admin-gexp patchwork settings-module)
  #~(lambda command
      (let ((pid (primitive-fork))
            (user (getpwnam "httpd")))
        (if (eq? pid 0)
            (dynamic-wind
              (const #t)
              (lambda ()
                (setgid (passwd:gid user))
                (setuid (passwd:uid user))

                (setenv "DJANGO_SETTINGS_MODULE" "guix.patchwork.settings")
                (setenv "PYTHONPATH" #$settings-module)
                (primitive-exit
                 (if (zero?
                      (apply system*
                             #$(file-append patchwork "/bin/patchwork-admin")
                             command))
                     0
                     1)))
              (lambda ()
                (primitive-exit 1)))
            (zero? (cdr (waitpid pid)))))))

(define (patchwork-django-admin-action patchwork settings-module)
  (shepherd-action
   (name 'django-admin)
   (documentation
    "Run a django admin command for patchwork")
   (procedure (patchwork-django-admin-gexp patchwork settings-module))))

(define patchwork-shepherd-services
  (match-lambda
    (($ <patchwork-configuration> patchwork domain
                                  settings-module static-path
                                  getmail-retriever-config)
     (define secret-key-file-creation-gexp
       (if (patchwork-settings-module? settings-module)
           (with-extensions (list guile-gcrypt)
             #~(let ((secret-key-file
                      #$(patchwork-settings-module-secret-key-file
                         settings-module)))
                 (use-modules (guix build utils)
                              (gcrypt random))

                 (unless (file-exists? secret-key-file)
                   (mkdir-p (dirname secret-key-file))
                   (call-with-output-file secret-key-file
                     (lambda (port)
                       (display (random-token 30 'very-strong) port)))
                   (let* ((pw  (getpwnam "httpd"))
                          (uid (passwd:uid pw))
                          (gid (passwd:gid pw)))
                     (chown secret-key-file uid gid)
                     (chmod secret-key-file #o400)))))
           #~()))

     (list (shepherd-service
            (requirement '(postgres))
            (provision (list (string->symbol
                              (string-append (package-name patchwork)
                                             "-setup"))))
            (start
               #~(lambda ()
                   (define run-django-admin-command
                     #$(patchwork-django-admin-gexp patchwork
                                                    settings-module))

                   #$secret-key-file-creation-gexp

                   (run-django-admin-command "migrate")))
            (stop #~(const #f))
            (actions
             (list (patchwork-django-admin-action patchwork
                                                  settings-module)))
            (respawn? #f)
            (documentation "Setup Patchwork."))))))

(define patchwork-getmail-configs
  (match-lambda
    (($ <patchwork-configuration> patchwork domain
                                  settings-module static-path
                                  getmail-retriever-config)
     (list
      (getmail-configuration
       (name (string->symbol (package-name patchwork)))
       (user "httpd")
       (directory (string-append
                   "/var/lib/getmail/" (package-name patchwork)))
       (rcfile
        (getmail-configuration-file
         (retriever getmail-retriever-config)
         (destination
          (getmail-destination-configuration
           (type "MDA_external")
           (path (file-append patchwork "/bin/patchwork-admin"))
           (extra-parameters
            '((arguments . ("parsemail"))))))
         (options
          (getmail-options-configuration
           (read-all #f)
           (delivered-to #f)
           (received #f)))))
       (idle (assq-ref
              (getmail-retriever-configuration-extra-parameters
               getmail-retriever-config)
              'mailboxes))
       (environment-variables
        (list "DJANGO_SETTINGS_MODULE=guix.patchwork.settings"
              #~(string-append "PYTHONPATH=" #$settings-module))))))))

(define patchwork-service-type
  (service-type
   (name 'patchwork-setup)
   (extensions
    (list (service-extension httpd-service-type
                             patchwork-httpd-configuration)
          (service-extension shepherd-root-service-type
                             patchwork-shepherd-services)
          (service-extension getmail-service-type
                             patchwork-getmail-configs)))
   (description
    "Patchwork patch tracking system.")))


;;;
;;; Mumi.
;;;

(define-record-type* <mumi-configuration>
  mumi-configuration make-mumi-configuration
  mumi-configuration?
  (mumi    mumi-configuration-mumi (default mumi))
  (mailer? mumi-configuration-mailer? (default #t))
  (sender  mumi-configuration-sender (default #f))
  (smtp    mumi-configuration-smtp (default #f)))

(define %mumi-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (mkdir-p "/var/mumi/db")
        (mkdir-p "/var/mumi/mails")
        (let* ((pw  (getpwnam "mumi"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (chown "/var/mumi" uid gid)
          (chown "/var/mumi/mails" uid gid)
          (chown "/var/mumi/db" uid gid)))))

(define %mumi-accounts
  (list (user-group (name "mumi") (system? #t))
        (user-account
         (name "mumi")
         (group "mumi")
         (system? #t)
         (comment "Mumi web server")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %mumi-log "/var/log/mumi.log")

(define %mumi-mailer-log "/var/log/mumi.mailer.log")

(define %mumi-worker-log "/var/log/mumi.worker.log")

(define (mumi-shepherd-services config)
  (define environment
    #~(list "LC_ALL=en_US.utf8"
            (string-append "GUIX_LOCPATH=" #$glibc-utf8-locales
                           "/lib/locale")))

  (match config
    (($ <mumi-configuration> mumi mailer? sender smtp)
     (list (shepherd-service
            (provision '(mumi))
            (documentation "Mumi bug-tracking web interface.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      `(#$(file-append mumi "/bin/mumi") "web"
                        ,@(if #$mailer? '() '("--disable-mailer")))
                      #:environment-variables #$environment
                      #:user "mumi" #:group "mumi"
                      #:log-file #$%mumi-log))
            (stop #~(make-kill-destructor)))
           (shepherd-service
            (provision '(mumi-worker))
            (documentation "Mumi bug-tracking web interface database worker.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append mumi "/bin/mumi") "worker")
                      #:environment-variables #$environment
                      #:user "mumi" #:group "mumi"
                      #:log-file #$%mumi-worker-log))
            (stop #~(make-kill-destructor)))
           (shepherd-service
            (provision '(mumi-mailer))
            (documentation "Mumi bug-tracking web interface mailer.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      `(#$(file-append mumi "/bin/mumi") "mailer"
                        ,@(if #$sender
                              (list (string-append "--sender=" #$sender))
                              '())
                        ,@(if #$smtp
                              (list (string-append "--smtp=" #$smtp))
                              '()))
                      #:environment-variables #$environment
                      #:user "mumi" #:group "mumi"
                      #:log-file #$%mumi-mailer-log))
            (stop #~(make-kill-destructor)))))))

(define %mumi-log-rotations
  (list (log-rotation
         (files (list %mumi-log
                      %mumi-mailer-log
                      %mumi-worker-log)))))

(define mumi-service-type
  (service-type
   (name 'mumi)
   (extensions
    (list (service-extension activation-service-type
                             (const %mumi-activation))
          (service-extension account-service-type
                             (const %mumi-accounts))
          (service-extension shepherd-root-service-type
                             mumi-shepherd-services)
          (service-extension rottlog-service-type
                             (const %mumi-log-rotations))))
   (description
    "Run Mumi, a Web interface to the Debbugs bug-tracking server.")
   (default-value
     (mumi-configuration))))

(define %default-gmnisrv-config-file
  (plain-file "gmnisrv.ini" "
listen=0.0.0.0:1965 [::]:1965

[:tls]
store=/var/lib/gemini/certs

organization=gmnisrv on Guix user

[localhost]
root=/srv/gemini
"))

(define-record-type* <gmnisrv-configuration>
  gmnisrv-configuration make-gmnisrv-configuration
  gmnisrv-configuration?
  (package     gmnisrv-configuration-package
               (default gmnisrv))
  (config-file gmnisrv-configuration-config-file
               (default %default-gmnisrv-config-file)))

(define gmnisrv-shepherd-service
  (match-lambda
    (($ <gmnisrv-configuration> package config-file)
     (list (shepherd-service
            (provision '(gmnisrv))
            (requirement '(networking))
            (documentation "Run the gmnisrv Gemini server.")
            (start (let ((gmnisrv (file-append package "/bin/gmnisrv")))
                     #~(make-forkexec-constructor
                        (list #$gmnisrv "-C" #$config-file)
                        #:user "gmnisrv" #:group "gmnisrv"
                        #:log-file "/var/log/gmnisrv.log")))
            (stop #~(make-kill-destructor)))))))

(define %gmnisrv-accounts
  (list (user-group (name "gmnisrv") (system? #t))
        (user-account
         (name "gmnisrv")
         (group "gmnisrv")
         (system? #t)
         (comment "gmnisrv Gemini server")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %gmnisrv-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (mkdir-p "/var/lib/gemini/certs")
        (let* ((pw  (getpwnam "gmnisrv"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (chown "/var/lib/gemini" uid gid)
          (chown "/var/lib/gemini/certs" uid gid)))))

(define gmnisrv-service-type
  (service-type
   (name 'guix)
   (extensions
    (list (service-extension activation-service-type
                             (const %gmnisrv-activation))
          (service-extension account-service-type
                             (const %gmnisrv-accounts))
          (service-extension shepherd-root-service-type
                             gmnisrv-shepherd-service)))
   (description
    "Run the gmnisrv Gemini server.")
   (default-value
     (gmnisrv-configuration))))

(define-record-type* <agate-configuration>
  agate-configuration make-agate-configuration
  agate-configuration?
  (package  agate-configuration-package
            (default agate))
  (content  agate-configuration-content
            (default "/srv/gemini"))
  (cert     agate-configuration-cert
            (default #f))
  (key      agate-configuration-key
            (default #f))
  (addr     agate-configuration-addr
            (default '("0.0.0.0:1965" "[::]:1965")))
  (hostname agate-configuration-hostname
            (default #f))
  (lang     agate-configuration-lang
            (default #f))
  (silent?  agate-configuration-silent
            (default #f))
  (serve-secret? agate-configuration-serve-secret
                 (default #f))
  (log-ip?  agate-configuration-log-ip
            (default #t))
  (user     agate-configuration-user
            (default "agate"))
  (group    agate-configuration-group
            (default "agate"))
  (log-file agate-configuration-log
            (default "/var/log/agate.log")))

(define agate-shepherd-service
  (match-lambda
    (($ <agate-configuration> package content cert key addr
                              hostname lang silent? serve-secret?
                              log-ip? user group log-file)
     (list (shepherd-service
            (provision '(agate))
            (requirement '(networking))
            (documentation "Run the agate Gemini server.")
            (start (let ((agate (file-append package "/bin/agate")))
                     #~(make-forkexec-constructor
                        (list #$agate
                              "--content" #$content
                              "--cert" #$cert
                              "--key" #$key
                              "--addr" #$@addr
                              #$@(if lang
                                     (list "--lang" lang)
                                     '())
                              #$@(if hostname
                                     (list "--hostname" hostname)
                                     '())
                              #$@(if silent? '("--silent") '())
                              #$@(if serve-secret? '("--serve-secret") '())
                              #$@(if log-ip? '("--log-ip") '()))
                        #:user #$user #:group #$group
                        #:log-file #$log-file)))
            (stop #~(make-kill-destructor)))))))

(define agate-accounts
  (lambda (config)
    (let ((group (agate-configuration-group config))
          (user (agate-configuration-user config)))
      `(,@(if (equal? group "agate")
              '()
              (list (user-group (name "agate") (system? #t))))
        ,(user-group
          (name group)
          (system? #t))
        ,(user-account
          (name user)
          (group group)
          (supplementary-groups '("agate"))
          (system? #t)
          (comment "agate server user")
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))))

(define agate-service-type
  (service-type
   (name 'agate)
   (extensions
    (list (service-extension account-service-type
                             agate-accounts)
          (service-extension shepherd-root-service-type
                             agate-shepherd-service)))
   (default-value (agate-configuration))
   (description "Run Agate, a simple Gemini protocol server written in
Rust.")))
