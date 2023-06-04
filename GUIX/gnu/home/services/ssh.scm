;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu home services ssh)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix modules)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module ((gnu home services utils)
                #:select (object->camel-case-string))
  #:autoload   (gnu packages base) (glibc-utf8-locales)
  #:use-module (gnu packages ssh)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (home-openssh-configuration
            home-openssh-configuration-authorized-keys
            home-openssh-configuration-known-hosts
            home-openssh-configuration-hosts
            home-ssh-agent-configuration

            openssh-host
            openssh-host-host-name
            openssh-host-identity-file
            openssh-host-name
            openssh-host-port
            openssh-host-user
            openssh-host-forward-x11?
            openssh-host-forward-x11-trusted?
            openssh-host-forward-agent?
            openssh-host-compression?
            openssh-host-proxy-command
            openssh-host-host-key-algorithms
            openssh-host-accepted-key-types
            openssh-host-extra-content
            proxy-jump
            proxy-jump-host-name
            proxy-jump-port
            proxy-jump-user
            proxy-command
            proxy-command->string

            home-openssh-service-type
            home-ssh-agent-service-type))

(define (serialize-field-name name)
  (match name
    ('accepted-key-types "PubkeyAcceptedKeyTypes")
    (_
     (let ((name (let ((str (symbol->string name)))
                   (if (string-suffix? "?" str)
                       (string->symbol (string-drop-right str 1))
                       name))))
       (object->camel-case-string name 'upper)))))

(define (serialize-string field value)
  (string-append "  " (serialize-field-name field)
                 " " value "\n"))

(define (address-family? obj)
  (memv obj (list AF_INET AF_INET6)))

(define-maybe address-family)

(define (serialize-address-family field family)
  (if (maybe-value-set? family)
      (string-append "  " (serialize-field-name field) " "
                     (cond ((= family AF_INET) "inet")
                           ((= family AF_INET6) "inet6")
                           ;; The 'else' branch is unreachable.
                           (else (raise (condition (&error)))))
                     "\n")
      ""))

(define (natural-number? obj)
  (and (integer? obj) (exact? obj) (> obj 0)))

(define (serialize-natural-number field value)
  (string-append "  " (serialize-field-name field) " "
                 (number->string value) "\n"))

(define (serialize-boolean field value)
  (string-append "  " (serialize-field-name field) " "
                 (if value "yes" "no") "\n"))

(define-maybe string)
(define-maybe natural-number)

(define (serialize-raw-configuration-string field value)
  (string-append value "\n"))
(define raw-configuration-string? string?)

(define (string-list? lst)
  (and (pair? lst) (every string? lst)))
(define (serialize-string-list field lst)
  (string-append "  " (serialize-field-name field) " "
                 (string-join lst ",") "\n"))

(define-maybe string-list)

(define-record-type <proxy-command>
  (proxy-command command)
  proxy-command?
  (command proxy-command->string))

(set-record-type-printer! <proxy-command>
  (lambda (obj port)
    (format port "#<proxy-command ~s>" (proxy-command->string obj))))

(define-configuration/no-serialization proxy-jump
  (user
   maybe-string
   "User name on the remote host.")
  (host-name
   (string)
   "Host name---e.g., @code{foo.example.org} or @code{192.168.1.2}.")
  (port
   maybe-natural-number
   "TCP port number to connect to."))

(define (proxy-jump->string proxy-jump)
  (match-record proxy-jump <proxy-jump>
    (host-name user port)
    (string-append
      (if (maybe-value-set? user) (string-append user "@") "")
      host-name
      (if (maybe-value-set? port) (string-append ":" (number->string port)) ""))))

(define (proxy-command-or-jump-list? x)
  (or (proxy-command? x)
      (and (list? x)
           (every proxy-jump? x))))

(define (serialize-proxy-command-or-jump-list field value)
  (if (proxy-command? value)
    (serialize-string 'proxy-command (proxy-command->string value))
    (serialize-string-list 'proxy-jump (map proxy-jump->string value))))

(define-maybe proxy-command-or-jump-list)

(define (sanitize-proxy-command properties)
  (lambda (value)
  (when (maybe-value-set? value)
    (warn-about-deprecation 'proxy-command properties #:replacement 'proxy))
  (unless (maybe-string? value)
    (configuration-field-error (source-properties->location properties) 'proxy-command value))
  value))

(define-configuration openssh-host
  (name
   (string)
   "Name of this host declaration.")
  (host-name
   maybe-string
   "Host name---e.g., @code{\"foo.example.org\"} or @code{\"192.168.1.2\"}.")
  (address-family
   maybe-address-family
   "Address family to use when connecting to this host: one of
@code{AF_INET} (for IPv4 only), @code{AF_INET6} (for IPv6 only).
Additionally, the field can be left unset to allow any address family.")
  (identity-file
   maybe-string
   "The identity file to use---e.g.,
@code{\"/home/charlie/.ssh/id_ed25519\"}.")
  (port
   maybe-natural-number
   "TCP port number to connect to.")
  (user
   maybe-string
   "User name on the remote host.")
  (forward-x11?
   (boolean #f)
   "Whether to forward remote client connections to the local X11 graphical
display.")
  (forward-x11-trusted?
   (boolean #f)
   "Whether remote X11 clients have full access to the original X11 graphical
display.")
  (forward-agent?
   (boolean #f)
   "Whether the authentication agent (if any) is forwarded to the remote
machine.")
  (compression?
   (boolean #f)
   "Whether to compress data in transit.")
  (proxy-command
   maybe-string
   "The command to use to connect to the server.  As an example, a command
to connect via an HTTP proxy at 192.0.2.0 would be: @code{\"nc -X
connect -x 192.0.2.0:8080 %h %p\"}.  Using 'proxy-command' is deprecated, use
'proxy' instead."
   (sanitizer (sanitize-proxy-command (current-source-location))))
  (proxy
   maybe-proxy-command-or-jump-list
   "The command to use to connect to the server or a list of SSH hosts to jump
through before connecting to the server.")
  (host-key-algorithms
   maybe-string-list
   "The list of accepted host key algorithms---e.g.,
@code{'(\"ssh-ed25519\")}.")
  (accepted-key-types
   maybe-string-list
   "The list of accepted user public key types.")
  (extra-content
   (raw-configuration-string "")
   "Extra content appended as-is to this @code{Host} block in
@file{~/.ssh/config}."))

(define (serialize-openssh-host config)
  (define (openssh-host-name-field? field)
    (eq? (configuration-field-name field) 'name))

  (string-append
   "Host " (openssh-host-name config) "\n"
   (string-concatenate
    (map (lambda (field)
           ((configuration-field-serializer field)
            (configuration-field-name field)
            ((configuration-field-getter field) config)))
         (remove openssh-host-name-field?
                 openssh-host-fields)))))

(define-record-type* <home-openssh-configuration>
  home-openssh-configuration make-home-openssh-configuration
  home-openssh-configuration?
  (authorized-keys home-openssh-configuration-authorized-keys ;list of file-like
                   (default #f))
  (known-hosts     home-openssh-configuration-known-hosts ;unspec | list of file-like
                   (default *unspecified*))
  (hosts           home-openssh-configuration-hosts   ;list of <openssh-host>
                   (default '())))

(define (openssh-configuration->string config)
  (string-join (map serialize-openssh-host
                    (home-openssh-configuration-hosts config))
               "\n"))

(define* (file-join name files #:optional (delimiter " "))
  "Return a file in the store called @var{name} that is the concatenation
of all the file-like objects listed in @var{files}, with @var{delimited}
inserted after each of them."
  (computed-file name
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils))

                       ;; Support non-ASCII file names.
                       (setenv "GUIX_LOCPATH"
                               #+(file-append glibc-utf8-locales
                                              "/lib/locale"))
                       (setlocale LC_ALL "en_US.utf8")

                       (call-with-output-file #$output
                         (lambda (output)
                           (for-each (lambda (file)
                                       (call-with-input-file file
                                         (lambda (input)
                                           (dump-port input output)))
                                       (display #$delimiter output))
                                     '#$files)))))))

(define (openssh-configuration-files config)
  (let* ((ssh-config (plain-file "ssh.conf"
                                 (openssh-configuration->string config)))
         (known-hosts (home-openssh-configuration-known-hosts config))
         (authorized-keys (home-openssh-configuration-authorized-keys config))
         (authorized-keys (and
                           authorized-keys
                           (file-join "authorized_keys" authorized-keys "\n"))))
    `(,@(if authorized-keys
            `((".ssh/authorized_keys" ,authorized-keys))
            '())
      ,@(if (unspecified? known-hosts)
            '()
            `((".ssh/known_hosts"
               ,(file-join "known_hosts" known-hosts "\n"))))
      (".ssh/config" ,ssh-config))))

(define openssh-activation
  (with-imported-modules (source-module-closure
                          '((gnu build activation)))
    #~(begin
        (use-modules (gnu build activation))

        ;; Make sure ~/.ssh is #o700.
        (let* ((home (getenv "HOME"))
               (dot-ssh (string-append home "/.ssh")))
          (mkdir-p/perms dot-ssh (getpw (getuid)) #o700)))))

(define home-openssh-service-type
  (service-type
   (name 'home-openssh)
   (extensions
    (list (service-extension home-files-service-type
                             openssh-configuration-files)
          (service-extension home-activation-service-type
                             (const openssh-activation))))
   (description "Configure the OpenSSH @acronym{SSH, secure shell} client
by providing a @file{~/.ssh/config} file, which is honored by the OpenSSH
client,@command{ssh}, and by other tools such as @command{guix deploy}.")
   (default-value (home-openssh-configuration))))


;;;
;;; Ssh-agent.
;;;
(define-record-type* <home-ssh-agent-configuration>
  home-ssh-agent-configuration make-home-ssh-agent-configuration
  home-ssh-agent-configuration?
  (openssh          home-ssh-agent-openssh          ;file-like
                    (default openssh))
  (socket-directory home-ssh-agent-socket-directory ;string
                    (default #~(string-append %user-runtime-dir "/ssh-agent")))
  (extra-options    home-ssh-agent-extra-options    ;list of string
                    (default '())))

(define (home-ssh-agent-services config)
  "Return a <shepherd-service> for an ssh-agent with CONFIG."
  (match-record config <home-ssh-agent-configuration>
    (openssh socket-directory extra-options)
    (let* ((ssh-agent (file-append openssh "/bin/ssh-agent"))
           (socket-file #~(string-append #$socket-directory "/socket"))
           (command #~`(#$ssh-agent
                        "-D" "-a" ,#$socket-file
                        #$@extra-options))
           (log-file #~(string-append %user-log-dir "/ssh-agent.log")))
      (list (shepherd-service
             (documentation "Run the ssh-agent.")
             (provision '(ssh-agent))
             (modules '((shepherd support)))    ;for '%user-runtime-dir', etc.
             (start #~(lambda _
                        (unless (file-exists? #$socket-directory)
                          (mkdir-p #$socket-directory)
                          (chmod #$socket-directory #o700))
                        (fork+exec-command #$command #:log-file #$log-file)))
             (stop #~(make-kill-destructor)))))))

(define (home-ssh-agent-environment-variables config)
  '(("SSH_AUTH_SOCK"
     . "${SSH_AUTH_SOCK-${XDG_RUNTIME_DIR-$HOME/.cache}/ssh-agent/socket}")))

(define home-ssh-agent-service-type
  (service-type
   (name 'home-ssh-agent)
   (default-value (home-ssh-agent-configuration))
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-ssh-agent-services)
          (service-extension home-environment-variables-service-type
                             home-ssh-agent-environment-variables)))
   (description
    "Install and configure @command{ssh-agent} as a Shepherd service.")))
