;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu home services gnupg)
  #:use-module (guix gexp)
  #:use-module ((guix records) #:select (match-record))
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:autoload   (gnu packages gnupg) (gnupg pinentry)
  #:export (home-gpg-agent-configuration
            home-gpg-agent-configuration?
            home-gpg-agent-configuration-gnupg
            home-gpg-agent-configuration-pinentry-program
            home-gpg-agent-configuration-ssh-support?
            home-gpg-agent-configuration-default-cache-ttl
            home-gpg-agent-configuration-max-cache-ttl
            home-gpg-agent-configuration-max-cache-ttl-ssh
            home-gpg-agent-configuration-extra-content

            home-gpg-agent-service-type))

(define raw-configuration-string? string?)

;; Configuration of 'gpg-agent'.
(define-configuration/no-serialization home-gpg-agent-configuration
  (gnupg
   (file-like gnupg)
   "The GnuPG package to use.")
  (pinentry-program
   (file-like (file-append pinentry "/bin/pinentry-curses"))
   "Pinentry program to use.  Pinentry is a small user interface that
@command{gpg-agent} delegates to anytime it needs user input for a passphrase
or @acronym{PIN, personal identification number} (@pxref{Top,,, pinentry,
Using the PIN-Entry}).")
  (ssh-support?
   (boolean #f)
   "Whether to enable @acronym{SSH, secure shell} support.  When true,
@command{gpg-agent} acts as a drop-in replacement for OpenSSH's
@command{ssh-agent} program, taking care of OpenSSH secret keys and directing
passphrase requests to the chosen Pinentry program.")
  (default-cache-ttl
    (integer 600)
    "Time a cache entry is valid, in seconds.")
  (max-cache-ttl
   (integer 7200)
   "Maximum time a cache entry is valid, in seconds.  After this time a cache
entry will be expired even if it has been accessed recently.")
  (default-cache-ttl-ssh
    (integer 1800)
    "Time a cache entry for SSH keys is valid, in seconds.")
  (max-cache-ttl-ssh
   (integer 7200)
   "Maximum time a cache entry for SSH keys is valid, in seconds.")
  (extra-content
   (raw-configuration-string "")
   "Raw content to add to the end of @file{~/.gnupg/gpg-agent.conf}."))

(define (home-gpg-agent-configuration-file config)
  "Return the @file{gpg-agent.conf} file for @var{config}."
  (match-record config <home-gpg-agent-configuration>
    (pinentry-program default-cache-ttl max-cache-ttl
                      default-cache-ttl-ssh max-cache-ttl-ssh
                      extra-content)
    (mixed-text-file "gpg-agent.conf"
                     "pinentry-program " pinentry-program "\n"
                     "default-cache-ttl "
                     (number->string default-cache-ttl) "\n"
                     "max-cache-ttl "
                     (number->string max-cache-ttl) "\n"
                     "default-cache-ttl-ssh "
                     (number->string default-cache-ttl-ssh) "\n"
                     "max-cache-ttl-ssh "
                     (number->string max-cache-ttl-ssh) "\n"
                     extra-content)))

(define (home-gpg-agent-shepherd-services config)
  "Return the possibly-empty list of Shepherd services for @var{config}."
  (match-record config <home-gpg-agent-configuration>
    (gnupg ssh-support?)
    ;; 'gpg-agent' is started on demand by GnuPG's programs, but it has to be
    ;; started explicitly when OpenSSH support is enabled (info "(gnupg) Agent
    ;; Options").
    (if ssh-support?
        (let ((endpoint (lambda (name socket)
                          #~(endpoint
                             (make-socket-address
                              AF_UNIX
                              (string-append %user-runtime-dir
                                             "/gnupg/" #$socket))
                             #:name #$name
                             #:socket-directory-permissions #o700))))
          (list (shepherd-service
                 (provision '(gpg-agent ssh-agent))
                 (modules '((shepherd support)))  ;for '%user-runtime-dir'
                 (start #~(make-systemd-constructor
                           (list #$(file-append gnupg "/bin/gpg-agent")
                                 "--supervised" "--enable-ssh-support")
                           (list #$(endpoint "ssh" "S.gpg-agent.ssh")
                                 #$(endpoint "browser" "S.gpg-agent.browser")
                                 #$(endpoint "extra" "S.gpg-agent.extra")
                                 ;; #$(endpoint "scdaemon" "S.scdaemon")
                                 #$(endpoint "std" "S.gpg-agent"))))
                 (stop #~(make-systemd-destructor))
                 (documentation "Start 'gpg-agent', the GnuPG passphrase
agent, with support for handling OpenSSH material."))))
        '())))

(define (home-gpg-agent-files config)
  `((".gnupg/gpg-agent.conf" ,(home-gpg-agent-configuration-file config))))

(define (home-gpg-agent-environment-variables config)
  "Return GnuPG environment variables needed for @var{config}."
  (if (home-gpg-agent-configuration-ssh-support? config)
      `(("SSH_AUTH_SOCK"
         . "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"))
      '()))

(define home-gpg-agent-service-type
  (service-type
   (name 'home-gpg-agent)
   (extensions
    (list (service-extension home-files-service-type
                             home-gpg-agent-files)
          (service-extension home-shepherd-service-type
                             home-gpg-agent-shepherd-services)
          (service-extension home-environment-variables-service-type
                             home-gpg-agent-environment-variables)))
   (default-value (home-gpg-agent-configuration))
   (description
    "Configure GnuPG's agent, @command{gpg-agent}, which is responsible for
managing OpenPGP and optionally SSH private keys.  When SSH support is
enabled, @command{gpg-agent} acts as a drop-in replacement for OpenSSH's
@command{ssh-agent}.")))
