;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services games)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages games)
  #:use-module (gnu system shadow)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu build linux-container)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (joycond-configuration
            joycond-configuration?
            joycond-service-type

            wesnothd-configuration
            wesnothd-configuration?
            wesnothd-service-type))

;;;
;;; Joycond
;;;

(define-configuration/no-serialization joycond-configuration
  (package (package joycond) "The joycond package to use"))

(define (joycond-shepherd-service config)
  (let ((joycond (joycond-configuration-package config)))
    (list (shepherd-service
           (documentation "Run joycond.")
           (provision '(joycond))
           (requirement '(bluetooth))
           (start #~(make-forkexec-constructor
                     (list #$(file-append joycond "/bin/joycond"))))
           (stop #~(make-kill-destructor))))))

(define joycond-service-type
  (service-type
   (name 'joycond)
   (description
    "Run @command{joycond} for pairing Nintendo joycons via Bluetooth.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             joycond-shepherd-service)))
   (default-value (joycond-configuration))))


;;;
;;; The Battle for Wesnoth server
;;;

(define-record-type* <wesnothd-configuration>
  wesnothd-configuration make-wesnothd-configuration wesnothd-configuration?
  (package wesnothd-configuration-package
           (default wesnoth-server))
  (port wesnothd-configuration-port
        (default 15000)))

(define %wesnothd-accounts
  (list (user-account
         (name "wesnothd")
         (group "wesnothd")
         (system? #t)
         (comment "Wesnoth daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "wesnothd")
         (system? #t))))

(define wesnothd-shepherd-service
  (match-lambda
    (($ <wesnothd-configuration> package port)
     (let ((wesnothd (least-authority-wrapper
                      (file-append package "/bin/wesnothd")
                      #:name "wesnothd"
                      #:mappings (list (file-system-mapping
                                        (source "/var/run/wesnothd")
                                        (target source)
                                        (writable? #t)))
                      #:namespaces (delq 'net %namespaces))))
       (shepherd-service
        (documentation "The Battle for Wesnoth server")
        (provision '(wesnoth-daemon))
        (requirement '(networking))
        (start #~(make-forkexec-constructor
                  (list #$wesnothd "-p" #$(number->string port))
                  #:user "wesnothd" #:group "wesnothd"))
        (stop #~(make-kill-destructor)))))))

(define wesnothd-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (let* ((user (getpw "wesnothd"))
               (directory "/var/run/wesnothd"))
          ;; wesnothd creates a Unix-domain socket in DIRECTORY.
          (mkdir-p directory)
          (chown directory (passwd:uid user) (passwd:gid user))))))

(define wesnothd-service-type
  (service-type
   (name 'wesnothd)
   (description
    "Run The Battle for Wesnoth server @command{wesnothd}.")
   (extensions
    (list (service-extension account-service-type
                             (const %wesnothd-accounts))
          (service-extension activation-service-type
                             (const wesnothd-activation))
          (service-extension shepherd-root-service-type
                             (compose list wesnothd-shepherd-service))))
   (default-value (wesnothd-configuration))))
