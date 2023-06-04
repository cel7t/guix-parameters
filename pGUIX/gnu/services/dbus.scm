;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu services dbus)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages admin)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module ((guix packages) #:select (package-name))
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (dbus-configuration
            dbus-configuration?
            dbus-root-service-type
            dbus-service  ; deprecated
            wrapped-dbus-service

            polkit-configuration
            polkit-configuration?
            polkit-service-type
            polkit-service))  ; deprecated

;;;
;;; D-Bus.
;;;

(define-record-type* <dbus-configuration>
  dbus-configuration make-dbus-configuration
  dbus-configuration?
  (dbus      dbus-configuration-dbus              ;file-like
             (default dbus))
  (services  dbus-configuration-services          ;list of <package>
             (default '()))
  (verbose?  dbus-configuration-verbose?          ;boolean
             (default #f)))

(define (system-service-directory services)
  "Return the system service directory, containing @code{.service} files for
all the services that may be activated by the daemon."
  (computed-file "dbus-system-services"
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define files
                         (append-map (lambda (service)
                                       (find-files
                                        (string-append
                                         service
                                         "/share/dbus-1/")
                                        "\\.service$"))
                                     (list #$@services)))

                       (mkdir #$output)
                       (for-each (lambda (file)
                                   (symlink file
                                            (string-append #$output "/"
                                                           (basename file))))
                                 files)
                       #t))))

(define (dbus-configuration-directory services)
  "Return a directory contains the @code{system-local.conf} file for DBUS that
includes the @code{etc/dbus-1/system.d} directories of each package listed in
@var{services}."
  (define build
    #~(begin
        (use-modules (sxml simple)
                     (srfi srfi-1))

        (define-syntax directives
          (syntax-rules ()
            ;; Expand the given directives (SXML expressions) only if their
            ;; key names a file that exists.
            ((_ (name directory) rest ...)
             (let ((dir directory))
               (if (file-exists? dir)
                   `((name ,dir)
                     ,@(directives rest ...))
                   (directives rest ...))))
            ((_)
             '())))

        (define (services->sxml services)
          ;; Return the SXML 'includedir' clauses for DIRS.
          `(busconfig
             ;; Increase this timeout to 300 seconds to work around race-y
             ;; failures such as <https://issues.guix.gnu.org/52051> on slow
             ;; computers with slow I/O.
            (limit (@ (name "auth_timeout")) "300000")
            (servicehelper "/run/setuid-programs/dbus-daemon-launch-helper")

            ;; First, the '.service' files of services subject to activation.
            ;; We use a fixed location under /etc because the setuid helper
            ;; looks for them in that location and nowhere else.  See
            ;; <https://bugs.freedesktop.org/show_bug.cgi?id=92458>.
            (servicedir "/etc/dbus-1/system-services")

            ,@(append-map (lambda (dir)
                            (directives
                             (includedir
                              (string-append dir "/etc/dbus-1/system.d"))
                             (includedir
                              (string-append dir "/share/dbus-1/system.d"))
                             (servicedir          ;for '.service' files
                              (string-append dir "/share/dbus-1/services"))))
                          services)))

        (mkdir #$output)

        ;; Provide /etc/dbus-1/system-services, which is where the setuid
        ;; helper looks for system service files.
        (symlink #$(system-service-directory services)
                 (string-append #$output "/system-services"))

        ;; 'system-local.conf' is automatically included by the default
        ;; 'system.conf', so this is where we stuff our own things.
        (call-with-output-file (string-append #$output "/system-local.conf")
          (lambda (port)
            (sxml->xml (services->sxml (list #$@services))
                       port)))))

  (computed-file "dbus-configuration" build))

(define (dbus-etc-files config)
  "Return a list of FILES for @var{etc-service-type} to build the
@code{/etc/dbus-1} directory."
  (list `("dbus-1" ,(dbus-configuration-directory
                     (dbus-configuration-services config)))))

(define %dbus-accounts
  ;; Accounts used by the system bus.
  (list (user-group (name "messagebus") (system? #t))
        (user-account
         (name "messagebus")
         (group "messagebus")
         (system? #t)
         (comment "D-Bus system bus user")
         (home-directory "/var/run/dbus")
         (shell (file-append shadow "/sbin/nologin")))))

(define dbus-setuid-programs
  ;; Return a list of <setuid-program> for the program that we need.
  (match-lambda
    (($ <dbus-configuration> dbus services)
     (list (setuid-program
            (program (file-append
                      dbus "/libexec/dbus-daemon-launch-helper")))))))

(define (dbus-activation config)
  "Return an activation gexp for D-Bus using @var{config}."
  (with-imported-modules (source-module-closure
                          '((gnu build activation)
                            (guix build utils)))
    #~(begin
        (use-modules (gnu build activation)
                     (guix build utils))

        (let ((user (getpwnam "messagebus")))
          ;; This directory contains the daemon's socket so it must be
          ;; world-readable.
          (mkdir-p/perms "/var/run/dbus" user #o755))

        (unless (file-exists? "/etc/machine-id")
          (format #t "creating /etc/machine-id...~%")
          (invoke (string-append #$(dbus-configuration-dbus config)
                                 "/bin/dbus-uuidgen")
                  "--ensure=/etc/machine-id")))))

(define dbus-shepherd-service
  (match-lambda
    (($ <dbus-configuration> dbus _ verbose?)
     (list (shepherd-service
            (documentation "Run the D-Bus system daemon.")
            (provision '(dbus-system))
            (requirement '(user-processes syslogd))
            (start #~(make-forkexec-constructor
                      (list (string-append #$dbus "/bin/dbus-daemon")
                            "--nofork" "--system" "--syslog-only")
                      #$@(if verbose?
                             ;; Since the verbose output goes to the console,
                             ;; not syslog, add a log file to capture it.
                             '(#:environment-variables '("DBUS_VERBOSE=1")
                               #:log-file "/var/log/dbus-daemon.log")
                             '())
                      #:pid-file "/var/run/dbus/pid"))
            (stop #~(make-kill-destructor)))))))

(define dbus-root-service-type
  (service-type (name 'dbus)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          dbus-shepherd-service)
                       (service-extension activation-service-type
                                          dbus-activation)
                       (service-extension etc-service-type
                                          dbus-etc-files)
                       (service-extension account-service-type
                                          (const %dbus-accounts))
                       (service-extension setuid-program-service-type
                                          dbus-setuid-programs)))

                ;; Extensions consist of lists of packages (representing D-Bus
                ;; services) that we just concatenate.
                (compose concatenate)

                ;; The service's parameters field is extended by augmenting
                ;; its <dbus-configuration> 'services' field.
                (extend (lambda (config services)
                          (dbus-configuration
                           (inherit config)
                           (services
                            (append (dbus-configuration-services config)
                                    services)))))

                (default-value (dbus-configuration))
                (description "Run the system-wide D-Bus inter-process message
bus.  It allows programs and daemons to communicate and is also responsible
for spawning (@dfn{activating}) D-Bus services on demand.")))

(define-deprecated (dbus-service #:key (dbus dbus) (services '()) verbose?)
  dbus-root-service-type
  "Return a service that runs the \"system bus\", using @var{dbus}, with
support for @var{services}.  When @var{verbose?} is true, it causes the
@samp{DBUS_VERBOSE} environment variable to be set to @samp{1}; a
verbose-enabled D-Bus package such as @code{dbus-verbose} should be provided
as @var{dbus} in this scenario.

@uref{http://dbus.freedesktop.org/, D-Bus} is an inter-process communication
facility.  Its system bus is used to allow system services to communicate and
be notified of system-wide events.

@var{services} must be a list of packages that provide an
@file{etc/dbus-1/system.d} directory containing additional D-Bus configuration
and policy files.  For example, to allow avahi-daemon to use the system bus,
@var{services} must be equal to @code{(list avahi)}."
  (service dbus-root-service-type
           (dbus-configuration (dbus dbus)
                               (services services)
                               (verbose? verbose?))))

(define (wrapped-dbus-service service program variables)
  "Return a wrapper for @var{service}, a package containing a D-Bus service,
where @var{program} is wrapped such that @var{variables}, a list of name/value
tuples, are all set as environment variables when the bus daemon launches it."
  (define wrapper
    (program-file (string-append (package-name service) "-program-wrapper")
                  #~(begin
                      (use-modules (ice-9 match))

                      (for-each (match-lambda
                                  ((variable value)
                                   (setenv variable value)))
                                '#$variables)

                      (apply execl (string-append #$service "/" #$program)
                             (string-append #$service "/" #$program)
                             (cdr (command-line))))))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (define service-directory
            "/share/dbus-1/system-services")

          (mkdir-p (dirname (string-append #$output
                                           service-directory)))
          (copy-recursively (string-append #$service
                                           service-directory)
                            (string-append #$output
                                           service-directory))
          (symlink (string-append #$service "/etc") ;for etc/dbus-1
                   (string-append #$output "/etc"))

          (for-each (lambda (file)
                      (substitute* file
                        (("Exec[[:blank:]]*=[[:blank:]]*([[:graph:]]+)(.*)$"
                          _ original-program arguments)
                         (string-append "Exec=" #$wrapper arguments
                                        "\n"))))
                    (find-files #$output "\\.service$")))))

  (computed-file (string-append (package-name service) "-wrapper")
                 build))


;;;
;;; Polkit privilege management service.
;;;

(define-record-type* <polkit-configuration>
  polkit-configuration make-polkit-configuration
  polkit-configuration?
  (polkit   polkit-configuration-polkit           ;file-like
            (default polkit))
  (actions  polkit-configuration-actions          ;list of file-like
            (default '())))

(define %polkit-accounts
  (list (user-group (name "polkitd") (system? #t))
        (user-account
         (name "polkitd")
         (group "polkitd")
         (system? #t)
         (comment "Polkit daemon user")
         (home-directory "/var/empty")
         (shell "/run/current-system/profile/sbin/nologin"))))

(define %polkit-pam-services
  (list (unix-pam-service "polkit-1")))

(define (polkit-directory packages)
  "Return a directory containing an @file{actions} and possibly a
@file{rules.d} sub-directory, for use as @file{/etc/polkit-1}."
  (with-imported-modules '((guix build union))
    (computed-file "etc-polkit-1"
                   #~(begin
                       (use-modules (guix build union) (srfi srfi-26))

                       (union-build #$output
                                    (map (cut string-append <>
                                              "/share/polkit-1")
                                         (list #$@packages)))))))

(define polkit-etc-files
  (match-lambda
    (($ <polkit-configuration> polkit packages)
     `(("polkit-1" ,(polkit-directory (cons polkit packages)))))))

(define polkit-setuid-programs
  (match-lambda
    (($ <polkit-configuration> polkit)
     (map file-like->setuid-program
          (list (file-append polkit "/lib/polkit-1/polkit-agent-helper-1")
                (file-append polkit "/bin/pkexec"))))))

(define polkit-service-type
  (service-type (name 'polkit)
                (extensions
                 (list (service-extension account-service-type
                                          (const %polkit-accounts))
                       (service-extension pam-root-service-type
                                          (const %polkit-pam-services))
                       (service-extension dbus-root-service-type
                                          (compose
                                           list
                                           polkit-configuration-polkit))
                       (service-extension etc-service-type
                                          polkit-etc-files)
                       (service-extension setuid-program-service-type
                                          polkit-setuid-programs)))

                ;; Extensions are lists of packages that provide polkit rules
                ;; or actions under share/polkit-1/{actions,rules.d}.
                (compose concatenate)
                (extend (lambda (config actions)
                          (polkit-configuration
                           (inherit config)
                           (actions
                            (append (polkit-configuration-actions config)
                                    actions)))))

                (default-value (polkit-configuration))
                (description
                 "Run the
@uref{http://www.freedesktop.org/wiki/Software/polkit/, Polkit privilege
management service}, which allows system administrators to grant access to
privileged operations in a structured way.  Polkit is a requirement for most
desktop environments, such as GNOME.")))

(define-deprecated (polkit-service #:key (polkit polkit))
  polkit-service-type
  "Return a service that runs the
@uref{http://www.freedesktop.org/wiki/Software/polkit/, Polkit privilege
management service}, which allows system administrators to grant access to
privileged operations in a structured way.  By querying the Polkit service, a
privileged system component can know when it should grant additional
capabilities to ordinary users.  For example, an ordinary user can be granted
the capability to suspend the system if the user is logged in locally."
  (service polkit-service-type
           (polkit-configuration (polkit polkit))))

;;; dbus.scm ends here
