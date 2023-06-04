;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022⁠–⁠2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu services audio)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages mpd)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (mpd-output
            mpd-output?
            mpd-output-name
            mpd-output-type
            mpd-output-enabled?
            mpd-output-format
            mpd-output-tags?
            mpd-output-always-on?
            mpd-output-mixer-type
            mpd-output-replay-gain-handler
            mpd-output-extra-options

            mpd-plugin
            mpd-plugin?
            mpd-plugin-plugin
            mpd-plugin-name
            mpd-plugin-enabled?
            mpd-plugin-extra-options

            mpd-partition
            mpd-partition?
            mpd-partition-name
            mpd-partition-extra-options

            mpd-configuration
            mpd-configuration?
            mpd-configuration-package
            mpd-configuration-user
            mpd-configuration-group
            mpd-configuration-shepherd-requirement
            mpd-configuration-log-file
            mpd-configuration-log-level
            mpd-configuration-music-directory
            mpd-configuration-music-dir
            mpd-configuration-playlist-directory
            mpd-configuration-playlist-dir
            mpd-configuration-db-file
            mpd-configuration-state-file
            mpd-configuration-sticker-file
            mpd-configuration-default-port
            mpd-configuration-endpoints
            mpd-configuration-address
            mpd-configuration-database
            mpd-configuration-partitions
            mpd-configuration-neighbors
            mpd-configuration-inputs
            mpd-configuration-archive-plugins
            mpd-configuration-input-cache-size
            mpd-configuration-decoders
            mpd-configuration-resampler
            mpd-configuration-filters
            mpd-configuration-outputs
            mpd-configuration-playlist-plugins
            mpd-configuration-extra-options
            mpd-service-type

            mympd-service-type
            mympd-configuration
            mympd-configuration?
            mympd-configuration-package
            mympd-configuration-shepherd-requirement
            mympd-configuration-user
            mympd-configuration-group
            mympd-configuration-work-directory
            mympd-configuration-cache-directory
            mympd-configuration-acl
            mympd-configuration-covercache-ttl
            mympd-configuration-http?
            mympd-configuration-host
            mympd-configuration-port
            mympd-configuration-log-level
            mympd-configuration-log-to
            mympd-configuration-lualibs
            mympd-configuration-uri
            mympd-configuration-script-acl
            mympd-configuration-ssl?
            mympd-configuration-ssl-port
            mympd-configuration-ssl-cert
            mympd-configuration-ssl-key
            mympd-configuration-pin-hash
            mympd-configuration-save-caches?
            mympd-ip-acl
            mympd-ip-acl?
            mympd-ip-acl-allow
            mympd-ip-acl-deny))

;;; Commentary:
;;;
;;; Audio related services
;;;
;;; Code:

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (string-drop-right str 1)
                                   str)
                               #\-) "_")))

(define list-of-symbol?
  (list-of symbol?))

;; Helpers for deprecated field types, to be removed later.
(define %lazy-group (make-symbol "%lazy-group"))

(define (%set-user-group user group)
  (user-account
   (inherit user)
   (group (user-group-name group))))


;;;
;;; MPD
;;;

(define (mpd-serialize-field field-name value)
  (let ((field (if (string? field-name) field-name
                   (uglify-field-name field-name)))
        (value (cond
                ((boolean? value) (if value "yes" "no"))
                ((string? value) value)
                (else (object->string value)))))
    #~(format #f "~a ~s~%" #$field #$value)))

(define (mpd-serialize-alist field-name value)
  #~(string-append #$@(generic-serialize-alist list mpd-serialize-field
                                               value)))

(define mpd-serialize-string mpd-serialize-field)
(define mpd-serialize-boolean mpd-serialize-field)

(define (mpd-serialize-list-of-strings field-name value)
  #~(string-append #$@(map (cut mpd-serialize-string field-name <>) value)))

(define (mpd-serialize-user-account field-name value)
  (mpd-serialize-string field-name (user-account-name value)))

(define (mpd-serialize-user-group field-name value)
  (mpd-serialize-string field-name (user-group-name value)))

(define-maybe string (prefix mpd-))
(define-maybe list-of-strings (prefix mpd-))
(define-maybe boolean (prefix mpd-))

(define %mpd-user
  (user-account
      (name "mpd")
      (group %lazy-group)
      (system? #t)
      (comment "Music Player Daemon (MPD) user")
      ;; MPD can use $HOME (or $XDG_CONFIG_HOME) to place its data
      (home-directory "/var/lib/mpd")
      (shell (file-append shadow "/sbin/nologin"))))

(define %mpd-group
  (user-group
   (name "mpd")
   (system? #t)))

;;; TODO: Procedures for deprecated fields, to be removed.

(define mpd-deprecated-fields '((music-dir . music-directory)
                                (playlist-dir . playlist-directory)
                                (address . endpoints)))

(define (port? value) (or (string? value) (integer? value)))

(define (mpd-serialize-deprecated-field field-name value)
  (if (maybe-value-set? value)
      (begin
        (warn-about-deprecation
         field-name #f
         #:replacement (assoc-ref mpd-deprecated-fields field-name))
        (match field-name
          ('playlist-dir (mpd-serialize-string "playlist_directory" value))
          ('music-dir (mpd-serialize-string "music_directory" value))
          ('address (mpd-serialize-string "bind_to_address" value))))
      ""))

(define (mpd-serialize-port field-name value)
  (when (string? value)
    (warning
     (G_ "string value for '~a' is deprecated, use integer instead~%")
     field-name))
  (mpd-serialize-field "port" value))

(define-maybe port (prefix mpd-))

;;; Procedures for unsupported value types, to be removed.

(define (mpd-user-sanitizer value)
  (cond ((user-account? value) value)
        ((string? value)
         (warning (G_ "string value for 'user' is deprecated, use \
user-account instead~%"))
         (user-account
          (inherit %mpd-user)
          (name value)
          ;; XXX: This is to be lazily substituted in (…-accounts)
          ;; with the value from 'group'.
          (group %lazy-group)))
        (else
         (configuration-field-error #f 'user value))))

(define (mpd-group-sanitizer value)
  (cond ((user-group? value) value)
        ((string? value)
         (warning (G_ "string value for 'group' is deprecated, use \
user-group instead~%"))
         (user-group
          (inherit %mpd-group)
          (name value)))
        (else
         (configuration-field-error #f 'group value))))

;;;

;; Generic MPD plugin record, lists only the most prevalent fields.
(define-configuration mpd-plugin
  (plugin
   maybe-string
   "Plugin name.")

  (name
   maybe-string
   "Name.")

  (enabled?
   maybe-boolean
   "Whether the plugin is enabled/disabled.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values
to be appended to the plugin configuration. See
@uref{https://mpd.readthedocs.io/en/latest/plugins.html,MPD plugin reference}
for available options.")

  (prefix mpd-))

(define (mpd-serialize-mpd-plugin field-name value)
  #~(format #f "~a {~%~a}~%"
            '#$field-name
            #$(serialize-configuration value mpd-plugin-fields)))

(define (mpd-serialize-list-of-mpd-plugin field-name value)
  #~(string-append #$@(map (cut mpd-serialize-mpd-plugin field-name <>)
                           value)))

(define list-of-mpd-plugin? (list-of mpd-plugin?))

(define-maybe mpd-plugin (prefix mpd-))

(define-configuration mpd-partition
  (name
   string
   "Partition name.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values
to be appended to the partition configuration. See
@uref{https://mpd.readthedocs.io/en/latest/user.html#configuring-partitions,Configuring Partitions}
for available options.")

  (prefix mpd-))

(define (mpd-serialize-mpd-partition field-name value)
  #~(format #f "partition {~%~a}~%"
            #$(serialize-configuration value mpd-partition-fields)))

(define (mpd-serialize-list-of-mpd-partition field-name value)
  #~(string-append #$@(map (cut mpd-serialize-mpd-partition #f <>) value)))

(define list-of-mpd-partition?
  (list-of mpd-partition?))

(define-configuration mpd-output
  (name
   (string "MPD")
   "The name of the audio output.")

  (type
   (string "pulse")
   "The type of audio output.")

  (enabled?
   (boolean #t)
   "Specifies whether this audio output is enabled when MPD is started. By
default, all audio outputs are enabled. This is just the default
setting when there is no state file; with a state file, the previous
state is restored.")

  (format
   maybe-string
   "Force a specific audio format on output. See
@uref{https://mpd.readthedocs.io/en/latest/user.html#audio-output-format,Global Audio Format}
for a more detailed description.")

  (tags?
   (boolean #t)
   "If set to @code{#f}, then MPD will not send tags to this output. This
is only useful for output plugins that can receive tags, for example the
@code{httpd} output plugin.")

  (always-on?
   (boolean #f)
   "If set to @code{#t}, then MPD attempts to keep this audio output always
open. This may be useful for streaming servers, when you don’t want to
disconnect all listeners even when playback is accidentally stopped.")

  (mixer-type
   (string "none")
   "This field accepts a string that specifies which mixer should be used
for this audio output: the @code{hardware} mixer, the @code{software}
mixer, the @code{null} mixer (allows setting the volume, but with no
effect; this can be used as a trick to implement an external mixer
External Mixer) or no mixer (@code{none})."
   (sanitizer
    (lambda (x)  ; TODO: deprecated, remove me later.
      (cond
       ((symbol? x)
        (warning (G_ "symbol value for 'mixer-type' is deprecated, \
use string instead~%"))
        (symbol->string x))
       ((string? x) x)
       (else
        (configuration-field-error #f 'mixer-type x))))))

  (replay-gain-handler
   maybe-string
   "This field accepts a string that specifies how
@uref{https://mpd.readthedocs.io/en/latest/user.html#replay-gain,Replay Gain}
is to be applied. @code{software} uses an internal software volume control,
@code{mixer} uses the configured (hardware) mixer control and @code{none}
disables replay gain on this audio output.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values
to be appended to the audio output configuration.")

  (prefix mpd-))

(define (mpd-serialize-mpd-output field-name value)
  #~(format #f "audio_output {~%~a}~%"
            #$(serialize-configuration value mpd-output-fields)))

(define (mpd-serialize-list-of-mpd-plugin-or-output field-name value)
  (let ((plugins outputs (partition mpd-plugin? value)))
    #~(string-append #$@(map (cut mpd-serialize-mpd-plugin "audio_output" <>)
                             plugins)
                     #$@(map (cut mpd-serialize-mpd-output #f <>) outputs))))

(define list-of-mpd-plugin-or-output?
  (list-of (lambda (x)
             (or (mpd-output? x) (mpd-plugin? x)))))

(define-configuration mpd-configuration
  (package
   (file-like mpd)
   "The MPD package."
   empty-serializer)

  (user
   (user-account %mpd-user)
   "The user to run mpd as."
   (sanitizer mpd-user-sanitizer))

  (group
   (user-group %mpd-group)
   "The group to run mpd as."
   (sanitizer mpd-group-sanitizer))

  (shepherd-requirement
   (list-of-symbol '())
   "This is a list of symbols naming Shepherd services that this service
will depend on."
   empty-serializer)

  (environment-variables
   (list-of-strings '("PULSE_CLIENTCONFIG=/etc/pulse/client.conf"
                      "PULSE_CONFIG=/etc/pulse/daemon.conf"))
   "A list of strings specifying environment variables."
   empty-serializer)

  (log-file
   (maybe-string "/var/log/mpd/log")
   "The location of the log file. Set to @code{syslog} to use the
local syslog daemon or @code{%unset-value} to omit this directive
from the configuration file.")

  (log-level
   maybe-string
   "Supress any messages below this threshold.
Available values: @code{notice}, @code{info}, @code{verbose},
@code{warning} and @code{error}.")

  (music-directory
   maybe-string
   "The directory to scan for music files.")

  (music-dir ; TODO: deprecated, remove later
   maybe-string
   "The directory to scan for music files."
   (serializer mpd-serialize-deprecated-field))

  (playlist-directory
   maybe-string
   "The directory to store playlists.")

  (playlist-dir ; TODO: deprecated, remove later
   maybe-string
   "The directory to store playlists."
   (serializer mpd-serialize-deprecated-field))

  (db-file
   maybe-string
   "The location of the music database.")

  (state-file
   maybe-string
   "The location of the file that stores current MPD's state.")

  (sticker-file
   maybe-string
   "The location of the sticker database.")

  (default-port
   (maybe-port 6600)
   "The default port to run mpd on.")

  (endpoints
   maybe-list-of-strings
   "The addresses that mpd will bind to. A port different from
@var{default-port} may be specified, e.g. @code{localhost:6602} and
IPv6 addresses must be enclosed in square brackets when a different
port is used.
To use a Unix domain socket, an absolute path or a path starting with @code{~}
can be specified here."
   (serializer
    (lambda (_ endpoints)
      (if (maybe-value-set? endpoints)
          (mpd-serialize-list-of-strings "bind_to_address" endpoints)
          ""))))

  (address ; TODO: deprecated, remove later
   maybe-string
   "The address that mpd will bind to.
To use a Unix domain socket, an absolute path can be specified here."
   (serializer mpd-serialize-deprecated-field))

  (database
   maybe-mpd-plugin
   "MPD database plugin configuration.")

  (partitions
   (list-of-mpd-partition '())
   "List of MPD \"partitions\".")

  (neighbors
   (list-of-mpd-plugin '())
   "List of MPD neighbor plugin configurations.")

  (inputs
   (list-of-mpd-plugin '())
   "List of MPD input plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "input" x))))

  (archive-plugins
   (list-of-mpd-plugin '())
   "List of MPD archive plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "archive_plugin" x))))

  (input-cache-size
   maybe-string
   "MPD input cache size."
   (serializer (lambda (_ x)
                 (if (maybe-value-set? x)
                     #~(string-append "\ninput_cache {\n"
                                      #$(mpd-serialize-string "size" x)
                                      "}\n") ""))))

  (decoders
   (list-of-mpd-plugin '())
   "List of MPD decoder plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "decoder" x))))

  (resampler
   maybe-mpd-plugin
   "MPD resampler plugin configuration.")

  (filters
   (list-of-mpd-plugin '())
   "List of MPD filter plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "filter" x))))

  (outputs
   (list-of-mpd-plugin-or-output (list (mpd-output)))
   "The audio outputs that MPD can use.
By default this is a single output using pulseaudio.")

  (playlist-plugins
   (list-of-mpd-plugin '())
   "List of MPD playlist plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "playlist_plugin" x))))

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values to be
appended to the configuration.")

  (prefix mpd-))

(define (mpd-serialize-configuration configuration)
  (mixed-text-file
   "mpd.conf"
   (serialize-configuration configuration mpd-configuration-fields)))

(define (mpd-log-rotation config)
  (match-record config <mpd-configuration> (log-file)
    (log-rotation
     (files (list log-file))
     (post-rotate #~(begin
                      (use-modules (gnu services herd))
                      (with-shepherd-action 'mpd ('reopen) #f))))))

(define (mpd-shepherd-service config)
  (match-record config <mpd-configuration> (user package shepherd-requirement
                                            log-file playlist-directory
                                            db-file state-file sticker-file
                                            environment-variables)
    (let ((config-file (mpd-serialize-configuration config))
          (username (user-account-name user)))
      (shepherd-service
       (documentation "Run the MPD (Music Player Daemon)")
       (requirement `(user-processes loopback ,@shepherd-requirement))
       (provision '(mpd))
       (start #~(begin
                  (and=> #$(maybe-value log-file)
                         (compose mkdir-p dirname))

                  (let ((user (getpw #$username)))
                    (for-each
                     (lambda (x)
                       (when (and x (not (file-exists? x)))
                         (mkdir-p x)
                         (chown x (passwd:uid user) (passwd:gid user))))
                     (list #$(maybe-value playlist-directory)
                           (and=> #$(maybe-value db-file) dirname)
                           (and=> #$(maybe-value state-file) dirname)
                           (and=> #$(maybe-value sticker-file) dirname))))

                  (make-forkexec-constructor
                   (list #$(file-append package "/bin/mpd")
                         "--no-daemon"
                         #$config-file)
                   #:environment-variables '#$environment-variables)))
       (stop  #~(make-kill-destructor))
       (actions
        (list (shepherd-configuration-action config-file)
              (shepherd-action
               (name 'reopen)
               (documentation "Re-open log files and flush caches.")
               (procedure
                #~(lambda (pid)
                    (if pid
                        (begin
                          (kill pid SIGHUP)
                          (format #t
                                  "Issued SIGHUP to Service MPD (PID ~a)."
                                  pid))
                        (format #t "Service MPD is not running.")))))))))))

(define (mpd-accounts config)
  (match-record config <mpd-configuration> (user group)
    ;; TODO: Deprecation code, to be removed.
    (let ((user (if (eq? (user-account-group user) %lazy-group)
                    (%set-user-group user group)
                    user)))
      (list user group))))

(define mpd-service-type
  (service-type
   (name 'mpd)
   (description "Run the Music Player Daemon (MPD).")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list mpd-shepherd-service))
          (service-extension account-service-type
                             mpd-accounts)
          (service-extension rottlog-service-type
                             (compose list mpd-log-rotation))))
   (default-value (mpd-configuration))))


;;;
;;; myMPD
;;;

(define (string-or-symbol? x)
  (or (symbol? x) (string? x)))

(define-configuration/no-serialization mympd-ip-acl
  (allow
   (list-of-strings '())
   "Allowed IP addresses.")

  (deny
   (list-of-strings '())
   "Disallowed IP addresses."))

(define-maybe/no-serialization integer)
(define-maybe/no-serialization mympd-ip-acl)

(define %mympd-user
  (user-account
      (name "mympd")
      (group %lazy-group)
      (system? #t)
      (comment "myMPD user")
      (home-directory "/var/empty")
      (shell (file-append shadow "/sbin/nologin"))))

(define %mympd-group
  (user-group
   (name "mympd")
   (system? #t)))

;;; TODO: Procedures for unsupported value types, to be removed.
(define (mympd-user-sanitizer value)
  (cond ((user-account? value) value)
        ((string? value)
         (warning (G_ "string value for 'user' is not supported, use \
user-account instead~%"))
         (user-account
          (inherit %mympd-user)
          (name value)
          ;; XXX: this is to be lazily substituted in (…-accounts)
          ;; with the value from 'group'.
          (group %lazy-group)))
        (else
         (configuration-field-error #f 'user value))))

(define (mympd-group-sanitizer value)
  (cond ((user-group? value) value)
        ((string? value)
         (warning (G_ "string value for 'group' is not supported, use \
user-group instead~%"))
         (user-group
          (inherit %mympd-group)
          (name value)))
        (else
         (configuration-field-error #f 'group value))))
;;;


;; XXX: The serialization procedures are insufficient since we require
;; access to multiple fields at once.
;; Fields marked with empty-serializer are never serialized and are
;; used for command-line arguments or by the service definition.
(define-configuration/no-serialization mympd-configuration
  (package
    (file-like mympd)
    "The package object of the myMPD server."
    empty-serializer)

  (shepherd-requirement
   (list-of-symbol '())
   "This is a list of symbols naming Shepherd services that this service
will depend on."
   empty-serializer)

  (user
   (user-account %mympd-user)
   "Owner of the @command{mympd} process."
   (sanitizer mympd-user-sanitizer)
   empty-serializer)

  (group
   (user-group %mympd-group)
   "Owner group of the @command{mympd} process."
   (sanitizer mympd-group-sanitizer)
   empty-serializer)

  (work-directory
   (string "/var/lib/mympd")
   "Where myMPD will store its data."
   empty-serializer)

  (cache-directory
   (string "/var/cache/mympd")
   "Where myMPD will store its cache."
   empty-serializer)

  (acl
   maybe-mympd-ip-acl
   "ACL to access the myMPD webserver.")

  (covercache-ttl
   (maybe-integer 31)
   "How long to keep cached covers, @code{0} disables cover caching.")

  (http?
   (boolean #t)
   "HTTP support.")

  (host
   (string "[::]")
   "Host name to listen on.")

  (port
   (maybe-port 80)
   "HTTP port to listen on.")

  (log-level
   (integer 5)
   "How much detail to include in logs, possible values: @code{0} to @code{7}.")

  (log-to
   (string-or-symbol "/var/log/mympd/log")
   "Where to send logs. By default, the service logs to
@file{/var/log/mympd.log}. The alternative is @code{'syslog}, which
sends output to the running syslog service under the @samp{daemon} facility."
   empty-serializer)

  (lualibs
   (maybe-string "all")
   "See
@url{https://jcorporation.github.io/myMPD/scripting/#lua-standard-libraries}.")

  (uri
   maybe-string
   "Override URI to myMPD.
See @url{https://github.com/jcorporation/myMPD/issues/950}.")

  (script-acl
   (maybe-mympd-ip-acl (mympd-ip-acl
                        (allow '("127.0.0.1"))))
   "ACL to access the myMPD script backend.")

  (ssl?
   (boolean #f)
   "SSL/TLS support.")

  (ssl-port
   (maybe-port 443)
   "Port to listen for HTTPS.")

  (ssl-cert
   maybe-string
   "Path to PEM encoded X.509 SSL/TLS certificate (public key).")

  (ssl-key
   maybe-string
   "Path to PEM encoded SSL/TLS private key.")

  (pin-hash
   maybe-string
   "SHA-256 hashed pin used by myMPD to control settings access by
prompting a pin from the user.")

  (save-caches?
   maybe-boolean
   "Whether to preserve caches between service restarts."))

(define (mympd-serialize-configuration config)
  (define serialize-value
    (match-lambda
      ((? boolean? val) (if val "true" "false"))
      ((? integer? val) (number->string val))
      ((? mympd-ip-acl? val) (ip-acl-serialize-configuration val))
      ((? string? val) val)))

  (define (ip-acl-serialize-configuration config)
    (define (serialize-list-of-strings prefix lst)
      (map (cut format #f "~a~a" prefix <>) lst))
    (string-join
     (append
      (serialize-list-of-strings "+" (mympd-ip-acl-allow config))
      (serialize-list-of-strings "-" (mympd-ip-acl-deny config))) ","))

  ;; myMPD configuration fields are serialized as individual files under
  ;; <work-directory>/config/.
  (match-record config <mympd-configuration> (work-directory acl
                                              covercache-ttl http? host port
                                              log-level lualibs uri script-acl
                                              ssl? ssl-port ssl-cert ssl-key
                                              pin-hash save-caches?)
    (define (serialize-field filename value)
      (when (maybe-value-set? value)
        (list (format #f "~a/config/~a" work-directory filename)
              (mixed-text-file filename (serialize-value value)))))

    (let ((filename-to-field `(("acl" . ,acl)
                               ("covercache_keep_days" . ,covercache-ttl)
                               ("http"                 . ,http?)
                               ("http_host"            . ,host)
                               ("http_port"            . ,port)
                               ("loglevel"             . ,log-level)
                               ("lualibs"              . ,lualibs)
                               ("mympd_uri"            . ,uri)
                               ("scriptacl"            . ,script-acl)
                               ("ssl"                  . ,ssl?)
                               ("ssl_port"             . ,ssl-port)
                               ("ssl_cert"             . ,ssl-cert)
                               ("ssl_key"              . ,ssl-key)
                               ("pin_hash"             . ,pin-hash)
                               ("save_caches"          . ,save-caches?))))
      (filter list?
              (generic-serialize-alist list serialize-field
                                       filename-to-field)))))

(define (mympd-shepherd-service config)
  (match-record config <mympd-configuration> (package shepherd-requirement
                                              user work-directory
                                              cache-directory log-level log-to)
    (let ((log-level* (format #f "MYMPD_LOGLEVEL=~a" log-level))
          (username (user-account-name user)))
      (shepherd-service
       (documentation "Run the myMPD daemon.")
       (requirement `(loopback user-processes
                               ,@(if (eq? log-to 'syslog)
                                     '(syslog)
                                     '())
                               ,@shepherd-requirement))
       (provision '(mympd))
       (start #~(begin
                  (let* ((pw (getpwnam #$username))
                         (uid (passwd:uid pw))
                         (gid (passwd:gid pw)))
                    (for-each (lambda (dir)
                                (mkdir-p dir)
                                (chown dir uid gid))
                              (list #$work-directory #$cache-directory)))

                  (make-forkexec-constructor
                   `(#$(file-append package "/bin/mympd")
                     "--user" #$username
                     #$@(if (eq? log-to 'syslog) '("--syslog") '())
                     "--workdir" #$work-directory
                     "--cachedir" #$cache-directory)
                   #:environment-variables (list #$log-level*)
                   #:log-file #$(if (string? log-to) log-to #f))))
       (stop #~(make-kill-destructor))))))

(define (mympd-accounts config)
  (match-record config <mympd-configuration> (user group)
    ;; TODO: Deprecation code, to be removed.
    (let ((user (if (eq? (user-account-group user) %lazy-group)
                    (%set-user-group user group)
                    user)))
      (list user group))))

(define (mympd-log-rotation config)
  (match-record config <mympd-configuration> (log-to)
    (if (string? log-to)
        (list (log-rotation
               (files (list log-to))))
        '())))

(define mympd-service-type
  (service-type
   (name 'mympd)
   (extensions
    (list  (service-extension shepherd-root-service-type
                              (compose list mympd-shepherd-service))
           (service-extension account-service-type
                              mympd-accounts)
           (service-extension special-files-service-type
                              mympd-serialize-configuration)
           (service-extension rottlog-service-type
                              mympd-log-rotation)))
   (description "Run myMPD, a frontend for MPD. (Music Player Daemon)")
   (default-value (mympd-configuration))))
