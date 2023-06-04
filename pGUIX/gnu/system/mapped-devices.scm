;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017, 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((guix modules) #:hide (file-name->module-name))
  #:use-module (guix i18n)
  #:use-module ((guix diagnostics)
                #:select (source-properties->location
                          formatted-message
                          &fix-hint
                          &error-location))
  #:use-module (guix deprecation)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system uuid)
  #:autoload   (gnu build file-systems) (find-partition-by-luks-uuid)
  #:autoload   (gnu build linux-modules)
                 (missing-modules)
  #:autoload   (gnu packages cryptsetup) (cryptsetup-static)
  #:autoload   (gnu packages linux) (mdadm-static lvm2-static)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (%mapped-device
            mapped-device
            mapped-device?
            mapped-device-source
            mapped-device-target
            mapped-device-targets
            mapped-device-type
            mapped-device-location

            mapped-device-kind
            mapped-device-kind?
            mapped-device-kind-open
            mapped-device-kind-close
            mapped-device-kind-check

            device-mapping-service-type
            device-mapping-service

            check-device-initrd-modules           ;XXX: needs a better place

            luks-device-mapping
            raid-device-mapping
            lvm-device-mapping))

;;; Commentary:
;;;
;;; This module supports "device mapping", a concept implemented by Linux's
;;; device-mapper.
;;;
;;; Code:

(define-record-type* <mapped-device> %mapped-device
  make-mapped-device
  mapped-device?
  (source    mapped-device-source)                ;string | list of strings
  (targets   mapped-device-targets)               ;list of strings
  (type      mapped-device-type)                  ;<mapped-device-kind>
  (location  mapped-device-location
             (default (current-source-location)) (innate)))

(define-syntax mapped-device-compatibility-helper
  (syntax-rules (target)
    ((_ () (fields ...))
     (%mapped-device fields ...))
    ((_ ((target exp) rest ...) (others ...))
     (%mapped-device others ...
                      (targets (list exp))
                      rest ...))
    ((_ (field rest ...) (others ...))
     (mapped-device-compatibility-helper (rest ...)
                                         (others ... field)))))

(define-syntax-rule (mapped-device fields ...)
  "Build an <mapped-device> record, automatically converting 'target' field
specifications to 'targets'."
  (mapped-device-compatibility-helper (fields ...) ()))

(define-deprecated (mapped-device-target md)
  mapped-device-targets
  (car (mapped-device-targets md)))

(define-record-type* <mapped-device-type> mapped-device-kind
  make-mapped-device-kind
  mapped-device-kind?
  (open      mapped-device-kind-open)             ;source target -> gexp
  (close     mapped-device-kind-close             ;source target -> gexp
             (default (const #~(const #f))))
  (check     mapped-device-kind-check             ;source -> Boolean
             (default (const #t))))


;;;
;;; Device mapping as a Shepherd service.
;;;

(define device-mapping-service-type
  (shepherd-service-type
   'device-mapping
   (match-lambda
     (($ <mapped-device> source targets
                         ($ <mapped-device-type> open close))
      (shepherd-service
       (provision (list (symbol-append 'device-mapping- (string->symbol (string-join targets "-")))))
       (requirement '(udev))
       (documentation "Map a device node using Linux's device mapper.")
       (start #~(lambda () #$(open source targets)))
       (stop #~(lambda _ (not #$(close source targets))))
       (respawn? #f))))
   (description "Map a device node using Linux's device mapper.")))

(define (device-mapping-service mapped-device)
  "Return a service that sets up @var{mapped-device}."
  (service device-mapping-service-type mapped-device))


;;;
;;; Static checks.
;;;

(define (check-device-initrd-modules device linux-modules location)
  "Raise an error if DEVICE needs modules beyond LINUX-MODULES to operate.
DEVICE must be a \"/dev\" file name."
  (define missing
    ;; Attempt to determine missing modules.
    (catch 'system-error
      (lambda ()
        (missing-modules device linux-modules))

      ;; If we can't do that (e.g., EPERM), skip the whole thing.
      (const '())))

  (unless (null? missing)
    ;; Note: What we suggest here is a list of module names (e.g.,
    ;; "usb_storage"), not file names (e.g., "usb-storage.ko").  This is
    ;; OK because we have machinery that accepts both the hyphen and the
    ;; underscore version.
    (raise (make-compound-condition
            (formatted-message (G_ "you may need these modules \
in the initrd for ~a:~{ ~a~}")
                               device missing)
            (condition
             (&fix-hint
              (hint (format #f (G_ "Try adding them to the
@code{initrd-modules} field of your @code{operating-system} declaration, along
these lines:

@example
 (operating-system
   ;; @dots{}
   (initrd-modules (append (list~{ ~s~})
                           %base-initrd-modules)))
@end example

If you think this diagnostic is inaccurate, use the @option{--skip-checks}
option of @command{guix system}.\n")
                            missing))))
            (condition
             (&error-location
              (location (source-properties->location location))))))))


;;;
;;; Common device mappings.
;;;

(define (open-luks-device source targets)
  "Return a gexp that maps SOURCE to TARGET as a LUKS device, using
'cryptsetup'."
  (with-imported-modules (source-module-closure
                          '((gnu build file-systems)
                            (guix build utils))) ;; For mkdir-p
    (match targets
      ((target)
       #~(let ((source #$(if (uuid? source)
                             (uuid-bytevector source)
                             source)))
           ;; XXX: 'use-modules' should be at the top level.
           (use-modules (rnrs bytevectors) ;bytevector?
                        ((gnu build file-systems)
                         #:select (find-partition-by-luks-uuid
                                   system*/tty))
                        ((guix build utils) #:select (mkdir-p)))

           ;; Create '/run/cryptsetup/' if it does not exist, as device locking
           ;; is mandatory for LUKS2.
           (mkdir-p "/run/cryptsetup/")

           ;; Use 'cryptsetup-static', not 'cryptsetup', to avoid pulling the
           ;; whole world inside the initrd (for when we're in an initrd).
           ;; 'cryptsetup open' requires standard input to be a tty to allow
           ;; for interaction but shepherd sets standard input to /dev/null;
           ;; thus, explicitly request a tty.
           (zero? (system*/tty
                   #$(file-append cryptsetup-static "/sbin/cryptsetup")
                   "open" "--type" "luks"

                   ;; Note: We cannot use the "UUID=source" syntax here
                   ;; because 'cryptsetup' implements it by searching the
                   ;; udev-populated /dev/disk/by-id directory but udev may
                   ;; be unavailable at the time we run this.
                   (if (bytevector? source)
                       (or (let loop ((tries-left 10))
                             (and (positive? tries-left)
                                  (or (find-partition-by-luks-uuid source)
                                      ;; If the underlying partition is
                                      ;; not found, try again after
                                      ;; waiting a second, up to ten
                                      ;; times.  FIXME: This should be
                                      ;; dealt with in a more robust way.
                                      (begin (sleep 1)
                                             (loop (- tries-left 1))))))
                           (error "LUKS partition not found" source))
                       source)

                   #$target)))))))

(define (close-luks-device source targets)
  "Return a gexp that closes TARGET, a LUKS device."
  (match targets
    ((target)
     #~(zero? (system* #$(file-append cryptsetup-static "/sbin/cryptsetup")
                       "close" #$target)))))

(define* (check-luks-device md #:key
                            needed-for-boot?
                            (initrd-modules '())
                            #:allow-other-keys
                            #:rest rest)
  "Ensure the source of MD is valid."
  (let ((source   (mapped-device-source md))
        (location (mapped-device-location md)))
    (or (not (zero? (getuid)))
        (if (uuid? source)
            (match (find-partition-by-luks-uuid (uuid-bytevector source))
              (#f
               (raise (make-compound-condition
                       (formatted-message (G_ "no LUKS partition with UUID '~a'")
                                          (uuid->string source))
                       (condition
                        (&error-location
                         (location (source-properties->location
                                    (mapped-device-location md))))))))
              ((? string? device)
               (check-device-initrd-modules device initrd-modules location)))
            (check-device-initrd-modules source initrd-modules location)))))

(define luks-device-mapping
  ;; The type of LUKS mapped devices.
  (mapped-device-kind
   (open open-luks-device)
   (close close-luks-device)
   (check check-luks-device)))

(define (open-raid-device sources targets)
  "Return a gexp that assembles SOURCES (a list of devices) to the RAID device
TARGET (e.g., \"/dev/md0\"), using 'mdadm'."
  (match targets
    ((target)
     #~(let ((sources '#$sources)

             ;; XXX: We're not at the top level here.  We could use a
             ;; non-top-level 'use-modules' form but that doesn't work when the
             ;; code is eval'd, like the Shepherd does.
             (every   (@ (srfi srfi-1) every))
             (format  (@ (ice-9 format) format)))
         (let loop ((attempts 0))
           (unless (every file-exists? sources)
             (when (> attempts 20)
               (error "RAID devices did not show up; bailing out"
                      sources))

             (format #t "waiting for RAID source devices~{ ~a~}...~%"
                     sources)
             (sleep 1)
             (loop (+ 1 attempts))))

         ;; Use 'mdadm-static' rather than 'mdadm' to avoid pulling its whole
         ;; closure (80 MiB) in the initrd when a RAID device is needed for boot.
         (zero? (apply system* #$(file-append mdadm-static "/sbin/mdadm")
                       "--assemble" #$target sources))))))

(define (close-raid-device sources targets)
  "Return a gexp that stops the RAID device TARGET."
  (match targets
    ((target)
     #~(zero? (system* #$(file-append mdadm-static "/sbin/mdadm")
                       "--stop" #$target)))))

(define raid-device-mapping
  ;; The type of RAID mapped devices.
  (mapped-device-kind
   (open open-raid-device)
   (close close-raid-device)))

(define (open-lvm-device source targets)
  #~(and
     (zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                     "vgchange" "--activate" "ay" #$source))
     ; /dev/mapper nodes are usually created by udev, but udev may be unavailable at the time we run this. So we create them here.
     (zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                     "vgscan" "--mknodes"))
     (every file-exists? (map (lambda (file) (string-append "/dev/mapper/" file))
                              '#$targets))))


(define (close-lvm-device source targets)
  #~(zero? (system* #$(file-append lvm2-static "/sbin/lvm")
                    "vgchange" "--activate" "n" #$source)))

(define lvm-device-mapping
  (mapped-device-kind
   (open open-lvm-device)
   (close close-lvm-device)))

;;; mapped-devices.scm ends here
