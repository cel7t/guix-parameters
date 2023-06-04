;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
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

(define-module (gnu installer parted)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu system uuid)
  #:use-module ((gnu build file-systems)
                #:select (canonicalize-device-spec
                          find-partition-by-label
                          find-partition-by-uuid
                          read-partition-uuid
                          read-luks-partition-uuid))
  #:use-module ((gnu build linux-boot)
                #:select (linux-command-line
                          find-long-option))
  #:use-module ((gnu build linux-modules)
                #:select (missing-modules))
  #:use-module ((gnu system linux-initrd)
                #:select (%base-initrd-modules))
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix read-print)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module (parted)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (<user-partition>
            user-partition
            make-user-partition
            user-partition?
            user-partition-name
            user-partition-type
            user-partition-file-name
            user-partition-disk-file-name
            user-partition-crypt-label
            user-partition-crypt-password
            user-partition-fs-type
            user-partition-bootable?
            user-partition-esp?
            user-partition-bios-grub?
            user-partition-size
            user-partition-start
            user-partition-end
            user-partition-mount-point
            user-partition-need-formatting?
            user-partition-parted-object

            find-esp-partition
            small-freespace-partition?
            esp-partition?
            boot-partition?
            efi-installation?
            default-esp-mount-point

            force-device-sync
            eligible-devices
            partition-user-type
            user-fs-type-name
            partition-filesystem-user-type
            partition-get-flags
            partition->user-partition
            create-special-user-partitions
            find-user-partition-by-parted-object

            device-description
            partition-end-formatted
            partition-print-number
            partition-description
            partitions-descriptions
            user-partition-description

            &max-primary-exceeded
            max-primary-exceeded?
            &extended-creation-error
            extended-creation-error?
            &logical-creation-error
            logical-creation-error?

            can-create-partition?
            mklabel
            mkpart
            rmpart

            auto-partition!

            &no-root-mount-point
            no-root-mount-point?
            &cannot-read-uuid
            cannot-read-uuid?
            cannot-read-uuid-partition

            check-user-partitions
            set-user-partitions-file-name
            format-user-partitions
            mount-user-partitions
            umount-user-partitions
            with-mounted-partitions
            user-partitions->file-systems
            user-partitions->configuration

            init-parted
            free-parted))


;;;
;;; Partition record.
;;;

(define-record-type* <user-partition>
  user-partition make-user-partition
  user-partition?
  (name                 user-partition-name ;string
                        (default #f))
  (type                 user-partition-type
                        (default 'normal)) ; 'normal | 'logical | 'extended
  (file-name            user-partition-file-name
                        (default #f))
  (disk-file-name       user-partition-disk-file-name
                        (default #f))
  (crypt-label          user-partition-crypt-label
                        (default #f))
  (crypt-password       user-partition-crypt-password ; <secret>
                        (default #f))
  (fs-type              user-partition-fs-type
                        (default 'ext4))
  (bootable?            user-partition-bootable?
                        (default #f))
  (esp?                 user-partition-esp?
                        (default #f))
  (bios-grub?           user-partition-bios-grub?
                        (default #f))
  (size                 user-partition-size
                        (default #f))
  (start                user-partition-start ;start as string (e.g. '11MB')
                        (default #f))
  (end                  user-partition-end ;same as start
                        (default #f))
  (mount-point          user-partition-mount-point ;string
                        (default #f))
  (need-formatting?     user-partition-need-formatting? ; boolean
                        (default #f))
  (parted-object        user-partition-parted-object ; <partition> from parted
                        (default #f)))


;;
;; Utilities.
;;

(define (find-esp-partition partitions)
  "Find and return the ESP partition among PARTITIONS."
  (find esp-partition? partitions))

(define* (small-freespace-partition? device
                                     partition
                                     #:key (max-size MEBIBYTE-SIZE))
  "Return #t is PARTITION is a free-space partition with less a size strictly
inferior to MAX-SIZE, #f otherwise."
  (let ((size (partition-length partition))
        (max-sector-size (/ max-size
                            (device-sector-size device))))
    (< size max-sector-size)))

(define (partition-user-type partition)
  "Return the type of PARTITION, to be stored in the TYPE field of
<user-partition> record. It can be 'normal, 'extended or 'logical."
  (cond ((normal-partition? partition)
         'normal)
        ((extended-partition? partition)
         'extended)
        ((logical-partition? partition)
         'logical)
        (else #f)))

(define (esp-partition? partition)
  "Return #t if partition has the ESP flag, return #f otherwise."
  (let* ((disk (partition-disk partition))
         (disk-type (disk-disk-type disk)))
    (and (data-partition? partition)
         (partition-is-flag-available? partition PARTITION-FLAG-ESP)
         (partition-get-flag partition PARTITION-FLAG-ESP))))

(define (boot-partition? partition)
  "Return #t if partition has the boot flag, return #f otherwise."
  (and (data-partition? partition)
       (partition-is-flag-available? partition PARTITION-FLAG-BOOT)
       (partition-get-flag partition PARTITION-FLAG-BOOT)))


;; The default mount point for ESP partitions.
(define default-esp-mount-point
  (make-parameter "/boot/efi"))

(define (efi-installation?)
  "Return #t if an EFI installation should be performed, #f otherwise."
  (file-exists? "/sys/firmware/efi"))

(define (user-fs-type-name fs-type)
  "Return the name of FS-TYPE as specified by libparted."
  (case fs-type
    ((ext4)  "ext4")
    ((btrfs) "btrfs")
    ((fat16) "fat16")
    ((fat32) "fat32")
    ((jfs)   "jfs")
    ((ntfs)  "ntfs")
    ((xfs)   "xfs")
    ((swap)  "linux-swap")))

(define (user-fs-type->mount-type fs-type)
  "Return the mount type of FS-TYPE."
  (case fs-type
    ((ext4)  "ext4")
    ((btrfs) "btrfs")
    ((fat16) "vfat")
    ((fat32) "vfat")
    ((jfs)   "jfs")
    ((ntfs)  "ntfs")
    ((xfs)   "xfs")))

(define (partition-filesystem-user-type partition)
  "Return the filesystem type of PARTITION, to be stored in the FS-TYPE field
of <user-partition> record."
  (let ((fs-type (partition-fs-type partition)))
    (and fs-type
         (let ((name (filesystem-type-name fs-type)))
           (cond
            ((string=? name "ext4") 'ext4)
            ((string=? name "btrfs") 'btrfs)
            ((string=? name "fat16") 'fat16)
            ((string=? name "fat32") 'fat32)
            ((string=? name "jfs") 'jfs)
            ((string=? name "ntfs") 'ntfs)
            ((string=? name "xfs") 'xfs)
            ((or (string=? name "swsusp")
                 (string=? name "linux-swap(v0)")
                 (string=? name "linux-swap(v1)"))
             'swap)
            (else
             (error (format #f "Unhandled ~a fs-type~%" name))))))))

(define (partition-get-flags partition)
  "Return the list of flags supported by the given PARTITION."
  (filter-map (lambda (flag)
                (and (partition-get-flag partition flag)
                     flag))
              (partition-flags partition)))

(define (partition->user-partition partition)
  "Convert PARTITION into a <user-partition> record and return it."
  (let* ((disk (partition-disk partition))
         (device (disk-device disk))
         (disk-type (disk-disk-type disk))
         (has-name? (disk-type-check-feature
                     disk-type
                     DISK-TYPE-FEATURE-PARTITION-NAME))
         (name (and has-name?
                    (data-partition? partition)
                    (partition-get-name partition))))
    (user-partition
     (name (and (and name
                     (not (string=? name "")))
                name))
     (type (or (partition-user-type partition)
               'normal))
     (file-name (partition-get-path partition))
     (disk-file-name (device-path device))
     (fs-type (or (partition-filesystem-user-type partition)
                  'ext4))
     (mount-point (and (esp-partition? partition)
                       (default-esp-mount-point)))
     (bootable? (boot-partition? partition))
     (esp? (esp-partition? partition))
     (parted-object partition))))

(define (create-special-user-partitions partitions)
  "Return a list with a <user-partition> record describing the ESP partition
found in PARTITIONS, if any."
  (filter-map (lambda (partition)
                (and (esp-partition? partition)
                     (partition->user-partition partition)))
              partitions))

(define (find-user-partition-by-parted-object user-partitions
                                              partition)
  "Find and return the <user-partition> record in USER-PARTITIONS list which
PARTED-OBJECT field equals PARTITION, return #f if not found."
  (find (lambda (user-partition)
          (equal? (user-partition-parted-object user-partition)
                  partition))
        user-partitions))

(define (read-partition-uuid/retry file-name)
  "Call READ-PARTITION-UUID with 5 retries spaced by 1 second.  This is useful
if the partition table is updated by the kernel at the time this function is
called, causing the underlying /dev to be absent."
  (define max-retries 5)

  (let loop ((retry max-retries))
    (catch #t
      (lambda ()
        (read-partition-uuid file-name))
      (lambda _
        (if (> retry 0)
            (begin
              (sleep 1)
              (loop (- retry 1)))
            (error
             (format #f (G_ "Could not open ~a after ~a retries~%.")
                     file-name max-retries)))))))


;;
;; Devices
;;

(define (with-delay-device-in-use? file-name)
  "Call DEVICE-IN-USE? with a few retries, as the first re-read will often
fail. See rereadpt function in wipefs.c of util-linux for an explanation."
  ;; Kernel always return EINVAL for BLKRRPART on loopdevices.
  (and (not (string-match "/dev/loop*" file-name))
       (let loop ((try 16))
         (usleep 250000)
         (let ((in-use? (device-in-use? file-name)))
           (if (and in-use? (> try 0))
               (loop (- try 1))
               in-use?)))))

(define* (force-device-sync device)
  "Force a flushing of the given DEVICE."
  (device-open device)
  (device-sync device)
  (device-close device))

(define (remove-logical-devices)
  "Remove all active logical devices."
   ((run-command-in-installer) "dmsetup" "remove_all"))

(define (installer-root-partition-path)
  "Return the root partition path, or #f if it could not be detected."
  (let* ((cmdline (linux-command-line))
         (root (find-long-option "root" cmdline)))
    (and root
         (or (and (access? root F_OK) root)
             (find-partition-by-label root)
             (and=> (uuid root)
                    find-partition-by-uuid)))))

;; Minimal installation device size.
(define %min-device-size
  (* 2 GIBIBYTE-SIZE)) ;2GiB

(define (mapped-device? device)
  "Return #true if DEVICE is a mapped device, false otherwise."
  (string-prefix? "/dev/dm-" device))

;; TODO: Use DM_TABLE_DEPS ioctl instead of dmsetup.
(define (mapped-device-parent-partition device)
  "Return the parent partition path of the mapped DEVICE."
  (let* ((command `("dmsetup" "deps" ,device "-o" "devname"))
         (parent #f)
         (handler
          (lambda (input)
            ;; We are parsing an output that should look like:
            ;; 1 dependencies  : (sda2)
            (let ((result
                   (string-match "\\(([^\\)]+)\\)"
                                 (get-string-all input))))
              (and result
                   (set! parent
                         (format #f "/dev/~a"
                                 (match:substring result 1))))))))
    (run-external-command-with-handler handler command)
    parent))

(define (eligible-devices)
  "Return all the available devices except the install device and the devices
which are smaller than %MIN-DEVICE-SIZE."

  (define the-installer-root-partition-path
    (let ((root (installer-root-partition-path)))
      (cond
       ((mapped-device? root)
        ;; If the partition is a mapped device (/dev/dm-X), locate the parent
        ;; partition.  It is the case when Ventoy is used to host the
        ;; installation image.
        (let ((parent (mapped-device-parent-partition root)))
          (installer-log-line "mapped device ~a -> ~a" parent root)
          parent))
       (else root))))

  (define (small-device? device)
    (let ((length (device-length device))
          (sector-size (device-sector-size device)))
      (and (< (* length sector-size) %min-device-size)
           (installer-log-line "~a is not eligible because it is smaller than \
~a."
                   (device-path device)
                   (unit-format-custom-byte device
                                            %min-device-size
                                            UNIT-GIGABYTE)))))

  ;; Read partition table of device and compare each path to the one
  ;; we're booting from to determine if it is the installation
  ;; device.
  (define (installation-device? device)
    ;; When using CDROM based installation, the root partition path may be the
    ;; device path.
    (and (or (string=? the-installer-root-partition-path
                       (device-path device))
             (let ((disk (disk-new device)))
               (and disk
                    (any (lambda (partition)
                           (string=? the-installer-root-partition-path
                                     (partition-get-path partition)))
                         (disk-partitions disk)))))
         (installer-log-line "~a is not eligible because it is the \
installation device."
                 (device-path device))))

  (remove
   (lambda (device)
     (or (installation-device? device)
         (small-device? device)))
   (devices)))


;;
;; Disk and partition printing.
;;

(define* (device-description device #:optional disk)
  "Return a string describing the given DEVICE."
  (let* ((type (device-type device))
         (file-name (device-path device))
         (model (device-model device))
         (type-str (device-type->string type))
         (disk-type (if disk
                        (disk-disk-type disk)
                        (disk-probe device)))
         (length (device-length device))
         (sector-size (device-sector-size device))
         (end (unit-format-custom-byte device
                                       (* length sector-size)
                                       UNIT-GIGABYTE)))
    (string-join
     `(,@(if (string=? model "")
             `(,type-str)
             `(,model ,(string-append "(" type-str ")")))
       ,file-name
       ,end
       ,@(if disk-type
             `(,(disk-type-name disk-type))
             '()))
     " ")))

(define (partition-end-formatted device partition)
  "Return as a string the end of PARTITION with the relevant unit."
  (unit-format-byte
   device
   (-
    (* (+ (partition-end partition) 1)
       (device-sector-size device))
    1)))

(define (partition-print-number partition)
  "Convert the given partition NUMBER to string."
  (let ((number (partition-number partition)))
    (number->string number)))

(define (partition-description partition user-partition)
  "Return a string describing the given PARTITION, located on the DISK of
DEVICE."

  (define (partition-print-type partition)
    "Return the type of PARTITION as a string."
    (if (freespace-partition? partition)
        (G_ "Free space")
        (let ((type (partition-type partition)))
          (match type
            ((type-symbol)
             (symbol->string type-symbol))))))

  (define (partition-print-flags partition)
    "Return the flags of PARTITION as a string of comma separated flags."
    (string-join
     (filter-map
      (lambda (flag)
        (and (partition-get-flag partition flag)
             (partition-flag-get-name flag)))
      (partition-flags partition))
     ","))

  (define (maybe-string-pad string length)
    "Returned a string formatted by padding STRING of LENGTH characters to the
right. If STRING is #f use an empty string."
    (if (and string (not (string=? string "")))
        (string-pad-right string length)
        ""))

  (let* ((disk (partition-disk partition))
         (device (disk-device disk))
         (disk-type (disk-disk-type disk))
         (has-name? (disk-type-check-feature
                     disk-type
                     DISK-TYPE-FEATURE-PARTITION-NAME))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED))
         (part-type (partition-print-type partition))
         (number (and (not (freespace-partition? partition))
                      (partition-print-number partition)))
         (name (and has-name?
                    (if (freespace-partition? partition)
                        (G_ "Free space")
                        (partition-get-name partition))))
         (start (unit-format device
                             (partition-start partition)))
         (end (partition-end-formatted device partition))
         (size (unit-format device (partition-length partition)))
         (fs-type (partition-fs-type partition))
         (fs-type-name (and fs-type
                            (filesystem-type-name fs-type)))
         (crypt-label (and user-partition
                           (user-partition-crypt-label user-partition)))
         (flags (and (not (freespace-partition? partition))
                     (partition-print-flags partition)))
         (mount-point (and user-partition
                           (user-partition-mount-point user-partition))))
    `(,(or number "")
      ,@(if has-extended?
            (list part-type)
            '())
      ,size
      ,(or fs-type-name "")
      ,(or flags "")
      ,(or mount-point "")
      ,(or crypt-label "")
      ,(maybe-string-pad name 30))))

(define (partitions-descriptions partitions user-partitions)
  "Return a list of strings describing all the partitions found on
DEVICE. METADATA partitions are not described. The strings are padded to the
right so that they can be displayed as a table."

  (define (max-length-column lists column-index)
    "Return the maximum length of the string at position COLUMN-INDEX in the
list of string lists LISTS."
    (apply max
           (map (lambda (list)
                  (string-length
                   (list-ref list column-index)))
                lists)))

  (define (pad-descriptions descriptions)
    "Return a padded version of the list of string lists DESCRIPTIONS. The
strings are padded to the length of the longer string in a same column, as
determined by MAX-LENGTH-COLUMN procedure."
    (let* ((description-length (length (car descriptions)))
           (paddings (map (lambda (index)
                            (max-length-column descriptions index))
                          (iota description-length))))
      (map (lambda (description)
             (map string-pad-right description paddings))
           descriptions)))

  (let* ((descriptions
          (map
           (lambda (partition)
             (let ((user-partition
                    (find-user-partition-by-parted-object user-partitions
                                                          partition)))
               (partition-description partition user-partition)))
           partitions))
         (padded-descriptions (if (null? partitions)
                                  '()
                                  (pad-descriptions descriptions))))
    (map (cut string-join <> " ") padded-descriptions)))

(define (user-partition-description user-partition)
  "Return a string describing the given USER-PARTITION record."
  (let* ((partition (user-partition-parted-object user-partition))
         (disk (partition-disk partition))
         (disk-type (disk-disk-type disk))
         (device (disk-device disk))
         (has-name? (disk-type-check-feature
                     disk-type
                     DISK-TYPE-FEATURE-PARTITION-NAME))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED))
         (name (user-partition-name user-partition))
         (type (user-partition-type user-partition))
         (type-name (symbol->string type))
         (fs-type (user-partition-fs-type user-partition))
         (fs-type-name (user-fs-type-name fs-type))
         (bootable? (user-partition-bootable? user-partition))
         (esp? (user-partition-esp? user-partition))
         (need-formatting? (user-partition-need-formatting? user-partition))
         (crypt-label (user-partition-crypt-label user-partition))
         (size (user-partition-size user-partition))
         (mount-point (user-partition-mount-point user-partition)))
    `(,@(if has-name?
            `((name . ,(format #f (G_ "Name: ~a")
                               (or name (G_ "None")))))
            '())
      ,@(if (and has-extended?
                 (freespace-partition? partition)
                 (not (eq? type 'logical)))
            `((type . ,(format #f (G_ "Type: ~a") type-name)))
            '())
      ,@(if (eq? type 'extended)
            '()
            `((fs-type . ,(format #f (G_ "File system type: ~a")
                                  fs-type-name))))
      ,@(if (or (eq? type 'extended)
                (eq? fs-type 'swap)
                (not has-extended?))
            '()
            `((bootable . ,(format #f (G_ "Bootable flag: ~:[off~;on~]")
                                   bootable?))))
      ,@(if (and (not has-extended?)
                 (not (eq? fs-type 'swap)))
            `((esp? . ,(format #f (G_ "ESP flag: ~:[off~;on~]") esp?)))
            '())
      ,@(if (freespace-partition? partition)
            (let ((size-formatted
                   (or size (unit-format device   ;XXX: i18n
                                         (partition-length partition)))))
              `((size . ,(format #f (G_ "Size: ~a") size-formatted))))
            '())
      ,@(if (or (eq? type 'extended)
                (eq? fs-type 'swap))
            '()
            `((crypt-label
               . ,(format #f (G_ "Encryption: ~:[No~a~;Yes (label '~a')~]")
                          crypt-label (or crypt-label "")))))
      ,@(if (or (freespace-partition? partition)
                (eq? fs-type 'swap))
            '()
            `((need-formatting?
               . ,(format #f (G_ "Format the partition? ~:[No~;Yes~]")
                          need-formatting?))))
      ,@(if (or (eq? type 'extended)
                (eq? fs-type 'swap))
            '()
            `((mount-point
               . ,(format #f (G_ "Mount point: ~a")
                          (or mount-point
                              (and esp? (default-esp-mount-point))
                              (G_ "None")))))))))


;;
;; Partition table creation.
;;

(define (mklabel device type-name)
  "Create a partition table on DEVICE. TYPE-NAME is the type of the partition
table, \"msdos\" or \"gpt\"."
  (let* ((type (disk-type-get type-name))
         (disk (disk-new-fresh device type)))
    (or disk
        (raise
         (condition
          (&error)
          (&message (message (format #f "Cannot create partition table of type
~a on device ~a." type-name (device-path device)))))))))


;;
;; Partition creation.
;;

;; The maximum count of primary partitions is exceeded.
(define-condition-type &max-primary-exceeded &condition
  max-primary-exceeded?)

;; It is not possible to create an extended partition.
(define-condition-type &extended-creation-error &condition
  extended-creation-error?)

;; It is not possible to create a logical partition.
(define-condition-type &logical-creation-error &condition
  logical-creation-error?)

(define (can-create-primary? disk)
  "Return #t if it is possible to create a primary partition on DISK, return
#f otherwise."
  (let ((max-primary (disk-get-max-primary-partition-count disk)))
    (find (lambda (number)
            (not (disk-get-partition disk number)))
          (iota max-primary 1))))

(define (can-create-extended? disk)
  "Return #t if it is possible to create an extended partition on DISK, return
#f otherwise."
  (let* ((disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED)))
    (and (can-create-primary? disk)
         has-extended?
         (not (disk-extended-partition disk)))))

(define (can-create-logical? disk)
  "Return #t is it is possible to create a logical partition on DISK, return
#f otherwise."
  (let* ((disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED)))
    (and has-extended?
         (disk-extended-partition disk))))

(define (can-create-partition? user-part)
  "Return #t if it is possible to create the given USER-PART record, return #f
otherwise."
  (let* ((type (user-partition-type user-part))
         (partition (user-partition-parted-object user-part))
         (disk (partition-disk partition)))
    (case type
      ((normal)
       (or (can-create-primary? disk)
           (raise
            (condition (&max-primary-exceeded)))))
      ((extended)
       (or (can-create-extended? disk)
           (raise
            (condition (&extended-creation-error)))))
      ((logical)
       (or (can-create-logical? disk)
           (raise
            (condition (&logical-creation-error))))))))

(define* (mkpart disk user-partition
                 #:key (previous-partition #f))
  "Create the given USER-PARTITION on DISK. The PREVIOUS-PARTITION argument as
to be set to the partition preceding USER-PARTITION if any."

  (define (parse-start-end start end)
    "Parse start and end strings as positions on DEVICE expressed with a unit,
like '100GB' or '12.2KiB'. Return a list of 4 elements, the start sector, its
range (1 unit large area centered on start sector), the end sector and its
range."
    (let ((device (disk-device disk)))
      (call-with-values
          (lambda ()
            (unit-parse start device))
        (lambda (start-sector start-range)
          (call-with-values
              (lambda ()
                (unit-parse end device))
            (lambda (end-sector end-range)
              (list start-sector start-range
                    end-sector end-range)))))))

  (define* (extend-ranges! start-range end-range
                           #:key (offset 0))
    "Try to extend START-RANGE by 1 MEBIBYTE to the right and END-RANGE by 1
MEBIBYTE to the left. This way, if the disk is aligned on 2048 sectors of
512KB (like frequently), we will have a chance for the
'optimal-align-constraint' to succeed. Do not extend ranges if that would
cause them to cross."
    (let* ((device (disk-device disk))
           (start-range-end (geometry-end start-range))
           (end-range-start (geometry-start end-range))
           (mebibyte-sector-size (/ MEBIBYTE-SIZE
                                    (device-sector-size device)))
           (new-start-range-end
            (+ start-range-end mebibyte-sector-size offset))
           (new-end-range-start
            (- end-range-start mebibyte-sector-size offset)))
      (when (< new-start-range-end new-end-range-start)
        (geometry-set-end start-range new-start-range-end)
        (geometry-set-start end-range new-end-range-start))))

  (match (parse-start-end (user-partition-start user-partition)
                          (user-partition-end user-partition))
    ((start-sector start-range end-sector end-range)
     (let* ((prev-end (if previous-partition
                          (partition-end previous-partition)
                          0))
            (start-distance (- start-sector prev-end))
            (type (user-partition-type user-partition))
            ;; There should be at least 2 unallocated sectors in front of each
            ;; logical partition, otherwise parted will fail badly:
            ;; https://gparted.org/h2-fix-msdos-pt.php#apply-action-fail.
            (start-offset (if previous-partition
                              (- 3 start-distance)
                              0))
            (start-sector* (if (and (eq? type 'logical)
                                    (< start-distance 3))
                               (+ start-sector start-offset)
                               start-sector)))
       ;; This is a hack.  Parted almost always fails to create optimally
       ;; aligned partitions (unless specifying percentages) because the
       ;; default range of 1MB centered on the start sector is not enough when
       ;; the optimal alignment is 2048 sectors of 512KB.
       (extend-ranges! start-range end-range #:offset start-offset)

       (let* ((device (disk-device disk))
              (disk-type (disk-disk-type disk))
              (length (device-length device))
              (name (user-partition-name user-partition))
              (filesystem-type
               (filesystem-type-get
                (user-fs-type-name
                 (user-partition-fs-type user-partition))))
              (flags `(,@(if (user-partition-bootable? user-partition)
                             `(,PARTITION-FLAG-BOOT)
                             '())
                       ,@(if (user-partition-esp? user-partition)
                             `(,PARTITION-FLAG-ESP)
                             '())
                       ,@(if (user-partition-bios-grub? user-partition)
                             `(,PARTITION-FLAG-BIOS-GRUB)
                             '())))
              (has-name? (disk-type-check-feature
                          disk-type
                          DISK-TYPE-FEATURE-PARTITION-NAME))
              (partition-type (partition-type->int type))
              (partition (partition-new disk
                                        #:type partition-type
                                        #:filesystem-type filesystem-type
                                        #:start start-sector*
                                        #:end end-sector))
              (user-constraint (constraint-new
                                #:start-align 'any
                                #:end-align 'any
                                #:start-range start-range
                                #:end-range end-range
                                #:min-size 1
                                #:max-size length))
              (dev-constraint
               (device-get-optimal-aligned-constraint device))
              (final-constraint (constraint-intersect user-constraint
                                                      dev-constraint))
              (no-constraint (constraint-any device))
              ;; Try to create a partition with an optimal alignment
              ;; constraint. If it fails, fallback to creating a partition
              ;; with no specific constraint.
              (partition-constraint-ok?
               (disk-add-partition disk partition final-constraint))
              (partition-no-contraint-ok?
               (or partition-constraint-ok?
                   (disk-add-partition disk partition no-constraint)))
              (partition-ok?
               (or partition-constraint-ok? partition-no-contraint-ok?)))
         (installer-log-line "Creating partition:")
         (installer-log-line "~/type: ~a" partition-type)
         (installer-log-line "~/filesystem-type: ~a"
                             (filesystem-type-name filesystem-type))
         (installer-log-line "~/flags: ~a" flags)
         (installer-log-line "~/start: ~a" start-sector*)
         (installer-log-line "~/end: ~a" end-sector)
         (installer-log-line "~/start-range: [~a, ~a]"
                             (geometry-start start-range)
                             (geometry-end start-range))
         (installer-log-line "~/end-range: [~a, ~a]"
                             (geometry-start end-range)
                             (geometry-end end-range))
         (installer-log-line "~/constraint: ~a"
                             partition-constraint-ok?)
         (installer-log-line "~/no-constraint: ~a"
                             partition-no-contraint-ok?)
         ;; Set the partition name if supported.
         (when (and partition-ok? has-name? name)
           (partition-set-name partition name))

         ;; Both partition-set-system and partition-set-flag calls can affect
         ;; the partition type.  Their order is important, see:
         ;; https://issues.guix.gnu.org/55549.
         (partition-set-system partition filesystem-type)

         ;; Set flags if required.
         (for-each (lambda (flag)
                     (and (partition-is-flag-available? partition flag)
                          (partition-set-flag partition flag 1)))
                   flags)

         (and partition-ok? partition))))))


;;
;; Partition destruction.
;;

(define (rmpart disk number)
  "Remove the partition with the given NUMBER on DISK."
  (let ((partition (disk-get-partition disk number)))
    (disk-remove-partition* disk partition)))


;;
;; Auto partitionning.
;;

(define* (create-adjacent-partitions! disk partitions
                                      #:key (last-partition-end 0))
  "Create the given PARTITIONS on DISK. LAST-PARTITION-END is the sector from
which we want to start creating partitions. The START and END of each created
partition are computed from its SIZE value and the position of the last
partition."
  (let ((device (disk-device disk)))
    (let loop ((partitions partitions)
               (remaining-space (- (device-length device)
                                   last-partition-end))
               (start last-partition-end))
      (match partitions
        (() '())
        ((partition . rest)
         (let* ((size (user-partition-size partition))
                (percentage-size (and (string? size)
                                      (read-percentage size)))
                (sector-size (device-sector-size device))
                (partition-size (if percentage-size
                                    (exact->inexact
                                     (* (/ percentage-size 100)
                                        remaining-space))
                                    size))
                (end-partition (min (- (device-length device) 1)
                                    (nearest-exact-integer
                                     (+ start partition-size 1))))
                (name (user-partition-name partition))
                (type (user-partition-type partition))
                (fs-type (user-partition-fs-type partition))
                (start-formatted (unit-format-custom device
                                                     start
                                                     UNIT-SECTOR))
                (end-formatted (unit-format-custom device
                                                   end-partition
                                                   UNIT-SECTOR))
                (new-user-partition (user-partition
                                     (inherit partition)
                                     (start start-formatted)
                                     (end end-formatted)))
                (new-partition
                 (mkpart disk new-user-partition)))
           (if new-partition
               (cons (user-partition
                      (inherit new-user-partition)
                      (file-name (partition-get-path new-partition))
                      (disk-file-name (device-path device))
                      (parted-object new-partition))
                     (loop rest
                           (if (eq? type 'extended)
                               remaining-space
                               (- remaining-space
                                  (partition-length new-partition)))
                           (if (eq? type 'extended)
                               (+ start 1)
                               (+ (partition-end new-partition) 1))))
               (error
                (format #f "Unable to create partition ~a~%" name)))))))))

(define (force-user-partitions-formatting user-partitions)
  "Set the NEED-FORMATTING? fields to #t on all <user-partition> records of
USER-PARTITIONS list and return the updated list."
  (map (lambda (p)
         (user-partition
          (inherit p)
          (need-formatting? #t)))
       user-partitions))

(define* (auto-partition! disk
                          #:key
                          (scheme 'entire-root))
  "Automatically create partitions on DISK. All the previous
partitions (except the ESP on a GPT disk, if present) are wiped. SCHEME is the
desired partitioning scheme. It can be 'entire-root or
'entire-root-home. 'entire-root will create a swap partition and a root
partition occupying all the remaining space. 'entire-root-home will create a
swap partition, a root partition and a home partition.

Return the complete list of partitions on DISK, including the ESP when it
exists."
  (let* ((device (disk-device disk))
         (disk-type (disk-disk-type disk))
         (has-extended? (disk-type-check-feature
                         disk-type
                         DISK-TYPE-FEATURE-EXTENDED))
         (partitions (filter data-partition? (disk-partitions disk)))
         (esp-partition (find-esp-partition partitions))
         ;; According to
         ;; https://wiki.archlinux.org/index.php/EFI_system_partition, the ESP
         ;; size should be at least 550MiB.
         (new-esp-size (nearest-exact-integer
                        (/ (* 550 MEBIBYTE-SIZE)
                           (device-sector-size device))))
         (end-esp-partition (and esp-partition
                                 (partition-end esp-partition)))
         (non-boot-partitions (remove esp-partition? partitions))
         (bios-grub-size (/ (* 3 MEBIBYTE-SIZE)
                            (device-sector-size device)))
         (five-percent-disk (nearest-exact-integer
                             (* 0.05 (device-length device))))
         (default-swap-size (nearest-exact-integer
                             (/ (* 4 GIGABYTE-SIZE)
                                (device-sector-size device))))
         ;; Use a 4GB size for the swap if it represents less than 5% of the
         ;; disk space. Otherwise, set the swap size to 5% of the disk space.
         (swap-size (min default-swap-size five-percent-disk)))

    ;; Remove everything but esp if it exists.
    (for-each
     (lambda (partition)
       (and (data-partition? partition)
            ;; Do not remove logical partitions ourselves, since
            ;; disk-remove-partition* will remove all the logical partitions
            ;; residing on an extended partition, which would lead to a
            ;; double-remove and ensuing SEGFAULT.
            (not (logical-partition? partition))
            (disk-remove-partition* disk partition)))
     non-boot-partitions)

    (let* ((start-partition
            (if (efi-installation?)
                (and (not esp-partition)
                     (user-partition
                      (fs-type 'fat32)
                      (esp? #t)
                      (size new-esp-size)
                      (mount-point (default-esp-mount-point))))
                (user-partition
                 (fs-type 'ext4)
                 (bootable? #t)
                 (bios-grub? #t)
                 (size bios-grub-size))))
           (new-partitions
            (cond
             ((or (eq? scheme 'entire-root)
                  (eq? scheme 'entire-encrypted-root))
              (let ((encrypted? (eq? scheme 'entire-encrypted-root)))
                `(,@(if start-partition
                        `(,start-partition)
                        '())
                  ,@(if encrypted?
                        '()
                        `(,(user-partition
                            (fs-type 'swap)
                            (size swap-size))))
                  ,(user-partition
                    (fs-type 'ext4)
                    (bootable? has-extended?)
                    (crypt-label (and encrypted? "cryptroot"))
                    (size "100%")
                    (mount-point "/")))))
             ((or (eq? scheme 'entire-root-home)
                  (eq? scheme 'entire-encrypted-root-home))
              (let ((encrypted? (eq? scheme 'entire-encrypted-root-home)))
                `(,@(if start-partition
                        `(,start-partition)
                        '())
                  ,(user-partition
                    (fs-type 'ext4)
                    (bootable? has-extended?)
                    (crypt-label (and encrypted? "cryptroot"))
                    (size "33%")
                    (mount-point "/"))
                  ,@(if has-extended?
                        `(,(user-partition
                            (type 'extended)
                            (size "100%")))
                        '())
                  ,@(if encrypted?
                        '()
                        `(,(user-partition
                            (type (if has-extended?
                                      'logical
                                      'normal))
                            (fs-type 'swap)
                            (size swap-size))))
                  ,(user-partition
                    (type (if has-extended?
                              'logical
                              'normal))
                    (fs-type 'ext4)
                    (crypt-label (and encrypted? "crypthome"))
                    (size "100%")
                    (mount-point "/home")))))))
           (new-partitions* (force-user-partitions-formatting
                             new-partitions)))
      (append (if esp-partition
                  (list (partition->user-partition esp-partition))
                  '())
              (create-adjacent-partitions! disk
                                           new-partitions*
                                           #:last-partition-end
                                           (or end-esp-partition 0))))))


;;
;; Convert user-partitions.
;;

;; No root mount point found.
(define-condition-type &no-root-mount-point &condition
  no-root-mount-point?)

;; Cannot not read the partition UUID.
(define-condition-type &cannot-read-uuid &condition
  cannot-read-uuid?
  (partition cannot-read-uuid-partition))

(define (check-user-partitions user-partitions)
  "Check the following statements:

The USER-PARTITIONS list contains one <user-partition> record with a
mount-point set to '/'.  Raise &no-root-mount-point condition otherwise.

All the USER-PARTITIONS with a mount point and that will not be formatted have
a valid UUID.  Raise a &cannot-read-uuid condition specifying the faulty
partition otherwise.

Return #t if all the statements are valid."
  (define (check-root)
    (let ((mount-points
           (map user-partition-mount-point user-partitions)))
      (or (member "/" mount-points)
          (raise
           (condition (&no-root-mount-point))))))

  (define (check-uuid)
    (let ((mount-partitions
           (filter user-partition-mount-point user-partitions)))
      (every
       (lambda (user-partition)
         (let ((file-name (user-partition-file-name user-partition))
               (need-formatting?
                (user-partition-need-formatting? user-partition)))
           (or need-formatting?
               (read-partition-uuid/retry file-name)
               (raise
                (condition
                 (&cannot-read-uuid
                  (partition file-name)))))))
       mount-partitions)))

  (and (check-root)
       (check-uuid)
       #t))

(define (set-user-partitions-file-name user-partitions)
  "Set the partition file-name of <user-partition> records in USER-PARTITIONS
list and return the updated list."
  (map (lambda (p)
         (let* ((partition (user-partition-parted-object p))
                (file-name (partition-get-path partition)))
           (user-partition
            (inherit p)
            (file-name file-name))))
       user-partitions))

(define (create-btrfs-file-system partition)
  "Create a btrfs file-system for PARTITION file-name."
   ((run-command-in-installer) "mkfs.btrfs" "-f" partition))

(define (create-ext4-file-system partition)
  "Create an ext4 file-system for PARTITION file-name."
   ((run-command-in-installer) "mkfs.ext4" "-F" partition))

(define (create-fat16-file-system partition)
  "Create a fat16 file-system for PARTITION file-name."
   ((run-command-in-installer) "mkfs.fat" "-F16" partition))

(define (create-fat32-file-system partition)
  "Create a fat32 file-system for PARTITION file-name."
   ((run-command-in-installer) "mkfs.fat" "-F32" partition))

(define (create-jfs-file-system partition)
  "Create a JFS file-system for PARTITION file-name."
   ((run-command-in-installer) "jfs_mkfs" "-f" partition))

(define (create-ntfs-file-system partition)
  "Create a JFS file-system for PARTITION file-name."
   ((run-command-in-installer) "mkfs.ntfs" "-F" "-f" partition))

(define (create-xfs-file-system partition)
  "Create an XFS file-system for PARTITION file-name."
   ((run-command-in-installer) "mkfs.xfs" "-f" partition))

(define (create-swap-partition partition)
  "Set up swap area on PARTITION file-name."
   ((run-command-in-installer) "mkswap" "-f" partition))

(define (call-with-luks-key-file password proc)
  "Write PASSWORD in a temporary file and pass it to PROC as argument."
  (call-with-temporary-output-file
   (lambda (file port)
     (put-string port password)
     (close port)
     (proc file))))

(define (user-partition-upper-file-name user-partition)
  "Return the file-name of the virtual block device corresponding to
USER-PARTITION if it is encrypted, or the plain file-name otherwise."
  (let ((crypt-label (user-partition-crypt-label user-partition))
        (file-name (user-partition-file-name user-partition)))
    (if crypt-label
        (string-append "/dev/mapper/" crypt-label)
        file-name)))

(define (luks-format-and-open user-partition)
  "Format and open the encrypted partition pointed by USER-PARTITION."
  (let* ((file-name (user-partition-file-name user-partition))
         (label (user-partition-crypt-label user-partition))
         (password (secret-content (user-partition-crypt-password user-partition))))
    (call-with-luks-key-file
     password
     (lambda (key-file)
       (installer-log-line "formatting and opening LUKS entry ~s at ~s"
               label file-name)
       ((run-command-in-installer) "cryptsetup" "-q" "luksFormat"
        file-name key-file)
       ((run-command-in-installer) "cryptsetup" "open" "--type" "luks"
        "--key-file" key-file file-name label)))))

(define (luks-ensure-open user-partition)
  "Ensure partition pointed by USER-PARTITION is opened."
  (unless (file-exists? (user-partition-upper-file-name user-partition))
    (let* ((file-name (user-partition-file-name user-partition))
           (label (user-partition-crypt-label user-partition))
           (password (secret-content (user-partition-crypt-password user-partition))))
      (call-with-luks-key-file
       password
       (lambda (key-file)
         (installer-log-line "opening LUKS entry ~s at ~s"
                             label file-name)
         ((run-command-in-installer) "cryptsetup" "open" "--type" "luks"
          "--key-file" key-file file-name label))))))

(define (luks-close user-partition)
  "Close the encrypted partition pointed by USER-PARTITION."
  (let ((label (user-partition-crypt-label user-partition)))
    (installer-log-line "closing LUKS entry ~s" label)
    ((run-command-in-installer) "cryptsetup" "close" label)))

(define (format-user-partitions user-partitions)
  "Format the <user-partition> records in USER-PARTITIONS list with
NEED-FORMATTING? field set to #t."
  (for-each
   (lambda (user-partition)
     (let* ((need-formatting?
             (user-partition-need-formatting? user-partition))
            (type (user-partition-type user-partition))
            (crypt-label (user-partition-crypt-label user-partition))
            (file-name (user-partition-upper-file-name user-partition))
            (fs-type (user-partition-fs-type user-partition)))
       (when crypt-label
         (luks-format-and-open user-partition))

       (case fs-type
         ((btrfs)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-btrfs-file-system file-name)))
         ((ext4)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-ext4-file-system file-name)))
         ((fat16)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-fat16-file-system file-name)))
         ((fat32)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-fat32-file-system file-name)))
         ((jfs)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-jfs-file-system file-name)))
         ((ntfs)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-ntfs-file-system file-name)))
         ((xfs)
          (and need-formatting?
               (not (eq? type 'extended))
               (create-xfs-file-system file-name)))
         ((swap)
          (create-swap-partition file-name))
         (else
          ;; TODO: Add support for other file-system types.
          #t))))
   user-partitions))

(define (sort-partitions user-partitions)
  "Sort USER-PARTITIONS by mount-points, so that the more nested mount-point
comes last. This is useful to mount/umount partitions in a coherent order."
  (sort user-partitions
        (lambda (a b)
          (let ((mount-point-a (user-partition-mount-point a))
                (mount-point-b (user-partition-mount-point b)))
            (string-prefix? mount-point-a mount-point-b)))))

(define (mount-user-partitions user-partitions)
  "Mount the <user-partition> records in USER-PARTITIONS list on their
respective mount-points."
  (let* ((mount-partitions (filter user-partition-mount-point user-partitions))
         (sorted-partitions (sort-partitions mount-partitions)))
    (for-each (lambda (user-partition)
                (let* ((mount-point
                        (user-partition-mount-point user-partition))
                       (target
                        (string-append (%installer-target-dir)
                                       mount-point))
                       (fs-type
                        (user-partition-fs-type user-partition))
                       (crypt-label
                        (user-partition-crypt-label user-partition))
                       (mount-type
                        (user-fs-type->mount-type fs-type))
                       (file-name
                        (user-partition-upper-file-name user-partition)))
                  (when crypt-label
                    (luks-ensure-open user-partition))
                  (mkdir-p target)
                  (installer-log-line "mounting ~s on ~s" file-name target)
                  (mount file-name target mount-type)))
              sorted-partitions)))

(define (umount-user-partitions user-partitions)
  "Unmount all the <user-partition> records in USER-PARTITIONS list."
  (let* ((mount-partitions (filter user-partition-mount-point user-partitions))
         (sorted-partitions (sort-partitions mount-partitions)))
    (for-each (lambda (user-partition)
                (let* ((mount-point
                        (user-partition-mount-point user-partition))
                       (crypt-label
                        (user-partition-crypt-label user-partition))
                       (target
                        (string-append (%installer-target-dir)
                                       mount-point)))
                  (installer-log-line "unmounting ~s" target)
                  (umount target)
                  (when crypt-label
                    (luks-close user-partition))))
              (reverse sorted-partitions))))

(define (find-swap-user-partitions user-partitions)
  "Return the subset of <user-partition> records in USER-PARTITIONS list with
the FS-TYPE field set to 'swap, return the empty list if none found."
  (filter (lambda (user-partition)
            (let ((fs-type (user-partition-fs-type user-partition)))
              (eq? fs-type 'swap)))
          user-partitions))

(define (start-swapping user-partitions)
  "Start swapping on <user-partition> records with FS-TYPE equal to 'swap."
  (let* ((swap-user-partitions (find-swap-user-partitions user-partitions))
         (swap-devices (map user-partition-file-name swap-user-partitions)))
    (for-each swapon swap-devices)))

(define (stop-swapping user-partitions)
  "Stop swapping on <user-partition> records with FS-TYPE equal to 'swap."
  (let* ((swap-user-partitions (find-swap-user-partitions user-partitions))
         (swap-devices (map user-partition-file-name swap-user-partitions)))
    (for-each swapoff swap-devices)))

(define-syntax-rule (with-mounted-partitions user-partitions exp ...)
  "Mount USER-PARTITIONS and start swapping within the dynamic extent of EXP."
  (dynamic-wind
    (lambda ()
      (mount-user-partitions user-partitions)
      (start-swapping user-partitions))
    (lambda ()
      exp ...)
    (lambda ()
      (umount-user-partitions user-partitions)
      (stop-swapping user-partitions)
      #f)))

(define (user-partition->file-system user-partition)
  "Convert the given USER-PARTITION record in a FILE-SYSTEM record from
(gnu system file-systems) module and return it."
  (let* ((mount-point (user-partition-mount-point user-partition))
         (fs-type (user-partition-fs-type user-partition))
         (crypt-label (user-partition-crypt-label user-partition))
         (mount-type (user-fs-type->mount-type fs-type))
         (file-name (user-partition-file-name user-partition))
         (upper-file-name (user-partition-upper-file-name user-partition))
         ;; Only compute uuid if partition is not encrypted.
         (uuid (or crypt-label
                   (uuid->string (read-partition-uuid file-name) fs-type))))
    `(file-system
       (mount-point ,mount-point)
       (device ,@(if crypt-label
                     `(,upper-file-name)
                     `((uuid ,uuid (quote ,fs-type)))))
       (type ,mount-type)
       ,@(if crypt-label
             '((dependencies mapped-devices))
             '()))))

(define (user-partitions->file-systems user-partitions)
  "Convert the given USER-PARTITIONS list of <user-partition> records into a
list of <file-system> records."
  (filter-map
   (lambda (user-partition)
     (let ((mount-point
            (user-partition-mount-point user-partition)))
       (and mount-point
            (user-partition->file-system user-partition))))
   user-partitions))

(define (user-partition->mapped-device user-partition)
  "Convert the given USER-PARTITION record into a MAPPED-DEVICE record
from (gnu system mapped-devices) and return it."
  (let ((label (user-partition-crypt-label user-partition))
        (file-name (user-partition-file-name user-partition)))
    `(mapped-device
      (source (uuid ,(uuid->string
                      (read-luks-partition-uuid file-name)
                      'luks)))
      (target ,label)
      (type luks-device-mapping))))

(define (root-user-partition? partition)
  "Return true if PARTITION is the root partition."
  (let ((mount-point (user-partition-mount-point partition)))
    (and mount-point
         (string=? mount-point "/"))))

(define (bootloader-configuration user-partitions)
  "Return the bootloader configuration field for USER-PARTITIONS."
  (let* ((root-partition (find root-user-partition?
                               user-partitions))
         (root-partition-disk (user-partition-disk-file-name root-partition)))
    `((bootloader-configuration
       ,@(if (efi-installation?)
             `((bootloader grub-efi-bootloader)
               (targets (list ,(default-esp-mount-point))))
             `((bootloader grub-bootloader)
               (targets (list ,root-partition-disk))))

       ;; XXX: Assume we defined the 'keyboard-layout' field of
       ;; <operating-system> right above.
       (keyboard-layout keyboard-layout)))))

(define (user-partition-missing-modules user-partitions)
  "Return the list of kernel modules missing from the default set of kernel
modules to access USER-PARTITIONS."
  (let ((devices (filter user-partition-crypt-label user-partitions))
        (root    (find root-user-partition? user-partitions)))
    (delete-duplicates
     (append-map (lambda (device)
                   (catch 'system-error
                     (lambda ()
                       (missing-modules device %base-initrd-modules))
                     (const '())))
                 (delete-duplicates
                  (map user-partition-file-name
                       (cons root devices)))))))

(define (initrd-configuration user-partitions)
  "Return an 'initrd-modules' field with everything needed for
USER-PARTITIONS, or return nothing."
  (match (user-partition-missing-modules user-partitions)
    (()
     '())
    ((modules ...)
     `((initrd-modules (append ',modules
                               %base-initrd-modules))))))

(define (user-partitions->configuration user-partitions)
  "Return the configuration field for USER-PARTITIONS."
  (let* ((swap-user-partitions (find-swap-user-partitions user-partitions))
         (swap-devices (map user-partition-file-name swap-user-partitions))
         (encrypted-partitions
          (filter user-partition-crypt-label user-partitions)))
    `((bootloader ,@(bootloader-configuration user-partitions))
      ,@(initrd-configuration user-partitions)
      ,@(if (null? swap-devices)
            '()
            (let* ((uuids (map (lambda (file)
                                 (uuid->string (read-partition-uuid file)))
                               swap-devices)))
              `((swap-devices
                 (list ,@(map (lambda (uuid)
                                `(swap-space
                                  (target (uuid ,uuid))))
                              uuids))))))
      ,@(if (null? encrypted-partitions)
            '()
            `((mapped-devices
               (list ,@(map user-partition->mapped-device
                            encrypted-partitions)))))

      ,(vertical-space 1)
      ,(let-syntax ((G_ (syntax-rules () ((_ str) str))))
         (comment (G_ "\
;; The list of file systems that get \"mounted\".  The unique
;; file system identifiers there (\"UUIDs\") can be obtained
;; by running 'blkid' in a terminal.\n")))
      (file-systems (cons*
                     ,@(user-partitions->file-systems user-partitions)
                     %base-file-systems)))))


;;
;; Initialization.
;;

(define (init-parted)
  "Initialize libparted support."
  (probe-all-devices!)
  ;; Remove all logical devices, otherwise "device-is-busy?" will report true
  ;; on all devices containaing active logical volumes.
  (remove-logical-devices)
  (exception-set-handler (lambda (exception)
                           EXCEPTION-OPTION-UNHANDLED)))

(define (free-parted devices)
  "Deallocate memory used for DEVICES in parted, force sync them and wait for
the devices not to be used before returning."
  ;; XXX: Formatting and further operations on disk partition table may fail
  ;; because the partition table changes are not synced, or because the device
  ;; is still in use, even if parted should have finished editing
  ;; partitions. This is not well understood, but syncing devices and waiting
  ;; them to stop returning EBUSY to BLKRRPART ioctl seems to be enough. The
  ;; same kind of issue is described here:
  ;; https://mail.gnome.org/archives/commits-list/2013-March/msg18423.html.
  (let ((device-file-names (map device-path devices)))
    (for-each force-device-sync devices)
    (for-each (lambda (file-name)
                (let/time ((time in-use?
                                 (with-delay-device-in-use? file-name)))
                  (if in-use?
                      (error
                       (format #f (G_ "Device ~a is still in use.")
                               file-name))
                      (installer-log-line "Syncing ~a took ~a seconds."
                              file-name (time-second time)))))
              device-file-names)))
