;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu build install)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (install-boot-config
            evaluate-populate-directive
            populate-root-file-system
            install-database-and-gc-roots
            populate-single-profile-directory
            mount-cow-store
            unmount-cow-store))

;;; Commentary:
;;;
;;; This module supports the installation of the GNU system on a hard disk.
;;; It is meant to be used both in a build environment (in derivations that
;;; build VM images), and on the bare metal (when really installing the
;;; system.)
;;;
;;; Code:

(define (install-boot-config bootcfg bootcfg-location mount-point)
  "Atomically copy BOOTCFG into BOOTCFG-LOCATION on the MOUNT-POINT.  Note
that the caller must make sure that BOOTCFG is registered as a GC root so
that the fonts, background images, etc. referred to by BOOTCFG are not GC'd."
  (let* ((target (string-append mount-point bootcfg-location))
         (pivot  (string-append target ".new")))
    (mkdir-p (dirname target))

    ;; Copy BOOTCFG instead of just symlinking it, because symlinks won't
    ;; work when /boot is on a separate partition.  Do that atomically.
    (copy-file bootcfg pivot)
    (rename-file pivot target)))

(define* (evaluate-populate-directive directive target
                                      #:key
                                      (default-gid 0)
                                      (default-uid 0)
                                      (error-on-dangling-symlink? #t))
  "Evaluate DIRECTIVE, an sexp describing a file or directory to create under
directory TARGET.  DEFAULT-UID and DEFAULT-GID are the default UID and GID in
the context of the caller.  If the directive matches those defaults then,
'chown' won't be run.  When ERROR-ON-DANGLING-SYMLINK? is true, abort with an
error when a dangling symlink would be created."
  (define target* (if (string-suffix? "/" target)
                      target
                      (string-append target "/")))
  (let loop ((directive directive))
    (catch 'system-error
      (lambda ()
        (match directive
          (('directory name)
           (mkdir-p (string-append target* name)))
          (('directory name uid gid)
           (let ((dir (string-append target* name)))
             (mkdir-p dir)
             ;; If called from a context without "root" permissions, "chown"
             ;; to root will fail.  In that case, do not try to run "chown"
             ;; and assume that the file will be chowned elsewhere (when
             ;; interned in the store for instance).
             (or (and (= uid default-uid) (= gid default-gid))
                 (chown dir uid gid))))
          (('directory name uid gid mode)
           (loop `(directory ,name ,uid ,gid))
           (chmod (string-append target* name) mode))
          (('file name)
           (call-with-output-file (string-append target* name)
             (const #t)))
          (('file name (? string? content))
           (call-with-output-file (string-append target* name)
             (lambda (port)
               (display content port))))
          ((new '-> old)
           (let ((new* (string-append target* new)))
             (let try ()
               (catch 'system-error
                 (lambda ()
                   (when error-on-dangling-symlink?
                     ;; When the symbolic link points to a relative path,
                     ;; checking if its target exists must be done relatively
                     ;; to the link location.
                     (unless (if (string-prefix? "/" old)
                                 (file-exists? old)
                                 (with-directory-excursion (dirname new*)
                                   (file-exists? old)))
                       (error (format #f "symlink `~a' points to nonexistent \
file `~a'" new* old))))
                   (symlink old new*))
                 (lambda args
                   ;; When doing 'guix system init' on the current '/', some
                   ;; symlinks may already exists.  Override them.
                   (if (= EEXIST (system-error-errno args))
                       (begin
                         (delete-file new*)
                         (try))
                       (apply throw args)))))))))
      (lambda args
        ;; Usually we can only get here when installing to an existing root,
        ;; as with 'guix system init foo.scm /'.
        (format (current-error-port)
                "error: failed to evaluate directive: ~s~%"
                directive)
        (apply throw args)))))

(define (directives store)
  "Return a list of directives to populate the root file system that will host
STORE."
  `((directory ,store 0 0 #o1775)

    (directory "/etc")
    (directory "/var/log")                          ; for shepherd
    (directory "/var/guix/gcroots")
    (directory "/var/empty")                        ; for no-login accounts
    (directory "/var/db")                           ; for dhclient, etc.
    (directory "/var/run")
    (directory "/run")
    (directory "/mnt")
    (directory "/var/guix/profiles/per-user/root" 0 0)

    ;; Link to the initial system generation.
    ("/var/guix/profiles/system" -> "system-1-link")

    ("/var/guix/gcroots/booted-system" -> "/run/booted-system")
    ("/var/guix/gcroots/current-system" -> "/run/current-system")
    ("/var/guix/gcroots/profiles" -> "/var/guix/profiles")

    (directory "/bin")
    (directory "/tmp" 0 0 #o1777)                 ; sticky bit
    (directory "/var/tmp" 0 0 #o1777)
    (directory "/var/lock" 0 0 #o1777)

    (directory "/home" 0 0)))

(define* (populate-root-file-system system target
                                    #:key (extras '()))
  "Make the essential non-store files and directories on TARGET.  This
includes /etc, /var, /run, /bin/sh, etc., and all the symlinks to SYSTEM.
EXTRAS is a list of directives appended to the built-in directives to populate
TARGET."
  ;; It's expected that some symbolic link targets do not exist yet, so do not
  ;; error on dangling links.
  (for-each (cut evaluate-populate-directive <> target
                 #:error-on-dangling-symlink? #f)
            (append (directives (%store-directory)) extras))

  ;; Add system generation 1.
  (let ((generation-1 (string-append target
                                     "/var/guix/profiles/system-1-link")))
    (let try ()
      (catch 'system-error
        (lambda ()
          (symlink system generation-1))
        (lambda args
          ;; If GENERATION-1 already exists, overwrite it.
          (if (= EEXIST (system-error-errno args))
              (begin
                (delete-file generation-1)
                (try))
              (apply throw args)))))))

(define %root-profile
  "/var/guix/profiles/per-user/root")

(define* (install-database-and-gc-roots root database profile
                                        #:key (profile-name "guix-profile"))
  "Install DATABASE, the store database, under directory ROOT.  Create
PROFILE-NAME and have it link to PROFILE, a store item."
  (define (scope file)
    (string-append root "/" file))

  (define (mkdir-p* dir)
    (mkdir-p (scope dir)))

  (define (symlink* old new)
    (symlink old (scope new)))

  (install-file database (scope "/var/guix/db/"))
  (chmod (scope "/var/guix/db/db.sqlite") #o644)
  (mkdir-p* "/var/guix/profiles")
  (mkdir-p* "/var/guix/gcroots")
  (symlink* "/var/guix/profiles" "/var/guix/gcroots/profiles")

  ;; Make root's profile, which makes it a GC root.
  (mkdir-p* %root-profile)
  (symlink* profile
            (string-append %root-profile "/" profile-name "-1-link"))
  (symlink* (string-append profile-name "-1-link")
            (string-append %root-profile "/" profile-name)))

(define* (populate-single-profile-directory directory
                                            #:key profile closure
                                            (profile-name "guix-profile")
                                            database)
  "Populate DIRECTORY with a store containing PROFILE, whose closure is given
in the file called CLOSURE (as generated by #:references-graphs.)  DIRECTORY
is initialized to contain a single profile under /root pointing to PROFILE.

When DATABASE is true, copy it to DIRECTORY/var/guix/db and create
DIRECTORY/var/guix/gcroots and friends.

PROFILE-NAME is the name of the profile being created under
/var/guix/profiles, typically either \"guix-profile\" or \"current-guix\".

This is used to create the self-contained tarballs with 'guix pack'."
  (define (scope file)
    (string-append directory "/" file))

  (define (mkdir-p* dir)
    (mkdir-p (scope dir)))

  (define (symlink* old new)
    (symlink old (scope new)))

  ;; Populate the store.
  (populate-store (list closure) directory
                  #:deduplicate? #f)

  (when database
    (install-database-and-gc-roots directory database profile
                                   #:profile-name profile-name))

  (match profile-name
    ("guix-profile"
     (mkdir-p* "/root")
     (symlink* (string-append %root-profile "/guix-profile")
               "/root/.guix-profile"))
    ("current-guix"
     (mkdir-p* "/root/.config/guix")
     (symlink* (string-append %root-profile "/current-guix")
               "/root/.config/guix/current"))
    (_
     #t)))

(define (mount-cow-store target backing-directory)
  "Make the store copy-on-write, using TARGET as the backing store.  This is
useful when TARGET is on a hard disk, whereas the current store is on a RAM
disk."
  (define (set-store-permissions directory)
    "Set the right perms on DIRECTORY to use it as the store."
    (chown directory 0 30000)      ;use the fixed 'guixbuild' GID
    (chmod directory #o1775))

  (let ((tmpdir (string-append target "/tmp")))
    (mkdir-p tmpdir)
    (mount tmpdir "/tmp" "none" MS_BIND))

  (let* ((rw-dir (string-append target backing-directory))
         (work-dir (string-append rw-dir "/../.overlayfs-workdir")))
    (mkdir-p rw-dir)
    (mkdir-p work-dir)
    (mkdir-p "/.rw-store")
    (set-store-permissions rw-dir)
    (set-store-permissions "/.rw-store")

    ;; Mount the overlay, then atomically make it the store.
    (mount "none" "/.rw-store" "overlay" 0
           (string-append "lowerdir=" (%store-directory) ","
                          "upperdir=" rw-dir ","
                          "workdir=" work-dir))
    (mount "/.rw-store" (%store-directory) "" MS_MOVE)
    (rmdir "/.rw-store")))

(define (umount* directory)
  "Unmount DIRECTORY, but retry a few times upon EBUSY."
  (let loop ((attempts 5))
    (catch 'system-error
      (lambda ()
        (umount directory))
      (lambda args
        (if (and (= EBUSY (system-error-errno args))
                 (> attempts 0))
            (begin
              (sleep 1)
              (loop (- attempts 1)))
            (apply throw args))))))

(define (unmount-cow-store target backing-directory)
  "Unmount copy-on-write store."
  (let ((tmp-dir "/remove"))
    (mkdir-p tmp-dir)
    (mount (%store-directory) tmp-dir "" MS_MOVE)

    ;; We might get EBUSY at this point, possibly because of lingering
    ;; processes with open file descriptors.  Use 'umount*' to retry upon
    ;; EBUSY, leaving a bit of time.  See <https://issues.guix.gnu.org/59884>.
    (umount* tmp-dir)

    (rmdir tmp-dir)
    (delete-file-recursively
     (string-append target backing-directory))))

;;; install.scm ends here
