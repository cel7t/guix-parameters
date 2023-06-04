;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-git-authenticate)
  #:use-module (git)
  #:use-module (guix git)
  #:use-module (guix git-authenticate)
  #:use-module ((guix channels) #:select (openpgp-fingerprint))
  #:use-module ((guix diagnostics)
                #:select (formatted-message? formatted-message-arguments))
  #:use-module (guix openpgp)
  #:use-module ((guix tests) #:select (random-text))
  #:use-module (guix tests git)
  #:use-module (guix tests gnupg)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports))

;; Test the (guix git-authenticate) tools.

(define (gpg+git-available?)
  (and (which (git-command))
       (which (gpg-command)) (which (gpgconf-command))))


(test-begin "git-authenticate")

(unless (which (git-command)) (test-skip 1))
(test-assert "unsigned commits"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "first commit")
        (add "b.txt" "B")
        (commit "second commit"))
    (with-repository directory repository
      (let ((commit1 (find-commit repository "first"))
            (commit2 (find-commit repository "second")))
        (guard (c ((unsigned-commit-error? c)
                   (oid=? (git-authentication-error-commit c)
                          (commit-id commit1))))
          (authenticate-commits repository (list commit1 commit2)
                                #:keyring-reference "master")
          'failed)))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "signed commits, SHA1 signature"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    ;; Force use of SHA1 for signatures.
    (call-with-output-file (string-append (getenv "GNUPGHOME") "/gpg.conf")
      (lambda (port)
        (display "digest-algo sha1" port)))

    (with-temporary-git-repository directory
        `((add "a.txt" "A")
          (add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint %ed25519-public-key-file)
                                    (name "Charlie"))))))
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file))))
      (with-repository directory repository
        (let ((commit (find-commit repository "first")))
          (guard (c ((unsigned-commit-error? c)
                     (oid=? (git-authentication-error-commit c)
                            (commit-id commit))))
            (authenticate-commits repository (list commit)
                                  #:keyring-reference "master")
            'failed))))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "signed commits, default authorizations"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (with-temporary-git-repository directory
        `((add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (commit "zeroth commit")
          (add "a.txt" "A")
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "b.txt" "B")
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file))))
      (with-repository directory repository
        (let ((commit1 (find-commit repository "first"))
              (commit2 (find-commit repository "second")))
          (authenticate-commits repository (list commit1 commit2)
                                #:default-authorizations
                                (list (openpgp-public-key-fingerprint
                                       (read-openpgp-packet
                                        %ed25519-public-key-file)))
                                #:keyring-reference "master"))))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "signed commits, .guix-authorizations"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (with-temporary-git-repository directory
        `((add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Charlie"))))))
          (commit "zeroth commit")
          (add "a.txt" "A")
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add ".guix-authorizations"
               ,(object->string `(authorizations (version 0) ()))) ;empty
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "b.txt" "B")
          (commit "third commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file))))
      (with-repository directory repository
        (let ((commit1 (find-commit repository "first"))
              (commit2 (find-commit repository "second"))
              (commit3 (find-commit repository "third")))
          ;; COMMIT1 and COMMIT2 are fine.
          (and (authenticate-commits repository (list commit1 commit2)
                                     #:keyring-reference "master")

               ;; COMMIT3 is signed by an unauthorized key according to its
               ;; parent's '.guix-authorizations' file.
               (guard (c ((unauthorized-commit-error? c)
                          (and (oid=? (git-authentication-error-commit c)
                                      (commit-id commit3))
                               (bytevector=?
                                (openpgp-public-key-fingerprint
                                 (unauthorized-commit-error-signing-key c))
                                (openpgp-public-key-fingerprint
                                 (read-openpgp-packet
                                  %ed25519-public-key-file))))))
                 (authenticate-commits repository
                                       (list commit1 commit2 commit3)
                                       #:keyring-reference "master")
                 'failed)))))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "signed commits, .guix-authorizations, unauthorized merge"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file
                                %ed25519-2-public-key-file
                                %ed25519-2-secret-key-file)
    (with-temporary-git-repository directory
        `((add "signer1.key"
               ,(call-with-input-file %ed25519-public-key-file
                  get-string-all))
          (add "signer2.key"
               ,(call-with-input-file %ed25519-2-public-key-file
                  get-string-all))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Alice"))))))
          (commit "zeroth commit")
          (add "a.txt" "A")
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (branch "devel")
          (checkout "devel")
          (add "devel/1.txt" "1")
          (commit "first devel commit"
                  (signer ,(key-fingerprint %ed25519-2-public-key-file)))
          (checkout "master")
          (add "b.txt" "B")
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (merge "devel" "merge"
                 (signer ,(key-fingerprint %ed25519-public-key-file))))
      (with-repository directory repository
        (let ((master1 (find-commit repository "first commit"))
              (master2 (find-commit repository "second commit"))
              (devel1  (find-commit repository "first devel commit"))
              (merge   (find-commit repository "merge")))
          (define (correct? c commit)
            (and (oid=? (git-authentication-error-commit c)
                        (commit-id commit))
                 (bytevector=?
                  (openpgp-public-key-fingerprint
                   (unauthorized-commit-error-signing-key c))
                  (openpgp-public-key-fingerprint
                   (read-openpgp-packet %ed25519-2-public-key-file)))))

          (and (authenticate-commits repository (list master1 master2)
                                     #:keyring-reference "master")

               ;; DEVEL1 is signed by an unauthorized key according to its
               ;; parent's '.guix-authorizations' file.
               (guard (c ((unauthorized-commit-error? c)
                          (correct? c devel1)))
                 (authenticate-commits repository
                                       (list master1 devel1)
                                       #:keyring-reference "master")
                 #f)

               ;; MERGE is authorized but one of its ancestors is not.
               (guard (c ((unauthorized-commit-error? c)
                          (correct? c devel1)))
                 (authenticate-commits repository
                                       (list master1 master2
                                             devel1 merge)
                                       #:keyring-reference "master")
                 #f)))))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "signed commits, .guix-authorizations, authorized merge"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file
                                %ed25519-2-public-key-file
                                %ed25519-2-secret-key-file)
    (with-temporary-git-repository directory
        `((add "signer1.key"
               ,(call-with-input-file %ed25519-public-key-file
                  get-string-all))
          (add "signer2.key"
               ,(call-with-input-file %ed25519-2-public-key-file
                  get-string-all))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Alice"))))))
          (commit "zeroth commit")
          (add "a.txt" "A")
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (branch "devel")
          (checkout "devel")
          (add ".guix-authorizations"
               ,(object->string                   ;add the second signer
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Alice"))
                                   (,(key-fingerprint
                                      %ed25519-2-public-key-file))))))
          (commit "first devel commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "devel/2.txt" "2")
          (commit "second devel commit"
                  (signer ,(key-fingerprint %ed25519-2-public-key-file)))
          (checkout "master")
          (add "b.txt" "B")
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (merge "devel" "merge"
                 (signer ,(key-fingerprint %ed25519-public-key-file)))
          ;; After the merge, the second signer is authorized.
          (add "c.txt" "C")
          (commit "third commit"
                  (signer ,(key-fingerprint %ed25519-2-public-key-file))))
      (with-repository directory repository
        (let ((master1 (find-commit repository "first commit"))
              (master2 (find-commit repository "second commit"))
              (devel1  (find-commit repository "first devel commit"))
              (devel2  (find-commit repository "second devel commit"))
              (merge   (find-commit repository "merge"))
              (master3 (find-commit repository "third commit")))
          (authenticate-commits repository
                                (list master1 master2 devel1 devel2
                                      merge master3)
                                #:keyring-reference "master"))))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "signed commits, .guix-authorizations removed"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (with-temporary-git-repository directory
        `((add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                               get-string-all))
          (add ".guix-authorizations"
               ,(object->string
                 `(authorizations (version 0)
                                  ((,(key-fingerprint
                                      %ed25519-public-key-file)
                                    (name "Charlie"))))))
          (commit "zeroth commit")
          (add "a.txt" "A")
          (commit "first commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (remove ".guix-authorizations")
          (commit "second commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file)))
          (add "b.txt" "B")
          (commit "third commit"
                  (signer ,(key-fingerprint %ed25519-public-key-file))))
      (with-repository directory repository
        (let ((commit1 (find-commit repository "first"))
              (commit2 (find-commit repository "second"))
              (commit3 (find-commit repository "third")))
          ;; COMMIT1 and COMMIT2 are fine.
          (and (authenticate-commits repository (list commit1 commit2)
                                     #:keyring-reference "master")

               ;; COMMIT3 is rejected because COMMIT2 removes
               ;; '.guix-authorizations'.
               (guard (c ((unauthorized-commit-error? c)
                          (oid=? (git-authentication-error-commit c)
                                 (commit-id commit2))))
                 (authenticate-commits repository
                                       (list commit1 commit2 commit3)
                                       #:keyring-reference "master")
                 'failed)))))))

(unless (gpg+git-available?) (test-skip 1))
(test-assert "introductory commit, valid signature"
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (let ((fingerprint (key-fingerprint %ed25519-public-key-file)))
      (with-temporary-git-repository directory
          `((add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                                 get-string-all))
            (add ".guix-authorizations"
                 ,(object->string
                   `(authorizations (version 0)
                                    ((,(key-fingerprint
                                        %ed25519-public-key-file)
                                      (name "Charlie"))))))
            (commit "zeroth commit" (signer ,fingerprint))
            (add "a.txt" "A")
            (commit "first commit" (signer ,fingerprint)))
        (with-repository directory repository
          (let ((commit0 (find-commit repository "zero"))
                (commit1 (find-commit repository "first")))
            ;; COMMIT0 is signed with the right key, and COMMIT1 is fine.
            (authenticate-repository repository
                                     (commit-id commit0)
                                     (openpgp-fingerprint fingerprint)
                                     #:keyring-reference "master"
                                     #:cache-key (random-text))))))))

(unless (gpg+git-available?) (test-skip 1))
(test-equal "introductory commit, missing signature"
  'intro-lacks-signature
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (let ((fingerprint (key-fingerprint %ed25519-public-key-file)))
      (with-temporary-git-repository directory
          `((add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                                 get-string-all))
            (add ".guix-authorizations"
                 ,(object->string
                   `(authorizations (version 0)
                                    ((,(key-fingerprint
                                        %ed25519-public-key-file)
                                      (name "Charlie"))))))
            (commit "zeroth commit")              ;unsigned!
            (add "a.txt" "A")
            (commit "first commit" (signer ,fingerprint)))
        (with-repository directory repository
          (let ((commit0 (find-commit repository "zero")))
            ;; COMMIT0 is not signed.
            (guard (c ((formatted-message? c)
                       ;; Message like "commit ~a lacks a signature".
                       (and (equal? (formatted-message-arguments c)
                                    (list (oid->string (commit-id commit0))))
                            'intro-lacks-signature)))
              (authenticate-repository repository
                                       (commit-id commit0)
                                       (openpgp-fingerprint fingerprint)
                                       #:keyring-reference "master"
                                       #:cache-key (random-text)))))))))

(unless (gpg+git-available?) (test-skip 1))
(test-equal "introductory commit, wrong signature"
  'wrong-intro-signing-key
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file
                                %ed25519-2-public-key-file
                                %ed25519-2-secret-key-file)
    (let ((fingerprint (key-fingerprint %ed25519-public-key-file))
          (wrong-fingerprint (key-fingerprint %ed25519-2-public-key-file)))
      (with-temporary-git-repository directory
          `((add "signer1.key" ,(call-with-input-file %ed25519-public-key-file
                                  get-string-all))
            (add "signer2.key" ,(call-with-input-file %ed25519-2-public-key-file
                                  get-string-all))
            (add ".guix-authorizations"
                 ,(object->string
                   `(authorizations (version 0)
                                    ((,(key-fingerprint
                                        %ed25519-public-key-file)
                                      (name "Charlie"))))))
            (commit "zeroth commit" (signer ,wrong-fingerprint))
            (add "a.txt" "A")
            (commit "first commit" (signer ,fingerprint)))
        (with-repository directory repository
          (let ((commit0 (find-commit repository "zero"))
                (commit1 (find-commit repository "first")))
            ;; COMMIT0 is signed with the wrong key--not the one passed as the
            ;; SIGNER argument to 'authenticate-repository'.
            (guard (c ((formatted-message? c)
                       ;; Message like "commit ~a signed by ~a instead of ~a".
                       (and (equal? (formatted-message-arguments c)
                                    (list (oid->string (commit-id commit0))
                                          wrong-fingerprint fingerprint))
                            'wrong-intro-signing-key)))
             (authenticate-repository repository
                                      (commit-id commit0)
                                      (openpgp-fingerprint fingerprint)
                                      #:keyring-reference "master"
                                      #:cache-key (random-text)))))))))

(unless (gpg+git-available?) (test-skip 1))
(test-equal "authenticate-repository, target not a descendant of intro"
  'target-commit-not-a-descendant-of-intro
  (with-fresh-gnupg-setup (list %ed25519-public-key-file
                                %ed25519-secret-key-file)
    (let ((fingerprint (key-fingerprint %ed25519-public-key-file)))
      (with-temporary-git-repository directory
          `((add "signer.key" ,(call-with-input-file %ed25519-public-key-file
                                 get-string-all))
            (add ".guix-authorizations"
                 ,(object->string
                   `(authorizations (version 0)
                                    ((,(key-fingerprint
                                        %ed25519-public-key-file)
                                      (name "Charlie"))))))
            (commit "zeroth commit" (signer ,fingerprint))
            (branch "pre-intro-branch")
            (checkout "pre-intro-branch")
            (add "b.txt" "B")
            (commit "alternate commit" (signer ,fingerprint))
            (checkout "master")
            (add "a.txt" "A")
            (commit "first commit" (signer ,fingerprint))
            (add "c.txt" "C")
            (commit "second commit" (signer ,fingerprint)))
        (with-repository directory repository
          (let ((commit1 (find-commit repository "first"))
                (commit-alt
                 (commit-lookup repository
                                (reference-target
                                 (branch-lookup repository
                                                "pre-intro-branch")))))
            (guard (c ((formatted-message? c)
                       (and (equal? (formatted-message-arguments c)
                                    (list (oid->string (commit-id commit-alt))
                                          (oid->string (commit-id commit1))))
                            'target-commit-not-a-descendant-of-intro)))
              (authenticate-repository repository
                                       (commit-id commit1)
                                       (openpgp-fingerprint fingerprint)
                                       #:end (commit-id commit-alt)
                                       #:keyring-reference "master"
                                       #:cache-key (random-text)))))))))

(test-end "git-authenticate")
