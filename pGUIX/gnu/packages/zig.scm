;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages zig)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm))

(define-public zig-0.10
  (package
    (name "zig")
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziglang/zig.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sh5xjsksl52i4cfv1qj36sz5h0ln7cq4pdhgs3960mk8a90im7b"))
       (patches (search-patches "zig-do-not-link-against-librt.patch"))))
    (build-system cmake-build-system)
    (inputs
     (list clang-15 ; Clang propagates llvm.
           lld-15
           zlib
           (list zstd "lib")))
    ;; Zig compiles fine with GCC, but also needs native LLVM libraries.
    (native-inputs
     (list llvm-15))
    (arguments
     `(#:configure-flags
       (list ,@(if (%current-target-system)
                   '(string-append "-DZIG_TARGET_TRIPLE="
                                   (%current-target-system))
                   '())
             (string-append "-DZIG_TARGET_MCPU=baseline")
             "-DZIG_SHARED_LLVM=ON"
             (string-append "-DZIG_LIB_DIR=" (assoc-ref %outputs "out")
                            "/lib/zig"))
       #:validate-runpath? #f       ; TODO: zig binary can't find ld-linux.
       #:out-of-source? #f ; for tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env-variables
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             ;; Set CC, since the stage 2 zig relies on it to find the libc
             ;; installation, and otherwise silently links against its own.
             (setenv "CC" ,(cc-for-target))
             ;; Set cache dir, otherwise Zig looks for `$HOME/.cache'.
             (setenv "ZIG_GLOBAL_CACHE_DIR"
                     (string-append (getcwd) "/zig-cache"))))
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Zig uses information about /usr/bin/env to determine the
             ;; version of glibc and other data.
             (substitute* "lib/std/zig/system/NativeTargetInfo.zig"
               (("/usr/bin/env") (search-input-file inputs "/bin/env")))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (when tests?
               (invoke (string-append (assoc-ref outputs "out") "/bin/zig")
                       "build" "test"
                       ;; We're not testing the compiler bootstrap chain.
                       "-Dskip-stage1"
                       "-Dskip-stage2-tests"
                       ;; Non-native tests try to link and execute non-native
                       ;; binaries.
                       "-Dskip-non-native")))))))
    (native-search-paths
     (list
      (search-path-specification
       (variable "C_INCLUDE_PATH")
       (files '("include")))
      (search-path-specification
       (variable "CPLUS_INCLUDE_PATH")
       (files '("include/c++" "include")))
      (search-path-specification
       (variable "LIBRARY_PATH")
       (files '("lib" "lib64")))))
    (synopsis "General purpose programming language and toolchain")
    (description "Zig is a general-purpose programming language and
toolchain.  Among other features it provides
@itemize
@item an Optional type instead of null pointers,
@item manual memory management,
@item generic data structures and functions,
@item compile-time reflection and compile-time code execution,
@item integration with C using zig as a C compiler, and
@item concurrency via async functions.
@end itemize")
    (home-page "https://github.com/ziglang/zig")
    ;; Currently building zig can take up to 10GB of RAM for linking stage1:
    ;; https://github.com/ziglang/zig/issues/6485
    (supported-systems %64bit-supported-systems)
    ;; Stage3 can take a lot of time and isn't verbose.
    (properties `((max-silent-time . 9600)))
    (license license:expat)))

(define-public zig-0.9
  (package
    (inherit zig-0.10)
    (name "zig")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziglang/zig.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfvgg23sw50ksy0z0ml6lkdsvmd0278mq29m23dbb2jsirkhry7"))
       (patches (search-patches "zig-use-system-paths.patch"
                                "zig-do-not-link-against-librt.patch"))))
    (inputs
     (list clang-13 ; Clang propagates llvm.
           lld-13))
    ;; Zig compiles fine with GCC, but also needs native LLVM libraries.
    (native-inputs
     (list llvm-13))
    (arguments
     `(#:configure-flags
       (list ,@(if (%current-target-system)
                   (string-append "-DZIG_TARGET_TRIPLE="
                                  (%current-target-system))
                   '()))
       #:out-of-source? #f ; for tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'set-cache-dir
           (lambda _
             ;; Set cache dir, otherwise Zig looks for `$HOME/.cache'.
             (setenv "ZIG_GLOBAL_CACHE_DIR"
                     (string-append (getcwd) "/zig-cache"))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (when tests?
               (invoke (string-append (assoc-ref outputs "out") "/bin/zig")
                       ;; Testing the standard library takes >7.5GB RAM, and
                       ;; will fail if it is OOM-killed.  The 'test-toolchain'
                       ;; target skips standard library and doc tests.
                       "build" "test-toolchain"
                       ;; Stage 2 is experimental, not what we run with `zig',
                       ;; and stage 2 tests require a lot of RAM.
                       "-Dskip-stage2-tests"
                       ;; Non-native tests try to link and execute non-native
                       ;; binaries.
                       "-Dskip-non-native")))))))))

(define-public zig zig-0.10)
