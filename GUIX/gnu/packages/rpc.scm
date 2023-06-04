;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages rpc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define-public grpc
  (package
    (name "grpc")
    (version "1.34.0")
    (outputs '("out" "static"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fs407hnlnm0b8sncjwys9rc7ia5nb7wxrpx39nq3pzzfs1lv3vq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:configure-flags
       (list "-DgRPC_ZLIB_PROVIDER=package"
             "-DgRPC_ABSL_PROVIDER=package"
             "-DgRPC_CARES_PROVIDER=package"
             "-DgRPC_SSL_PROVIDER=package"
             "-DgRPC_PROTOBUF_PROVIDER=package"
             "-DgRPC_RE2_PROVIDER=package"
             (string-append "-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "out"))
             "-DCMAKE_INSTALL_LIBDIR=lib"
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (assoc-ref %outputs "out") "/lib")
             "-DCMAKE_VERBOSE_MAKEFILE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-shared
           (lambda* (#:key (configure-flags '()) #:allow-other-keys)
             (mkdir "../build-shared")
             (with-directory-excursion "../build-shared"
               (apply invoke
                      "cmake" "../source"
                      "-DBUILD_SHARED_LIBS=ON"
                      configure-flags)
               (apply invoke "make"
                      `("-j" ,(number->string (parallel-job-count)))))))
         (add-after 'install 'install-shared-libraries
           (lambda _
             (with-directory-excursion "../build-shared"
               (invoke "make" "install"))))
         (add-before 'strip 'move-static-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (static (assoc-ref outputs "static")))
               (mkdir-p (string-append static "/lib"))
               (with-directory-excursion
                 (string-append out "/lib")
                 (for-each
                   (lambda (file)
                     (rename-file file
                                  (string-append static "/lib/" file)))
                   (find-files "." "\\.a$"))))
             #t)))))
    (inputs
     (list abseil-cpp-cxxstd11 c-ares/cmake openssl re2 zlib))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("protobuf" ,protobuf)
       ("python" ,python-wrapper)))
    (home-page "https://grpc.io")
    (synopsis "High performance universal RPC framework")
    (description "gRPC is a modern high performance @dfn{Remote Procedure Call}
(RPC) framework that can run in any environment.  It can efficiently connect
services in and across data centers with pluggable support for load balancing,
tracing, health checking and authentication.  It is also applicable in last
mile of distributed computing to connect devices, mobile applications and
browsers to backend services.")
    (license license:asl2.0)))

(define-public grpc-for-python-grpcio
  (package
    (inherit grpc)
    (name "grpc")
    (version "1.47.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc")
                    (commit (string-append "v" version))
                    (recursive? #true)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nl2d92f3576m69991d7gwyk1giavm04fagr612yjh90rni01ikw"))))
    (inputs
     (list abseil-cpp-20211102.0 c-ares/cmake openssl re2 zlib))))

;; Some packages require this older version.
(define-public grpc-1.16.1
  (package
    (inherit grpc)
    (version "1.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "grpc" version))
              (sha256
               (base32
                "1jimqz3115f9pli5w6ik9wi7mjc7ix6y7yrq4a1ab9fc3dalj7p2"))))
    (arguments
     (substitute-keyword-arguments (package-arguments grpc)
       ((#:phases phases)
        `(modify-phases ,phases
           ;; Note: This would be nicer as a snippet, but that creates a tarball
           ;; instead of a checkout and breaks assumptions made by the builder.
           (add-after 'unpack 'rename-gettid
             (lambda _
               ;; Rename custom gettid() syscall wrapper to avoid conflict
               ;; with gettid() from glibc 2.30.
               (substitute* '("src/core/lib/gpr/log_linux.cc"
                              "src/core/lib/gpr/log_posix.cc"
                              "src/core/lib/iomgr/ev_epollex_linux.cc")
                 (("gettid\\(")
                  "sys_gettid("))))))))
    (inputs
     (modify-inputs (package-inputs grpc)
       (replace "abseil-cpp" abseil-cpp-20200923.3)))
    (native-inputs
     (modify-inputs (package-native-inputs grpc)
       (replace "protobuf" protobuf-3.6)))))

(define-public python-grpc-stubs
  (package
    (name "python-grpc-stubs")
    (version "1.24.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "grpc-stubs" version))
              (sha256
               (base32
                "19dkm365g38lvxm799d29dnzg60g8in8251c18qkvsv4n92h8axh"))))
    (build-system python-build-system)
    (propagated-inputs (list python-grpcio python-typing-extensions))
    (home-page "https://github.com/shabbyrobe/grpc-stubs")
    (synopsis "gRPC typing stubs for Python")
    (description "This is a PEP-561-compliant stub-only package which provides
type information of gRPC.")
    (license license:expat)))

(define-public python-grpcio
  (package
    (name "python-grpcio")
    (version "1.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpcio" version))
       (sha256
        (base32
         "00gqhz0b1sqnfx6zy7h5z41b6mpsq57r1f3p95xradcvmdgskfsx"))
       (modules '((guix build utils) (ice-9 ftw)))
       (snippet
        '(begin
           ;; Delete this generated file.
           (delete-file "src/python/grpcio/grpc/_cython/cygrpc.cpp")
           (with-directory-excursion "third_party"
             ;; Delete the bundled source code of libraries that are possible
             ;; to provide as inputs.
             (for-each delete-file-recursively
                       (scandir "."
                                (lambda (file)
                                  (not (member file
                                               '("." ".."
                                                 "address_sorting"
                                                 "upb"
                                                 "xxhash")))))))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'use-system-libraries
            (lambda _
              (substitute* "setup.py"
                (("EXTENSION_INCLUDE_DIRECTORIES = \\(" m)
                 (string-append m " ('" #$(this-package-input "grpc")
                                "/include/grpc/impl/codegen/',) + ")))
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_CARES" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_OPENSSL" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_ZLIB" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_RE2" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_ABSL" "1")
              (setenv "GRPC_PYTHON_BUILD_WITH_CYTHON" "1")
              ;; Fix the linker options to link with abseil-cpp, which is
              ;; looked under /usr/lib.
              (substitute* "setup.py"
                (("pathlib.Path\\('/usr').glob\\('lib\\*/libabsl_\\*.so')")
                 (format #f "pathlib.Path('~a').glob('lib*/libabsl_*.so')"
                         #$(this-package-input "abseil-cpp"))))))
          (add-before 'build 'configure-compiler
            (lambda _
              (substitute* '("setup.py" "src/python/grpcio/commands.py")
                (("'cc'") "'gcc'")))))))
    (inputs
     (list abseil-cpp-20211102.0 c-ares grpc-for-python-grpcio openssl re2 zlib))
    (native-inputs
     (list python-cython))
    (propagated-inputs
     (list python-six))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework")
    (description "This package provides a Python library for communicating
with the HTTP/2-based RPC framework gRPC.")
    (license license:asl2.0)))

(define-public python-grpcio-tools
  (package
    (name "python-grpcio-tools")
    (version "1.47.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "grpcio-tools" version))
              (modules '((guix build utils)))
              (snippet
               ;; This file is auto-generated.
               '(delete-file "grpc_tools/_protoc_compiler.cpp"))
              (sha256
               (base32
                "0g3xwv55lvf5w64zb44dipwqz7729cbqc7rib77ddqab91w56jzn"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'configure
                          (lambda _
                            (setenv "GRPC_PYTHON_BUILD_WITH_CYTHON" "1"))))))
    (native-inputs (list python-cython))
    (propagated-inputs (list python-grpcio python-protobuf))
    (home-page "https://grpc.io")
    (synopsis "Protobuf code generator for gRPC")
    (description "The gRPC tools for Python provide a special plugin for
generating server and client code from @file{.proto} service definitions.")
    (license license:asl2.0)))

(define-public apache-thrift
  (package
    (name "apache-thrift")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wmnb3h0xq8qc5a9g9lliszh6qg254f5856h72viab46bizmdd4a"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           flex
           bison))
    (inputs
     (list boost libressl))
    (outputs '("out" "lib" "include"))
    (home-page "https://thrift.apache.org/")
    (synopsis
     "Lightweight, language-independent software stack for point-to-point
RPC")
    (description
     "Thrift provides clean abstractions and implementations for data
transport, data serialization, and application level processing.  The code
generation system takes a simple definition language as input and generates
code across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.")
    (license license:asl2.0)))
