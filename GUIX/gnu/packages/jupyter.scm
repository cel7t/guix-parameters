;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages jupyter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages version-control))

(define-public python-jupyter-protocol
  (package
    (name "python-jupyter-protocol")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_protocol" version))
              (sha256
               (base32
                "075vbaak6hlk9606lw61ldv72p6694k938jd1kvkm6spd0pczpmn"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-dateutil
           python-ipython-genutils
           python-jupyter-core
           python-pyzmq
           python-traitlets))
    (native-inputs
     (list python-ipykernel python-ipython python-mock python-pytest))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter protocol implementation")
    (description
     "This Python library is an experimental implementation of the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
protocol} to be used by both clients and kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_protocol")))))

(define-public python-jupyter-kernel-mgmt
  (package
    (name "python-jupyter-kernel-mgmt")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jupyter_kernel_mgmt" version))
              (sha256
               (base32
                "0977ixfi1pzjgy84hl0zycg4wpllmid98fhzcpy0lxd322w4sl7x"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; There are 8 test failures, most of them in 'test_client_loop.py'
      ;; (see: https://github.com/takluyver/jupyter_kernel_mgmt/issues/48).
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "pytest" "-vv" "jupyter_kernel_mgmt")))))))
    (propagated-inputs
     (list python-dateutil
           python-entrypoints
           python-jupyter-core
           python-jupyter-protocol
           python-pyzmq
           python-tornado
           python-traitlets))
    (native-inputs
     (list python-async-generator
           python-ipykernel
           python-ipython
           python-pytest
           python-pytest-asyncio))
    (home-page "https://jupyter.org")
    (synopsis "Discover, launch, and communicate with Jupyter kernels")
    (description
     "This package is an experimental refactoring of the machinery for
launching and using Jupyter kernels.")
    (license license:bsd-3)
    (properties '((upstream-name . "jupyter_kernel_mgmt")))))

(define-public python-jupyter-kernel-test
  (package
    (name "python-jupyter-kernel-test")
    (version "0.3")
    (home-page "https://github.com/jupyter/jupyter_kernel_test")
    (source (origin
              ;; PyPI has a ".whl" file but not a proper source release.
              ;; Thus, fetch code from Git.
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00iy74i4i8is6axb9vlsm0b9wxkvyyxnbl8r0i4gaj3xd788jm83"))))
    (build-system python-build-system)
    (arguments
     ;; The repo doesn't contain a "setup.py" file so install files manually.
     '(#:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (version (python-version (assoc-ref inputs "python")))
                             (pydir (string-append out "/lib/python"
                                                   version "/site-packages/"
                                                   "jupyter_kernel_test")))
                        (for-each (lambda (file)
                                    (install-file file pydir))
                                  (find-files "jupyter_kernel_test"
                                              "\\.py$"))
                        #t))))))
    (propagated-inputs
     (list python-jupyter-kernel-mgmt python-jupyter-protocol
           python-jsonschema))
    (synopsis "Test Jupyter kernels")
    (description
     "@code{jupyter_kernel_test} is a tool for testing Jupyter kernels.  It
tests kernels for successful code execution and conformance with the
@uref{https://jupyter-client.readthedocs.io/en/latest/messaging.html, Jupyter
Messaging Protocol}.")
    (license license:bsd-3)))

(define-public xeus
  (package
    (name "xeus")
    (version "2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyter-xeus/xeus")
                    (commit version)))
              (sha256
               (base32
                "1k1h416qkw3yra6ayfa61nv0v4ify2wvp5x27slgbcw6c88w7fb1"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_STATIC_LIBS=OFF"
                           "-DDISABLE_ARCH_NATIVE=ON" ;no '-march=native'
                           "-DBUILD_TESTING=ON")))
    (native-inputs
     (list pkg-config
           ;; The following inputs are used by the test suite.
           googletest
           python-pytest
           python-wrapper
           python-jupyter-kernel-test
           python-jupyter-client))
    (inputs
     (list xtl
           nlohmann-json
           cppzmq
           zeromq
           openssl
           `(,util-linux "lib")))       ;libuuid
    (home-page "https://quantstack.net/xeus")
    (synopsis "C++ implementation of the Jupyter Kernel protocol")
    (description
     "@code{xeus} is a library meant to facilitate the implementation of
kernels for Jupyter.  It takes the burden of implementing the Jupyter Kernel
protocol so developers can focus on implementing the interpreter part of the
kernel.

Several Jupyter kernels are built upon @code{xeus}, such as @code{xeus-cling},
a kernel for the C++ programming language, and @code{xeus-python}, an
alternative Python kernel for Jupyter.")
    (license license:bsd-3)))

(define-public python-jupyterlab-pygments
  (package
    (name "python-jupyterlab-pygments")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_pygments" version))
       (sha256
        (base32
         "0ij14mmnc39nmf84i0av6j9glazjic7wzv1qyhr0j5966s3s1kfg"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; there are no tests
    (propagated-inputs
     (list python-pygments))
    (home-page "https://jupyter.org")
    (synopsis "Pygments theme using JupyterLab CSS variables")
    (description
     "This package contains a syntax coloring theme for pygments making use of
the JupyterLab CSS variables.")
    (license license:bsd-3)))

(define-public python-jupyterlab-server
  (package
    (name "python-jupyterlab-server")
    (version "2.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_server" version))
       (sha256
        (base32 "1gxbfa5s0v4z0v8kagkm2bz8hlli5pwhr89y68w5kxcrqfsg9q00"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "-c" "/dev/null" "tests"
                        ;; XXX: These tests appear to fail due to the lack of
                        ;; locales.
                        "-k" "not locale and not language")))))))
    (propagated-inputs
     (list python-babel
           python-entrypoints
           python-importlib-metadata    ;TODO: remove after Python >= 3.10
           python-jinja2
           python-json5
           python-jsonschema
           python-jupyter-server
           python-packaging
           python-requests))
    (native-inputs
     (list python-ipykernel
           python-jupyter-server
           python-openapi-core
           python-openapi-spec-validator
           python-pytest
           python-pytest-console-scripts
           python-pytest-tornasync
           python-ruamel.yaml
           python-strict-rfc3339))
    (home-page "https://jupyter.org")
    (synopsis "Server components for JupyterLab applications")
    (description "JupyterLab Server sits between JupyterLab and Jupyter
Server, and provides a set of REST API handlers and utilities that are used by
JupyterLab.  It is a separate project in order to accommodate creating
JupyterLab-like applications from a more limited scope.")
    (license license:bsd-3)))

(define-public python-jupyter-packaging
  (package
    (name "python-jupyter-packaging")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_packaging" version))
       (sha256
        (base32
         "1b7ssc627vgrdl21c09w9sxk5fc1ps3g7f70laxag4yw1bb5ax5j"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Disable isolation so that the package environment can be
                ;; setup without connectivity.
                (setenv "SOURCE_DATE_EPOCH" "315532800")
                (substitute* "tests/test_build_api.py"
                  (("\"-m\", \"build\"" all)
                   (string-append all ", \"--no-isolation\"")))
                (invoke "python" "-m" "pytest" "-vv")))))))
    (propagated-inputs
     (list python-deprecation python-packaging python-setuptools
           python-tomlkit python-wheel))
    (native-inputs
     (list python-pypa-build python-coverage python-pytest
           python-pytest-cov python-pytest-mock))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter packaging utilities")
    (description "This package provides tools to help build and install
Jupyter Python packages that require a pre-build step that may include
JavaScript build steps.")
    (license license:bsd-3)))

(define-public python-jupyter-server
  (package
    (name "python-jupyter-server")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_server" version))
       (sha256
        (base32
         "0fj6l34m6vk3yic87isz9bzgg4qsbr285x1faamf512bsrxghmn7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (let ((home (string-append (getcwd) "/guix-home")))
                 (setenv "HOME" home))
               ;; Add jupyter-server executable to PATH.
               (setenv "PATH"
                       (string-append (assoc-ref outputs "out") "/bin:"
                                      (getenv "PATH")))
               (with-directory-excursion "jupyter_server"
                 ;; The pytest fixtures are only loaded when the file is
                 ;; called conftest.py.
                 (rename-file "pytest_plugin.py" "conftest.py")
                 (invoke "pytest" "-vv"
                         ;; Fails with internal server error
                         "-k" "not test_list_formats"
                         ;; Integration tests require a server.
                         "-m" "not integration_test"))))))))
    (propagated-inputs
     (list python-anyio
           python-argon2-cffi
           python-jinja2
           python-jupyter-client
           python-jupyter-core
           python-nbconvert
           python-nbformat
           python-prometheus-client
           python-pyzmq
           python-send2trash
           python-terminado
           python-tornado-6
           python-traitlets
           python-websocket-client))
    (native-inputs
     (list python-coverage
           python-ipykernel
           python-pytest
           python-pytest-console-scripts
           python-pytest-cov
           python-pytest-mock
           python-pytest-tornasync
           python-requests))
    (home-page "https://jupyter.org")
    (synopsis "Core services, APIs, and REST endpoints for Jupyter web applications")
    (description
     "This package provides the backend—i.e. core services, APIs, and REST
endpoints—to Jupyter web applications.")
    (license license:expat)))

(define-public python-jupyterlab-widgets
  (package
    (name "python-jupyterlab-widgets")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyterlab_widgets" version))
       (sha256
        (base32
         "0kdib439i9pbv90cscq5c7w4nvv8214k9ik4dnbd152yf897cvpa"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-jupyter-packaging python-setuptools))
    (home-page "https://github.com/jupyter-widgets/ipywidgets")
    (synopsis "Interactive widgets for Jupyter Notebooks")
    (description "ipywidgets, also known as jupyter-widgets or simply widgets,
are interactive HTML widgets for Jupyter notebooks and the IPython kernel.")
    (license license:bsd-3)))

(define-public python-jupyter-server-mathjax
  (package
    (name "python-jupyter-server-mathjax")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_server_mathjax" version))
       (sha256
        (base32 "1cz7grhj9jih9mgw4xk7a4bqy1fwlb1jsawh6ykxnvpydn76rnb4"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "/tmp"
                  (invoke "pytest" "-vv"
                          "--pyargs" "jupyter_server_mathjax"))))))))
    (propagated-inputs (list python-jupyter-server))
    (native-inputs
     (list python-jupyter-server
           python-jupyter-packaging
           python-pytest
           python-pytest-tornasync))
    (home-page "https://jupyter.org")
    (synopsis "Jupyter Server extension for serving Mathjax")
    (description "This package provides a Jupyter Server extension for serving
Mathjax, the JavaScript display engine for mathematics.")
    (license license:bsd-3)))

(define-public python-nbclient
  (package
    (name "python-nbclient")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbclient" version))
       (sha256
        (base32
         "0cbhs8l8ma5nzm1i4484gsrb7189m1lmniashp929pxsqq1s929z"))))
    (build-system python-build-system)
    ;; Tests require tools from nbconvert, which would introduces a cycle.
    (arguments '(#:tests? #false))
    (propagated-inputs
     (list python-jupyter-client
           python-nbformat
           python-nest-asyncio
           python-traitlets))
    (home-page "https://jupyter.org")
    (synopsis "Client library for executing notebooks")
    (description
     "This package provides a client library for executing notebooks.
It was formerly known as nbconvert's @code{ExecutePreprocessor.}")
    (license license:bsd-3)))

(define-public python-nbdime
  (package
    (name "python-nbdime")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbdime" version))
       (sha256
        (base32 "12v41lricbg713lzlfcx0cilfm9spndaanhp39q4ydvix4h76xk7"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-ipython-genutils
            ;; TODO: Remove when a release newer than 3.1.1 is made.
            (lambda _
              (substitute* "nbdime/config.py"
                (("from ipython_genutils import py3compat")
                 "")
                (("py3compat\\.getcwd")
                 "os.getcwd")))))))
    (propagated-inputs
     (list python-colorama
           python-gitpython
           python-jinja2
           python-jupyter-server
           python-jupyter-server-mathjax
           python-nbformat
           python-pygments
           python-requests
           python-tornado-6))
    (native-inputs
     (list python-jupyter-server
           python-mock
           python-notebook
           python-pytest
           python-pytest-tornado
           python-tabulate))
    (home-page "https://nbdime.readthedocs.io")
    (synopsis "Diff tools for Jupyter Notebooks")
    (description "@code{nbdime} provides tools for diffing and merging of
Jupyter Notebooks.  It includes the following commands:
@table @command
@item nbdiff compare notebooks in a terminal-friendly way
@item nbmerge three-way merge of notebooks with automatic conflict resolution
@item nbdiff-web rich rendered diff of notebooks
@item nbmerge-web web-based three-way merge tool for notebooks
nbshow present a single notebook in a terminal-friendly way
@end table")
    (license license:bsd-3)))

(define-public python-nbstripout
  (package
    (name "python-nbstripout")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nbstripout" version))
              (sha256
               (base32
                "1n57nvxsc94gz9w8ymi83bjkfhfwkpmx4y14m6gjrmlqd49m1aw6"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bad-tests
           (lambda _
             ;; These tests use git and hg, and they are sensitive to the
             ;; exact printed output.
             (for-each delete-file (list "tests/test-git.t"
                                         "tests/test-hg.t"
                                         "tests/test-status.t"
                                         "tests/test-uninstall.t"))))
         (add-before 'check 'set-CRAMSHELL
           (lambda _
             (setenv "CRAMSHELL" (which "bash")))))))
    (propagated-inputs (list python-nbformat))
    (native-inputs
     (list python-pytest
           python-pytest-cram
           python-pytest-flake8
           python-pytest-runner))
    (home-page "https://github.com/kynan/nbstripout")
    (synopsis "Strips outputs from Jupyter and IPython notebooks")
    (description
     "This package opens a notebook, strips its output, and writes the outputless
version to the original file.")
    (license license:expat)))

(define-public repo2docker
  (package
    (name "repo2docker")
    (version "2021.08.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyterhub/repo2docker/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "111irpghzys0s5ixs8paskz7465cls1sm9d5bg45a15jklcw84a9"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'patch-shebangs 'fix-install-miniforge
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (substitute* (find-files
                                      out "^(install-miniforge|install-nix|\
nix-shell-wrapper|repo2docker-entrypoint)")
                          (("^#!(.*)/bin/bash")
                           "#!/bin/bash"))
                        (substitute* (find-files out "^freeze\\.py$")
                          (("^#!(.*)/bin/python3")
                           "#!/bin/python3\n")))))
                  (add-after 'install 'make-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "doc"))
                             (doc (string-append out "/share/doc/"
                                                 ,(package-name this-package))))
                        (setenv "PYTHONPATH"
                                (string-append (getcwd) ":"
                                               (getenv "GUIX_PYTHONPATH")))
                        ;; Don't treat warnings as errors.
                        (substitute* "docs/Makefile"
                          (("(SPHINXOPTS[[:blank:]]+= )-W" _ group)
                           group))
                        (with-directory-excursion "docs"
                          (invoke  "make" "html")
                          (copy-recursively "build/html"
                                            (string-append doc "/html")))))))))
    (inputs
     (list python-traitlets
           python-toml
           python-semver
           python-ruamel.yaml
           python-requests
           python-json-logger
           python-jinja2
           python-iso8601
           python-escapism
           python-docker
           python-chardet))
    (native-inputs
     (list python-sphinx python-entrypoints python-recommonmark
           python-sphinxcontrib-autoprogram python-pydata-sphinx-theme))
    (home-page "https://repo2docker.readthedocs.io/en/latest/index.html#")
    (synopsis "Generate docker images from repositories")
    (description
     "repo2docker fetches a repository (from GitHub, GitLab, Zenodo, Figshare,
Dataverse installations, a Git repository or a local directory) and builds a
container image in which the code can be executed.  The image build process is
based on the configuration files found in the repository.  repo2docker can be
used to explore a repository locally by building and executing the constructed
image of the repository, or as a means of building images that are pushed to a
Docker registry.")
    (license license:bsd-3)))

(define-public python-bash-kernel
  (package
   (name "python-bash-kernel")
   (version "0.7.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "bash_kernel" version))
            (sha256
             (base32
              "0w0nbr3iqqsgpk83rgd0f5b02462bkyj2n0h6i9dwyc1vpnq9350"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'bash-references
          (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bash_kernel/kernel.py"
               (("\"bash\"")
                (string-append "\"" (assoc-ref inputs "bash") "/bin/bash\""))
               (("\\['bash', ")
                (string-append "['" (assoc-ref inputs "bash") "/bin/bash', ")))
             #t))
        (add-after 'install 'install-kernelspec
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (setenv "HOME" "/tmp")
              (invoke "python" "-m" "bash_kernel.install" "--prefix" out)
              #t))))))
   (inputs
     (list bash
           python-pexpect
           python-ipykernel
           python-jupyter-client))
   (home-page "https://github.com/takluyver/bash_kernel")
   (synopsis "Jupyter kernel for Bash")
   (description "A bash shell kernel for Jupyter.")
   (license license:expat)))

(define-public python-sparqlkernel
  (package
    (name "python-sparqlkernel")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlkernel" version))
              (sha256
               (base32
                "004v22nyi5cnpxq4fiws89p7i5wcnzv45n3n70axdd6prh6rkapx"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'no-custom-css
            (lambda _
              (substitute* "sparqlkernel/install.py"
                (("install_custom_css\\( destd, PKGNAME \\)") ""))))
          (add-after 'add-install-to-pythonpath 'install-kernelspec
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke
               (string-append #$output "/bin/jupyter-sparqlkernel")
               "install"
               (string-append "--InstallKernelSpec.prefix=" #$output)))))))
    (propagated-inputs
     (list python-ipykernel
           python-notebook
           python-pygments
           python-rdflib
           python-sparqlwrapper
           python-traitlets))
    (home-page "https://github.com/paulovn/sparql-kernel")
    (synopsis "Jupyter kernel for SPARQL")
    (description "This module installs a Jupyter kernel for SPARQL.  It allows
sending queries to an SPARQL endpoint, fetching and presenting the results in
a notebook.")
    (license license:bsd-3)))

(define-public python-ipympl
  (package
    (name "python-ipympl")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipympl" version))
       (sha256
        (base32 "11rppjdqzgs4pfiq8gww5xkpbk21fp86vvv839v56b9rqq06j2b4"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-ipython
           python-ipython-genutils
           python-ipywidgets
           python-matplotlib
           python-numpy
           python-pillow
           python-traitlets))
    (native-inputs
     (list python-jupyter-packaging))
    (home-page "https://matplotlib.org/ipympl/")
    (synopsis "Matplotlib Jupyter Extension")
    (description "Leveraging the Jupyter interactive widgets framework, ipympl
enables the interactive features of matplotlib in the Jupyter notebook and in
JupyterLab.")
    (license license:bsd-3)))

(define-public python-ipydatawidgets
  (package
    (name "python-ipydatawidgets")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipydatawidgets" version))
       (sha256
        (base32 "1g65nzlsb1cipmvh9v27b22kkmzwvg8zbf32hmg1c25mb65vbr6h"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v")))))))
    (propagated-inputs
     (list python-ipywidgets python-numpy python-six python-traittypes))
    (native-inputs
     (list python-jupyter-packaging
           python-nbval
           python-pytest
           python-pytest-cov))
    (home-page "https://github.com/vidartf/ipydatawidgets")
    (synopsis "Widgets to help facilitate reuse of large datasets across widgets")
    (description
     "This package provides a set of widgets to help facilitate reuse of large
datasets across widgets.")
    (license license:bsd-3)))

(define-public python-voila
  (package
    (name "python-voila")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/voila-dashboards/voila")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "10qn34ddmcwcl9zxa0gwxarxr64k8hx4yysdwrf0iqvmzmkwmbbj"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.cfg"
                (("nbclient>=0.4.0,<0.6")
                 "nbclient"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "pytest" "-vv"
                        ;; Many tests depend on Node JavaScript dependencies
                        ;; and a running HTTP server; ignore them.
                        "--ignore" "tests/app"
                        "--ignore" "tests/server")))))))
    (propagated-inputs
     (list python-jupyter-client
           python-jupyter-server
           python-jupyterlab-server
           python-nbclient
           python-nbconvert
           python-traitlets
           python-websockets))
    (native-inputs
     (list python-ipywidgets
           python-matplotlib
           python-mock
           python-numpy
           python-pandas
           python-pytest
           python-pytest-tornasync
           python-tornado-6))
    (home-page "https://github.com/voila-dashboards/voila")
    (synopsis "Render live Jupyter notebooks with interactive widgets")
    (description
     "Voilà turns Jupyter notebooks into standalone web applications.  Unlike
the usual HTML-converted notebooks, each user connecting to the Voilà tornado
application gets a dedicated Jupyter kernel which can execute the callbacks to
changes in Jupyter interactive widgets.")
    (license license:bsd-3)))
