;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2022 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2021-2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2022 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Tomasz Jeneralczyk <tj@schwi.pl>
;;; Copyright © 2022 jgart <jgart@dismail.de>
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

(define-module (gnu packages python-check)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages django)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-tappy
  (package
    (name "python-tappy")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tap.py" version))
       (sha256
        (base32
         "0w4w6pqjkv54j7rv6vdrpfxa72c5516bnlhpcqr3vrb4zpmyxvpm"))))
    (build-system python-build-system)
    (home-page "https://github.com/python-tap/tappy")
    (synopsis "Tools for Test Anything Protocol")
    (description "Tappy is a set of tools for working with the Test Anything
Protocol (TAP) in Python.  TAP is a line based test protocol for recording test
data in a standard way.")
    (license license:bsd-3)))

(define-public python-beartype
  (package
    (name "python-beartype")
    (version "0.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beartype" version))
       (sha256
        (base32 "0amzckgw9c93bl4jf0q6322j9wyyf3i8vl03yixfkrpllzv6kv14"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "pytest" "-vv" "beartype_test"
                             ;; These tests rely on git through the
                             ;; "get_main_readme_file" helper.
                             "-k"
                             (string-append "not test_doc_readme "
                                            "and not test_sphinx "
                                            "and not test_pep561_mypy"))))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/beartype/beartype")
    (synopsis "Fast runtime type checking for Python")
    (description "Beartype aims to be a very fast runtime type checking tool
written in pure Python.")
    (license license:expat)))

(define-public python-pytest-click
  (package
    (name "python-pytest-click")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (pypi-uri "pytest_click" version))
       (sha256
        (base32 "1rcv4m850rl7djzdgzz2zhjd8g5ih8w6l0sj2f9hsynymlsq82xl"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-click python-pytest))
    (home-page "https://github.com/Stranger6667/pytest-click")
    (synopsis "Py.test plugin for Click")
    (description "This package provides a plugin to test Python click
interfaces with pytest.")
    (license license:expat)))

(define-public python-pytest-cram
  (package
    (name "python-pytest-cram")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-cram" version))
              (sha256
               (base32
                "0405ymmrsv6ii2qhq35nxfjkb402sdb6d13xnk53jql3ybgmiqq0"))))
    (build-system python-build-system)
    (propagated-inputs (list python-cram python-pytest))
    (home-page "https://github.com/tbekolay/pytest-cram")
    (synopsis "Run cram tests with pytest")
    (description "Cram tests command line applications; Pytest tests Python
applications.  @code{pytest-cram} tests Python command line applications by
letting you write your Python API tests with pytest, and your command line
tests in cram.")
    (license license:expat)))

(define-public python-pytest-csv
  (package
    (name "python-pytest-csv")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)               ;no tests in PyPI archive
       (uri (git-reference
             (url "https://github.com/nicoulaj/pytest-csv")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17518f2fn5l98lyk9p8r7215c1whi61imzrh6ahrmcksr8w0zz04"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest")))))))
    (native-inputs
     (list python-pytest-flake8 python-pytest-xdist python-tabulate))
    (propagated-inputs
     (list python-pytest python-six))
    (home-page "https://github.com/nicoulaj/pytest-csv")
    (synopsis "CSV reporter for Pytest")
    (description "This package provides a plugin for Pytest that enables a
CSV output mode for Pytest.  It can be enabled via the @option{--csv} option
it adds to the Pytest command line interface (CLI).")
    (license license:gpl3+)))

(define-public python-testfixtures
  (package
    (name "python-testfixtures")
    (version "6.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testfixtures" version))
       (sha256
        (base32 "1nlv2hz20czjp4a811ichl5kwg99rh84l0mw9wq4rk3idzfs1hsy"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))          ; PyTest-Django fails to build in master
    (native-inputs
     (list python-pytest python-pytest-cov
           ;;("python-pytest-django" ,python-pytest-django)
           python-twine python-wheel))
    (synopsis "Tests components for Python")
    (description "Testfixtures is a collection of helpers and mock objects that
are useful when writing automated tests in Python.")
    (home-page "https://testfixtures.readthedocs.io/en/latest/")
    (license license:expat)))

(define-public python-cucumber-tag-expressions
  (package
    (name "python-cucumber-tag-expressions")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cucumber-tag-expressions" version))
       (sha256
        (base32 "0q7rn4l4ppjd1zsglr37ccc5xccg4iigaw827282zfzfsvzda573"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Ignore the configuration file since we don't
                     ;; need HTML reports, etc.
                     (invoke "pytest" "-c" "/dev/null" "-vv")))))))
    (native-inputs
     (list python-invoke python-pathpy python-pytest))
    (home-page "https://github.com/cucumber/tag-expressions-python")
    (synopsis "Tag-expression parser for cucumber/behave")
    (description
     "This package provides a tag-expression parser for Cucumber and
@command{behave}.")
    (license license:expat)))

(define-public python-coveralls
  (package
    (name "python-coveralls")
    (version "3.2.0")
    (home-page "https://github.com/coveralls-clients/coveralls-python")
    (source
     (origin
       ;; The PyPI release lacks tests, so we pull from git instead.
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1915ab77nfb1rfw4i2ps0zy19wpf20lwxn81qxxbwyd2gy7m0fn8"))
       (modules '((guix build utils)))
       (snippet '(substitute* "setup.py"
                  (("'coverage>=4.1,<6.0',") "'coverage',")))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-git-test
                    (lambda _
                      ;; Remove test that requires 'git' and the full checkout.
                      (delete-file "tests/git_test.py")))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          ;; Test fails for unknown reasons. No fix available.
                          (invoke "pytest" "-vv" "-k" "not test_reporter_with_branches")
                          (format #t "test suite not run~%")))))))
    (propagated-inputs
     (list python-coverage python-docopt python-pyyaml python-requests))
    (native-inputs
     (list python-mock python-pytest python-responses))
    (synopsis "Show coverage stats online via coveralls.io")
    (description
     "Coveralls.io is a service for publishing code coverage statistics online.
This package provides seamless integration with coverage.py (and thus pytest,
nosetests, etc...) in Python projects.")
    (license license:expat)))

(define-public python-junit-xml
  ;; XXX: There are no tags or PyPI releases, so take the latest commit
  ;; and use the version defined in setup.py.
  (let ((version "1.9")
        (commit "4bd08a272f059998cedf9b7779f944d49eba13a6")
        (revision "0"))
    (package
      (name "python-junit-xml")
      (version (git-version version revision commit))
      (home-page "https://github.com/kyrus/python-junit-xml")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b8kbjhk3j10rk0vcniy695m3h43yip6y93h1bd6jjh0cp7s09c7"))))
      (build-system python-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda _
                        (invoke "pytest" "-vv"))))))
      (native-inputs
       (list python-pytest))
      (propagated-inputs
       (list python-six))
      (synopsis "Create JUnit XML test results")
      (description
       "This package provides a Python module for creating JUnit XML test
result documents that can be read by tools such as Jenkins or Bamboo.")
      (license license:expat))))

(define-public python-pyinstrument
  (package
    (name "python-pyinstrument")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyinstrument" version))
       (sha256
        (base32 "18n3waxsxcd48pmcp8158s5rlancll2000amrdck9zfj5hfpkhhx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flaky
           python-pytest
           python-pytest-asyncio
           python-pytest-trio))
    (arguments
     `(;; TODO: Get tests to work.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest" "-vv")))))))
    (home-page "https://github.com/joerick/pyinstrument")
    (synopsis "Call stack profiler for Python")
    (description
     "Pyinstrument is a Python profiler to help you optimize your code.")
    (license license:bsd-3)))

(define-public python-vcrpy
  (package
    (name "python-vcrpy")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vcrpy" version))
       (sha256
        (base32 "16gmzxs3lzbgf1828n0q61vbmwyhpvzdlk37x6gdk8n05zr5n2ap"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? outputs #:allow-other-keys)
             (when tests?
               (substitute* "tox.ini"
                 (("AWS_ACCESS_KEY_ID") "PYTHONPATH"))
               ;; These tests require network access.
               (delete-file "tests/unit/test_stubs.py")
               (invoke "pytest" "tests/unit")))))))
    (propagated-inputs
     (list python-pyyaml python-six python-wrapt python-yarl))
    (native-inputs
     (list python-black
           python-coverage
           python-flake8
           python-flask
           python-httplib2
           python-ipaddress
           python-mock
           python-pytest
           python-pytest-cov
           python-pytest-httpbin
           python-tox
           python-urllib3))
    (home-page "https://github.com/kevin1024/vcrpy")
    (synopsis "Automatically mock your HTTP interactions")
    (description
     "VCR.py simplifies and speeds up tests that make HTTP requests.  The first
time you run code that is inside a VCR.py context manager or decorated function,
VCR.py records all HTTP interactions that take place through the libraries it
supports and serializes and writes them to a flat file (in yaml format by
default).  This flat file is called a cassette.  When the relevant piece of code
is executed again, VCR.py will read the serialized requests and responses from
the aforementioned cassette file, and intercept any HTTP requests that it
recognizes from the original test run and return the responses that corresponded
to those requests.  This means that the requests will not actually result in
HTTP traffic, which confers several benefits including:
@enumerate
@item The ability to work offline
@item Completely deterministic tests
@item Increased test execution speed
@end enumerate
If the server you are testing against ever changes its API, all you need to do
is delete your existing cassette files, and run your tests again.  VCR.py will
detect the absence of a cassette file and once again record all HTTP
interactions, which will update them to correspond to the new API.")
    (license license:expat)))

(define-public python-pytest-socket
  (package
    (name "python-pytest-socket")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-socket" version))
              (sha256
               (base32
                "1dkr86nxkxc0ka3rdnpmk335m8gl1zh1sy8i7w4w1jsidbf82jvw"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests fail a lot, probably requiring Internet access.
     (list #:tests? #f
           #:phases #~(modify-phases %standard-phases
                        (replace 'build
                          (lambda _
                            (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                                    #$version)
                            (setenv "SOURCE_DATE_EPOCH" "315532800")
                            (invoke "python"
                                    "-m"
                                    "build"
                                    "--wheel"
                                    "--no-isolation"
                                    ".")))
                        (add-before 'check 'disable-unsupported-test
                          (lambda _
                            (substitute* "tests/test_async.py"
                              (("def test_asynctest")
                               "def __off_test_asynctest"))))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "python" "-m" "pytest" "-vvv")))))))
    (native-inputs (list python-httpx
                         python-poetry-core
                         python-pypa-build
                         python-pytest
                         python-pytest-httpbin
                         python-pytest-randomly
                         python-starlette))
    (home-page "https://pypi.org/project/pytest-socket/")
    (synopsis "Pytest plugin to disable socket calls during tests")
    (description
     "This package provides Pytest extension which disables all network calls flowing
through Python's socket interface")
    (license license:expat)))

(define-public python-pytest-ordering
  (package
    (name "python-pytest-ordering")
    (version "0.6")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ftobia/pytest-ordering")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14msj5gyqza0gk3x7h1ivmjrwza82v84cj7jx3ks0fw9lpin7pjq"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "-k"
                     ;; This test fails because of a type mismatch of an
                     ;; argument passed to @code{pytest.main}.
                     "not test_run_marker_registered"))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/ftobia/pytest-ordering")
    (synopsis "Pytest plugin to run your tests in a specific order")
    (description
     "This plugin defines Pytest markers to ensure that some tests, or groups
of tests run in a specific order.")
    (license license:expat)))

(define-public python-pytest-astropy-header
(package
  (name "python-pytest-astropy-header")
  (version "0.2.2")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pytest-astropy-header" version))
      (sha256
        (base32 "046v4arinv8b5jz05pvhnc0n1aqqndwvhlsl635ahxabr40i32bp"))))
  (build-system python-build-system)
  (native-inputs
   (list python-pytest python-setuptools-scm))
  (home-page "https://www.astropy.org/")
  (synopsis
   "Pytest plugin adding diagnostic data to the header of the test output")
  (description
    "This plugin package provides a way to include information about the system,
Python installation, and select dependencies in the header of the output when
running pytest.  It can be used with packages that are not affiliated with the
Astropy project, but is optimized for use with astropy-related projects.")
  (license license:bsd-3)))

(define-public python-pytest-astropy
  (package
    (name "python-pytest-astropy")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-astropy" version))
       (sha256
        (base32 "04g2rh261s3s6ym8mwi4iv2a6anbgwvwzcvkyilfck6yxrncdqw5"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f)) ; there are no tests
    (native-inputs
     (list python-attrs python-pytest-mock python-setuptools-scm))
    (propagated-inputs
     (list python-hypothesis
           python-pytest-arraydiff
           python-pytest-astropy-header
           python-pytest-cov
           python-pytest-doctestplus
           python-pytest-filter-subpackage
           python-pytest-openfiles
           python-pytest-remotedata))
    (home-page "https://github.com/astropy/pytest-astropy")
    (synopsis
     "Metapackage for all the testing machinery used by the Astropy Project")
    (description
     "This is a meta-package that pulls in the dependencies that are used by
astropy related packages.")
    (license license:bsd-3)))

(define-public python-pytest-arraydiff
  (package
    (name "python-pytest-arraydiff")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-arraydiff" version))
       (sha256
        (base32 "1livzfbi7ag17hskd5845dh1kdir24f7jrbw8y2s1pyhzyz4jhbi"))))
    (build-system python-build-system)
    (arguments
     ;; Tests require python-astropy, which itself requires this package.
     ;; Disable tests to avoid the circular dependency problem.
     '(#:tests? #f))
    (native-inputs
     (list python-pytest python-setuptools-scm)) ; for sanity-check
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/astropy/pytest-arraydiff")
    (synopsis "Pytest plugin to help with comparing array output from tests")
    (description
     "This is a py.test plugin to facilitate the generation and comparison of
data arrays produced during tests, in particular in cases where the arrays
are too large to conveniently hard-code them in the tests.")
    (license license:bsd-3)))

(define-public python-pytest-doctestplus
  (package
    (name "python-pytest-doctestplus")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-doctestplus" version))
       (sha256
        (base32 "10ciqylgziihxwxryxvxgmkqgws51pqcarn0gbh1d4cxx55rx5vs"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           #~(list "-k" (string-append
                         "not test_remote_data_url"
                         " and not test_remote_data_float_cmp"
                         " and not test_remote_data_ignore_whitespace"
                         " and not test_remote_data_ellipsis"
                         " and not test_remote_data_requires"
                         " and not test_remote_data_ignore_warnings"))))
    (native-inputs
     (list python-numpy python-pytest python-setuptools-scm))
    (home-page "https://github.com/astropy/pytest-doctestplus")
    (synopsis "Pytest plugin with advanced doctest features")
    (description
     "This package contains a plugin for the Pytest framework that provides
advanced doctest support and enables the testing of reStructuredText files.")
    (license license:bsd-3)))

(define-public python-pytest-exploratory
  (package
    (name "python-pytest-exploratory")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_exploratory" version))
       (sha256
        (base32 "159rcqv6wrdqdlag1gz39n6fk58232hbxshan043ljgpp1qfs6xk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "tests")))))))
    (propagated-inputs
     (list python-ipython python-py python-pytest))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/nokia/pytest-exploratory")
    (synopsis "Interactive console for Pytest")
    (description "This Pytest plugin provides an IPython extension that allows
for interactively selecting and running Pytest tests.")
    (license license:expat)))

(define-public python-pytest-filter-subpackage
  (package
    (name "python-pytest-filter-subpackage")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-filter-subpackage" version))
       (sha256
        (base32 "10hpl3f7g2bm29lakmp8492b7lr0dp90khfni12m4gl02xks7bhz"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-pytest-doctestplus
           python-setuptools-scm))
    (home-page "https://github.com/astropy/pytest-filter-subpackage")
    (synopsis "Pytest plugin for filtering based on sub-packages")
    (description
     "This package contains a simple plugin for the pytest framework that
provides a shortcut to testing all code and documentation for a given
sub-package.")
    (license license:bsd-3)))

(define-public python-pytest-helpers-namespace
  (package
    (name "python-pytest-helpers-namespace")
    (version "2021.3.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-helpers-namespace" version))
       (sha256
        (base32
         "0pyj2d45zagmzlajzqdnkw5yz8k49pkihbydsqkzm413qnkzb38q"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-pytest python-setuptools ; needs setuptools >= 50.3.2
           python-setuptools-scm python-setuptools-declarative-requirements))
    (home-page "https://github.com/saltstack/pytest-helpers-namespace")
    (synopsis "Pytest Helpers Namespace Plugin")
    (description "Pytest Helpers Namespace Plugin provides a helpers pytest
namespace which can be used to register helper functions without requiring
someone to import them in their actual tests to use them.")
    (license license:asl2.0)))

(define-public python-pytest-metadata
  (package
    (name "python-pytest-metadata")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-metadata" version))
       (sha256
        (base32 "1wgcz0w053lnjln0081kjmfflaq7bwncxdzx7k63kr9lkpa0ddbi"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-metadata")
    (synopsis "Access test session metadata with Pytest")
    (description
     "@code{pytest-metadata} is a @command{pytest} plugin that provides
access to test session metadata.")
    (license license:mpl2.0)))

(define-public python-pytest-openfiles
  (package
    (name "python-pytest-openfiles")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-openfiles" version))
       (sha256
        (base32 "0n0a7fdc9m86360y96l23fvdmd6rw04bl6h5xqgl9qxfv08jk70p"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-setuptools-scm python-pytest))
    (propagated-inputs
     (list python-psutil))
    (home-page "https://github.com/astropy/pytest-openfiles")
    (synopsis "Pytest plugin for detecting inadvertent open file handles")
    (description
     "This package provides a plugin for the pytest framework that allows
developers to detect whether any file handles or other file-like objects
were inadvertently left open at the end of a unit test.")
    (license license:bsd-3)))

(define-public python-pytest-remotedata
  (package
    (name "python-pytest-remotedata")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-remotedata" version))
       (sha256
        (base32 "1j5106j331cfdyfcwzrbs3yby84mq1b0kddfysq12z2dwdcca8dy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-k" (string-append
                                 "not test_default_behavior"
                                 " and not test_strict_with_decorator"))))
    (native-inputs
     (list python-pytest python-setuptools-scm))
    (propagated-inputs
     (list python-packaging))
    (home-page "https://github.com/astropy/pytest-remotedata")
    (synopsis "Pytest plugin for controlling remote data access")
    (description
     "This package provides a plugin for the Pytest framework that allows
developers to control unit tests that require access to data from the
internet.")
    (license license:bsd-3)))

(define-public python-pytest-repeat
  (package
    (name "python-pytest-repeat")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-repeat" version))
       (sha256
        (base32 "0nxdbghjz6v4xidl5ky9wlx6z4has3vygj5r7va5ccdb8nbjilsw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (propagated-inputs
     (list python-pytest))
    (native-inputs
     (list python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-repeat")
    (synopsis "Pytest plugin for repeating tests")
    (description "@code{pytest-repeat} is a plugin for Pytest that makes it
enables repeating a single test, or multiple tests, a specific number of
times.")
    (license license:mpl2.0)))

(define-public python-pytest-mockito
  (package
    (name "python-pytest-mockito")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/kaste/pytest-mockito")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hnpazaw3mglx1c405z2hkavgan99rqb3wgrcqk8x5kmhpay53xx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "-vv")))))))
    (propagated-inputs
     (list python-mockito python-pytest))
    (home-page "https://github.com/kaste/pytest-mockito")
    (synopsis "Mockito base fixtures for Pytest")
    (description "The @code{pytest-mockito} plugin provides base Mockito
fixtures for Pytest.  It covers the main entry points of the Mockito mocking
framework and makes it easy to undo any monkey patching.  The fixtures are:
@itemize
@item when
@item when2
@item expect
@item patch
@item unstub
@item spy2
@end itemize")
    (license license:expat)))

(define-public python-pytest-mpl
  (package
    (name "python-pytest-mpl")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mpl" version))
       (sha256
        (base32 "0sa4229xkkah3fdg9wnqnvb9j13xsd3x1h5rwbsgb3sf2a0icmrd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-m" "mpl_image_compare")))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-jinja2
           python-matplotlib
           python-nose
           python-packaging
           python-pillow))
    (home-page "https://github.com/matplotlib/pytest-mpl")
    (synopsis "Pytest plugin to help with testing figures output from Matplotlib")
    (description
     "This is a plugin to facilitate image comparison for Matplotlib figures
in Pytest.")
    (license license:bsd-3)))

(define-public python-pytest-pydocstyle
  (package
    (name "python-pytest-pydocstyle")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/henry0312/pytest-pydocstyle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w6fivz4nb4b70wzmi5sk17qs9pd05rnh03fmch6v00r3dmfpk39"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; test requires the package itself
    (propagated-inputs
     (list python-pydocstyle
           python-pytest))              ;apparently required
    (home-page "https://github.com/henry0312/pytest-pydocstyle")
    (synopsis "Pytest plugin to run @command{pydocstyle}")
    (description "This package provides a Pytest plugin to run
@command{pydocstyle}.")
    (license license:expat)))

(define-public python-covdefaults
  (package
    (name "python-covdefaults")
    (version "1.1.0")
    (source
     (origin
       ;; The PyPI tarball does not include tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/covdefaults")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11a24c0wzv01n55fy4kdpnyqna4m9k0mp58kmhiaks34xw4k37hq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-coverage python-pytest))
    (home-page "https://github.com/asottile/covdefaults")
    (synopsis "Coverage plugin to provide opinionated default settings")
    (description
     "Covdefaults is a coverage plugin to provide opinionated default
 settings.")
    (license license:expat)))

(define-public python-pytest-subtests
  (package
    (name "python-pytest-subtests")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-subtests" version))
       (sha256
        (base32 "05zvnxx0hdrd9w4z51qhchg3nkz5s47agryw68g8q7krq5kim5nr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs (list python-pytest python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-subtests")
    (synopsis "Unittest subTest() support and subtests fixture")
    (description "This Pytest plugin provides unittest @code{subTest()}
support and @code{subtests} fixture.")
    (license license:expat)))

(define-public python-pytest-vcr
  ;; This commit fixes integration with pytest-5
  (let ((commit "4d6c7b3e379a6a7cba0b8f9d20b704dc976e9f05")
        (revision "1"))
    (package
      (name "python-pytest-vcr")
      (version (git-version "1.0.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ktosiek/pytest-vcr")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1yk988zi0la6zpcm3fff0mxf942di2jiymrfqas19nyngj5ygaqs"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests/"))))))
      (propagated-inputs
       (list python-pytest python-vcrpy))
      (home-page "https://github.com/ktosiek/pytest-vcr")
      (synopsis "Plugin for managing VCR.py cassettes")
      (description
       "Plugin for managing VCR.py cassettes.")
      (license license:expat))))

(define-public python-pytest-doctest-custom
  (package
    (name "python-pytest-doctest-custom")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-doctest-custom" version))
       (sha256
        (base32 "0kxkdd6q9c3h31kc88lbyfll4c45b0zjd24cbr4c083fcvcy7lip"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "test_pytest_doctest_custom.py")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/danilobellini/pytest-doctest-custom")
    (synopsis
     "Pytest plugin to customize string representations of doctest results")
    (description "This package provides a Pytest plugin for customizing string
representations of doctest results.  It can change the display hook used by
doctest to render the object representations.")
    (license license:expat)))

(define-public python-pytest-checkdocs
  (package
    (name "python-pytest-checkdocs")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-checkdocs" version))
       (sha256
        (base32 "1bn1wr3yz8avkwacffyh26za7mg20f9pajpakfk4cn7yvmgbhcrb"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))      ;no tests in pypi archive
    (propagated-inputs
     (list python-docutils
           python-importlib-metadata
           python-pep517
           python-pytest))
    (native-inputs (list python-setuptools-scm))
    (home-page "https://github.com/jaraco/pytest-checkdocs")
    (synopsis "Check the README when running tests")
    (description
     "This package provides a pytest plugin that checks the long description
of the project to ensure it renders properly.")
    (license license:expat)))

(define-public python-re-assert
  (package
    (name "python-re-assert")
    (version "1.1.0")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/re-assert")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rssq4wpqmx1c17hjfx5l3sn3zmnlz9jffddiqrs4f6h7m6cadai"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-covdefaults python-coverage python-pytest))
    (propagated-inputs
     (list python-regex))
    (home-page "https://github.com/asottile/re-assert")
    (synopsis "Show where your regex match assertion failed")
    (description
     "@code{re-assert} provides a helper class to make assertions of regexes
simpler.")
    (license license:expat)))

(define-public python-pytest-trio
  (package
    (name "python-pytest-trio")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-trio" version))
       (sha256
        (base32 "0c8cqf9by2884riksrqymqfp2g1d2d798a2zalcw9hmf34c786y0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-W" "error" "-ra" "-v" "--pyargs"
                       "pytest_trio" "--verbose" "--cov" "-k"
                       (string-append
                         ;; Needs network
                         "not test_async_yield_fixture_with_nursery"
                         " and not test_try"
                         ;; No keyboard interrupt in our build environment.
                         " and not test_actual_test"))))))))
    (native-inputs
     (list python-hypothesis python-pytest python-pytest-cov))
    (propagated-inputs
      (list python-async-generator python-outcome python-pytest python-trio))
    (home-page "https://github.com/python-trio/pytest-trio")
    (synopsis "Pytest plugin for trio")
    (description
     "This is a pytest plugin to help you test projects that use Trio, a
friendly library for concurrency and async I/O in Python.")
    ;; Either license applies.
    (license (list license:expat license:asl2.0))))

(define-public python-pytest-flake8
  (package
    (name "python-pytest-flake8")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flake8" version))
       (sha256
        (base32
         "0syx68xk5ss3hgp3nr2y122w0fgkzr5936ghsqrkymh3m5hrf9gh"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flake8))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/tholo/pytest-flake8")
    (synopsis "Pytest plugin to check FLAKE8 requirements")
    (description
     "This package provides a pytest plugin for efficiently checking PEP8
compliance.")
    (license license:bsd-3)))

(define-public python-pytest-isort
  (package
    (name "python-pytest-isort")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest_isort" version))
       (sha256
        (base32 "0v0qa5l22y3v0nfkpvghbinzyj2rh4f54k871lrp992lbvf02y06"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; No tests in PyPi tarball.
    (propagated-inputs
     (list python-isort python-pytest))
    (home-page "https://github.com/moccu/pytest-isort/")
    (synopsis "Pytest plugin to check import ordering using isort")
    (description
     "This package provides a pytest plugin to check import ordering using
isort.")
    (license license:bsd-3)))

(define-public python-pytest-shutil
  (package
    (name "python-pytest-shutil")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-shutil" version))
       (sha256
        (base32
         "0q8j0ayzmnvlraml6i977ybdq4xi096djhf30n2m1rvnvrhm45nq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-path-instead-of-path.py
           ;; path.py is obsolete.
           (lambda _
             (substitute* "setup.py"
               (("'path.py'")
                "'path'"))))
         (add-after 'unpack 'patch-tests
           (lambda _
             (mkdir "/tmp/bin")
             (substitute* "tests/integration/test_cmdline_integration.py"
               (("dirname = '/bin'")
                "dirname = '/tmp/bin'")
               (("bindir = os.path.realpath\\('/bin'\\)")
                "bindir = os.path.realpath('/tmp/bin')")))))))
    (propagated-inputs
     (list python-contextlib2 python-execnet python-path python-termcolor))
    (native-inputs
     (list python-mock python-pytest python-setuptools-git))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Assorted shell and environment tools for py.test")
    (description
     "This package provides assorted shell and environment tools for the
py.test testing framework.")
    (license license:expat)))

(define-public python-pytest-fixture-config
  (package
    (name "python-pytest-fixture-config")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-fixture-config" version))
       (sha256
        (base32
         "13i1qpz22w3x4dmw8vih5jdnbqfqvl7jiqs0dg764s0zf8bp98a1"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-setuptools-git))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Fixture configuration utils for py.test")
    (description
     "This package provides fixture configuration utilities for the py.test
testing framework.")
    (license license:expat)))

(define-public python-pytest-virtualenv
  (package
    (name "python-pytest-virtualenv")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-virtualenv" version))
       (sha256
        (base32
         "03w2zz3crblj1p6i8nq17946hbn3zqp9z7cfnifw47hi4a4fww12"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Reference the virtualenv executable directly, to avoid the need
         ;; for PYTHONPATH, which gets cleared when instantiating a new
         ;; virtualenv with pytest-virtualenv.
         (add-after 'unpack 'patch-virtualenv-executable
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((virtualenv (assoc-ref inputs "python-virtualenv"))
                    (virtualenv-bin (string-append virtualenv
                                                   "/bin/virtualenv")))
               (substitute* "pytest_virtualenv.py"
                 (("^DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE.*$")
                  (format #f "DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE = '~a'"
                          virtualenv-bin)))
               #t))))))
    (propagated-inputs
     (list python-pytest-shutil python-pytest-fixture-config))
    (inputs
     (list python-virtualenv))
    (native-inputs
     (list python-mock python-pytest python-setuptools-git))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Virtualenv fixture for py.test")
    (description "This package provides a virtualenv fixture for the py.test
framework.")
    (license license:expat)))

(define-public python-pytest-pycodestyle
  (package
    (name "python-pytest-pycodestyle")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-pycodestyle" version))
       (sha256
        (base32
         "1clyjypn93hwvz17f4i6n2688835d4y8qsq2aw17d6fkbqiy8mg7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pycodestyle))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/henry0312/pytest-pycodestyle")
    (synopsis "Pytest plugin to run pycodestyle")
    (description "This package provides a plugin to run @code{pycodestyle}
for the @code{pytest} framework.")
    (license license:expat)))

(define-public python-pytest-benchmark
  (package
    (name "python-pytest-benchmark")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-benchmark" version))
       (sha256
        (base32
         "1la802m5r49y1zqilmhqh0qvbnz139lw0qb3jmm9lngy7sw8a1zv"))))
    (build-system python-build-system)
    (arguments
     '(#:test-target "check"))
    (propagated-inputs
     (list python-py-cpuinfo))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/ionelmc/pytest-benchmark")
    (synopsis "Pytest fixture for benchmarking code")
    (description
     "This package provides a pytest fixture that will group the tests into
rounds that are calibrated to the chosen timer.")
    (license license:bsd-2)))

(define-public python-pytest-xvfb
  (package
    (name "python-pytest-xvfb")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xvfb" version))
       (sha256
        (base32 "1kyq5rg27dsnj7dc6x9y7r8vwf8rc88y2ppnnw6r96alw0nn9fn4"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-tests
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")

             ;; This test is meant to run on Windows.
             (delete-file "tests/test_xvfb_windows.py")
             #t)))))
    (native-inputs
     (list python-pytest python-pytest-runner xorg-server-for-tests))
    (propagated-inputs
     (list python-pyvirtualdisplay))
    (home-page "https://github.com/The-Compiler/pytest-xvfb")
    (synopsis "Pytest plugin to run Xvfb for tests")
    (description
     "This package provides a Pytest plugin to run Xvfb for tests.")
    (license license:expat)))

(define-public python-pytest-services
  (package
    (name "python-pytest-services")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-services" version))
        (sha256
         (base32
          "0b2zfv04w6m3gp2v44ifdhx22vcji069qnn95ry3zcyxib7cjnq3"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests not included in release tarball.
    (propagated-inputs
     (list python-psutil python-requests))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-services")
    (synopsis "Services plugin for pytest testing framework")
    (description
     "This plugin provides a set of fixtures and utility functions to start
service processes for your tests with pytest.")
    (license license:expat)))

(define-public python-pytest-toolbox
  (package
    (name "python-pytest-toolbox")
    (version "0.4")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/samuelcolvin/pytest-toolbox")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wqkr3g5gmqdxmhzfsxbwy8pm3cadaj6a8cxq58w9bacly4hqbh0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (substitute* "setup.cfg"
                 ((".*timeout.*") ""))
               ;; Make the installed plugin discoverable by Pytest.
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pydantic python-pytest python-pytest-isort))
    (home-page "https://github.com/samuelcolvin/pytest-toolbox")
    (synopsis "Numerous useful plugins for Pytest")
    (description
     "Pytest Toolbox contains many useful plugins for Pytest.  Among them are
new fixtures, new methods and new comparison objects.")
    (license license:expat)))

(define-public python-pytest-aiohttp
  (package
    (name "python-pytest-aiohttp")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-aiohttp" version))
       (sha256
        (base32
         "0kx4mbs9bflycd8x9af0idcjhdgnzri3nw1qb0vpfyb3751qaaf9"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-aiohttp))
    (home-page "https://github.com/aio-libs/pytest-aiohttp/")
    (synopsis "Pytest plugin for aiohttp support")
    (description "This package provides a pytest plugin for aiohttp support.")
    (license license:asl2.0)))

(define-public python-nbval
  (package
    (name "python-nbval")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbval" version))
       (sha256
        (base32 "0h3xrnw0mj1srigrx2rfnd73h8s0xjycclmjs0vx7qkfyqpcvvyg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-test
           (lambda _
             ;; This test fails because of a mismatch in the output of LaTeX
             ;; equation environments.  Seems OK to skip.
             (delete-file
              "tests/ipynb-test-samples/test-latex-pass-correctouput.ipynb")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "-k"
                       (string-append
                        ;; This only works with Pytest < 5.
                        "not nbdime_reporter"
                        ;; https://github.com/computationalmodelling/nbval/pull/148.
                        " and not test_timeouts"
                        ;; It seems the output format has changed; the following
                        ;; test fails with "Unexpected output fields from
                        ;; running code: {'text/plain'}".
                        " and not test_conf_ignore_stderr "))))))))
    (native-inputs
     (list python-pytest python-pytest-cov python-sympy))
    (propagated-inputs
     (list python-ipykernel python-jupyter-client python-nbformat
           python-six))
    (home-page "https://github.com/computationalmodelling/nbval")
    (synopsis "Pytest plugin to validate Jupyter notebooks")
    (description
     "This plugin adds functionality to Pytest to recognise and collect Jupyter
notebooks.  The intended purpose of the tests is to determine whether execution
of the stored inputs match the stored outputs of the @file{.ipynb} file.  Whilst
also ensuring that the notebooks are running without errors.")
    (license license:bsd-3)))

(define-public python-pytest-flask
  (package
    (name "python-pytest-flask")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flask" version))
       (sha256
        (base32
         "1hln7mwgdzfi5ma0kqfsi768l7p24jhkw8l0imhifwy08nh7hmjd"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flask python-pytest python-setuptools-scm
           python-werkzeug))
    (home-page "https://github.com/pytest-dev/pytest-flask")
    (synopsis "Pytest fixtures to test Flask applications")
    (description
     "This pytest plugin provides fixtures to simplify Flask app testing.")
    (license license:expat)))

(define-public python-pytest-console-scripts
  (package
    (name "python-pytest-console-scripts")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-console-scripts" version))
       (sha256
        (base32
         "1qsw3i2h3psyi5avwf14panx8wxqfik2z7294dy37w8ha415iwn7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "--verbose"
                       ;; This one test fails because of PATH assumptions
                       "-k" "not test_elsewhere_in_the_path")))))))
    (propagated-inputs
     (list python-mock python-pytest))
    (native-inputs
     (list python-setuptools-scm))
    (home-page "https://github.com/kvas-it/pytest-console-scripts")
    (synopsis "Pytest plugin for testing console scripts")
    (description
     "This package provides a pytest plugin for testing console scripts.")
    (license license:expat)))

(define-public python-pytest-tornado
  (package
    (name "python-pytest-tornado")
    (version "0.8.1")
    (source (origin
              (method git-fetch)        ;no tests in pypi archive
              (uri (git-reference
                    (url "https://github.com/eugeniy/pytest-tornado")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05hgq1m9g35kpc01im7ci1wd85xi1rdxnyms9izjg65c9976zn6x"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (propagated-inputs (list python-pytest python-setuptools python-tornado))
    (home-page "https://github.com/eugeniy/pytest-tornado")
    (synopsis "Pytest plugin to ease testing tornado applications")
    (description
     "This package provides a py.test plugin providing fixtures and markers to
simplify testing of asynchronous tornado applications.")
    (license license:asl2.0)))

(define-public python-pytest-tornasync
  (package
    (name "python-pytest-tornasync")
    (version "0.6.0.post2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-tornasync" version))
       (sha256
        (base32
         "0pdyddbzppkfqwa7g17sdfl4w2v1hgsky78l8f4c1rx2a7cvd0fp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false ; TODO: fails at "from test import MESSAGE, PAUSE_TIME"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "--verbose")))))))
    (propagated-inputs
     (list python-pytest python-tornado))
    (home-page "https://github.com/eukaryote/pytest-tornasync")
    (synopsis "Pytest plugin for testing Tornado code")
    (description
     "This package provides a simple pytest plugin that provides some helpful
fixtures for testing Tornado (version 5.0 or newer) apps and easy handling of
plain (undecoratored) native coroutine tests.")
    (license license:expat)))

(define-public python-pytest-celery
  (package
    (name "python-pytest-celery")
    (version "0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-celery" version))
       (sha256
        (base32 "01pli108qqiiyrn8qsqqabcpazrzj27r7cji9wgglsk76by61l6g"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests and circular dependency on python-celery
       #:phases
       (modify-phases %standard-phases
         (delete 'sanity-check)))) ; checks for celery
    (home-page "https://github.com/graingert/pytest-celery")
    (synopsis "Shim pytest plugin to enable @code{celery.contrib.pytest}")
    (description
     "This package provides a shim Pytest plugin to enable a Celery marker.")
    (license license:bsd-3)))

(define-public python-pytest-env
  (package
    (name "python-pytest-env")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-env" version))
       (sha256
        (base32 "1hl0ln0cicdid4qjk7mv90lw9xkb0v71dlj7q7rn89vzxxm9b53y"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/MobileDynasty/pytest-env")
    (synopsis "Pytest plugin that allows you to add environment variables")
    (description
     "This is a @code{py.test} plugin that enables you to set environment
variables in the @file{pytest.ini} file.")
    (license license:expat)))

(define-public python-pyux
  (package
    (name "python-pyux")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyux" version))
       (sha256
        (base32
         "1i17xh4dy238ibrjdgh8vn78fk5q6dj37mcznpvdfzidj57js7ca"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                  ;the mini test suite fails
    (home-page "https://github.com/farizrahman4u/pyux")
    (synopsis "Utility to check API integrity in Python libraries")
    (description "The pyux utility detects API changes in Python
libraries.")
    (license license:expat)))

(define-public python-pytest-qt
  (package
    (name "python-pytest-qt")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-qt" version))
       (sha256
        (base32 "09c9psfn3zigpaw1l1cmynpa3csxa49wc2ih5lzl24skdkw0njvi"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-qpa
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     (list python-pyqt))
    (native-inputs
     (list python-pytest python-pytest-runner python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-qt")
    (synopsis "Pytest support for PyQt and PySide applications")
    (description
     "@code{pytest-qt} is a Pytest plugin that allows programmers to write
tests for PyQt5 and PySide2 applications.

The main usage is to use the @code{qtbot} fixture, responsible for handling
@code{qApp} creation as needed and provides methods to simulate user
interaction, like key presses and mouse clicks.")
    (license license:expat)))

(define-public python-codacy-coverage
  (package
    (name "python-codacy-coverage")
    (version "1.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codacy-coverage" version))
        (sha256
         (base32
          "1g0c0w56xdkmqb8slacyw5qhzrkp814ng3ddh2lkiij58y9m2imr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     (list python-check-manifest python-requests))
    (home-page "https://github.com/codacy/python-codacy-coverage")
    (synopsis "Codacy coverage reporter for Python")
    (description "This package analyses Python test suites and reports how much
of the code is covered by them.  This tool is part of the Codacy suite for
analysing code quality.")
    (license license:expat)))

(define-public python-httmock
  (package
    (name "python-httmock")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "httmock" version))
        (sha256
         (base32
          "1zj1fcm0n6f0wr9mr0hmlqz9430fnr5cdwd5jkcvq9j44bnsrfz0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/patrys/httmock")
    (synopsis "Mocking library for requests")
    (description "This package provides a library for replying fake data to
Python software under test, when they make an HTTP query.")
    (license license:asl2.0)))

(define-public python-atpublic
  (package
    (name "python-atpublic")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "atpublic" version))
        (sha256
         (base32
          "060v2b5jfn7p99j09amxlb6w9ynwbq7fix31kl0caz0hs09fx61h"))))
    (build-system pyproject-build-system)
    (arguments (list #:build-backend "pdm.backend"))
    (native-inputs
     (list python-pytest python-pdm-backend python-sybil python-pytest-cov))
    (home-page "https://public.readthedocs.io/")
    (synopsis "@code{@@public} decorator for populating @code{__all__}")
    (description
     "This Python module adds a @code{@@public} decorator and function which
populates a module's @code{__all__} and optionally the module globals.  With
it, the declaration of a name's public export semantics are not separated from
the implementation of that name.")
    (license (list license:asl2.0
                   license:lgpl3))))    ; only for setup_helpers.py

(define-public python-memory-profiler
  (package
    (name "python-memory-profiler")
    (version "0.61")
    (source
     (origin
       ;; PyPi tarball lacks tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pythonprofilers/memory_profiler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n6g47qqmnn7abh3v25535hd8bmfvhf9bnp72m7bkd89f715m7xh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: @profile is not loaded in some test files and there are 3
          ;; tests fail, disable them for now.
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (with-directory-excursion "test"
                (for-each delete-file
                          '("test_as.py"
                            "test_func.py"
                            "test_gen.py"
                            "test_loop.py"
                            "test_loop_decorated.py"
                            "test_mprofile.py"
                            "test_nested.py"
                            "test_precision_command_line.py"
                            "test_unicode.py")))
              (substitute* "test/test_attributes.py"
                (("def test_with_profile") "def __off_test_with_profile"))
              (substitute* "test/test_stream_unicode.py"
                (("def test_unicode") "def __off_test_unicode"))
              (substitute* "test/test_tracemalloc.py"
                (("def test_memory_profiler")
                 "def __off_test_memory_profiler")))))))
    (native-inputs
     (list python-pytest python-pytest-fixture-config python-safety))
    (propagated-inputs (list python-psutil))
    (home-page "https://github.com/pythonprofilers/memory_profiler")
    (synopsis "Memory profiler for Python")
    (description
     "This package provides a module for monitoring the memory usage of a
Python program.")
    (license license:bsd-3)))

(define-public python-mockito
  (package
    (name "python-mockito")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)               ;no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/kaste/mockito-python")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fg8jflcf4c929gd4zbcrk73d08waaqjfjmdjrgnv54mzl35pjxl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (native-inputs
     (list python-numpy python-pytest))
    (home-page "https://github.com/kaste/mockito-python")
    (synopsis "Mocking library for Python")
    (description "This package provides a Python implementation of the Java
library of the same name.  It eases monkey patching, for example to stub out
side effects when unit testing.")
    (license license:expat)))

(define-public python-mypy-extensions
  (package
    (name "python-mypy-extensions")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mypy_extensions" version))
        (sha256
         (base32
          "1a04qsk8hd1lqns8w1j7cr0vmvbhg450di5k1i16kqxkbf7q30id"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)); no tests
    (home-page "https://github.com/python/mypy_extensions")
    (synopsis "Experimental extensions for MyPy")
    (description "The @code{python-mypy-extensions} module defines
experimental extensions to the standard @code{typing} module that are
supported by the MyPy typechecker.")
    (license license:expat)))

(define-public python-mypy
  (package
    (name "python-mypy")
    (version "0.971")
    (source
     (origin
       ;; Because of https://github.com/python/mypy/issues/9584, the
       ;; mypyc/analysis directory is missing in the PyPI archive, leading to
       ;; test failures.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python/mypy")
             (commit (string-append "v" version))
             ;; Fetch git submodules otherwise typeshed is not fetched.
             ;; Typeshed is a collection of Python sources type annotation
             ;; (data) files.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0i8swdynms1wpiprgqn24za6mx8rlgxr2jash3cb5xi8jyf58n97"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "mypyc"
                       ;; XXX: This test gets an unexpected DeprecationWarning
                       ;; from recent versions of setuptools.  Ignore for now.
                       "-k" "not testImports")))))))
    (native-inputs
     (list python-attrs
           python-lxml
           python-psutil
           python-pytest
           python-pytest-forked
           python-pytest-xdist
           python-virtualenv))
    (propagated-inputs
     (list python-mypy-extensions python-tomli python-typing-extensions))
    (home-page "https://www.mypy-lang.org/")
    (synopsis "Static type checker for Python")
    (description "Mypy is an optional static type checker for Python that aims
to combine the benefits of dynamic typing and static typing.  Mypy combines
the expressive power and convenience of Python with a powerful type system and
compile-time type checking.  Mypy type checks standard Python programs; run
them using any Python VM with basically no runtime overhead.")
    ;; Most of the code is under MIT license; Some files are under Python Software
    ;; Foundation License version 2: stdlib-samples/*, mypyc/lib-rt/pythonsupport.h and
    ;; mypyc/lib-rt/getargs.c
    (license (list license:expat license:psfl))))

;;; This variant exists to break a cycle between python-pylama and python-isort.
(define-public python-mypy-minimal
  (hidden-package
   (package
     (inherit python-mypy)
     (name "python-mypy-minimal")
     (arguments
      `(#:tests? #f
        #:phases (modify-phases %standard-phases
                   ;; XXX: Fails with: "In procedure utime: No such file or
                   ;; directory".
                   (delete 'ensure-no-mtimes-pre-1980))))
     (native-inputs '()))))

(define-public python-nptyping
  (package
    (name "python-nptyping")
    (version "2.0.0")
    (source (origin
              (method git-fetch)        ;pypi only contains a binary wheel
              (uri (git-reference
                    (url "https://github.com/ramonhagenaars/nptyping")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0839mcrv5jljq9k9124ssnl1hc1inbxwlwjk72imabsbqssjy9rb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-source-date-epoch
           (lambda _
             ;; Otherwise the wheel building test would fail with "ZIP does
             ;; not support timestamps before 1980".
             (setenv "SOURCE_DATE_EPOCH" "315532800"))))))
    (native-inputs
     (list python-beartype
           python-mypy
           python-typeguard
           python-wheel))
    (propagated-inputs (list python-numpy python-typing-extensions))
    (home-page "https://github.com/ramonhagenaars/nptyping")
    (synopsis "Type hints for Numpy")
    (description "This package provides extensive dynamic type checks for
dtypes and shapes of arrays for NumPy, extending @code{numpy.typing}.")
    (license license:expat)))

(define-public python-pylama
  (package
    (name "python-pylama")
    (version "7.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pylama" version))
       (sha256
        (base32
         "13vx7daqz2918y9s8q3v2i3xaq3ah43a9p58srqi6hqskkpm7blv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-tests
                    (lambda _
                      ;; Fails with: "ImportError: cannot import name
                      ;; 'commented_out_code_line_numbers' from 'eradicate'".
                      (delete-file "pylama/lint/pylama_eradicate.py")
                      ;; Requires python-astroid, which fails to build on
                      ;; Python 3.9+ (see:
                      ;; https://github.com/PyCQA/astroid/issues/881).
                      (delete-file "pylama/lint/pylama_pylint.py"))))))
    (native-inputs
     (list python-py python-pytest python-radon))
    (propagated-inputs
     `(("python-mccabe" ,python-mccabe)
       ("python-mypy", python-mypy-minimal)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-pydocstyle" ,python-pydocstyle)
       ("python-pyflakes" ,python-pyflakes)))
    (home-page "https://github.com/klen/pylama")
    (synopsis "Code audit tool for python")
    (description "Pylama is a code audit tool for Python and JavaScript to check
for style, syntax and other code health metrics.  It is essentially a
convenient wrapper above tools such as Pyflakes, pydocstyle, pycodestyle and
McCabe, among others.")
    (license license:lgpl3+)))

(define-public python-pyannotate
  (package
    (name "python-pyannotate")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyannotate" version))
       (sha256
        (base32
         "16bm0mf7wxvy0lgmcs1p8n1ji8pnvj1jvj8zk3am70dkp825iv84"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-mypy-extensions python-six))
    (home-page
     "https://github.com/dropbox/pyannotate")
    (synopsis "Auto-generate PEP-484 annotations")
    (description "This package, PyAnnotate, is used to auto-generate PEP-484
annotations.")
    (license license:asl2.0)))

(define-public python-eradicate
  (package
    (name "python-eradicate")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eradicate" version))
       (sha256
        (base32
         "1j30g9jfmbfki383qxwrfds8b23yiwywj40lng4lqcf5yab4ahr7"))))
    (build-system python-build-system)
    (home-page "https://github.com/myint/eradicate")
    (synopsis "Remove commented-out code from Python sources")
    (description "The @command{eradicate} command removes commented-out code
from Python files.  It does this by detecting block comments that contain
valid Python syntax that are likely to be commented out code.")
    (license license:expat)))

(define-public python-robber
  (package
    (name "python-robber")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "robber" version))
              (sha256
               (base32
                "0xp5csgv2g9q38hscml6bc5i1nm4xy5lzqqiimm2drxsf0hw2nq5"))))
    (build-system python-build-system)
    ;; There are no tests in the tarball downloaded from PyPI.
    ;; The last version tagged in Github (0.1.0) is older than the one on PyPI.
    ;; Reported upstream: <https://github.com/vesln/robber.py/issues/20>.
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-mock python-termcolor))
    ;; URL of the fork used to generate the package available on PyPI.
    (home-page "https://github.com/EastAgile/robber.py")
    (synopsis "Test-driven development (TDD) assertion library for Python")
    (description "Robber is a Python assertion library for test-driven and
behavior-driven development (TDD and BDD).")
    (license license:expat)))

(define-public python-stestr
  (package
    (name "python-stestr")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stestr" version))
       (sha256
        (base32
         "1kg9gfdr4bj2m7s1r44z530a0ba4p17j4jlhcn1xha0j8jmyfgn2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;to avoid circular dependencies
    (native-inputs
     (list python-pbr))
    (propagated-inputs
     (list python-cliff
           python-fixtures
           python-future
           python-pyyaml
           python-subunit
           python-testtools
           python-voluptuous))
    (home-page "https://stestr.readthedocs.io/en/latest/")
    (synopsis "Parallel Python test runner")
    (description "This package provides the @command{stestr} command, a
parallel Python test runner built around @code{subunit}.  It is designed to
execute @code{unittest} test suites using multiple processes to split up
execution of a test suite.  It will also store a history of all test runs to
help in debugging failures and optimizing the scheduler to improve speed.")
    (license license:asl2.0)))

;; This is only used by python-sanic
(define-public python-pytest-sanic
  (package
    (name "python-pytest-sanic")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-sanic" version))
              (sha256
                (base32
                  "0shq1bqnydj0l3ipb73j1qh5kqcjvzkps30zk8grq3dwmh3wmnkr"))))
    (build-system python-build-system)
    (arguments
     ;; Tests depend on python-sanic.
     `(#:tests? #f))
    (propagated-inputs
      (list python-httpx python-async-generator python-pytest
            python-websockets))
    (home-page
      "https://github.com/yunstanford/pytest-sanic")
    (synopsis "Pytest plugin for Sanic")
    (description "A pytest plugin for Sanic.  It helps you to test your
code asynchronously.")
    (license license:expat)))

(define-public python-allpairspy
  (package
    (name "python-allpairspy")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "allpairspy" version))
       (sha256
        (base32 "1c987h13dly9919d15w3h747rgn50ilnv7dginhlprxbj564hn4k"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner))
    (home-page "https://github.com/thombashi/allpairspy")
    (synopsis "Pairwise test combinations generator")
    (description
     "This is a Python library for test combinations generator.  The generator
allows one to create a set of tests using @emph{pairwise combinations} method,
reducing a number of combinations of variables into a lesser set that covers
most situations.")
    (license license:expat)))

(define-public python-pytest-mp
  (package
    (name "python-pytest-mp")
    (version "0.0.4p2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ansible/pytest-mp")
             (commit "49a8ff2ca9ef62d8c86854ab31d6b5d5d6cf3f28")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01v98b6n3yvkfmxf2v38xk5ijqlk6ika0yljwkhl5bh6qhq23498"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest python-psutil))
    (arguments
     ;; tests require setuptools-markdown, which is deprecated and not in guix
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-useless-requirements
           (lambda _
             (substitute* "setup.py"
               ((" setup_requires=") " #")))))))
    (home-page "https://github.com/ansible/pytest-mp")
    (synopsis  "Segregate tests into several processes")
    (description "pytest-mp is a minimalist approach to distribute and
segregate pytest tests across processes using python's multiprocessing library
and is heavily inspired by pytest-concurrent and pytest-xdist.  As a very
early beta, it doesn't pledge or intend to support the majority of platforms
or use cases.  Design is based on supporting slow, io-bound testing with often
tedious system under test configuration that can benefit from running several
tests at one time.")
    (license license:expat)))

(define-public python-aioresponses
  (package
    (name "python-aioresponses")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aioresponses" version))
       (sha256
        (base32 "16p8mdyfirddrsay62ji7rwcrqmmzxzf2isdbfm9cj5p338rbr42"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke
                "pytest" "-vv" "tests" "-k"
                (string-append
                 ;; These tests require network access.
                 "not test_address_as_instance_of_url_combined_with_pass_through "
                 "and not test_pass_through_with_origin_params"))))))))
    (native-inputs
     (list python-pbr python-ddt python-pytest))
    (propagated-inputs
     (list python-aiohttp))
    (home-page "https://github.com/pnuckowski/aioresponses")
    (synopsis "Mock out requests made by ClientSession from aiohttp package")
    (description
     "Aioresponses is a helper to mock/fake web requests in python aiohttp
package.  For requests module there are a lot of packages that help us with
testing (eg. httpretty, responses, requests-mock).  When it comes to testing
asynchronous HTTP requests it is a bit harder (at least at the beginning).
The purpose of this package is to provide an easy way to test asynchronous
HTTP requests.")
    (license license:expat)))

(define-public python-avocado-framework
  (package
    (name "python-avocado-framework")
    (version "96.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "avocado-framework" version))
       (sha256
        (base32 "0zhz6423p0b5gqx2mvg7dmq8m9gbsay7wqjdwzirlwcg2v3rcz0m"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; The test suite hangs, due to a serious bug in Python/Avocado (see:
      ;; https://github.com/avocado-framework/avocado/issues/4935).
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; These are runtime dependencies (inputs).
              (substitute* "avocado/plugins/spawners/podman.py"
                (("default='/usr/bin/podman'")
                 "default='podman'"))
              (substitute* "avocado/utils/podman.py"
                (("\"/usr/bin/env\", \"python3\"")
                 (format #f "~s" (search-input-file inputs "bin/python"))))
              (substitute* "avocado/utils/memory.py"
                (("\"sync\"")
                 (format #f "~s" (search-input-file inputs "bin/sync")))
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))
              ;; Batch process the tests modules with less care; if something
              ;; is wrong, the test suite will fail.  These are tests
              ;; dependencies (native inputs).
              (substitute* (find-files "selftests" "\\.py$")
                (("#!/usr/bin/env")
                 (string-append "#!" (search-input-file (or native-inputs inputs)
                                                        "bin/env")))
                (("/bin/(false|true|sh|sleep|sudo)" _ name)
                 (search-input-file (or native-inputs inputs)
                                    (string-append "bin/" name))))))
          (add-after 'unpack 'remove-broken-entrypoints
            ;; The avocado-external-runner entry point fails to load, the
            ;; 'scripts' top level package not being found (see:
            ;; https://github.com/avocado-framework/avocado/issues/5370).
            (lambda _
              (substitute* "setup.py"
                (("'avocado-external-runner = scripts.external_runner:main'.*")
                 ""))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (setenv "PYTHONPATH" (getcwd))
                (invoke "./selftests/check.py" "--skip" "static-checks")))))))
    (native-inputs (list bash-minimal coreutils-minimal perl sudo))
    (inputs (list bash-minimal coreutils-minimal))
    (home-page "https://avocado-framework.github.io/")
    (synopsis "Tools and libraries to help with automated testing")
    (description "Avocado is a set of tools and libraries to help with
automated testing, i.e. a test framework.  Native tests are written in Python
and they follow the unittest pattern, but any executable can serve as a
test.  The following output formats are supported:
@table @asis
@item xUnit
an XML format that contains test results in a structured form, and are used by
other test automation projects, such as Jenkins.
@item JSON
a widely used data exchange format.  The JSON Avocado plugin outputs job
information, similarly to the xunit output plugin.
@item TAP
Provides the basic TAP (Test Anything Protocol) results.  Unlike most existing
Avocado machine readable outputs this one is streamlined (per test results).
@end table")
    (license license:gpl2)))            ;some files are under GPLv2 only

(define-public python-parameterizedtestcase
  (package
    (name "python-parameterizedtestcase")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parameterizedtestcase" version))
       (sha256
        (base32 "0zhjmsd16xacg4vd7zb75kw8q9khn52wvad634v1bvz7swaivk2c"))))
    (build-system python-build-system)
    (native-inputs (list python-setuptools-57)) ;for use_2to3
    (home-page
     "https://github.com/msabramo/python_unittest_parameterized_test_case")
    (synopsis "Parameterized tests for Python's unittest module")
    (description "This package provides parameterized tests for Python's
@code{unittest} module taking inspiration from pytest.")
    (license license:expat)))

(define-public python-pytest-rerunfailures
  (package
    (name "python-pytest-rerunfailures")
    (version "10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-rerunfailures" version))
       (sha256
        (base32 "15v68kggjvkflbqr0vz8gp5yp3pcsk0rz05bpg2l4xp0a6nin7ly"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/pytest-dev/pytest-rerunfailures")
    (synopsis "Pytest plugin to re-run flaky tests")
    (description "This package provides a pytest plugin to re-run tests to
eliminate flaky failures.")
    (license license:mpl2.0)))

(define-public python-xunitparser
  (package
    (name "python-xunitparser")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xunitparser" version))
       (sha256
        (base32 "00lapxi770mg7jkw16zy3a91hbdfz4a9h43ryczdsgd3z4cl6vyf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; See https://github.com/laurentb/xunitparser/pull/11
         (add-after 'unpack 'fix-test-suite
           (lambda _
             (substitute* "xunitparser.py"
               (("(^ +)self.stderr = None" m indent)
                (string-append m "\n" indent "self._cleanup = False\n"))))))))
    (home-page "http://git.p.engu.in/laurentb/xunitparser/")
    (synopsis "Read JUnit/XUnit XML files and map them to Python objects")
    (description "xunitparser reads a JUnit/XUnit XML file and maps it to
Python objects.  It tries to use the objects available in the standard
@code{unittest} module.")
    (license license:expat)))

(define-public python-test-utils
  (package
    (name "python-test-utils")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "test-utils" version))
       (sha256
        (base32 "0cs0gyihnkj8ya4yg3ld3ly73mpxrkn2gq9acamclhqvhxsv7zd6"))))
    (build-system python-build-system)
    (home-page "https://github.com/Kami/python-test-utils/")
    (synopsis "Utilities for functional and integration tests")
    (description
     "This package provides a collection of utility functions and classes
which make writing and running functional and integration tests easier.")
    (license license:asl2.0)))

(define-public python-nox
  (package
    (name "python-nox")
    (version "2022.11.21")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wntrblm/nox")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xfd63h75wiiyri4f7qyvy50f2ny0v4r4wx2h4px9ddbkh2k5g9p"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; NOTE: This manipulation looks not clear as upstream package
               ;; contains "nox/tox_to_nox.jinja2" file which is not copied
               ;; during install phase and causes check and sanity-check
               ;; phases fail due to missing file. Try to find more simple
               ;; solution.
               (add-after 'unpack 'rename-tox-to-nox-jinja2
                 (lambda _
                   (rename-file "nox/tox_to_nox.jinja2" "nox/tox_to_nox.jinja2.py")))
               (add-after 'install 'rename-tox-to-nox-jinja2-back
                 (lambda _
                   (let* ((src-file (car (find-files (string-append #$output "/lib")
                                                     "tox_to_nox\\.jinja2\\.py$")))
                          (dst-file (string-drop-right src-file 3)))
                     (rename-file src-file dst-file)))))))
    (propagated-inputs
     (list python-argcomplete
           python-colorlog
           python-packaging
           python-py
           python-virtualenv))
    (native-inputs
     (list python-jinja2
           python-pytest
           python-tox))
    (home-page "https://nox.thea.codes/")
    (synopsis "Flexible test automation")
    (description
     "@code{nox} is a command-line tool that automates testing in multiple
Python environments, similar to @code{tox}.  Unlike tox, Nox uses a standard
Python file for configuration.")
    (license license:asl2.0)))

(define-public python-tox
  (package
    (name "python-tox")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tox" version))
       (sha256
        (base32
         "0nk0nyzhzamcrvn0qqzzy54isxxqwdi28swml7a2ym78c3f9sqpb"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "-k"
                        (string-join
                         (map (lambda (test)
                                (string-append "not test_" test))
                              '("invocation_error"
                                "create_KeyboadInterrupt"
                                "exit_code"
                                "tox_get_python_executable"
                                "find_alias_on_path"
                                "get_executable"
                                "get_executable_no_exist"
                                "get_sitepackagesdir_error"
                                "spinner_stdout_not_unicode"
                                "provision_non_canonical_dep"
                                "package_setuptools"
                                "package_poetry"
                                "parallel_interrupt"
                                "provision_missing"
                                "provision_from_pyvenv"
                                "provision_interrupt_child"
                                "create"
                                "run_custom_install_command"
                                "toxuone_env"
                                "different_config_cwd"
                                "test_usedevelop"
                                "build_backend_without_submodule"
                                "parallel"
                                "parallel_live"
                                "tox_env_var_flags_inserted_isolated"))
                         " and "))))))))
    (propagated-inputs
     (list python-filelock
           python-packaging
           python-pluggy
           python-py
           python-six
           python-toml
           python-virtualenv))
    (native-inputs
     (list python-flaky
           python-pathlib2
           python-pytest                ; >= 2.3.5
           python-pytest-freezegun
           python-pytest-timeout
           python-setuptools-scm))
    (home-page "https://tox.readthedocs.io")
    (synopsis "Virtualenv-based automation of test activities")
    (description "Tox is a generic virtualenv management and test command line
tool.  It can be used to check that a package installs correctly with
different Python versions and interpreters, or run tests in each type of
supported environment, or act as a frontend to continuous integration
servers.")
    (license license:expat)))

(define-public python-sybil
  (package
    (name "python-sybil")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sybil" version))
        (sha256
          (base32 "03ak1w93linfqx6c9lwgq5niyy3j9yblv4ip40hmlzmg0hidq0kg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest")))))))
    (native-inputs (list python-pytest python-pytest-cov))
    (home-page "https://github.com/simplistix/sybil")
    (synopsis "Automated testing for examples in code and documentation")
    (description
      "This library provides a way to check examples in your code and
documentation by parsing them from their source and evaluating the
parsed examples as part of your normal test run.  Integration is
provided for the main Python test runners.")
    (license license:expat)))

(define-public python-pytest-parawtf
  (package
    (name "python-pytest-parawtf")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-parawtf" version))
              (sha256
               (base32
                "08s86hy58lvrd90cnayzydvac4slaflj0ph9yknakcc42anrm023"))))
    (build-system python-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 ;; https://github.com/flub/pytest-parawtf/issues/1
                 (invoke "pytest" "-k" "not test_mark")))))))
    (propagated-inputs (list python-pytest))
    (home-page "https://github.com/flub/pytest-parawtf/")
    (synopsis "Finally spell paramete?ri[sz]e correctly")
    (description
"@code{python-pytest} uses one of four different spellings of
parametrize.  This plugin allows you to use all four.")
    (license license:expat)))

(define-public python-pytest-httpx
  (package
    (name "python-pytest-httpx")
    (version "0.22.0")
    (source
     (origin
       ;; pypi package doesn't include the tests
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Colin-b/pytest_httpx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ncpd74hmsz4sadvjg99fnfscxpgh3mc2siini0dhxzwgwdkk5i7"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-httpx))
    (native-inputs (list python-pytest python-pytest-asyncio))
    (home-page "https://colin-b.github.io/pytest_httpx/")
    (synopsis "Pytest plugin to mock httpx")
    (description "This package provides a pytest fixture to mock httpx
requests to be replied to with user provided responses.")
    (license license:expat)))

(define-public python-pycotap
  (package
    (name "python-pycotap")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycotap" version))
       (sha256
        (base32 "1v69fxial9i5wlap6wc4igq3hydvxbak7dlgb7cikk8wjgafqf7r"))))
    (build-system python-build-system)
    (home-page "https://el-tramo.be/pycotap")
    (synopsis "Tiny Python TAP test runner")
    (description "This package provides a simple Python test runner for
unittest that outputs Test Anything Protocol (TAP) results to standard
output.  Contrary to other TAP runners for Python, pycotap...
@itemize
@item
prints TAP (and only TAP) to standard output instead of to a separate file,
allowing you to pipe it directly to TAP pretty printers and processors;
@item only contains a TAP reporter, so no parsers, no frameworks, no
dependencies, etc;
@item
is configurable: you can choose how you want the test output and test result
diagnostics to end up in your TAP output (as TAP diagnostics, YAML blocks, or
attachments).
@end itemize")
    (license license:expat)))

(define-public python-xvfbwrapper
  (package
    (name "python-xvfbwrapper")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xvfbwrapper" version))
              (sha256
               (base32
                "097wxhvp01ikqpg1z3v8rqhss6f1vwr399zpz9a05d2135bsxx5w"))))
    (build-system python-build-system)
    (propagated-inputs (list xorg-server-for-tests))
    (home-page "https://github.com/cgoldberg/xvfbwrapper")
    (synopsis "Python module for controlling virtual displays with Xvfb")
    (description
     "Xvfb (X virtual framebuffer) is a display server implementing
the X11 display server protocol.  It runs in memory and does not require a
physical display.  Only a network layer is necessary.  Xvfb is useful for
running acceptance tests on headless servers.")
    (license license:expat)))

(define-public python-vulture
  (package
    (name "python-vulture")
    (version "2.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "vulture" version))
              (sha256
               (base32
                "0cl0v3dadxvff0pgq1j120m064a3nmnbjjylkmcxp7zd2jh81yv7"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "pytest" "-vv" "tests" "-k"
                                      ;; skip test that uses python-pint
                                      ;; pint has many dependencies
                                      "not test_whitelists_with_python")))))))
    (native-inputs (list python-pytest python-pytest-cov))
    (propagated-inputs (list python-toml))
    (home-page "https://github.com/jendrikseipp/vulture")
    (synopsis "Find dead Python code")
    (description
     "Vulture finds unused code in Python programs.  This is useful for
cleaning up and finding errors in large code bases.  If you run Vulture on
both your library and test suite you can find untested code.  Due to Python's
dynamic nature, static code analyzers like Vulture are likely to miss some
dead code.  Also, code that is only called implicitly may be reported as
unused.")
    (license license:expat)))
