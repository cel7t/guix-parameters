;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-gem)
  #:use-module (guix import gem)
  #:use-module (guix upstream)
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix build-system ruby) #:select (rubygems-uri))
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix tests)
  #:use-module ((guix build utils) #:select (delete-file-recursively))
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define test-foo-json
  "{
  \"name\": \"foo\",
  \"version\": \"1.0.0\",
  \"sha\": \"f3676eafca9987cb5fe263df1edf2538bf6dafc712b30e17be3543a9680547a8\",
  \"info\": \"A cool gem\",
  \"homepage_uri\": \"https://example.com\",
  \"dependencies\": {
    \"runtime\": [
      { \"name\": \"bundler\" },
      { \"name\": \"bar\" }
    ]
  },
  \"licenses\": [\"MIT\", \"Apache 2.0\"]
}")

(define test-foo-v2-json
  "{
  \"name\": \"foo\",
  \"version\": \"2.0.0\",
  \"sha\": \"f3676eafca9987cb5fe263df1edf2538bf6dafc712b30e17be3543a9680547a8\",
  \"info\": \"A cool gem\",
  \"homepage_uri\": \"https://example.com\",
  \"dependencies\": {
    \"runtime\": [
      { \"name\": \"bundler\" },
      { \"name\": \"bar\" }
    ]
  },
  \"licenses\": [\"MIT\", \"Apache 2.0\"]
}")

(define test-bar-json
  "{
  \"name\": \"bar\",
  \"version\": \"1.0.0\",
  \"sha\": \"f3676eafca9987cb5fe263df1edf2538bf6dafc712b30e17be3543a9680547a8\",
  \"info\": \"Another cool gem\",
  \"homepage_uri\": \"https://example.com\",
  \"dependencies\": {
    \"runtime\": [
      { \"name\": \"bundler\" }
    ]
  },
  \"licenses\": null
}")

(define test-bundler-json
  "{
  \"name\": \"bundler\",
  \"version\": \"1.14.2\",
  \"sha\": \"3bb53e03db0a8008161eb4c816ccd317120d3c415ba6fee6f90bbc7f7eec8690\",
  \"info\": \"Ruby gem bundler\",
  \"homepage_uri\": \"https://bundler.io/\",
  \"dependencies\": {
    \"runtime\": []
  },
  \"licenses\": [\"MIT\"]
}")

(test-begin "gem")

(test-assert "gem->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://rubygems.org/api/v1/gems/foo.json"
              (values (open-input-string test-foo-json)
                      (string-length test-foo-json)))
             (_ (error "Unexpected URL: " url)))))
    (match (gem->guix-package "foo")
      (`(package
          (name "ruby-foo")
          (version "1.0.0")
          (source (origin
                    (method url-fetch)
                    (uri (rubygems-uri "foo" version))
                    (sha256
                     (base32
                      "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
          (build-system ruby-build-system)
          (propagated-inputs (list bundler ruby-bar))
          (synopsis "A cool gem")
          (description "This package provides a cool gem")
          (home-page "https://example.com")
          (license (list license:expat license:asl2.0)))
       #t)
      (x
       (pk 'fail x #f)))))

(test-assert "gem->guix-package with a specific version"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://rubygems.org/api/v2/rubygems/foo/versions/2.0.0.json"
              (values (open-input-string test-foo-v2-json)
                      (string-length test-foo-v2-json)))
             (_ (error "Unexpected URL: " url)))))
    (match (gem->guix-package "foo" #:version "2.0.0")
      (`(package
          (name "ruby-foo")
          (version "2.0.0")
          (source (origin
                    (method url-fetch)
                    (uri (rubygems-uri "foo" version))
                    (sha256
                     (base32
                      "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
          (build-system ruby-build-system)
          (propagated-inputs (list bundler ruby-bar))
          (synopsis "A cool gem")
          (description "This package provides a cool gem")
          (home-page "https://example.com")
          (license (list license:expat license:asl2.0)))
       #t)
      (x
       (pk 'fail x #f)))))

(test-assert "gem-recursive-import"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://rubygems.org/api/v1/gems/foo.json"
              (values (open-input-string test-foo-json)
                      (string-length test-foo-json)))
             ("https://rubygems.org/api/v1/gems/bar.json"
              (values (open-input-string test-bar-json)
                      (string-length test-bar-json)))
             ("https://rubygems.org/api/v1/gems/bundler.json"
              (values (open-input-string test-bundler-json)
                      (string-length test-bundler-json)))
             (_ (error "Unexpected URL: " url)))))
        (match (gem-recursive-import "foo")
          (`((package
               (name "ruby-bar")
               (version "1.0.0")
               (source
                (origin
                  (method url-fetch)
                  (uri (rubygems-uri "bar" version))
                  (sha256
                   (base32
                    "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
               (build-system ruby-build-system)
               (propagated-inputs (list bundler))
               (synopsis "Another cool gem")
               (description "Another cool gem")
               (home-page "https://example.com")
               (license #f))                      ;no licensing info
             (package
               (name "ruby-foo")
               (version "1.0.0")
               (source
                (origin
                  (method url-fetch)
                  (uri (rubygems-uri "foo" version))
                  (sha256
                   (base32
                    "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
               (build-system ruby-build-system)
               (propagated-inputs (list bundler ruby-bar))
               (synopsis "A cool gem")
               (description "This package provides a cool gem")
               (home-page "https://example.com")
               (license (list license:expat license:asl2.0))))
           #t)
          (x
           (pk 'fail x #f)))))

(test-assert "gem-recursive-import with a specific version"
  ;; Replace network resources with sample data.
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://rubygems.org/api/v2/rubygems/foo/versions/2.0.0.json"
              (values (open-input-string test-foo-v2-json)
                      (string-length test-foo-v2-json)))
             ("https://rubygems.org/api/v1/gems/bar.json"
              (values (open-input-string test-bar-json)
                      (string-length test-bar-json)))
             ("https://rubygems.org/api/v1/gems/bundler.json"
              (values (open-input-string test-bundler-json)
                      (string-length test-bundler-json)))
             (_ (error "Unexpected URL: " url)))))
        (match (gem-recursive-import "foo" "2.0.0")
          (`((package
               (name "ruby-bar")
               (version "1.0.0")
               (source
                (origin
                  (method url-fetch)
                  (uri (rubygems-uri "bar" version))
                  (sha256
                   (base32
                    "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
               (build-system ruby-build-system)
               (propagated-inputs (list bundler))
               (synopsis "Another cool gem")
               (description "Another cool gem")
               (home-page "https://example.com")
               (license #f))                      ;no licensing info
             (package
               (name "ruby-foo")
               (version "2.0.0")
               (source
                (origin
                  (method url-fetch)
                  (uri (rubygems-uri "foo" version))
                  (sha256
                   (base32
                    "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
               (build-system ruby-build-system)
               (propagated-inputs (list bundler ruby-bar))
               (synopsis "A cool gem")
               (description "This package provides a cool gem")
               (home-page "https://example.com")
               (license (list license:expat license:asl2.0))))
           #t)
          (x
           (pk 'fail x #f)))))

(test-equal "package-latest-release"
  (list '("https://rubygems.org/downloads/foo-1.0.0.gem")
        (list (upstream-input
               (name "bundler")
               (downstream-name name)
               (type 'propagated))
              (upstream-input
               (name "bar")
               (downstream-name "ruby-bar")
               (type 'propagated))))
  (mock ((guix http-client) http-fetch
         (lambda (url . rest)
           (match url
             ("https://rubygems.org/api/v1/gems/foo.json"
              (values (open-input-string test-foo-json)
                      (string-length test-foo-json)))
             (_ (error "Unexpected URL: " url)))))
        (let ((source (package-latest-release
                       (dummy-package "ruby-foo"
                                      (version "0.1.2")
                                      (source (dummy-origin
                                               (method url-fetch)
                                               (uri (rubygems-uri "foo"
                                                                  version))))))))
          (list (upstream-source-urls source)
                (upstream-source-inputs source)))))

(test-end "gem")
