;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020-2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Baptiste Strazzul <bstrazzull@hotmail.fr>
;;; Copyright © 2022 SeerLite <seerlite@disroot.org>
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

(define-module (gnu packages gnuzilla)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system mozilla)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:autoload (json parser) (json->scm))

(define-public mozjs
  (package
    (name "mozjs")
    (version "102.2.0")
    (source (origin
              (method url-fetch)
              ;; TODO: Switch to IceCat source once available on ftp.gnu.org.
              (uri (string-append "https://ftp.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "1zwpgis7py1bf8p88pz3mpai6a02qrdb8ww2fa9kxxdl9b8r2k81"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'
      #:modules `((guix build cargo-utils)
                  ,@%gnu-build-system-modules)
      #:test-target "check-jstests"
      #:configure-flags
      #~(list
         ;; Disable debugging symbols to save space.
         "--disable-debug"
         "--disable-debug-symbols"
         ;; This is important because without it gjs will segfault during the
         ;; configure phase.  With jemalloc only the standalone mozjs console
         ;; will work.
         "--disable-jemalloc"
         "--enable-tests"
         "--enable-hardening"
         "--enable-optimize"
         "--enable-release"
         "--enable-readline"
         "--enable-shared-js"
         "--with-system-icu"
         "--with-system-nspr"
         "--with-system-zlib"
         "--with-intl-api")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (let ((null-hash
                     "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo\\.lock$"))
                (for-each generate-all-checksums
                          '("js" "third_party/rust")))))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; The configure script does not accept environment variables as
              ;; arguments.  It also must be run from a different directory,
              ;; but not the root directory either.
              (mkdir "run-configure-from-here")
              (chdir "run-configure-from-here")
              (setenv "SHELL" (which "sh"))
              (setenv "CONFIG_SHELL" (which "sh"))
              (setenv "AUTOCONF" (which "autoconf"))
              (apply invoke "python" "../configure.py"
                     "--enable-project=js"
                     (string-append "--prefix=" #$output)
                     configure-flags)))
          (add-before 'check 'adjust-tests
            (lambda _
              (with-directory-excursion "../js/src/tests"
                (substitute* "shell/os.js"
                  ;; FIXME: Why does the killed process have an exit status?
                  ((".*killed process should not have exitStatus.*")
                   ""))

                ;; The test suite expects a lightly patched ICU.  Disable tests
                ;; that do not work with the system version.  See
                ;; "intl/icu-patches" for clues.

                ;; See <https://unicode-org.atlassian.net/browse/ICU-20992> and
                ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=1636984> and
                ;; related patch for why this is failing.
                (delete-file "non262/Intl/DateTimeFormat/\
fractional-second-digits-append-item.js")
                ;; FIXME: got "0 \u251CAM/PM: noon\u2524", expected "0 (AM/PM: noon)"
                (delete-file "non262/Intl/DateTimeFormat/day-period-hour-cycle.js")
                ;; FIXME: got "en-US-posix", expected "en-US-POSIX".
                (delete-file "non262/Intl/available-locales-supported.js")
                ;; FIXME: got "en-US", expected "en-US-POSIX"
                (delete-file "non262/Intl/available-locales-resolved.js"))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "JSTESTS_EXTRA_ARGS"
                      (string-join
                       (list
                        ;; Do not run tests marked as "random".
                        "--exclude-random"
                        ;; Exclude web platform tests.
                        "--wpt=disabled"
                        ;; Respect the daemons configured number of jobs.
                        (string-append "--worker-count="
                                       (number->string (parallel-job-count)))))))))))
    (native-inputs
     (list autoconf
           llvm                         ;for llvm-objdump
           m4
           perl
           pkg-config
           python-wrapper
           rust
           `(,rust "cargo")))
    (inputs
     (list icu4c readline zlib))
    (propagated-inputs
     (list nspr))                ; in the Requires.private field of mozjs-*.pc
    (home-page
     "https://spidermonkey.dev/")
    (synopsis "Mozilla JavaScript engine")
    (description "SpiderMonkey is Mozilla's JavaScript engine written
in C/C++.")
    (license license:mpl2.0))) ; and others for some files

(define-public mozjs-91
  (package
    (inherit mozjs)
    (version "91.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "0qh7j960wdp5zcfqhkj8ki47spp9i9ms12xx0v0kxvmmw36jpgjk"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mozjs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'check 'disable-timezone-tests
              (lambda _
                (with-directory-excursion "../js/src/tests"
                  ;; FIXME: Assertion failed: got "2021a", expected "2021a3"?
                  (delete-file "non262/Intl/DateTimeFormat/timeZone_version.js")
                  ;; XXX: Delete all tests that test time zone functionality,
                  ;; because the test suite uses /etc/localtime to figure out
                  ;; the offset from the hardware clock, which does not work
                  ;; in the build container.  See <tests/non262/Date/shell.js>.
                  (delete-file-recursively "non262/Date")
                  (delete-file
                   "non262/Intl/DateTimeFormat/tz-environment-variable.js"))))))))
    (inputs (modify-inputs (package-inputs mozjs)
              (replace "icu4c" icu4c-69)))))

(define-public mozjs-78
  (package
    (inherit mozjs)
    (name "mozjs")
    (version "78.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.mozilla.org/pub/firefox"
                                  "/releases/" version "esr/source/firefox-"
                                  version "esr.source.tar.xz"))
              (sha256
               (base32
                "0l91cxdc5v9fps79ckb1kid4gw6v5qng1jd9zvaacwaiv628shx4"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments mozjs)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-for-python-3.10
              (lambda _
                ;; Some classes were moved from collections to collections.abc
                ;; in Python 3.10.
                (substitute* "python/mozbuild/mozbuild/util.py"
                  (("collections\\.Sequence")
                   "collections.abc.Sequence"))
                (substitute* "python/mozbuild/mozbuild/makeutil.py"
                  (("from collections import Iterable")
                   "from collections.abc import Iterable"))
                (substitute* "python/mozbuild/mozbuild/backend/configenvironment.py"
                  (("from collections import Iterable, OrderedDict")
                   "from collections import OrderedDict\n\
from collections.abc import Iterable"))
                (substitute*
                    "testing/mozbase/manifestparser/manifestparser/filters.py"
                  (("from collections import defaultdict, MutableSequence")
                   "from collections import defaultdict\n\
from collections.abc import MutableSequence"))))
            (replace 'configure
              (lambda* (#:key configure-flags #:allow-other-keys)
                ;; The configure script does not accept environment variables as
                ;; arguments.  It also must be run from a different directory,
                ;; but not the root directory either.
                (mkdir "run-configure-from-here")
                (chdir "run-configure-from-here")
                (setenv "SHELL" (which "sh"))
                (setenv "CONFIG_SHELL" (which "sh"))
                (setenv "AUTOCONF" (which "autoconf"))
                (apply invoke "../js/src/configure"
                       (cons (string-append "--prefix=" #$output)
                             configure-flags))))
            (replace 'adjust-tests
              (lambda _
                (with-directory-excursion "../js/src/tests"
                  ;; The test suite expects a lightly patched ICU 67.  Since
                  ;; Guix is about to switch to ICU 68, massage the tests to
                  ;; work with that instead of patching ICU.  Try removing this
                  ;; phase for newer versions of mozjs.

                  ;; These tests look up locale names and expects to get
                  ;; "GB" instead of "UK".
                  (substitute* "non262/Intl/DisplayNames/language.js"
                    (("Traditionell, GB")
                     "Traditionell, UK"))
                  (substitute* "non262/Intl/DisplayNames/region.js"
                    (("\"GB\": \"GB\"")
                     "\"GB\": \"UK\""))

                  ;; XXX: Some localized time formats have changed, and
                  ;; substitution fails for accented characters, even though
                  ;; it works in the REPL(?).  Just delete these for now.
                  (delete-file "non262/Intl/Date/toLocaleString_timeZone.js")
                  (delete-file "non262/Intl/Date/toLocaleDateString_timeZone.js")

                  ;; Similarly, these get an unexpected "A" suffix when looking
                  ;; up a time in the "ar-MA-u-ca-islamicc" locale, which is
                  ;; tricky to substitute.
                  (delete-file "non262/Intl/DateTimeFormat/format_timeZone.js")
                  (delete-file "non262/Intl/DateTimeFormat/format.js")

                  ;; This file compares a generated list of ICU locale names
                  ;; with actual lookups.  Some have changed slightly, i.e.
                  ;; daf-Latn-ZZ -> daf-Latn-CI, so drop it for simplicity.
                  (delete-file "non262/Intl/Locale/likely-subtags-generated.js"))))
            (replace 'pre-check
              (lambda _
                (with-directory-excursion "../js/src/tests"
                  (substitute* "shell/os.js"
                    ;; FIXME: Why does the killed process have an exit status?
                    ((".*killed process should not have exitStatus.*")
                     ""))

                  ;; XXX: Delete all tests that test time zone functionality,
                  ;; because the test suite uses /etc/localtime to figure out
                  ;; the offset from the hardware clock, which does not work
                  ;; in the build container.  See <tests/non262/Date/shell.js>.
                  (delete-file-recursively "non262/Date")
                  (delete-file "non262/Intl/DateTimeFormat/tz-environment-variable.js")

                  (setenv "JSTESTS_EXTRA_ARGS"
                          (string-join
                           (list
                            ;; Do not run tests marked as "random".
                            "--exclude-random"
                            ;; Exclude web platform tests.
                            "--wpt=disabled"
                            ;; Respect the daemons configured number of jobs.
                            (string-append "--worker-count="
                                           (number->string
                                            (parallel-job-count)))))))))))))
    (native-inputs
     (list autoconf-2.13
           automake
           llvm                         ;for llvm-objdump
           perl
           pkg-config
           python-3
           rust
           `(,rust "cargo")))
    (inputs
     (list icu4c-69 readline zlib))))


;;;
;;; Localization helper procedures.
;;;
(define mozilla-compare-locales
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/l10n/compare-locales/")
          (changeset "RELEASE_8_1_0")))
    (file-name "mozilla-compare-locales")
    (sha256 (base32 "00bpkaqf2ng1nn9ajyb5mli0jq58q5fm2n3yy90jy0hp4q2gbs50"))))

(define (mozilla-locale locale changeset hash-string)
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url (string-append "https://hg.mozilla.org/l10n-central/"
                              locale))
          (changeset changeset)))
    (file-name (string-append "mozilla-locale-" locale))
    (sha256 (base32 hash-string))))

(define-syntax-rule (mozilla-locales (hash-string changeset locale) ...)
  (list (mozilla-locale locale changeset hash-string)
        ...))

(define (update-mozilla-locales changesets.json)
  "Output a new list of Mozilla locales, to update the ALL-MOZILLA-LOCALES
variable defined below.  It requires guile-json to be installed."
  (match (call-with-input-file changesets.json json->scm)
    (((lang ("revision" . revision) platforms pin) ...)
     (let ((data (reverse (map (lambda (rev lang)
                                 `(,(list->string (make-list 40 #\0))
                                   ,(string-take rev 12) ,lang))
                               revision lang))))
       (format #t "~{~s~%~}" data)
       data))))

(define all-mozilla-locales
  (mozilla-locales
   ;;                      sha256                            changeset    locale
   ;;---------------------------------------------------------------------------
   ("1s59ihmj8x6z0ssq4xav689jb5azrpdnay8csgjm1b9pw7wmvcli" "a6940ae1a02f" "ach")
   ("1cqixlk9f8p63jz20wzsvnfb7xa82ba725gzdydlwz2axgp09c26" "4e2c7d1ddbed" "af")
   ("19r1yhmfxqasyslc8gr9as5w1scscz1xr8iqy9zi4b90fdjzs0ac" "06897e40a7ea" "an")
   ("0nfknb1p03j9fgmkwlm1mzdyh10g0l33x34ab39kc072apziyv0n" "9272819b09e2" "ar")
   ("11qqblqfffbmkdr5b6mxzq02i8rj1hjq3iy6xv5i5xxy311b3vpb" "f706d22e6910" "ast")
   ("0q2p1a437qr2nshdd4934qkv2sblmykiwzir149c8p9m5sjk0dyw" "f5c2a9800add" "az")
   ("0gxxm3lv18kj0922bw0g897azc4nkrszm5irgwdkgmkbyr97li83" "98ba7d51484f" "be")
   ("10vrbbaabjns824ndya6c09znm815pak2xpvqgjydl3r5qvacx65" "5c79c77311cd" "bg")
   ("1il7yyifx5xzj0v2lna315mppl5pk1vi4m9r66hdz1lzc485m1wp" "c80c87ef7193" "bn")
   ("1p1ig4v9pczzrnnxzr20r61g7xk85k5lgnxhzgn1rx518p85xrkm" "6a1bcb9851b2" "br")
   ("08q33bk9kdvbyc4ib58bsn2b67ki3d2yzskkf5r2n5zlglblksa2" "939779cb58d6" "bs")
   ("1bdkywrqah85fh8kfnz163qnc02ffx0a4vlnx5pq1wg9j4f1gznf" "9a45ccf144f1" "ca")
   ("0hhmp5dzc0rssykl0b2n9h0vfy4svwhxmhpsr3rcfpbg2s0r5h6l" "4f60e18fc248" "ca-valencia")
   ("18phbl9raqsbys9wa8z0gq0h0pw3b55v6ngn67r4373lfc0s9cxv" "b4ef404c7de8" "cak")
   ("0147qyw1y97mgqhkb46wblxv61lq2mvvm5n5ihwf49l5cyizi0jg" "f56ef18f05df" "cs")
   ("08sbhnsxndlsaijnxndc367qcbrzb29m7bpkcawinz9fg6mz3573" "4f9d92f04f5e" "cy")
   ("09cm5kk1sh2a6ws1fj185jrknhfa6n4bhj7nfnr4lsk339n0kvv9" "902503567e30" "da")
   ("0r630bki5d4ylicq6lzh6s2mvsq225933szswmm0sap507rj75cm" "6000baf7a412" "de")
   ("0749qjsfv3rn94rnci3ydgndw27qlr3w3w97kfwma2gmsf3m3r0p" "4a948952d1f4" "dsb")
   ("0yc64i7mpxhs4qlyyh59i2aiz0vpap0bn8slcsk8b71wd1d7cy5m" "153a16a13733" "el")
   ("0d4m5ji6ii10yap8y24cxx3fr60ba1jqi10hy3j1cq6ni7hzavga" "7ce17ae529ac" "en-CA")
   ("12jzqcfbgdhfm8f2gqp15bdnin62li07jwicjc8ynn4cl9a19xiz" "a25d9eea7c23" "en-GB")
   ("0gbb8hfc5yvjah63mapjxm53wxrf98srbrbg43b9g734zfi4n8y5" "4ed3471dad5d" "eo")
   ("19lw7zmqx2irjmv5y6r7nncp6nysn06abf10n3dzj5nzgays6sln" "853fe7acb415" "es-AR")
   ("0rq4pa380b77rkv6dq7sqx8a8i33jk3vs5rgcl8fv7slqvsw23kd" "921b67bf27a5" "es-CL")
   ("1dyxzab9hln5spch66wmfagfzmfdiaxgpq7qlzvdfg98wkqdj25n" "c9a210ea496c" "es-ES")
   ("1gwpmfl37wxl7rynqswgj41liljflgxydvljd4zdhk3jzqn605fb" "ddd35183d81c" "es-MX")
   ("0c3blniddklapifnjbz03f2frw24jjgwgwn6swl5mwm2z0y6wa9f" "82d23ffaa7d3" "et")
   ("05mq2df6n6fr8m5m2xwl0f6n1q3zgjrnk465ldx1nfr9agrhd36c" "13975626d549" "eu")
   ("1l1jyqqiy551903j6yzh9ayg1pf26q2hr9h3jj4l51xzp7x4ms2q" "039e1fdb7c71" "fa")
   ("091l05y9sggxznv0y11b9zy5qf146p0hb5faw4ix7yn5p5kca2f5" "7bd3722d82de" "ff")
   ("1lllwjvmbl5dx44fcvsqg08fbflkc8dx5as9n6nf4xlkzydx6izm" "39808e88c9d1" "fi")
   ("10ha955vvyf5vbciricm72kplj9j0s00g2333lmg46jiwi7aviiv" "426d373db6a7" "fr")
   ("11zdfk8jvdy1k9z1q162cwapplcbz35l3dq4mv45brdin3v0x8kr" "96cd93d18389" "fy-NL")
   ("1l5xr25gmssyachwmfprlnp2v2xj4b0hp8gxrf7fi5bvv9c2fynb" "de3daf7d3f9d" "ga-IE")
   ("06h9ijfsn1cgz5fvxxbisk7fvqhgsw99id92575hgyg1p8xb1n69" "f04aea656d9e" "gd")
   ("19913i5n8yyfhz9j21krlk7wqsyy89vggxc1m1swv94y2ix8rpaj" "1b8cdb87bf69" "gl")
   ("0k5x31bfl2l0r9b5dkn03frm1gd8n6igai7ch2f1kj0skflynwww" "82df570c4241" "gn")
   ("03rygnj9xhfvykark7dls117kwhid13ghvrsqiial0vp8m7ymx79" "e2e41d7beaa5" "gu-IN")
   ("0vyraplj1x7b5im15anhzdaq03k2gqfyybgklrd4f9ry6msh5jhx" "de724e943805" "he")
   ("1zqps586jrgxpf7xx2l3qw3ch3fr31r9a25y861fpxrpvh7ygw7g" "898598e1a5c6" "hi-IN")
   ("0czq68l3qdhdc0mgazlrh8x83a6d5qrjwgjv8xvsmmzi7h68ww0l" "2711d1515af0" "hr")
   ("1wwvrnm38gk2rnw4vmcranh8hk61gjcj9292pck3ckiv6mq7kd4s" "03e02f3d0676" "hsb")
   ("0yvgdxlbyhhg3lk43hxx9gx66rcm7gaj02zzzx5zni8dhvlk2v6m" "d423ada9dc00" "hu")
   ("1kjm7bg4990jhfbxwc38r4lqm2fbcxc82nc2d4qkqwikr4aah4b9" "06836af0bd6e" "hy-AM")
   ("1dla7r3snvagb7m985xxqyi7gl498cr5xsz8msw0zpg6xmi05rlx" "299bd950d538" "ia")
   ("0w8w9xn93akir7nqcp5iwr3kqvba5gbirg7gmzlzg7mgrhw8pcsa" "dee087477b99" "id")
   ("10iakv1c1d20ihxg1s7c3zc1zfw18vr2kspcw7bk5k02rmrffgcn" "320095d063ed" "is")
   ("1xn5pa3rc7l6k2migm3c0dx71q1hk7mjls045xpv9cy8gvirnj94" "4722680fb5bf" "it")
   ("0va9zfj3wqh1yvbgh3x808yvdyxkg780vazjg887wbxpfk1n6hxa" "cb3cfe294621" "ja")
   ("0qvjc3fhk6jg2c3g6mymmnslg1rkkxmv9gi3whf2bc5mzfgyc5zw" "7efe92bd7780" "ja-JP-mac")
   ("0zfiwwz0axbd219ar32c7a6b8h816sf04ajx6jl74s5kyij79y82" "4c1fe3a18da9" "ka")
   ("1aiik4758r5df76q2a132y5fjdjrsxshjylk7irwsr7yy0c7f63g" "acdf76048aa0" "kab")
   ("0icxh4sgv6m1yykycb9d9c43k3r6w02f9c6jr04wm8hvqq5icfl5" "9b418ff7936b" "kk")
   ("1cqlhggf46lr7w399k7qb7a1hc56f32w1dy26i2racpppzza5plc" "9771ada0b5f8" "km")
   ("0p04irnb7x7y37m6lz388x9dynn8rnk000avpp296barajnhs5i8" "645aa37a2112" "kn")
   ("1lbc1fxr5i0ccymlsd8axz3633cqggg5k8ak5yqwi704g7kvv9g2" "1cd68952d119" "ko")
   ("11b55bxg73zqfdn5gy9nn5imab2mv0hxr603whggp7vymllgb7kb" "26bb83959bfe" "lij")
   ("15jsijm6d26i0105gz0f7sh2yh2v4pmw4j95cwkdrb1d8m935jlz" "b9829891f153" "lt")
   ("0liwwv13fgyw97nizhsyy53xrbf8jks5ics7qkkxfccawjrg5rlb" "e5f09d03d959" "lv")
   ("0w420yf3hdnl7dp9mn9ghc20cq79k24fn9adn3syk723ppl6mkb0" "7884845e94f3" "mk")
   ("13wkr7rgqsv9w3d9w7k8lnxdzgfixsq4bmc27zsyc770l6062ml6" "030db7412202" "mr")
   ("06nsadcnxx0ssdp67x47dj9crihn1ycgd5mwvn5m7lkb97r4ch9f" "40a7703e875c" "ms")
   ("0mlnjdzck6dp9p59ajj3sr63r36fn0qi8r9scrqrqcisgfclw9sg" "daca40056531" "my")
   ("0z1hgx9d5i9w20f9k9azzng1y3lmm5d6hdjkj7rf6r5710bhhzh5" "664bd049e105" "nb-NO")
   ("1466qvrs13g2iix1k35cknb2pia9b66li2ncvdyqf0jsd92z9k8x" "eaa6ae781ba0" "ne-NP")
   ("0jgmz2lkzj0aspd9mabygzic6li5s2b25y0r6vjg8mv3jm5pi86j" "5ef8f1c9739e" "nl")
   ("1m46x0h20vdfbzjxlz0ac43dbaq40xnyldr2yzbf7pk5di9pzxa6" "f08e15466d5d" "nn-NO")
   ("0r3zvihqh6iya3z1qm7if0m3zfgf81s9qp7x7qc701h2q357wkgf" "6712c0e12ec2" "oc")
   ("102j89jm28c223yhhkrj76xxj4kmy58hcs2r0jn15xa1qkqv1fdk" "8f36cc819e00" "pa-IN")
   ("1j9za6s0i46abn8dsrlkpck3qxxw35rhfcg1xs1vp8sc4ckg8qwi" "c3b0c1c02b94" "pl")
   ("1k9lbsvckpwl4xax8kxp5yxsfkspq2mhqm77jh5nl9aw88dsn55b" "cc32bf9630fe" "pt-BR")
   ("0f0jyvbn2sa5m66wqdzh4607g4gd0270xq37ipd9ls52b4764bd8" "5478d7242086" "pt-PT")
   ("19znkkialh1d4np7qcp80xkagrf1j2xah2s1xxzsh854p3d30zs7" "dd934a76fb01" "rm")
   ("1xn26r8g8zzkb5myfgmnjbk8k4i0brkvbrvnhp7k5nkrnsin25di" "d57ab3dee73d" "ro")
   ("04rhkxlmpp5pyzw85ldbfkmiigqr2p9y5cbns5dqs6sz1633ccxp" "7aea98f33a20" "ru")
   ("1zzkkxl7ayxh5gx9snyvp5czflsvi77hp48li613h3c2fii4n9qk" "7c986f4b5044" "sco")
   ("1219i0ghhqj3s1i0rm68jjkvivh6y40938wav7z4ifck527sq6r1" "bc9e55d4e3ea" "si")
   ("05i5p3n3a6hgl6rf10yjs4vag7y3rn2jwgxsddcdcqiv6405zn81" "8814afd7f67e" "sk")
   ("1wcqdbm1py1ldq6cj2g201m715nql79r6szr71jvrxacb711c62a" "20013dc06e96" "sl")
   ("0g2izkaa4ipwgwyhy77ciyrxxpf4pxzj9mjqvxriy5prmkhm3zjs" "d86d5d2b6eef" "son")
   ("1cc99m6srjg8698dkc3il70crby0mdv43v3ijwy069k4w50hyjjg" "49bb5fae5d9b" "sq")
   ("13kfssq4fhq9mb36as6sxiaffl17qyg1wdw8kpz3ilqm86bsjrgl" "d6a0ab79b06f" "sr")
   ("1wx9snbm0431q97i0q0nv4wbsqcv9nhllwfr88crlp7bfj5w8czw" "4bab04993da3" "sv-SE")
   ("0laglkfl8ml0ip5vmm3z2q790hgwax9gfqwq3wm68a2nnsyck8fw" "4b3316c4aa48" "szl")
   ("15pnlic4q7m48y0mc5yh8w5yscjwzx0mmfrbj3f2b9jxxa44nx4h" "48c7aab86db8" "ta")
   ("16qar5y0wns75ryi8bfv7yln3mjsyn7qi4345zcniz3ydgkczh46" "7bbb3dac8f47" "te")
   ("112g7x8h0qz16r5faam386x4a0rgwd4zy02d5agmg9m0kbficdfx" "009fd0852454" "th")
   ("1l80kh9byqxnz5vkz357rb39g4y9sgn9pr0v29ywp6d009img1qg" "dad9caecd7a9" "tl")
   ("1nalvjlxxsa9wldhhdb93bgfc7dl084xayh7i7mgg93yjgrym93x" "0c48082d37cd" "tr")
   ("0wkxqcfgsra2ljws28kiwajv73w9aa8drsc4fqlg9krbicnjl2n8" "f6d105faedcc" "trs")
   ("18jf4ysrfv3635fyxc1vwr970dr2r05bl3h8v599pwp2g7jzyjpm" "e1011388a55e" "uk")
   ("1ihmksjsz54yii23qda5iv8cxrj9144afy75hzhvfi6m182kj4h8" "c4e927eab511" "ur")
   ("0d42dhfa2vcw24wsvwf95pw20np0pz8c0is6p4307n981n8s207y" "7063df917cb3" "uz")
   ("04khnkrg8css55hyna01jqay9c2ppxk5znbx2zj9i25knhvvx1lq" "1753054e6ab8" "vi")
   ("0fi5kxn78xp7s15svkqlf4748j4pzxh941nm52n6kbbrhyi3qcqn" "93bc595dc32e" "xh")
   ("0jg676vd37wqgzjnm0yynj7xrvm6fsgdwg296h78wnyc33zc4ads" "edd4e468bc31" "zh-CN")
   ("1y4wldm3z95mfjlficp994jyqg0lj07wi35b79dy1s8ljy3jilil" "0bad1f7d2b2d" "zh-TW")))

;; XXXX: Workaround 'snippet' limitations.
(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define %icecat-base-version "102.11.0")
(define %icecat-version (string-append %icecat-base-version "-guix0-preview1"))
(define %icecat-build-id "20230509000000") ;must be of the form YYYYMMDDhhmmss

;; 'icecat-source' is a "computed" origin that generates an IceCat tarball
;; from the corresponding upstream Firefox ESR tarball, using the 'makeicecat'
;; script from the upstream IceCat project.
(define icecat-source
  (let* ((major-version (first  (string-split %icecat-base-version #\.)))
         (minor-version (second (string-split %icecat-base-version #\.)))
         (sub-version   (third  (string-split %icecat-base-version #\.)))

         (upstream-firefox-version (string-append %icecat-base-version "esr"))
         (upstream-firefox-source
          (origin
            (method url-fetch)
            (uri (string-append
                  "https://ftp.mozilla.org/pub/firefox/releases/"
                  upstream-firefox-version "/source/"
                  "firefox-" upstream-firefox-version ".source.tar.xz"))
            (sha256
             (base32
              "0cyfij00i9m1pwwyfhiq2qasbfr2ykdwziqykj8878kyjvkj1wg7"))))

         ;; The upstream-icecat-base-version may be older than the
         ;; %icecat-base-version.
         (upstream-icecat-base-version "102.11.0")
         (gnuzilla-commit "5a1894fc966840dc242110ec0459471c637a4675")
         (gnuzilla-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.savannah.gnu.org/gnuzilla.git")
                  (commit gnuzilla-commit)))
            (file-name (git-file-name "gnuzilla"
                                      ;;upstream-icecat-base-version
                                      (string-take gnuzilla-commit 8)))
            (sha256
             (base32
              "1816lm4cl45l1zbc043827rfnzzzxckhxk16gvj3a8kyg7fxvv3v"))))

         ;; 'search-patch' returns either a valid file name or #f, so wrap it
         ;; in 'assume-valid-file-name' to avoid 'local-file' warnings.
         (makeicecat-patch
          (local-file (assume-valid-file-name
                       (search-patch "icecat-makeicecat.patch")))))

    (origin
      (method computed-origin-method)
      (file-name (string-append "icecat-" %icecat-version ".tar.xz"))
      (sha256 #f)
      (uri
       (delay
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))
              (let ((firefox-dir
                     (string-append "firefox-" #$%icecat-base-version))
                    (icecat-dir
                     (string-append "icecat-" #$%icecat-version)))

                (set-path-environment-variable
                 "PATH" '("bin")
                 (list #+python
                       #+(canonical-package bash)
                       #+(canonical-package coreutils)
                       #+(canonical-package findutils)
                       #+(canonical-package patch)
                       #+(canonical-package xz)
                       #+(canonical-package sed)
                       #+(canonical-package grep)
                       #+(canonical-package bzip2)
                       #+(canonical-package gzip)
                       #+(canonical-package tar)))

                (set-path-environment-variable
                 "PYTHONPATH"
                 (list #+(format #f "lib/python~a/site-packages"
                                 (version-major+minor
                                  (package-version python))))
                 '#+(cons python-jsonschema
                          (map second
                               (package-transitive-propagated-inputs
                                python-jsonschema))))

                ;; We copy the gnuzilla source directory because it is
                ;; read-only in 'gnuzilla-source', and the makeicecat script
                ;; uses "cp -a" to copy parts of it and assumes that the
                ;; copies will be writable.
                (copy-recursively #+gnuzilla-source "/tmp/gnuzilla"
                                  #:log (%make-void-port "w"))

                (with-directory-excursion "/tmp/gnuzilla"
                  (make-file-writable "makeicecat")
                  (invoke "patch" "--force" "--no-backup-if-mismatch"
                          "-p1" "--input" #+makeicecat-patch)
                  (patch-shebang "makeicecat")
                  (substitute* "makeicecat"
                    (("^readonly FFMAJOR=(.*)" all ffmajor)
                     (unless (string=? #$major-version
                                       (string-trim-both ffmajor))
                       ;; The makeicecat script cannot be expected to work
                       ;; properly on a different version of Firefox, even if
                       ;; no errors occur during execution.
                       (error "makeicecat major version mismatch"))
                     (string-append "readonly FFMAJOR=" #$major-version "\n"))
                    (("^readonly FFMINOR=.*")
                     (string-append "readonly FFMINOR=" #$minor-version "\n"))
                    (("^readonly FFSUB=.*")
                     (string-append "readonly FFSUB=" #$sub-version "\n"))
                    (("^readonly DATADIR=.*")
                     "readonly DATADIR=/tmp/gnuzilla/data\n")
                    (("^readonly SOURCEDIR=.*")
                     (string-append "readonly SOURCEDIR=" icecat-dir "\n"))
                    (("/bin/sed")
                     #+(file-append (canonical-package sed) "/bin/sed"))))

                (format #t "Unpacking upstream firefox tarball...~%")
                (force-output)
                (invoke "tar" "xf" #+upstream-firefox-source)
                (rename-file firefox-dir icecat-dir)

                (with-directory-excursion icecat-dir
                  (format #t "Populating l10n directory...~%")
                  (force-output)
                  (mkdir "l10n")
                  (with-directory-excursion "l10n"
                    (for-each
                     (lambda (locale-dir)
                       (let ((locale
                              (string-drop (basename locale-dir)
                                           (+ 32  ; length of hash
                                              (string-length "-mozilla-locale-")))))
                         (format #t "  ~a~%" locale)
                         (force-output)
                         (copy-recursively locale-dir locale
                                           #:log (%make-void-port "w"))
                         (for-each make-file-writable (find-files locale))
                         (with-directory-excursion locale
                           (when (file-exists? ".hgtags")
                             (delete-file ".hgtags"))
                           (mkdir-p "browser/chrome/browser/preferences")
                           (call-with-output-file
                               "browser/chrome/browser/preferences/advanced-scripts.dtd"
                             (lambda (port) #f)))))
                     '#+all-mozilla-locales)
                    (copy-recursively #+mozilla-compare-locales
                                      "compare-locales"
                                      #:log (%make-void-port "w"))
                    (delete-file "compare-locales/.gitignore")
                    (delete-file "compare-locales/.hgignore")
                    (delete-file "compare-locales/.hgtags")))

                (format #t "Running makeicecat script...~%")
                (force-output)
                (invoke "bash" "/tmp/gnuzilla/makeicecat")

                (format #t "Packing IceCat source tarball...~%")
                (force-output)
                (setenv "XZ_DEFAULTS" (string-join (%xz-parallel-args)))
                (invoke "tar" "cfa" #$output
                        ;; Avoid non-determinism in the archive.  We set the
                        ;; mtime of files in the archive to early 1980 because
                        ;; the build process fails if the mtime of source
                        ;; files is pre-1980, due to the creation of zip
                        ;; archives.
                        "--mtime=@315619200" ; 1980-01-02 UTC
                        "--owner=root:0"
                        "--group=root:0"
                        "--sort=name"
                        icecat-dir)))))))))

(define-public icecat-minimal
  (package
    (name "icecat-minimal")
    (version %icecat-version)
    (source icecat-source)
    (build-system mozilla-build-system)
    (inputs
     (list alsa-lib
           bzip2
           cups
           dbus-glib
           gdk-pixbuf
           glib
           gtk+
           ;; UNBUNDLE-ME! graphite2
           cairo
           pango
           freetype
           font-dejavu
           ;; UNBUNDLE-ME! harfbuzz
           libcanberra
           libgnome
           libjpeg-turbo
           libpng-apng
           ;; UNBUNDLE-ME! libogg
           ;; UNBUNDLE-ME! libtheora ; wants theora-1.2, not yet released
           ;; UNBUNDLE-ME! libvorbis
           libxft
           libevent
           libxinerama
           libxscrnsaver
           libxcomposite
           libxt
           libffi
           ;; Support for FFmpeg 6 was only added in version 112 (see:
           ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1819374).
           ffmpeg-5
           libvpx
           icu4c
           pixman
           pulseaudio
           mesa
           pciutils
           mit-krb5
           hunspell
           libnotify
           nspr
           nss
           shared-mime-info
           sqlite
           eudev
           unzip
           zip
           zlib))
    (native-inputs
     ;; The following patches are specific to the Guix packaging of IceCat,
     ;; and therefore we prefer to leave them out of 'source', which should be
     ;; a tarball suitable for compilation on any system that IceCat supports.
     ;; (Bug fixes and security fixes, however, should go in 'source').
     (list
      ;; XXX TODO: Adapt these patches to IceCat 102.
      ;; ("icecat-avoid-bundled-libraries.patch"
      ;;  ,(search-patch "icecat-avoid-bundled-libraries.patch"))
      ;; ("icecat-use-system-graphite2+harfbuzz.patch"
      ;;  ,(search-patch "icecat-use-system-graphite2+harfbuzz.patch"))
      ;; ("icecat-use-system-media-libs.patch"
      ;;  ,(search-patch "icecat-use-system-media-libs.patch"))
      rust
      `(,rust "cargo")
      rust-cbindgen-0.23
      llvm
      clang
      perl
      node
      python-wrapper
      yasm
      nasm                         ; XXX FIXME: only needed on x86_64 and i686
      pkg-config
      m4
      which))
    (arguments
     (list
      #:tests? #f                       ;not worth the cost

      ;; Some dynamic lib was determined at runtime, so rpath check may fail.
      #:validate-runpath? #f

      #:configure-flags
      #~(list
         "--enable-application=browser"
         "--with-distribution-id=org.gnu"
         "--enable-geckodriver"
         ;; Do not require addons in the global app or system directories to
         ;; be signed by Mozilla.
         "--with-unsigned-addon-scopes=app,system"
         "--allow-addon-sideload"

         "--enable-pulseaudio"

         "--disable-tests"
         "--disable-updater"
         "--disable-crashreporter"
         ;; The --disable-eme option is not available on aarch64.
         #$(if (target-aarch64?) "" "--disable-eme")

         ;; Building with debugging symbols takes ~5GiB, so disable it.
         "--disable-debug"
         "--disable-debug-symbols"

         "--enable-rust-simd"
         "--enable-release"
         "--enable-optimize"
         "--enable-strip"
         "--disable-elf-hack"

         ;; Clang is needed to build Stylo, Mozilla's new CSS engine.  We must
         ;; specify the clang paths manually, because otherwise the Mozilla
         ;; build system looks in the directories returned by llvm-config
         ;; --bindir and llvm-config --libdir, which return paths in the llvm
         ;; package where clang is not found.
         (string-append "--with-clang-path="
                        (search-input-file %build-inputs "bin/clang"))
         (string-append "--with-libclang-path="
                        (dirname (search-input-file %build-inputs
                                                    "lib/libclang.so")))

         ;; Hack to work around missing "unofficial" branding in icecat.
         "--enable-official-branding"

         ;; TODO: Add support for wasm sandboxed libraries.
         "--without-wasm-sandboxed-libraries"

         ;; Avoid bundled libraries.
         "--with-system-jpeg"           ;must be libjpeg-turbo
         "--with-system-png"            ;must be libpng-apng
         "--with-system-zlib"
         ;; UNBUNDLE-ME! "--with-system-bz2"
         ;; UNBUNDLE-ME! "--with-system-libevent"
         ;; UNBUNDLE-ME! "--with-system-ogg"
         ;; UNBUNDLE-ME! "--with-system-vorbis"
         ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
         ;; UNBUNDLE-ME! "--with-system-libvpx"
         "--with-system-icu"
         "--with-system-nspr"
         "--with-system-nss"

         ;; UNBUNDLE-ME! "--with-system-harfbuzz"
         ;; UNBUNDLE-ME! "--with-system-graphite2"
         "--enable-system-pixman"
         "--enable-system-ffi"
         ;; UNBUNDLE-ME! "--enable-system-sqlite"
         )

      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'

      #:modules `((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'apply-guix-specific-patches
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (let ((patch (search-input-file inputs "bin/patch")))
                (for-each (match-lambda
                            ((label . file)
                             (when (and (string-prefix? "icecat-" label)
                                        (string-suffix? ".patch" label))
                               (format #t "applying '~a'...~%" file)
                               (invoke patch "--force" "--no-backup-if-mismatch"
                                       "-p1" "--input" file))))
                          (or native-inputs inputs)))))
          (add-after 'apply-guix-specific-patches 'remove-bundled-libraries
            (lambda _
              ;; Remove bundled libraries that we don't use, since they may
              ;; contain unpatched security flaws, they waste disk space and
              ;; memory, and may cause confusion.
              (for-each (lambda (file)
                          (format #t "deleting '~a'...~%" file)
                          (delete-file-recursively file))
                        '( ;; FIXME: Removing the bundled icu breaks configure.
                          ;;   * The bundled icu headers are used in some places.
                          ;;   * The version number is taken from the bundled copy.
                          ;;"intl/icu"
                          ;;
                          ;; FIXME: A script from the bundled nspr is used.
                          ;;"nsprpub"
                          ;;
                          ;; FIXME: Some of the bundled NSS sources are used
                          ;; to build third_party/prio.
                          ;;"security/nss"
                          ;;
                          ;; TODO: Use more system media libraries.  See:
                          ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                          ;;   * libtheora: esr60 wants v1.2, not yet released.
                          ;;   * soundtouch: avoiding the bundled library would
                          ;;     result in some loss of functionality.  There's
                          ;;     also an issue with exception handling
                          ;;     configuration.  It seems that this is needed in
                          ;;     some moz.build:
                          ;;       DEFINES['ST_NO_EXCEPTION_HANDLING'] = 1
                          ;;   * libopus
                          ;;   * speex
                          ;;
                          "modules/freetype2"
                          ;; "media/libjpeg"  ; needed for now, because media/libjpeg/moz.build is referenced from config/external/moz.build
                          ;; UNBUNDLE-ME! "modules/zlib"
                          ;; UNBUNDLE-ME! "ipc/chromium/src/third_party/libevent"
                          ;; UNBUNDLE-ME! "media/libvpx"
                          ;; UNBUNDLE-ME! "media/libogg"
                          ;; UNBUNDLE-ME! "media/libvorbis"
                          ;; UNBUNDLE-ME! "media/libtheora" ; wants theora-1.2, not yet released
                          ;; UNBUNDLE-ME! "media/libtremor"
                          ;; UNBUNDLE-ME! "gfx/harfbuzz"
                          ;; UNBUNDLE-ME! "gfx/graphite2"
                          "js/src/ctypes/libffi"
                          ;; UNBUNDLE-ME! "db/sqlite3"
                          ))))
          (add-after 'remove-bundled-libraries 'fix-ffmpeg-runtime-linker
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Arrange to load libavcodec.so by its absolute file name.
              (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                (("libavcodec\\.so")
                 (search-input-file inputs "lib/libavcodec.so")))))
          (add-after 'fix-ffmpeg-runtime-linker 'build-sandbox-whitelist
            (lambda* (#:key inputs #:allow-other-keys)
              (define (runpath-of lib)
                (call-with-input-file lib
                  (compose elf-dynamic-info-runpath
                           elf-dynamic-info
                           parse-elf
                           get-bytevector-all)))
              (define (runpaths-of-input label)
                (let* ((dir (string-append (assoc-ref inputs label) "/lib"))
                       (libs (find-files dir "\\.so$")))
                  (append-map runpath-of libs)))
              ;; Populate the sandbox read-path whitelist as needed by ffmpeg.
              (let* ((whitelist
                      (map (cut string-append <> "/")
                           (delete-duplicates
                            `(,(string-append (assoc-ref inputs "shared-mime-info")
                                              "/share/mime")
                              ,(string-append (assoc-ref inputs "font-dejavu")
                                              "/share/fonts")
                              "/run/current-system/profile/share/fonts"
                              ,@(append-map runpaths-of-input
                                            '("mesa" "ffmpeg"))))))
                     (whitelist-string (string-join whitelist ","))
                     (port (open-file "browser/app/profile/icecat.js" "a")))
                (format #t "setting 'security.sandbox.content.read_path_whitelist' to '~a'~%"
                        whitelist-string)
                (format port "~%pref(\"security.sandbox.content.read_path_whitelist\", ~S);~%"
                        whitelist-string)
                (close-output-port port))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo.lock$"))
                (for-each generate-all-checksums
                          '("services"
                            "js"
                            "third_party/rust"
                            "dom/media"
                            "dom/webauthn"
                            "toolkit"
                            "gfx"
                            "storage"
                            "modules"
                            "xpcom/rust"
                            "media"
                            "mozglue/static/rust"
                            "netwerk"
                            "remote"
                            "intl"
                            "servo"
                            "security/manager/ssl"
                            "build")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invokation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("\"--frozen\",") ""))))
          (delete 'bootstrap)
          (replace 'configure
            ;; configure does not work followed by both "SHELL=..." and
            ;; "CONFIG_SHELL=..."; set environment variables instead
            (lambda* (#:key outputs configure-flags #:allow-other-keys)
              (let* ((bash (which "bash"))
                     (abs-srcdir (getcwd))
                     (flags `(,(string-append "--prefix=" #$output)
                              ,(string-append "--with-l10n-base="
                                              abs-srcdir "/l10n")
                              ,@configure-flags)))
                (setenv "SHELL" bash)
                (setenv "CONFIG_SHELL" bash)

                (setenv "AR" "llvm-ar")
                (setenv "NM" "llvm-nm")
                (setenv "CC" "clang")
                (setenv "CXX" "clang++")
                (setenv "LDFLAGS" (string-append "-Wl,-rpath="
                                                 #$output "/lib/icecat"))

                (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                (setenv "MOZ_BUILD_DATE" #$%icecat-build-id) ; avoid timestamp

                ;; XXX TODO: Fix this to work on systems other than x86_64-linux.
                (setenv "GUIX_PYTHONPATH"
                        (string-append (getcwd)
                                       "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))

                (mkdir ".mozbuild")
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/.mozbuild"))

                (format #t "build directory: ~s~%" (getcwd))
                (format #t "configure flags: ~s~%" flags)

                (call-with-output-file "mozconfig"
                  (lambda (port)
                    (for-each (lambda (flag)
                                (format port "ac_add_options ~a\n" flag))
                              flags)))

                (invoke "./mach" "configure"))))
          (replace 'build
            (lambda* (#:key (make-flags '()) (parallel-build? #t)
                      #:allow-other-keys)
              (apply invoke "./mach" "build"
                     ;; mach will use parallel build if possible by default
                     `(,@(if parallel-build?
                             '()
                             '("-j1"))
                       ,@make-flags))))
          (add-after 'build 'neutralise-store-references
            (lambda _
              ;; Mangle the store references to compilers & other build tools in
              ;; about:buildconfig, reducing IceCat's closure by 1 GiB on x86-64.
              (let* ((obj-dir (match (scandir "." (cut string-prefix? "obj-" <>))
                                ((dir) dir)))
                     (file (string-append
                            obj-dir
                            "/dist/bin/chrome/toolkit/content/global/buildconfig.html")))
                (substitute* file
                  (("[0-9a-df-np-sv-z]{32}" hash)
                   (string-append (string-take hash 8)
                                  "<!-- Guix: not a runtime dependency -->"
                                  (string-drop hash 8)))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "./mach" "install")
              ;; The geckodriver binary is not installed by the above, for some
              ;; reason.  Use 'find-files' to avoid having to deal with the
              ;; system/architecture-specific file name.
              (install-file (first (find-files "." "geckodriver"))
                            (string-append #$output "/bin"))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (ld-libs '#$(map (lambda (label)
                                        (file-append (this-package-input label) "/lib"))
                                      '("libpng-apng"
                                        "libxscrnsaver"
                                        "mesa"
                                        "pciutils"
                                        "mit-krb5"
                                        "eudev"
                                        "pulseaudio"
                                        ;; For the integration of native notifications
                                        ;; (same reason as icedove)
                                        "libnotify"))))
                (wrap-program (car (find-files lib "^icecat$"))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  ;; The following line is commented out because the icecat
                  ;; package on guix has been observed to be unstable when
                  ;; using wayland, and the bundled extensions stop working.
                  ;;   `("MOZ_ENABLE_WAYLAND" = ("1"))
                  `("LD_LIBRARY_PATH" prefix ,ld-libs)))))
          (add-after 'wrap-program 'install-desktop-entry
            (lambda _
              ;; Install the '.desktop' file.
              (let* ((desktop-file "taskcluster/docker/icecat-snap/icecat.desktop")
                     (applications (string-append #$output "/share/applications")))
                (substitute* desktop-file
                  (("^Exec=icecat")     (string-append "Exec=" #$output "/bin/icecat"))
                  (("IceCat")           "GNU IceCat")
                  (("Icon=.*")          "Icon=icecat\n")
                  (("NewWindow")        "new-window")
                  (("NewPrivateWindow") "new-private-window")
                  (("StartupNotify=true")
                   "StartupNotify=true\nStartupWMClass=Navigator"))
                (install-file desktop-file applications))))
          (add-after 'install-desktop-entry 'install-icons
            (lambda _
              (with-directory-excursion "browser/branding/official"
                (for-each
                 (lambda (file)
                   (let* ((size (string-filter char-numeric? file))
                          (icons (string-append #$output "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                     (mkdir-p icons)
                     (copy-file file (string-append icons "/icecat.png"))))
                 '("default16.png" "default22.png" "default24.png"
                   "default32.png" "default48.png" "content/icon64.png"
                   "mozicon128.png" "default256.png"))))))))
    (home-page "https://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.  This package also includes the
@command{geckodriver} command, which can be useful for automated web
testing.

WARNING: IceCat 102 has not yet been released by the upstream IceCat project.
This is a preview release, and does not currently meet the privacy-respecting
standards of the IceCat project.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))

(define %icecat-locales
  '("ach" "af" "an" "ar" "ast" "az" "be" "bg" "bn" "br" "bs" "ca" "cak"
    "ca-valencia" "cs" "cy" "da" "de" "dsb" "el" "en-CA" "en-GB" "eo" "es-AR"
    "es-CL" "es-ES" "es-MX" "et" "eu" "fa" "ff" "fi" "fr" "fy-NL" "ga-IE" "gd"
    "gl" "gn" "gu-IN" "he" "hi-IN" "hr" "hsb" "hu" "hy-AM" "ia" "id" "is" "it"
    "ja" "ja-JP-mac" "ka" "kab" "kk" "km" "kn" "ko" "lij" "lt" "lv" "mk" "mr" "ms"
    "my" "nb-NO" "ne-NP" "nl" "nn-NO" "oc" "pa-IN" "pl" "pt-BR" "pt-PT" "rm" "ro"
    "ru" "sco" "si" "sk" "sl" "son" "sq" "sr" "sv-SE" "szl" "ta" "te" "th" "tl"
    "tr" "trs" "uk" "ur" "uz" "vi" "xh" "zh-CN" "zh-TW"))

(define %icedove-build-id "20230527000000") ;must be of the form YYYYMMDDhhmmss
(define %icedove-version "102.11.2")

;; Provides the "comm" folder which is inserted into the icecat source.
;; Avoids the duplication of Icecat's source tarball.
(define thunderbird-comm-source
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url "https://hg.mozilla.org/releases/comm-esr102")
          (changeset "c406f857789ca8a8943b895f4cadfc61921860ab")))
    (file-name (string-append "thunderbird-" %icedove-version "-checkout"))
    (sha256
     (base32
      "1id7wlj16zbgxyxx672vls7h3sq1153grrfqgw71y2ps6zwzxg67"))))

(define (comm-source->locales+changeset source)
  "Given SOURCE, a checkout of the Thunderbird 'comm' component, return the
list of languages supported as well as the currently used changeset."
  (match (update-mozilla-locales
          (string-append source "/mail/locales/l10n-changesets.json"))
    (((_ changeset locale) ...)
     (values locale (first changeset)))))

;;; Generated with comm-source->locales+changeset.
(define %icedove-locales
  '("af" "ar" "ast" "be" "bg" "br" "ca" "cak" "cs" "cy" "da" "de" "dsb" "el"
    "en-CA" "en-GB" "es-AR" "es-ES" "es-MX" "et" "eu" "fi" "fr" "fy-NL" "ga-IE"
    "gd" "gl" "he" "hr" "hsb" "hu" "hy-AM" "id" "is" "it" "ja" "ja-JP-mac" "ka"
    "kab" "kk" "ko" "lt" "lv" "ms" "nb-NO" "nl" "nn-NO" "pa-IN" "pl" "pt-BR"
    "pt-PT" "rm" "ro" "ru" "sk" "sl" "sq" "sr" "sv-SE" "th" "tr" "uk" "uz" "vi"
    "zh-CN" "zh-TW"))

;;; To find out which changeset to use for the comm-l10n repo, use the
;;; 'comm-source->locales+changeset' procedure on the thunderbird-comm-source
;;; checkout directory.  The complete localization data should be released as
;;; a tarball in the next release (see:
;;; https://bugzilla.mozilla.org/show_bug.cgi?id=1817086).  When this tarball
;;; is available, it should replace the complete 'l10n' directory at the root
;;; of the IceCat source, instead of only the 'calendar', chat and mail
;;; directories that it provides.
(define thunderbird-comm-l10n
  (let* ((changeset "95b46b8428d5")
         (version (git-version %icedove-version "0" changeset)))
    (origin
      (method hg-fetch)
      (uri (hg-reference
            (url "https://hg.mozilla.org/projects/comm-l10n")
            (changeset changeset)))
      (file-name (git-file-name "comm-l10n" version))
      (sha256
       (base32
        "0hfsiv9p7s2ik6648gm1774d187vlm1i1c9xwyd8g8ihk2dzyn5i")))))

(define icedove-source
  (let ((name (string-append "icedove-" %icedove-version)))
    (origin
      (method computed-origin-method)
      (file-name (string-append name ".tar.xz"))
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules (source-module-closure '((guix build utils)))
           #~(begin
               (use-modules (guix build utils)
                            (sxml simple))

               (set-path-environment-variable
                "PATH" '("bin")
                (list #+(canonical-package tar)
                      #+(canonical-package xz)))

               ;; Extract the base Icecat tarball, renaming its top-level
               ;; directory.
               (invoke "tar" "--transform" (string-append "s,[^/]*," #$name ",")
                       "-xf" #$icecat-source)
               (chdir #$name)

               ;; Merge the Thunderdbird localization data.
               (copy-recursively #$thunderbird-comm-l10n "l10n")

               ;; Add the Thunderbird-specific "comm" directory..
               (mkdir "comm")
               (copy-recursively #$thunderbird-comm-source "comm")
               (delete-file "sourcestamp.txt")

               ;; Adjust the application name.
               (substitute* "comm/mail/confvars.sh"
                 (("MOZ_APP_NAME=thunderbird")
                  "MOZ_APP_NAME=icedove")
                 (("MOZ_UPDATER=1")
                  "MOZ_UPDATER=0"))

               ;; Remove branding to comply with Mozilla's trademark policy
               (with-directory-excursion "comm/mail/branding/nightly"
                 (delete-file "content/about-wordmark.svg")
                 (call-with-output-file "content/about-wordmark.svg"
                   (lambda (port)
                     (sxml->xml '(svg (@ (xmlns "http://www.w3.org/2000/svg")
                                         (viewBox "0 0 789.1 90.78")
                                         (width "333")
                                         (height "48")
                                         (fill "#fff"))
                                      (text (@ (x "400") (y "70")
                                               (text-anchor "middle")
                                               (font-size "90"))
                                            "Icedove Daily"))
                                port)))
                 (substitute* '("locales/en-US/brand.properties"
                                "locales/en-US/brand.ftl"
                                "locales/en-US/brand.dtd"
                                "configure.sh")
                   (("Thunderbird") "Icedove")
                   (("mozilla.org") "guix.gnu.org")))
               ;; Remove other mentions of Thunderbird in user-visible text.
               (with-directory-excursion "comm/mail/base/content"
                 (substitute* '("overrides/app-license-name.html")
                   (("Thunderbird") "Icedove")))
               (with-directory-excursion "comm/mail/components/"
                 (substitute* '("MailGlue.jsm"
                                "extensions/schemas/addressBook.json"
                                "extensions/schemas/tabs.json"
                                "extensions/schemas/cloudFile.json"
                                "extensions/schemas/chrome_settings_overrides.json"
                                "extensions/schemas/windows.json"
                                "extensions/parent/ext-mail.js"
                                "im/messages/mail/Info.plist"
                                "enterprisepolicies/moz.build"
                                "enterprisepolicies/helpers/moz.build"
                                "enterprisepolicies/schemas/moz.build")
                   (("Thunderbird") "Icedove")))
               (substitute* '("comm/mailnews/base/prefs/content/accountUtils.js"
                              "comm/mail/base/content/customizeToolbar.js"
                              "comm/suite/components/customizeToolbar.js")
                 (("AppConstants.MOZ_APP_NAME (.)= \"thunderbird" _ e)
                  (format #f "AppConstants.MOZ_APP_NAME ~a= \"icedove" e)))

               ;; Override addon URLs and settings
               (substitute* "comm/mail/app/profile/all-thunderbird.js"
                 (("(pref\\(\"extensions.webservice.discoverURL\").*" _ m)
                  (string-append m ", \"https://directory.fsf.org/wiki/Icedove\");"))
                 (("(pref\\(\"extensions.getAddons.search.url\").*" _ m)
                  (string-append m ", \"https://guix.gnu.org/packages\");"))
                 (("(pref\\(\"extensions.update.enabled\").*" _ m)
                  (string-append m ", false);"))
                 (("(pref\\(\"extensions.systemAddon.update.enabled\").*" _ m)
                  (string-append m ", false);"))
                 (("(pref\\(\"lightweightThemes.update.enabled\").*" _ m)
                  (string-append m ", false);"))

                 ;; XXX: The autoDisableScopes is tweaked by the makeicecat
                 ;; script, but it doesn't know about Thunderbird.  This is
                 ;; necessary to allow picking up the extensions found in the
                 ;; system global application directory, such as the language
                 ;; packs.
                 (("\"extensions.autoDisableScopes\", 15")
                  "\"extensions.autoDisableScopes\", 3")

                 ;; Set the default locale to that of the operating system.
                 ((".*extensions.autoDisableScopes.*" anchor)
                  (string-append anchor
                                 "pref(\"intl.locale.requested\", \"\");\n")))

               ;; Step out of the directory and create the tarball.
               (chdir "..")
               (format #t "Packing Icedove source tarball...~%")
               (force-output)
               (setenv "XZ_DEFAULTS" (string-join (%xz-parallel-args)))
               (invoke "tar" "cfa" #$output
                       "--mtime=@315619200" ;1980-01-02 UTC
                       "--owner=root:0"
                       "--group=root:0"
                       "--sort=name"
                       #$name))))))))

(define-public icedove-minimal
  (package
    (name "icedove-minimal")
    (version %icedove-version)
    (source icedove-source)
    (properties
     `((cpe-name . "thunderbird_esr")))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                             ;no check target
      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'
      #:modules `((guix build utils)          ;find-files
                  (sxml simple)
                  (ice-9 regex)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934\
ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo.lock$"))
                (for-each generate-all-checksums
                          '("third_party/rust"
                            "toolkit/library/rust")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invokation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("\"--frozen\",") ""))))
          ;; Fixes issue where each installation directory generates its own
          ;; profile (see:
          ;; https://trac.torproject.org/projects/tor/ticket/31457).
          (add-after 'patch-source-shebangs 'fix-profile-setting
            (lambda _
              (substitute* "comm/mail/moz.configure"
                (("\"MOZ_DEDICATED_PROFILES\", True")
                 "\"MOZ_DEDICATED_PROFILES\", False"))))
          (add-after 'build 'neutralize-store-references
            (lambda _
              ;; Mangle the store references to compilers & other build tools in
              ;; about:buildconfig, reducing Icedove's closure significant.
              ;; The resulting files are saved in lib/thunderbird/omni.ja
              (substitute*
                  ;; Use find because the path "obj-x86_64-pc-linux-gnu" contains
                  ;; the architecture and the system -> more complicated.
                  (find-files "." "buildconfig.html")
                (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                          (regexp-quote (%store-directory)))
                  _ store hash)
                 (string-append store
                                (string-take hash 8)
                                "<!-- Guix: not a runtime dependency -->"
                                (string-drop hash 8))))))
          (delete 'bootstrap)
          (replace 'configure
            (lambda* (#:key native-inputs inputs configure-flags
                      #:allow-other-keys)
              (let* ((bash (which "bash"))
                     (abs-srcdir (getcwd))
                     (srcdir (string-append "../" (basename abs-srcdir)))
                     (flags `(,(string-append "--prefix=" #$output)
                              ,@configure-flags))
                     (mozconfig (string-append (getcwd) "/.mozconfig")))
                (setenv "SHELL" bash)
                (setenv "CONFIG_SHELL" bash)
                (setenv "QA_CONFIGURE_OPTIONS" ".*")
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/mach_state"))
                (setenv "MOZCONFIG"
                        (string-append (getcwd) "/.mozconfig"))

                (setenv "AR" "llvm-ar")
                (setenv "NM" "llvm-nm")
                (setenv "CC" "clang")
                (setenv "CXX" "clang++")

                (setenv "MOZ_NOSPAM" "1")
                (setenv "MACH_USE_SYSTEM_PYTHON" "1")
                (setenv "PYTHON"
                        (search-input-file inputs "/bin/python"))
                (setenv "MOZ_BUILD_DATE" #$%icedove-build-id) ; avoid timestamp
                (setenv "MOZ_APP_NAME" "icedove")
                (setenv "LDFLAGS" (string-append "-Wl,-rpath=" #$output
                                                 "/lib/icedove"))
                (mkdir-p (string-append (getcwd) "/builddir"))
                (with-output-to-file mozconfig
                  (lambda ()
                    (display
                     (string-append
                      "ac_add_options --allow-addon-sideload\n"
                      "ac_add_options --with-unsigned-addon-scopes=app,system\n"
                      "ac_add_options --disable-crashreporter\n"
                      "ac_add_options --disable-debug\n"
                      "ac_add_options --disable-debug-symbols\n"
                      "ac_add_options --disable-elf-hack\n"
                      "ac_add_options --disable-jit\n"
                      "ac_add_options --disable-necko-wifi\n"
                      "ac_add_options --disable-official-branding\n"
                      "ac_add_options --disable-tests\n"
                      "ac_add_options --disable-updater\n"
                      "ac_add_options --disable-webrtc\n"
                      "ac_add_options --enable-application=comm/mail\n"
                      "ac_add_options --enable-default-toolkit=\"cairo-gtk3\"\n"
                      "ac_add_options --enable-optimize\n"
                      "ac_add_options --enable-pulseaudio\n"
                      "ac_add_options --enable-release\n"
                      "ac_add_options --enable-strip\n"
                      "ac_add_options --enable-system-ffi\n"
                      "ac_add_options --enable-system-pixman\n"
                      "ac_add_options --prefix=" #$output "\n"
                      "ac_add_options --with-clang-path="
                      (search-input-file (or native-inputs inputs)
                                         "bin/clang") "\n"
                      "ac_add_options --with-distribution-id=org.gnu\n"
                      "ac_add_options --with-libclang-path="
                      #$(this-package-native-input "clang") "/lib\n"
                      "ac_add_options --with-system-bz2\n"
                      "ac_add_options --with-system-icu\n"
                      "ac_add_options --with-system-jpeg\n"
                      "ac_add_options --with-system-libevent\n"
                      "ac_add_options --with-system-nspr\n"
                      "ac_add_options --with-system-nss\n"
                      "ac_add_options --with-system-zlib\n"
                      "ac_add_options --without-wasm-sandboxed-libraries\n"
                      "mk_add_options MOZ_MAKE_FLAGS=-j"
                      (number->string (parallel-job-count)) "\n"))))
                (display (getcwd))
                (newline)
                (display "mach configure")
                (invoke "./mach" "configure"))))
          (replace 'build
            (lambda _ (invoke "./mach" "build")))
          (replace 'install
            (lambda _ (invoke "./mach" "install")))
          ;; Thunderbird doesn't provide any .desktop file.
          ;; See https://bugzilla.mozilla.org/show_bug.cgi?id=1637575
          (add-after 'install 'install-desktop-file
            (lambda _
              (let ((apps (string-append #$output "/share/applications")))
                (mkdir-p apps)
                (with-output-to-file (string-append apps "/icedove.desktop")
                  (lambda _
                    (format #t
                            "[Desktop Entry]~@
                            Name=Icedove~@
                            Exec=~a/bin/icedove~@
                            Icon=icedove~@
                            GenericName=Mail/News Client~@
                            Categories=Network;Email;~@
                            Terminal=false~@
                            StartupNotify=true~@
                            MimeType=x-scheme-handler/mailto;~@
                            Type=Application~@
                            Actions=ComposeMessage;~@
                            [Desktop Action ComposeMessage]~@
                            Name=Write new message~@
                            Exec=~@*~a/bin/icedove -compose~%"
                            #$output))))))
          (add-after 'install-desktop-file 'install-icons
            ;; TODO: Use actual Icedove branding icons (currently the stock
            ;; Thunderbird icon is used).
            (lambda _
              (with-directory-excursion "comm/mail/branding/thunderbird"
                (for-each
                 (lambda (file)
                   (let* ((size (string-filter char-numeric? file))
                          (icons (string-append #$output "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                     (mkdir-p icons)
                     (copy-file file (string-append icons "/icedove.png"))))
                 '("default16.png" "default22.png" "default24.png"
                   "default32.png" "default48.png" "default256.png")))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib (string-append #$output "/lib"))
                     (gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (pulseaudio #$(this-package-input "pulseaudio"))
                     (pulseaudio-lib (string-append pulseaudio "/lib"))
                     (eudev #$(this-package-input "eudev"))
                     (eudev-lib (string-append eudev "/lib"))
                     ;; For the integration of native notifications (same reason as icecat)
                     (libnotify #$(this-package-input "libnotify"))
                     (libnotify-lib (string-append libnotify "/lib")))
                (wrap-program (car (find-files lib "^icedove$"))
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  `("LD_LIBRARY_PATH" prefix (,pulseaudio-lib ,eudev-lib ,libnotify-lib)))))))))
    (inputs
     (list alsa-lib
           bzip2
           cairo
           cups
           dbus-glib
           ;; Support for FFmpeg 6 was only added in version 112 (see:
           ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1819374).
           freetype
           gdk-pixbuf
           glib
           gtk+
           gtk+-2
           hunspell
           icu4c
           libcanberra
           libevent
           libffi
           libgnome
           libjpeg-turbo
           libnotify
           libpng-apng
           libvpx
           libxcomposite
           libxft
           libxinerama
           libxscrnsaver
           libxt
           mesa
           mit-krb5
           nss
           pango
           pixman
           pulseaudio
           sqlite
           startup-notification
           eudev
           unzip
           zip
           zlib))
    (native-inputs
     (list `(,rust "cargo")
           clang
           llvm
           m4
           nasm
           node
           perl
           pkg-config
           python-wrapper
           rust
           rust-cbindgen-0.23
           which
           yasm))
    (home-page "https://www.thunderbird.net")
    (synopsis "Rebranded Mozilla Thunderbird email client")
    (description
     "This package provides an email client built based on Mozilla
Thunderbird.  It supports email, news feeds, chat, calendar and contacts.")
    (license license:mpl2.0)))

(define (make-l10n-package project version source locales)
  "Return a package for PROJECT, a symbol (either icecat or icedove), with
their corresponding VERSION, SOURCE and LOCALES variables."
  (unless (member project '(icecat icedove))
    (error "only icecat or icedove components are currently supported"))

  (let ((name (if (eq? 'icecat project)
                  "IceCat"
                  "Icedove")))
    (package
      (name (format #f "~a-l10n" project))
      (version version)
      (source source)
      (outputs (cons "out" locales))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 format)
                    (ice-9 ftw)
                    (srfi srfi-1)
                    (srfi srfi-26))
        #:tests? #f                     ;no tests, this is data
        #:phases
        #~(modify-phases %standard-phases
            (delete 'bootstrap)
            (delete 'install)
            (replace 'configure
              (lambda _
                ;; The following configuration is inspired by guidance at
                ;; https://firefox-source-docs.mozilla.org/build/buildsystem/locales.html.
                (call-with-output-file ".mozconfig"
                  (lambda (p)
                    (format p "~{~a~%~}"
                            (list (if (eq? 'icecat '#$project)
                                      "ac_add_options --enable-project=browser"
                                      "ac_add_options --enable-project=comm/mail")
                                  "ac_add_options --disable-compile-environment"
                                  (string-append
                                   "ac_add_options --with-l10n-base="
                                   (getcwd) "/l10n")
                                  ;; Hack, otherwise the build system throws:
                                  ;; 'RuntimeError: File "brand.dtd" not found'.
                                  "ac_add_options --enable-official-branding"
                                  "mk_add_options MOZ_OBJDIR=obj"))))
                (setenv "CONFIG_SHELL" (which "bash"))
                (setenv "MOZBUILD_STATE_PATH"
                        (string-append (getcwd) "/mach_state"))
                (setenv "MOZCONFIG" (string-append (getcwd) "/.mozconfig"))
                (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                (setenv "BUILD_BACKENDS" "FasterMake,RecursiveMake")))
            (replace 'build             ;build and install data files
              (lambda* (#:key outputs #:allow-other-keys)
                (define (find-file dir name)
                  (let ((files (find-files dir name)))
                    (when (null? files)
                      (error "could not find file in dir" name dir))
                    (car files)))

                (for-each
                 (lambda (l)
                   (let* ((out (assoc-ref outputs l))
                          ;; The older lib/$project/distribution/extensions
                          ;; directory is deprecated.  Use the newer app-global
                          ;; directory, which is lib/$project/extensions.
                          (ext-dir-prefix
                           (format
                            #f "lib/~a/~:[~;browser/~]extensions"
                            '#$project (eq? 'icecat '#$project)))
                          (all-ext (string-append #$output "/" ext-dir-prefix))
                          (ext-dir (string-append out "/" ext-dir-prefix))
                          ;; XXX: Because Icedove doesn't have a makeicedove
                          ;; script that substitutes all the Thunderbird
                          ;; references to Icedove, the MOZ_LANGPACK_EID
                          ;; defined in comm/mail/locales/Makefile.in uses
                          ;; 'thunderbird' in its ID extension rather than
                          ;; 'icedove'.
                          (name (format #f "langpack-~a@~a.mozilla.org.xpi"
                                        l (if (eq? 'icedove '#$project)
                                              'thunderbird
                                              '#$project))))
                     (format #t "processing locale `~a'...~%" l)
                     (if (eq? 'icecat '#$project)
                         ;; XXX: For some reasons, for IceCat, there are some
                         ;; parsing errors that cause the build system to
                         ;; return an unclean exit code; use system* to ignore
                         ;; errors.
                         (system* "./mach" "build" (string-append "langpack-" l))
                         (invoke "./mach" "build" (string-append "langpack-" l)))
                     (mkdir-p ext-dir)
                     (let ((xpi (find-file "obj" (string-append
                                                  "\\." l "\\.langpack\\.xpi$"))))
                       (copy-file xpi (string-append ext-dir "/" name))
                       ;; Symlink to the main output so that a user can
                       ;; install all of the language packs at once.
                       (mkdir-p all-ext)
                       (symlink (string-append ext-dir "/" name)
                                (string-append all-ext "/" name)))))
                 (if (eq? 'icedove '#$project)
                     '#$%icedove-locales
                     '#$%icecat-locales)))))))
      (native-inputs
       (list m4
             perl
             python-wrapper
             node
             unzip))
      (home-page "https://www.mozilla.org/")
      (synopsis (string-append "Language localization data for " name))
      (description (string-append "This package contains the various language
localization data files (language pack extensions) for " name ".  The
individual localization packages can be installed by using the output
associated with their name."))
      (license license:mpl2.0))))

(define-public icecat-l10n
  (make-l10n-package 'icecat %icecat-version icecat-source %icecat-locales))

(define-public icedove-l10n
  (make-l10n-package 'icedove %icedove-version icedove-source %icedove-locales))

;;; This hack exists because there's no way to configure extra extension
;;; search paths for IceCat or Icedove.  The global extensions directory is
;;; constructed relatively to the executable file name.
(define (make-mozilla-with-l10n project base l10n-package)
  "Return a package definition for PROJECT (a symbol such as 'icecat or
'icedove) that combines the BASE package with L10N-PACKAGE."

  (unless (member project '(icecat icedove))
    (error "only icecat or icedove components are currently supported"))

  (let ((name (symbol->string project))
        (icecat? (eq? 'icecat project)))
    (package
      (inherit base)
      (name (symbol->string project))
      ;; Use the copy-build-system, as it provides the necessary UTF-8 locales
      ;; support.
      (build-system copy-build-system)
      (arguments
       (list
        #:imported-modules `(,@%copy-build-system-modules
                             (guix build union))
        #:modules '((guix build copy-build-system)
                    (guix build union)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'install
              (lambda _
                (union-build #$output (list #$base #$l10n-package)
                             #:create-all-directories? #t)

                (define* (expose name #:optional (proc copy-file)
                                 #:key (source #$base))
                  (let ((dest (string-append #$output "/" name)))
                    (mkdir-p (dirname dest))
                    (proc (string-append source "/" name) dest)))

                (let ((wrapper (string-append "lib/" #$name "/" #$name))
                      (real-binary (string-append "lib/" #$name "/." #$name
                                                  "-real"))
                      (desktop-file (string-append "share/applications/"
                                                   #$name ".desktop")))
                  ;; Copy wrapper file.
                  (delete-file (string-append #$output "/" wrapper))
                  (expose wrapper)

                  ;; Recreate bin symlink.
                  (delete-file (string-append #$output "/bin/" #$name))
                  (symlink (string-append #$output "/" wrapper)
                           (string-append #$output "/bin/" #$name))

                  ;; Copy actual binary.
                  (delete-file (string-append #$output "/" real-binary))
                  (expose real-binary)

                  ;; Copy desktop file.
                  (delete-file (string-append #$output "/" desktop-file))
                  (expose desktop-file)

                  ;; Adjust the references in the desktop file and wrapper.
                  (substitute* (list (string-append #$output "/" desktop-file)
                                     (string-append #$output "/" wrapper))
                    ((#$base) #$output)))))))))))

(define-public icecat
  (make-mozilla-with-l10n 'icecat icecat-minimal icecat-l10n))

(define-public icedove
  (make-mozilla-with-l10n 'icedove icedove-minimal icedove-l10n))

(define-public icedove/wayland
  (package
    (inherit icedove)
    (name "icedove-wayland")
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((exe (string-append #$output "/bin/icedove")))
            (mkdir-p (dirname exe))
            (call-with-output-file exe
              (lambda (port)
                (format port "#!~a
 MOZ_ENABLE_WAYLAND=1 exec ~a $@"
                        #$(file-append bash-minimal "/bin/bash")
                        #$(file-append icedove "/bin/icedove"))))
            (chmod exe #o555)
            ;; Provide the manual and .desktop file.
            (copy-recursively (string-append #$icedove "/share")
                              (string-append #$output "/share"))
            (substitute* (string-append #$output
                                        "/share/applications/icedove.desktop")
              ((#$icedove) #$output))))))
    (native-inputs '())
    (inputs '())))

(define-public firefox-decrypt
  (package
    (name "firefox-decrypt")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Unode/firefox_decrypt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17yyyxp47z4m8hnflcq34rc1y871515kr3f1y42j1l0yx3g0il07"))))
    (build-system trivial-build-system)
    (inputs
     (list nss python))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append
                  (assoc-ref %build-inputs "python") "/bin"))
         (copy-file (string-append (assoc-ref %build-inputs "source")
                                   "/firefox_decrypt.py")
                    "firefox_decrypt.py")
         (substitute* "firefox_decrypt.py"
           (("/usr/bin/env python") (which "python3"))
           (("libnss3.so") (string-append (assoc-ref %build-inputs "nss")
                                          "/lib/nss/libnss3.so")))
         (install-file "firefox_decrypt.py" (string-append %output "/bin"))
         #t)))
    (home-page "https://github.com/Unode/firefox_decrypt/")
    (synopsis "Tool to extract passwords from Mozilla profiles")
    (description "Firefox Decrypt is a tool to extract passwords from
Mozilla (Firefox, Waterfox, Thunderbird, SeaMonkey) profiles.")
    (license license:gpl3+)))

(define-public lz4json
  (package
    (name "lz4json")
    (version "2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/andikleen/lz4json")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xxn8yzr6j8j6prmbj6mxspdczigarfiv3vlm9k70yxmky65ijh3"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list lz4))
    (arguments
     `(#:tests? #f                              ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)                    ; no configure script
         (replace 'install                      ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "lz4jsoncat" bin)
               (install-file "lz4jsoncat.1" man)))))
       #:make-flags `(,(string-append "CC=" ,(cc-for-target)))))
    (home-page "https://github.com/andikleen/lz4json")
    (synopsis "C decompress tool for mozilla lz4json format")
    (description
     "@code{lz4json} is a little utility to unpack lz4json files as generated
by Firefox's bookmark backups and session restore.  This is a different format
from what the normal lz4 utility expects.  The data is dumped to stdout.")
    (license license:bsd-2)))
