;; Enumeration Example
(parameter-type
    (name 'locale-type)
    (universe
     '(ca_ES cs_CZ da_DK de_DE
       el_GR en_AU en_CA en_GB
       en_US es_AR es_CL es_ES
       es_MX fi_FI fr_BE fr_CA
       fr_CH fr_FR ga_IE it_IT
       ja_JP ko_KR nb_NO nl_NL
       pl_PL pt_PT ro_RO ru_RU
       sv_SE tr_TR uk_UA vi_VN
       zh_CN))
    (negation #f)
    (description "Type for Locales"))

;; Negation Example
(package-parameter
   (name 'tests)
   (morphisms
    (parameter/morphism-match
     (! -> #:transform (without-tests . #:package-name))
     (_ -> #:transform (with-tests . #:package-name))))
   (descrption "Toggle for tests")
   (universal? #t))

;; Optimize
(package-parameter
 (name 'gcc-oflag)
 (type
  (parameter/type _ '(-O0 -O1 -O2 -O3 -Os -Ofast -Og -Oz) #f))
 (morphisms
  (parameter/morphism-match
   (_ + gnu-build-system -> #:procedure
        `(package/inherit ,#:package
           (arguments
            (substitute-keyword-arguments (package-arguments ,#:package)
              ((#:make-flags flags #~'())
               #~(append #$flags
                         (list (string-append "CFLAGS=" ,#:parameter-value)))))))))))

;; Static
;; XXX: might be a better idea to use substitute-keyword-arguments
;;      some packages have --disable-static in the package definition
(package-parameter
 (name 'static-lib)
 (morphisms
  (parameter/morphism-match
   (_ -> (with-configure-flag . "--disable-shared")
      (with-configure-flag . "--enable-static")))))

;; Arch
;;   --tune is the recommended way of building packages for a given architecture
;;   --target might be better suited for this however, but it is a ~guix build~ option
;;   Ask Efraim in the next meet

;; Main Example
(package-with-parameters
 [(local
      (parameter/parameter-list
       'next
       ('tree-sitter #:dependencies '(next))
       ('pgtk
        (parameter/morphism _ -> (with-configure-flag
                             #:package-name "=--with-pgtk"))
        #:dependencies '(tree-sitter x11))
       ('xwidgets
        (parameter/morphism _ -> (with-configure-flag
                             #:package-name "=--with-xwidgets")))
       ('wide-int
        (parameter/morphism _ -> #:transform
                            (with-configure-flag
                             #:package-name "=--with-wide-int")))))
  (one-of '((_ x11! pgtk)
            (_ x11! xwidgets)))]
 (inherit emacs)
 (name "emacs-parameterized")
 (source
  (origin
    (inherit (package-source emacs))
    (parameter/match
     [(next)
      '((method git-fetch)
        (uri (git-reference
              (url "https://git.savannah.gnu.org/git/emacs.git/")
              (commit (string-append "emacs-" version))))
        (file-name (git-file-name name version))
        (patches
         (search-patches
          "emacs-exec-path.patch"
          "emacs-fix-scheme-indent-function.patch"
          "emacs-native-comp-driver-options.patch"
          (parameter/if 'pgtk
                        "emacs-pgtk-super-key-fix.patch"
                        nil)))
        (sha256
         (base32
          "09jm1q5pvd1dc0xq5rhn66v1j235zlr72kwv5i27xigvi9nfqkv1")))])))
 (arguments
  (substitute-keyword-arguments (package-arguments emacs)
    (parameter/match
     [(x11!)
      '(((#:configure-flags flags #~'())
         #~(delete "--with-cairo" #$flags))
        ((#:modules _) (%emacs-modules build-system))
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'restore-emacs-pdmp)
             (delete 'strip-double-wrap))))]
     [(#:and xwidgets pgtk!)
      '(((#:configure-flags flags #~'())
         #~(cons "--with-xwidgets" #$flags))
        ((#:modules _) (%emacs-modules build-system))
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'restore-emacs-pdmp)
             (delete 'strip-double-wrap))))])))
 (inputs
  (parameter/modify-inputs
   [(next) (prepend sqlite)]
   [(tree-sitter) (prepend tree-sitter)]
   [(xwidgets) (prepend gsettings-desktop-schemas
                        webkitgtk-with-libsoup2)]
   [(x11!)
    (delete "libx11" "gtk+" "libxft" "libtiff" "giflib" "libjpeg"
            "imagemagick" "libpng" "librsvg" "libxpm" "libice" "libsm"
            "cairo" "pango" "harfbuzz" "libotf" "m17n-lib" "dbus")])))


