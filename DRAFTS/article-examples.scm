;; Examples for Parameterization
;;   (X): points requiring inputs/suggestions

;; 5 examples ~ 5-10 min
;;   Goes over all record types and almost all features
;;   Syntax is 'final' -
;;     will not change much after implementing suggestions

;; Enumeration Example
;; parameter-type: a guix record-type*
(define locale-parameter-type
  (parameter-type
   ;; name: must be a symbol
   (name 'locale-type)
   ;; universe: list of all symbols the type supports
   (accepted-values
    '(ca_ES cs_CZ da_DK de_DE
            el_GR en_AU en_CA en_GB
            en_US es_AR es_CL es_ES
            es_MX fi_FI fr_BE fr_CA
            fr_CH fr_FR ga_IE it_IT
            ja_JP ko_KR nb_NO nl_NL
            pl_PL pt_PT ro_RO ru_RU
            sv_SE tr_TR uk_UA vi_VN
            zh_CN))
   ;; negation unsupported!
   ;; set to the 'negative' symbol otherwise
   ;; by default - the first symbol in universe
   (negation #f)
   (default 'en_US) ;; *
   ;; description - string describing type
   (description "Type for Locales")))

;; Negation Example
;; package-parameter: record type for parameters
;; `parameter` is already a thing in scheme, hence package-parameter
(define-global-parameter
  (package-parameter
   ;; name: must be a symbol
   (name 'tests)
   ;; `morphisms`:
   ;;    term describing ALL package-variant generating methods
   ;;    includes transforms, modify-inputs, package functions
   ;;    (1) better name for morphisms?
   ;; macro that takes values on the left and morphisms on the right
   ;; (2) slash convention:
   ;;   functions/macros that operate on parmaters prefixed with `parameter/`
   ;;   easy to distinguish between such functions and record fields
   ;;   same for `parameter-spec/`
   (variants
    (parameter-variant-match ; no more + or ->
     (#:off #:transform (without-tests #:package-name))))
   ;; description: strings describing parameter
   (description "Toggle for tests")
   ;; universal: parameters that can work even if not in the spec
   (predicate #t)))
;; predicate: lambda to run against package
;; if satisfied, then parameter can be used against it without being inspec
;; #f by true
;; no type or dependencies mentioned
;;   default type - boolean
;;   default dependencies - '()

;; Optimize
(package-parameter
 (name 'gcc-oflag)
 (type
  (parameter-type
   (name '_)
   (accepted-values '(-O0 -O1 -O2 -O3 -Os -Ofast -Og -Oz))
   (negation #f)))
 (variants
  (parameter-variant-match
   (_ #:build-system gnu-build-system
      #:lambda
      (package/inherit #:package
        (arguments
         (substitute-keyword-arguments
          (package-arguments #:package)
          ((#:make-flags flags #~'())
           #~(append
              #$flags
              (list (string-append "CFLAGS="
                                   #:parameter-value)))))))))))

;; Static
;; XXX: might be a better idea to use substitute-keyword-arguments
;;      some packages have --disable-static in the package definition
(package-parameter
 (name 'static-lib)
 (variants
  (parameter-variant-match
   (_ #:transform
      (with-configure-flag #:package-name "=--disable-shared")
      (with-configure-flag #:package-name "=--enable-static")))))

;; Arch
;;   --tune is the recommended way of building packages for a given architecture
;;   --target might be better suited for this however, but it is a ~guix build~ option
;;   Ask Efraim in the next meet

;; Main Example
;;; package-with-paramters
;;    takes a parameter-spec as the first argument, and applies it to the body
(package-with-parameters
 [parameter-spec
  (local
      (list
       (package-parameter (name 'next))
       (package-parameter (name 'tree-sitter)
                          (dependencies '(tree-sitter)))
       (package-parameter
        (name 'pgtk)
        (variants
         (parameter-variant-match
          (_ #:transform (with-configure-flag
                          #:package-name "=--with-pgtk"))))
        (dependencies '(tree-sitter x11)))
       (package-parameter
        (name 'xwidgets)
        (variants
         (parameter-variant-match
          (_ #:transform (with-configure-flag
                          #:package-name "=--with-xwidgets")))))
       (package-parameter
        (name 'wide-int)
        (variants
         (parameter-variant-match
          (_ #:transform (with-configure-flag
                          #:package-name "=--with-wide-int")))))))
  (one-of '((_ (x11 #:off) pgtk)
            (_ (x11 #:off) xwidgets)))]
 (inherit emacs)
 (name "emacs-parameterized")
 (source
  (parameter-if
   'next
   (origin
    (inherit (package-source emacs))
    (method git-fetch)
    (uri (git-reference
          (url "https://git.savannah.gnu.org/git/emacs.git/")
          (commit (string-append "emacs-" version))))
    (file-name (git-file-name name version))
    (patches
     (search-patches
      "emacs-exec-path.patch"
      "emacs-fix-scheme-indent-function.patch"
      "emacs-native-comp-driver-options.patch"
      (parameter-if 'pgtk
                    "emacs-pgtk-super-key-fix.patch"
                    nil)))
    (sha256
     (base32
      "09jm1q5pvd1dc0xq5rhn66v1j235zlr72kwv5i27xigvi9nfqkv1")))
   (origin
    (inherit (package-source emacs)))))
 (arguments
  (substitute-keyword-arguments (package-arguments emacs)
    (parameter-match
     [((x11 #:off))
      '(((#:configure-flags flags #~'())
         #~(delete "--with-cairo" #$flags))
        ((#:modules _) (%emacs-modules build-system))
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'restore-emacs-pdmp)
             (delete 'strip-double-wrap))))]
     [(#:all xwidgets (pgtk #:off))
      '(((#:configure-flags flags #~'())
         #~(cons "--with-xwidgets" #$flags))
        ((#:modules _) (%emacs-modules build-system))
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'restore-emacs-pdmp)
             (delete 'strip-double-wrap))))])))
 (inputs
  (parameter-modify-inputs
   [(next) (prepend sqlite)]
   [(tree-sitter) (prepend tree-sitter)]
   [(xwidgets) (prepend gsettings-desktop-schemas
                        webkitgtk-with-libsoup2)]
   [((x11 #:off))
    (delete "libx11" "gtk+" "libxft" "libtiff" "giflib" "libjpeg"
            "imagemagick" "libpng" "librsvg" "libxpm" "libice" "libsm"
            "cairo" "pango" "harfbuzz" "libotf" "m17n-lib" "dbus")])))


