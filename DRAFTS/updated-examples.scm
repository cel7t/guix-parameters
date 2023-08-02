;; Examples for Parameterization

; parameter-type: a guix record-type*
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
    ;; default value -
    ;;   if negation supported, second value
    ;;   if not supported, first value
    (default 'en_US)
    ;; description - string describing type
    (description "Type for Locales"))

;; Negation Example
;; package-parameter: record type for parameters
;; `parameter` is already a thing in scheme, hence package-parameter
(package-parameter
 ;; name: must be a symbol
   (name 'tests)
   ;; `morphisms`:
   ;;    term describing ALL package-variant generating methods
   ;;    includes transforms, modify-inputs, package functions
   ;;    (1) better name for morphisms?
   (morphisms ('all ('on (list (cons transform transforms...) (cons procedure procedures...))))
    ;; macro that takes values on the left and morphisms on the right
    ;; (2) slash convention:
    ;;   functions/macros that operate on parmaters prefixed with `parameter/`
    ;;   easy to distinguish between such functions and record fields
    ;;   same for `parameter-spec/`
    (parameter-spec/morphism-match ;; *
     ;; ! -> refers to parameter-type's negation
     ;; useful since we don't need to keep track of the type values
     ;; morphisms are indexed by keywords (like #:transform here)
     ((-O2 -O3) + (gnu-build-system lisp-build-system) -> #:transform (without-tests #:package-name))))
     ((-O2 -O3) -> #:transform (without-tests #:package-name))))
     ((-O2 -O3) (gnu-build-system lisp-build-system) -> (without-tests #:package-name))))
   ;; description: strings describing parameter
   (description "Toggle for tests")
   ;; universal: parameters that can work even if not in the spec
   (works-with-all-packages? #t)) ;; *
;; no type or dependencies mentioned
;;   default type - boolean
;;   default dependencies - '()

(define boolean (parameter-type ...))

;; Optimize
(package-parameter
 (name 'gcc-oflag)
 (type
  ;; parameter-type/lambda: anonymous parameter types
  ;; *
  (parameter-type (name 'gcc-oflag-type) (universe '(-O0 -O1 -O2 -O3 -Os -Ofast -Og -Oz)) (negation #f)))
 (inputs (parameter/dependencies #:parameter gcc #:packages (glibc . morphisms) glibc))
 (morphisms
  (parameter/morphism-match
   ;; #:package and #:parameter-value substitute in the package
   ;; and the value of the positive parmeter respectively
   ;; _ : matches all non-negative values of the parameter
   ;; + <build-systems> : build systems necessary for match
   (... #:transform (without-tests "=gcc@2.0")
        #:procedure
        `(package/inherit ,#:package
           (arguments
            (substitute-keyword-arguments
             (package-arguments ,#:package)
              ((#:make-flags flags #~'())
               #~(append
                  #$flags
                  (list (string-append "CFLAGS="
                                       ,#:parameter-value)))))))))))

;; Static
;; XXX: might be a better idea to use substitute-keyword-arguments
;;      some packages have --disable-static in the package definition
(package-parameter
 (name 'static-lib)
 (morphisms
  (parameter/morphism-match
   ;; by default, if no keyword is provided transform is assumed
   (_ -> (with-configure-flag #:package-name "=--disable-shared")
         (with-configure-flag #:package-name "=--enable-static")))))

;; Arch
;;   --tune is the recommended way of building packages for a given architecture
;;   --target might be better suited for this however, but it is a ~guix build~ option
;;   Ask Efraim in the next meet

;; Main Example
;;; package-with-paramters
;;    takes a parameter-spec as the first argument, and applies it to the body
(package-with-parameters
 ;; parameter-spec: record type for a package's parameter logic
 [parameter-spec
  (local ; parameters that are LOCAL to package
      ;; parameter/parameter-list: convenience function that converts a list of
      ;; partial package-parameter definitions into a full-fledged one
      (parameter/parameter-list
       ;; 'sym -> (package-parameter (name 'sym))
       'next
       ;; (3) dependencies: parameters and packages a given parameter requires to work
       ;;  normally a field in the package-parameter record
       ;;  uses dependency-match, similar to morphism-match
       ;;  #:parameter -> parameteric dependencies (ex. x11, (pgtk on) etc..)
       ;;  #:package -> package dependencies (injected)
       ('tree-sitter #:dependencies '(next))
       ('pgtk
        (parameter/morphism _ -> (with-configure-flag
                             #:package-name "=--with-pgtk"))
        #:dependencies '(tree-sitter x11))
       ('xwidgets
        (parameter/morphism _ -> (with-configure-flag
                             #:package-name "=--with-xwidgets")))
       ('wide-int
        (parameter/morphism _ #:apply #:transform
                            (with-configure-flag
                             #:package-name "=--with-wide-int")))))
  (optional) (required) ;; * -> !
  (optional '(x11)) ;; scheme's parameterize SRFI
  (required '((gcc-oflag #:off))
  ; ; one-of: _ means also accept none being true
  (one-of '(((x11 _) x11! (x11 v2) (pgtk off)) ; psym! => (psym !)
            (x11! xwidgets)))]
 ;; from here:
 ;;  package body
 (inherit emacs)
 (name "emacs-parameterized")
 (source
  ;; (parameter/if 'psym expr expr2)
  ;;   -> expr if 'psym satisfied, expr2 otherwise
  (parameter/if
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
      (parameter/if 'pgtk
                    "emacs-pgtk-super-key-fix.patch"
                    nil)))
    (sha256
     (base32
      "09jm1q5pvd1dc0xq5rhn66v1j235zlr72kwv5i27xigvi9nfqkv1")))
   (origin
    (inherit (package-source emacs)))))
 (arguments
  (substitute-keyword-arguments (package-arguments emacs)
    ;; very useful macro that matches against a list of parameters
    ;; any of them need to be satisfied
    (parameter/match
     [(x11!)
      '(((#:configure-flags flags #~'())
         #~(delete "--with-cairo" #$flags))
        ((#:modules _) (%emacs-modules build-system))
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'restore-emacs-pdmp)
             (delete 'strip-double-wrap))))]
     ;; #:all : all parameters must be satisfied
     [(#:all xwidgets pgtk!)
      '(((#:configure-flags flags #~'())
         #~(cons "--with-xwidgets" #$flags))
        ((#:modules _) (%emacs-modules build-system))
        ((#:phases phases)
         #~(modify-phases #$phases
             (delete 'restore-emacs-pdmp)
             (delete 'strip-double-wrap))))])))
 (inputs
  ;; parameter/modify-inputs: modify inputs with cdr if car is satisfied
  (parameter/modify-inputs
   [(next) (prepend sqlite)]
   [(tree-sitter) (prepend tree-sitter)]
   [(xwidgets) (prepend gsettings-desktop-schemas
                        webkitgtk-with-libsoup2)]
   [(x11!)
    (delete "libx11" "gtk+" "libxft" "libtiff" "giflib" "libjpeg"
            "imagemagick" "libpng" "librsvg" "libxpm" "libice" "libsm"
            "cairo" "pango" "harfbuzz" "libotf" "m17n-lib" "dbus")])))


