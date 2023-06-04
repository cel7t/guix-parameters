;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2021, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2016, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2018, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2019 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2019 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2021 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2021 Andy Tai <atai@atai.org>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Tobias Kortkamp <tobias.kortkamp@gmail.com>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2022 dan <i@dan.games>
;;; Copyright © 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2023 Eric Bavier <bavier@posteo.net>
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

(define-module (gnu packages graphics)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages pulseaudio)  ; libsndfile, libsamplerate
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix utils))

(define-public mmm
  (package
    (name "mmm")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/hodefoting/mmm")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1xmcv6rwinqsbr863rgl9005h2jlmd7k2qrwsc1h4fb8r61ykpjl"))))
    (build-system meson-build-system)
    (native-inputs
     (list luajit pkg-config))
    (inputs
     (list alsa-lib sdl sdl2))
    (synopsis "Memory Mapped Machine")
    (description "MMM is a shared memory protocol for virtualising access to
framebuffer graphics, audio output and input event.")
    (home-page "https://github.com/hodefoting/mrg")
    (license license:isc)))

(define-public directfb
  (package
    (name "directfb")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/deniskropp/DirectFB")
         (commit "DIRECTFB_1_7_7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bs3yzb7hy3mgydrj8ycg7pllrd2b6j0gxj596inyr7ihssr3i0y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-buildtime
           ;; Remove embedded build time for reproducible builds
           (lambda _
             (substitute* "src/core/core.c"
               (("..BUILDTIME..") ""))))
         (add-after 'unpack 'disable-configure-during-bootstrap
           (lambda _
             (substitute* "autogen.sh"
               (("^.*\\$srcdir/configure.*") ""))
             #t)))))
    (native-inputs
     (list autoconf automake libtool perl pkg-config))
    (inputs
     (list alsa-lib
           ffmpeg
           freetype
           glu
           gstreamer
           imlib2
           jasper
           libjpeg-turbo
           libcddb
           libdrm
           libtimidity
           libmad
           libmng
           libmpeg2
           libmpeg3
           mesa
           libpng
           sdl
           (librsvg-for-system)
           libtiff
           tslib
           libvdpau
           libvorbis
           wayland
           libwebp
           libx11
           libxcomposite
           libxext
           xorgproto
           zlib))
    (propagated-inputs
     (list flux))
    (synopsis "DFB Graphics Library")
    (description "DirectFB is a graphics library which was designed with embedded
systems in mind.  It offers maximum hardware accelerated performance at a
minimum of resource usage and overhead.")
    (home-page "https://github.com/deniskropp/DirectFB")
    (license license:lgpl2.1+)))

(define-public minifb
  (let ((commit "43f8c1309341f4709a471b592d04434326042483")
        (revision "1"))
    (package
      (name "minifb")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri
                 (git-reference
                  (url "https://github.com/emoon/minifb")
                  (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1z0720azsgi83yg4ysmfvpvsg0566s2cq59xx52w8w5rpkla4cjh"))))
      (build-system cmake-build-system)
      (arguments
       ;; Don't build examples.
       '(#:configure-flags '("-DMINIFB_BUILD_EXAMPLES=0")
         #:phases
         ;; There is no install target, so we have to copy the static library
         ;; and headers to the output directory ourselves.
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (includedir (string-append out "/include"))
                      (libdir (string-append out "/lib")))
                 (mkdir-p includedir)
                 (mkdir-p libdir)
                 (for-each (lambda (header)
                             (copy-file header
                                        (string-append includedir "/"
                                                       (basename header))))
                           (find-files "../source/include" "\\.h$"))
                 (copy-file "libminifb.a" (string-append libdir "/libminifb.a"))))))
         ;; No check target.
         #:tests? #f))
      ;; libminifb.a won't work without these libraries, so propagate them.
      (propagated-inputs (list libx11 libxkbcommon mesa))
      (synopsis "Small library for rendering pixels to a framebuffer")
      (description "MiniFB (Mini FrameBuffer) is a small, cross-platform
library that makes it easy to render (32-bit) pixels in a window.")
      (home-page "https://github.com/emoon/minifb")
      (license license:expat))))

(define-public flux
  (package
    (name "flux")
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/deniskropp/flux")
         (commit "e45758a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11f3ypg0sdq5kj69zgz6kih1yrzgm48r16spyvzwvlswng147410"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "Interface description language")
    (description "Flux is an interface description language used by DirectFB.
Fluxcomp compiles .flux files to .cpp or .c files.")
    (home-page "https://www.directfb.org/")
    (license license:lgpl2.1+))) ; Same as DirectFB

(define-public fox
  (package
    (name "fox")
    (version "1.6.57")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://fox-toolkit.org/ftp/fox-" version ".tar.gz"))
       (sha256
        (base32 "08w98m6wjadraw1pi13igzagly4b2nfa57kdqdnkjfhgkvg1bvv5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "configure"
               (("-I/usr/include/freetype2")
                (string-append "-I"
                               (string-append
                                (assoc-ref %build-inputs "freetype")
                                "/include/freetype2"))))
             #t)))))
    (native-inputs
     (list doxygen))
    (inputs
     `(("bzip2" ,lbzip2)
       ("freetype" ,freetype)
       ("gl" ,mesa)
       ("glu" ,glu)
       ("jpeg" ,libjpeg-turbo)
       ("png" ,libpng)
       ("tiff" ,libtiff)
       ("x11" ,libx11)
       ("xcursor" ,libxcursor)
       ("xext" ,libxext)
       ("xfixes" ,libxfixes)
       ("xft" ,libxft)
       ("xinput" ,libxi)
       ("xrandr" ,libxrandr)
       ("xrender" ,libxrender)
       ("xshm" ,libxshmfence)
       ("zlib" ,zlib)))
    (synopsis "Widget Toolkit for building GUI")
    (description"FOX (Free Objects for X) is a C++ based Toolkit for developing
Graphical User Interfaces easily and effectively.   It offers a wide, and
growing, collection of Controls, and provides state of the art facilities such
as drag and drop, selection, as well as OpenGL widgets for 3D graphical
manipulation.  FOX also implements icons, images, and user-convenience features
such as status line help, and tooltips.  Tooltips may even be used for 3D
objects!")
    (home-page "http://www.fox-toolkit.org")
    (license license:lgpl2.1+)))

(define-public autotrace
  (package
    (name "autotrace")
    (version "0.31.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/autotrace/autotrace")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fsg13pg72ac51l3fkzvyf7h9mzbvfxp9vfjfiwkyvx6hbm83apj"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~'("--disable-static")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-pkg-config-file
                 (lambda _
                   ;; autotrace can be built against either GraphicsMagick or
                   ;; ImageMagick.  However the pkg-config file refers to
                   ;; non-existent MAGICK_ variables instead of GRAPHICSMAGICK_
                   ;; or IMAGEMAGICK_; fix that.
                   (substitute* "autotrace.pc.in"
                     (("@MAGICK_(LIBS|CFLAGS)@" _ var)
                      (string-append "@IMAGEMAGICK_" var "@"))))))))
    (native-inputs
     (list which
           autoconf
           automake
           libtool
           intltool
           pkg-config
           procps))                     ;for tests
    (inputs
     (list glib
           imagemagick
           libjpeg-turbo
           libpng
           pstoedit))
    (home-page "https://github.com/autotrace/autotrace")
    (synopsis "Bitmap to vector graphics converter")
    (description "AutoTrace is a utility for converting bitmap into vector
graphics.  It can trace outlines and midlines, effect color reduction or
despeckling and has support for many input and output formats.  It can be used
with the @command{autotrace} utility or as a C library, @code{libautotrace}.")
    (license (list license:gpl2+        ;for the utility itself
                   license:lgpl2.1+)))) ;for use as a library

(define-public embree
  (package
    (name "embree")
    (version "3.13.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/embree/embree")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kcvz7g6j56anv9zjyd3gidxl46vipw0gg82lns12m45cd43iwxm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no tests (apparently)
       #:configure-flags
         (list
          "-DEMBREE_ISPC_SUPPORT=OFF")))
    (inputs
     (list tbb glfw))
    (home-page "https://www.embree.org/")
    (synopsis "High performance ray tracing kernels")
    (description
     "Embree is a collection of high-performance ray tracing kernels.
Embree is meant to increase performance of photo-realistic rendering
applications.")
    (license license:asl2.0)))

(define-public openvdb
  (package
    (name "openvdb")
    (version "8.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AcademySoftwareFoundation/openvdb/")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0856697hnwk8xsp29kx8y2p1kliy0bdwfsznxm38v4690vna15rk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCMAKE_EXE_LINKER_FLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))))
    (inputs
     (list boost c-blosc ilmbase tbb zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.openvdb.org/")
    (synopsis "Sparse volume data structure and tools")
    (description "OpenVDB is a C++ library comprising a hierarchical data
structure and a large suite of tools for the efficient storage and
manipulation of sparse volumetric data discretized on three-dimensional grids.
It was developed by DreamWorks Animation for use in volumetric applications
typically encountered in feature film production.")
    (license license:mpl2.0)))

(define-public blender
  (package
    (name "blender")
    (version "3.3.5")                   ;3.3.x is the current LTS version
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.blender.org/source/"
                                  "blender-" version ".tar.xz"))
              (sha256
               (base32
                "1pwl4lbc00g0bj97rd8l9fnrv3w1gny9ci6mrma3pp2acgs56502"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Test files are very large and not included in the release tarball.
      #:tests? #f
      #:configure-flags
      (let ((python-version (version-major+minor (package-version python))))
        #~(list "-DWITH_CODEC_FFMPEG=ON"
                "-DWITH_CODEC_SNDFILE=ON"
                "-DWITH_CYCLES=ON"
                "-DWITH_DOC_MANPAGE=ON"
                "-DWITH_FFTW3=ON"
                "-DWITH_IMAGE_OPENJPEG=ON"
                "-DWITH_INPUT_NDOF=ON"
                "-DWITH_INSTALL_PORTABLE=OFF"
                "-DWITH_JACK=ON"
                "-DWITH_MOD_OCEANSIM=ON"
                "-DWITH_OPENVDB=ON"
                "-DWITH_OPENSUBDIV=ON"
                "-DWITH_PYTHON_INSTALL=OFF"
                (string-append "-DPYTHON_LIBRARY=python" #$python-version)
                (string-append "-DPYTHON_LIBPATH="
                               (assoc-ref %build-inputs "python")
                               "/lib")
                (string-append "-DPYTHON_INCLUDE_DIR="
                               (assoc-ref %build-inputs "python")
                               "/include/python" #$python-version)
                (string-append "-DPYTHON_VERSION=" #$python-version)
                (string-append "-DPYTHON_NUMPY_INCLUDE_DIRS="
                               (assoc-ref %build-inputs "python-numpy")
                               "/lib/python" #$python-version
                               "/site-packages/numpy/core/include/")
                (string-append "-DPYTHON_NUMPY_PATH="
                               (assoc-ref %build-inputs "python-numpy")
                               "/lib/python" #$python-version
                               "/site-packages/")
                ;; OpenEXR propagates ilmbase, but its include files do not
                ;; appear in the C_INCLUDE_PATH, so we need to add
                ;; "$ilmbase/include/OpenEXR/" to the C_INCLUDE_PATH to
                ;; satisfy the dependency on "half.h" and "Iex.h".
                (string-append "-DCMAKE_CXX_FLAGS=-I"
                               (search-input-directory %build-inputs
                                                       "include/OpenEXR"))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-broken-import
            (lambda _
              (substitute* "release/scripts/addons/io_scene_fbx/json2fbx.py"
                (("import encode_bin")
                 "from . import encode_bin")))))))
    (inputs
     (list boost
           embree
           ffmpeg-5
           fftw
           freetype
           glew
           gmp                        ;needed for boolean operations on meshes
           ilmbase
           jack-1
           jemalloc
           libjpeg-turbo
           libpng
           libsndfile
           libtiff
           libx11
           libxi
           libxrender
           openal
           opencolorio
           openexr-2
           openimageio
           openjpeg
           opensubdiv
           openvdb
           pugixml
           python
           python-numpy
           tbb
           zlib
           `(,zstd "lib")))
    (home-page "https://blender.org/")
    (synopsis "3D graphics creation suite")
    (description
     "Blender is a 3D graphics creation suite.  It supports the entirety of
the 3D pipeline—modeling, rigging, animation, simulation, rendering,
compositing and motion tracking, even video editing and game creation.  The
application can be customized via its API for Python scripting.")
    (license license:gpl2+)))

(define-public goxel
  (package
    (name "goxel")
    (version "0.10.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/guillaumechereau/goxel")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qvz566awhp03yp696fn3c80hnky41fpbi4sqg4lx69ibx4zvl9k"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "release")))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("gtk3" ,gtk+)
       ("glfw" ,glfw)
       ("scons" ,scons)))
    (home-page "https://goxel.xyz/")
    (synopsis "Voxel editor")
    (description
     "Goxel is a voxel editor that features unlimited scene size, unlimited
history buffer, 24-bit RGB colors, layers, procedural rendering, ray tracing,
and export to various formats including the format used by Magicavoxel.")
    (license license:gpl3+)))

(define-public assimp
  (package
    (name "assimp")
    (version "5.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/assimp/assimp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j0pd279n6xyy95x782ha8j75kbx0ck7vs5wv3krhbyfim9bw64l"))))
    (build-system cmake-build-system)
    (inputs
     (list zlib))
    (home-page "https://www.assimp.org/")
    (synopsis "Asset import library")
    (description
     "The Open Asset Import Library loads more than 40 3D file formats into
one unified data structure.  Additionally, assimp features various mesh post
processing tools: normals and tangent space generation, triangulation, vertex
cache locality optimization, removal of degenerate primitives and duplicate
vertices, sorting by primitive type, merging of redundant materials and many
more.")
    (license license:bsd-3)))

(define-public assimp-5.0
  (package
    (inherit assimp)
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/assimp/assimp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "assimp" version))
              (sha256
               (base32
                "1w2484lg823bql7lpfq84vnsfsnag5v65qrbphslj866z9ia68l7"))))))

(define-public mikktspace
  ;; The latest commit is used as there is no release.
  (let ((commit   "3e895b49d05ea07e4c2133156cfa94369e19e409")
        (revision "0"))
    (package
      (name "mikktspace")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mmikk/MikkTSpace")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rjh9zflx51hdhnfadal87v4hhkrbprkv692hjkg9wkxx0ch39zi"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (replace 'build
                   (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
                     (invoke #$(cc-for-target) "mikktspace.c" "-O2" "-g" "-fPIC"
                             "-shared" "-o" "libmikktspace.so")))
                 (replace 'install
                   (lambda _
                     (install-file "mikktspace.h"
                                   (string-append #$output "/include"))
                     (install-file "libmikktspace.so"
                                   (string-append #$output "/lib")))))))
      (home-page "http://www.mikktspace.com/")
      (synopsis "Library for a common standard for tangent spaces")
      (description
       "This package provides a common standard tangent space library used in
baking tools to produce normal maps.")
      (license license:zlib))))

(define-public openshadinglanguage
  (package
    (name "openshadinglanguage")
    (version "1.11.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AcademySoftwareFoundation/OpenShadingLanguage")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x0lc163vl2b57l75bf5zxlr6vm2y1f1izlxdnrw3vsapv3r9k9g"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DUSE_PARTIO=OFF") ; TODO: not packaged
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'add-ilmbase-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; OpenEXR 2 propagates ilmbase, but its include files do not
             ;; appear in the C_INCLUDE_PATH.
             (let ((headers (string-append
                             (assoc-ref inputs "ilmbase")
                             "/include/OpenEXR")))
               (setenv "C_INCLUDE_PATH"
                       (string-append headers ":"
                                      (or (getenv "C_INCLUDE_PATH") "")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append headers ":"
                                      (or (getenv "CPLUS_INCLUDE_PATH") ""))))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ctest" "--exclude-regex"
                       (string-join
                        (list
                         "osl-imageio"       ; OIIO not compiled with freetype
                         "osl-imageio.opt"   ; OIIO not compiled with freetype
                         "texture-udim"      ; file does not exist
                         "texture-udim.opt"  ; file does not exist
                         "example-deformer"  ; could not find OSLConfig
                         "python-oslquery")  ; no module oslquery
                        "|"))))))))
    (native-inputs
     (list bison
           clang-9
           flex
           llvm-9
           pybind11
           python-wrapper))
    (inputs
     (list boost
           imath
           openexr-2
           openimageio
           pugixml
           qtbase-5
           zlib))
    (home-page "https://github.com/AcademySoftwareFoundation/OpenShadingLanguage")
    (synopsis "Shading language for production GI renderers")
    (description "Open Shading Language (OSL) is a language for programmable
shading in advanced renderers and other applications, ideal for describing
materials, lights, displacement, and pattern generation.")
    (license license:bsd-3)))

(define-public cgal
  (package
    (name "cgal")
    (version "5.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/CGAL/cgal/releases/download/v" version
                    "/CGAL-" version ".tar.xz"))
              (sha256
               (base32
                "0yjzq12ivizp23y7zqm30x20psv9gzwbcdrhyd3f7h0ds94m1c40"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; Prevent two mostly-duplicate directories.  Use Guix's versioned
       ;; default for licences instead of CGAL's unversioned one.
       (list (string-append "-DCGAL_INSTALL_DOC_DIR=share/doc/"
                            ,name "-" ,version))
       #:tests? #f))                    ; no test target
    (inputs
     (list mpfr gmp boost))
    (home-page "https://www.cgal.org/")
    (synopsis "Computational geometry algorithms library")
    (description
     "CGAL provides easy access to efficient and reliable geometric algorithms
in the form of a C++ library.  CGAL is used in various areas needing geometric
computation, such as: computer graphics, scientific visualization, computer
aided design and modeling, geographic information systems, molecular biology,
medical imaging, robotics and motion planning, mesh generation, numerical
methods, etc.  It provides data structures and algorithms such as
triangulations, Voronoi diagrams, polygons, polyhedra, mesh generation, and
many more.")

    ;; The 'LICENSE' file explains that a subset is available under more
    ;; permissive licenses.
    (license license:gpl3+)))

(define-public imath
  (package
    (name "imath")
    (version "3.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AcademySoftwareFoundation/Imath")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nyld18mf220ghm1vidnfnn0rdns9z5i4l9s66xgd0kfdgarb31f"))))
    (build-system cmake-build-system)
    (arguments
     ;; XXX: On i686-linux, tests fail due to rounding issues (excess
     ;; precision), as was discussed and patched long ago:
     ;; <https://issues.guix.gnu.org/22049>.  It seems the relevant fixes
     ;; didn't make it upstream, so skip tests.
     (list #:tests? (not (target-x86-32?))))
    (home-page "https://github.com/AcademySoftwareFoundation/Imath")
    (synopsis "Library of math operations for computer graphics")
    (description
     "Imath is a C++ representation of 2D and 3D vectors and matrices and other
mathematical objects, functions, and data types common in computer graphics
applications, including the \"half\" 16-bit floating-point type.")
    (license license:bsd-3)))

(define-public ilmbase
  (package
    (name "ilmbase")
    (version "2.5.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openexr/openexr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "ilmbase" version))
              (sha256
               (base32
                "1vja0rbilcd1wn184w8nbcmck00n7bfwlddwiaxw8dhj64nx4468"))
              (patches (search-patches "ilmbase-fix-tests.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'change-directory
                          (lambda _
                            (chdir "IlmBase")
                            #t))
                        #$@(if (target-x86-32?)
                               #~((add-after 'change-directory 'skip-test
                                    (lambda _
                                      ;; XXX: This test fails on i686,
                                      ;; possibly due to excess precision when
                                      ;; comparing floats.  Skip it.
                                      (substitute* "ImathTest/testFun.cpp"
                                        (("assert \\(bit_cast<unsigned>.*" all)
                                         (string-append "// " all "\n"))))))
                               #~()))))
    (home-page "https://www.openexr.com/")
    (synopsis "Utility C++ libraries for threads, maths, and exceptions")
    (description
     "IlmBase provides several utility libraries for C++.  Half is a class
that encapsulates ILM's 16-bit floating-point format.  IlmThread is a thread
abstraction.  Imath implements 2D and 3D vectors, 3x3 and 4x4 matrices,
quaternions and other useful 2D and 3D math functions.  Iex is an
exception-handling library.")
    (license license:bsd-3)))

(define-public lib2geom
  (package
    (name "lib2geom")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/inkscape/lib2geom.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dq981g894hmvhd6rmfl1w32mksg9hpvpjs1qvfxrnz87rhkknj8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%cmake-build-system-modules)
       #:configure-flags '("-D2GEOM_BUILD_SHARED=ON"
                           "-D2GEOM_BOOST_PYTHON=ON"
                           ;; Compiling the Cython bindings fail (see:
                           ;; https://gitlab.com/inkscape/lib2geom/issues/21).
                           "-D2GEOM_CYTHON_BINDINGS=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-python-lib-install-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((python-version (@ (guix build python-build-system)
                                       python-version))
                    (python-maj-min-version (python-version
                                             (assoc-ref inputs "python")))
                    (site-package (string-append
                                   (assoc-ref outputs "out")
                                   "/lib/python" python-maj-min-version
                                   "/site-packages")))
               (substitute* '("src/cython/CMakeLists.txt"
                              "src/py2geom/CMakeLists.txt")
                 (("PYTHON_LIB_INSTALL \"[^\"]*\"")
                  (format #f "PYTHON_LIB_INSTALL ~s" site-package))))))
         ,@(if (target-x86-32?)
               `((add-after 'unpack 'skip-faulty-test
                   (lambda _
                     ;; This test fails on i686 when comparing floating point
                     ;; values, probably due to excess precision.  However,
                     ;; '-fexcess-precision' is not implemented for C++ in
                     ;; GCC 10 so just skip it.
                     (substitute* "tests/CMakeLists.txt"
                       (("bezier-test") "")))))
               '()))))
    (native-inputs `(("python" ,python-wrapper)
                     ("googletest" ,googletest)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("cairo" ,cairo)
              ("pycairo" ,python-pycairo)
              ("double-conversion" ,double-conversion)
              ("glib" ,glib)
              ("gsl" ,gsl)))
    (propagated-inputs
     (list boost))               ;referred to in 2geom/pathvector.h.
    (home-page "https://gitlab.com/inkscape/lib2geom/")
    (synopsis "C++ 2D graphics library")
    (description "2geom is a C++ library of mathematics for paths, curves,
and other geometric calculations.  Designed for vector graphics, it tackles
Bézier curves, conic sections, paths, intersections, transformations, and
basic geometries.")
    ;; Because the library is linked with the GNU Scientific Library
    ;; (GPLv3+), the combined work must be licensed as GPLv3+ (see:
    ;; https://gitlab.com/inkscape/inkscape/issues/784).
    (license license:gpl3+)))

(define-public python-booleanoperations
  (package
    (name "python-booleanoperations")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "booleanOperations" version ".zip"))
       (sha256
        (base32 "1f41lb19m8azchl1aqz6j5ycbspb8jsf1cnn42hlydxd68f85ylc"))))
    (build-system python-build-system)
    (propagated-inputs (list python-fonttools-minimal python-pyclipper))
    (native-inputs
     (list python-defcon-bootstrap
           python-fontpens-bootstrap
           python-setuptools-scm
           python-pytest
           python-wheel
           unzip))
    (home-page "https://github.com/typemytype/booleanOperations")
    (synopsis "Boolean operations on paths")
    (description "Boolean operations on paths which uses a super fast
@url{http://www.angusj.com/delphi/clipper.php, polygon clipper library by
Angus Johnson}.")
    (license license:expat)))

(define-public pstoedit
  (package
    (name "pstoedit")
    (version "3.77")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pstoedit/pstoedit/"
                                  version "/pstoedit-" version ".tar.gz"))
              (sha256
               (base32
                "02av76j75g5sq3bg353yl6dlllda9ihmmk4c8hvgiscix816nv4s"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     `(("ghostscript" ,ghostscript)
       ("imagemagick" ,imagemagick)
       ("libplot" ,plotutils)
       ("libjpeg" ,libjpeg-turbo)
       ("zlib" ,zlib)))               ;else libp2edrvmagick++.so fails to link
    (home-page "http://www.pstoedit.net/")
    (synopsis "Converter for PostScript and PDF graphics")
    (description "The @code{pstoedit} utility allows translating graphics
in the PostScript or PDF (Portable Document Format) formats to various
other vector formats such as:
@itemize
@item Tgif (.obj)
@item gnuplot
@item xfig (.fig)
@item Flattened PostScript
@item DXF, a CAD (Computed-Aided Design) exchange format
@item PIC (for troff/groff)
@item MetaPost (for usage with TeX/LaTeX)
@item LaTeX2e picture
@item GNU Metafile (for use with plotutils/libplot)
@item Any format supported by ImageMagick
@end itemize")
    (license license:gpl2+)))

(define-public alembic
  (package
    (name "alembic")
    (version "1.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alembic/alembic")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04cvzr87zqx55si4j3dqiidbmfx92ja3mc1dj0v6ddvl0cwj3m7i"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DUSE_HDF5=ON")))
    (inputs
     (list hdf5 imath zlib))
    (home-page "https://www.alembic.io/")
    (synopsis "Framework for storing and sharing scene data")
    (description "Alembic is a computer graphics interchange framework.  It
distills complex, animated scenes into a set of baked geometric results.")
    (license license:bsd-3)))

(define-public mangohud
  (package
    (name "mangohud")
    (version "0.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flightlessmango/MangoHud/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19dp8l5njzl9xah0bhwlkl39vc8w2rnpvpdrhgaz3hnhz8b0r5df"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:configure-flags
      #~(list "-Duse_system_vulkan=enabled"
              "-Duse_system_spdlog=enabled"
              "-Dwith_xnvctrl=disabled"
              "-Dappend_libdir_mangohud=false"
              (string-append "-Dvulkan_datadir="
                             #$(this-package-input "vulkan-headers") "/share"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Mangohud tries to build the imgui library as a meson submodule,
          ;; so we change the dependency to the imgui input instead.
          (add-after 'unpack 'unbundle-imgui
            (lambda _
              (substitute* "meson.build"
                (("dearimgui_sp = .*")
                 "")
                (("dearimgui_sp.get_variable\\('imgui_dep'\\)")
                 (string-append
                  "declare_dependency(dependencies: "
                  "cpp.find_library('imgui'), include_directories: '"
                  #$(this-package-input "imgui") "/include/imgui')")))))
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/meson.build"
                (("\\\\\\$LIB")
                 "lib"))
              (substitute* "src/overlay.cpp"
                (("glxinfo")
                 (search-input-file inputs "bin/glxinfo")))
              (substitute* "src/loaders/loader_x11.cpp"
                (("libX11.so.6")
                 (search-input-file inputs "lib/libX11.so.6")))
              (substitute* "src/pci_ids.cpp"
                (("/usr/share/hwdata/pci.ids")
                 (search-input-file inputs "share/hwdata/pci.ids")))
              (substitute* "src/dbus.cpp"
                (("libdbus-1.so.3")
                 (search-input-file inputs "lib/libdbus-1.so.3"))))))))
    (inputs
     (list dbus
           glslang
           `(,hwdata "pci")
           imgui-1.86
           libx11
           mesa
           mesa-utils
           python-mako
           spdlog
           vulkan-headers
           vulkan-loader))
    (native-inputs (list pkg-config python))
    (home-page "https://github.com/flightlessmango/MangoHud/")
    (synopsis "Vulkan and OpenGL overlay for monitoring performance and hardware")
    (description "MangoHud is a Vulkan and OpenGL overlay for monitoring
frames per second (FPS), temperatures, CPU/GPU load and more.")
    (license license:expat)))

(define-public ogre
  (package
    (name "ogre")
    (version "13.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OGRECave/ogre")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "157vpfzivg2wf349glyd0cpbyaw1j3fm4nggban70pghql3x48kb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-imgui
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "imgui-source")
                               "../imgui-source")))
         (add-before 'configure 'pre-configure
           ;; CMakeLists.txt forces a CMAKE_INSTALL_RPATH value.  As
           ;; a consequence, we cannot suggest ours in configure flags.  Fix
           ;; it.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("set\\(CMAKE_INSTALL_RPATH .*") "")))))
       #:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (runpath
               (string-join (list (string-append out "/lib")
                                  (string-append out "/lib/OGRE"))
                            ";")))
         (list (string-append "-DCMAKE_INSTALL_RPATH=" runpath)
               "-DIMGUI_DIR=../imgui-source"
               "-DOGRE_BUILD_DEPENDENCIES=OFF"
               "-DOGRE_BUILD_TESTS=TRUE"
               "-DOGRE_INSTALL_DOCS=TRUE"
               "-DOGRE_INSTALL_SAMPLES=TRUE"
               "-DOGRE_INSTALL_SAMPLES_SOURCE=TRUE"))))
    (native-inputs `(("doxygen" ,doxygen)
                     ("imgui-source" ,(package-source imgui-1.86))
                     ("googletest" ,googletest)
                     ("pkg-config" ,pkg-config)
                     ("python" ,python)))
    (inputs (list freeimage
                  freetype
                  libxaw
                  libxrandr
                  libxt
                  mesa
                  pugixml
                  sdl2
                  zlib))
    (synopsis "Scene-oriented, flexible 3D engine written in C++")
    (description
     "OGRE (Object-Oriented Graphics Rendering Engine) is a scene-oriented,
flexible 3D engine written in C++ designed to make it easier and more intuitive
for developers to produce applications utilising hardware-accelerated 3D
graphics.")
    (home-page "https://www.ogre3d.org/")
    (license license:expat)))

(define-public openexr
  (package
    (name "openexr")
    (version "3.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/AcademySoftwareFoundation/openexr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c9vla0kbsbbhkk42jlbf94nzfb1anqh7dy9b0b3nna1qr6v4bh6"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-test-directory
                 (lambda _
                   (substitute* (list
                                 "src/test/OpenEXRUtilTest/tmpDir.h"
                                 "src/test/OpenEXRFuzzTest/tmpDir.h"
                                 "src/test/OpenEXRTest/tmpDir.h"
                                 "src/test/OpenEXRCoreTest/main.cpp")
                     (("/var/tmp")
                      "/tmp"))))
               #$@(if (target-64bit?)
                      #~()
                      #~((add-after 'patch-test-directory 'disable-broken-tests
                           (lambda _
                             ;; Disable tests that fail at least on i686-linux.
                             (substitute* '("src/test/OpenEXRCoreTest/main.cpp"
					    "src/test/OpenEXRTest/main.cpp")
                               (("TEST \\(testCompression, \"basic\"\\);")
                                "")
                               (("TEST\\( testNoCompression, \"core_compression\" \\);")
                                "")
                               (("TEST\\( testRLECompression, \"core_compression\" \\);")
                                "")
                               (("TEST\\( testZIPCompression, \"core_compression\" \\);")
                                "")
                               (("TEST\\( testZIPSCompression, \"core_compression\" \\);")
                                "")
                               (("TEST\\( testB44Compression, \"core_compression\" \\);")
                                "")
                               (("TEST\\( testB44ACompression, \"core_compression\" \\);")
                                "")
                               (("TEST \\(testOptimizedInterleavePatterns, \"basic\"\\);")
                                "")))))))))
    (inputs (list imath zlib))
    (home-page "https://www.openexr.com/")
    (synopsis "High-dynamic-range file format library")
    (description
     "OpenEXR provides the specification and reference implementation of the
EXR file format.  The purpose of EXR format is to accurately and efficiently
represent high-dynamic-range scene-linear image data and associated metadata,
with strong support for multi-part, multi-channel use cases.")
    (license license:bsd-3)))

(define-public openexr-2
  (package
    (name "openexr")
    (version (package-version ilmbase))
    (source (origin
              (inherit (package-source ilmbase))
              (file-name (git-file-name "openexr" version))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "OpenEXR")
             #t))
         (add-after 'change-directory 'patch-test-directory
           (lambda _
             (substitute* '("IlmImfFuzzTest/tmpDir.h"
                            "IlmImfTest/tmpDir.h"
                            "IlmImfUtilTest/tmpDir.h")
               (("/var/tmp") "/tmp"))))
         (add-after 'change-directory 'increase-test-timeout
           (lambda _
             ;; On some architectures, we need to override the CTest default
             ;; timeout of 1500 seconds for the OpenEXR.IlmImf test.
             (substitute* "IlmImfTest/CMakeLists.txt"
               (("add_test\\(NAME OpenEXR\\.IlmImf.*" all)
                (string-append
                 all
                 "set_tests_properties(OpenEXR.IlmImf PROPERTIES TIMEOUT 15000)")))
             #t))
         ,@(if (not (target-64bit?))
               `((add-after 'change-directory 'disable-broken-test
                   (lambda _
                     (substitute* "IlmImfTest/main.cpp"
                       ;; This test fails on i686. Upstream developers suggest
                       ;; that this test is broken on i686 and can be safely
                       ;; disabled:
                       ;; https://github.com/openexr/openexr/issues/67#issuecomment-21169748
                       ((".*testOptimizedInterleavePatterns.*") "")
                       ;; This one fails similarly on i686.
                       ((".*testCompression.*") "")))))
               '()))))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list ilmbase ;used in public headers
           zlib))                           ;OpenEXR.pc reads "-lz"
    (home-page (package-home-page openexr))
    (synopsis (package-synopsis openexr))
    (description (package-description openexr))
    (license (package-license openexr))))

(define-public openimageio
  (package
    (name "openimageio")
    (version "2.2.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenImageIO/oiio")
                    (commit (string-append "Release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aicxbshzv1g9d8d08vsj1a9klaycxaifvvp565qjv70wyma2vkr"))))
    (build-system cmake-build-system)
    ;; FIXME: To run all tests successfully, test image sets from multiple
    ;; third party sources have to be present.  For details see
    ;; <https://github.com/OpenImageIO/oiio/blob/master/INSTALL.md>
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DUSE_EXTERNAL_PUGIXML=1")))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("boost" ,boost)
       ("fmt" ,fmt-8)
       ("libheif" ,libheif)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("giflib" ,giflib)
       ("openexr" ,openexr-2)
       ("ilmbase" ,ilmbase)
       ("pugixml" ,pugixml)
       ("python" ,python-wrapper)
       ("pybind11" ,pybind11)
       ("robin-map" ,robin-map)
       ("zlib" ,zlib)))
    (synopsis "C++ library for reading and writing images")
    (description
     "OpenImageIO is a library for reading and writing images, and a bunch of
related classes, utilities, and applications.  There is a particular emphasis
on formats and functionality used in professional, large-scale animation and
visual effects work for film.")
    (home-page "https://www.openimageio.org")
    (license license:bsd-3)))

(define-public openscenegraph
  (package
    (name "openscenegraph")
    (version "3.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openscenegraph/OpenSceneGraph")
             (commit (string-append "OpenSceneGraph-" version))))
       (sha256
        (base32 "00i14h82qg3xzcyd8p02wrarnmby3aiwmz0z43l50byc9f8i05n1"))
       (file-name (git-file-name name version))))
    (properties
     `((upstream-name . "OpenSceneGraph")
       (output-synopsis "pluginlib" "Plugins as shared libraries")))
    (build-system cmake-build-system)
    (outputs (list "out" "pluginlib"))
    (arguments
     (list
      #:tests? #f                      ; no test target available
      ;; Without this flag, 'rd' will be added to the name of the
      ;; library binaries and break linking with other programs.
      #:build-type "Release"
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_RPATH="
                             #$output "/lib:"
                             #$output "/lib64"))
      #:modules `((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 regex))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'copy-plugins
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (pluginlib (assoc-ref outputs "pluginlib")))
                (mkdir-p (string-append pluginlib "/lib/pkgconfig"))
                (with-directory-excursion (string-append out "/lib/osgPlugins-"
                                                         #$version)
                  (for-each
                   (lambda (lib)
                     (let ((blib (basename lib))
                           (m (string-match "([^/]*)\\.so$" lib)))
                       (symlink (canonicalize-path lib)
                                (string-append pluginlib "/lib/lib" blib))
                       (call-with-output-file (string-append
                                               pluginlib
                                               "/lib/pkgconfig/"
                                               (match:substring m 1) ".pc")
                         (lambda (port)
                           (format port "libdir=~a/lib~%" pluginlib)
                           (newline port)
                           (format port "Name: ~a~%" (match:substring m 1))
                           (format port "Version: ~a~%" #$version)
                           (display "Description: A plugin for openscenegraph\n"
                                    port)
                           (display "Requires: openscenegraph\n" port)
                           (format port "Libs: -L${libdir} -l~a~%"
                                   (match:substring m 1))))))
                   (find-files "." "\\.so")))))))))
    (native-inputs
     (list pkg-config unzip))
    (inputs
     `(("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)       ; required for the JPEG texture plugin.
       ("jasper" ,jasper)
       ("librsvg" ,librsvg)
       ("libxrandr" ,libxrandr)
       ("ffmpeg" ,ffmpeg-4)
       ("mesa" ,mesa)))
    (synopsis "High-performance real-time graphics toolkit")
    (description
     "The OpenSceneGraph is a high-performance 3D graphics toolkit
used by application developers in fields such as visual simulation, games,
virtual reality, scientific visualization and modeling.")
    (home-page "http://www.openscenegraph.org")
    ;; The 'LICENSE' file explains that the source is licensed under
    ;; LGPL 2.1, but with 4 exceptions. This version is called OSGPL.
    (license license:lgpl2.1)))

(define-public gr-framework
  (package
    (name "gr-framework")
    (version "0.58.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sciapp/gr")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0q1rz4iyxbh0dc22y4w28ry3hr0yypdwdm6pw2zlwgjya7wkbvsw"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "3rdparty")
            #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))    ; no test target
    (inputs
     `(("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("ghostscript" ,ghostscript)
       ("glfw" ,glfw)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("libxt" ,libxt)
       ("pixman" ,pixman)
       ("qtbase" ,qtbase-5)
       ("qhull" ,qhull)
       ("zlib" ,zlib)))
    (home-page "https://gr-framework.org/")
    (synopsis "Graphics library for visualisation applications")
    (description "GR is a universal framework for cross-platform visualization
applications.  It offers developers a compact, portable and consistent graphics
library for their programs.  Applications range from publication quality 2D
graphs to the representation of complex 3D scenes.  GR is essentially based on
an implementation of a @acronym{GKS, Graphical Kernel System}.  As a
self-contained system it can quickly and easily be integrated into existing
applications (i.e. using the @code{ctypes} mechanism in Python or @code{ccall}
in Julia).")
    (license license:expat)))

(define-public openmw-openscenegraph
  ;; OpenMW prefers its own fork of openscenegraph:
  ;; https://wiki.openmw.org/index.php?title=Development_Environment_Setup#OpenSceneGraph.
  (let ((commit "36a962845a2c87a6671fd822157e0729d164e940"))
    (hidden-package
     (package
       (inherit openscenegraph)
       (version (git-version "3.6" "1" commit))
       (outputs (list "out"))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/OpenMW/osg/")
                (commit commit)))
          (file-name (git-file-name (package-name openscenegraph) version))
          (sha256
           (base32
            "05yhgq3qm5q277y32n5sf36vx5nv5qd3zlhz4csgd3a6190jrnia"))))
       (arguments
        (substitute-keyword-arguments (package-arguments openscenegraph)
          ((#:configure-flags flags)
           ;; As per the above wiki link, the following plugins are enough:
           #~(append
              '("-DBUILD_OSG_PLUGINS_BY_DEFAULT=0"
                "-DBUILD_OSG_PLUGIN_OSG=1"
                "-DBUILD_OSG_PLUGIN_DDS=1"
                "-DBUILD_OSG_PLUGIN_TGA=1"
                "-DBUILD_OSG_PLUGIN_BMP=1"
                "-DBUILD_OSG_PLUGIN_JPEG=1"
                "-DBUILD_OSG_PLUGIN_PNG=1"
                "-DBUILD_OSG_DEPRECATED_SERIALIZERS=0"
                ;; The jpeg plugin requires conversion between integers and booleans
                "-DCMAKE_CXX_FLAGS=-fpermissive")
              #$flags))
          ((#:phases phases)
           #~(modify-phases #$phases
               (delete 'copy-plugins)))))))))

(define-public povray
  (package
    (name "povray")
    (version "3.7.0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/POV-Ray/povray")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q114n4m3r7qy3yn954fq7p46rg7ypdax5fazxr9yj1jklf1lh6z"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libraries.
                  (delete-file-recursively "libraries")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     `(("boost" ,boost)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("openexr" ,openexr-2)
       ("sdl" ,sdl)
       ("zlib" ,zlib)))
    (arguments
     '(#:configure-flags
       (list "COMPILED_BY=Guix"
             (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost") "/lib")
             "--disable-optimiz-arch")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'run-prebuild
           (lambda _
             (setenv "HOME" (getcwd))
             (with-directory-excursion "unix"
               (substitute* "prebuild.sh"
                 (("/bin/sh") (which "sh")))
               (invoke "sh" "prebuild.sh"))
             #t))
         ;; The bootstrap script is run by the prebuild script in the
         ;; "run-prebuild" phase.
         (delete 'bootstrap))))
    (synopsis "Tool for creating three-dimensional graphics")
    (description
     "@code{POV-Ray} is short for the Persistence of Vision Raytracer, a tool
for producing high-quality computer graphics.  @code{POV-Ray} creates
three-dimensional, photo-realistic images using a rendering technique called
ray-tracing.  It reads in a text file containing information describing the
objects and lighting in a scene and generates an image of that scene from the
view point of a camera also described in the text file.  Ray-tracing is not a
fast process by any means, but it produces very high quality images with
realistic reflections, shading, perspective and other effects.")
    (home-page "http://www.povray.org/")
    (license license:agpl3+)))

(define-public ctl
  (package
    (name "ctl")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ampas/CTL/archive/ctl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gg04pyvw0m398akn0s1l07g5b1haqv5na1wpi5dii1jjd1w3ynp"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))                    ;no 'test' target

    ;; Headers include OpenEXR and IlmBase headers.
    (propagated-inputs (list openexr-2))

    (home-page "https://ampasctl.sourceforge.net")
    (synopsis "Color Transformation Language")
    (description
     "The Color Transformation Language, or CTL, is a small programming
language that was designed to serve as a building block for digital color
management systems.  CTL allows users to describe color transforms in a
concise and unambiguous way by expressing them as programs.  In order to apply
a given transform to an image, the color management system instructs a CTL
interpreter to load and run the CTL program that describes the transform.  The
original and the transformed image constitute the CTL program's input and
output.")

    ;; The web site says it's under a BSD-3 license, but the 'LICENSE' file
    ;; and headers use different wording.
    (license (license:non-copyleft "file://LICENSE"))))

(define-public brdf-explorer
  ;; There are no release tarballs, and not even tags in the repo,
  ;; so use the latest revision.
  (let ((commit "5b2cd46f38a06e47207fa7229b72d37beb945019")
        (revision "1"))
    (package
      (name "brdf-explorer")
      (version (string-append "1.0.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wdas/brdf")
                      (commit commit)))
                (sha256
                 (base32
                  "06vzbiajzbi2xl8jlff5d45bc9wd68i3jdndfab1f3jgfrd8bsgx"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'configure
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out")))
                          (invoke "qmake"
                                  (string-append "prefix=" out)))))
                    (add-after 'install 'wrap-program
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin"))
                               (data (string-append
                                      out "/share/brdf")))
                          (with-directory-excursion bin
                            (rename-file "brdf" ".brdf-real")
                            (call-with-output-file "brdf"
                              (lambda (port)
                                (format port "#!/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.brdf-real~%"
                                        data bin)))
                            (chmod "brdf" #o555)))
                        #t)))))
      (native-inputs
       (list qttools-5)) ;for 'qmake'
      (inputs
       (list qtbase-5 mesa glew freeglut zlib))
      (home-page "https://www.disneyanimation.com/technology/brdf.html")
      (synopsis
       "Analyze bidirectional reflectance distribution functions (BRDFs)")
      (description
       "BRDF Explorer is an application that allows the development and analysis
of bidirectional reflectance distribution functions (BRDFs).  It can load and
plot analytic BRDF functions (coded as functions in OpenGL's GLSL shader
language), measured material data from the MERL database, and anisotropic
measured material data from MIT CSAIL.  Graphs and visualizations update in
real time as parameters are changed, making it a useful tool for evaluating
and understanding different BRDFs (and other component functions).")
      (license license:ms-pl))))

(define-public agg
  (package
    (name "agg")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "ftp://ftp.fau.de/gentoo/distfiles/agg-"
                          version ".tar.gz")
                         (string-append
                          "ftp://ftp.ula.ve/gentoo/distfiles/agg-"
                          version ".tar.gz")

                         ;; Site was discontinued.
                         (string-append "http://www.antigrain.com/agg-"
                                        version ".tar.gz")))
              (sha256
               (base32 "07wii4i824vy9qsvjsgqxppgqmfdxq0xa87i5yk53fijriadq7mb"))
              (patches (search-patches "agg-am_c_prototype.patch"
                                       "agg-2.5-gcc8.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--x-includes=" (assoc-ref %build-inputs "libx11")
                            "/include")
             (string-append "--x-libraries=" (assoc-ref %build-inputs "libx11")
                            "/lib")
             "--disable-examples")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             ;; let's call configure from configure phase and not now
             (substitute* "autogen.sh" (("./configure") "# ./configure"))
             (invoke "sh" "autogen.sh"))))))
    (native-inputs
     (list pkg-config libtool autoconf automake))
    (inputs
     (list libx11 freetype sdl))

    ;; Antigrain.com was discontinued.
    (home-page "https://agg.sourceforge.net/antigrain.com/index.html")
    (synopsis "High-quality 2D graphics rendering engine for C++")
    (description
     "Anti-Grain Geometry is a high quality rendering engine written in C++.
It supports sub-pixel resolutions and anti-aliasing.  It is also a library for
rendering @acronym{SVG, Scalable Vector Graphics}.")
    (license license:gpl2+)))

(define-public python-pastel
  (package
    (name "python-pastel")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pastel" version))
       (sha256
        (base32
         "0dnaw44ss10i10z4ksy0xljknvjap7rb7g0b8p6yzm5x4g2my5a6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _ (invoke "pytest" "pastel" "tests/"))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/sdispater/pastel")
    (synopsis "Library to colorize strings in your terminal")
    (description "Pastel is a simple library to help you colorize strings in
your terminal.")
    (license license:expat)))

(define-public facedetect
  (let ((commit "5f9b9121001bce20f7d87537ff506fcc90df48ca")
        (revision "0"))
    (package
      (name "facedetect")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/wavexx/facedetect")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1jiz72y3ykkxkiij1qqjf45gxg223sghkjir7sr663x91kviwkjd"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~`(("facedetect" "bin/facedetect")
            ("README.rst" ,(string-append "share/doc/" #$name
                                          "-" #$version "/README.rst")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "facedetect"
                  (("^DATA_DIR = .*")
                   (string-append "DATA_DIR = '"
                                  #$opencv "/share/opencv"
                                  #$(version-major (package-version opencv))
                                  "'\n")))))
            (add-after 'install 'wrap
              (lambda _
                (let ((program (string-append #$output "/bin/facedetect")))
                  (patch-shebang program)
                  (wrap-program program
                    `("GUIX_PYTHONPATH" prefix
                      ,(search-path-as-string->list
                        (getenv "GUIX_PYTHONPATH"))))))))))
      (inputs
       (list bash-minimal
             opencv
             python
             python-numpy))
      (home-page "https://www.thregr.org/~wavexx/software/facedetect/")
      (synopsis "Face detector")
      (description "@code{facedetect} is a face detector for batch processing.
It answers the question: \"Is there a face in this image?\" and gives back
either an exit code or the coordinates of each detect face in the standard
output.  @code{facedetect} is used in software such as @code{fgallery} to
improve the thumbnail cutting region, so that faces are always centered.")
      (license license:gpl2+))))

(define-public fgallery
  (package
    (name "fgallery")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://www.thregr.org/~wavexx/software/fgallery/releases/"
                "fgallery-" version ".zip"))
              (sha256
               (base32
                "0zf6r88m2swgj1ylgh3qa1knzb4if501hzvga37h9psy8k179w8n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (script (string-append out "/bin/fgallery"))
                    (perl5lib (getenv "PERL5LIB")))
               (define (bin-directory input-name)
                 (string-append (assoc-ref inputs input-name) "/bin"))

               (invoke "make" "install" (string-append "PREFIX=" out))

               ;; fgallery copies files from store when it is run. The
               ;; read-only permissions from the store directories will cause
               ;; fgallery to fail. Do not preserve file attributes when
               ;; copying files to prevent it.
               (substitute* script
                 (("'cp'")
                  "'cp', '--no-preserve=all'"))

               (wrap-program script
                 `("PATH" ":" prefix
                   ,(map bin-directory '("imagemagick"
                                         "lcms"
                                         "facedetect"
                                         "fbida"
                                         "libjpeg-turbo"
                                         "zip"
                                         "jpegoptim"
                                         "pngcrush"
                                         "p7zip")))
                 `("PERL5LIB" ":" prefix (,perl5lib)))))))))
    (native-inputs
     (list unzip))
    (inputs
     (list bash-minimal
           imagemagick
           lcms
           facedetect
           fbida
           libjpeg-turbo
           zip
           perl
           perl-cpanel-json-xs
           perl-image-exiftool
           jpegoptim
           pngcrush
           p7zip))
    (home-page "https://www.thregr.org/~wavexx/software/fgallery/")
    (synopsis "Static photo gallery generator")
    (description
     "FGallery is a static, JavaScript photo gallery generator with minimalist
look.  The result can be uploaded on any web server without additional
requirements.")
    (license license:gpl2+)))

(define-public opensubdiv
  (package
    (name "opensubdiv")
    (version "3.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PixarAnimationStudios/OpenSubdiv")
                    (commit (string-append "v" (string-join (string-split version #\.)
                                                            "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cippg6aqc5dlya1cmh3908pwssrg52fwgyylnvz5343yrxmgk12"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-glew-location
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "GLEW_LOCATION" (assoc-ref inputs "glew"))
                      #t))
                  (add-before 'check 'start-xorg-server
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; The test suite requires a running X server.
                      (system "Xvfb :1 &")
                      (setenv "DISPLAY" ":1")
                      #t)))))
    (native-inputs
     (list xorg-server-for-tests))
    (inputs
     (list glew
           libxrandr
           libxcursor
           libxinerama
           libxi
           zlib
           glfw))
    (home-page "https://graphics.pixar.com/opensubdiv/")
    (synopsis "High performance subdivision surface evaluation")
    (description "OpenSubdiv is a set of libraries that implement high
performance subdivision surface (subdiv) evaluation on massively parallel CPU
and GPU architectures.")
    (license license:asl2.0)))

(define-public opencsg
  (let ((dot-to-dash (lambda (c) (if (char=? c #\.) #\- c))))
    (package
      (name "opencsg")
      (version "1.4.2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/floriankirsch/OpenCSG")
               (commit (string-append "opencsg-"
                                      (string-map dot-to-dash version)
                                      "-release"))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00m4vs6jn3scqczscc4591l1d6zg6anqp9v1ldf9ymf70rdyvm7m"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/Makefile"
                 (("/usr/local") (assoc-ref outputs "out")))
               #t))
           (add-before 'build 'skip-example
             (lambda _ (chdir "src") #t)))))
      (inputs
       (list glew freeglut))
      (synopsis "Library for rendering Constructive Solid Geometry (CSG)")
      (description
       "OpenCSG is a library for rendering Constructive Solid Geometry (CSG) using
OpenGL.  CSG is an approach for modeling complex 3D-shapes using simpler ones.
For example, two shapes can be combined by uniting them, by intersecting them,
or by subtracting one shape from the other.")
      (home-page "https://www.opencsg.org/")
      (license license:gpl2))))

(define-public coin3D
  (package
    (name "coin3D")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coin3d/coin")
             (commit (string-append "Coin-" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ayg0hl8wanhadahm5xbghghxw1qjwqbrs3dl3ngnff027hsyf8p"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete binaries
           (for-each delete-file
                     '("cfg/csubst.exe"
                       "cfg/wrapmsvc.exe"))
           ;; Delete references to packaging tool cpack. Otherwise the build
           ;; fails with "add_subdirectory given source "cpack.d" which is not
           ;; an existing directory."
           (substitute* "CMakeLists.txt"
             ((".*cpack.d.*") ""))
           #t))))
    (build-system cmake-build-system)
    (native-inputs
     (list doxygen graphviz))
    (inputs
     (list boost freeglut glew))
    (arguments
     `(#:configure-flags
       (list
        "-DCOIN_BUILD_DOCUMENTATION_MAN=ON"
        (string-append "-DBOOST_ROOT="
                       (assoc-ref %build-inputs "boost")))))
    (home-page "https://github.com/coin3d/coin")
    (synopsis
     "High-level 3D visualization library with Open Inventor 2.1 API")
    (description
     "Coin is a 3D graphics library with an Application Programming Interface
based on the Open Inventor 2.1 API.  For those who are not familiar with Open
Inventor, it is a scene-graph based retain-mode rendering and model interaction
library, written in C++, which has become the de facto standard graphics
library for 3D visualization and visual simulation software in the scientific
and engineering community.")
    (license license:bsd-3)))

(define-deprecated coin3D-4 coin3D)
(export coin3D-4)

(define-public skia
  ;; Releases follow those of Chromium, about every 6 weeks.  The release
  ;; version can be found on this page:
  ;; https://skia.org/docs/user/release/release_notes/.  The commit used
  ;; should be the last commit, as recommended at
  ;; https://skia.org/docs/user/release/.
  (let ((version "98")
        (revision "0")
        (commit "55c56abac381e1ae3f0116c410bed81b05e0a38a"))
    (package
      (name "skia")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://skia.googlesource.com/skia.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ldns2j1g2wj2phlxr9zqkdgs5g64pisxhwxcrq9ijn8a3jhafr2"))))
      (build-system gnu-build-system)   ;actually GN + Ninja
      (arguments
       (list
        ;; Running the test suite would require 'dm'; unfortunately the tool
        ;; can only be built for debug builds, which require fetching third
        ;; party sources.
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "BUILD.gn"
                  ;; Workaround a bug in the zlib third_party definition, that
                  ;; fails the build even when zlib is found from the system.
                  (("deps = \\[ \"//third_party/zlib\" ]")
                   "deps = []"))
                (invoke "gn" "gen" "build"
                        (string-append
                         ;;
                         "--args="
                         "cc=\"gcc\" "              ;defaults to 'cc'
                         "is_official_build=true "  ;to use system libraries
                         "is_component_build=true " ;build as a shared library
                         ;; Specify where locate the harfbuzz and freetype
                         ;; includes.
                         (format #f "extra_cflags=[\"-I~a\",\"-I~a\"] "
                                 (search-input-directory inputs
                                                         "include/harfbuzz")
                                 (search-input-directory inputs
                                                         "include/freetype2"))
                         ;; Otherwise the validate-runpath phase fails.
                         "extra_ldflags=[\"-Wl,-rpath=" #$output "/lib\"] "
                         ;; Disabled, otherwise the build system attempts to
                         ;; download the SDK at build time.
                         "skia_use_dng_sdk=false "))))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (let ((job-count (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")))
                  (invoke "ninja" "-j" job-count "-C" "build"))))
            (replace 'install
              (lambda _
                ;; Install headers.
                (for-each (lambda (h)
                            (install-file h (string-append
                                             #$output "/include/skia/"
                                             (dirname h))))
                          (find-files "." "\\.h$"))
                ;; Install libraries.
                (for-each (lambda (lib)
                            (install-file lib (string-append #$output "/lib")))
                          (find-files "build" "^lib.*\\.(a|so)"))
                ;; This pkgconfig file is useful at least to the
                ;; python-skia-pathops package.
                (define skia.pc (string-append #$output
                                               "/lib/pkgconfig/skia.pc"))
                (mkdir-p (dirname skia.pc))
                (call-with-output-file skia.pc
                  (lambda (port)
                    (format port "\
prefix=~a
exec_prefix=${prefix}
libdir=${prefix}/lib
includedir=${prefix}/include/skia

Name: skia
Description: 2D graphic library for drawing text, geometries and images.
URL: https://skia.org/
Version: ~a
Libs: -L${libdir} -lskia
Cflags: -I${includedir}~%" #$output #$version))))))))
      (native-inputs (list gn libjpeg-turbo ninja pkg-config python-wrapper))
      (inputs (list expat fontconfig freetype harfbuzz mesa libwebp zlib))
      (home-page "https://skia.org/")
      (synopsis "2D graphics library")
      (description
       "Skia is a 2D graphics library for drawing text, geometries, and images.
It supports:
@itemize
@item 3x3 matrices with perspective
@item antialiasing, transparency, filters
@item shaders, xfermodes, maskfilters, patheffects
@item subpixel text
@end itemize")
      (license license:bsd-3))))

(define-public superfamiconv
  (package
    (name "superfamiconv")
    (version "0.8.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Optiroc/SuperFamiconv")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0848szv6a2b8wdganh6mw5i8vn8cqvn1kbwzx7mb9wlrf5wzqn37"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (bindir (string-append outdir "/bin")))
               (install-file "bin/superfamiconv" bindir)
               #t))))))
    (home-page "https://github.com/Optiroc/SuperFamiconv")
    (synopsis "Tile graphics converter supporting SNES, Game Boy Color
and PC Engine formats")
    (description "SuperFamiconv is a converter for tiled graphics, supporting
the graphics formats of the SNES, Game Boy Color and PC Engine game consoles.
Automated palette selection is supported.")
    (license license:expat)))

(define-public drawpile
  ;; This commit fix building with libmicrohttpd>=0.71.
  (let ((commit "ed1a75deb113da2d1df91a28f557509c4897130e")
        (revision "1"))
    (package
      (name "drawpile")
      (version (string-append "2.1.17-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/drawpile/Drawpile")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1y21h1hk9ipkjvhjgas0c5hkjyan92vsxbxrn60c906hzqln2fr1"))))
      (build-system qt-build-system)
      (arguments
       '(#:configure-flags
         (list "-DTESTS=ON" "-DTOOLS=ON" "-DKIS_TABLET=ON")))
      (native-inputs
       (list extra-cmake-modules pkg-config))
      (inputs
       (list giflib
             karchive
             kdnssd
             libmicrohttpd
             libsodium
             libvpx
             libxi
             ;; ("miniupnpc" ,miniupnpc) ;segfaults for some reason
             qtbase-5
             qtkeychain
             qtmultimedia-5
             qtsvg-5
             qtx11extras))
      (home-page "https://drawpile.net")
      (synopsis "Collaborative drawing program")
      (description "Drawpile is a drawing program that allows share the canvas
with other users in real time.

Some feature highlights:
@itemize
@item Shared canvas using the built-in server or a dedicated server
@item Record, play back and export drawing sessions
@item Simple animation support
@item Layers and blending modes
@item Text layers
@item Supports pressure sensitive Wacom tablets
@item Built-in chat
@item Supports OpenRaster file format
@item Encrypted connections using SSL
@item Automatic port forwarding with UPnP
@end itemize\n")
      (license license:gpl3+))))

(define-public openxr
  (package
    (name "openxr")
    (version "1.0.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KhronosGroup/OpenXR-SDK")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled jsoncpp.
           (delete-file-recursively "src/external/jsoncpp")))
       (sha256
        (base32 "0s66xgwkdj5vn05l493hqydrxfpxxidd6mcb8l7l5awhn88cy16f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (native-inputs
     (list pkg-config python shaderc vulkan-headers))
    (inputs
     (list jsoncpp mesa vulkan-loader wayland))
    (home-page "https://www.khronos.org/openxr/")
    (synopsis "Generated headers and sources for OpenXR loader")
    (description "This package contains OpenXR headers, as well as source code
and build scripts for the OpenXR loader.")
    ;; Dual licensed.  Either license applies.
    (license (list license:asl2.0 license:expat))))

(define-public monado
  (package
    (name "monado")
    (version "21.0.0")
    (source (origin
          (method url-fetch)
          (uri (string-append "https://gitlab.freedesktop.org/" name "/"
                              name "/-/archive/v" version "/"
                              name "-v" version ".tar.bz2"))
          (sha256
           (base32
            "0n04k7a8b0i8ga0kbzh7qxmvni1ijawgk98s83519vxg4d0yyjbq"))))
    (build-system meson-build-system)
    (inputs
     (list ffmpeg
           glslang
           eudev
           libusb
           libxcb
           libxrandr
           mesa
           python
           v4l-utils
           vulkan-loader))
    (native-inputs
     (list eigen pkg-config vulkan-headers))
    (arguments
     `(#:configure-flags
       (list "-Dinstall-active-runtime=false")))
    (home-page "https://monado.freedesktop.org/")
    (synopsis "OpenXR runtime")
    (description "Monado is an OpenXR runtime delivering immersive experiences
such as VR and AR on mobile, PC/desktop, and any other device.  Monado aims to be
a complete and conforming implementation of the OpenXR API made by Khronos.")
    (license license:boost1.0)))

(define-public azpainter
  (package
    (name "azpainter")
    (version "3.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/azelpg/azpainter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lk74drrksk340fzyzvrq0ixwj498adshbp505cj163qsqnndj7y"))))
    (build-system gnu-build-system) ;actually a home grown build system
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (invoke "./configure"
                           (string-append "--prefix="
                                          #$output))))
               (replace 'build
                 (lambda* (#:key parallel-build? #:allow-other-keys)
                   (let ((job-count (if parallel-build?
                                        (number->string (parallel-job-count))
                                        "1")))
                     (invoke "ninja" "-j" job-count "-C" "build"))))
               (add-before 'install 'disable-cache-generation
                 (lambda _
                   (setenv "DESTDIR" "/") #t))
               (replace 'install
                 (lambda _
                   (invoke "ninja" "-C" "build" "install"))))))
    (inputs (list fontconfig
                  freetype
                  libjpeg-turbo
                  libpng
                  libtiff
                  libwebp
                  libx11
                  libxcursor
                  libxext
                  libxi
                  zlib))
    (native-inputs (list ninja pkg-config))
    (home-page "http://azsky2.html.xdomain.jp/soft/azpainter.html")
    (synopsis "Paint software for editing illustrations and images")
    (description
     "AzPainter is a lightweight full color painting application for editing
illustrations and images.

Features include:
@itemize
@item Layers
@item Many artistic filters
@item Good range of selection tools
@item Pen pressure support with automatic brush size adjustment
@item Support for 16-bit color images with transparency (RGBA)
@item Support for image formats like PSD, PNG, JPEG, TIFF, WebP
@end itemize
")
    (license license:gpl3+)))

(define-public discregrid
  (let ((commit "4c27e1cc88be828c6ac5b8a05759ac7e01cf79e9")
        (revision "0"))
    (package
      (name "discregrid")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/InteractiveComputerGraphics/Discregrid")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01cwfpw19rc9k5glx9dhnqpihd0is28a9b53qvzp5kgjmdq2v1p0"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              (delete-file-recursively "extern/cxxopts")
              (substitute* '("cmd/discrete_field_to_bitmap/main.cpp"
                             "cmd/generate_density_map/main.cpp"
                             "cmd/generate_sdf/main.cpp")
                (("^#include <cxxopts/cxxopts\\.hpp>")
                 "#include <cxxopts.hpp>")
                (("cxxopts::OptionException")
                 "cxxopts::exceptions::parsing"))))))
      (build-system cmake-build-system)
      (outputs '("out" "bin"))
      (arguments
       (list #:tests? #f                ; No tests
             #:configure-flags
             #~(list (string-append "-DCMAKE_INSTALL_BINDIR="
                                    #$output:bin "/bin")
                     ;; Bespoke version of BUILD_SHARED_LIBS.
                     "-DBUILD_AS_SHARED_LIBS=ON")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-cmake
                   (lambda _
                     (let ((port (open-file "cmd/CMakeLists.txt" "a")))
                       (display "install(TARGETS
  DiscreteFieldToBitmap
  GenerateDensityMap
  GenerateSDF)
"
                                port)
                       (close-port port)))))))
      (inputs
       (list cxxopts eigen))
      (home-page "https://github.com/InteractiveComputerGraphics/Discregrid")
      (synopsis "Discretize functions on regular grids")
      (description "Discregrid is a C++ library for the parallel discretization
of (preferably smooth) functions on regular grids.  It generates a (cubic)
polynomial discretization given a box-shaped domain, a grid resolution, and a
3D scalar field.  The library can also serialize and deserialize the generated
discrete grid, and compute and discretize the signed distance field
corresponding to a triangle mesh.  The following programs are included with
Discregrid:

@itemize
@item @code{GenerateSDF}: Computes a discrete (cubic) signed distance field
from a triangle mesh in OBJ format.

@item @code{DiscreteFieldToBitmap}: Generates an image in bitmap format of a
two-dimensional slice of a previously computed discretization.

@item @code{GenerateDensityMap}: Generates a density map from a previously
generated discrete signed distance field using the cubic spline kernel.
@end itemize")
      (license license:expat))))

(define-public mmg
  (package
    (name "mmg")
    (version "5.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MmgTools/mmg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "173biz5skbwg27i5w6layg7mydjzv3rmi1ywhra4rx9rjf5c0cc5"))))
    (build-system cmake-build-system)
    (outputs '("out" "lib" "doc"))
    (arguments
     (list #:configure-flags
           #~(list (string-append "-DCMAKE_INSTALL_PREFIX=" #$output:lib)
                   (string-append "-DCMAKE_INSTALL_RPATH=" #$output:lib "/lib")
                   ;; The build doesn't honor -DCMAKE_INSTALL_BINDIR, hence
                   ;; the adjust-bindir phase.
                   ;;(string-append "-DCMAKE_INSTALL_BINDIR=" #$output "/bin")
                   "-DBUILD_SHARED_LIBS=ON"
                   "-DBUILD_TESTING=ON"
                   ;; The longer tests are for continuous integration and
                   ;; depend on input data which must be downloaded.
                   "-DONLY_VERY_SHORT_TESTS=ON"
                   ;; TODO: Add Elas (from
                   ;; https://github.com/ISCDtoolbox/LinearElasticity).
                   "-DUSE_ELAS=OFF"
                   ;; TODO: Figure out how to add VTK to inputs without
                   ;; causing linking errors in ASLI of the form:
                   ;;
                   ;;   ld: /gnu/store/…-vtk-9.0.1/lib/libvtkWrappingPythonCore-9.0.so.1:
                   ;;     undefined reference to `PyUnicode_InternFromString'
                   ;;
                   ;; Also, adding VTK to inputs requires adding these as well:
                   ;;
                   ;;   double-conversion eigen expat freetype gl2ps glew hdf5
                   ;;   jsoncpp libjpeg-turbo libpng libtheora libtiff libx11
                   ;;   libxml2 lz4 netcdf proj python sqlite zlib
                   "-DUSE_VTK=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'build-doc
                 (lambda _
                   ;; Fontconfig wants to write to a cache directory.
                   (setenv "HOME" "/tmp")
                   (invoke "make" "doc")))
               (add-after 'install 'install-doc
                 (lambda _
                   (copy-recursively
                    "../source/doc/man" (string-append #$output
                                                       "/share/man/man1"))
                   (copy-recursively
                    "doc" (string-append #$output:doc "/share/doc/"
                                         #$name "-" #$version))))
               (add-after 'install 'adjust-bindir
                 (lambda _
                   (let ((src (string-append #$output:lib "/bin"))
                         (dst (string-append #$output "/bin")))
                     (copy-recursively src dst)
                     (delete-file-recursively src))))
               ;; Suffixing program names with build information, i.e.,
               ;; optimization flags and whether debug symbols were generated,
               ;; is unusual and fragilizes scripts calling these programs.
               (add-after 'adjust-bindir 'fix-program-names
                 (lambda _
                   (with-directory-excursion (string-append #$output "/bin")
                     (rename-file "mmg2d_O3d" "mmg2d")
                     (rename-file "mmg3d_O3d" "mmg3d")
                     (rename-file "mmgs_O3d" "mmgs")))))))
    (native-inputs
     ;; For the documentation
     (list doxygen graphviz
           ;; TODO: Fix failing LaTeX invocation (which results in equations
           ;; being inserted literally into PNGs rather than being typeset).
           ;;texlive-tiny

           perl))                            ;used to generate Fortran headers
    (inputs
     (list scotch))
    (home-page "http://www.mmgtools.org/")
    (synopsis "Surface and volume remeshers")
    (description "Mmg is a collection of applications and libraries for
bidimensional and tridimensional surface and volume remeshing.  It consists
of:

@itemize
@item the @code{mmg2d} application and library: mesh generation from a set of
edges, adaptation and optimization of a bidimensional triangulation and
isovalue discretization;

@item the @code{mmgs} application and library: adaptation and optimization of
a surface triangulation and isovalue discretization;

@item the @code{mmg3d} application and library: adaptation and optimization of
a tetrahedral mesh, isovalue discretization and Lagrangian movement;

@item the @code{mmg} library gathering the @code{mmg2d}, @code{mmgs} and
@code{mmg3d} libraries.
@end itemize")
    (license license:lgpl3+)))

(define-public f3d
  (package
    (name "f3d")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f3d-app/f3d")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gcwpdkz3ylaxi133zri1cxkvj6za5s1hbgqqc8fn10q2dkkdd44"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (delete-file "external/cxxopts.hpp")
            (delete-file "external/json.hpp")
            (substitute* "application/F3DOptionsParser.cxx"
              (("^#include \"cxxopts\\.hpp\"")
               "#include <cxxopts.hpp>")
              (("^#include \"json\\.hpp\"")
               "#include <nlohmann/json.hpp>")
              (("cxxopts::OptionException")
               "cxxopts::exceptions::parsing"))
            (substitute* "library/src/engine.cxx"
              (("^#include <json\\.hpp>")
               "#include <nlohmann/json.hpp>"))))))
    (build-system cmake-build-system)
    ;; The package cannot easily be split into out and lib outputs because
    ;; VTK's vtkModule.cmake complains, and also the CMake files in
    ;; /lib/cmake/f3d expect the f3d executable and library to be available,
    ;; as they set up targets for both of them.
    (arguments
     (list
      ;; Many tests require files supplied by git-lfs.
      ;; Also, some tests segfault (after an exception?) but the tested
      ;; behavior, i.e., when the program is run manually, does not (for
      ;; example, TestNonExistentConfigFile and TestInvalidConfigFile).
      ;; Upstream is aware of occasionally flaky tests [1], but the tests
      ;; run in CI seem to be passing.
      ;; Anyway, the program runs and is able to open at least STL files
      ;; without issue.
      ;;
      ;; [1]: https://github.com/f3d-app/f3d/issues/92
      #:tests? #f
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_DOCDIR=" #$output
                             "/share/doc/" #$name "-" #$version)
              "-DBUILD_TESTING=OFF"
              "-DF3D_GENERATE_MAN=ON"
              "-DF3D_INSTALL_DEFAULT_CONFIGURATION_FILE=ON"
              "-DF3D_INSTALL_DEFAULT_CONFIGURATION_FILE_IN_PREFIX=ON"
              "-DF3D_INSTALL_MIME_TYPES_FILES=ON"
              "-DF3D_INSTALL_THUMBNAILER_FILES=ON"
              "-DF3D_MODULE_ALEMBIC=ON"
              "-DF3D_MODULE_ASSIMP=ON"
              "-DF3D_MODULE_EXTERNAL_RENDERING=ON"
              "-DF3D_MODULE_OCCT=ON")))
    (native-inputs
     (list cxxopts
           help2man
           nlohmann-json))
    (inputs
     (list alembic
           assimp
           double-conversion
           eigen
           expat
           fontconfig
           freetype
           glew
           hdf5
           imath
           jsoncpp
           libjpeg-turbo
           libpng
           libtiff
           libx11
           lz4
           netcdf
           opencascade-occt
           vtk
           zlib))
    (home-page "https://f3d-app.github.io/f3d/")
    (synopsis "VTK-based 3D viewer")
    (description "F3D (pronounced @samp{/fɛd/}) is a VTK-based 3D viewer with
simple interaction mechanisms and which is fully controllable using arguments
on the command line.  It supports a range of file formats (including animated
glTF, STL, STEP, PLY, OBJ, FBX), and provides numerous rendering and texturing
options.")
    (license license:bsd-3)))

(define-public gpaint
  (package
    (name "gpaint")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://alpha.gnu.org/gnu/"
                                  name "/"
                                  name "-2-" version ".tar.gz"))
              (sha256
               (base32
                "13jv0zqbnyxjw7fa9x0yl08rrkqq0mdvki0yzbj6vqifvs393v5h"))))
    (build-system gnu-build-system)
    (inputs (list gtk+-2 libglade))
    (native-inputs
     (list gettext-minimal `(,glib "bin") pkg-config))
    (synopsis "Simple paint program for GNOME")
    (description
     "GNU Paint is a simple, easy-to-use paint program for the GNOME
environment.  It supports drawing freehand as well as basic shapes and text.
It features cut-and-paste for irregular regions or polygons.")
    (home-page "https://www.gnu.org/software/gpaint/")
    (license license:gpl3+)))

