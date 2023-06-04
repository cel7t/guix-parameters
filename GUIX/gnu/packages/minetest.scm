;;; Copyright © 2014, 2015, 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Trevor Hass <thass@okstate.edu>
;;; Copyright © 2020, 2021, 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
(define-module (gnu packages minetest)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system minetest)
  #:use-module ((guix licenses) #:prefix license:))

(define-public minetest
  (package
    (name "minetest")
    (version "5.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minetest/minetest")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "008l44zwwsarwk4hn7wx2nj2m21b1iqsphl7g69rrlxj760zl0pl"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libraries.
                  (delete-file-recursively "lib")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DRUN_IN_PLACE=0"
              "-DENABLE_FREETYPE=1"
              "-DENABLE_GETTEXT=1"
              "-DENABLE_SYSTEM_JSONCPP=TRUE"
              (string-append "-DIRRLICHTMT_INCLUDE_DIR="
                             (search-input-directory %build-inputs
                                                     "include/irrlichtmt"))
              (string-append "-DCURL_INCLUDE_DIR="
                             (search-input-directory %build-inputs
                                                     "include/curl"))
              (string-append "-DZSTD_INCLUDE_DIR="
                             (dirname
                              (search-input-file %build-inputs
                                                 "include/zstd.h")))
              (string-append "-DZSTD_LIBRARY="
                             (search-input-file %build-inputs
                                                "lib/libzstd.so")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'patch-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/filesys.cpp"
                 ;; Use store-path for "rm" instead of non-existing FHS path.
                 (("\"/bin/rm\"")
                  (format #f "~s" (search-input-file inputs "bin/rm"))))
               (substitute* "src/CMakeLists.txt"
                 ;; Let minetest binary remain in build directory.
                 (("set\\(EXECUTABLE_OUTPUT_PATH .*\\)") ""))
               (substitute* "src/unittest/test_servermodmanager.cpp"
                 ;; do no override MINETEST_SUBGAME_PATH
                 (("(un)?setenv\\(\"MINETEST_SUBGAME_PATH\".*\\);")
                  "(void)0;"))
               (setenv "MINETEST_SUBGAME_PATH" ; for check
                       (string-append (getcwd) "/games"))))
           (delete 'check)
           (add-after 'install 'check
             (lambda* (#:key tests? #:allow-other-keys)
               ;; Thanks to our substitutions, the tests should also run
               ;; when invoked on the target outside of `guix build'.
               (when tests?
                 (setenv "HOME" "/tmp")
                 (invoke "src/minetest" "--run-unittests")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "MINETEST_SUBGAME_PATH")
            (files '("share/minetest/games")))
           (search-path-specification
            (variable "MINETEST_MOD_PATH")
            (files '("share/minetest/mods")))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list coreutils
           curl
           freetype
           gettext-minimal
           gmp
           irrlicht-for-minetest
           jsoncpp
           libjpeg-turbo
           libpng
           libogg
           libvorbis
           libxxf86vm
           luajit
           mesa
           ncurses
           openal
           sqlite
           `(,zstd "lib")))
    (propagated-inputs
     (list minetest-data))
    (synopsis "Infinite-world block sandbox game")
    (description
     "Minetest is a sandbox construction game.  Players can create and destroy
various types of blocks in a three-dimensional open world.  This allows
forming structures in every possible creation, on multiplayer servers or as a
single player.  Mods and texture packs allow players to personalize the game
in different ways.")
    (home-page "https://www.minetest.net/")
    (license license:lgpl2.1+)))

(define minetest-data
  (package
    (name "minetest-data")
    (version (package-version minetest))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minetest/minetest_game")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02kbj1h6jsq6k8x4v2ir0njczdz7nyx6dbym85ixxp3mrqxiws61"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." "/share/minetest/games/minetest_game"))))
    (synopsis "Main game data for the Minetest game engine")
    (description
     "Game data for the Minetest infinite-world block sandbox game.")
    (home-page "https://www.minetest.net/")
    (license license:lgpl2.1+)))

(define-public (minetest-topic topic-id)
  "Return an URL (as a string) pointing to the forum topic with
numeric identifier TOPIC-ID on the official Minetest forums."
  (string-append "https://forum.minetest.net/viewtopic.php?t="
                 (number->string topic-id)))

(define-public minetest-moreores
  (package
    (name "minetest-moreores")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minetest-mods/moreores")
             (commit (string-append "v" version))))
       (sha256 (base32 "1chfqbc6bb27aacjc67j5l5wcdvmcsvk2rfmangipd7nwini3y34"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 549))
    (synopsis "Additional ore types, tools, swords, and rails for Minetest")
    (description
     "This Minetest mod adds new ore types to the game (mithril, silver) as well
as swords and tools made of different materials.  It also adds copper rails.")
    (license license:zlib)
    (properties `((upstream-name . "Calinou/moreores")))))

(define-public minetest-sound-api-core
  (package
    (name "minetest-sound-api-core")
    ;; No tags, no releases. The author intended to let users use it as a
    ;; submodules for other projects.
    ;; https://github.com/mt-mods/basic_materials/issues/4
    (version "2022-02-27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mt-mods/sound_api_core")
             (commit "6956e49e775f325116f8e0c643899c089c691e1e")))
       (sha256
        (base32 "1ys6g2skhkksa4cx9agxhsibj5js8z4y2q1ngis9ddr38p756pcy"))
       (file-name (git-file-name name version))
       (snippet
        '(begin
           (call-with-output-file "mod.conf"
             (lambda (port)
               (format port "\
name = sound_api_core")))))))
    (build-system minetest-mod-build-system)
    (propagated-inputs '())
    (home-page "https://github.com/mt-mods/sound_api_core")
    (synopsis "Core for game agnostic sounds")
    (description
     "This library can be used to get some specific sounds, whatever the game.")
    (license license:expat)))

(define-public minetest-basic-materials
  (package
    (name "minetest-basic-materials")
    ;; Upstream uses dates as version numbers.
    (version "2022-03-28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mt-mods/basic_materials")
             (commit "9d55f9916d20779ecbf93c7e95dae8adebd2079b")))
       (sha256
        (base32 "0nzx5mdw26mk2by14hxyvbqckgz8k67vlh2ch30skssvh4984bjw"))
       (file-name (git-file-name name version))
       (snippet
        '(begin
           (use-modules (guix build utils))
           (substitute* "mod.conf"
             (("optional_depends =")
              "depends = sound_api_core
optional_depends ="))
           (substitute* "nodes.lua"
             (("basic_materials.modpath \\.\\. \"/sound_api_core/init.lua\"")
              "minetest.get_modpath(\"sound_api_core\") .. \"/init.lua\""))))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     ;; basic_materials:silver_wire cannot be crafted without
     ;; moreores:silver_ingot.
     (list minetest-moreores minetest-sound-api-core))
    (home-page (minetest-topic 21000))
    (synopsis "Some \"basic\" materials and items for other Minetest mods to use")
    (description
     "The Minetest mod \"basic_materials\" provides a small selection of
\"basic\" materials and items that other mods should use when possible -- things
like steel bars and chains, wire, plastic strips and sheets, and more.")
    (license
     (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/basic_materials")))))

(define-public minetest-coloredwood
  (package
    (name "minetest-coloredwood")
    ;; Upstream uses dates as version numbers.
    (version "2021-04-14-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/coloredwood")
             (commit "be4df6fc889419155bed8638bbb752493e78cbd5")))
       (sha256
        (base32 "1swirfk6b4xfbiwv8adyw5yl2lyfpp8ymfipzq9ivyvmif8nl3ki"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-unifieddyes))
    (home-page (minetest-topic 2411))
    (synopsis "Painted wood in Minetest")
    (description
     "This Minetest mod provides hundreds of colours of wood and fences to
Minetest, using Unified Dyes.  If the \"moreblocks\" mod is active,
coloured and cut wood shapes are provided as well.")
    (license
     ;; LGPL for code, CC-BY-SA for textures
     (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/coloredwood")))))

(define-public minetest-ethereal
  ;; ContentDB release 2021-07-28 is slightly ahead of the
  ;; initial version 1.29 -- i.e., some released changes have been
  ;; made to version 1.29 without a corresponding version bump.
  (let ((commit "7670c1da9274901f57f6682384af2b3bae005a86")
        (revision "0"))
    (package
      (name "minetest-ethereal")
      (version (git-version "1.29" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/TenPlus1/ethereal")
               (commit commit)))
         (sha256
          (base32 "1hal8bq4fydsip7s8rqz4vlaaqy9rhzxmryd0j2qnqm9286yjgkk"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (home-page (minetest-topic 14638))
      (synopsis "The Ethereal mod adds many new biomes to Minetest")
      (description
       "The Ethereal Minetest mod uses the v7 map generator to add many new
biomes to the world.  It adds new trees, plants, food items, tweaks and some
special items, intending to make an interesting adventure.")
      ;; CC0: some textures
      (license (list license:cc0 license:expat))
      (properties `((upstream-name . "TenPlus1/ethereal"))))))

(define-public minetest-homedecor-modpack
  (package
    (name "minetest-homedecor-modpack")
    ;; Upstream doesn't tag releases, so use the release title from
    ;; ContentDB as version.
    (version "2022-05-18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mt-mods/homedecor_modpack")
             (commit "5ffdc26673169e05492141709fbb18e8fb6e5937")))
       (sha256
        (base32 "03pf254r3hnznklw7lf3q4rzqg0a1y4c9rjjhzssf1q7ai5pdrkn"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-basic-materials minetest-unifieddyes))
    (home-page (minetest-topic 2041))
    (synopsis "Home decor mod for Minetest")
    (description
     ;; TRANSLATORS: ‘homedecor’ is the name is the name of a Minetest mod
     ;; and should not be translated.
     "The homedecor Minetest mod provides a large seleection of items that
might be found inside and around homes, such as sofas, chairs, tables, fences
and a variety of other stuff.")
    (license
     (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/homedecor_modpack")))))

(define-public minetest-mesecons
  ;; The release on ContentDB does not have its own version number.
  (let ((commit "27c3c515b49af91c1dbc427f31a820722854eb24")
        (revision "63"))
    (package
      (name "minetest-mesecons")
      (version (git-version "1.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minetest-mods/mesecons")
               (commit commit)))
         (sha256
          (base32 "1l0kwjj8ns8hv6z520g6ph5swknar336dbi5qr3dfsy18ydk1j92"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (home-page "https://mesecons.net")
      (synopsis
       "Digital circuitry for Minetest, including wires, buttons and lights")
      (description
       "Mesecons is a mod for Minetest implementing various items related
to digital circuitry, such as wires, buttons, lights and programmable
controllers.  Among other things, there are also pistons, solar panels,
pressure plates and note blocks.

Mesecons has a similar goal to Redstone in Minecraft, but works in its own way,
with different rules and mechanics.")
      ;; LGPL for code, CC-BY-SA for textures.
      (license (list license:lgpl3 license:cc-by-sa3.0))
      (properties `((upstream-name . "Jeija/mesecons"))))))

(define-public minetest-mineclone
  (package
    (name "minetest-mineclone")
    (version "0.80.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.minetest.land/MineClone2/MineClone2")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ndgzasjmj8397bb3y2aqalx9v72llwgvp9pv05mms3chpji2675"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "share/minetest/games/mineclone"))))
    (synopsis "Minecraft clone based on Minetest engine")
    (description
     "MineClone is a Minetest subgame, that aims to recreate Minecraft as
closely as the engine allows.")
    (home-page "https://content.minetest.net/packages/Wuzzy/mineclone2/")
    (license license:gpl3+)))

(define-public minetest-mobs
  (package
    (name "minetest-mobs")
    ;; Upstream does not tag release, so use the ContentDB release
    ;; title instead.
    (version "2021-12-12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/TenPlus1/mobs_redo")
             (commit "6a4a02f3fbf1038c69e72aaafa52a1e7d6106da8")))
       (sha256
        (base32 "0vgv7jpm9v3dwq4l9jxdd5z14yq164w8kin1d05jfv3ck4hwlwvr"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 9917))
    (synopsis "Mob library for Minetest mods, for animals, monsters etc.")
    (description
     "This Minetest mod provides an API for adding mods (moving entities
like animals and monsters), but does not include any mobs itself.  To actually
add some mobs, a mod like e.g. @code{mobs_animal} provided by the
@code{minetest-mobs-animal} package needs to be enabled.")
    ;; CC0: mob_swing.ogg
    ;; CC-BY 3.0: mob_spell.ogg
    ;; Expat: everything else
    (license (list license:expat license:cc0 license:cc-by3.0))
    (properties `((upstream-name . "TenPlus1/mobs")))))

(define-public minetest-mobs-animal
  (package
    (name "minetest-mobs-animal")
    ;; Upstream does not use version numbers, so use the release title
    ;; from ContentDB instead;
    (version "2021-11-14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/TenPlus1/mobs_animal")
             (commit "3e15456bce7779aa0dc09a8890f7b5180c1ac771")))
       (sha256
        (base32 "08686mj3jh8fsziqp878jpaj5267s4n6i86dr1gnxyxbsrjraqpn"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-mobs))
    (home-page "https://notabug.org/TenPlus1/mobs_animal")
    (synopsis "Add animals to Minetest")
    (description
     "This Minetest mod adds various animals to Minetest, such as bees,
bunnies, chickens, cows, kittens, rats, sheep, warthogs, penguins and pandas.")
    ;; CC0: some textures and sounds
    (license (list license:cc0 license:expat))
    (properties `((upstream-name . "TenPlus1/mobs_animal")))))

(define-public minetest-mobs-monster
  (package
    (name "minetest-mobs-monster")
    ;; Upstream does not use version numbers, so use the release title
    ;; from ContentDB instead;
    (version "2022-12-10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/TenPlus1/mobs_monster")
             (commit "1b197f9ae136179a764ef45824464b667ade52e6")))
       (sha256
        (base32 "15g8acrzvsiccxchfmgjhyf2lmkbrpdjqv3v7hmqz7xqypi8wm3h"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs (list minetest-mobs))
    (home-page "https://notabug.org/TenPlus1/mobs_monster")
    (synopsis "Add monsters with Mobs Redo on minetest")
    (description
     "This Minetest mod adds many types of monsters to Minetest, that live on the
surface or deep underground.")
    (license license:expat)
    (properties `((upstream-name . "TenPlus1/mobs_monster")))))

(define-public minetest-pipeworks
  (package
    (name "minetest-pipeworks")
    ;; Upstream uses dates as version numbers.
    (version "2021-04-14-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/pipeworks")
             (commit "db6d1bd9c109e1e543b97cc3fa8a11400da23bcd")))
       (sha256
        (base32 "1flhcnf17dn1v86kcg47a1n4cb0lybd11ncxrkxn3wmf10ibsrm0"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-basic-materials))
    (home-page (minetest-topic 2155))
    (synopsis "Pipes, item-transport tubes and related devices for Minetest")
    (description
     "Pipeworks is a mod for Minetest implementing 3D pipes and tubes for
transporting liquids and items and some related devices.  Pipes and tubes can
go horizontally or vertically.  Item tubes can also be used for sorting items
and extracting items from chests or putting items in chests.  Autocrafters can
automatically follow craft recipes to make new items and can be fed by item
tubes.  Deployers can place items in the world as a player would.  Node
breakers simulate a player punching a node.")
    ;; CC-BY-SA for textures, LGPL for code
    (license (list license:cc-by-sa4.0 license:lgpl3))
    (properties `((upstream-name . "VanessaE/pipeworks")))))

(define-public minetest-technic
  (package
    (name "minetest-technic")
    ;; Upstream doesn't keep version numbers, so use the release
    ;; date on ContentDB instead.
    (version "2022-10-30")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minetest-mods/technic")
             (commit "6a9dfec36f7265f9328fe378732417d9b57060b4")))
       (sha256
        (base32 "0ng1aklm02n2lbar345yf88g3alzvnh0m0s5rb2bcw2k5lib8468"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-pipeworks minetest-basic-materials))
    (home-page (minetest-topic 2538))
    (synopsis "Machinery and automation for Minetest")
    (description
     "This Minetest mod adds machinery and automation to Minetest.
It adds various ores that can be processed for constructing various
machinery, such as power generators, force field emitters, quarries
and a workshop for repairing tools.  Most machines are electrically
powered.")
    ;; CC BY-SA 3.0: some texture
    ;; WTFPL: some textures
    ;; CC BY-SA3.0: some textures
    ;; CC BY-SA4.0: some sounds
    (license (list license:lgpl2.1+ license:cc-by-sa3.0 license:cc-by-sa4.0
                   license:wtfpl2))
    (properties `((upstream-name . "RealBadAngel/technic")))))

(define-public minetest-throwing
  ;; The latest release on ContentDB is ahead of the latet
  ;; tagged commit.
  (let ((commit "31f0cf5f868673dc82f24ddc432b45c9cd282d27")
        (revision "0"))
    (package
      (name "minetest-throwing")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minetest-mods/throwing")
               (commit commit)))
         (sha256
          (base32 "1s5kkr6rxxv2dhbbjzv62gw1s617hnpjavw1v9fv11v3mgigdfjb"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (home-page (minetest-topic 16365))
      (synopsis "API for throwing things in Minetest")
      (description
       "This Minetest mod provides an API for registering throwable things and
throwing things like arrows.  However, this mod does not provide an actual
arrow and bow, but @code{minetest-throwing-arrows} does.")
      (license license:mpl2.0)
      (properties `((upstream-name . "Palige/throwing"))))))

(define-public minetest-throwing-arrows
  ;; There is only one tagged commit (version 1.1),
  ;; there are no releases on ContentDB and the latest
  ;; commit has a compatibility fix for Minetest 5.4.0-dev.
  (let ((commit "059cc897af0aebfbd2c54ac5588f2b842f44f159")
        (revision "0"))
    (package
      (name "minetest-throwing-arrows")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minetest-mods/throwing_arrows")
               (commit commit)))
         (sha256
          (base32 "0m2pmccpfxn878zd00pmrpga2h6gknz4f3qprck0fq94mksmwqs3"))
         (file-name (git-file-name name version))))
      (build-system minetest-mod-build-system)
      (propagated-inputs
       (list minetest-throwing))
      (home-page (minetest-topic 16365))
      (synopsis "Arrows and bows for Minetest")
      (description
       ;; TRANSLATORS: "throwing" is the name of a Minetest mod and should
       ;; not be translated.
       "This mod adds arrows and bows to Minetest.  It is a compatible
replacement for the throwing mod by PilzAdam that uses the throwing API.")
      (license license:mpl2.0))))

(define-public minetest-worldedit
  (package
    (name "minetest-worldedit")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Uberi/Minetest-WorldEdit")
             (commit "2f26fb76459c587868199160b9d7b5d6d7852e50")))
       (sha256
        (base32 "0lsvihkixi2na1b0vmml9vwgs0g24hqqshl73ffhkzh6jsq4cagq"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 572))
    (synopsis "In-game world editor for Minetest")
    (description
     "WorldEdit is a mod for Minetest.  It allows for creating various
geometric shapes and copying regions.  It can also export and import regions
to and from the file system.")
    (license license:agpl3)
    (properties `((upstream-name . "sfan5/worldedit")))))

(define-public minetest-unifieddyes
  (package
    (name "minetest-unifieddyes")
    ;; Upstream uses dates as version numbers.
    (version "2021-04-20-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/VanessaE/unifieddyes")
             (commit "ff3b2d30fa0df5c7181fdd401b989de6271c3bb3")))
       (sha256
        (base32 "0rba9n192xcpmxwnq7ixb6mn32gkpic247j3w4mwinrqcyscacsv"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-basic-materials))
    (home-page (minetest-topic 2178))
    (synopsis
     "Unified Dyes expands the standard dye set of Minetest to up to 256 colours")
    (description "The purpose of this mod originally was to supply a complete
set of colours for Minetest mod authors to use for colourised nodes or
reference in recipes.  Since the advent of the default dyes mod in the standard
Minetest game, this mod has become an extension of the default mod an a library
for general colour handling.")
    (license license:gpl2+)
    (properties `((upstream-name . "VanessaE/unifieddyes")))))

(define-public minetest-unified-inventory
  (package
    (name "minetest-unified-inventory")
    ;; Upstream doesn't keep version numbers, so use the release title
    ;; on ContentDB instead.
    (version "2021-12-26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minetest-mods/unified_inventory")
             (commit "d6688872c84417d2f61d6f5e607aea39d78920aa")))
       (sha256
        (base32 "1rlw96s2yyxdbz0h9byayyx9nsbqdr4ric91w0k3dkjr71aj8a3b"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 12767))
    (synopsis "Replace the default inventory in Minetest and add a crafting guide")
    (description
     "The Unified Inventory Minetest mod relaces the default survival an
creative inventory.  It includes a node, item and tool browser, a crafting
guide, a trash and refill slot for creative mode, bags and waypoints for keeping
track of important locations.")
    ;; CC-BY: some textures and icons
    ;; CC-BY-SA: some textures and icons
    ;; LGLPL2.1+: code and some textures
    ;; GPL2+: some textures
    ;; GPL3: bags.lua
    ;; GFDL: some icons
    ;; public domain, CC0: some icons
    (license (list license:gpl3 license:gpl2+ license:lgpl2.1+ license:cc-by3.0
                   license:cc-by4.0 license:cc-by-sa3.0 license:public-domain
                   license:cc0 license:fdl1.2+))
    (properties `((upstream-name . "RealBadAngel/unified_inventory")))))

(define-public minetest-advtrains
  (package
    (name "minetest-advtrains")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.bananach.space/advtrains.git")
             (commit (string-append "release-" version))))
       (sha256
        (base32 "1q2jj8181pjgsakl28xadv0z4sszq1lb5rpgj070wr0px6mp447p"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page "https://advtrains.de/")
    (synopsis "Adds good-looking, realistic trains with realistic rails")
    (description
     "This mod features realistic trains and various equipment for railways,
with a focus on automated train operation.  This package contains no actual
trains, please use @code{minetest-basic-trains}.

Main features:

@itemize
@item
almost-realistic tracks with actual curves and switches;
@item
railway signals, controllable by various means;
@item
ATC: simple, command-like automatic train control;
@item
LuaATC: Powerful Lua-scripted automatic train operation (requires some
programming knowledge);
@item
an interlocking system, featuring track sections, routes and automatic
stopping before signals.
@end itemize")
    (license (list license:cc-by-sa3.0 license:agpl3+))
    (properties `((upstream-name . "orwell/advtrains")))))

(define-public minetest-basic-trains
  (package
    (name "minetest-basic-trains")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://git.bananach.space/basic_trains.git/")
             (commit
              "d44c410f7c2a7202ee68b66fc50febae89e0c5dc")))
       (sha256
        (base32
         "0vvzndj48kgdz2bfgivfm217sbmc2lmxpp2mispcy7byn4i26prx"))
       (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (propagated-inputs
     (list minetest-advtrains))
    (home-page
     "http://advtrains.de/wiki/doku.php?id=usage:trains:basic_trains")
    (synopsis "Collection of basic trains for the Advanced Trains mod")
    (description
     "This modpack contains the trains which were the ``default'' trains in
advtrains up to version 2.2.1.")
    (license (list license:cc-by-sa3.0 license:agpl3+))
    (properties `((upstream-name . "orwell/basic_trains")))))

(define-public minetest-oneblock
  (package
    (name "minetest-oneblock")
    (version "2022-09-01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NO411/oneblock")
                    (commit "3582c825137e61b3d2ae3d60b8b9746acd6dfe3d")))
              (sha256
               (base32
                "1pkdrj99qqwmz7c86w2mh081ynyxhiwv2rl01xjm1wfpazx5zhdg"))
              (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page "https://github.com/NO411/oneblock")
    (synopsis "Build your island in the sky with random items!")
    (description
     "This package provides an extension of the Minetest game that lets you
build your island in the sky.  Every 30 seconds you will receive a random
block or item from the oneblock to expand the island!")
    (license license:gpl3+)
    (properties `((upstream-name . "NO11/oneblock")))))

(define-public minetest-wielded-light
  (package
    (name "minetest-wielded-light")
    (version "2022-06-24")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minetest-mods/wielded_light")
                    (commit "b5236562af9772dff8522fe2bda5b5f738e81b88")))
              (sha256
               (base32
                "0m5rf8wkc9iq04xppjfva9d83qmhlnx8fibdbi2d3pkwwl6p2y5c"))
              (file-name (git-file-name name version))))
    (build-system minetest-mod-build-system)
    (home-page (minetest-topic 19378))
    (synopsis "Adds shining for wielded and dropped items")
    (description
     "With this Minetest extension, all bright nodes lighten the player
environment if wielded.")
    (license license:gpl3+)
    (properties `((upstream-name . "bell07/wielded_light")))))
