;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2014, 2015, 2017, 2019, 2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020 André Batista <nandre@riseup.net>
;;; Copyright © 2020 Helio Machado <0x2b3bfa0+guix@googlemail.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Kyle Meyer <kyle@kyleam.com>
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

(define-module (guix licenses)
  #:use-module (srfi srfi-9)
  #:export (license? license-name license-uri license-comment
            agpl1 agpl3 agpl3+
            apsl2
            asl1.1 asl2.0
            boost1.0
            bsd-0 bsd-1 bsd-2 bsd-3 bsd-4
            non-copyleft
            cc0
            cc-by2.0 cc-by3.0 cc-by4.0
            cc-by-sa2.0 cc-by-sa3.0 cc-by-sa4.0
            cddl1.0 cddl1.1
            cecill cecill-b cecill-c
            artistic2.0 clarified-artistic
            copyleft-next
            cpl1.0
            cua-opl1.0
            edl1.0
            epl1.0
            epl2.0
            eupl1.1
            eupl1.2
            expat expat-0
            freetype
            freebsd-doc
            giftware
            gpl1 gpl1+ gpl2 gpl2+ gpl3 gpl3+
            gfl1.0
            fdl1.1+ fdl1.2+ fdl1.3+
            opl1.0+ osl2.1
            isc
            ijg
            ibmpl1.0
            imlib2
            ipa
            knuth
            lal1.3
            lgpl2.0 lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3 lgpl3+ llgpl
            lpl1.02
            lppl lppl1.0+ lppl1.1+ lppl1.2 lppl1.2+
            lppl1.3 lppl1.3+
            lppl1.3a lppl1.3a+
            lppl1.3b lppl1.3b+
            lppl1.3c lppl1.3c+
            miros
            mpl1.0 mpl1.1 mpl2.0
            ms-pl
            ncsa
            nmap
            ogl-psi1.0
            openldap2.8 openssl
            perl-license
            psfl public-domain
            qpl
            qwt1.0
            repoze
            ruby
            sgifreeb2.0
            silofl1.1
            sleepycat
            tcl/tk
            unicode
            unlicense
            vim
            w3c
            x11 x11-style
            zpl2.1
            zlib
            fsf-free
            wtfpl2
            wxwindows3.1+
            hpnd
            fsdg-compatible))

;;; Commentary:
;;;
;;; Available licenses.
;;;
;;; This list is based on these links:
;;; https://github.com/NixOS/nixpkgs/blob/master/lib/licenses.nix
;;; https://www.gnu.org/licenses/license-list
;;;
;;; Please update spdx-string->license from guix/import/utils.scm
;;; when modifying this list to avoid mismatches.
;;;
;;; Code:

(define-record-type <license>
  (license name uri comment)
  actual-license?
  (name    license-name)
  (uri     license-uri)
  (comment license-comment))

(define-syntax define-license-predicate
  (syntax-rules (define define*)
    "Define PREDICATE as a license predicate that, when applied to trivial
cases, reduces to #t at macro-expansion time."
    ((_ predicate (variables ...) (procedures ...)
        (define variable _) rest ...)
     (define-license-predicate
       predicate
       (variable variables ...) (procedures ...)
       rest ...))
    ((_ predicate (variables ...) (procedures ...)
        (define* (procedure _ ...) _ ...)
        rest ...)
     (define-license-predicate
       predicate
       (variables ...) (procedure procedures ...)
       rest ...))
    ((_ predicate (variables ...) (procedures ...))
     (define-syntax predicate
       (lambda (s)
         (syntax-case s (variables ... procedures ...)
           ((_ variables) #t) ...
           ((_ (procedures _)) #t) ...
           ((_ obj) #'(actual-license? obj))
           (id
            (identifier? #'id)
            #'actual-license?)))))))

(define-syntax begin-license-definitions
  (syntax-rules ()
    ((_ predicate definitions ...)
     (begin
       ;; Define PREDICATE such that it expands to #t when passed one of the
       ;; identifiers in DEFINITIONS.
       (define-license-predicate predicate () () definitions ...)

       definitions ...))))

(begin-license-definitions license?

(define agpl1
  (license "AGPL 1"
           "https://gnu.org/licenses/agpl.html"
           "https://gnu.org/licenses/why-affero-gpl.html"))

(define agpl3
  (license "AGPL 3"
           "https://gnu.org/licenses/agpl.html"
           "https://gnu.org/licenses/why-affero-gpl.html"))

(define agpl3+
  (license "AGPL 3+"
           "https://gnu.org/licenses/agpl.html"
           "https://gnu.org/licenses/why-affero-gpl.html"))

(define apsl2
  (license "APSL 2.0"
           "https://directory.fsf.org/wiki/License:APSL-2.0"
           "https://www.gnu.org/licenses/license-list.html#apsl2"))

(define asl1.1
  (license "ASL 1.1"
           "http://directory.fsf.org/wiki/License:Apache1.1"
           "https://www.gnu.org/licenses/license-list#apache1"))

(define asl2.0
  (license "ASL 2.0"
           "http://directory.fsf.org/wiki/License:Apache2.0"
           "https://www.gnu.org/licenses/license-list#apache2"))

(define boost1.0
  (license "Boost 1.0"
           "http://directory.fsf.org/wiki/License:Boost1.0"
           "https://www.gnu.org/licenses/license-list#boost"))

(define bsd-0
  (license "Zero-Clause BSD"
           "https://spdx.org/licenses/0BSD.html"
           "https://opensource.org/licenses/0BSD"))

(define bsd-1
  (license "BSD 1-Clause"
           "https://spdx.org/licenses/BSD-1-Clause.html"
           "https://opensource.org/licenses/BSD-1-Clause"))

(define bsd-2
  (license "FreeBSD"
           "http://directory.fsf.org/wiki/License:FreeBSD"
           "https://www.gnu.org/licenses/license-list#FreeBSD"))

(define bsd-3
  (license "Modified BSD"
           "http://directory.fsf.org/wiki/License:BSD_3Clause"
           "https://www.gnu.org/licenses/license-list#ModifiedBSD"))

(define bsd-4
  (license "Original BSD"
           "http://directory.fsf.org/wiki/License:BSD_4Clause"
           "https://www.gnu.org/licenses/license-list#OriginalBSD"))

(define* (non-copyleft uri #:optional (comment ""))
  "Return a lax, permissive, non-copyleft license (for example a variant of
the 3-clause BSD license or the Expat license), whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "non-copyleft"
           uri
           (string-append
            "This is a lax, non-copyleft free software license.  "
            "Check the URI for details.  "
            comment)))

(define cc0
  (license "CC0"
           "http://directory.fsf.org/wiki/License:CC0"
           "http://www.gnu.org/licenses/license-list.html#CC0"))

(define cc-by-sa4.0
  (license "CC-BY-SA 4.0"
           "http://creativecommons.org/licenses/by-sa/4.0/"
           "Creative Commons Attribution-ShareAlike 4.0 International"))

(define cc-by-sa3.0
  (license "CC-BY-SA 3.0"
           "http://creativecommons.org/licenses/by-sa/3.0/"
           "Creative Commons Attribution-ShareAlike 3.0 Unported"))

(define cc-by-sa2.0
  (license "CC-BY-SA 2.0"
           "http://creativecommons.org/licenses/by-sa/2.0/"
           "Creative Commons Attribution-ShareAlike 2.0 Generic"))

(define cc-by4.0
  (license "CC-BY 4.0"
           "http://creativecommons.org/licenses/by/4.0/"
           "Creative Commons Attribution 4.0 Unported"))

(define cc-by3.0
  (license "CC-BY 3.0"
           "http://creativecommons.org/licenses/by/3.0/"
           "Creative Commons Attribution 3.0 Unported"))

(define cc-by2.0
  (license "CC-BY 2.0"
           "http://creativecommons.org/licenses/by/2.0/"
           "Creative Commons Attribution 2.0 Generic"))

(define cddl1.0
  (license "CDDL 1.0"
           "http://directory.fsf.org/wiki/License:CDDLv1.0"
           "https://www.gnu.org/licenses/license-list#CDDL"))

;; CDDL1.1 is the same as 1.0, except that "Sun Microsystems, Inc" becomes "Oracle",
;; "LOST PROFITS" becoms "LOSS OF GOODWILL" and a section is added between 6.2
;; and 6.3.
(define cddl1.1
  (license "CDDL 1.1"
           "https://oss.oracle.com/licenses/CDDL+GPL-1.1"
           "https://www.gnu.org/licenses/license-list#CDDL"))

(define cecill                                    ;copyleft
  (license "CeCILL"
           "http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html"
           "https://www.gnu.org/licenses/license-list.html#CeCILL"))

(define cecill-b                                  ;non-copyleft
  (license "CeCILL-B"
           "http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html"
           "https://www.gnu.org/licenses/license-list.html#CeCILL"))

(define cecill-c                                  ;weak copyleft
  (license "CeCILL-C"
           "http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.html"
           "https://www.gnu.org/licenses/license-list.html#CeCILL"))

(define artistic2.0
  (license "Artistic License 2.0"
           "http://www.perlfoundation.org/artistic_license_2_0"
           "http://www.gnu.org/licenses/license-list.html#ArtisticLicense2"))

(define clarified-artistic
  (license "Clarified Artistic"
           ;; http://directory.fsf.org/wiki/User:Jgay/license-categorization#Clarified_Artistic_License
           "http://gianluca.dellavedova.org/2011/01/03/clarified-artistic-license/"
           "https://www.gnu.org/licenses/license-list.html#ArtisticLicense2"))

(define copyleft-next
  (license "copyleft-next"
           "https://raw.github.com/richardfontana/copyleft-next/master/Releases/copyleft-next-0.3.0"
           "GPL-compatible copyleft license"))

(define cpl1.0
  (license "CPL 1.0"
           "http://directory.fsf.org/wiki/License:CPLv1.0"
           "https://www.gnu.org/licenses/license-list#CommonPublicLicense10"))

(define cua-opl1.0
  (license "CUA Office Public License v1.0"
           "https://spdx.org/licenses/CUA-OPL-1.0.html"
           "https://opensource.org/licenses/CUA-OPL-1.0"))

(define edl1.0
  (license "EDL 1.0"
           "http://directory.fsf.org/wiki/License:EDLv1.0"
           "https://eclipse.org/org/documents/edl-v10.php"))

(define epl1.0
  (license "EPL 1.0"
           "http://directory.fsf.org/wiki/License:EPLv1.0"
           "https://www.gnu.org/licenses/license-list#EPL"))

(define epl2.0
  (license "EPL 2.0"
           "https://www.eclipse.org/legal/epl-2.0/"
           "https://www.gnu.org/licenses/license-list#EPL2"))

(define eupl1.1
  (license "EUPL 1.1"
           "https://directory.fsf.org/wiki/License:EUPL-1.1"
           "https://www.gnu.org/licenses/license-list#EUPL-1.1"))

(define eupl1.2
  (license "EUPL 1.2"
           "https://directory.fsf.org/wiki/License:EUPL-1.2"
           "https://www.gnu.org/licenses/license-list#EUPL-1.2"))

;; Some people call it the MIT license. For clarification see:
;; https://www.gnu.org/licenses/license-list.html#Expat
(define expat
  (license "Expat"
           "http://directory.fsf.org/wiki/License:Expat"
           "https://www.gnu.org/licenses/license-list.html#Expat"))

(define expat-0
  (license "Expat No Attribution"
           ;; Note: There is a later formulation of the same license at
           ;; <https://github.com/aws/mit-0>.
           "https://romanrm.net/mit-zero"
           "Expat license with the attribution paragraph removed."))

(define freetype
  (license "Freetype"
           "http://directory.fsf.org/wiki/License:Freetype"
           "https://www.gnu.org/licenses/license-list.html#freetype"))

(define giftware
  (license "Giftware"
           "https://liballeg.org/license.html"
           "The Allegro 4 license"))

(define gpl1
  (license "GPL 1"
           "https://www.gnu.org/licenses/old-licenses/gpl-1.0.html"
           #f))

(define gpl1+
  (license "GPL 1+"
           "https://www.gnu.org/licenses/old-licenses/gpl-1.0.html"
           #f))

(define gpl2
  (license "GPL 2"
           "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"
           "https://www.gnu.org/licenses/license-list#GPLv2"))

(define gpl2+
  (license "GPL 2+"
           "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"
           "https://www.gnu.org/licenses/license-list#GPLv2"))

(define gpl3
  (license "GPL 3"
           "https://www.gnu.org/licenses/gpl.html"
           "https://www.gnu.org/licenses/license-list#GNUGPLv3"))

(define gpl3+
  (license "GPL 3+"
           "https://www.gnu.org/licenses/gpl.html"
           "https://www.gnu.org/licenses/license-list#GNUGPLv3"))

;; The “GUST font license” is legally equivalent to LPPL v1.3c as it only
;; extends the LPPL with an optional request.
(define gfl1.0
  (license "GUST font license 1.0"
           "http://www.gust.org.pl/projects/e-foundry/licenses/GUST-FONT-LICENSE.txt"
           "https://www.gnu.org/licenses/license-list#LPPL-1.3a"))

(define fdl1.1+
  (license "FDL 1.1+"
           "https://www.gnu.org/licenses/fdl-1.1"
           "https://www.gnu.org/licenses/license-list#FDL"))

(define fdl1.2+
  (license "FDL 1.2+"
           "https://www.gnu.org/licenses/fdl-1.2"
           "https://www.gnu.org/licenses/license-list#FDL"))

(define fdl1.3+
  (license "FDL 1.3+"
           "https://www.gnu.org/licenses/fdl.html"
           "https://www.gnu.org/licenses/license-list#FDL"))

(define freebsd-doc
  (license "FreeBSD Documentation License"
           "https://www.freebsd.org/copyright/freebsd-doc-license.html"
           "https://www.gnu.org/licenses/license-list.html#FreeBSDDL"))

(define opl1.0+
  (license "Open Publication License 1.0 or later"
           "http://opencontent.org/openpub/"
           "https://www.gnu.org/licenses/license-list#OpenPublicationL"))

(define osl2.1
  (license "The Open Software License 2.1"
           "https://opensource.org/licenses/osl-2.1.php"
           "https://www.gnu.org/licenses/license-list#OSL"))

(define isc
  (license "ISC"
           "http://directory.fsf.org/wiki/License:ISC"
           "https://www.gnu.org/licenses/license-list.html#ISC"))

(define ijg
  (license "IJG"
           "http://directory.fsf.org/wiki/License:JPEG"
           "https://www.gnu.org/licenses/license-list#ijg"))

(define ibmpl1.0
  (license "IBMPL 1.0"
           "http://directory.fsf.org/wiki/License:IBMPLv1.0"
           "https://www.gnu.org/licenses/license-list#IBMPL"))

(define imlib2
  (license "Imlib2"
           "http://directory.fsf.org/wiki/License:Imlib2"
           "https://www.gnu.org/licenses/license-list#imlib"))

(define ipa
  (license "IPA Font License"
           "http://directory.fsf.org/wiki/License:IPA_Font_License"
           "https://www.gnu.org/licenses/license-list#IPAFONT"))

(define knuth
  (license "Donald Knuth's license for TeX"
           "http://www.ctan.org/license/knuth"
           "Modification are only permitted under a different name."))

(define lal1.3
  (license "Free Art License 1.3"
           "http://artlibre.org/licence/lal/en/"
           "https://www.gnu.org/licenses/license-list#FreeArt"))

(define lgpl2.0
  (license "LGPL 2.0"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.0.html"
           "https://www.gnu.org/licenses/why-not-lgpl.html"))

(define lgpl2.0+
  (license "LGPL 2.0+"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.0.html"
           "https://www.gnu.org/licenses/why-not-lgpl.html"))

(define lgpl2.1
  (license "LGPL 2.1"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html"
           "https://www.gnu.org/licenses/license-list#LGPLv2.1"))

(define lgpl2.1+
  (license "LGPL 2.1+"
           "https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html"
           "https://www.gnu.org/licenses/license-list#LGPLv2.1"))

(define lgpl3
  (license "LGPL 3"
           "https://www.gnu.org/licenses/lgpl.html"
           "https://www.gnu.org/licenses/license-list#LGPLv3"))

(define lgpl3+
  (license "LGPL 3+"
           "https://www.gnu.org/licenses/lgpl.html"
           "https://www.gnu.org/licenses/license-list#LGPLv3"))

(define llgpl
  (license "LLGPL"
           "https://opensource.franz.com/preamble.html"
           "Lisp Lesser General Public License"))

(define lpl1.02 ;Lucent
  (license "LPL 1.02"
           "https://directory.fsf.org/wiki/License:LPL-1.02"
           "https://www.gnu.org/licenses/license-list.html#lucent102"))

(define lppl
  (license "LPPL (any version)"
           "https://www.latex-project.org/lppl/lppl-1-0/"
           "LaTeX Project Public License 1.0"))

(define lppl1.0+
  (license "LPPL 1.0+"
           "https://www.latex-project.org/lppl/lppl-1-0/"
           "LaTeX Project Public License 1.0"))

(define lppl1.1+
  (license "LPPL 1.1+"
           "https://www.latex-project.org/lppl/lppl-1-1/"
           "LaTeX Project Public License 1.1"))

(define lppl1.2
  (license "LPPL 1.2"
           "http://directory.fsf.org/wiki/License:LPPLv1.2"
           "https://www.gnu.org/licenses/license-list#LPPL-1.2"))

(define lppl1.2+
  (license "LPPL 1.2+"
           "http://directory.fsf.org/wiki/License:LPPLv1.2"
           "https://www.gnu.org/licenses/license-list#LPPL-1.2"))

(define lppl1.3
  (license "LPPL 1.3"
           "https://www.latex-project.org/lppl/lppl-1-3/"
           "LaTeX Project Public License 1.3"))

(define lppl1.3+
  (license "LPPL 1.3+"
           "https://www.latex-project.org/lppl/lppl-1-3/"
           "LaTeX Project Public License 1.3+"))

(define lppl1.3a
  (license "LPPL 1.3a"
           "http://directory.fsf.org/wiki/License:LPPLv1.3a"
           "https://www.gnu.org/licenses/license-list#LPPL-1.3a"))

(define lppl1.3a+
  (license "LPPL 1.3a+"
           "http://directory.fsf.org/wiki/License:LPPLv1.3a"
           "https://www.gnu.org/licenses/license-list#LPPL-1.3a"))

(define lppl1.3b
  (license "LPPL 1.3b"
           "https://www.latex-project.org/lppl/lppl-1-3b/"
           "LaTeX Project Public License 1.3b"))

(define lppl1.3b+
  (license "LPPL 1.3b+"
           "https://www.latex-project.org/lppl/lppl-1-3b/"
           "LaTeX Project Public License 1.3b or later"))

(define lppl1.3c
  (license "LPPL 1.3c"
           "https://www.latex-project.org/lppl/lppl-1-3c/"
           "LaTeX Project Public License 1.3c"))

(define lppl1.3c+
  (license "LPPL 1.3c+"
           "https://www.latex-project.org/lppl/lppl-1-3c/"
           "LaTeX Project Public License 1.3c or later"))

(define miros
  (license "MirOS"
           "https://www.mirbsd.org/MirOS-Licence.htm"
           "MirOS License"))

(define mpl1.0
  (license "MPL 1.0"
           "http://www.mozilla.org/MPL/1.0/"
           "https://www.gnu.org/licenses/license-list.html#MPL"))

(define mpl1.1
  (license "MPL 1.1"
           "http://directory.fsf.org/wiki/License:MPLv1.1"
           "https://www.gnu.org/licenses/license-list#MPL"))

(define mpl2.0
  (license "MPL 2.0"
           "http://directory.fsf.org/wiki/License:MPLv2.0"
           "https://www.gnu.org/licenses/license-list#MPL-2.0"))

(define ms-pl
  (license "Ms-PL"                                ;Microsoft Public License
           "http://directory.fsf.org/wiki/License:MsPL"
           "http://www.gnu.org/licenses/license-list.html#ms-pl"))

(define ncsa
  (license "NCSA/University of Illinois Open Source License"
           "http://directory.fsf.org/wiki/License:IllinoisNCSA"
           "https://www.gnu.org/licenses/license-list#NCSA"))

(define nmap
  (license "Nmap license"
           "https://svn.nmap.org/nmap/LICENSE"
           "https://fedoraproject.org/wiki/Licensing/Nmap"))

(define ogl-psi1.0
  (license "Open Government Licence for Public Sector Information"
           "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/1/"
           #f))

(define openssl
  (license "OpenSSL"
           "http://directory.fsf.org/wiki/License:OpenSSL"
           "https://www.gnu.org/licenses/license-list#OpenSSL"))

(define openldap2.8
  (license "OpenLDAPv2.8"
           "http://directory.fsf.org/wiki/License:OpenLDAPv2.8"
           "https://www.gnu.org/licenses/license-list#newOpenLDAP"))
              ;; lists OpenLDAPv2.7, which is virtually identical

(define perl-license
  ;; The license of Perl, GPLv1+ or Artistic (we ignore the latter here).
  ;; We define this alias to avoid circular dependencies introduced by the use
  ;; of the '(package-license perl)' idiom.
  gpl1+)

(define psfl
  (license "Python Software Foundation License"
           "http://docs.python.org/license.html"
           #f))

(define public-domain
  (license "Public Domain"
           "http://directory.fsf.org/wiki/License:PublicDomain"
           "https://www.gnu.org/licenses/license-list#PublicDomain"))

(define qpl
  (license "QPL"
           "http://directory.fsf.org/wiki/License:QPLv1.0"
           "http://www.gnu.org/licenses/license-list.html#QPL"))

(define qwt1.0
  (license "QWT 1.0"
           "http://qwt.sourceforge.net/qwtlicense.html"
           "GNU Lesser General Public License with exceptions"))

(define repoze
  (license "Repoze"
           "http://repoze.org/LICENSE.txt"
           "A BSD-like license with a clause requiring all changes to be
           attributed by author and date."))

(define ruby
  (license "Ruby License"
           "http://directory.fsf.org/wiki/License:Ruby"
           "https://www.ruby-lang.org/en/about/license.txt"))

(define sgifreeb2.0
  (license "SGI Free Software License B, version 2.0"
           "http://directory.fsf.org/wiki/License:SGIFreeBv2"
           "https://www.gnu.org/licenses/license-list.html#SGIFreeB"))

(define silofl1.1
  (license "SIL OFL 1.1"
           "http://scripts.sil.org/OFL_web"
           "https://www.gnu.org/licenses/license-list#SILOFL"))

(define sleepycat
  (license "Sleepycat"
           "http://directory.fsf.org/wiki/License:Sleepycat"
           "https://www.gnu.org/licenses/license-list#BerkeleyDB"))

(define tcl/tk
  (license "Tcl/Tk"
           "http://www.tcl.tk/software/tcltk/license.html"
           "A non-copyleft free software license from the Tcl/Tk project"))

(define vim
  (license "Vim"
           "http://directory.fsf.org/wiki/License:Vim7.2"
           "http://www.gnu.org/licenses/license-list.html#Vim"))

(define unicode
  (license "Unicode"
           "https://directory.fsf.org/wiki/License:Unicode"
           "http://www.gnu.org/licenses/license-list.html#Unicode"))

(define unlicense
  (license "Unlicense"
           "https://unlicense.org/"
           "https://www.gnu.org/licenses/license-list.html#Unlicense"))

(define w3c
  (license "W3C Software Notice and License"
           "https://directory.fsf.org/wiki/License:W3C_31Dec2002"
           "https://www.gnu.org/licenses/license-list.en.html#W3C"))

(define wtfpl2
  (license "WTFPL 2"
           "http://www.wtfpl.net"
           "http://www.wtfpl.net/about/"))

(define wxwindows3.1+
  (license "wxWindows 3.1+"
           "https://wxwidgets.org/about/licence"
           "https://www.gnu.org/licenses/license-list.html#Wxwind"))

(define x11
  (license "X11"
           "http://directory.fsf.org/wiki/License:X11"
           "https://www.gnu.org/licenses/license-list#X11License"))

(define* (x11-style uri #:optional (comment ""))
  "Return an X11-style license, whose full text can be found at URI,
which may be a file:// URI pointing the package's tree."
  (license "X11-style"
           uri
           (string-append
            "This is an X11-style, non-copyleft free software license.  "
            "Check the URI for details.  "
            comment)))

(define zpl2.1
  (license "Zope Public License 2.1"
           "http://directory.fsf.org/wiki?title=License:ZopePLv2.1"
           "https://www.gnu.org/licenses/license-list.html#Zope2.0"))

(define zlib
  (license "Zlib"
           "https://zlib.net/zlib_license.html"
           "https://www.gnu.org/licenses/license-list#ZLib"))

(define hpnd
  (license "HPND"
           "https://directory.fsf.org/wiki/License:HPND"
           "https://www.gnu.org/licenses/license-list#HPND"))

(define* (fsf-free uri #:optional (comment ""))
  "Return a license that does not fit any of the ones above or a collection
of licenses, approved as free by the FSF.  More details can be found at URI."
  (license "FSF-free"
           uri
           comment))

(define* (fsdg-compatible uri #:optional (comment ""))
  "Return a license that does not fit any of the ones above or a collection
of licenses, not necessarily free, but in accordance with FSDG as Non-functional
Data.  More details can be found at URI.  See also
https://www.gnu.org/distros/free-system-distribution-guidelines.en.html#non-functional-data."
  (license "FSDG-compatible"
           uri
           comment)))

;;; licenses.scm ends here
