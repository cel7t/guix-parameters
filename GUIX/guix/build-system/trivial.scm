;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system trivial)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:export (trivial-build-system))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                guile builder (modules '()) allowed-references)
  "Return a bag for NAME."
  (bag
    (name name)
    (system system)
    (target target)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs))
    (build-inputs native-inputs)
    (outputs outputs)
    (build (if target trivial-cross-build trivial-build))
    (arguments `(#:guile ,guile
                 #:builder ,builder
                 #:modules ,modules
                 #:allowed-references ,allowed-references))))

(define* (trivial-build name inputs
                        #:key
                        outputs guile
                        system builder (modules '())
                        search-paths allowed-references)
  "Run build expression BUILDER, an expression, for SYSTEM.  SOURCE is
ignored."
  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f))
                      (builder -> (if (pair? builder)
                                      (sexp->gexp builder)
                                      builder)))
    (gexp->derivation name (with-build-variables inputs outputs builder)
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:modules modules
                      #:allowed-references allowed-references
                      #:guile-for-build guile)))

(define* (trivial-cross-build name
                              #:key
                              target
                              source build-inputs target-inputs host-inputs
                              outputs guile system builder (modules '())
                              search-paths native-search-paths
                              allowed-references)
  "Run build expression BUILDER, an expression, for SYSTEM.  SOURCE is
ignored."
  (mlet %store-monad  ((guile (package->derivation (or guile (default-guile))
                                                   system #:graft? #f))
                       (builder -> (if (pair? builder)
                                       (sexp->gexp builder)
                                       builder)))
    (gexp->derivation name (with-build-variables
                               (append build-inputs target-inputs host-inputs)
                               outputs
                             builder)
                      #:system system
                      #:target target
                      #:graft? #f
                      #:modules modules
                      #:allowed-references allowed-references
                      #:guile-for-build guile)))

(define trivial-build-system
  (build-system
    (name 'trivial)
    (description
     "Trivial build system, to run arbitrary Scheme build expressions")
    (lower lower)))
