;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Guix package of Project Isidore, and of the dependencies Guix lacks or
;;; ships too old.  This file evaluates to the package:
;;;
;;;   guix build -f guix.scm       compiles, tests, and saves bin/project-isidore
;;;   guix shell -D -f guix.scm    development environment: SBCL and the inputs
;;;
;;; Production builds it with the host's own Guix: the deploy hook that
;;; .github/workflows/deploy.yml runs installs it with `guix package -f'.
;;; `guix time-machine --commit=COMMIT --' before any of these uses that
;;; Guix; `ssh oci-a1-flex guix describe' names the host's.

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix utils)
             (guix build-system asdf)
             ((guix licenses) #:prefix license:)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define vcs-file?
  ;; Only the files under version control, so that build products and
  ;; editor files in the checkout do not change the source hash.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define-public sbcl-graph-db
  (let ((commit "4d6bf30a9318fdc5a0c14a0cb5af6f192aa3dea7") ; v4.0.1
        (revision "0"))
    (package
      (name "sbcl-graph-db")
      (version (git-version "4.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kraison/vivace-graph")
               (commit commit)))
         (file-name (git-file-name "cl-graph-db" version))
         (sha256
          (base32 "0qvbwmrhn1x4kcj03djnb70c5dl3iisn18khz9dpbnps2mqm3yfi"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The engine and its replication transport only.  The full
       ;; "graph-db" system adds the HTTP API (rest.lisp: ningle on a
       ;; Clack server of its own), which Project Isidore does not serve.
       (list #:asd-systems ''("graph-db/core" "graph-db/replication")
             ;; The test systems need FiveAM, Drakma and the full system.
             #:tests? #f))
      (inputs
       (list sbcl-alexandria
             sbcl-bordeaux-threads
             sbcl-cffi
             sbcl-cl-fad
             sbcl-cl-json
             sbcl-cl-ppcre
             sbcl-cl-store
             sbcl-ieee-floats
             sbcl-iterate
             sbcl-local-time
             sbcl-log4cl
             sbcl-md5
             sbcl-split-sequence
             sbcl-usocket
             sbcl-uuid))
      (home-page "https://github.com/kraison/vivace-graph")
      (synopsis "Graph database in Common Lisp (VivaceGraph)")
      (description
       "VivaceGraph is an ACID-compliant object graph database written in
Common Lisp, with user-defined indexes, map-reduce views, replication and a
Prolog-like query language.")
      (license license:expat))))

;; Guix's sbcl-clog is 1.2, whose INITIALIZE has no :LACK-MIDDLEWARE-LIST
;; (src/main.lisp mounts the Slack endpoints with it).  This is the v2.4
;; release, whose clog.asd still says 2.2.
(define-public sbcl-clog-2
  (package
    (inherit sbcl-clog)
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rabbibotton/clog")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-clog" version))
       (sha256
        (base32 "1z91j1yb156m12pxk7xiwr4fqgf58xi5fm23rxzv8p6w6x9snvcm"))))
    (arguments
     (list #:asd-systems ''("clog")))
    (inputs
     (list sbcl-alexandria
           sbcl-atomics
           sbcl-bordeaux-threads
           sbcl-cl-isaac
           sbcl-cl-pass
           sbcl-cl-ppcre
           sbcl-cl-sqlite
           sbcl-cl-template
           sbcl-clack
           sbcl-closer-mop
           sbcl-dbi
           sbcl-hunchentoot
           sbcl-lack
           sbcl-mgl-pax
           sbcl-parse-float
           sbcl-quri
           sbcl-trivial-gray-streams
           sbcl-websocket-driver))))

(define-public project-isidore
  (package
    (name "project-isidore")
    (version "2.0.0")
    (source (local-file "." "project-isidore-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("project-isidore")
      #:phases
      #~(modify-phases %standard-phases
          ;; 'build compiled the system and 'check ran its tests, each in a
          ;; process of its own; this one loads the system alone and saves
          ;; the executable.  Loading it starts nothing and opens nothing:
          ;; INITIALIZE-APPLICATION does that when the program runs.  (The
          ;; name must be lower case: BUILD-PROGRAM names an ASDF system
          ;; after it.)
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append #$output "/bin/project-isidore")
               outputs
               #:dependencies '("project-isidore")
               #:entry-program '((project-isidore/main:application-toplevel))))))))
    (native-inputs
     (list sbcl-rove))
    (inputs
     (list sbcl-alexandria
           sbcl-bordeaux-threads
           sbcl-cffi
           sbcl-chronicity
           sbcl-cl+ssl
           sbcl-cl-ppcre
           sbcl-clog-2
           sbcl-dexador
           sbcl-flexi-streams
           sbcl-graph-db
           sbcl-ironclad
           sbcl-jzon
           sbcl-local-time
           sbcl-montezuma
           sbcl-quri
           sbcl-slynk
           sbcl-usocket))
    (home-page "https://www.bhw.name/blog/project-isidore-doc.html")
    (synopsis "Personal web application")
    (description
     "Project Isidore is a personal web application: a website, a Douay-Rheims
Bible reader with search, and a Slack bot that books vans, served by one
process.")
    (license license:agpl3+)))

project-isidore
