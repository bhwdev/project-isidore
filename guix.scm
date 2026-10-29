;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Guix package of Project Isidore, and of the dependencies Guix lacks or
;;; ships too old.  This file evaluates to the package:
;;;
;;;   guix build -f guix.scm       builds bin/ProjectIsidore (make.lisp)
;;;   guix shell -m manifest.scm   development environment of its inputs
;;;
;;; .github/workflows/deploy.yml packs it into the container image.  Prefix any of these
;;; with `guix time-machine -C channels.scm --' to use the Guix revision the
;;; definitions are pinned to.

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
;; (webserver.lisp mounts the Slack endpoints with it).  This is the commit
;; ocicl pinned before the move to Guix.
(define-public sbcl-clog-2
  (let ((commit "6ab1e21f40c30b9ab1acea0b62564044355c2edf")
        (revision "0"))
    (package
      (inherit sbcl-clog)
      (version (git-version "2.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rabbibotton/clog")
               (commit commit)))
         (file-name (git-file-name "cl-clog" version))
         (sha256
          (base32 "13l6v4xxqhlkb7zlzf089s6c6cr0kcn49vn213cd9rcdr0nkq3k4"))))
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
             sbcl-websocket-driver)))))

(define-public project-isidore
  (package
    (name "project-isidore")
    (version "1.2.2")
    (source (local-file "." "project-isidore-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("project-isidore")
      #:phases
      #~(modify-phases %standard-phases
          ;; make.lisp compiles, tests and dumps the program in one image, as
          ;; it does outside Guix.  It runs from the copy of the source in
          ;; the output, so that the ASDF system the program is dumped with
          ;; (and serves assets/ from) lives in the store.
          (replace 'build
            (lambda _
              (let ((source (string-append
                             #$output "/share/common-lisp/sbcl/project-isidore"))
                    (bin (string-append #$output "/bin")))
                ;; ASDF's fasl cache and the CRUD graph-db the test suites
                ;; open; neither is part of the output.
                (setenv "HOME" (getcwd))
                (setenv "PROJECT_ISIDORE_DATA" (string-append (getcwd) "/data/"))
                ;; The sandbox has no network and no bible store.
                (setenv "PROJECT_ISIDORE_OFFLINE" "1")
                (invoke "sbcl"
                        "--dynamic-space-size" "8Gb"
                        "--control-stack-size" "16Mb"
                        "--script" (string-append source "/make.lisp"))
                (mkdir-p bin)
                (rename-file (string-append source "/bin/ProjectIsidore")
                             (string-append bin "/ProjectIsidore"))
                (delete-file-recursively (string-append source "/bin")))))
          ;; make.lisp ran the test suites before dumping.
          (delete 'check))))
    (inputs
     (list sbcl-alexandria
           sbcl-bordeaux-threads
           sbcl-cffi
           sbcl-chronicity
           sbcl-cl+ssl
           sbcl-cl-csv
           sbcl-cl-ppcre
           sbcl-clog-2
           sbcl-dexador
           sbcl-flexi-streams
           sbcl-graph-db
           sbcl-hunchentoot
           sbcl-ironclad
           sbcl-jzon
           sbcl-langutils
           sbcl-local-time
           sbcl-montezuma
           ;; The test systems are loaded into the dumped image too.
           sbcl-parachute
           sbcl-quri
           sbcl-serapeum
           sbcl-series
           sbcl-slynk))
    (home-page "https://www.bhw.name/blog/project-isidore-doc.html")
    (synopsis "Personal web application")
    (description
     "Project Isidore is a personal web application: a website, a Douay-Rheims
Bible reader with search, and a Slack bot that books vans.")
    (license license:agpl3+)))

project-isidore
