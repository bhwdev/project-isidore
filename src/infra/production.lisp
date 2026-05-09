;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
(in-package :cl-user)

(consfigurator:defpackage-consfig :project-isidore/infra
  (:use #:common-lisp #:consfigurator)
  (:export #:oci-a1-flex))

(in-package :project-isidore/infra)
(in-consfig "project-isidore/infra")
(named-readtables:in-readtable :consfigurator)

(defparameter *prod-ql-dist-ver* "20260430154000"
  "Production Ultralisp Distribution Version STRING passed as an ENV variable
'QL_DIST_VER', which is utilized in MAKE.LISP. Timestamp must have the same
format as given in (ql-dist:available-versions (ql::dist \"ultralisp\"))")

(defhost oci-a1-flex (:deploy :ssh)
  "Declarative Configuration for Production Server. To deploy to production run
locally in the listener,

CL-USER> (ql:quickload \"project-isidore/infra\")
CL-USER> (project-isidore/infra:oci-a1-flex)

NOTE that if (ql:quickload \"consfigurator\") fails with CFFI unable to find C
libraries, locally sudo apt install packages listed in
`consfigurator.property.package:+consfigurator-system-dependencies+'.

One piece of information is unique to this host: remote root SSH login access on
the local machine in ~/.ssh/config."
  (os:debian-stable "trixie" :aarch64) ; Has SBCL version 2.5.2.debian.
  (timezone:configured "America/Toronto")
  (apt:installed "firewalld")
  (systemd:enabled "firewalld")
  (systemd:started "firewalld")
  (cmd:single "firewall-cmd --permanent --add-service=https")
  (cmd:single "firewall-cmd --reload")
  (systemd:restarted "firewalld")
  (git:pulled "https://gitlab.com/bhw/project-isidore.git"
              (pathname "/usr/local/src/project-isidore/"))
  (apt:installed "sbcl")
  (cmd:single :env (list :BUILD_DIR "/usr/local/src/project-isidore/"
                         :QL_DIST_VER *prod-ql-dist-ver*)
              (concatenate 'string
                           "cd /usr/local/src/project-isidore/ && "
                           "sbcl "
                           ;; KLUDGE "--control-stack-size='4Mb' breaks."
                           "--control-stack-size '16Mb' "
                           "--dynamic-space-size '8Gb' "
                           ;; NOTE Use --script and not --load.
                           ;; http://www.sbcl.org/manual/#Toplevel-Options
                           "--script '/usr/local/src/project-isidore/make.lisp'"))
  (cmd:single "install -m 755 /usr/local/src/project-isidore/bin/ProjectIsidore /usr/local/bin/ProjectIsidore")
  ;; https://systemd-by-example.com/
  (file:has-content
   "/etc/systemd/system/project-isidore.service"
   "
[Unit]
Description=Project Isidore web application.
After=network.target

[Service]
Type=simple
User=root
Group=root
WorkingDirectory=/usr/local/src/project-isidore/
EnvironmentFile=/usr/local/etc/project-isidore/secrets.env
ExecStart=/usr/local/bin/ProjectIsidore
Restart=always

[Install]
WantedBy=multi-user.target")
  (systemd:enabled "project-isidore.service")
  (systemd:restarted "project-isidore.service")
  ;; Reverse Proxy: Caddy will communicate with the Hunchentoot server(s)
  ;; locally. Forward requests from https://123.456.789.100:443 to
  ;; http://127.0.0.1:8080 where Hunchentoot accepts HTTP requests. Obviate need
  ;; to distribute CL+SSL (openssl) when providing release binaries.
  (apt:installed "caddy" "curl")
  (systemd:enabled "caddy")
  (systemd:started "caddy")
  ;; Caddy listens on port 2019 for incremental API configuration.
  ;; We do this as Matrix Synapse is also using Caddy.
  (file:has-content
   "/etc/caddy/Caddyfile"
   "
www.bhw.name {
     reverse_proxy localhost:8080
}
matrix.bhw.name, matrix.bhw.name:8448 {
    reverse_proxy localhost:8008
}")
  (systemd:restarted "caddy")
  (reboot:at-end))
