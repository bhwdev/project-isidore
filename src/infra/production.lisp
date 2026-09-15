;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
(in-package :cl-user)

(consfigurator:defpackage-consfig :project-isidore/infra
    (:use #:common-lisp #:consfigurator)
  (:export #:oci-a1-flex))

(in-package :project-isidore/infra)
(in-consfig "project-isidore/infra")
(named-readtables:in-readtable :consfigurator)


(defhost oci-a1-flex (:deploy :ssh)
  "Declarative Configuration for Production Server. To deploy to production run
locally in the listener,

CL-USER> (asdf:load-system \"project-isidore/infra\")
CL-USER> (project-isidore/infra:oci-a1-flex)

NOTE that if (asdf:load-system \"consfigurator\") fails with CFFI unable to find
C libraries, locally apt install packages listed in
`consfigurator.property.package:+consfigurator-system-dependencies+'.

One piece of information is unique to this host: remote root SSH login access on
the local machine in ~/.ssh/config.

The host is an OCI VM.Standard.A1.Flex (Ampere Altra / Neoverse-N1), resized
from 2 OCPU / 12 GB to 4 OCPU / 24 GB on 2026-09-05 so that llama-server (below)
has cores to spare beside the web application and the Matrix homeserver."
  (os:debian-stable "trixie" :aarch64) ; Has SBCL version 2.5.2.debian.
  (timezone:configured "America/Toronto")
  (apt:installed "firewalld")
  (systemd:enabled "firewalld")
  (systemd:started "firewalld")
  (cmd:single "firewall-cmd --permanent --add-service=https")
  (cmd:single "firewall-cmd --reload")
  (systemd:restarted "firewalld")
  (apt:installed "sbcl" "sbcl-source" "git")
  ;; Put the server checkout exactly at origin/master so that GIT:PULLED below
  ;; is a no-op fast-forward (its "git pull" is must-succeed and refuses a
  ;; diverged checkout, which is what a rewritten master upstream leaves
  ;; behind).  Guarded so that a first deploy, which has no checkout yet, does
  ;; not fail here.  NOTE the command runs in root's home, not the checkout,
  ;; hence the explicit -C.
  (cmd:single "test -d /usr/local/src/project-isidore/.git && git -C /usr/local/src/project-isidore fetch origin && git -C /usr/local/src/project-isidore reset --hard origin/master || true")
  (git:pulled "https://gitlab.com/bhw/project-isidore.git"
              (pathname "/usr/local/src/project-isidore/"))
  ;; ocicl lives in root's ~/.local/bin, which a non-interactive ssh shell
  ;; does not put on PATH ("sh: ocicl: not found" otherwise).
  (cmd:single "PATH=\"$HOME/.local/bin:$PATH\" ocicl setup")
  ;; Dependency pinning lives in ocicl.csv at the project root, brought in by
  ;; the git:pulled step above. make.lisp loads ocicl-runtime; ocicl auto-fetches
  ;; any system listed in ocicl.csv that's missing from the local ocicl/ dir on
  ;; the first asdf:load-system call.
  (cmd:single :env (list :BUILD_DIR "/usr/local/src/project-isidore/")
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
  ;; llama.cpp for the van-booking bot's model fallback (src/booking-llm.lisp).
  ;; The upstream release tarball runs as-is on trixie and picks its armv8.2
  ;; (dotprod) CPU kernels at runtime, so nothing is built here.  Both
  ;; downloads are pinned by sha256, verified on every deploy and skipped when
  ;; already in place.  The server binds to the loopback interface only, so
  ;; the firewall needs nothing.
  (apt:installed "curl" "ca-certificates")
  (file:directory-exists "/opt/llama.cpp/")
  (file:directory-exists "/var/local/project-isidore/models/")
  (cmd:single (concatenate 'string
                           "T=/usr/local/src/llama-b10819-bin-ubuntu-arm64.tar.gz; "
                           "S=6f6f7e1e9b371d4840860a79ccbf4ad6f7da9da76349ce73079e3289b01033ec; "
                           "test \"$(cat /opt/llama.cpp/VERSION 2>/dev/null)\" = b10819 || ( "
                           "curl -fL --retry 3 -o \"$T\" https://github.com/ggml-org/llama.cpp/releases/download/b10819/llama-b10819-bin-ubuntu-arm64.tar.gz && "
                           "echo \"$S  $T\" | sha256sum -c --status && "
                           "rm -rf /opt/llama.cpp/bin && mkdir -p /opt/llama.cpp/bin && "
                           "tar xzf \"$T\" -C /opt/llama.cpp/bin --strip-components=1 && "
                           "echo b10819 > /opt/llama.cpp/VERSION )"))
  (cmd:single (concatenate 'string
                           "M=/var/local/project-isidore/models/Qwen3.5-0.8B-Q8_0.gguf; "
                           "S=0ad885ffd4bb022fc4f0d33a3308fa108ef8613159d3b3a67e23abca056b7a6c; "
                           "( test -f \"$M\" && echo \"$S  $M\" | sha256sum -c --status ) || ( "
                           "curl -fL --retry 3 -o \"$M.part\" https://huggingface.co/unsloth/Qwen3.5-0.8B-GGUF/resolve/main/Qwen3.5-0.8B-Q8_0.gguf && "
                           "echo \"$S  $M.part\" | sha256sum -c --status && "
                           "mv \"$M.part\" \"$M\" && chmod 644 \"$M\" )"))
  ;; Three of the four cores, niced below the web application; the memory cap
  ;; confines any blow-up to this unit (the host has no swap).  Thinking is
  ;; off because llama.cpp does not enforce the JSON grammar while a model
  ;; thinks; --cache-reuse keeps the fixed system prompt in the KV cache.
  (file:has-content
   "/etc/systemd/system/llama-server.service"
   "
[Unit]
Description=llama.cpp server: intent normalizer for the van-booking bot (loopback only).
After=network.target

[Service]
Type=simple
DynamicUser=yes
ExecStart=/opt/llama.cpp/bin/llama-server --host 127.0.0.1 --port 8081 -m /var/local/project-isidore/models/Qwen3.5-0.8B-Q8_0.gguf -t 3 -c 2048 --parallel 1 --cache-reuse 256 --jinja --reasoning-budget 0 --chat-template-kwargs '{\"enable_thinking\":false}' --no-webui
Restart=always
RestartSec=5
Nice=10
CPUWeight=50
MemoryHigh=3G
MemoryMax=4G
OOMScoreAdjust=500
NoNewPrivileges=yes
ProtectSystem=strict
ProtectHome=yes
PrivateTmp=yes
ReadOnlyPaths=/var/local/project-isidore/models

[Install]
WantedBy=multi-user.target")
  (systemd:daemon-reloaded)
  (systemd:enabled "llama-server.service")
  (systemd:restarted "llama-server.service")
  ;; https://systemd-by-example.com/
  ;; VAN_LLM_URL turns the model fallback on; an operator can turn it off
  ;; without a deploy by putting an empty VAN_LLM_URL= in secrets.env, since
  ;; EnvironmentFile= overrides Environment=.
  (file:has-content
   "/etc/systemd/system/project-isidore.service"
   "
[Unit]
Description=Project Isidore web application.
After=network.target llama-server.service
Wants=llama-server.service

[Service]
Type=simple
User=root
Group=root
WorkingDirectory=/usr/local/src/project-isidore/
Environment=VAN_LLM_URL=http://127.0.0.1:8081/v1/chat/completions
EnvironmentFile=/usr/local/etc/project-isidore/secrets.env
ExecStart=/usr/local/bin/ProjectIsidore
Restart=always

[Install]
WantedBy=multi-user.target")
  (systemd:daemon-reloaded)
  (systemd:enabled "project-isidore.service")
  (systemd:restarted "project-isidore.service")
  ;; Reverse Proxy: Caddy will communicate with the Hunchentoot server(s)
  ;; locally. Forward requests from https://123.456.789.100:443 to
  ;; http://127.0.0.1:8080 where Hunchentoot accepts HTTP requests. Obviate need
  ;; to distribute CL+SSL (openssl) when providing release binaries.
  (apt:installed "caddy")
  (systemd:enabled "caddy")
  (systemd:started "caddy")
  ;; This form OWNS /etc/caddy/Caddyfile: FILE:HAS-CONTENT rewrites the whole
  ;; file on every deploy, so a site block added by hand on the server is gone
  ;; at the next push (that took vdb.bhw.name down on 2026-09-06).  Every site
  ;; this host's Caddy fronts is declared here: the Matrix homeserver
  ;; (tuwunel, 8008/8448) and the public VolunteerDB demo, whose own deploy
  ;; runs with [proxy] caddy = false for exactly this reason (volunteerdb
  ;; repo, deploy/sites/bhwdemo.toml).  The vdb block adds what that app
  ;; never emits itself: compression, the security headers, a deny-all
  ;; robots.txt and a request-body cap; the app listens on 127.0.0.1:8100.
  ;; Certificates come from Let's Encrypt over TLS-ALPN-01 on 443, so a new
  ;; name needs no firewall change (80 stays closed).
  (file:has-content
   "/etc/caddy/Caddyfile"
   "
www.bhw.name {
     reverse_proxy localhost:8080
}
matrix.bhw.name, matrix.bhw.name:8448 {
    # MatrixRTC (Element Call).  The homeserver's .well-known/matrix/client
    # advertises https://matrix.bhw.name/livekit/jwt as its rtc_focus, and
    # lk-jwt-service (127.0.0.1:8090) hands the client back an SFU URL of
    # wss://matrix.bhw.name/livekit/sfu, so both prefixes must be routed and
    # stripped here.  Losing these two blocks is what made every voice call
    # fail with 404 M_UNRECOGNIZED on /livekit/jwt/sfu/get: the POST landed on
    # tuwunel, which knows no such endpoint.  Media itself does not pass
    # through Caddy -- it goes straight to livekit-server on 7881/tcp and
    # 7882/udp, both open in firewalld's permanent config.
    handle_path /livekit/jwt/* {
        reverse_proxy 127.0.0.1:8090
    }
    handle_path /livekit/sfu/* {
        reverse_proxy 127.0.0.1:7880
    }
    handle {
        reverse_proxy localhost:8008
    }
}
vdb.bhw.name {
    encode zstd gzip
    request_body {
        max_size 12MB
    }
    header {
        Strict-Transport-Security \"max-age=31536000; includeSubDomains\"
        X-Content-Type-Options nosniff
        Referrer-Policy strict-origin-when-cross-origin
        X-Frame-Options DENY
        X-Robots-Tag noindex
        -Server
    }
    respond /robots.txt \"User-agent: *
Disallow: /\" 200
    reverse_proxy 127.0.0.1:8100
}")
  (systemd:restarted "caddy")
  (reboot:at-end))
