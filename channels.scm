;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; The Guix revision guix.scm is built against:
;;;
;;;   guix time-machine -C channels.scm -- build -f guix.scm
;;;
;;; It is the commit of the v1.5.0 release, so that CI can use the release's
;;; binary installation as it is, without building a Guix of its own: moving
;;; to another commit means changing GUIX_VERSION in
;;; .github/workflows/deploy.yml to the release that contains it, or running
;;; `guix time-machine -C channels.scm --' there.  Check that `guix build -f
;;; guix.scm' still passes, since the test suites run during that build.

(list (channel
       (name 'guix)
       (url "https://codeberg.org/guix/guix.git")
       (branch "master")
       (commit "230aa373f315f247852ee07dff34146e9b480aec") ; v1.5.0
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
