;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Development environment: SBCL and every Common Lisp dependency of Project
;;; Isidore, taken from the inputs of the package in guix.scm.
;;;
;;;   guix shell -m manifest.scm -- sbcl
;;;   guix time-machine -C channels.scm -- shell -m manifest.scm -- sbcl
;;;
;;; ASDF finds the dependencies through $XDG_CONFIG_DIRS, which the shell
;;; sets; make.lisp shows a source registry that uses only those.

(use-modules (guix profiles))

(package->development-manifest (load "guix.scm"))
