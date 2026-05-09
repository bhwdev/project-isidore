;;;; generate-doc.lisp
;;; # environment setup
;;; sbcl --load /home/ben/common-lisp/project-isidore/src/generate-doc.lisp
;;; # Add updated manual.html to commit
;;; git add /home/ben/common-lisp/project-isidore/assets/reference-manual.html
;;; See subheading 'Generate Reference Manual' at
;;; https://www.bhw.name/assets/blog/project-isidore-doc.html

;; Load ocicl-runtime so ASDF can resolve project-isidore + declt and any
;; transitive deps from ./ocicl.csv. Mirrors make.lisp's prelude.
(let ((runtime (or (uiop:getenvp "OCICL_RUNTIME")
                   (merge-pathnames ".local/share/ocicl/ocicl-runtime.lisp"
                                    (user-homedir-pathname)))))
  (when (probe-file runtime) (load runtime)))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))
(asdf:load-system :project-isidore)
(asdf:load-system :net.didierverna.declt)
(net.didierverna.declt:declt :project-isidore
                             ;; Generate 'project-isidore.texi' in TEXI-DIRECTORY
                             :file-name "reference-manual"
                             :output-directory
                             (asdf:system-relative-pathname
                              :project-isidore "assets/")
                             :library-name "Project Isidore"
                             ;; links are machine specific
                             :locations nil
                             ;; :long will print generation time. This will be
                             ;; picked up by git. Otherwise I would pick :long
                             :declt-notice :short)
(uiop:run-program `("/usr/bin/makeinfo"
                    ;; Convert .texi to .html
                    "--html" ,(concatenate 'string
                                           (namestring
                                            (asdf:system-relative-pathname
                                             :project-isidore "assets/"))
                                           "reference-manual.texi")
                    "--no-split"
                    "--css-include" "global.css"
                    "--output" ,(concatenate 'string
                                             (namestring
                                              (asdf:system-relative-pathname
                                               :project-isidore "assets/"))
                                             "reference-manual.html"))
                  :output t)
(uiop:quit)
