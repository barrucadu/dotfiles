;; Stop emacs inserting a call to package-initialise here.
(setq package--init-file-ensured t)

;; Follow symlinks to version-controlled files without asking. This
;; has to be here, rather than in the configuration.el.org, as that
;; file is version-controlled: whenever it changes emacs asks for
;; confirmation before loading it.
(setq vc-follow-symlinks t)

;; Load the rest of the configuration.
(defconst *configuration-org-file* (expand-file-name (concat user-emacs-directory "configuration.el.org")))
(defconst *configuration-el-file*  (expand-file-name (concat user-emacs-directory "configuration.el.el")))
;; If the .el file exists and is not older than the .org file, load
;; it. Otherwise tangle and load the .org file.
(if (and (file-exists-p *configuration-el-file*) (not (time-less-p (nth 5 (file-attributes *configuration-el-file*))
                                                                   (nth 5 (file-attributes *configuration-org-file*)))))
    (load-file *configuration-el-file*)
    (org-babel-load-file *configuration-org-file*))
