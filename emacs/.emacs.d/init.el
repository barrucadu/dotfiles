;; Stop emacs inserting a call to package-initialise here.
(setq package--init-file-ensured t)

;; Follow symlinks to version-controlled files without asking. This
;; has to be here, rather than in the configuration.el.org, as that
;; file is version-controlled: whenever it changes emacs asks for
;; confirmation before loading it.
(setq vc-follow-symlinks t)

;; Load the rest of the configuration.
(org-babel-load-file "~/.emacs.d/configuration.el.org")
