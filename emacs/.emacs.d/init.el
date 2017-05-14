;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Use a non-version-controlled file for customised variables.  I
;; generally don't use this, but some things like to vomit custom
;; things in here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Follow symlinks to version-controlled files without asking. This
;; has to be here, rather than in the configuration.el.org, as that
;; file is version-controlled: whenever it changes emacs asks for
;; confirmation before loading it.

(setq vc-follow-symlinks t)

;; Load the rest of the configuration.
(org-babel-load-file "~/.emacs.d/configuration.el.org")
