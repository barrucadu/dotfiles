; Requires: agda (Hackage)

(load-file "~/.emacs.d/init/utils.el")

(dolist (file (sort (file-expand-wildcards "~/.cabal/share/*-ghc-*/Agda-*/emacs-mode") #'string<))
  (add-to-list 'load-path (expand-file-name (concat file "/"))))

(with-library agda2-mode
  (setq agda2-include-dirs `(,(expand-file-name "~/agda/lib-0.7/src") "."))
  (agda2-highlight-set-faces 'agda2-highlight 'default-faces))
