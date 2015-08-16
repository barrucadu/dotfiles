; Requires: markdown-mode

(load-file "~/.emacs.d/init/utils.el")

(with-library markdown-mode
  (add-to-list 'auto-mode-alist '("/*..md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("/*..markdown$" . markdown-mode)))
