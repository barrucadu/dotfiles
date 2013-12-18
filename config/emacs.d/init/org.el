(load-file "~/.emacs.d/init/utils.el")

(with-library org
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))
