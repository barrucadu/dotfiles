(load-file "~/.emacs.d/init/utils.el")

(with-library python-mode
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))
