; Requires: yaml-mode

(load-file "~/.emacs.d/init/utils.el")

(with-library python-mode
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
    (lambda ()
      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


