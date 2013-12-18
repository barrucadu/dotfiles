(load-file "~/.emacs.d/init/utils.el")

(with-library lua-mode
  (add-to-list 'auto-mode-alist '("/*..lua$" . lua-mode)))
