(load-file "~/.emacs.d/init/utils.el")

(with-library pkgbuild-mode
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))
