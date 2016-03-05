; Requires: nix-mode

(load-file "~/.emacs.d/init/utils.el")

(with-library nix-mode
  (add-to-list 'auto-mode-alist '("/*..nix$" . nix-mode)))
