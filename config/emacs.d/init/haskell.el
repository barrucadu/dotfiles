(load-file "~/.emacs.d/init/utils.el")

(with-library haskell-mode
  (with-library inf-haskell
    (add-to-list 'auto-mode-alist '("\\.hs$"  . haskell-mode))
    (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))

    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

    (setq haskell-font-lock-symbols t)))
