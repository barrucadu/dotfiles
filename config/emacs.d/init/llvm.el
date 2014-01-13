(load-file "~/.emacs.d/init/utils.el")

(setq load-path
      (cons (expand-file-name "~/.emacs.d/llvm-mode/") load-path))

(with-library llvm-mode
  (add-to-list 'auto-mode-alist '("/*..ll$" . llvm-mode)))
