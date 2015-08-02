(load-file "~/.emacs.d/init/utils.el")

(with-library erlang
  (add-to-list 'auto-mode-alist '("/*..erl$" . erlang-mode)))
