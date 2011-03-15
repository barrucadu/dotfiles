;;; sml-mode-startup.el --- automatically extracted autoloads
;;; Code:
(add-to-list 'load-path
             (or (file-name-directory load-file-name) (car load-path)))

;;;### (autoloads (sml-yacc-mode sml-lex-mode sml-cm-mode sml-mode)
;;;;;;  "sml-mode" "sml-mode.el" (19344 19039))
;;; Generated autoloads from sml-mode.el

(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

(autoload 'sml-mode "sml-mode" "\
\\<sml-mode-map>Major mode for editing ML code.
This mode runs `sml-mode-hook' just before exiting.
\\{sml-mode-map}

\(fn)" t nil)

(add-to-list 'completion-ignored-extensions ".cm/")

(add-to-list 'auto-mode-alist '("\\.cm\\'" . sml-cm-mode))

(autoload 'sml-cm-mode "sml-mode" "\
Major mode for SML/NJ's Compilation Manager configuration files.

\(fn)" t nil)

(autoload 'sml-lex-mode "sml-mode" "\
Major Mode for editing ML-Lex files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.grm\\'" . sml-yacc-mode))

(autoload 'sml-yacc-mode "sml-mode" "\
Major Mode for editing ML-Yacc files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sml-proc" "sml-proc.el" (19344 5059))
;;; Generated autoloads from sml-proc.el

(autoload 'run-sml "sml-proc" nil t)

;;;***

;;;### (autoloads nil nil ("sml-compat.el" "sml-defs.el" "sml-move.el"
;;;;;;  "sml-util.el") (19344 21563 982091))

;;;***

