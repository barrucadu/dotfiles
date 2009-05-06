;; -*- lisp -*-

; XHTML Wizard
(defun xhtml-wizard ()
  "Insert the basic XHTML tags I use."
  (interactive)
  (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
  (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" dir=\"ltr\">\n")
  (insert "  <head>\n")
  (insert "	<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\"/>\n")
  (insert "	<meta name=\"keywords\" content=\"\"/>\n")
  (insert "	<meta name=\"description\" content=\"\"/>\n")
  (insert "	<meta name=\"author\" content=\"Michael Walker\"/>\n")
  (insert "	<meta name=\"robots\" content=\"FOLLOW,INDEX\"/>\n")
  (insert "\n")
  (insert "	<title></title>\n")
  (insert "\n")
  (insert "	<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\" />\n")
  (insert "	<link rel=\"shortcut icon\" type=\"application/ico\" href=\"/favicon.ico\" />\n")
  (insert "	<base href=\"http://www./\"/>\n")
  (insert "  </head>\n")
  (insert "\n")
  (insert "  <body>\n")
  (insert "\n")
  (insert "  </body>\n")
  (insert "</html>\n"))

; XHTML Preview
(defun xhtml-preview ()
  "Preview the currently opened XHTML file"
  (interactive)
  (save-excursion
	(let ((cb (current-buffer)))
	  (set-buffer (get-buffer-create "*TEMP*"))
	  (call-process "opera" nil t nil (buffer-file-name cb)))))

; PHP Function
(defun php-function ()
  "Create a PHP function"
  (interactive)
  (insert "function ()\n")
  (insert "{\n")
  (insert "\n")
  (insert "}\n"))

(provide 'xhtml-wizard)
(provide 'xhtml-preview)
(provide 'php-function)