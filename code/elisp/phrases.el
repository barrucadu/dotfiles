;; -*- lisp -*-

; Id est
(defun ie ()
  "Insert the Latin phrase i.e. in text"
  (interactive)
  (insert "\\textit{id est}"))

; Quod erat demonstrandum
(defun qed ()
  "Insert the Latin phrase QED in text"
  (interactive)
  (insert "\\textit{quod erat demonstrandum}"))

; Quod erat faciendum
(defun qef ()
  "Insert the Latin phrase QEF in text"
  (interactive)
  (insert "\\textit{quod erat faciendum}"))

; Quod est
(defun qest ()
  "Insert the Latin phrase quod est in text"
  (interactive)
  (insert "\\textit{quod est}"))

; Exempli gratia
(defun eg ()
  "Insert the Latin phrase e.g. in text"
  (interactive)
  (insert "\\textit{exempli gratia}"))

; Sub exempli gratia
(defun seg ()
  "Insert the Latin phrase s.e.g. in text"
  (interactive)
  (insert "\\textit{sub exempli gratia}"))

; Et al
(defun et-al ()
  "Insert the Latin phrase et al in text"
  (interactive)
  (insert "\\textit{et al}"))

; Et cetera
(defun etc ()
  "Insert the Latin phrase etc. in text"
  (interactive)
  (insert "\\textit{et cetera}"))

; Vel non
(defun vel-non ()
  "Insert the Latin phrase vel non in text"
  (interactive)
  (insert "\\textit{vel non}"))

; Ad libitum
(defun ad-lib ()
  "Insert the Latin phrase ad libitum in text"
  (interactive)
  (insert "\\textit{ad libitum}"))

; Vox populi
(defun vox-pop ()
  "Insert the Latin phrase vox populi in text"
  (interactive)
  (insert "\\textit{vox populi}"))

; Status quo
(defun stat-quo ()
  "Insert the Latin phrase status quo in text"
  (interactive)
  (insert "\\textit{status quo}"))

; Ex nihilo nihil fit
(defun ex-nihilo-nihil-fit ()
  "Insert the Latin phrase ex nihilo nihil fit in text"
  (interactive)
  (insert "\\textit{ex nihilo nihil fit}"))

(provide 'ie)
(provide 'eg)
(provide 'et-al)
(provide 'etc)
(provide 'vel-non)
(provide 'ad-lib)
(provide 'vox-pop)
(provide 'stat-quo)
