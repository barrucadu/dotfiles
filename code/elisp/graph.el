;; -*- lisp -*-

; Parametric eqns
(defun gnuplot-parametric ()
  "Insert all the basic code for a parametric graph."
  (interactive)
  (insert "set parametric\n\n")
  (insert "xT(x) = \n")
  (insert "yT(x) = \n\n")
  (insert "plot xT(t), yT(t)\n"))

; Some useful lines
(defun gnuplot-sane-defaults ()
  "Insert some useful lines."
  (interactive)
  (insert "set xzeroaxis\n")
  (insert "set yzeroaxis\n\n"))

(provide 'gnuplot-parametric)
(provide 'gnuplot-sane-defaults)
