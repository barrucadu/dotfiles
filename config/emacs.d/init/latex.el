(load "auctex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-electric-sub-and-superscript t)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(setq auto-mode-alist (append '(("/*..tex$" . LaTeX-mode))
                              auto-mode-alist))

;; Start off a LaTeX document
(define-skeleton latex-wizard
  "Insert a LaTeX article skeleton with a wide range of included packages"
  "Title: "

  "\\documentclass[12pt,a4paper]{article}\n"
  "\\pagestyle{headings}\n\n"
  "\\usepackage[utf8x]{inputenc}\n"
  "\\usepackage{amsmath}\n"
  "\\usepackage{amsfonts}\n"
  "\\usepackage{amssymb}\n"
  "\\usepackage{amsthm}\n"
  "\\usepackage{graphicx}\n"
  "\\usepackage[margin=0.9in]{geometry}\n"
  "\\usepackage{ucs}\n"
  "\\usepackage[british]{babel}\n"
  "\\usepackage[nodayofweek]{datetime}\n"
  "\\usepackage{enumitem}\n"
  "\\usepackage{multirow}\n"
  "\\usepackage{tabularx}\n"
  "\\usepackage{float}\n"
  "\\usepackage{listings}\n"
  "\\usepackage{pdflscape}\n"
  "\\usepackage[usenames,dvipsnames]{color}\n"
  "\\usepackage{cite}\n\n"

  "\\lstset{basicstyle=\\small\\ttfamily}\n"
  "\\lstset{showstringspaces=false}\n"
  "\\lstset{numbers=left, numberstyle=\\tiny, stepnumber=1, numbersep=5pt}\n"
  "\\lstset{keywordstyle=\\color{MidnightBlue}\\bfseries}\n"
  "\\lstset{commentstyle=\\color{JungleGreen}}\n"
  "\\lstset{identifierstyle=\\color{OliveGreen}}\n"
  "\\lstset{stringstyle=\\color{Red}}\n"
  "\\lstset{backgroundcolor=\\color{LightGray}}\n"
  "\\lstset{breaklines=true}\n\n"

  "\\floatstyle{boxed}\n"
  "\\restylefloat{figure}\n\n"

  "\\author{Michael Walker}\n"
  "\\title{" str "}\n\n"
  "\\begin{document}\n\n"
  "\\maketitle{}\n\n" _ "\n\n"
  "\\end{document}")

(define-skeleton latex-skeleton
  "Insert a very minimal LaTeX article skeelton"
  "Title: "

  "\\documentclass[12pt,a4paper]{article}\n"
  "\\pagestyle{headings}\n\n"
  "\\usepackage[utf8x]{inputenc}\n"
  "\\usepackage[margin=0.9in]{geometry}\n"
  "\\usepackage{ucs}\n"
  "\\usepackage[british]{babel}\n"
  "\\usepackage[nodayofweek]{datetime}\n"

  "\\author{Michael Walker}\n"
  "\\title{" str "}\n\n"
  "\\begin{document}\n\n"
  "\\maketitle{}\n\n" _ "\n\n"
  "\\end{document}")

(define-skeleton latex-presentation
  "Insert Beamer boilerplate for presentations"
  "Title: "

  "\\documentclass{beamer}\n"
  "\\usetheme{default}\n\n"

  "\\author{Michael Walker}\n"
  "\\title{" str "}\n"
  "\\institute{Department of Computer Science\\\\\n"
  "  University of York\\\\\n"
  "  \\texttt{msw504@york.ac.uk}\n"
  "}\n\n"

  "\\begin{document}\n\n"

  "\\begin{frame}[plain]\n"
  "  \\titlepage\n"
  "\\end{frame}\n\n"

  "\\begin{frame}{Overview}\n"
  _
  "\\end{frame}\n\n"

  "\\end{document}")

; TODO: figure out why (eval-after-load 'latex-mode …) didn't work
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key "\C-cl" 'latex-wizard)
            (local-set-key "\C-cL" 'latex-skeleton)
            (local-set-key "\C-cp" 'latex-presentation)))
