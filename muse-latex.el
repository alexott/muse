;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse LaTeX Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-latex nil
  "Rules for marking up a Muse file as a LaTeX article."
  :group 'muse-publish)

(defcustom muse-latex-extension ".tex"
  "Default file extension for publishing LaTeX files."
  :type 'string
  :group 'muse-latex)

(defcustom muse-latex-pdf-extension ".pdf"
  "Default file extension for publishing LaTeX files to PDF."
  :type 'string
  :group 'muse-latex)

(defcustom muse-latex-header
  "\\documentclass{article}

\\usepackage[english]{babel}
\\usepackage[latin1]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage[pdftex]{graphicx}

\\begin{document}

\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

<lisp>(and muse-publish-generate-contents
	   \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n\n"
  "Header used for publishing LaTeX files."
  :type '(choice string file)
  :group 'muse-latex)

(defcustom muse-latex-footer "\n\\end{document}"
  "Footer used for publishing LaTeX files."
  :type '(choice string file)
  :group 'muse-latex)

(defcustom muse-latex-markup-regexps
  `(;; numeric ranges
    (10000 "\\([0-9]+\\)-\\([0-9]+\\)" 0 "\\1--\\2")

    ;; characters which need quoting
    (10100 "\\([$#_]\\)" 0 "\\\\\\1")

    ;; be careful of closing quote pairs
    (10200 "\"'" 0 "\"\\\\-'")

    ;; join together the parts of a list or table
    (10300 ,(concat
	     "\\\\end{\\(tabular\\|description\\|itemize\\|enumerate\\)}\n+"
	     "\\\\begin{\\1}\\({[^\n}]+}\\)?\n+") 0 ""))
  "List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
		  (list :tag "Markup rule"
			integer
			(choice regexp symbol)
			integer
			(choice string function symbol))
		  function))
  :group 'muse-latex)

(defcustom muse-latex-markup-functions
  '((table . muse-latex-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-latex)

(defcustom muse-latex-markup-strings
  '((image-with-desc . "\\includegraphics[width=\\textwidth]{%s}")
    (image-link      . "\\includegraphics[width=\\textwidth]{%s}")
    (url-with-image  . "%% %s\n\\includegraphics[width=\\textwidth]{%s}")
    (url-link        . "\\href{%s}{%s}")
    (email-addr      . "\\verb|%s|")
    (emdash          . "---")
    (rule            . "\\bigskip")
    (enddots         . "\\ldots{}")
    (dots            . "\\dots{}")
    (part            . "\\part{")
    (part-end        . "}")
    (chapter         . "\\chapter{")
    (chapter-end     . "}")
    (section         . "\\section{")
    (section-end     . "}")
    (subsection      . "\\subsection{")
    (subsection-end  . "}")
    (subsubsection   . "\\subsubsection{")
    (subsubsection-end . "}")
    (footnote        . "\\footnote{")
    (footnote-end    . "{")
    (footnotemark    . "\\footnotemark[%d]")
    (footnotetext    . "\\footnotetext[%d]{")
    (footnotetext-end . "}")
    (begin-underline . "\\underline{")
    (end-underline   . "}")
    (begin-literal   . "\\verb|")
    (end-literal     . "|")
    (begin-emph      . "\\emph{")
    (end-emph        . "}")
    (begin-more-emph . "\\textbf{")
    (end-more-emph   . "}")
    (begin-most-emph . "\\textbf{\\emph{")
    (end-most-emph   . "}}")
    (begin-verse     . "\\begin{verse}\n")
    (end-verse-line  . " \\\\")
    (verse-space     . "~~~~")
    (end-verse       . "\n\\end{verse}")
    (begin-example   . "\\begin{quote}\n\\begin{verbatim}")
    (end-example     . "\\end{verbatim}\n\\end{quote}")
    (begin-center    . "\\begin{center}\n")
    (end-center      . "\n\\end{center}")
    (begin-quote     . "\\begin{quote}\n")
    (end-quote       . "\n\\end{quote}")
    (begin-uli       . "\\begin{itemize}\n\\item ")
    (end-uli         . "\n\\end{itemize}")
    (begin-oli       . "\\begin{enumerate}\n\\item ")
    (end-oli         . "\n\\end{enumerate}")
    (begin-ddt       . "\\begin{description}\n\\item[")
    (start-dde       . "] ")
    (end-ddt         . "\\end{description}"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-latex)

(defcustom muse-latex-markup-specials
  '((?\\ . "\\\\"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-latex)

(defun muse-latex-markup-table ()
  (let* ((str (prog1
		  (match-string 1)
		(delete-region (match-beginning 0) (match-end 0))))
	 (fields (split-string str "\\s-*|+\\s-*"))
	 (type (and (string-match "\\s-*\\(|+\\)\\s-*" str)
		    (length (match-string 1 str)))))
    (insert "\\begin{tabular}{" (make-string (length fields) ?l) "}\n")
    (insert (mapconcat 'identity fields " & "))
    (insert " \\\\\n\\end{tabular}")))

(defun muse-latex-fixup-dquotes ()
  "Fixup double quotes."
  (let ((open t))
    (while (search-forward "\"" nil t)
      (unless (get-text-property (match-beginning 0) 'read-only)
	(if (and (bolp) (eq (char-before) ?\n))
	    (setq open t))
	(if open
	    (progn
	      (replace-match "``")
	      (setq open nil))
	  (replace-match "''")
	  (setq open t))))))

(defun muse-latex-finalize-buffer ()
  (goto-char (point-min))
  (muse-latex-fixup-dquotes))

(defun muse-latex-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-latex-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command (format "cd %s; pdflatex %s"
			     (file-name-directory output-path) file)))
	(and (= 0 (shell-command command))
	     (= 0 (shell-command command))
	     (= 0 (shell-command command))))))
   ".aux" ".toc" ".out" ".log"))

(unless (assoc "latex" muse-publishing-styles)
  (muse-define-style "latex"
		     :suffix    'muse-latex-extension
		     :regexps   'muse-latex-markup-regexps
		     :functions 'muse-latex-markup-functions
		     :strings   'muse-latex-markup-strings
		     :specials  'muse-latex-markup-specials
		     :after     'muse-latex-finalize-buffer
		     :header    'muse-latex-header
		     :footer    'muse-latex-footer
		     :browser   'find-file)

  (muse-derive-style "pdf" "latex"
		     :final   'muse-latex-pdf-generate
		     :browser 'muse-latex-pdf-browse-file
		     :osuffix 'muse-latex-pdf-extension))

(provide 'muse-latex)

;;; muse-latex.el ends here
