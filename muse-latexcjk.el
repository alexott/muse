;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse LaTeX Publishing with CJK support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-latex)

(defcustom muse-latexcjk-header
  "\\documentclass{article}

\\usepackage{CJK}
\\usepackage{indentfirst}
\\usepackage[CJKbookmarks=true]{hyperref}
\\usepackage[pdftex]{graphicx}

\\begin{document}
\\begin{CJK*}<lisp>(muse-latexcjk-encoding)</lisp>

\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

<lisp>(and muse-publish-generate-contents
	   \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n\n"
  "Header used for publishing LaTeX files(CJK)."
  :type '(choice string file)
  :group 'muse-latex)

(defcustom muse-latexcjk-footer 
  "
\\end{CJK*}
\\end{document}"
  "Footer used for publishing LaTeX files(CJK)."
  :type '(choice string file)
  :group 'muse-latex)

(defcustom muse-latexcjk-encoding-map
  '((utf-8		. "{UTF8}{song}")
    (japanese-iso-8bit	. "[dnp]{JIS}{min}")
    (chinese-big5	. "{Bg5}{bsmi}")
    (mule-utf-8         . "{UTF8}{song}")
    (chinese-iso-8bit   . "{GB}{song}")
    (chinese-gbk        . "{GBK}{song}"))
  "An alist mapping emacs coding systems to appropriate CJK codings.
  Use the base name of the coding system (ie, without the -unix)"
  :type '(alist :key-type coding-system :value-type string)
  :group 'muse-latex)

(defcustom muse-latexcjk-encoding-default "{GB}{song}"
  "The default emacs coding  use if no special characters are found"
  :type 'string
  :group 'muse-latex)

(defun muse-latexcjk-encoding ()
  (muse-latexcjk-transform-content-type buffer-file-coding-system))

(defun muse-latexcjk-transform-content-type (content-type)
  "Using `muse-cjklatex-encoding-map', try and resolve an emacs coding
system to an associated CJK coding system."
  (let ((match (assoc (coding-system-base content-type)
		      muse-latexcjk-encoding-map)))
    (if match
	(cdr match)
      muse-latexcjk-encoding-default)))

(unless (assoc "latexcjk" muse-publishing-styles)
  (muse-derive-style "latexcjk" "latex"
		     :header    'muse-latexcjk-header
		     :footer    'muse-latexcjk-footer)
  (muse-derive-style "pdfcjk" "latexcjk"
		     :final   'muse-latex-pdf-generate
		     :browser 'muse-latex-pdf-browse-file
		     :osuffix 'muse-latex-pdf-extension))

(provide 'muse-latexcjk)
