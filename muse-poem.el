;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Poem Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;; I use a very particular for recording poetry.  It is:
;;
;;   Title
;;
;;
;;   Body of poem
;;
;;
;;   Annotations, history, notes, etc.
;;
;; The `muse-poem' module makes it easy to attractively publish and
;; reference poems in this format, using the "memoir" module for LaTeX
;; publishing.  It will also markup poems for every other output
;; style, though none are nearly as pretty.
;;
;; Once a poem is written in this format, just publish it to PDF using
;; the "poem-pdf" style.  To make an inlined reference to a poem that
;; you've written -- for example, from a blog page -- there is a
;; "poem" tag defined by this module:
;;
;;   <poem title="name.of.poem.page">
;;
;; Let's assume the template above was called "name.of.poem.page";
;; then the above tag would result in this inclusion:
;;
;;   ** Title
;;
;;   > Body of poem
;;
;; I use this module for publishing all of the poems on my website,
;; which are at: http://www.newartisans.com/johnw/poems.html.

(require 'muse-latex)
(require 'muse-project)

(defgroup muse-poem nil
  "Rules for marking up a Muse file as a LaTeX article."
  :group 'muse-latex)

(defcustom muse-poem-latex-header
  "\\documentclass[14pt,oneside]{memoir}

\\usepackage[english]{babel}
\\usepackage[latin1]{inputenc}
\\usepackage[T1]{fontenc}

\\setlength{\\beforepoemtitleskip}{-5.0ex}

\\begin{document}

\\pagestyle{empty}

\\renewcommand{\\poemtoc}{section}
\\settocdepth{section}

\\mbox{}
\\vfill

\\poemtitle{<lisp>(muse-publishing-directive \"title\")</lisp>}

\\settowidth{\\versewidth}{<lisp>muse-poem-longest-line</lisp>}\n\n"
  "Header used for publishing LaTeX poems."
  :type '(choice string file)
  :group 'muse-poem)

(defcustom muse-poem-latex-footer "\n\\vfill
\\mbox{}

\\end{document}"
  "Footer used for publishing LaTeX files."
  :type '(choice string file)
  :group 'muse-poem)

(defvar muse-poem-markup-strings
  '((begin-verse . "\\begin{verse}[\\versewidth]\n")
    (verse-space . "\\vin "))
  "Redefine the section heads, to match my private LaTeX setup.")

(defcustom muse-chapbook-latex-header
  "\\documentclass{book}

\\usepackage[english]{babel}
\\usepackage[latin1]{inputenc}
\\usepackage[T1]{fontenc}

\\setlength{\\beforepoemtitleskip}{-5.0ex}

\\begin{document}

\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

\\tableofcontents

\\renewcommand{\\poemtoc}{section}
\\settocdepth{section}\n"
  "Header used for publishing books to LaTeX."
  :type '(choice string file)
  :group 'muse-poem)

(defcustom muse-chapbook-latex-footer "\n\\end{document}"
  "Footer used for publishing books to LaTeX."
  :type '(choice string file)
  :group 'muse-poem)

(defvar muse-poem-longest-line "")

(defvar muse-poem-chapbook-strings
  '((begin-verse . "\\newpage
\\mbox{}
\\vfill

\\poemtitle{<lisp>(muse-publishing-directive \"title\")</lisp>}

\\settowidth{\\versewidth}{<lisp>muse-poem-longest-line</lisp>}

\\begin{verse}[\\versewidth]\n")
    (end-verse   . "\n\\end{verse}\n\\vfill\n\\mbox{}")
    (verse-space . "\\vin "))
  "Redefine the section heads, to match my private LaTeX setup.")

(defun muse-poem-prepare-buffer ()
  (goto-char (point-min))
  (insert "#title ")
  (forward-line 1)
  (delete-region (point) (1+ (line-end-position)))
  (insert "\n<verse>")
  (let ((beg (point)) end line)
    (if (search-forward "\n\n\n" nil t)
	(progn
	  (setq end (copy-marker (match-beginning 0) t))
	  (replace-match "\n</verse>\n")
	  (delete-region (point) (point-max)))
      (goto-char (point-max))
      (setq end (point))
      (insert "</verse>\n"))
    (goto-char (1+ beg))
    (set (make-local-variable 'muse-poem-longest-line) "")
    (while (< (point) end)
      (setq line (buffer-substring-no-properties (point)
						 (line-end-position)))
      (if (> (length line) (length muse-poem-longest-line))
	  (setq muse-poem-longest-line line))
      (forward-line 1))
    nil))

(eval-when-compile
  (defvar muse-current-project))

(defvar muse-poem-tag '("poem" nil t muse-poem-markup-tag))

(defun muse-poem-markup-tag (beg end attrs)
  "This markup tag allows a poem to be included from another project page.
The form of usage is:
  <poem title=\"page.name\">"
  (let ((page (cdr (assoc (cdr (assoc "title" attrs))
			  (muse-project-file-alist))))
	beg start end text)
    (if (null page)
	(insert "  *Reference to\n  unknown poem \""
		(cdr (assoc "title" attrs)) "\".*\n")
      (setq beg (point))
      (insert
       (with-temp-buffer
	 (insert-file-contents page)
	 (goto-char (point-min))
	 (if (assoc "nohead" attrs)
	     (progn
	       (forward-line 3)
	       (delete-region (point-min) (point)))
	   (insert "** ")
	   (search-forward "\n\n\n")
	   (replace-match "\n\n"))
	 (if (search-forward "\n\n\n" nil t)
	     (setq end (match-beginning 0))
	   (setq end (point-max)))
	 (buffer-substring-no-properties (point-min) end)))
      (setq end (point-marker))
      (goto-char beg)
      (unless (assoc "nohead" attrs)
	(forward-line 2))
      (while (< (point) end)
	(insert "> ")
	(forward-line 1)))))

(add-to-list 'muse-publish-markup-tags muse-poem-tag)

(unless (assoc "poem-latex" muse-publishing-styles)
  (muse-derive-style "poem-latex" "latex"
		     :before  'muse-poem-prepare-buffer
		     :strings 'muse-poem-markup-strings
		     :header  'muse-poem-latex-header
		     :footer  'muse-poem-latex-footer)

  (muse-derive-style "poem-pdf" "pdf"
		     :before  'muse-poem-prepare-buffer
		     :strings 'muse-poem-markup-strings
		     :header  'muse-poem-latex-header
		     :footer  'muse-poem-latex-footer)

  (muse-derive-style "chapbook-latex" "latex"
		     :before  'muse-poem-prepare-buffer
		     :strings 'muse-poem-chapbook-strings
		     :header  'muse-chapbook-latex-header
		     :footer  'muse-chapbook-latex-footer)

  (muse-derive-style "chapbook-pdf" "pdf"
		     :before  'muse-poem-prepare-buffer
		     :strings 'muse-poem-chapbook-strings
		     :header  'muse-chapbook-latex-header
		     :footer  'muse-chapbook-latex-footer))

(provide 'muse-poem)

;;; muse-poem.el ends here
