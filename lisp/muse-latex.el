;;; muse-latex.el --- publish entries in LaTex or PDF format

;; Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; Emacs Muse is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:

;; Li Daobing (lidaobing AT gmail DOT com) provided CJK support.

;; Trent Buck (trentbuck AT gmail DOT com) gave valuable advice for
;; how to treat LaTeX specials and the like.

;; Matthias Kegelmann (mathias DOT kegelmann AT sdm DOT de) provided a
;; scenario where we would need to respect the <contents> tag.

;; Jean Magnan de Bornier (jean AT bornier DOT net) provided the
;; markup string for link-and-anchor.

;;; Code:

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
           (not muse-latex-permit-contents-tag)
           \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n\n"
  "Header used for publishing LaTeX files.  This may be text or a filename."
  :type 'string
  :group 'muse-latex)

(defcustom muse-latex-footer "\n\\end{document}\n"
  "Footer used for publishing LaTeX files.  This may be text or a filename."
  :type 'string
  :group 'muse-latex)

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
           (not muse-latex-permit-contents-tag)
           \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n\n"
  "Header used for publishing LaTeX files (CJK).  This may be text or a
filename."
  :type 'string
  :group 'muse-latex)

(defcustom muse-latexcjk-footer
  "\n\\end{CJK*}
\\end{document}\n"
  "Footer used for publishing LaTeX files (CJK).  This may be text or a
filename."
  :type 'string
  :group 'muse-latex)

(defcustom muse-latex-markup-regexps
  `(;; numeric ranges
    (10000 "\\([0-9]+\\)-\\([0-9]+\\)" 0 "\\1--\\2")

    ;; be careful of closing quote pairs
    (10100 "\"'" 0 "\"\\\\-'"))
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
  '((image-with-desc . "\\begin{figure}[h]
\\centering\\includegraphics[width=0.75\\textwidth]{%s.%s}
\\caption{%s}
\\end{figure}")
    (image           . "\\begin{figure}[h]
\\centering\\includegraphics[width=0.75\\textwidth]{%s.%s}
\\end{figure}")
    (image-link      . "%% %s
\\includegraphics[width=0.75\\textwidth]{%s.%s}")
    (anchor-ref      . "\\ref{%s}")
    (url             . "\\url{%s}")
    (url-and-desc    . "\\href{%s}{%s}\\footnote{%1%}")
    (link            . "\\href{%s}{%s}\\footnote{%1%}")
    (link-and-anchor . "\\href{%1%}{%3%}\\footnote{%1%}")
    (email-addr      . "\\verb|%s|")
    (anchor          . "\\label{%s}")
    (emdash          . "---")
    (comment-begin   . "% ")
    (rule            . "\\bigskip")
    (no-break-space  . "~")
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
    (section-other   . "\\paragraph{")
    (section-other-end . "}")
    (footnote        . "\\footnote{")
    (footnote-end    . "}")
    (footnotetext    . "\\footnotetext[%d]{")
    (begin-underline . "\\underline{")
    (end-underline   . "}")
    (begin-literal   . "\\texttt{")
    (end-literal     . "}")
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
    (begin-uli       . "\\begin{itemize}\n")
    (end-uli         . "\n\\end{itemize}")
    (begin-uli-item  . "\\item ")
    (begin-oli       . "\\begin{enumerate}\n")
    (end-oli         . "\n\\end{enumerate}")
    (begin-oli-item  . "\\item ")
    (begin-dl        . "\\begin{description}\n")
    (end-dl          . "\n\\end{description}")
    (begin-ddt       . "\\item[")
    (end-ddt         . "] \\mbox{}\n"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-latex)

(defcustom muse-latexcjk-encoding-map
  '((utf-8              . "{UTF8}{song}")
    (japanese-iso-8bit  . "[dnp]{JIS}{min}")
    (chinese-big5       . "{Bg5}{bsmi}")
    (mule-utf-8         . "{UTF8}{song}")
    (chinese-iso-8bit   . "{GB}{song}")
    (chinese-gbk        . "{GBK}{song}"))
  "An alist mapping emacs coding systems to appropriate CJK codings.
Use the base name of the coding system (ie, without the -unix)."
  :type '(alist :key-type coding-system :value-type string)
  :group 'muse-latex)

(defcustom muse-latexcjk-encoding-default "{GB}{song}"
  "The default Emacs buffer encoding to use in published files.
This will be used if no special characters are found."
  :type 'string
  :group 'muse-latex)

(defun muse-latexcjk-encoding ()
  (when (boundp 'buffer-file-coding-system)
    (muse-latexcjk-transform-content-type buffer-file-coding-system)))

(defun muse-latexcjk-transform-content-type (content-type)
  "Using `muse-cjklatex-encoding-map', try and resolve an emacs coding
system to an associated CJK coding system."
  (let ((match (and (fboundp 'coding-system-base)
                    (assoc (coding-system-base content-type)
                           muse-latexcjk-encoding-map))))
    (if match
        (cdr match)
      muse-latexcjk-encoding-default)))

(defcustom muse-latex-markup-specials-document
  '((?\\ . "\\textbackslash{}")
    (?\_ . "\\textunderscore{}")
    (?\< . "\\textless{}")
    (?\> . "\\textgreater{}")
    (?^  . "\\^{}")
    (?\~ . "\\~{}")
    (?\@ . "\\@")
    (?\$ . "\\$")
    (?\% . "\\%")
    (?\{ . "\\{")
    (?\} . "\\}")
    (?\& . "\\&")
    (?\# . "\\#"))
  "A table of characters which must be represented specially.
These are applied to the entire document, sans already-escaped
regions."
  :type '(alist :key-type character :value-type string)
  :group 'muse-latex)

(defcustom muse-latex-markup-specials-example
  '()
  "A table of characters which must be represented specially.
These are applied to <example> regions.

With the default interpretation of <example> regions, no specials
need to be escaped."
  :type '(alist :key-type character :value-type string)
  :group 'muse-latex)

(defcustom muse-latex-markup-specials-literal
  '((?\n . "\\\n")
    (?_  . "\\textunderscore{}")
    (?\< . "\\textless{}")
    (?\> . "\\textgreater{}")
    (?^  . "\\^{}")
    (?\~ . "\\~{}")
    (?\$ . "\\$")
    (?\% . "\\%")
    (?\{ . "\\{")
    (?\} . "\\}")
    (?\& . "\\&")
    (?\# . "\\#"))
  "A table of characters which must be represented specially.
This applies to =monospaced text= and <code> regions."
  :type '(alist :key-type character :value-type string)
  :group 'muse-latex)

(defcustom muse-latex-markup-specials-url
  '((?\\ . "\\textbackslash{}")
    (?\_ . "\\_")
    (?\< . "\\<")
    (?\> . "\\>")
    (?\$ . "\\$")
    (?\% . "\\%")
    (?\{ . "\\{")
    (?\} . "\\}")
    (?\& . "\\&")
    (?\# . "\\#"))
  "A table of characters which must be represented specially.
These are applied to URLs."
  :type '(alist :key-type character :value-type string)
  :group 'muse-latex)

(defcustom muse-latex-markup-specials-image
  '((?\\ . "\\textbackslash{}")     ; cannot find suitable replacement
    (?\< . "\\<")
    (?\> . "\\>")
    (?\$ . "\\$")
    (?\% . "\\%")
    (?\{ . "\\{")
    (?\} . "\\}")
    (?\& . "\\&")
    (?\# . "\\#")                   ; cannot find suitable replacement
    )
  "A table of characters which must be represented specially.
These are applied to image filenames."
  :type '(alist :key-type character :value-type string)
  :group 'muse-latex)

(defun muse-latex-decide-specials (context)
  "Determine the specials to escape, depending on CONTEXT."
  (cond ((memq context '(underline emphasis document url-desc verbatim))
         muse-latex-markup-specials-document)
        ((eq context 'image)
         muse-latex-markup-specials-image)
        ((memq context '(email url))
         muse-latex-markup-specials-url)
        ((eq context 'literal)
         muse-latex-markup-specials-literal)
        ((eq context 'example)
         muse-latex-markup-specials-example)
        (t (error "Invalid context '%s' in muse-latex" context))))

(defun muse-latex-markup-table ()
  (let* ((table-info (muse-publish-table-fields (match-beginning 0)
                                                (match-end 0)))
         (row-len (car table-info))
         (field-list (cdr table-info)))
    (when table-info
      (muse-insert-markup "\\begin{tabular}{" (make-string row-len ?l) "}\n")
      (dolist (fields field-list)
        (let ((type (car fields)))
          (setq fields (cdr fields))
          (when (= type 3)
            (muse-insert-markup "\\hline\n"))
          (insert (car fields))
          (setq fields (cdr fields))
          (dolist (field fields)
            (muse-insert-markup " & ")
            (insert field))
          (muse-insert-markup " \\\\\n")
          (when (= type 2)
            (muse-insert-markup "\\hline\n"))))
      (muse-insert-markup "\\end{tabular}"))))

(defun muse-latex-fixup-dquotes ()
  "Fixup double quotes."
  (goto-char (point-min))
  (let ((open t))
    (while (search-forward "\"" nil t)
      (unless (get-text-property (match-beginning 0) 'read-only)
        (when (or (bobp)
                  (eq (char-before) ?\n))
          (setq open t))
        (if open
            (progn
              (replace-match "``")
              (setq open nil))
          (replace-match "''")
          (setq open t))))))

(defcustom muse-latex-permit-contents-tag nil
  "If nil, ignore <contents> tags.  Otherwise, insert table of contents.

Most of the time, it is best to have a table of contents on the
first page, with a new page immediately following.  To make this
work with documents published in both HTML and LaTeX, we need to
ignore the <contents> tag.

If you don't agree with this, then set this option to non-nil,
and it will do what you expect."
  :type 'boolean
  :group 'muse-latex)

(defun muse-latex-munge-buffer ()
  (muse-latex-fixup-dquotes)
  (when (and muse-latex-permit-contents-tag
             muse-publish-generate-contents)
    (goto-char (car muse-publish-generate-contents))
    (muse-insert-markup "\\tableofcontents")))

(defun muse-latex-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-latex-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command (format "cd \"%s\"; pdflatex \"%s\""
                             (file-name-directory output-path) file))
            (times 0)
            result)
        ;; XEmacs can sometimes return a non-number result.  We'll err
        ;; on the side of caution by continuing to attempt to generate
        ;; the PDF if this happens and treat the final result as
        ;; successful.
        (while (and (< times 2)
                    (or (not (numberp result))
                        (not (eq result 0))
                        ;; table of contents takes 2 passes
                        (file-readable-p
                         (muse-replace-regexp-in-string
                          "\\.tex\\'" ".toc" file t t))))
          (setq result (shell-command command)
                times (1+ times)))
        (if (or (not (numberp result))
                (eq result 0))
            t
          nil))))
   ".aux" ".toc" ".out" ".log"))

;;; Register the Muse LATEX Publishers

(muse-define-style "latex"
                   :suffix    'muse-latex-extension
                   :regexps   'muse-latex-markup-regexps
                   :functions 'muse-latex-markup-functions
                   :strings   'muse-latex-markup-strings
                   :specials  'muse-latex-decide-specials
                   :before-end 'muse-latex-munge-buffer
                   :header    'muse-latex-header
                   :footer    'muse-latex-footer
                   :browser   'find-file)

(muse-derive-style "pdf" "latex"
                   :final   'muse-latex-pdf-generate
                   :browser 'muse-latex-pdf-browse-file
                   :link-suffix 'muse-latex-pdf-extension
                   :osuffix 'muse-latex-pdf-extension)

(muse-derive-style "latexcjk" "latex"
                   :header    'muse-latexcjk-header
                   :footer    'muse-latexcjk-footer)

(muse-derive-style "pdfcjk" "latexcjk"
                   :final   'muse-latex-pdf-generate
                   :browser 'muse-latex-pdf-browse-file
                   :link-suffix 'muse-latex-pdf-extension
                   :osuffix 'muse-latex-pdf-extension)

(provide 'muse-latex)

;;; muse-latex.el ends here
