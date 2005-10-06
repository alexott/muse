;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Publishing Using groff -mom -mwww
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-groff nil
  "Rules for marking up a Muse file with groff -mom -mwww macros."
  :group 'muse-publish)

(defcustom muse-groff-extension ".groff"
  "Default file extension for publishing groff -mom -mwww files."
  :type 'string
  :group 'muse-groff)

(defcustom muse-groff-pdf-extension ".pdf"
  "Default file extension for publishing groff -mom -mwww files to PDF."
  :type 'string
  :group 'muse-groff)

(defcustom muse-groff-header
  ".TITLE \"<lisp>(muse-publishing-directive \"title\")</lisp>\"
.SUBTITLE \"<lisp>(muse-publishing-directive \"date\")</lisp>\"
.AUTHOR \"<lisp>(muse-publishing-directive \"author\")</lisp>\"
.PRINTSTYLE TYPESET
.de para
'PP
'ne 2
..
.blm para
.START
<lisp>(and muse-publish-generate-contents \".TOC\n\")</lisp>\n"
  "Header used for publishing groff -mom -mwww files."
  :type '(choice string file)
  :group 'muse-groff)

(defcustom muse-groff-footer " "
  "Footer used for publishing groff -mom -mwww files."
  :type '(choice string file)
  :group 'muse-groff)

(defcustom muse-groff-markup-regexps
  `()
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
		  (list :tag "Markup rule"
			integer
			(choice regexp symbol)
			integer
			(choice string function symbol))
		  function))
  :group 'muse-groff)

(defcustom muse-groff-markup-functions
  '((table . muse-groff-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-groff)

(defcustom muse-groff-markup-strings
  '((image-with-desc . "\n.MPIMG -R %s\n")
    (image-link      . "\n.MPIMG -R %s\n")
    (url-with-image  . "\n.\\\" %s\n.MPIMG -R %s")
    (url-link        . "\n.URL %s %s\n")
    (email-addr      . "\f[C]%s\f[]")
    (emdash          . "\\(em")
    (rule            . "\n.RULE\n")
    (enddots         . "....")
    (dots            . "...")
;;     (part            . "\\part{")
;;     (part-end        . "}")
;;     (chapter         . "\\chapter{")
;;     (chapter-end     . "}")
    (section         . ".HEAD \"")
    (section-end     . "\"")
    (subsection      . ".SUBHEAD \"")
    (subsection-end  . "\"")
    (subsubsection   . ".PARAHEAD \"")
    (subsubsection-end . "\"")
;;     (footnote        . "\\c\n.FOOTNOTE\n")
;;     (footnote-end    . "\n.FOOTNOTE OFF\n")
;;     (footnotemark    . "\\footnotemark[%d]")
;;     (footnotetext    . "\\footnotetext[%d]{")
;;     (footnotetext-end . "}")
    (begin-underline . "\n.UNDERSCORE \"")
    (end-underline   . "\"\n")
    (begin-literal   . "\\fC")
    (end-literal     . "\\fP")
    (begin-emph      . "\\fI")
    (end-emph        . "\\fP")
    (begin-more-emph . "\\fB")
    (end-more-emph   . "\\fP")
    (begin-most-emph . "\\f(BI")
    (end-most-emph   . "\\fP")
    (begin-verse     . ".QUOTE")
    (end-verse-line  . "")
    (last-stanza-end . "")
    (end-verse       . ".QUOTE OFF")
    (begin-example   . (concat
                        ".QUOTE_FONT CR\n.QUOTE_INDENT 1\n.QUOTE_SIZE -2\n"
                        ".UNDERLINE_QUOTES OFF\n.QUOTE"))
    (end-example     . ".QUOTE OFF")
    (begin-center    . "\n.CENTER\n")
    (end-center      . "\n.QUAD L\n")
    (begin-quote     . ".BLOCKQUOTE")
    (end-quote       . ".BLOCKQUOTE OFF")
    (begin-uli       . ".LIST BULLET\n.ITEM\n")
    (end-uli         . "\n.LIST OFF")
    (begin-oli       . ".LIST DIGIT\n.ITEM\n")
    (end-oli         . "\n.LIST OFF")
    (begin-ddt       . "\\fB")
    (start-dde       . "\\fP\n.IR 4P\n")
    (end-ddt         . ".IRX CLEAR"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-groff)

(defcustom muse-groff-markup-specials
  '((?\\ . "\\e"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-groff)

;; (defun muse-latex-markup-table ()
;;   (let* ((str (prog1
;; 		  (match-string 1)
;; 		(delete-region (match-beginning 0) (match-end 0))))
;; 	 (fields (split-string str "\\s-*|+\\s-*"))
;; 	 (type (and (string-match "\\s-*\\(|+\\)\\s-*" str)
;; 		    (length (match-string 1 str)))))
;;     (insert "\\begin{tabular}{" (make-string (length fields) ?l) "}\n")
;;     (insert (mapconcat 'identity fields " & "))
;;     (insert " \\\\\n\\end{tabular}")))

(defun muse-groff-protect-leading-chars ()
  "Protect leading periods and apostrophes from being interpreted as
command characters."
  (while (re-search-forward "^[.']" nil t)
    (replace-match "\\\\&\\&" t)))

(defun muse-groff-concat-lists ()
  "Join like lists."
  (let (arg begin)
    (while (re-search-forward "^\.LIST[ \t]+\\(.*\\)\n" nil t)
      (setq arg (match-string 1))
      (if (string= arg "OFF")
          (setq begin (match-beginning 0))
        (if (and begin (string= type arg))
            (delete-region begin (match-end 0))
          (setq type arg
                begin 0))))))

(defun muse-groff-fixup-dquotes ()
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

(defun muse-groff-prepare-buffer ()
  (goto-char (point-min))
  (muse-groff-protect-leading-chars))

(defun muse-groff-finalize-buffer ()
  (goto-char (point-min))
  (muse-groff-concat-lists))

(defun muse-groff-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-groff-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command
             (format
              (concat "file=%s; ext=%s; cd %s && cp $file$ext $file.ref && "
                      "groff -mom -mwww -t $file$ext > $file.ps && "
                      "ps2pdf12 $file.ps")
              (file-name-sans-extension file)
              muse-groff-extension
              (file-name-directory output-path))))
	(shell-command command))))
   ".ps"))

(unless (assoc "groff" muse-publishing-styles)
  (muse-define-style "groff"
		     :suffix    'muse-groff-extension
		     :regexps   'muse-groff-markup-regexps
		     :functions 'muse-groff-markup-functions
		     :strings   'muse-groff-markup-strings
		     :specials  'muse-groff-markup-specials
		     :before    'muse-groff-prepare-buffer
		     :after     'muse-groff-finalize-buffer
		     :header    'muse-groff-header
		     :footer    'muse-groff-footer
		     :browser   'find-file)

  (muse-derive-style "groff-pdf" "groff"
		     :final   'muse-groff-pdf-generate
		     :browser 'muse-groff-pdf-browse-file
		     :osuffix 'muse-groff-pdf-extension))

(provide 'muse-groff)

;;; muse-groff.el ends here
