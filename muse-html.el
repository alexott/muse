;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse HTML Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-regexps)

(defgroup muse-html nil
  "Options controlling the behaviour of Muse HTML publishing.
See `muse-html' for more information."
  :group 'muse-publish)

(defcustom muse-html-extension ".html"
  "Default file extension for publishing HTML files."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-style-sheet
  "body {
  background: white; color: black;
  margin-left: 3%; margin-right: 7%;
}

p { margin-top: 1% }
p.verse { margin-left: 3% }

.example { margin-left: 3% }

h2 {
  margin-top: 25px;
  margin-bottom: 0px;
}
h3 { margin-bottom: 0px; }"
  "Text to prepend to a Muse mail message being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-header
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">
<html>
  <head>
    <title><lisp>
  (concat (muse-publishing-directive \"title\")
	  (let ((author (muse-publishing-directive \"author\")))
	    (if (not (string= author (user-full-name)))
		(concat \" (by \" author \")\"))))</lisp></title>
    <meta name=\"generator\" content=\"muse.el\">
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
	  content=\"<lisp>muse-html-meta-content-type</lisp>\">
    <lisp>
      (let ((maintainer (muse-style-element :maintainer)))
	(when maintainer
	  (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\">\")))
    </lisp>
    <style type=\"text/css\">
      <lisp>muse-html-style-sheet</lisp>
    </style>
  </head>
  <body>
    <h1><lisp>
  (concat (muse-publishing-directive \"title\")
	  (let ((author (muse-publishing-directive \"author\")))
	    (if (not (string= author (user-full-name)))
		(concat \" (by \" author \")\"))))</lisp></h1>
    <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing HTML files."
  :type '(choice string file)
  :group 'muse-html)

(defcustom muse-html-footer "
<!-- Page published by Emacs Muse ends here -->
  </body>
</html>\n"
  "Footer used for publishing HTML files."
  :type '(choice string file)
  :group 'muse-html)

(defcustom muse-html-anchor-on-word nil
  "When true, anchors surround the closest word. This allows you
to select them in a browser (ie, for pasting), but has the
side-effect of marking up headers in multiple colours if your
header style is different from your link style."
  :type 'boolean
  :group 'muse-html)

(defcustom muse-html-table-attributes
  "class=\"muse-table\" border=\"2\" cellpadding=\"5\""
  "The attribute to be used with HTML <table> tags.
Note that since Muse supports direct insertion of HTML tags, you
can easily create any kind of table you want, as long as each
line begins at column 0 (to prevent it from being blockquote'd).
To make such a table, use this idiom:

  <verbatim>
  <table>
    [... contents of my table, in raw HTML ...]
  </verbatim></table>

It may look strange to have the tags out of sequence, but this is
because the Muse verbatim tag is handled during a different pass
than the HTML table tag."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-markup-regexps
  `(;; join together the parts of a list or table
    (10000 "</\\([oud]l\\)>\\s-*<\\1>\\s-*" 0 "")
    (10100 ,(concat "  </t\\(body\\|head\\|foot\\)>\\s-*</table>\\s-*"
		    "<table[^>]*>\\s-*<t\\1>\n") 0 "")
    (10200 "</table>\\s-*<table[^>]*>\n" 0 "")

    ;; the beginning of the buffer begins the first paragraph
    (10300 "\\`\n*\\([^<-]\\|<\\(em\\|strong\\|code\\)>\\|<a \\)" 0
	   "<p class=\"first\">\\1")
    ;; plain paragraph separator
    (10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?\n"
		    "\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-html-markup-paragraph)
    (10500 ,(concat "\\([^>"
                    muse-regexp-space
                    "]\\)\\s-*\\'")
           0 "\\1</p>\n"))
  "List of markup rules for publishing a Muse page to HTML.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
		  (list :tag "Markup rule"
			integer
			(choice regexp symbol)
			integer
			(choice string function symbol))
		  function))
  :group 'muse-html)

(defcustom muse-html-markup-functions
  '((anchor   . muse-html-markup-anchor)
    (table    . muse-html-markup-table)
    (footnote . muse-html-markup-footnote))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-html)

(defcustom muse-html-markup-strings
  '((image-with-desc . "<img src=\"%s\" alt=\"%s\">")
    (image-link      . "<img src=\"%s\">")
    (url-with-image  . "<a class=\"image-link\" href=\"%s\"><img src=\"%s\"></a>")
    (url-link        . "<a href=\"%s\">%s</a>")
    (email-addr      . "<a href=\"mailto:%s\">%s</a>")
    (emdash          . " &mdash; ")
    (rule            . "<hr>")
    (fn-sep          . "<hr>\n")
    (enddots         . "....")
    (dots            . "...")
    (section         . "<h2>")
    (section-end     . "</h2>")
    (subsection      . "<h3>")
    (subsection-end  . "</h3>")
    (subsubsection   . "<h4>")
    (subsubsection-end . "</h4>")
    (begin-underline . "<u>")
    (end-underline   . "</u>")
    (begin-literal   . "<code>")
    (end-literal     . "</code>")
    (begin-emph      . "<em>")
    (end-emph        . "</em>")
    (begin-more-emph . "<strong>")
    (end-more-emph   . "</strong>")
    (begin-most-emph . "<strong><em>")
    (end-most-emph   . "</em></strong>")
    (begin-verse     . "<p class=\"verse\">\n")
    (verse-space     . "&nbsp;&nbsp;")
    (end-verse-line  . "<br>")
    (last-stanza-end . "<br>")
    (empty-verse-line . "<br>")
    (end-verse       . "</p>")
    (begin-example   . "<pre class=\"example\">")
    (end-example     . "</pre>")
    (begin-center    . "<center>\n")
    (end-center      . "\n</center>")
    (begin-quote     . "<blockquote>\n")
    (end-quote       . "\n</blockquote>")
    (begin-uli       . "<ul>\n<li>")
    (end-uli         . "</li>\n</ul>")
    (begin-oli       . "<ol>\n<li>")
    (end-oli         . "</li>\n</ol>")
    (begin-ddt       . "<dl>\n<dt><strong>")
    (start-dde       . "</strong></dt>\n<dd>")
    (end-ddt         . "</dd>\n</dl>"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-html)

(defcustom muse-html-markup-tags
  '(("class" t t muse-html-class-tag))
  "A list of tag specifications, for specially marking up HTML."
  :type '(repeat (list (string :tag "Markup tag")
		       (boolean :tag "Expect closing tag" :value t)
		       (boolean :tag "Parse attributes" :value nil)
		       function))
  :group 'muse-html)

(defcustom muse-html-markup-specials
  '((?\" . "&quot;")
    (?\< . "&lt;")
    (?\> . "&gt;")
    (?\& . "&amp;"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-html)

(defcustom muse-html-meta-http-equiv "Content-Type"
  "The http-equiv attribute used for the HTML <meta> tag."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-meta-content-type "text/html"
  "The content type used for the HTML <meta> tag."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-meta-content-encoding (if (featurep 'mule)
					      'detect
					    "iso-8859-1")
  "If set to the symbol 'detect, use `muse-coding-map' to try
  and determine the HTML charset from emacs's coding. If set to a string, this
  string will be used to force a particular charset"
  :type '(choice string symbol)
  :group 'muse-html)

(defcustom muse-html-charset-default "iso-8859-1"
  "The default HTML meta charset to use if no translation is found in
  `muse-coding-map'"
  :type 'string
  :group 'muse-html)

(defcustom muse-html-encoding-default 'iso-8859-1
  "The default emacs coding  use if no special characters are found"
  :type 'symbol
  :group 'muse-html)

(defcustom muse-html-encoding-map
  '((iso-2022-jp	. "iso-2022-jp")
    (utf-8		. "utf-8")
    (japanese-iso-8bit	. "euc-jp")
    (chinese-big5	. "big5")
    (mule-utf-8         . "utf-8")
    (chinese-iso-8bit   . "gb2312")
    (chinese-gbk        . "gbk"))
  "An alist mapping emacs coding systems to appropriate HTML charsets.
  Use the base name of the coding system (ie, without the -unix)"
  :type '(alist :key-type coding-system :value-type string)
  :group 'muse-html)

(defun muse-html-transform-content-type (content-type)
  "Using `muse-html-encoding-map', try and resolve an emacs coding
system to an associated HTML coding system. If no match is found,
`muse-html-charset-default' is used instead."
  (let ((match (assoc (coding-system-base content-type)
		      muse-html-encoding-map)))
    (if match
	(cdr match)
      muse-html-charset-default)))

(defun muse-html-insert-anchor (anchor)
  "Insert an anchor, either around the word at point, or within a tag."
  (skip-chars-forward muse-regexp-space)
  (if (looking-at "<\\([^ />]+\\)>")
      (let ((tag (match-string 1)))
	(goto-char (match-end 0))
	(insert "<a name=\"" anchor "\" id=\"" anchor "\">")
	(when muse-html-anchor-on-word
	  (or (and (search-forward (format "</%s>" tag)
				   (line-end-position) t)
		   (goto-char (match-beginning 0)))
	      (forward-word 1)))
	(insert "</a>"))
    (insert "<a name=\"" anchor "\" id=\"" anchor "\">")
    (when muse-html-anchor-on-word
      (forward-word 1))
    (insert "</a>")))

(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit)
    (save-excursion
      (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t))))

(defun muse-html-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (match-beginning 0))
    (unless (eq (char-before) ?\>) (insert "</p>"))
    (goto-char end)
    (unless (and (eq (char-after) ?\<)
		 (not (or (looking-at "<\\(em\\|strong\\|code\\)>")
			  (and (looking-at "<a ")
			       (not (looking-at "<a[^>]+><img"))))))
      (cond
       ((looking-back "\\(</h[1-4]>\\|<hr>\\)\n\n")
	(insert "<p class=\"first\">"))
       ((looking-back "<\\(blockquote\\|center\\)>\n")
	(insert "<p class=\"quoted\">"))
       (t
	(insert "<p>"))))))

(defun muse-html-markup-anchor ()
  (save-match-data
    (muse-html-insert-anchor (match-string 1))) "")

(defun muse-html-escape-string (str)
  "Convert to character entities any non-alphanumeric characters
outside a few punctuation symbols, that risk being misinterpreted
if not escaped."
  (when str
    (let (pos code len)
      (save-match-data
	(while (setq pos (string-match (concat "[^-"
                                               muse-regexp-alnum
                                               "/:._=@\\?~#]")
				       str pos))
	  (setq code (int-to-string (aref str pos))
		len (length code)
		str (replace-match (concat "&#" code ";") nil nil str)
		pos (+ 3 len pos)))
	str))))

(defun muse-html-markup-footnote ()
  (if (/= (line-beginning-position) (match-beginning 0))
      "<sup><a name=\"fnr.\\1\" href=\"#fn.\\1\">\\1</a></sup>"
    (prog1
	"<p class=\"footnote\"><a name=\"fn.\\1\" href=\"#fnr.\\1\">\\1.</a>"
      (save-excursion
	(save-match-data
	  (let* ((beg (goto-char (match-end 0)))
		 (end (and (search-forward "\n\n" nil t)
			   (prog1
			       (copy-marker (match-beginning 0))
			     (goto-char beg)))))
	    (while (re-search-forward (concat "^["
                                              muse-regexp-blank
                                              "]+\\([^\n]\\)")
                                      end t)
	      (replace-match "\\1" t))))))))

(defun muse-html-markup-table ()
  (let* ((str (prog1
		  (match-string 1)
		(delete-region (match-beginning 0) (match-end 0))))
	 (fields (split-string str "\\s-*|+\\s-*"))
	 (type (and (string-match "\\s-*\\(|+\\)\\s-*" str)
		    (length (match-string 1 str))))
	 (part (cond ((= type 1) "tbody")
		     ((= type 2) "thead")
		     ((= type 3) "tfoot")))
	 (col (cond ((= type 1) "td")
		    ((= type 2) "th")
		    ((= type 3) "td")))
	 field)
    (insert "<table " muse-html-table-attributes ">\n"
	    "  <" part ">\n"
	    "    <tr>\n")
    (dolist (field fields)
      (insert "      <" col ">" field "</" col ">\n"))
    (insert "    </tr>\n"
	    "  </" part ">\n"
	    "</table>\n")))

;; Handling of tags for HTML

(defun muse-html-insert-contents (depth)
  (let ((max-depth (or depth 2))
	(index 1)
	base contents l)
    (save-excursion
      (goto-char (point-min))
      (search-forward "Page published by Emacs Muse begins here" nil t)
      (catch 'done
	(while (re-search-forward "^<h\\([0-9]+\\)>\\(.+?\\)</h\\1>" nil t)
	  (unless (get-text-property (point) 'read-only)
	    (setq l (1- (string-to-int (match-string 1))))
	    (if (null base)
		(setq base l)
	      (if (< l base)
		  (throw 'done t)))
	    (when (<= l max-depth)
	      (setq contents (cons (cons l (match-string-no-properties 2))
				   contents))
	      (goto-char (match-beginning 2))
	      (muse-html-insert-anchor (concat "sec" (int-to-string index)))
	      (setq index (1+ index)))))))
    (setq index 1 contents (reverse contents))
    (let ((depth 1) (sub-open 0) (p (point)))
      (insert "<dl class=\"contents\">\n")
      (while contents
	(insert "<dt class=\"contents\">\n")
	(insert "<a href=\"#sec" (int-to-string index) "\">"
		(muse-publish-strip-tags (cdar contents))
		"</a>\n")
	(setq index (1+ index))
	(insert "</dt>\n")
	(setq depth (caar contents)
	      contents (cdr contents))
	(if contents
	    (cond
	     ((< (caar contents) depth)
	      (let ((idx (caar contents)))
		(while (< idx depth)
		  (insert "</dl>\n</dd>\n")
		  (setq sub-open (1- sub-open)
			idx (1+ idx)))))
	     ((> (caar contents) depth)	; can't jump more than one ahead
	      (insert "<dd>\n<dl class=\"contents\">\n")
	      (setq sub-open (1+ sub-open))))))
      (while (> sub-open 0)
	(insert "</dl>\n</dd>\n")
	(setq sub-open (1- sub-open)))
      (insert "</dl>\n")
      (muse-publish-mark-read-only p (point)))))

(defun muse-html-class-tag (beg end attrs)
  (goto-char beg)
  (insert "<span class=\"" (cdr (assoc "name" attrs)) "\">")
  (goto-char end)
  (insert "</span>"))

;; Register the Muse HTML Publisher

(defun muse-html-browse-file (file)
  (browse-url (concat "file:" file)))

(defun muse-html-encoding ()
  (if (stringp muse-html-meta-content-encoding)
      muse-html-meta-content-encoding
    (muse-html-transform-content-type
     (or buffer-file-coding-system
	 muse-html-encoding-default))))

(defun muse-html-prepare-buffer ()
  (set (make-local-variable 'muse-publish-url-transforms)
       (cons 'muse-html-escape-string muse-publish-url-transforms))
  (make-local-variable 'muse-html-meta-http-equiv)
  (set (make-local-variable 'muse-html-meta-content-type)
       (concat muse-html-meta-content-type "; charset="
	       (muse-html-encoding))))

(defun muse-html-finalize-buffer ()
  (when muse-publish-generate-contents
    (goto-char (car muse-publish-generate-contents))
    (muse-html-insert-contents (cdr muse-publish-generate-contents)))
  (when (memq buffer-file-coding-system '(no-conversion undecided-unix))
    ;; make it agree with the default charset
    (setq buffer-file-coding-system muse-html-encoding-default)))

(unless (assoc "html" muse-publishing-styles)
  (muse-define-style "html"
		     :suffix    'muse-html-extension
		     :regexps   'muse-html-markup-regexps
		     :functions 'muse-html-markup-functions
		     :strings   'muse-html-markup-strings
		     :tags      'muse-html-markup-tags
		     :specials  'muse-html-markup-specials
		     :before    'muse-html-prepare-buffer
		     :after     'muse-html-finalize-buffer
		     :header    'muse-html-header
		     :footer    'muse-html-footer
		     :browser   'muse-html-browse-file))

(provide 'muse-html)

;;; muse-html.el ends here
