;;; emacs-wiki-blosxom.el --- Publish a wiki tree for serving by Blosxom

;; Copyright 2004 Gary V. Vaughan (gary AT gnu DOT org)
;; Copyright 2004 Brad Collins (brad AT chenla DOT org)

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-blosxom.el
;; Version: 0.0.1
;; Date: Wed, 24 March 2004
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Publish a local Emacs Wiki tree for serving by Blosxom
;; URL: http://tkd.kicks-ass.net/arch/gary@gnu.org--2004/emacs-wiki--gary--1.0
;; Compatibility: Emacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; I maintain the hypertext parts of my website with John Wiegley's
;; emacs-wiki.el, now maintained by Michael Olson at
;; http://www.mwolson.org/projects/EmacsWiki.html.  You will need to
;; install a copy of that file before this one is of any use to you.
;;
;; Blosxom wiki publishes a tree of categorised Wiki files to a mirrored
;; tree of blosxom stories to be served by blosxom.cgi.
;;
;; Each Blosxom Wiki file must include `#date yyyy-mm-dd', or optionally
;; the longer `#date yyyy-mm-dd hh:mm', plus whatever normal emacs wiki
;; content is desired.
;;
;; If you want to change `blosxom-directory' and some other variables,
;; either use Customize or use `blosxom-option-customized'.  For
;; example:
;;
;;    (blosxom-option-customized 'blosxom-directory "~/Blosxom")
;;    (blosxom-option-customized 'blosxom-publishing-directory
;;                               "~/public_html/blog")
;;
;; and if you want to modify other emacs-wiki variables for the blosxom
;; project:
;;
;;    (add-to-list 'blosxom-custom-variables
;;                 '(some-emacs-wiki-variable . "some-blosxom-wiki-value"))
;;    (blosxom-option-customized 'blosxom-custom-variables
;;                               blosxom-custom-variables)
;;
;; See `emacs-wiki-update-project' and `blosxom-custom-variables' for more
;; details.

;;; Contributors:

;; Brad Collins (brad AT chenla DOT org) ported this file (originally
;; called `emacs-wiki-bloxsom.el' to Muse.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Blosxom Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-blosxom nil
  "Options controlling the behaviour of Muse BLOSXOM publishing.
See `muse-blosxom' for more information."
  :group 'muse-publish)

(defcustom muse-blosxom-extension ".txt"
  "Default file extension for publishing BLOSXOM files."
  :type 'string
  :group 'muse-blosxom)

(defcustom muse-blosxom-header
  "<lisp>(muse-publishing-directive \"title\")</lisp>\n"
  "Header used for publishing BLOSXOM files."
  :type '(choice string file)
  :group 'muse-blosxom)

(defcustom muse-blosxom-footer "\n"
  "Footer used for publishing BLOSXOM files."
  :type '(choice string file)
  :group 'muse-blosxom)

(defcustom muse-blosxom-anchor-on-word nil
  "When true, anchors surround the closest word. This allows you
to select them in a browser (ie, for pasting), but has the
side-effect of marking up headers in multiple colours if your
header style is different from your link style."
  :type 'boolean
  :group 'muse-blosxom)

(defcustom muse-blosxom-table-attributes
  "class=\"muse-table\" border=\"2\" cellpadding=\"5\""
  "The attribute to be used with BLOSXOM <table> tags.
Note that since Muse supports direct insertion of BLOSXOM tags, you
can easily create any kind of table you want, as long as each
line begins at column 0 (to prevent it from being blockquote'd).
To make such a table, use this idiom:

  <verbatim>
  <table>
    [... contents of my table, in raw BLOSXOM ...]
  </verbatim></table>

It may look strange to have the tags out of sequence, but this is
because the Muse verbatim tag is handled during a different pass
than the BLOSXOM table tag."
  :type 'string
  :group 'muse-blosxom)

(defcustom muse-blosxom-markup-regexps
  `(

   ;;(emacs-wiki-tag-regexp 0 muse-markup-custom-tags)

    ;; join together the parts of a list or table
    (10000 "</\\([oud]l\\)>\\s-*<\\1>\\s-*" 0 "")
    (10100 ,(concat "  </t\\(body\\|head\\|foot\\)>\\s-*</table>\\s-*"
		    "<table[^>]*>\\s-*<t\\1>\n") 0 "")
    (10200 "</table>\\s-*<table[^>]*>\n" 0 "")

    ;; the beginning of the buffer begins the first paragraph
    (10300 "\\`\n*\\([^<]\\|<\\(em\\|strong\\|code\\)>\\|<a \\)" 0
	   "<p class=\"first\">\\1")
    ;; plain paragraph separator
    (10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?\n"
		    "\\([ \t]*\n\\)+\\(<\\(blockquote\\|center\\)>\n\\)?") 0
     muse-blosxom-markup-paragraph)
    (10500 "\\([^> \n\t]\\)\\s-*\\'" 0 "\\1</p>\n")
    (10600 "^#\\([A-C]\\)\\([0-9]*\\)\\s-*\\([_oX>CP]\\)\\s-*\\(.+\\)"
	   0 planner-markup-task)
    (10700 "^\\.#\\([0-9]+\\)" 0 planner-markup-note)
    (10800 "^#\\(date\\)\\s-+\\(.+\\)\n+" 0 muse-blosxom-markup-date-directive))
  "List of markup rules for publishing a Muse page to BLOSXOM.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
		  (list :tag "Markup rule"
			(choice regexp symbol)
			integer
			(choice string function symbol))
		  function))
  :group 'muse-blosxom)

(defcustom muse-blosxom-markup-functions
  '((anchor   . muse-blosxom-markup-anchor)
    (table    . muse-blosxom-markup-table)
    (footnote . muse-blosxom-markup-footnote))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-blosxom)

(defcustom muse-blosxom-markup-strings
  '((image-with-desc . "<img src=\"%s\" alt=\"%s\">")
    (image-link      . "<img src=\"%s\">")
    (url-with-image  . "<a href=\"%s\"><img src=\"%s\"></a>")
    (url-link        . "<a href=\"%s\">%s</a>")
    (email-addr      . "<a href=\"mailto:%s\">%s</a>")
    (emdash          . " &#151; ")
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
  :group 'muse-blosxom)

(defcustom muse-blosxom-markup-specials
  '((?\" . "&quot;")
    (?\< . "&lt;")
    (?\> . "&gt;")
    (?\& . "&amp;"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-blosxom)

(defcustom muse-blosxom-meta-http-equiv "Content-Type"
  "The http-equiv attribute used for the BLOSXOM <meta> tag."
  :type 'string
  :group 'muse-blosxom)

(defcustom muse-blosxom-meta-content-type "text/blosxom"
  "The content type used for the BLOSXOM <meta> tag."
  :type 'string
  :group 'muse-blosxom)

(defcustom muse-blosxom-meta-content-encoding (if (featurep 'mule)
					      'detect
					    "iso-8859-1")
  "If set to the symbol 'detect, use `muse-coding-map' to try
  and determine the BLOSXOM charset from emacs's coding. If set to a string, this
  string will be used to force a particular charset"
  :type '(choice string symbol)
  :group 'muse-blosxom)

(defcustom muse-blosxom-charset-default "iso-8859-1"
  "The default BLOSXOM meta charset to use if no translation is found in
  `muse-coding-map'"
  :type 'string
  :group 'muse-blosxom)

(defcustom muse-blosxom-encoding-default 'iso-8859-1
  "The default emacs coding  use if no special characters are found"
  :type 'symbol
  :group 'muse-blosxom)

(defcustom muse-blosxom-encoding-map
  '((iso-2022-jp	. "iso-2022-jp")
    (utf-8		. "utf-8")
    (japanese-iso-8bit	. "euc-jp")
    (chinese-big5	. "big5"))
  "An alist mapping emacs coding systems to appropriate BLOSXOM charsets.
  Use the base name of the coding system (ie, without the -unix)"
  :type '(alist :key-type coding-system :value-type string)
  :group 'muse-blosxom)

(defun muse-blosxom-transform-content-type (content-type)
  "Using `muse-blosxom-encoding-map', try and resolve an emacs coding
system to an associated BLOSXOM coding system. If no match is found,
`muse-blosxom-charset-default' is used instead."
  (let ((match (assoc (coding-system-base content-type)
		      muse-blosxom-encoding-map)))
    (if match
	(cadr match)
      muse-blosxom-charset-default)))

(defun muse-blosxom-insert-anchor (anchor)
  "Insert an anchor, either around the word at point, or within a tag."
  (skip-chars-forward " \t\n")
  (if (looking-at "<\\([^ />]+\\)>")
      (let ((tag (match-string 1)))
	(goto-char (match-end 0))
	(insert "<a name=\"" anchor "\" id=\"" anchor "\">")
	(when muse-blosxom-anchor-on-word
	  (or (and (search-forward (format "</%s>" tag)
				   (line-end-position) t)
		   (goto-char (match-beginning 0)))
	      (forward-word 1)))
	(insert "</a>"))
    (insert "<a name=\"" anchor "\" id=\"" anchor "\">")
    (when muse-blosxom-anchor-on-word
      (forward-word 1))
    (insert "</a>")))

(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit)
    (save-excursion
      (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t))))

(defun muse-blosxom-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (match-beginning 0))
    (unless (eq (char-before) ?\>) (insert "</p>"))
    (goto-char end)
    (unless (and (eq (char-after) ?\<)
		 (not (or (looking-at "<\\(em\\|strong\\|code\\)>")
			  (looking-at "<a "))))
      (cond
       ((looking-back "\\(</h[1-4]>\\|<hr>\\)\n\n")
	(insert "<p class=\"first\">"))
       ((looking-back "<\\(blockquote\\|center\\)>\n")
	(insert "<p class=\"quoted\">"))
       (t
	(insert "<p>"))))))

(defun muse-blosxom-markup-anchor ()
  (save-match-data
    (muse-blosxom-insert-anchor (match-string 1))) "")

(defun muse-blosxom-escape-string (str)
  "Convert to character entities any non-alphanumeric characters
outside a few punctuation symbols, that risk being misinterpreted
if not escaped."
  (when str
    (let (pos code len)
      (save-match-data
	(while (setq pos (string-match "[^-[:alnum:]/:._=@\\?~#]"
				       str pos))
	  (setq code (int-to-string (aref str pos))
		len (length code)
		str (replace-match (concat "&#" code ";") nil nil str)
		pos (+ 3 len pos)))
	str))))

(defun muse-blosxom-markup-footnote ()
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
	    (while (re-search-forward "^[ \t]+\\([^\n]\\)" end t)
	      (replace-match "\\1" t))))))))

(defun muse-blosxom-markup-table ()
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
    (insert "<table " muse-blosxom-table-attributes ">\n"
	    "  <" part ">\n"
	    "    <tr>\n")
    (dolist (field fields)
      (insert "      <" col ">" field "</" col ">\n"))
    (insert "    </tr>\n"
	    "  </" part ">\n"
	    "</table>\n")))

;; Handling of tags for BLOSXOM

(defun muse-blosxom-insert-contents (depth)
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
	      (muse-blosxom-insert-anchor (concat "sec" (int-to-string index)))
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

;; Register the Muse BLOSXOM Publisher

(defun muse-blosxom-browse-file (file)
  (browse-url (concat "file:" file)))

(defun muse-blosxom-encoding ()
  (if (stringp muse-blosxom-meta-content-encoding)
      muse-blosxom-meta-content-encoding
    (muse-blosxom-transform-content-type
     (or buffer-file-coding-system
	 muse-blosxom-encoding-default))))

(defun muse-blosxom-prepare-buffer ()
  (set (make-local-variable 'muse-publish-url-transforms)
       (cons 'muse-blosxom-escape-string muse-publish-url-transforms))
  (make-local-variable 'muse-blosxom-meta-http-equiv)
  (set (make-local-variable 'muse-blosxom-meta-content-type)
       (concat muse-blosxom-meta-content-type "; charset="
	       (muse-blosxom-encoding))))

(defun muse-blosxom-finalize-buffer ()
  (when muse-publish-generate-contents
    (goto-char (car muse-publish-generate-contents))
    (muse-blosxom-insert-contents (cdr muse-publish-generate-contents)))
  (when (memq buffer-file-coding-system '(no-conversion undecided-unix))
    ;; make it agree with the default charset
    (setq buffer-file-coding-system muse-blosxom-encoding-default)))

(unless (assoc "blosxom" muse-publishing-styles)
  (muse-define-style "blosxom"
		     :suffix    'muse-blosxom-extension
		     :regexps   'muse-blosxom-markup-regexps
		     :functions 'muse-blosxom-markup-functions
		     :strings   'muse-blosxom-markup-strings
		     :specials  'muse-blosxom-markup-specials
		     :before    'muse-blosxom-prepare-buffer
		     :after     'muse-blosxom-finalize-buffer
		     :header    'muse-blosxom-header
		     :footer    'muse-blosxom-footer
		     :browser   'muse-blosxom-browse-file))

;;; Mode

;;; Maintain (published-file . date) alist

(defvar blosxom-page-date-alist nil)

(defun muse-blosxom-markup-date-directive ()
  "Add a date entry to `blosxom-page-date-alist' for this page."
  (when (string= (match-string 1) "date")
    (let ((date (match-string 2)))
      (save-match-data
	(add-to-list
	 'blosxom-page-date-alist
	 `(,(muse-published-file) . ,date)))))
  "")

(defun blosxom-set-time (file)
  "Reset the modification timestamp for published FILE.
Blosxom uses the modification time of a published file as its publication
date-time.  Adding this function to `emacs-wiki-after-file-publish-hook'
will set the modification time of the published page according to the value
stored in `blosxom-page-date-alist'."
  (let* ((page (,use-page-name file))
	 (published (muse-published-file page))
	 (date (cdr (assoc published blosxom-page-date-alist))))
    (when date
      (shell-command
       (format "touch --date='%s' %s" date published)))))

(provide 'muse-blosxom)

;;; muse-blosxom.el ends here
