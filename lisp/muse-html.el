;;; muse-html.el --- Publish to HTML and XHTML.

;; Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:

;; Zhiqiang Ye (yezq AT mail DOT cbi DOT pku DOT edu DOT cn) suggested
;; appending an 'encoding="..."' fragment to the first line of the
;; sample publishing header so that when editing the resulting XHTML
;; file, Emacs would use the proper encoding.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse HTML Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-regexps)

(defgroup muse-html nil
  "Options controlling the behavior of Muse HTML publishing.
See `muse-html' for more information."
  :group 'muse-publish)

(defcustom muse-html-extension ".html"
  "Default file extension for publishing HTML files."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-style-sheet
  "<style type=\"text/css\">
body {
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
h3 { margin-bottom: 0px; }
    </style>"
  "Store your stylesheet definitions here.
This is used in `muse-html-header'.
You can put raw CSS in here or a <link> tag to an external stylesheet.
This text may contain <lisp> markup tags.

An example of using <link> is as follows.

<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/default.css\">

If you are using XHTML, make sure to close the tag properly, as
shown in the following example.

<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/default.css\" />"
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
    <lisp>muse-html-style-sheet</lisp>
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

(defcustom muse-xhtml-header
  "<?xml version=\"1.0\" encoding=\"<lisp>
  (muse-html-encoding)</lisp>\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title><lisp>
  (concat (muse-publishing-directive \"title\")
          (let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat \" (by \" author \")\"))))</lisp></title>
    <meta name=\"generator\" content=\"muse.el\" />
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
          content=\"<lisp>muse-html-meta-content-type</lisp>\" />
    <lisp>
      (let ((maintainer (muse-style-element :maintainer)))
        (when maintainer
          (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\" />\")))
    </lisp>
    <lisp>muse-html-style-sheet</lisp>
  </head>
  <body>
    <h1><lisp>
  (concat (muse-publishing-directive \"title\")
          (let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat \" (by \" author \")\"))))</lisp></h1>
    <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing XHTML files."
  :type '(choice string file)
  :group 'muse-html)

(defcustom muse-xhtml-footer "
<!-- Page published by Emacs Muse ends here -->
  </body>
</html>\n"
  "Footer used for publishing XHTML files."
  :type '(choice string file)
  :group 'muse-html)

(defcustom muse-html-anchor-on-word nil
  "When true, anchors surround the closest word. This allows you
to select them in a browser (i.e. for pasting), but has the
side-effect of marking up headers in multiple colors if your
header style is different from your link style."
  :type 'boolean
  :group 'muse-html)

(defcustom muse-html-table-attributes
  "class=\"muse-table\" border=\"2\" cellpadding=\"5\""
  "The attribute to be used with HTML <table> tags.
Note that since Muse supports direct insertion of HTML tags, you
can easily create any kind of table you want, as long as each
line begins at column 0 (to prevent it from being blockquoted).
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

    ;; beginning of doc, end of doc, or plain paragraph separator
    (10300 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?"
                    "\\(?:\n\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\|\\`\\s-*\\|\\s-*\\'\\)"
                    "\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-html-markup-paragraph))
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
  "Strings used for marking up text as HTML.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-html)

(defcustom muse-xhtml-markup-strings
  '((image-with-desc . "<img src=\"%s\" alt=\"%s\" />")
    (image-link      . "<img src=\"%s\" alt=\"\" />")
    (url-with-image  . "<a class=\"image-link\" href=\"%s\"><img src=\"%s\" alt=\"\" /></a>")
    (rule            . "<hr />")
    (fn-sep          . "<hr />\n")
    (begin-underline . "<span style=\"text-decoration: underline;\">\n")
    (end-underline   . "</span>")
    (begin-center    . "<span style=\"text-align: center;\">\n")
    (end-verse-line  . "<br />")
    (last-stanza-end . "<br />")
    (empty-verse-line . "<br />")
    (end-center      . "\n</span>"))
  "Strings used for marking up text as XHTML.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles.

If a markup rule is not found here, `muse-html-markup-strings' is
searched."
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
  "The content type used for the HTML <meta> tag.
If you are striving for XHTML 1.1 compliance, you may want to
change this to \"application/xhtml+xml\"."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-meta-content-encoding (if (featurep 'mule)
                                              'detect
                                            "iso-8859-1")
  "The charset to append to the HTML <meta> tag.
If set to the symbol 'detect, use `muse-html-encoding-map' to try
and determine the HTML charset from emacs's coding.  If set to a
string, this string will be used to force a particular charset"
  :type '(choice string symbol)
  :group 'muse-html)

(defcustom muse-html-charset-default "iso-8859-1"
  "The default HTML meta charset to use if no translation is found in
`muse-html-encoding-map'."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-encoding-default 'iso-8859-1
  "The default Emacs buffer encoding to use in published files.
This will be used if no special characters are found."
  :type 'symbol
  :group 'muse-html)

(defcustom muse-html-encoding-map
  '((iso-8859-1         . "iso-8859-1")
    (iso-2022-jp        . "iso-2022-jp")
    (utf-8              . "utf-8")
    (japanese-iso-8bit  . "euc-jp")
    (chinese-big5       . "big5")
    (mule-utf-8         . "utf-8")
    (chinese-iso-8bit   . "gb2312")
    (chinese-gbk        . "gbk"))
  "An alist mapping emacs coding systems to appropriate HTML charsets.
Use the base name of the coding system (i.e. without the -unix)."
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
                                   (muse-line-end-position) t)
                   (goto-char (match-beginning 0)))
              (forward-word 1)))
        (insert "</a>"))
    (insert "<a name=\"" anchor "\" id=\"" anchor "\">")
    (when muse-html-anchor-on-word
      (forward-word 1))
    (insert "</a>\n")))

(defun muse-html-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (match-beginning 0))
    (when (save-excursion
            (save-match-data
              (and (re-search-backward "<\\(/?\\)p[ >]" nil t)
                   (not (string-equal (match-string 1) "/")))))
      (insert "</p>"))
    (goto-char end))
  (cond
   ((eobp)
    (unless (bolp)
      (insert "\n")))
   ((eq (char-after) ?\<)
    (cond
     ((looking-at "<\\(em\\|strong\\|code\\|span\\)[ >]")
      (insert "<p>"))
     ((looking-at "<a ")
      (if (looking-at "<a[^>]+><img")
          (insert "<p class=\"image-link\">")
        (insert "<p>")))
     ((looking-at "<img[ >]")
      (insert "<p class=\"image-link\">"))))
   ((muse-looking-back "\\(</h[1-4]>\\|<hr>\\)\n\n")
    (insert "<p class=\"first\">"))
   ((muse-looking-back "<\\(blockquote\\|center\\)>\n")
    (insert "<p class=\"quoted\">"))
   (t
    (insert "<p>"))))

(defun muse-html-markup-anchor ()
  (save-match-data
    (muse-html-insert-anchor (match-string 1))) "")

(defun muse-html-escape-string (str &rest ignored)
  "Convert to character entities any non-alphanumeric characters
outside a few punctuation symbols, that risk being misinterpreted
if not escaped."
  (when str
    (let (pos code len ch)
      (save-match-data
        (while (setq pos (string-match (concat "[^-"
                                               muse-regexp-alnum
                                               "/:._=@\\?~#]")
                                       str pos))
          (setq ch (aref str pos)
                code (cond
                      ((char-equal ch ?\&)
                       "&amp;")
                      ((char-equal ch ?\<)
                       "&lt;")
                      ((char-equal ch ?\>)
                       "&gt;")
                      (t (concat "&#"
                                 (int-to-string
                                  (cond ((fboundp 'char-to-ucs)
                                         (char-to-ucs ch))
                                        ((fboundp 'char-to-int)
                                         (char-to-int ch))
                                        (t ch)))
                                 ";")))
                len (length code)
                str (concat (substring str 0 pos)
                            code
                            (when (< pos (length str))
                              (substring str (1+ pos) nil)))
                pos (+ len pos)))
        str))))

(defun muse-html-markup-footnote ()
  (if (/= (muse-line-beginning-position) (match-beginning 0))
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
                    ((= type 3) "td"))))
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
            (setq l (1- (string-to-number (match-string 1))))
            (if (null base)
                (setq base l)
              (if (< l base)
                  (throw 'done t)))
            (when (<= l max-depth)
              (setq contents (cons (cons l (muse-match-string-no-properties 2))
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
             ((> (caar contents) depth) ; can't jump more than one ahead
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
                     :browser   'muse-html-browse-file)

  (muse-derive-style "xhtml" "html"
                     :strings   'muse-xhtml-markup-strings
                     :header    'muse-xhtml-header
                     :footer    'muse-xhtml-footer))

(provide 'muse-html)

;;; muse-html.el ends here
