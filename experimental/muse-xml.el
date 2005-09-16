;;; muse-xml.el --- publish XML files

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A Compact RelaxNG schema is available in `examples/muse.rnc'.  The
;; current maintainer has no idea how to make use of it, except that
;; it might come in handy with nxml-mode, xml.el, xml-parse.el, or
;; XSLT.
;;
;; This file is currently in experimental state.  This means that the
;; published output is subject to change.  This also means that you
;; still have the opportunity to correct braindeaded publishing
;; choices by sending email to the list :^) .

;;; Contributors:

;; Peter K. Lee (saint AT corenova DOT com) made the initial
;; implementation of planner-publish.el, which was heavily borrowed
;; from.
;;
;; Brad Collins (brad AT chenla DOT org) provided a Compact RelaxNG
;; schema.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse XML Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-regexps)

(defgroup muse-xml nil
  "Options controlling the behavior of Muse XML publishing.
See `muse-xml' for more information."
  :group 'muse-publish)

(defcustom muse-xml-extension ".xml"
  "Default file extension for publishing XML files."
  :type 'string
  :group 'muse-xml)

(defcustom muse-xml-header
  "<?xml version=\"1.0\" encoding=\"<lisp>
  (muse-xml-encoding)</lisp>\"?>
<MUSE>
  <pageinfo>
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>
    <author><lisp>(muse-publishing-directive \"author\")</lisp></author>
    <maintainer><lisp>(muse-style-element :maintainer)</lisp></maintainer>
    <pubdate><lisp>(muse-publishing-directive \"date\")</lisp></pubdate>
  </pageinfo>
  <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing XML files.
This may be text or a filename."
  :type 'string
  :group 'muse-xml)

(defcustom muse-xml-footer "
  <!-- Page published by Emacs Muse ends here -->
</page>\n"
  "Footer used for publishing Xml XML files.
This may be text or a filename."
  :type 'string
  :group 'muse-xml)

(defcustom muse-xml-markup-regexps
  `(;; Join together the parts of a table
    (10000 ,(concat "  </t\\(body\\|head\\|foot\\)>\\s-*</table>\\s-*"
                    "<table[^>]*>\\s-*<t\\1>\n") 0 "")
    (10100 "</table>\\s-*<table[^>]*>\n" 0 "")

    ;; Join together the parts of a list
    (10200 "</list>\\s-*<list[^>]*>\\s-*" 0 "")

    ;; Beginning of doc, end of doc, or plain paragraph separator
    (10300 ,(concat "\\(\n</\\(blockquote\\|format\\)>\\)?"
                    "\\(\\(\n\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\)\\|\\`\\s-*\\|\\s-*\\'\\)"
                    "\\(<\\(blockquote\\|format type=\"center\"\\)>\n\\)?")
           0 muse-xml-markup-paragraph))
  "List of markup rules for publishing a Muse page to XML.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-xml)

(defcustom muse-xml-markup-functions
  '((anchor . muse-xml-markup-anchor)
    (table . muse-xml-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-xml)

(defcustom muse-xml-markup-strings
  '((image-with-desc . "<image href=\"%s\">%s</image>")
    (image-link      . "<image href=\"%s\"></image>")
    (url-with-image  . "<link type=\"image\" href=\"%s\">%s</link>")
    (url-link        . "<link type=\"url\" href=\"%s\">%s</link>")
    (email-addr      . "<link type=\"email\" href=\"%s\">%s</link>")
    (emdash          . " &mdash; ")
    (rule            . "<hr>")
    (fn-sep          . "<hr>\n")
    (enddots         . "....")
    (dots            . "...")
    (section         . "<section level=\"1\"><title>")
    (section-end     . "</title>")
    (subsection      . "<section level=\"2\"><title>")
    (subsection-end  . "</title>")
    (subsubsection   . "<section level=\"3\"><title>")
    (subsubsection-end . "</title>")
    (section-other   . "<section level=\"%s\"><title>")
    (section-other-end . "</title>")
    (section-close   . "</section>")
    (footnote        . "<footnote>")
    (footnote-end    . "</footnote>")
    (begin-underline . "<format type=\"underline\">")
    (end-underline   . "</format>")
    (begin-literal   . "<code>")
    (end-literal     . "</code>")
    (begin-emph      . "<format type=\"emphasis\" level=\"1\">")
    (end-emph        . "</format>")
    (begin-more-emph . "<format type=\"emphasis\" level=\"2\">")
    (end-more-emph   . "</format>")
    (begin-most-emph . "<format type=\"emphasis\" level=\"3\">")
    (end-most-emph   . "</format>")
    (begin-verse     . "<verse>\n")
    (begin-verse-line . "<line>")
    (end-verse-line  . "</line>")
    (empty-verse-line . "<line />")
    (begin-last-stanza-line . "<line>")
    (end-last-stanza-line . "</line>")
    (end-verse       . "</verse>")
    (begin-example   . "<example>")
    (end-example     . "</example>")
    (begin-center    . "<format type=\"center\">\n")
    (end-center      . "\n</format>")
    (begin-quote     . "<blockquote>\n")
    (end-quote       . "\n</blockquote>")
    (begin-uli       . "<list type=\"unordered\">\n<item>")
    (end-uli         . "</item>\n</list>")
    (begin-oli       . "<list type=\"ordered\">\n<item>")
    (end-oli         . "</item>\n</list>")
    (begin-ddt       . "<list type=\"definition\">\n<item><term>")
    (start-dde       . "</term>\n<definition>")
    (end-ddt         . "</definition>\n</item>\n</list>"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-xml)

(defcustom muse-xml-markup-specials
  '((?\" . "&quot;")
    (?\< . "&lt;")
    (?\> . "&gt;")
    (?\& . "&amp;"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-xml)

(defcustom muse-xml-encoding-default 'utf-8
  "The default Emacs buffer encoding to use in published files.
This will be used if no special characters are found."
  :type 'symbol
  :group 'muse-xml)

(defcustom muse-xml-charset-default "utf-8"
  "The default Xml XML charset to use if no translation is
found in `muse-xml-encoding-map'."
  :type 'string
  :group 'muse-xml)

(defcustom muse-xml-encoding-map
  '((iso-8859-1         . "iso-8859-1")
    (iso-2022-jp        . "iso-2022-jp")
    (utf-8              . "utf-8")
    (japanese-iso-8bit  . "euc-jp")
    (chinese-big5       . "big5")
    (mule-utf-8         . "utf-8")
    (chinese-iso-8bit   . "gb2312")
    (chinese-gbk        . "gbk"))
  "An alist mapping emacs coding systems to appropriate Xml charsets.
Use the base name of the coding system (i.e. without the -unix)."
  :type '(alist :key-type coding-system :value-type string)
  :group 'muse-xml)

(defun muse-xml-transform-content-type (content-type)
  "Using `muse-xml-encoding-map', try and resolve an emacs
coding system to an associated XML coding system. If no
match is found, `muse-xml-charset-default' is used instead."
  (let ((match (and (fboundp 'coding-system-base)
                    (assoc (coding-system-base content-type)
                           muse-xml-encoding-map))))
    (if match
        (cdr match)
      muse-xml-charset-default)))

(defun muse-xml-encoding ()
  (muse-xml-transform-content-type
   (or (and (boundp 'buffer-file-coding-system)
            buffer-file-coding-system)
       muse-xml-encoding-default)))

(defun muse-xml-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (match-beginning 0))
    (when (save-excursion
            (save-match-data
              (and (re-search-backward "<\\(/?\\)p[ >]"
                                       nil t)
                   (not (string-equal (match-string 1) "/")))))
      (insert "</p>"))
    (goto-char end))
  (cond
   ((eobp)
    (unless (bolp)
      (insert "\n")))
   ((eq (char-after) ?\<)
    (when (looking-at (concat "<\\(format\\|code\\|link\\|image"
                              "\\|anchor\\|footnote\\)[ >]"))
      (insert "<p>")))
   (t
    (insert "<p>"))))

(defun muse-xml-markup-anchor ()
  (save-match-data
    (muse-xml-insert-anchor (match-string 1))) "")

(defun muse-xml-insert-anchor (anchor)
  "Insert an anchor, either around the word at point, or within a tag."
  (skip-chars-forward muse-regexp-space)
  (when (looking-at "<\\([^ />]+\\)>")
    (goto-char (match-end 0)))
  (insert "<anchor id=\"" anchor "\" />"))

(defun muse-xml-markup-table ()
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
    (insert "<table>\n"
            "  <" part ">\n"
            "    <tr>\n")
    (dolist (field fields)
      (insert "      <" col ">" field "</" col ">\n"))
    (insert "    </tr>\n"
            "  </" part ">\n"
            "</table>\n")))

(defun muse-xml-fixup-tables ()
  "Sort table parts."
  (goto-char (point-min))
  (let (last)
    (while (re-search-forward "^<table[^>]*>$" nil t)
      (unless (get-text-property (point) 'read-only)
        (forward-line 1)
        (save-restriction
          (let ((beg (point)))
            (narrow-to-region beg (and (re-search-forward "^</table>"
                                                          nil t)
                                       (match-beginning 0))))
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (sort-subr nil
                       (function
                        (lambda ()
                          (if (re-search-forward
                               "^\\s-*<t\\(head\\|body\\|foot\\)>$" nil t)
                              (goto-char (match-beginning 0))
                            (goto-char (point-max)))))
                       (function
                        (lambda ()
                          (if (re-search-forward
                               "^\\s-*</t\\(head\\|body\\|foot\\)>$" nil t)
                              (goto-char (match-end 0))
                            (goto-char (point-max)))))
                       (function
                        (lambda ()
                          (looking-at "\\s-*<t\\(head\\|body\\|foot\\)>")
                          (cond ((string= (match-string 1) "head") 1)
                                ((string= (match-string 1) "foot") 2)
                                (t 3)))))))))))

(defun muse-xml-finalize-buffer ()
  (when (boundp 'buffer-file-coding-system)
    (when (memq buffer-file-coding-system '(no-conversion undecided-unix))
      ;; make it agree with the default charset
      (setq buffer-file-coding-system muse-xml-encoding-default))))

;; Register the Muse XML Publisher

(unless (assoc "xml" muse-publishing-styles)
  (muse-define-style "xml"
                     :suffix     'muse-xml-extension
                     :regexps    'muse-xml-markup-regexps
                     :functions  'muse-xml-markup-functions
                     :strings    'muse-xml-markup-strings
                     :specials   'muse-xml-markup-specials
                     :before-end 'muse-xml-fixup-tables
                     :after      'muse-xml-finalize-buffer
                     :header     'muse-xml-header
                     :footer     'muse-xml-footer
                     :browser    'find-file))

(provide 'muse-xml)

;;; muse-xml.el ends here
