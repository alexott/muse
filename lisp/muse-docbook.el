;;; muse-docbook.el --- Publish DocBook files.

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

;; Dale P. Smith (dpsm AT en DOT com) improved the markup
;; significantly and made many valuable suggestions.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse DocBook XML Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-regexps)

(defgroup muse-docbook nil
  "Options controlling the behavior of Muse DocBook XML publishing.
See `muse-docbook' for more information."
  :group 'muse-publish)

(defcustom muse-docbook-extension ".xml"
  "Default file extension for publishing DocBook XML files."
  :type 'string
  :group 'muse-docbook)

(defcustom muse-docbook-header
  "<?xml version=\"1.0\" encoding=\"<lisp>
  (muse-docbook-encoding)</lisp>\"?>
<!DOCTYPE article PUBLIC \"-//OASIS//DTD DocBook V4.2//EN\"
                  \"http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd\">
<article>
  <articleinfo>
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>
    <author><lisp>(muse-docbook-get-author
                    (muse-publishing-directive \"author\"))</lisp></author>
    <pubdate><lisp>(muse-publishing-directive \"date\")</lisp></pubdate>
  </articleinfo>
  <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing DocBook XML files.
This may be text or a filename."
  :type 'string
  :group 'muse-docbook)

(defcustom muse-docbook-footer "
  <!-- Page published by Emacs Muse ends here -->
</article>\n"
  "Footer used for publishing DocBook XML files.
This may be text or a filename."
  :type 'string
  :group 'muse-docbook)

(defcustom muse-docbook-markup-regexps
  `(;; join together the parts of a list or table
    (10000 "</\\([oud]l\\)>\\s-*<\\1>\\s-*" 0 "")
    (10100 ,(concat "  </t\\(body\\|head\\|foot\\)>\\s-*"
                    "</tgroup>\\s-*</informaltable>\\s-*"
                    "<informaltable[^>]*>\\s-*<tgroup[^>]*>\\s-*"
                    "<t\\1>\n") 0 "")
    (10200 ,(concat "  </tgroup>\\s-*</informaltable>\\s-*"
                    "<informaltable[^>]*>\\s-*<tgroup[^>]*>\n") 0 "")

    ;; Merge consecutive list tags
    (10300 ,(concat "</\\(itemized\\|ordered\\|variable\\)list>"
                    "\\s-*<\\1list" "[^>]*>\\s-*") 0 "")

    ;; beginning of doc, end of doc, or plain paragraph separator
    (10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?"
                    "\\(?:\n\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\|\\`\\s-*\\|\\s-*\\'\\)"
                    "\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-docbook-markup-paragraph))
  "List of markup rules for publishing a Muse page to DocBook XML.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-docbook)

(defcustom muse-docbook-markup-functions
  '((anchor . muse-docbook-markup-anchor)
    (table . muse-docbook-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-docbook)

(defcustom muse-docbook-markup-strings
  '((url-link        . "<ulink url=\"%s\">%s</ulink>")
    (email-addr      . "<email>%s</email>")
    (emdash          . " &mdash; ")
    (rule            . "")
    (enddots         . "....")
    (dots            . "...")
    (section         . "<section><title>")
    (section-end     . "</title>")
    (subsection      . "<section><title>")
    (subsection-end  . "</title>")
    (subsubsection   . "<section><title>")
    (subsubsection-end . "</title>")
    (section-other   . "<section><title>")
    (section-other-end . "</title>")
    (section-close   . "</section>")
    (footnote        . "<footnote><para>")
    (footnote-end    . "</para></footnote>")
    (begin-underline . "")
    (end-underline   . "")
    (begin-literal   . "<systemitem>")
    (end-literal     . "</systemitem>")
    (begin-emph      . "<emphasis>")
    (end-emph        . "</emphasis>")
    (begin-more-emph . "<emphasis role=\"strong\">")
    (end-more-emph   . "</emphasis>")
    (begin-most-emph . "<emphasis role=\"strong\"><emphasis>")
    (end-most-emph   . "</emphasis></emphasis>")
    (begin-verse     . "<literallayout>\n")
    (verse-space     . "  ")
    (end-verse       . "</literallayout>")
    (begin-example   . "<programlisting>")
    (end-example     . "</programlisting>")
    (begin-center    . "<center>\n")
    (end-center      . "\n</center>")
    (begin-quote     . "<blockquote>\n")
    (end-quote       . "\n</blockquote>")
    (begin-uli       . "<itemizedlist mark=\"bullet\">\n<listitem><para>")
    (end-uli         . "</para></listitem>\n</itemizedlist>")
    (begin-oli       . "<orderedlist>\n<listitem><para>")
    (end-oli         . "</para></listitem>\n</orderedlist>")
    (begin-ddt       . "<variablelist>\n<varlistentry>\n<term>")
    (start-dde       . "</term>\n<listitem><para>")
    (end-ddt         . "</para></listitem>\n</varlistentry>\n</variablelist>"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-docbook)

(defcustom muse-docbook-markup-specials
  '((?\" . "&quot;")
    (?\< . "&lt;")
    (?\> . "&gt;")
    (?\& . "&amp;"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-docbook)

(defcustom muse-docbook-encoding-default 'utf-8
  "The default Emacs buffer encoding to use in published files.
This will be used if no special characters are found."
  :type 'symbol
  :group 'muse-docbook)

(defcustom muse-docbook-charset-default "utf-8"
  "The default DocBook XML charset to use if no translation is
found in `muse-docbook-encoding-map'."
  :type 'string
  :group 'muse-docbook)

(defcustom muse-docbook-encoding-map
  '((iso-8859-1         . "iso-8859-1")
    (iso-2022-jp        . "iso-2022-jp")
    (utf-8              . "utf-8")
    (japanese-iso-8bit  . "euc-jp")
    (chinese-big5       . "big5")
    (mule-utf-8         . "utf-8")
    (chinese-iso-8bit   . "gb2312")
    (chinese-gbk        . "gbk"))
  "An alist mapping emacs coding systems to appropriate DocBook charsets.
Use the base name of the coding system (i.e. without the -unix)."
  :type '(alist :key-type coding-system :value-type string)
  :group 'muse-docbook)

(defun muse-docbook-transform-content-type (content-type)
  "Using `muse-docbook-encoding-map', try and resolve an emacs
coding system to an associated DocBook XML coding system. If no
match is found, `muse-docbook-charset-default' is used instead."
  (let ((match (and (fboundp 'coding-system-base)
                    (assoc (coding-system-base content-type)
                           muse-docbook-encoding-map))))
    (if match
        (cdr match)
      muse-docbook-charset-default)))

(defun muse-docbook-encoding ()
  (muse-docbook-transform-content-type
   (or (and (boundp 'buffer-file-coding-system)
            buffer-file-coding-system)
       muse-docbook-encoding-default)))

(defun muse-docbook-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (match-beginning 0))
    (when (save-excursion
            (save-match-data
              (and (re-search-backward "<\\(/?\\)\\(para\\|footnote\\)[ >]"
                                       nil t)
                   (or (and (string= (match-string 2) "para")
                            (not (string= (match-string 1) "/")))
                       (and (string= (match-string 2) "footnote")
                            (string= (match-string 1) "/"))))))
      (insert "</para>"))
    (goto-char end))
  (cond
   ((eobp)
    (unless (bolp)
      (insert "\n")))
   ((eq (char-after) ?\<)
    (when (looking-at (concat "<\\(emphasis\\|systemitem"
                              "\\|ulink\\|anchor\\|email\\)[ >]"))
      (insert "<para>")))
   (t
    (insert "<para>"))))

(defun muse-docbook-markup-anchor ()
  (save-match-data
    (muse-docbook-insert-anchor (match-string 1))) "")

(defun muse-docbook-insert-anchor (anchor)
  "Insert an anchor, either around the word at point, or within a tag."
  (skip-chars-forward muse-regexp-space)
  (when (looking-at "<\\([^ />]+\\)>")
    (goto-char (match-end 0)))
  (insert "<anchor id=\"" anchor "\" />"))

(defun muse-docbook-markup-table ()
  (let* ((str (prog1
                  (match-string 1)
                (delete-region (match-beginning 0) (match-end 0))))
         (fields (split-string str "\\s-*|+\\s-*"))
         (type (and (string-match "\\s-*\\(|+\\)\\s-*" str)
                    (length (match-string 1 str))))
         (part (cond ((= type 1) "tbody")
                     ((= type 2) "thead")
                     ((= type 3) "tfoot"))))
    (insert "<informaltable>\n"
            "  <tgroup cols='" (number-to-string (length fields)) "'>\n"
            "  <" part ">\n"
            "    <row>\n")
    (dolist (field fields)
      (insert "      <entry>" field "</entry>\n"))
    (insert "    </row>\n"
            "  </" part ">\n"
            "  </tgroup>\n"
            "</informaltable>\n")))

(defun muse-docbook-get-author (&optional author)
  "Split the AUTHOR directive into separate fields.
AUTHOR should be of the form: \"Firstname Other Names Lastname\",
and anything after `Firstname' is optional."
  (setq author (save-match-data (split-string author)))
  (let ((num-el (length author)))
    (cond ((eq num-el 1)
           (concat "<firstname>" (car author) "</firstname>"))
          ((eq num-el 2)
           (concat "<firstname>" (nth 0 author) "</firstname>"
                   "<surname>" (nth 1 author) "</surname>"))
          ((eq num-el 3)
           (concat "<firstname>" (nth 0 author) "</firstname>"
                   "<othername>" (nth 1 author) "</othername>"
                   "<surname>" (nth 2 author) "</surname>"))
          (t
           (let (first last)
             (setq first (car author))
             (setq author (nreverse (cdr author)))
             (setq last (car author))
             (setq author (nreverse (cdr author)))
             (concat "<firstname>" first "</firstname>"
                     "<othername>"
                     (mapconcat 'identity author " ")
                     "</othername>"
                     "<surname>" last "</surname>"))))))

(defun muse-docbook-fixup-tables ()
  "Sort table parts."
  (goto-char (point-min))
  (let (last)
    (while (re-search-forward "^<informaltable>$" nil t)
      (unless (get-text-property (point) 'read-only)
        (forward-line 2)
        (save-restriction
          (let ((beg (point)))
            (narrow-to-region beg (and (re-search-forward "^  </tgroup>$"
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

(defun muse-docbook-finalize-buffer ()
  (when (boundp 'buffer-file-coding-system)
    (when (memq buffer-file-coding-system '(no-conversion undecided-unix))
      ;; make it agree with the default charset
      (setq buffer-file-coding-system muse-docbook-encoding-default))))

;; Register the Muse DocBook XML Publisher

(unless (assoc "docbook" muse-publishing-styles)
  (muse-define-style "docbook"
                     :suffix     'muse-docbook-extension
                     :regexps    'muse-docbook-markup-regexps
                     :functions  'muse-docbook-markup-functions
                     :strings    'muse-docbook-markup-strings
                     :specials   'muse-docbook-markup-specials
                     :before-end 'muse-docbook-fixup-tables
                     :after      'muse-docbook-finalize-buffer
                     :header     'muse-docbook-header
                     :footer     'muse-docbook-footer
                     :browser    'find-file))

(provide 'muse-docbook)

;;; muse-docbook.el ends here
