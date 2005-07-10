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
<!DOCTYPE article PUBLIC \"-//OASIS//DTD DocBook V4.2//EN\">
<article>
  <articleinfo>
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>
    <author><lisp>(muse-publishing-directive \"author\")</lisp></author>
    <pubdate><lisp>(muse-publishing-directive \"date\")</lisp></pubdate>
  </articleinfo>
  <!-- Page published by Emacs Muse begins here -->\n"
  "Header used for publishing DocBook XML files."
  :type '(choice string file)
  :group 'muse-docbook)

(defcustom muse-docbook-footer "
  <!-- Page published by Emacs Muse ends here -->
</article>\n"
  "Footer used for publishing DocBook XML files."
  :type '(choice string file)
  :group 'muse-docbook)

(defcustom muse-docbook-markup-regexps
  `(;; join together the parts of a list or table
    (10000 "</\\([oud]l\\)>\\s-*<\\1>\\s-*" 0 "")
    (10100 "</tbody>\\s-*</table>\\s-*<table[^>]*>\\s-*<tbody>\\s-*" 0 "")
    (10200 "</table>\\s-*<table[^>]*>\\s-*" 0 "")

    ;; beginning of doc, end of doc, or plain paragraph separator
    (10300 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?"
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
  '((table . muse-docbook-markup-table))
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
    (footnote        . "<footnote><para>")
    (footnote-end    . "</para></footnote>")
    (begin-underline . "")
    (end-underline   . "")
    (begin-literal   . "<filename>")
    (end-literal     . "</filename>")
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
  (let ((match (assoc (coding-system-base content-type)
                      muse-docbook-encoding-map)))
    (if match
        (cdr match)
      muse-docbook-charset-default)))

(defun muse-docbook-encoding ()
  (muse-docbook-transform-content-type
   (or buffer-file-coding-system
       muse-docbook-encoding-default)))

(defun muse-docbook-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (match-beginning 0))
    (when (save-excursion
            (save-match-data
              (and (re-search-backward "<\\(/?\\)para[ >]" nil t)
                   (not (string-equal (match-string 1) "/")))))
      (insert "</para>"))
    (goto-char end))
  (if (eobp)
      (cond
       ((bolp)
        nil)
       (t
        (insert "\n")))
    (unless (eq (char-after) ?\<)
      (insert "<para>"))))

(defun muse-docbook-markup-table ()
  (let* ((str (save-match-data
                (if (featurep 'xemacs)
                    ;; more emacs divergence. :(
                    (replace-in-string (match-string 1) " *|+ *$" "")
                  (match-string 1))))
         (fields (append (save-match-data
                           (split-string str (concat "["
                                                     muse-regexp-blank
                                                     "]*|+["
                                                     muse-regexp-blank
                                                     "]*")))
                         (list (match-string 4))))
         (len (length (match-string 3)))
         (row (cond ((= len 1) "tbody")
                    ((= len 2) "thead")
                    ((= len 3) "tfoot")))
         (col "entry"))
    (concat "<table>\n" "<" row ">\n" "<row>\n<" col ">"
            (mapconcat 'identity fields (format "</%s><%s>" col col))
            "</" col ">\n" "</row>\n" "</" row ">\n"
            "</table>\n")))

(defcustom muse-docbook-merged-tags
  '("itemizedlist" "orderedlist" "section" "variablelist")
  "Tags which need to be merged together if they are consecutive."
  :type '(repeat (string :tag "Tag"))
  :group 'muse-docbook)

(defun muse-docbook-fixup-tags ()
  "Merge multiple sections of tags from `muse-docbook-merged-tags'."
  (dolist (tag muse-docbook-merged-tags)
    (goto-char (point-min))
    (let (last)
      (while (re-search-forward (concat "\n*<" tag ">") nil t)
        (when last
          (replace-match (concat "\n</" tag ">\n\n<" tag ">")))
        (setq last (match-beginning 0)))
      (when last
        (goto-char (point-max))
        (insert "</" tag ">")))))

(defun muse-docbook-finalize-buffer ()
  (when (memq buffer-file-coding-system '(no-conversion undecided-unix))
    ;; make it agree with the default charset
    (setq buffer-file-coding-system muse-docbook-encoding-default)))

;; Register the Muse DocBook XML Publisher

(unless (assoc "docbook" muse-publishing-styles)
  (muse-define-style "docbook"
                     :suffix     'muse-docbook-extension
                     :regexps    'muse-docbook-markup-regexps
                     :functions  'muse-docbook-markup-functions
                     :strings    'muse-docbook-markup-strings
                     :specials   'muse-docbook-markup-specials
                     :before-end 'muse-docbook-fixup-tags
                     :after      'muse-docbook-finalize-buffer
                     :header     'muse-docbook-header
                     :footer     'muse-docbook-footer
                     :browser    'find-file))

(provide 'muse-docbook)

;;; muse-docbook.el ends here
