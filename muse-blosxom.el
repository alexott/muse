;;; muse-blosxom.el --- Publish a document tree for serving by (py)Blosxom

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: muse-blosxom.el
;; Version: 3.00
;; Date: Wed, 23 March 2005
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Publish a document tree for serving by (py)Blosxom
;; URL: http://www.mwolson.org/projects/MuseMode.html
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

;; Blosxom publishes a tree of categorised files to a mirrored tree of
;; blosxom stories to be served by blosxom.cgi or pyblosxom.cgi.
;;
;; Each Blosxom file must include `#date yyyy-mm-dd', or optionally
;; the longer `#date yyyy-mm-dd-hh-mm', plus whatever normal content
;; is desired.
;;
;; This date directive is not used directly by (py)blosxom or this
;; program.  You need to find two additional items to make use of this
;; feature.
;;
;;  1. A script to gather date directives from the entire blog tree
;;     into a single file.  The file must associate a blog entry with
;;     a date.
;;
;;  2. A plugin for (py)blosxom that reads this file.
;;
;; These 2 things are provided for pyblosxom in the contrib/pyblosxom
;; subdirectory.  `getstamps.py' provides the 1st service, while
;; `hardcodedates.py' provides the second service.  Eventually it is
;; hoped that a blosxom plugin and script will be found/written.

;;; Contributors:

;; Gary Vaughan (gary AT gnu DOT org) is the original author of
;; `emacs-wiki-blosxom.el', which is the ancestor of this file.

;; Brad Collins (brad AT chenla DOT org) ported this file to Muse.

;; Michael Olson (mwolson AT gnu DOT org) further adapted this file to
;; Muse and continues to maintain it.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Blosxom Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-html)

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

(defcustom muse-blosxom-markup-regexps
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
           0 "\\1</p>\n")
    ;; planner stuff
    (10600 "^#\\([A-C]\\)\\([0-9]*\\)\\s-*\\([_oX>CP]\\)\\s-*\\(.+\\)"
	   0 planner-markup-task)
    (10700 "^\\.#\\([0-9]+\\)" 0 planner-markup-note)
    ;; date directive
    (10800 "^#date\\s-+\\(.+\\)\n+" 0 muse-blosxom-markup-date-directive))
  "List of markup rules for publishing a Muse page to BLOSXOM.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
		  (list :tag "Markup rule"
			(choice regexp symbol)
			integer
			(choice string function symbol))
		  function))
  :group 'muse-blosxom)

;;; Register the BLOSXOM Publisher

(unless (assoc "blosxom" muse-publishing-styles)
  (muse-derive-style "blosxom-html" "html"
		     :suffix    'muse-blosxom-extension
		     :regexps   'muse-blosxom-markup-regexps
		     :header    'muse-blosxom-header
		     :footer    'muse-blosxom-footer)

  (muse-derive-style "blosxom-xhtml" "xhtml"
		     :suffix    'muse-blosxom-extension
		     :regexps   'muse-blosxom-markup-regexps
		     :header    'muse-blosxom-header
		     :footer    'muse-blosxom-footer))

;;; Maintain (published-file . date) alist

(defvar muse-blosxom-page-date-alist nil)

;; This isn't really used for anything, but it may be someday
(defun muse-blosxom-markup-date-directive ()
  "Add a date entry to `muse-blosxom-page-date-alist' for this page."
  (let ((date (match-string 1)))
    (save-match-data
      (add-to-list
       'muse-blosxom-page-date-alist
       `(,buffer-file-name . ,date))))
  "")

(provide 'muse-blosxom)

;;; muse-blosxom.el ends here
