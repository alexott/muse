;;; muse-xhtml.el --- Muse XHTML publishing style.

;; Copyright (C) 2005 Free Software Foundation, Inc.

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

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse XHTML Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-html)

(defgroup muse-xhtml nil
  "Options controlling the behaviour of Muse XHTML publishing.
See `muse-xhtml' for more information."
  :group 'muse-publish)

(defcustom muse-xhtml-extension ".html"
  "Default file extension for publishing XHTML files."
  :type 'string
  :group 'muse-xhtml)

(defcustom muse-xhtml-style-sheet
  "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/default.css\" />"
  "Store your stylesheet definitions here.
This is used in `muse-xhtml-header'.
You can put raw CSS in here or a <link> tag to an external stylesheet.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'muse-xhtml)

(defcustom muse-xhtml-header
  "<?xml version=\"1.0\"?>
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
    <meta http-equiv=\"<lisp>muse-xhtml-meta-http-equiv</lisp>\"
	  content=\"<lisp>muse-html-meta-content-type</lisp>\" />
    <lisp>
      (let ((maintainer (muse-style-element :maintainer)))
	(when maintainer
	  (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\" />\")))
    </lisp>
    <lisp>muse-xhtml-style-sheet</lisp>
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
  :group 'muse-xhtml)

(defcustom muse-xhtml-footer "
<!-- Page published by Emacs Muse ends here -->
  </body>
</html>\n"
  "Footer used for publishing XHTML files."
  :type '(choice string file)
  :group 'muse-xhtml)

(defcustom muse-xhtml-markup-strings
  '((image-with-desc . "<img src=\"%s\" alt=\"%s\" />")
    (image-link      . "<img src=\"%s\" alt=\"\" />")
    (url-with-image  . "<a class=\"image-link\" href=\"%s\"><img src=\"%s\" alt=\"\" /></a>")
    (begin-underline . "<span style=\"text-decoration: underline;\">\n")
    (end-underline   . "</span>")
    (begin-center    . "<span style=\"text-align: center;\">\n")
    (end-center      . "\n</span>"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles.

If a markup rule is not found here, `muse-html-markup-strings' is
searched."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-xhtml)

(defcustom muse-xhtml-meta-http-equiv "Content-Type"
  "The http-equiv attribute used for the HTML <meta> tag."
  :type 'string
  :group 'muse-xhtml)

(defcustom muse-xhtml-meta-content-type "text/html"
  "The content type used for the HTML <meta> tag."
  :type 'string
  :group 'muse-xhtml)

(defun muse-xhtml-prepare-buffer ()
  "Prepare this buffer for use with the XHTML publisher."
  ;; Call the HTML "constructor"
  (muse-html-prepare-buffer)
  ;; Be safe by making this buffer-local
  (make-local-variable 'muse-xhtml-meta-http-equiv)
  ;; Reset the value of `muse-html-meta-content-type' -- it has
  ;; already been made buffer local by the HTML constructor.
  (setq muse-html-meta-content-type
        (concat muse-xhtml-meta-content-type "; charset="
                (muse-html-encoding))))

;;; Register the XHTML Publisher

(unless (assoc "xhtml" muse-publishing-styles)
  (muse-derive-style "xhtml" "html"
		     :suffix    'muse-xhtml-extension
		     :strings   'muse-xhtml-markup-strings
		     :header    'muse-xhtml-header
		     :footer    'muse-xhtml-footer
		     :before    'muse-xhtml-prepare-buffer))

(provide 'muse-xhtml)

;;; muse-xhtml.el ends here
