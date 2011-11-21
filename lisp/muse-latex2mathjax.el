;; muse-latex2mathjax.el --- generate MathJaX code from inline LaTeX code

;; Copyright (C) 2011
;;   Free Software Foundation, Inc.

;; Author: Leo Butler <mwolson@gnu.org>
;; Created: 19-Nov-2011

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
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

;;; MathJaX (http://www.mathjax.org/) is a Javascript application that
;;; converts LaTeX code to MathML. The syntax is needs is \( ... \)
;;; for inline math and \[ ... \] for centered math.
;;;
;;; This code provides the conversion of Muse tags <latex> and <math>
;;; for use with MathJaX.

;;; To do

;;; To do

;;; Code

(require 'muse-publish)
(require 'muse-html)

(defgroup muse-latex2mathjax nil
  "Publishing LaTeX formulas as MathJaX."
  :group 'muse-publish)

(defgroup muse-mathjax-html nil
  "Support for publishing LaTeX formulas as MathJaX."
  :group 'muse-publish)

(defcustom muse-latex2mathjax-centered-begin-delimiters
  '(("context"	. "\\startformula ")
    (nil	. "\\["))
  "An assoc list of the opening delimiters for centered math mode. The
CAR is the style and the CDR is the delimiter. NIL is the default, and
all options after the default are ignored."
  :group 'muse-latex2mathjax
  :type '(alist :key-type sexp :value-type string))

(defcustom muse-latex2mathjax-centered-end-delimiters
  '(("context"	. "\\endformula ")
    (nil	. "\\]"))
  "An assoc list of the opening delimiters for centered math mode. The
CAR is the style and the CDR is the delimiter. NIL is the default, and
all options after the default are ignored."
  :group 'muse-latex2mathjax
  :type '(alist :key-type sexp :value-type string))

(defcustom muse-latex2mathjax-inline-begin-delimiters
  '(("mathjax-html"	. " \\( ")
    (nil		. "$"))
  "An assoc list of the opening delimiters for inline math mode. The
CAR is the style and the CDR is the delimiter. NIL is the default, and
all options after the default are ignored."
  :group 'muse-latex2mathjax
  :type '(alist :key-type sexp :value-type string))

(defcustom muse-latex2mathjax-inline-end-delimiters
  '(("mathjax-html"	. " \\) ")
    (nil		. "$"))
  "An assoc list of the opening delimiters for inline math mode. The
CAR is the style and the CDR is the delimiter. NIL is the default, and
all options after the default are ignored."
  :group 'muse-latex2mathjax
  :type '(alist :key-type sexp :value-type string))


(defcustom muse-mathjax-src-url
  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  "URL to source the MathJaX Javascript code."
  :type 'string
  :group 'muse-mathjax-html)

(defcustom muse-mathjax-configuration
  "
  tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']],
            displayMath: [['$$','$$'], ['\\\\[','\\\\]']]}
  "
  "Configuration of the MathJaX processer. See
http://www.mathjax.org/docs/1.1/options/tex2jax.html#configure-tex2jax
for information.

This configuration information is inserted into the <head>
section of the HTML page before the link to the MathJaX page
itself."
  :type 'string
  :group 'muse-mathjax-html)

(defcustom muse-mathjax-html-header
  (muse-replace-regexp-in-string
   "<head>"
   (concat "<head>\n"
	   "<script type=\"text/x-mathjax-config\">
             MathJax.Hub.Config({
	     <lisp>muse-mathjax-configuration</lisp>
             });
            </script>
	    <script src=\"<lisp>muse-mathjax-src-url</lisp>\" type=\"text/javascript\"></script>\n")
   muse-html-header)
  "Header for HTML files generated with the mathjax-html
style. See `muse-mathjax-configuration' and
`muse-mathjax-src-url'."
  :type 'string
  :group 'muse-mathjax-html)


(defun muse-publish-latex-delimiters (centered begin)
  "Queries the variables
`muse-latex2mathjax-inline-begin-delimiters',
`muse-latex2mathjax-inline-end-delimiters',
`muse-latex2mathjax-centered-begin-delimiters' and
`muse-latex2mathjax-centered-end-delimiters' for the correct
delimiter to insert."
  (let* ((delimiter-alist (cond (centered
				 (if begin muse-latex2mathjax-centered-begin-delimiters
				   muse-latex2mathjax-centered-end-delimiters))
				(t
				 (if begin muse-latex2mathjax-inline-begin-delimiters
				   muse-latex2mathjax-inline-end-delimiters))))
	 delim style)
    (while (and
	    (setq style			(caar delimiter-alist))
	    (null (setq delim		(if (muse-style-derived-p style) (cdar delimiter-alist))))
	    (setq delimiter-alist	(cdr delimiter-alist))))
    (or delim (cdr (assoc nil delimiter-alist)))))
  
(defvar muse-publish-latex-tag-as-is '("latex" "context" "mathjax-html")
  "Styles which should publish La/TeX source as is.")

(defun muse-publish-latex-tag-as-is ()
  "Query the variable `muse-publish-latex-tag-as-is' if STYLE should
publish <latex> tags as literals."
  (memq t
	(mapcar (lambda(x) (muse-style-derived-p x)) muse-publish-latex-tag-as-is)))


(muse-derive-style "mathjax-html" "html"
                   :header    'muse-mathjax-html-header)

(defun muse-latex2mathjax ())

(provide 'muse-latex2mathjax)

;;; muse-latex2mathjax.el ends here
