;; muse-mathjax.el --- generate MathJaX code from inline LaTeX code

;; Copyright (C) 2011
;;   Free Software Foundation, Inc.

;; Author: Leo Butler <leo.butler@member.fsf.org>
;; Created: 19-Nov-2011

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

;;; Code

(require 'muse-publish)
(require 'muse-html)

(defgroup muse-mathjax nil
  "Support for publishing LaTeX formulas as MathJaX."
  :group 'muse-publish)

(defcustom muse-mathjax-src-url
  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  "URL to source the MathJaX Javascript code."
  :type 'string
  :group 'muse-mathjax)

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
itself. See `muse-mathjax-html-header'."
  :type 'string
  :group 'muse-mathjax)

(defvar muse-mathjax-html-header
  (muse-replace-regexp-in-string
   "<head>"
   (concat "<head>\n"
	   "<script src=\"<lisp>muse-mathjax-src-url</lisp>\" type=\"text/javascript\"></script>\n"
	   "<script type=\"text/x-mathjax-config\">
             MathJax.Hub.Config({
	     <lisp>muse-mathjax-configuration</lisp>
             });
            </script>")
   muse-html-header)
  "Header for HTML files generated with the mathjax-html
style. See `muse-mathjax-configuration' and
`muse-mathjax-src-url'.")

(defvar muse-mathjax-xhtml-header
  (muse-replace-regexp-in-string
   "<head>"
   (concat "<head>\n"
	   "<script src=\"<lisp>muse-mathjax-src-url</lisp>\" type=\"text/javascript\"></script>\n"
	   "<script type=\"text/x-mathjax-config\">
             MathJax.Hub.Config({
	     <lisp>muse-mathjax-configuration</lisp>
             });
            </script>")
   muse-xhtml-header)
  "Header for XHTML files generated with the mathjax-xhtml
style. See `muse-mathjax-configuration' and
`muse-mathjax-src-url'.")


(muse-derive-style "mathjax-html" "html"
                   :header    'muse-mathjax-html-header)
(muse-derive-style "mathjax-xhtml" "xhtml"
                   :header    'muse-mathjax-xhtml-header)

(defun muse-mathjax ())

(provide 'muse-mathjax)

;;; muse-mathjax.el ends here
