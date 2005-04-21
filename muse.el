;;; muse.el --- An authoring and publishing tool for Emacs.

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: muse.el
;; Version: 3.00 ALPHA
;; Date: Thu 11-Mar-2004
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: An authoring and publishing tool for Emacs
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

;; Muse is a tool for easily authoring and publishing documents.  It
;; allows for rapid prototyping of hyperlinked text, which may then be
;; exported to multiple output formats -- such as HTML, LaTeX,
;; Texinfo, etc.

;; The markup rules used by Muse are intended to be very friendly to
;; people familiar with Emacs.  See the included README file for more
;; information.

;;; Code:

(require 'muse-regexps)

(defvar muse-version "3.00 ALPHA"
  "The version of Muse currently loaded")

(defun muse-version ()
  "Display the version of Muse that is currently loaded."
  (interactive)
  (message muse-version))

(defgroup muse nil
  "Options controlling the behaviour of Muse.
The markup used by Muse is intended to be very friendly to people
familiar with Emacs."
  :group 'hypermedia)

(defcustom muse-tag-regexp
  (concat "<\\([^/" muse-regexp-space "][^" muse-regexp-space
          "</>]*\\)\\(\\s-+[^<>]+[^</>]\\)?\\(/\\)?>")
  "A regexp used to find XML-style tags within a buffer when publishing.
Group 1 should be the tag name, group 2 the properties, and group
3 the optional immediate ending slash."
  :type 'regexp
  :group 'muse)

(defcustom muse-link-regexp
  "\\[\\[\\([^][\t\n]+\\)\\]\\(?:\\[\\([^][\n]+\\)\\]\\)?\\]"
  "Regexp used to match [[target][description]] links.
Paren group 1 must match the URL, and paren group 2 the description."
  :type 'regexp
  :group 'muse)

(defcustom muse-url-regexp
  (concat "\\<\\(?:https?:/?/?\\|ftp:/?/?\\|gopher://\\|"
	  "telnet://\\|wais://\\|file:/\\|s?news:\\|"
	  "mailto:\\)"
	  "[^]  \n \"'()<>[^`{}]*[^]    \n \"'()<>[^`{}.,;]+")
  "A regexp used to match URLs within a Muse page."
  :type 'regexp
  :group 'muse)

(defcustom muse-file-regexp
  "[/?]\\|\\.\\(html?\\|pdf\\|mp3\\|el\\|zip\\|txt\\|tar\\)\\(\\.\\(gz\\|bz2\\)\\)?\\'"
  "A link matching this regexp will be regarded as a link to a file."
  :type 'regexp
  :group 'muse-publish)

(defcustom muse-image-regexp
  "\\.\\(eps\\|gif\\|jp\\(e?g\\)\\|p\\(bm\\|ng\\)\\|tiff\\|x\\([bp]m\\)\\)\\'"
  "A link matching this regexp will be published inline as an image.
For example:

  [[./wife.jpg][A picture of my wife]]

If you omit the description, the alt tag of the resulting HTML
buffer will be the name of the file."
  :type 'regexp
  :group 'muse-publish)

(defcustom muse-ignored-extensions-regexp
  "\\.\\(bz2\\|gz\\|[Zz]\\)\\'"
  "A regexp of extensions to omit from the ending of a Muse page name."
  :type 'string
  :group 'muse)

(defvar muse-under-windows-p (memq system-type '(ms-dos windows-nt)))

;;; Return an list of known wiki names and the files they represent.

(defsubst muse-delete-file-if-exists (file)
  (when (file-exists-p file)
    (delete-file file)
    (message "Removed %s" file)))

(defsubst muse-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun muse-page-name (&optional name)
  "Return the canonical form of a Muse page name.
All this means is that certain extensions, like .gz, are removed."
  (save-match-data
    (unless name
      (setq name buffer-file-name))
    (if name
	(let ((page (file-name-nondirectory name)))
	  (if (string-match muse-ignored-extensions-regexp page)
	      (replace-match "" t t page)
	    page)))))

(defun muse-eval-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (let ((object (eval (read form))))
      (cond
       ((stringp object) object)
       ((and (listp object)
	     (not (eq object nil)))
	(let ((string (pp-to-string object)))
	  (substring string 0 (1- (length string)))))
       ((numberp object)
	(number-to-string object))
       ((eq object nil) "")
       (t
	(pp-to-string object))))))

;; The following code was extracted from cl

(defun muse-const-expr-p (x)
  (cond ((consp x)
	 (or (eq (car x) 'quote)
	     (and (memq (car x) '(function function*))
		  (or (symbolp (nth 1 x))
		      (and (eq (and (consp (nth 1 x))
				    (car (nth 1 x))) 'lambda) 'func)))))
	((symbolp x) (and (memq x '(nil t)) t))
	(t t)))

(put 'muse-assertion-failed 'error-conditions '(error))
(put 'muse-assertion-failed 'error-message "Assertion failed")

(defun muse-list* (arg &rest rest)
  "Return a new list with specified args as elements, cons'd to last arg.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'."
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defmacro muse-assert (form &optional show-args string &rest args)
  "Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used."
  (let ((sargs
	 (and show-args
	      (delq nil (mapcar
			 (function
			  (lambda (x)
			    (and (not (muse-const-expr-p x)) x)))
			 (cdr form))))))
    (list 'progn
	  (list 'or form
		(if string
		    (muse-list* 'error string (append sargs args))
		  (list 'signal '(quote muse-assertion-failed)
			(muse-list* 'list (list 'quote form) sargs))))
	  nil)))

(provide 'muse)

;;; muse.el ends here
