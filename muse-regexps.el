;;; muse-regexps.el --- Define regexps used by Muse

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: muse-regexps.el
;; Version: 3.00 ALPHA
;; Date: 2004-12-29
;; Keywords: hypermedia
;; Author: Michael Olson (mwolson AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
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

;; This file is the part of the Muse project that describes regexps
;; that are used throughout the project.

;;;_ + Startup

;; To be written.

;;;_ + Usage

;;;_ + Contributors

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Regular Expressions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun muse-extreg-usable-p ()
  "Return non-nil if extended character classes can be used,
nil otherwise.

This is used when deciding the initial values of the muse-regexp
options."
  (save-match-data
    (string-match "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"
                  emacs-version)
    (cond ((featurep 'xemacs) nil)             ; unusable on XEmacs
          ((> emacs-major-version 21) t)       ; usable if > 21
          ((< emacs-major-version 21) nil)
          ((< emacs-minor-version 3) nil)
          ;; don't trust the 21.3.1 release or its predecessors
          ((> (string-to-int (match-string 1 emacs-version)) 1) t)
          (t nil))))

(defgroup muse-regexp nil
  "Options relating to regular expressions as used in publishing
and syntax highlighting."
  :group 'muse)

(defcustom muse-regexp-blank
  (if (muse-extreg-usable-p)
      "[:blank:]"
    " \t")
  "Regexp to use in place of \"[:blank:]\".
This should be something that matches spaces and tabs.

It is like a regexp, but should be embeddable inside brackets.
Muse will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:blank:]" " \t")
  :group 'muse-regexp)

(defcustom muse-regexp-space
  (if (muse-extreg-usable-p)
      "[:space:]"
    " \t\n")
  "Regexp to use in place of \"[:space:]\".
This should be something that matches spaces, tabs, and newlines.

It is like a regexp, but should be embeddable inside brackets.
muse will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:space:]" " \t\n")
  :group 'muse-regexp)

(defcustom muse-regexp-alnum
  (if (muse-extreg-usable-p)
      "[:alnum:]"
    "A-Za-z0-9")
  "Regexp to use in place of \"[:alnum:]\".
This should be something that matches all letters and numbers.

It is like a regexp, but should be embeddable inside brackets.
muse will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:alnum:]" "A-Za-z0-9")
  :group 'muse-regexp)

(defcustom muse-regexp-lower
  (if (muse-extreg-usable-p)
      "[:lower:]"
    "a-z")
  "Regexp to use in place of \"[:lower:]\".
This should match all lowercase characters.

It is like a regexp, but should be embeddable inside brackets.
muse will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:lower:]" "a-z")
  :group 'muse-regexp)

(defcustom muse-regexp-upper
  (if (muse-extreg-usable-p)
      "[:upper:]"
    "A-Z")
  "Regexp to use in place of \"[:upper:]\".
This should match all uppercase characters.

It is like a regexp, but should be embeddable inside brackets.
muse will detect the appropriate value correctly most of
the time."
  :type 'string
  :options '("[:upper:]" "A-Z")
  :group 'muse-regexp)

(provide 'muse-regexps)
;;; muse-regexps.el ends here
