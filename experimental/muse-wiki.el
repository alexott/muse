;;; muse-blosxom.el --- Publish a document tree for serving by (py)Blosxom

;; Copyright (C) 2004, 2005  Free Software Foundation, Inc.

;; Date: Wed, 13 June 2005
;; Author: Michael Olson (mwolson AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)

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

;; This file is in experimental status.
;;
;; It was a collection of routines that were removed from Muse because
;; they didn't fit neatly into its paradigm.  Eventually it is hoped
;; that this will provide bare WikiName and interwiki support.
;;
;; Yann Hodique's implementation of WikiName highlighting will
;; probably be merged into this file, or something to that effect.

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Wiki Publishing and Highlighting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This was a removed rule from `muse-publish-markup-regexps'.
;;
;;   ["''''" 0 ""]

(defcustom muse-interwiki-names
  '(("GnuEmacs" . "http://www.gnu.org/software/emacs/emacs.html")
    ("TheEmacsWiki" .
     (lambda (tag)
       (concat "http://www.emacswiki.org/cgi-bin/wiki.pl?"
               (or tag "SiteMap"))))
    ("MeatballWiki" .
     (lambda (tag)
       (concat "http://www.usemod.com/cgi-bin/mb.pl?"
               (or tag "MeatballWiki")))))
  "A table of WikiNames that refer to external entities.
The format of this table is an alist, or series of cons cells.
Each cons cell must be of the form:

  (WIKINAME . STRING-OR-FUNCTION)

The second part of the cons cell may either be a STRING, which in most
cases should be a URL, or a FUNCTION.  If a function, it will be
called with one argument: the tag applied to the Interwiki name, or
nil if no tag was used.  If the cdr was a STRING and a tag is used,
the tag is simply appended.

Here are some examples:

  (\"JohnWiki\" . \"http://alice.dynodns.net/wiki?\")

Referring to [[JohnWiki#EmacsModules]] then really means:

  http://alice.dynodns.net/wiki?EmacsModules

If a function is used for the replacement text, you can get creative
depending on what the tag is.  Tags may contain any alphabetic
character, any number, % or _.  If you need other special characters,
use % to specify the hex code, as in %2E.  All browsers should support
this."
  :type '(repeat (cons (string :tag "WikiName")
                       (choice (string :tag "URL") function)))
  :group 'muse)

(defcustom muse-publish-small-title-words
  '("the" "and" "at" "on" "of" "for" "in" "an" "a")
  "Strings that should be downcased in a page title."
  :type '(repeat string)
  :group 'muse-publish)

(defun muse-publish-pretty-title (title)
  "Return a pretty version of the given TITLE."
  (save-match-data
    (let ((case-fold-search nil))
      (while (string-match "\\([A-Za-z]\\)\\([A-Z0-9]\\)" title)
        (setq title (replace-match "\\1 \\2" t nil title)))
      (let* ((words (split-string title))
             (w (cdr words)))
        (while w
          (if (member (downcase (car w))
                      muse-publish-small-title-words)
              (setcar w (downcase (car w))))
          (setq w (cdr w)))
        (mapconcat 'identity words " ")))))

(defun muse-publish-nop-tag (beg end)
  (when (looking-at "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+")
    (goto-char (match-end 0))
    (muse-publish-mark-read-only beg (point))))

;; regenerate the index if any pages were published
(with-current-buffer (muse-generate-index t t)
  (muse-replace-markup muse-publish-index-page)
  (let ((backup-inhibited t))
    (write-file (muse-published-file muse-publish-index-page)))
  (kill-buffer (current-buffer))
  (message "All pages in %s have been published." (car project)))


(defun muse-count-chars (string char)
  (let ((i 0)
        (l (length string))
        (count 0))
    (while (< i l)
      (if (eq char (aref string i))
          (setq count (1+ count)))
      (setq i (1+ i)))
    count))

(defun muse-set-sym-and-url-regexp (sym value)
  (setq muse-url-or-name-regexp
        (concat "\\("
                (if (eq sym 'muse-name-regexp)
                    value
                  muse-name-regexp) "\\|"
                  (if (eq sym 'muse-name-regexp)
                      (if (boundp 'muse-url-regexp)
                          muse-url-regexp
                        "")
                    value) "\\)")
        muse-url-or-name-regexp-group-count
        (- (muse-count-chars
            muse-url-or-name-regexp ?\() 2))
  (set sym value))

(defcustom muse-name-regexp
  (concat "\\(" muse-extended-link-regexp "\\|"
          "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\(#\\S-+[A-Za-z0-9]\\)?" "\\)")
  "Regexp used to match WikiNames."
  :type 'regexp
  :set 'muse-set-sym-and-url-regexp
  :group 'muse)

(defvar muse-url-or-name-regexp nil
  "Matches either a Wiki link or a URL.  This variable is auto-generated.")

(defvar muse-url-or-name-regexp-group-count nil
  "Matches either a Wiki link or a URL.  This variable is auto-generated.")

;; Utility functions to extract parts of a Wiki name

(defvar muse-serving-p nil
  "Non-nil when Muse is serving a page directly.")

(defsubst muse-transform-name (name)
  "Transform NAME as per `muse-publishing-transforms', returning NAME"
  (save-match-data
    (mapc (function
           (lambda (elt)
             (let ((reg (car elt))
                   (rep (cdr elt)))
               (when (string-match reg name)
                 (setq name (replace-match rep t nil name))))))
          muse-publishing-transforms)
    name))

(defsubst muse-published-name (name &optional current)
  "Return the externally visible NAME for a wiki page, possibly transformed
  via `muse-publishing-transforms'. If CURRENT is provided, convert any
  path to be relative to it"
  (muse-transform-name
   (progn
     (when current
       (setq name (file-relative-name name
                                      (file-name-directory
                                       (muse-transform-name current)))))
     (concat (if muse-serving-p
                 (unless (string-match "\\?" name) "wiki?")
               muse-publishing-file-prefix)
             name
             (unless muse-serving-p
               muse-publishing-file-suffix)))))

(defsubst muse-published-file (&optional file)
  "Return the filename of the published file. Since this is based on the
  published-name, it will be filtered through
  `muse-publishing-transforms'"
  (expand-file-name (muse-published-name (muse-page-name
                                          file))
                    muse-publishing-directory))

(defcustom muse-publishing-transforms nil
  "A list of cons cells mapping regexps to replacements, which is
applied when generating the published name from the wiki file
name. The replacements run in order so you can chain them
together.

An example is how I publish the Muse documentation.  The Muse
homepage is in a file called EmacsWiki.  With the following
settings I can publish directly to my webserver via tramp (the
first rule catches 'WikiMarkup' for instance):

  (setq muse-publishing-directory \"/webserver:/var/www/\")
  (setq muse-publishing-transforms
        '((\".*Wiki.*\" . \"emacs/wiki/\\&\")
         (\"EmacsWiki\\|WelcomePage\" . \"index\")))

Then when trying to publish a page EmacsWiki:

  (muse-published-file \"EmacsWiki\")

You get:

\"/webserver:/var/www/emacs/wiki/index.html\""
  :type '(repeat
          (cons
           (regexp :tag "String to match")
           (string :tag "Replacement string")))
  :group 'muse-publish)

(defsubst muse-wiki-url-p (name)
  "Return non-nil if NAME is a URL."
  (save-match-data
    (and name
         (string-match muse-url-regexp name))))

(defun muse-wiki-visible-name (wiki-name)
  "Return the visible part of a Wiki link.
This only really means something if [[extended][links]] are involved."
  (save-match-data
    (let ((name wiki-name))
      (if (string-match muse-extended-link-regexp name)
          (if (match-string 2 name)
              (setq name (match-string 3 name))
            (setq name (match-string 1 name))))
      (if (and (not (muse-wiki-url-p name))
               (string-match "#" name))
          (if (= 0 (match-beginning 0))
              (setq name (muse-page-name))
            (let ((base (substring name 0 (match-beginning 0))))
              (if (assoc base muse-interwiki-names)
                  (setq name (concat (substring name 0 (match-beginning 0))
                                     ":" (substring name (match-end 0))))
                (setq name base)))))
      name)))

(defun muse-wiki-tag (wiki-name)
  (save-match-data
    (if (string-match "#" wiki-name)
        (substring wiki-name (match-end 0)))))

(defun muse-wiki-link-target (wiki-name)
  "Return the target of a Wiki link.  This might include anchor tags."
  (save-match-data
    (let ((name wiki-name) lookup)
      (if (string-match "^\\[\\[\\([^]]+\\)\\]" name)
          (setq name (match-string 1 name)))
      (if (and muse-interwiki-names
               (string-match "\\`\\([^#]+\\)\\(#\\(.+\\)\\)?\\'" name)
               (setq lookup (assoc (match-string 1 name)
                                   muse-interwiki-names)))
          (let ((tag (match-string 3 name))
                (target (cdr lookup)))
            (if (stringp target)
                (setq name (concat target tag))
              (setq name (funcall target tag))))
        (if (and (> (length name) 0)
                 (eq (aref name 0) ?#))
            (setq name (concat (muse-page-name) name))))
      name)))

(defun muse-wiki-base (wiki-name)
  "Find the WikiName or URL mentioned by a Wiki link.
This means without tags, in the case of a WikiName."
  (save-match-data
    (let ((file (muse-wiki-link-target wiki-name)))
      (if (muse-wiki-url-p file)
          file
        (if (string-match "#" file)
            (substring file 0 (match-beginning 0))
          file)))))
