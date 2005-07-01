;;; muse-wiki.el --- wiki features for muse

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'muse-regexps)
(require 'muse-mode)

(defgroup muse-wiki nil
  "Options controlling the behavior of Emacs Muse Wiki features."
  :group 'muse-mode)

(defcustom muse-wiki-wikiword-regexp
  (concat "\\<\\(\\(?:[" muse-regexp-upper
          "][" muse-regexp-lower "]+\\)\\(?:["
          muse-regexp-upper "][" muse-regexp-lower "]+\\)+\\)\\>")
  "Regexp used to match WikiWords"
  :type 'regexp
  :group 'muse-wiki)

(defcustom muse-wiki-interwiki-alist
  '(("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/"))
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

Referring to [[JohnWiki::EmacsModules]] then really means:

  http://alice.dynodns.net/wiki?EmacsModules

If a function is used for the replacement text, you can get creative
depending on what the tag is.  Tags may contain any alphabetic
character, any number, % or _.  If you need other special characters,
use % to specify the hex code, as in %2E.  All browsers should support
this."
  :type '(repeat (cons (string :tag "WikiName")
                       (choice (string :tag "URL") function)))
  :group 'muse-wiki)

(defvar muse-wiki-interwiki-regexp
  (concat "\\<\\(" (mapconcat 'car muse-wiki-interwiki-alist "\\|")
          "\\)\\(?:\\(?:#\\|::\\)\\(\\sw+\\)\\)?\\>"))

(defun muse-wiki-wikiword-at-point ()
  (and (looking-at muse-wiki-wikiword-regexp)
       (match-string 1)))

(defun muse-wiki-interwiki-at-point ()
  (muse-wiki-interwiki-handle))

(defun muse-wiki-interwiki-expand (url)
  (setq url (or (muse-wiki-interwiki-handle url)
                url))
  (muse-publish-read-only url))

(defun muse-wiki-interwiki-handle (&optional url)
  (save-match-data
    (when (if url
              (string-match muse-wiki-interwiki-regexp url)
            (looking-at muse-wiki-interwiki-regexp))
      (let ((subst (cdr (assoc (match-string 1 url)
                               muse-wiki-interwiki-alist)))
            (word (match-string 2 url)))
        (if (functionp subst)
            (funcall subst word)
          (concat subst word))))))

(eval-after-load 'muse-colors
  '(progn
     (defun muse-wiki-colors-wikiword ()
       ;; remove flyspell overlays
       (when (fboundp 'flyspell-unhighlight-at)
         (let ((cur (match-beginning 0)))
           (while (> (match-end 0) cur)
             (flyspell-unhighlight-at cur)
             (setq cur (1+ cur)))))
       (let* ((link (match-string-no-properties 1))
              (props (muse-link-properties
                      (match-string-no-properties 1)
                      (muse-link-face (match-string 1)))))
         (add-text-properties (match-beginning 1) (match-end 0) props)))

     (add-to-list 'muse-colors-markup
                  '(muse-wiki-interwiki-regexp t muse-wiki-colors-wikiword)
                  t)
     (add-to-list 'muse-colors-markup
                  '(muse-wiki-wikiword-regexp t muse-wiki-colors-wikiword)
                  t)

     (muse-configure-highlighting 'muse-colors-markup muse-colors-markup)))

(eval-after-load 'muse-publish
  '(progn
     (add-to-list 'muse-publish-markup-regexps
                  '(3100 muse-wiki-interwiki-regexp 0 url)
                  t)
     (add-to-list 'muse-publish-markup-regexps
                  '(3200 muse-wiki-wikiword-regexp 0 url)
                  t)
     (add-to-list 'muse-publish-url-transforms
                  'muse-wiki-interwiki-expand)))

(eval-after-load 'muse-mode
  '(progn
     (add-to-list 'muse-mode-link-functions 'muse-wiki-interwiki-at-point t)
     (add-to-list 'muse-mode-link-functions 'muse-wiki-wikiword-at-point t)
     (add-to-list 'muse-mode-handler-functions 'muse-wiki-interwiki-handle)))

(provide 'muse-wiki)
;;; muse-wiki.el ends here
