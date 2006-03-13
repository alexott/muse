;;; muse-backlink.el --- backlinks for Muse

;; Copyright (C) 2005, 2006 Free Software Foundation, Inc.

;; Author: Jim Ottaway <j.ottaway@lse.ac.uk>
;; Keywords:

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
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

;; Hierarchical backlink insertion into new muse pages.
;;
;; To add:
;;
;; (require 'muse-backlink)
;; (muse-backlink-install)
;;
;; To control what gets backlinked, modify
;; `muse-backlink-exclude-backlink-regexp' and
;; `muse-backlink-exclude-backlink-parent-regexp'.
;;
;; To stop backlinking temporarily:
;; (setq muse-backlink-create-backlinks nil)
;;
;; To remove the backlink functionality completely:
;;
;; (muse-backlink-remove)

;;; Contributors:

;;; Code:

(require 'muse)
(require 'muse-project)

(eval-when-compile (require 'muse-mode))

(defgroup muse-backlink nil
  "Hierarchical backlinking for Muse."
  :group 'muse)

(defcustom muse-backlink-create-backlinks t
  "When non-nil, create hierarchical backlinks in new Muse pages.
For control over which pages will receive backlinks, see
`muse-backlink-exclude-backlink-parent-regexp' and
`muse-backlink-exclude-backlink-regexp'."
  :type 'boolean
  :group 'muse-backlink)

(defcustom muse-backlink-avoid-bad-links t
  "When non-nil, avoid bad links when backlinking."
  :type 'boolean
  :group 'muse-backlink)

;; The default for exclusion stops backlinks from being added to and
;; from planner day pages.
(defcustom muse-backlink-exclude-backlink-parent-regexp
  "^[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]$"
  "Regular expression matching pages whose children should not have backlinks."
  :type 'regexp
  :group 'muse-backlink)

(defcustom muse-backlink-exclude-backlink-regexp
  "^[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]$"
  "Regular expression matching pages that should not have backlinks."
  :type 'regexp
  :group 'muse-backlink)

(defcustom muse-backlink-separator "/"
  "String that separates backlinks.
Should be something that will not appear as a substring in an explicit
link that has no description."
  :type 'string
  :group 'muse-backlink)

(defcustom muse-backlink-before-string "backlinks: "
  "String to come before the backlink list."
  :type 'string
  :group 'muse-backlink)

(defcustom muse-backlink-after-string ""
  "String to come after the backlink list."
  :type 'string
  :group 'muse-backlink)

(defcustom muse-backlink-separator "/"
  "String that separates backlinks.
Should be something that will not appear as a substring in an explicit
link that has no description."
  :type 'string
  :group 'muse-backlink)

(defcustom muse-backlink-regexp
  (concat "^"
          (regexp-quote muse-backlink-before-string)
          "\\("
          (regexp-quote muse-backlink-separator)
          ".+\\)"
          (regexp-quote muse-backlink-after-string))
  ;; Really, I want something like this, but I can't make it work:
  ;;   (concat "^\\("
  ;;           (regexp-quote muse-backlink-separator)
  ;;           "\\(?:"
  ;;           muse-explicit-link-regexp
  ;;           "\\)\\)+")
  "Regular expression to match backlinks in a buffer.
Match 1 is the list of backlinks without `muse-backlink-before-string'
and `muse-backlink-after-string'."
  :type 'regexp
  :group 'muse-backlink)

(defun muse-backlink-goto-insertion-point ()
  "Find the right place to add backlinks."
  (goto-char (point-min))
  (when (looking-at "\\(?:^#.+[ \t]*\n\\)+")
    (goto-char (match-end 0))))

(defun muse-backlink-get-current ()
  "Return a list of backlinks in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward muse-backlink-regexp nil t)
      (split-string (match-string 1)
                    (regexp-quote muse-backlink-separator) t))))

(defun muse-backlink-format-link-list (links)
  "Format the list of LINKS as backlinks."
  (concat muse-backlink-separator
          (mapconcat #'identity links muse-backlink-separator)))

(defun muse-backlink-insert-links (links)
  "Insert backlinks to LINKS into the current page.
LINKS is a list of links ordered by ancestry, with the parent as the
last element."
  (muse-backlink-goto-insertion-point)
  (insert muse-backlink-before-string
          (muse-backlink-format-link-list links)
          muse-backlink-after-string
          ;; Could have this in the after string, but they might get
          ;; deleted.
          "\n\n"))

(defun muse-backlink-unsaved-page-p (page project)
  "Return non-nil if PAGE is in PROJECT but has not been saved."
  (member
   page
   (mapcar
    #'(lambda (b)
        (with-current-buffer b
          (and (derived-mode-p 'muse-mode)
               (equal muse-current-project project)
               (not (muse-project-page-file
                     (muse-page-name)
                     muse-current-project))
               (muse-page-name))))
    (buffer-list))))

(defvar muse-backlink-links nil
  "Internal variable.
The links to insert in the forthcomingly visited muse page.")

(defvar muse-backlink-parent-buffer nil
  "Internal variable.
The parent buffer of the forthcomingly visited muse page.")

(defun muse-backlink-insert-hook-func ()
  "Insert backlinks into the current buffer and clean up."
  (unwind-protect
      (when muse-backlink-links
        (muse-backlink-insert-links muse-backlink-links)
        (when muse-backlink-avoid-bad-links
          (save-buffer)
          (when muse-backlink-parent-buffer
            (with-current-buffer muse-backlink-parent-buffer
              (font-lock-fontify-buffer)))))
    (setq muse-backlink-links nil
          muse-backlink-parent-buffer nil)
    (remove-hook 'muse-mode-hook #'muse-backlink-insert-hook-func)))

(defun muse-backlink-handle-link (link)
  "When appropriate, arrange for backlinks on visiting LINK."
  (when (and muse-backlink-create-backlinks
             (memq this-command
                   '(muse-follow-name-at-point muse-follow-name-at-mouse))
             (not muse-publishing-p)
             (not (and (boundp 'muse-colors-fontifying-p)
                       muse-colors-fontifying-p)))
    (require 'muse-mode)
    (setq
     muse-backlink-links
     (save-match-data
       (let* ((orig-link (or link (match-string 1)))
              (link (if (string-match "#" orig-link)
                        (substring orig-link 0 (match-beginning 0))
                      orig-link)))
         (unless
             (or (not muse-current-project)
                 (string-match muse-url-regexp orig-link)
                 (string-match muse-image-regexp orig-link)
                 (and (boundp 'muse-wiki-interwiki-regexp)
                      (string-match muse-wiki-interwiki-regexp
                                    orig-link))
                 ;; Don't add a backlink if the page already
                 ;; exists, whether it has been saved or not.
                 (or (muse-project-page-file link muse-current-project)
                     (muse-backlink-unsaved-page-p link muse-current-project))
                 (string-match muse-backlink-exclude-backlink-parent-regexp
                               (muse-page-name))
                 (string-match muse-backlink-exclude-backlink-regexp link))
           (add-hook 'muse-mode-hook #'muse-backlink-insert-hook-func)
           (when muse-backlink-avoid-bad-links
             (setq muse-backlink-parent-buffer (current-buffer))
             (unless (muse-project-page-file
                      (muse-page-name) muse-current-project)
               ;; It must be modified...
               (save-buffer)))
           (append (muse-backlink-get-current)
                   (list (muse-make-link (muse-page-name)))))))))
  ;; Make sure we always return nil
  nil)

(defun muse-backlink-install ()
  "Add backlinking functionality to muse-mode."
  (add-to-list 'muse-explicit-link-functions #'muse-backlink-handle-link))

(defun muse-backlink-remove ()
  "Remove backlinking functionality from muse-mode."
  (setq muse-explicit-link-functions
        (delq #'muse-backlink-handle-link muse-explicit-link-functions)))

(provide 'muse-backlink)
;;; muse-backlink.el ends here
