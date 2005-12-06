;;; muse-registry.el --- URL registry for Muse

;; Copyright (C) 2005  Bastien Guerry
;; Time-stamp: <2005-11-24 23:09:53 guerry>
;;
;; Author: bzg@altern.org
;; Version: $Id: muse-registry.el,v 0.3 2005/11/24 23:10:10 guerry Exp $
;; Keywords: planner muse registry

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This module provides a way to keep track of all the URLs in your
;; projects, and to list them depending on the current buffer.  The
;; URLs are defined in `muse-url-protocols' - it does NOT include
;; wikiwords (for now).
;;
;; If a URL has been created by `planner-create-task-from-buffer',
;; going to that buffer and calling `muse-registry-show' will show you
;; where planner put the URL.
;;
;; Say for example that you created a task from an e-mail.  Go to that
;; e-mail and call `muse-registry-show': it will open a new buffer
;; displaying the files (in a muse links format) where a link to this
;; e-mail has been added.

;;; Getting Started:

;; Put this in your init file:
;;
;; (require 'muse-registry)
;; (muse-registry-initialize)
;;
;; You MUST put it after your planner config have been loaded.
;;
;; If you want the registry to be updated each time you save a Muse
;; file, add this:
;;
;; (muse-registry-insinuate)
;;
;; If you don't want to update the registry each time a file is
;; written, you can do it manually with `muse-registry-update': it
;; will update the registry for saved muse/planner buffers only.
;;
;; There's no default `define-key' for `muse-registry-show' because
;; it's not bounded to one particular mode.  You can bound it to
;; whatever you want.

;;; Todo:

;; 1) Better windows manipulations
;; 2) Wiki links support

;;; Problems:
;;
;; If you're using this with Planner, the default value of
;; `planner-bibtex-separator' must be changed from ":" to something
;; else.
;;
;; (setq planner-bibtex-separator "#")
;;
;; "#" as a separator enables you to perform fuzzy-matching on bibtex
;; URLs as well.

;;; History:
;;
;; 2000.11.22 - new release.
;; 2005.11.18 - first release.

;;; Code:

(provide 'muse-registry)

;;;_* Prerequisites

;;(require 'cl)
(require 'muse)
(require 'planner)

;;;_* Options

(defgroup muse-registry nil
  "A registry for muse and planner."
  :prefix "muse-registry-"
  :group 'muse)

;; You can setq this var to what do you like.
(defcustom muse-registry-file
  (concat (getenv "HOME") "/.muse-registry.el")
  "The registry file."
  :type 'string
  :group 'muse-registry)

(defcustom muse-registry-min-keyword-size 3
  "Minimum size for keywords."
  :type 'integer
  :group 'muse-registry)

(defcustom muse-registry-max-keyword-size 10
  "Maximum size for keywords."
  :type 'integer
  :group 'muse-registry)

(defcustom muse-registry-max-number-of-keywords 3
  "Maximum number of keywords."
  :type 'integer
  :group 'muse-registry)

(defcustom muse-registry-ignore-keywords
  '("E-Mail" "from" "www")
  "A list of ignored keywords.")

(defcustom muse-registry-show-level 0
  "Level for `muse-registry-show'.
0 means that this function shows only exact matches.
1 means that this function also shows descriptive matches.
2 (or more) means that this function also shows fuzzy matches."
  :type 'boolean
  :group 'muse-registry)

;;;_* Other variables and constants

(defvar muse-registry-alist nil
  "An alist containing the muse registry.")

(defconst muse-registry-url-regexp
  (concat "\\(" (mapconcat 'car muse-url-protocols "\\|") "\\)"
          "[^][[:space:]\"'()^`{}]*[^][[:space:]\"'()^`{}.,;\n]+")
  "A regexp that matches muse URL links.")

(defconst muse-registry-link-regexp
  (concat "\\[\\[\\(" muse-registry-url-regexp
          "\\)\\]\\[\\([^][]+\\)\\]\\]")
  "A regexp that matches muse explicit links.")

(defconst muse-registry-url-or-link-regexp
  (concat "\\(" muse-registry-url-regexp "\\)\\|"
          muse-registry-link-regexp)
  "A regexp that matches both muse URL and explicit links.
The link is returned by `match-string' 3 or 1.
The protocol is returned bu `match-string' 4 or 2.
The description is returned by `match-string' 5")

;;;_* Core code

;;;###autoload
(defun muse-registry-initialize (&optional from-scratch)
  "Set `muse-registry-alist' from `muse-registry-file'.
If `muse-registry-file' doesn't exist, create it.
If FROM-SCRATCH is non-nil, make the registry from scratch."
  (interactive "P")
  (if (or (not (file-exists-p muse-registry-file))
          from-scratch)
      (muse-registry-make-new-registry)
    (muse-registry-read-registry))
  (message "Muse registry initialized"))

(defun muse-registry-update nil
  "Update the registry from the current buffer."
  (interactive)
  (let* ((from-file (buffer-file-name))
         (new-entries
          (muse-registry-new-entries from-file)))
    (muse-registry-update-registry from-file new-entries))
  (with-temp-buffer
    (find-file muse-registry-file)
    (eval-buffer)
    (kill-buffer (current-buffer))))

(defun muse-registry-insinuate nil
  "Call `muse-registry-update' after saving in muse/planner modes.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'planner-mode-hook
            (lambda nil
              (make-local-hook 'after-save-hook)
              (add-hook 'after-save-hook 'muse-registry-update t t)))
  (add-hook 'muse-mode-hook
            (lambda nil
              (make-local-hook 'after-save-hook)
              (add-hook 'after-save-hook 'muse-registry-update t t))))

(defun muse-registry-show (&optional level)
  "Show entries at LEVEL.
See `muse-registry-show-level' for details."
  (interactive "p")
  (let ((annot (run-hook-with-args-until-success
                'planner-annotation-functions))
        (level (or level muse-registry-show-level)))
    (if (not annot)
        (message "Annotation is not supported for this buffer")
      (progn
        (let ((entries (muse-registry-get-entries annot level)))
          (if (not entries)
              (message
               (format "No match (level %d) for \"%s\"" level
                       (progn (string-match
                               muse-registry-url-or-link-regexp annot)
                              (match-string 5 annot))))
            (progn
              (delete-other-windows)
              (switch-to-buffer-other-window
               (set-buffer (get-buffer-create "*Muse registry*")))
              (erase-buffer)
              (mapc (lambda (elem)
                      (mapc (lambda (entry) (insert entry)) elem)
                      (when elem (insert "\n"))) entries)
              (muse-mode))))))))

(defun muse-registry-create nil
  "Create `muse-registry-file'."
  (let ((items muse-registry-alist)
        item)
    (with-temp-buffer
      (find-file muse-registry-file)
      (erase-buffer)
      (insert
       (with-output-to-string
         (princ ";; -*- emacs-lisp -*-\n")
         (princ ";; Muse registry\n;; What are you doing here?\n\n")
         (princ "(setq muse-registry-alist\n'(\n")
         (while items
           (when (setq item (pop items))
             (prin1 item)
             (princ "\n")))
         (princ "))\n")))
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Muse registry created"))

(defun muse-registry-entry-output (entry)
  "Make an output string for ENTRY."
  (concat " - [[pos://" (car entry)
          "#" (second entry) "]["
          (muse-registry-get-project-name (car entry))
          ": " (file-name-nondirectory (car entry))
          "]] - [[" (third entry) "][" (fourth entry) "]]\n"))

(defun muse-registry-get-project-name (file)
  "Get project name for FILE."
  (let ((file1 (directory-file-name
                (file-name-directory file))))
    (replace-regexp-in-string "/?[^/]+/" "" file1)))

(defun muse-registry-read-registry nil
  "Set `muse-registry-alist' from `muse-registry-file'."
  (with-temp-buffer
    (find-file muse-registry-file)
    (eval-buffer)
    (kill-buffer (current-buffer))))

(defun muse-registry-update-registry (from-file new-entries)
  "Update the registry FROM-FILE with NEW-ENTRIES."
  (with-temp-buffer
    (find-file muse-registry-file)
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^(\"" from-file) nil t)
      (delete-region (muse-line-beginning-position)
                     (muse-line-end-position)))
    (goto-char (point-min))
    (re-search-forward "^(\"" nil t)
    (goto-char (match-beginning 0))
    (mapc (lambda (elem)
            (insert
             (with-output-to-string (prin1 elem)) "\n"))
          new-entries)
    (save-buffer)
    (kill-buffer (current-buffer)))
    (message (format "Muse registry updated for URLs in %s"
                     (file-name-nondirectory
                      (buffer-file-name)))))

(defun muse-registry-make-new-registry nil
  "Make a new `muse-registry-alist' from scratch."
  (setq muse-registry-alist nil)
  (let ((muse-directories (mapcar 'caadr muse-project-alist))
        muse-directory)
    (while muse-directories
      (when (setq muse-directory (pop muse-directories))
        (mapcar (lambda (file)
                  (unless (or (string-match "d" (tenth file))
                              (string-match muse-project-ignore-regexp
                                            (first file)))
                    (mapc (lambda (elem)
                            (add-to-list 'muse-registry-alist elem))
                          (muse-registry-new-entries (car file)))))
                (directory-files-and-attributes muse-directory t)))))
  (muse-registry-create))

(defun muse-registry-new-entries (file)
  "List links in FILE that will be put in the registry."
  (let (result)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward muse-registry-url-or-link-regexp nil t)
        (let* ((point (number-to-string (match-beginning 0)))
               (link (or (match-string-no-properties 3)
                         (match-string-no-properties 1)))
               (desc (or (match-string-no-properties 5)
                         (progn (string-match
                                 muse-registry-url-regexp link)
                                (substring
                                 link (length (match-string 1 link))))))
               (keywords (muse-registry-get-keywords desc))
               (ln-keyword (muse-registry-get-link-keywords link)))
          (add-to-list 'result
                       (list file point link desc keywords ln-keyword)))))
    result))

(defun muse-registry-get-entries (annot level)
  "Show the relevant entries in the registry.
ANNOT is the annotation for the current buffer.
LEVEL is set interactively or  set to `muse-registry-show-level'."
  (when (string-match muse-registry-url-or-link-regexp annot)
    (let* ((link (or (match-string 3 annot)
                     (match-string 1 annot)))
           (desc (or (match-string 5 annot) ""))
           exact-match descriptive fuzzy)
      (dolist (entry muse-registry-alist)
        (let* ((output (muse-registry-entry-output entry))
               (keyword (fifth entry))
               (ln-keyword (sixth entry)))
          ;; exact matching
          (when (equal (third entry) link)
            (add-to-list 'exact-match output))
          ;; descriptive matching
          (when (and (> level 0) (equal (fourth entry) desc))
            (unless (member output exact-match)
              (add-to-list 'descriptive output)))
          ;; fuzzy matching
          (when (and (> level 1)
                     (or (string-match ln-keyword link)
                         (string-match keyword desc)))
            ;; use (muse-registry-get-keywords)?
            (unless (or (member output exact-match)
                        (member output descriptive))
              (add-to-list 'fuzzy output)))))
      (when exact-match
        (add-to-list 'exact-match
                     (concat "* Exact match(es):\n\n")))
      (when descriptive
        (add-to-list 'descriptive
                     (concat "* Description match(es):\n\n")))
      (when fuzzy
        (add-to-list 'fuzzy
                     (concat "* Fuzzy match(es):\n\n")))
      (cond (fuzzy (list exact-match descriptive fuzzy))
            (descriptive (list exact-match descriptive))
            (exact-match (list exact-match))
            (t nil)))))

(defun muse-registry-get-link-keywords (link)
  "Make a list of keywords for LINK."
  (setq link (car (split-string link "#" t))))

(defun muse-registry-get-keywords (desc)
  "Make a list of keywords for DESC."
  (let ((kw (split-string desc "[ ./]+" t)))
    (mapcar (lambda (wd) (setq kw (delete wd kw)))
            muse-registry-ignore-keywords)
    (setq kw
          (mapcar (lambda (a)
                    (when (>= (length a) muse-registry-min-keyword-size)
                      (substring
                       a 0 (if (> (length a) muse-registry-max-keyword-size)
                               muse-registry-max-keyword-size (length a)))))
                  kw))
    (setq kw (delq nil kw))
    (setq kw (nthcdr (- (length kw)
                        muse-registry-max-number-of-keywords) kw))
    (mapconcat (lambda (e) e) kw ".*")))

;;; muse-registry.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
