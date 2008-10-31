;;; muse-ikiwiki.el --- integrate with Ikiwiki

;; Copyright (C) 2008  Free Software Foundation, Inc.

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

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Ikiwiki Integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse)
(require 'muse-html)
(require 'muse-publish)

(eval-when-compile
  (require 'muse-colors))

(defgroup muse-ikiwiki nil
  "Options controlling the behavior of Muse integration with Ikiwiki."
  :group 'muse-publish)

(defcustom muse-ikiwiki-header ""
  "Header used for publishing Ikiwiki output files.
This may be text or a filename."
  :type 'string
  :group 'muse-ikiwiki)

(defcustom muse-ikiwiki-footer ""
  "Footer used for publishing Ikiwiki output files.
This may be text or a filename."
  :type 'string
  :group 'muse-ikiwiki)

;;; Publishing

(defun muse-ikiwiki-publish-file (file name &optional style)
  "Publish a single file for Ikiwiki.
The name of the style is given by STYLE.  It defaults to \"ikiwiki\".
The name of the real file is NAME, and the name of the temporary
file containing the content is FILE."
  (if (not (stringp file))
      (message "Error: No file given to publish")
    (unless style
      (setq style "ikiwiki"))
    (let ((muse-batch-publishing-p t)
          (title (muse-page-name name))
          (output-path file)
          (target file)
          (muse-publishing-current-file file)
          (muse-publishing-current-output-path file)
          (font-lock-verbose nil)
          muse-current-output-style)
      ;; don't activate VC when publishing files
      (setq vc-handled-backends nil)
      (setq muse-current-output-style (list :base style :path file))
      (setq auto-mode-alist
            (delete (cons (concat "\\." muse-file-extension "\\'")
                          'muse-mode-choose-mode)
                    auto-mode-alist))
      (muse-with-temp-buffer
        (muse-insert-file-contents file)
        (run-hooks 'muse-before-publish-hook)
        (let ((muse-inhibit-before-publish-hook t))
          (muse-publish-markup-buffer title style))
        (when (muse-write-file output-path t)
          (muse-style-run-hooks :final style file output-path target))))))

;;; Colors

(defface muse-ikiwiki-directive
  '((((class color) (background light))
     (:foreground "dark green"))
    (((class color) (background dark))
     (:foreground "green")))
  "Face for Ikiwiki directives."
  :group 'muse-ikiwiki)

(defun muse-colors-ikiwiki-directive ()
  "Color ikiwiki directives."
  (let ((start (match-beginning 0)))
    (unless (or (eq (get-text-property start 'invisible) 'muse)
                (get-text-property start 'muse-comment)
                (get-text-property start 'muse-directive))
      ;; beginning of line or space or symbol
      (save-excursion
        (and
         (catch 'valid
           (while t
             (skip-chars-forward "^\"]" muse-colors-region-end)
             (cond ((eq (point) (point-max))
                    (throw 'valid nil))
                   ((eq (char-after) ?\")
                    (if (and (< (1+ (point)) muse-colors-region-end)
                             (eq (char-after (1+ (point))) ?\"))
                        (if (and (> (+ 2 (point)) muse-colors-region-end)
                                 (eq (char-after (+ 2 (point))) ?\"))
                            ;; triple-quote
                            (forward-char 3)
                            (or (re-search-forward
                                 "\"\"\"" muse-colors-region-end t)
                                (throw 'valid nil))
                          ;; empty quotes ("")
                          (forward-char 2))
                      ;; quote with content
                      (forward-char)
                      (skip-chars-forward "^\"" muse-colors-region-end)))
                   ((eq (char-after) ?\])
                    (forward-char)
                    (when (and (< (point) muse-colors-region-end)
                               (eq (char-after (point)) ?\]))
                      (forward-char)
                      (throw 'valid t)))
                   ((eq (point) muse-colors-region-end)
                    (throw 'valid nil))
                   (t (throw 'valid nil)))))
         ;; found a valid directive
         (progn
           (add-text-properties start (point)
                                '(face muse-ikiwiki-directive
                                  muse-directive t))
           (when (save-excursion
                   (let ((end (point)))
                     (goto-char start)
                     (skip-chars-forward "^\n" end)
                     (and (eq (char-after) ?\n)
                          (not (= (point) end)))))
             (add-text-properties start (point)
                                  '(font-lock-multiline t)))))))))

(defun muse-ikiwiki-insinuate-colors ()
  (add-to-list 'muse-colors-markup
               '("\\[\\[!" ?\[ muse-colors-ikiwiki-directive)
               nil))

(eval-after-load "muse-colors" '(muse-ikiwiki-insinuate-colors))

;; Styles
(muse-derive-style "ikiwiki" "xhtml"
                   :header 'muse-ikiwiki-header
                   :footer 'muse-ikiwiki-footer)

(provide 'muse-ikiwiki)

;;; muse-ikiwiki.el ends here
