;;; muse-book.el --- publish entries into a compilation

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Book Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-project)
(require 'muse-latex)

(defgroup muse-book nil
  "Module for publishing a series of Muse pages as a complete book.
Each page will become a separate chapter in the book, unless the
style keyword :nochapters is used, in which case they are all run
together as if one giant chapter."
  :group 'muse-publish)

(defcustom muse-book-before-publish-hook nil
  "A hook run in the book buffer before it is marked up."
  :type 'hook
  :group 'muse-book)

(defcustom muse-book-after-publish-hook nil
  "A hook run in the book buffer after it is marked up."
  :type 'hook
  :group 'muse-book)

(defcustom muse-book-latex-header
  "\\documentclass{book}

\\usepackage[english]{babel}
\\usepackage[latin1]{inputenc}
\\usepackage[T1]{fontenc}

\\begin{document}

\\title{<lisp>(muse-publishing-directive \"title\")</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

\\tableofcontents\n"
  "Header used for publishing books to LaTeX.  This may be text or a filename."
  :type 'string
  :group 'muse-book)

(defcustom muse-book-latex-footer "\n\\end{document}"
  "Footer used for publishing books to LaTeX.  This may be text or a filename."
  :type 'string
  :group 'muse-book)

(defun muse-book-publish-chapter (title entry style &optional nochapters)
  "Publish the chapter TITLE for the file ENTRY using STYLE.
TITLE is a string, ENTRY is a cons of the form (PAGE-NAME .
FILE), and STYLE is a Muse style list.

This routine does the same basic work as `muse-publish-markup-buffer',
but treating the page as if it were a single chapter within a book."
  (let ((muse-publishing-directives (list (cons "title" title)))
        (muse-publishing-current-file (cdr entry))
        (beg (point)) end)
    (insert-file-contents (cdr entry))
    (setq end (copy-marker (point-max) t))
    (muse-publish-markup-region beg end (car entry) style)
    (goto-char beg)
    (unless (or nochapters
                (muse-style-element :nochapters style))
      (insert "\n")
      (muse-insert-markup (muse-markup-text 'chapter))
      (insert (let ((chap (muse-publishing-directive "title")))
                (if (string= chap title)
                    (car entry)
                  chap)))
      (muse-insert-markup (muse-markup-text 'chapter-end))
      (insert "\n\n"))
    (save-restriction
      (narrow-to-region beg end)
      (muse-publish-markup (or title "")
                           '((100 "<\\(lisp\\)>" 0
                              muse-publish-markup-tag)))
      (muse-style-run-hooks :after style))
    (goto-char end)))

(defun muse-book-publish-project
  (project book title style &optional output-dir force)
  "Publish PROJECT under the name BOOK with the given TITLE and STYLE.
BOOK should be a page name, i.e., letting the style determine the
prefix and/or suffix.  The book is published to OUTPUT-DIR.  If FORCE
is nil, the book is only published if at least one of its component
pages has changed since it was last published."
  (interactive
   (let ((project (muse-read-project "Publish project as book: " nil t)))
     (append (list project
                   (read-string "Basename of book (without extension): ")
                   (read-string "Title of book: "))
             (muse-publish-get-info))))
  ;; Cleanup some of the arguments
  (setq project (muse-project project)
        style (muse-style style))
  ;; See if any of the project's files need saving first
  (muse-project-save-buffers project)
  ;; Publish each page in the project as a chapter in one large book
  (let* ((output-path (muse-publish-output-file book output-dir style))
         (output-suffix (muse-style-element :osuffix style))
         (target output-path)
         (pats (cadr project))
         (publish force) published)
    (when output-suffix
      (setq target (concat (file-name-sans-extension target)
                           output-suffix)))
    ;; Unless force is non-nil, determine if the book needs publishing
    (unless force
      (while pats
        (if (symbolp (car pats))
            (if (eq :book-end (car pats))
                (setq pats nil)
              (setq pats (cddr pats)))
          (let ((entries (muse-project-file-entries (car pats))))
            (while entries
              (if (and (not (muse-project-private-p (cdar entries)))
                       (file-newer-than-file-p (cdar entries) target))
                  (setq publish t entries nil)
                (setq entries (cdr entries)))))
          (setq pats (cdr pats)))))
    ;; Create the book from all its component parts
    (if (not publish)
        (message "The book \"%s\" is up-to-date." book)
      (muse-with-temp-buffer
        (let ((style-final  (muse-style-element :final  style t))
              (style-header (muse-style-element :header style))
              (style-footer (muse-style-element :footer style))
              (muse-publishing-current-style style)
              (muse-publishing-directives
               (list (cons "title" title)
                     (cons "date" (format-time-string "%B %e, %Y"))))
              (muse-publishing-p t)
              (muse-current-project project)
              nochapters)
          (run-hooks 'muse-before-book-publish-hook)
          (setq pats (cadr project))
          (let ((style-final style-final)
                (style-header style-header)
                (style-footer style-footer))
            (while pats
              (if (symbolp (car pats))
                  (cond
                   ((eq :book-part (car pats))
                    (insert "\n")
                    (muse-insert-markup (muse-markup-text 'part))
                    (insert (cadr pats))
                    (muse-insert-markup (muse-markup-text 'part-end))
                    (insert "\n")
                    (setq pats (cddr pats)))
                   ((eq :book-chapter (car pats))
                    (insert "\n")
                    (muse-insert-markup (muse-markup-text 'chapter))
                    (insert (cadr pats))
                    (muse-insert-markup (muse-markup-text 'chapter-end))
                    (insert "\n")
                    (setq pats (cddr pats)))
                   ((eq :nochapters (car pats))
                    (setq nochapters t
                          pats (cddr pats)))
                   ((eq :book-style (car pats))
                    (setq style (muse-style (cadr pats)))
                    (setq style-final  (muse-style-element :final  style t)
                          style-header (muse-style-element :header style)
                          style-footer (muse-style-element :footer style)
                          muse-publishing-current-style style)
                    (setq pats (cddr pats)))
                   ((eq :book-funcall (car pats))
                    (funcall (cadr pats))
                    (setq pats (cddr pats)))
                   ((eq :book-end (car pats))
                    (setq pats nil))
                   (t
                    (setq pats (cddr pats))))
                (let ((entries (muse-project-file-entries (car pats))))
                  (while (and entries (car entries) (caar entries))
                    (unless (muse-project-private-p (cdar entries))
                      (muse-book-publish-chapter title (car entries)
                                                 style nochapters)
                      (setq published t))
                    (setq entries (cdr entries))))
                (setq pats (cdr pats)))))
          (goto-char (point-min))
          (if style-header (muse-insert-file-or-string style-header book))
          (goto-char (point-max))
          (if style-footer (muse-insert-file-or-string style-footer book))
          (run-hooks 'muse-after-book-publish-hook)
          (let ((backup-inhibited t))
            (write-file output-path))
          (if style-final
              (funcall style-final book output-path target)))))
    (if published
        (message "The book \"%s\" has been published." book))
    published))

(unless (assoc "book-latex" muse-publishing-styles)
  (muse-derive-style "book-latex" "latex"
                     :header 'muse-book-latex-header
                     :footer 'muse-book-latex-footer)

  (muse-derive-style "book-pdf" "pdf"
                     :header 'muse-book-latex-header
                     :footer 'muse-book-latex-footer))

(provide 'muse-book)

;;; muse-book.el ends here
