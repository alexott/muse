;; muse-latex2png.el --- generate PNG images from inline LaTeX code

;; Copyright (C) 2004, 2005 Ganesh Swami
;; Copyright (C) 2005, 2006 Free Software Foundation, Inc.

;; Author: Ganesh Swami <ganesh AT iamganesh DOT com>
;; Created: May 01 2004

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

;; Read The Fine Documentation at
;; http://www.sfu.ca/~gswamina/EmacsWikiBlog.html

;; Michael Olson adapted this for Muse.

;;; To Do:

;; If we are publishing a LaTeX-based style, the
;; muse-publish-latex-tag function should insert the LaTeX code as-is.

;;; Code

(require 'muse-publish)

(defgroup muse-latex2png nil
  "Publishing LaTeX formulas as PNG files."
  :group 'muse-publish)

(defcustom muse-latex2png-img-dest "./latex"
  "The folder where the generated images will be placed.
This is relative to the current publishing directory."
  :type 'string
  :group 'muse-latex2png)

(defcustom muse-latex2png-scale-factor 2.5
  "The scale factor to be used for sizing the resulting LaTeX output."
  :type 'number
  :group 'muse-latex2png)

(defcustom muse-latex2png-fg "Black"
  "The foreground color."
  :type 'string
  :group 'muse-latex2png)

(defcustom muse-latex2png-bg "Transparent"
  "The background color."
  :type 'string
  :group 'muse-latex2png)

(defcustom muse-latex2png-template
  "\\documentclass{article}
\\usepackage{fullpage}
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
%preamble%
\\pagestyle{empty}
\\begin{document}
{%code%}
\\end{document}\n"
  "The LaTeX template to use."
  :type 'string
  :group 'muse-latex2png)

(defcustom muse-latex2png-use-xhtml nil
  "Indicate whether the output must be valid XHTML.
This is used for inserting the IMG tag."
  :type 'boolean
  :group 'muse-latex2png)

(defun muse-latex2png-move2pubdir (file prefix pubdir)
  "Move FILE to the PUBDIR folder.

This is done so that the resulting images do not clutter your
main publishing directory.

Old files with PREFIX in the name are deleted."
  (when file
    (if (file-exists-p file)
        (progn
          (unless (file-directory-p pubdir)
            (message "Creating latex directory %s" pubdir)
            (make-directory pubdir))
          (copy-file file (expand-file-name (file-name-nondirectory file)
                                            pubdir)
                     t)
          (delete-file file)
          (concat muse-latex2png-img-dest "/" (file-name-nondirectory file)))
      (message "Cannot find %s!" file))))

(defun muse-publish-latex-tag (beg end attrs)
  (let ((end-marker (set-marker (make-marker) (1+ end)))
        (pubdir (expand-file-name
                 muse-latex2png-img-dest
                 (file-name-directory muse-publishing-current-output-path))))
    (save-restriction
      (narrow-to-region beg end)
      (let* ((text (buffer-substring-no-properties beg end))
             ;; the prefix given to the image file.
             (prefix (cdr (assoc "prefix" attrs)))
             ;; preamble (for extra options)
             (preamble (cdr (assoc "preamble" attrs)))
             ;; display inline or as a block
             (display (car (assoc "inline" attrs))))
        (delete-region beg end)
        (goto-char (point-min))
        (unless (file-directory-p pubdir)
          (make-directory pubdir))
        (let ((path (muse-latex2png-move2pubdir
                     (muse-latex2png text prefix preamble)
                     prefix pubdir)))
          (when path
            (muse-insert-markup
             "<img src=\"" path
             "\" alt=\"latex2png equation\" "
             (if display (concat "class=\"latex-inline\"")
               (concat "class=\"latex-display\""))
             (if muse-latex2png-use-xhtml
                 " />"
               ">"))
            (muse-insert-markup "<!-- " text "-->")
            (goto-char (point-max))))))))

(defun muse-latex2png (code prefix preamble)
  "Convert the LaTeX CODE into a png file beginning with PREFIX.
PREAMBLE indicates extra packages and definitions to include."
  (unless preamble
    (setq preamble ""))
  (unless prefix
    (setq prefix "muse-latex2png"))
  (let* ((tmpdir (cond ((boundp 'temporary-file-directory)
                        temporary-file-directory)
                       ((fboundp 'temp-directory)
                        (temp-directory))
                       (t "/tmp")))
         (texfile (expand-file-name
                   (concat prefix "_"  (format "%d" (abs (sxhash code))))
                   tmpdir))
         (defalt-directory default-directory))
    (with-temp-file (concat texfile ".tex")
      (insert muse-latex2png-template)
      (goto-char (point-min))
      (while (search-forward "%preamble%" nil t)
        (replace-match preamble nil t))
      (goto-char (point-min))
      (while (search-forward "%code%" nil t)
        (replace-match code nil t)))
    (setq default-directory tmpdir)
    (call-process "latex" nil nil nil texfile)
    (if (file-exists-p (concat texfile ".dvi"))
        (progn
          (shell-command-to-string
           (concat "dvipng " texfile ".dvi -E"
                   " -fg " muse-latex2png-fg
                   " -bg " muse-latex2png-bg " -T tight"
                   " -x " (format  "%s" (* muse-latex2png-scale-factor 1000))
                   " -y " (format  "%s" (* muse-latex2png-scale-factor 1000))
                   " -o " texfile ".png"))
          (if (file-exists-p (concat texfile ".png"))
              (progn
                (delete-file (concat texfile ".dvi"))
                (delete-file (concat texfile ".tex"))
                (delete-file (concat texfile ".aux"))
                (delete-file (concat texfile ".log"))
                (concat texfile ".png"))
            (message "Failed to create png file")
            nil))
      (message (concat "Failed to create dvi file " texfile))
      nil)))

;;; Insinuate with muse-publish

(add-to-list 'muse-publish-markup-tags
             '("latex" t t nil muse-publish-latex-tag)
             t)

(provide 'muse-latex2png)
;;; muse-latex2png.el ends here
