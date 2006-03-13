;; muse-latex2png.el --- generate PNG images from inline LaTeX code

;; Copyright (C) 2004, 2005 Ganesh Swami
;; Copyright (C) 2005 Free Software Foundation, Inc.

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

(defvar latex2png-scale-factor 2.5
  "The scale factor to be used for sizing the resulting latex.")
(defvar latex2png-fg "Black"
  "The foreground color.")
(defvar latex2png-bg "Transparent"
  "The background color.")

(defun latex2png-move2pubdir (file prefix pubdir)
  "Move FILE to the PUBDIR folder.

This is done so that the resulting images do not clutter your
main publishing directory.

Old files with PREFIX in the name are deleted."
  (if (and (file-exists-p file)
           (file-directory-p pubdir))
      (progn
        (copy-file file (concat pubdir (file-name-nondirectory file)) t)
        (delete-file file)
        (concat "./latex/" (file-name-nondirectory file)))
    (message "The latex folder does not exist!")))

(defun muse-publish-latex-tag (beg end attrs)
  (let ((end-marker (set-marker (make-marker) (1+ end)))
        (pubdir (concat (file-name-directory
                         muse-publishing-current-output-path)
                        "/latex/")))
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
        (muse-insert-markup
         "<img src=\""
         (latex2png-move2pubdir (latex2png text prefix preamble)
                                prefix pubdir)
         "\" alt=\"latex2png equation\" "
         (if display (concat "class=\"latex-inline\"")
           (concat "class=\"latex-display\""))
         " />")
        (muse-insert-markup "<!-- " text "-->")
        (goto-char (point-max))))))

(defun latex2png (math prefix preamble)
  "Convert the MATH code into a png with PREFIX."
  (unless preamble
    (setq preamble ""))
  (let ((texfile (expand-file-name
                  (concat prefix "_"  (format "%d" (abs (sxhash math))))
                  (cond ((boundp 'temporary-file-directory)
                         temporary-file-directory)
                        ((fboundp 'temp-directory)
                         (temp-directory)))))
        (oldcddir default-directory))
    (with-temp-file (concat texfile ".tex")
      (insert (concat "\\documentclass{article}
\\usepackage{fullpage}
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}\n" preamble
"\n\\pagestyle{empty}
\\begin{document}"
"{"
 math
"}\n"
"\n\\end{document}\n\n")))
    (cd "/tmp")
    (call-process "latex" nil nil nil texfile)
    (if (file-exists-p (concat texfile ".dvi"))
        (progn
          (shell-command-to-string
           (concat "dvipng " texfile ".dvi -E "
                   " -fg " latex2png-fg
                   " -bg " latex2png-bg " -T tight"
                   " -x " (format  "%s" (* latex2png-scale-factor 1000))
                   " -y " (format  "%s" (* latex2png-scale-factor 1000))
                   " -o " texfile ".png"))
          (if (file-exists-p (concat texfile ".png"))
              (progn
                (delete-file (concat texfile ".dvi"))
                (delete-file (concat texfile ".tex"))
                (delete-file (concat texfile ".aux"))
                (delete-file (concat texfile ".log"))
                (cd oldcddir)
                (concat texfile ".png"))
            (message "Failed to create png file")))
      (message (concat "Failed to create dvi file " texfile)))))

;;; Insinuate with muse-publish

(add-to-list 'muse-publish-markup-tags
             '("latex" t t muse-publish-latex-tag)
             t)

(provide 'muse-latex2png)
;;; muse-latex2png.el ends here
