;;; muse-xml-common.el --- common routines for XML-like publishing styles

;; Copyright (C) 2005  Free Software Foundation, Inc.

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

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse XML Publishing - Common Elements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-regexps)

(defun muse-xml-escape-string (str &rest ignored)
  "Convert to character entities any non-alphanumeric characters
outside a few punctuation symbols, that risk being misinterpreted
if not escaped."
  (when str
    (let (pos code len ch)
      (save-match-data
        (while (setq pos (string-match (concat "[^-"
                                               muse-regexp-alnum
                                               "/:._=@\\?~#\"<>&;]")
                                       str pos))
          (setq ch (aref str pos)
                code (concat "&#"
                                 (int-to-string
                                  (cond ((fboundp 'char-to-ucs)
                                         (char-to-ucs ch))
                                        ((fboundp 'char-to-int)
                                         (char-to-int ch))
                                        (t ch)))
                                 ";")
                len (length code)
                str (concat (substring str 0 pos)
                            code
                            (when (< pos (length str))
                              (substring str (1+ pos) nil)))
                pos (+ len pos)))
        str))))

(defun muse-xml-sort-table (table)
  "Sort the given table structure so that it validates properly."
  ;; Note that the decision matrix must have a nil diagonal, or else
  ;; elements with the same type will be reversed with respect to each
  ;; other.
  (let ((decisions '((nil nil nil)      ; body < header, body < footer
                     (t nil t)          ; header stays where it is
                     (t nil nil))))     ; footer < header
    (sort table #'(lambda (l r)
                    (nth (1- (car r))
                         (nth (1- (car l)) decisions))))))

(provide 'muse-xml-common)

;;; muse-xml-common.el ends here
