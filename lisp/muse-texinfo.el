;;; muse-texinfo.el --- Publish entries to Texinfo format or PDF.

;; Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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
;; Muse Texinfo Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)
(require 'muse-latex)
(require 'texnfo-upd)

(defgroup muse-texinfo nil
  "Rules for marking up a Muse file as a Texinfo article."
  :group 'muse-publish)

(defcustom muse-texinfo-process-natively t
  "If non-nil, use the Emacs `texinfmt' module to make Info files."
  :type 'boolean
  :require 'texinfmt
  :group 'muse-texinfo)

(defcustom muse-texinfo-extension ".texi"
  "Default file extension for publishing Texinfo files."
  :type 'string
  :group 'muse-texinfo)

(defcustom muse-texinfo-info-extension ".info"
  "Default file extension for publishing Info files."
  :type 'string
  :group 'muse-texinfo)

(defcustom muse-texinfo-pdf-extension ".pdf"
  "Default file extension for publishing PDF files."
  :type 'string
  :group 'muse-texinfo)

(defcustom muse-texinfo-header
  "\\input texinfo  @c -*-texinfo-*-

@setfilename <lisp>(concat (muse-page-name) \".info\")</lisp>
@settitle <lisp>(muse-publishing-directive \"title\")</lisp>

@documentencoding iso-8859-1

@iftex
@finalout
@end iftex

@titlepage
@title <lisp>(muse-publishing-directive \"title\")</lisp>
@author <lisp>(muse-publishing-directive \"author\")</lisp>
@end titlepage

@node Top, Overview, , (dir)
@top Overview
@c Page published by Emacs Muse begins here\n\n"
  "Text to prepend to a Muse page being published as Texinfo.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'muse-texinfo)

(defcustom muse-texinfo-footer
  "\n@c Page published by Emacs Muse ends here
<lisp>(and muse-publish-generate-contents \"@contents\")</lisp>
@bye\n"
  "Text to append to a Muse page being published as Texinfo.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'muse-texinfo)

(defcustom muse-texinfo-markup-regexps
  '(;; join together the parts of a list or table
    (10000
     "@end \\(\\(multi\\)?table\\|itemize\\|enumerate\\)\n+@\\1.*\n+" 0 ""))
  "List of markup rules for publishing a Muse page to Texinfo.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-texinfo)

(defcustom muse-texinfo-markup-functions
  '((table . muse-texinfo-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-texinfo)

(defcustom muse-texinfo-markup-strings
  '((image-with-desc . "@image{%s}")
    (image-link      . "@image{%s}")
    (url-with-image  . "@uref{%s, %s}")
    (url-link        . "@uref{%s, %s}")
    (email-addr      . "@email{%s}")
    (emdash          . "---")
    (rule            . "@sp 1")
    (enddots         . "....")
    (dots            . "...")
    (section         . "@chapter ")
    (subsection      . "@section ")
    (subsubsection   . "@subsection ")
    (footnote        . "@footnote{")
    (footnote-end    . "}")
    (begin-underline . "")
    (end-underline   . "")
    (begin-literal   . "@samp{")
    (end-literal     . "}")
    (begin-emph      . "@emph{")
    (end-emph        . "}")
    (begin-more-emph . "@strong{")
    (end-more-emph   . "}")
    (begin-most-emph . "@strong{@emph{")
    (end-most-emph   . "}}")
    (begin-verse     . "@display\n")
    (end-verse-line  . "")
    (verse-space     . "@ @ ")
    (end-verse       . "\n@end display")
    (begin-example   . "@example")
    (end-example     . "@end example")
    (begin-center    . "@center\n")
    (end-center      . "\n@end center")
    (begin-quote     . "@quotation\n")
    (end-quote       . "\n@end quotation")
    (begin-uli       . "@itemize @bullet\n@item\n")
    (end-uli         . "\n@end itemize")
    (begin-oli       . "@enumerate\n@item\n")
    (end-oli         . "\n@end enumerate")
    (begin-ddt       . "@table @strong\n@item ")
    (start-dde       . "\n")
    (end-ddt         . "\n@end table"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-texinfo)

(defcustom muse-texinfo-markup-specials
  '((?@ . "@@"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-texinfo)

(defun muse-texinfo-markup-table ()
  (let* ((str (prog1
                  (match-string 1)
                (delete-region (match-beginning 0) (match-end 0))))
         (fields (split-string str "\\s-*|+\\s-*")))
    (insert "@multitable @columnfractions ")
    (dotimes (field (length fields))
      (insert (number-to-string (/ 1.0 (length fields))) " "))
    (insert "\n@item " (mapconcat 'identity fields " @tab "))
    (insert "\n@end multitable")))

(defun muse-texinfo-finalize-buffer ()
  (goto-char (point-min))
  (muse-latex-fixup-dquotes)
  (texinfo-insert-node-lines (point-min) (point-max) t)
  (texinfo-all-menus-update t))

(defun muse-texinfo-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-texinfo-info-generate (file output-path final-target)
  ;; The version of `texinfmt.el' that comes with Emacs 21 doesn't
  ;; support @documentencoding, so hack it in.
  (when (and (not (featurep 'xemacs))
             (eq emacs-major-version 21))
    (put 'documentencoding 'texinfo-format
         'texinfo-discard-line-with-args))
  (muse-publish-transform-output
   file output-path final-target "Info"
   (function
    (lambda (file output-path)
      (if muse-texinfo-process-natively
          (save-window-excursion
            (save-excursion
              (find-file file)
              (let ((inhibit-read-only t))
                (texinfo-format-buffer))
              (save-buffer)
              (kill-buffer (current-buffer))
              (let ((buf (get-file-buffer file)))
                (with-current-buffer buf
                  (set-buffer-modified-p nil)
                  (kill-buffer (current-buffer))))
              t))
        (= 0 (shell-command
              (concat "makeinfo --enable-encoding --output="
                      output-path " " file))))))))

(defun muse-texinfo-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (= 0 (shell-command (concat "texi2pdf -q --clean --output="
                                  output-path " " file)))))))

(unless (assoc "texi" muse-publishing-styles)
  (muse-define-style "texi"
                     :suffix    'muse-texinfo-extension
                     :regexps   'muse-texinfo-markup-regexps
                     :functions 'muse-texinfo-markup-functions
                     :strings   'muse-texinfo-markup-strings
                     :specials  'muse-texinfo-markup-specials
                     :after     'muse-texinfo-finalize-buffer
                     :header    'muse-texinfo-header
                     :footer    'muse-texinfo-footer
                     :browser   'find-file)

  (muse-derive-style "info" "texi"
                     :final   'muse-texinfo-info-generate
                     :osuffix 'muse-texinfo-info-extension
                     :browser 'info)

  (muse-derive-style "info-pdf" "texi"
                     :final   'muse-texinfo-pdf-generate
                     :osuffix 'muse-texinfo-pdf-extension
                     :browser 'muse-texinfo-pdf-browse-file))

(provide 'muse-texinfo)

;;; muse-texinfo.el ends here
