;;; muse-colors.el --- Coloring and highlighting used by Muse

;; Copyright (C) 2004, 2005  Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: muse-colors.el
;; Date: Thu 11-Mar-2004
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; URL: http://www.mwolson.org/projects/MuseMode.html
;; Compatibility: Emacs21

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Contributors:

;; Lan Yufeng (nlany DOT web AT gmail DOT com) found an error where
;; headings were being given the wrong face, contributing a patch to
;; fix this.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Muse Highlighting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-mode)
(require 'muse-regexps)
(require 'font-lock)

(defgroup muse-colors nil
  "Options controlling the behavior of Emacs Muse highlighting.
See `muse-colors-buffer' for more information."
  :group 'muse-mode)

(defcustom muse-colors-autogen-headings t
  "Specify whether the heading faces should be auto-generated.
The default is to scale them.

Choosing 'outline will copy the colors from the outline-mode
headings.

If you want to customize each of the headings individually, set
this to nil."
  :type '(choice (const :tag "Default (scaled) headings" t)
                 (const :tag "Use outline-mode headings" outline)
                 (const :tag "Don't touch the headings" nil))
  :group 'muse-colors)

(defvar muse-colors-outline-faces-list
  (if (facep 'outline-1)
      '(outline-1 outline-2 outline-3 outline-4)
    ;; These are supposed to be equivalent in coloring
    '(font-lock-function-name-face
      font-lock-variable-name-face
      font-lock-keyword-face
      font-lock-builtin-face))
  "Outline faces to use when assigning Muse header faces.")

(defun muse-make-faces ()
  (let (newsym)
    (dolist (num '(1 2 3 4))
      (setq newsym (intern (concat "muse-header-"
                                   (int-to-string num))))
      (cond
       ((null muse-colors-autogen-headings)
        (make-empty-face newsym))
       ((featurep 'xemacs)
        (if (eq muse-colors-autogen-headings 'outline)
            (copy-face (nth (1- num)
                            muse-colors-outline-faces-list)
                       newsym)
          (eval `(defface ,newsym
                   '((t (:size
                         ,(nth (1- num) '("24pt" "18pt" "14pt" "12pt"))
                         :bold t)))
                   "Muse header face"
                   :group 'muse-colors))))
       ((< emacs-major-version 21)
        (if (eq muse-colors-autogen-headings 'outline)
            (copy-face (nth (1- num)
                            muse-colors-outline-faces-list)
                       newsym)
          (copy-face 'default newsym)))
       ((eq muse-colors-autogen-headings 'outline)
        (eval `(defface ,newsym
                 '((t (:inherit
                       ,(nth (1- num)
                             muse-colors-outline-faces-list))))
                 "Muse header face"
                 :group 'muse-colors)))
       (t
        (eval `(defface ,newsym
                 '((t (:height ,(1+ (* 0.1 (- 5 num)))
                               :inherit variable-pitch
                               :weight bold)))
                 "Muse header face"
                 :group 'muse-colors)))))))

(defface muse-link-face
  '((((class color) (background light))
     (:foreground "green" :underline "green" :bold t))
    (((class color) (background dark))
     (:foreground "cyan" :underline "cyan" :bold t))
    (t (:bold t)))
  "Face for Muse cross-references."
  :group 'muse-colors)

(defface muse-bad-link-face
  '((((class color) (background light))
     (:foreground "red" :underline "red" :bold t))
    (((class color) (background dark))
     (:foreground "coral" :underline "coral" :bold t))
    (t (:bold t)))
  "Face for bad Muse cross-references."
  :group 'muse-colors)

(defcustom muse-colors-buffer-hook nil
  "A hook run after a region is highlighted.
Each function receives three arguments: BEG END VERBOSE.
BEG and END mark the range being highlighted, and VERBOSE specifies
whether progress messages should be displayed to the user."
  :type 'hook
  :group 'muse-colors)

(defvar muse-colors-regexp nil)
(defvar muse-colors-vector nil)

(defun muse-configure-highlighting (sym val)
  (setq muse-colors-regexp
        (concat "\\(" (mapconcat (function
                                  (lambda (rule)
                                    (if (symbolp (car rule))
                                        (symbol-value (car rule))
                                      (car rule)))) val "\\|") "\\)")
        muse-colors-vector (make-vector 128 nil))
  (let ((rules val))
    (while rules
      (if (eq (cadr (car rules)) t)
          (let ((i 0) (l 128))
            (while (< i l)
              (unless (aref muse-colors-vector i)
                (aset muse-colors-vector i (nth 2 (car rules))))
              (setq i (1+ i))))
        (aset muse-colors-vector (cadr (car rules))
              (nth 2 (car rules))))
      (setq rules (cdr rules))))
  (set sym val))

(eval-when-compile
  (defvar end))

(defun muse-colors-emphasized ()
  ;; Here we need to check four different points - the start and end
  ;; of the leading *s, and the start and end of the trailing *s.  We
  ;; allow the outsides to be surrounded by whitespace or punctuation,
  ;; but no word characters, and the insides must not be surrounded by
  ;; whitespace or punctuation.  Thus the following are valid:
  ;;
  ;; " *foo bar* "
  ;; "**foo**,"
  ;; and the following is invalid:
  ;; "** testing **"
  (let* ((beg (match-beginning 0))
         (e1 (match-end 0))
         (leader (- e1 beg))
         b2 e2 multiline)
    (unless (eq (get-text-property beg 'invisible) 'muse)
      ;; check if it's a header
      (if (eq (char-after e1) ?\ )
          (when (or (= beg (point-min))
                    (eq (char-before beg) ?\n))
            (add-text-properties
             (muse-line-beginning-position) (muse-line-end-position)
             (list 'face (intern (concat "muse-header-"
                                         (int-to-string leader))))))
        ;; beginning of line or space or symbol
        (when (or (= beg (point-min))
                  (memq (char-syntax (char-before beg)) '(?\  ?\-))
                  (memq (char-before beg)
                        '(?\- ?\[ ?\< ?\( ?\' ?\` ?\" ?\n)))
          (save-excursion
            (skip-chars-forward "^*<>\n" end)
            (when (eq (char-after) ?\n)
              (setq multiline t)
              (skip-chars-forward "^*<>" end))
            (setq b2 (point))
            (skip-chars-forward "*" end)
            (setq e2 (point))
            ;; Abort if space exists just before end
            ;; or bad leader
            ;; or no '*' at end
            (unless (or (> leader 3)
                        (not (eq leader (- e2 b2)))
                        (eq (char-before b2) ?\ )
                        (not (eq (char-after b2) ?*)))
              (add-text-properties beg e1 '(invisible muse))
              (add-text-properties
               e1 b2 (list 'face (cond ((= leader 1) 'italic)
                                       ((= leader 2) 'bold)
                                       ((= leader 3) 'bold-italic))))
              (add-text-properties b2 e2 '(invisible muse))
              (when multiline
                (add-text-properties
                 beg e2 '(font-lock-multiline t))))))))))

(defun muse-colors-underlined ()
  (let ((start (match-beginning 0))
        multiline)
    (unless (eq (get-text-property start 'invisible) 'muse)
      ;; beginning of line or space or symbol
      (when (or (= start (point-min))
                (memq (char-syntax (char-before start)) '(?\  ?\-))
                (memq (char-before start)
                      '(?\- ?\[ ?\< ?\( ?\' ?\` ?\" ?\n)))
        (save-excursion
          (skip-chars-forward "^_<>\n" end)
          (when (eq (char-after) ?\n)
            (setq multiline t)
            (skip-chars-forward "^_<>" end))
          ;; Abort if space exists just before end
          ;; or no '_' at end
          (unless (or (eq (char-before (point)) ?\ )
                      (not (eq (char-after (point)) ?_)))
            (add-text-properties start (1+ start) '(invisible muse))
            (add-text-properties (1+ start) (point) '(face underline))
            (add-text-properties (point)
                                 (min (1+ (point)) (point-max))
                                 '(invisible muse))
            (when multiline
              (add-text-properties
               start (min (1+ (point)) (point-max))
               '(font-lock-multiline t)))))))))

(defun muse-colors-verbatim ()
  (skip-chars-forward (concat "^" muse-regexp-space "=>") end))

(defcustom muse-colors-markup
  `(;; render in teletype and suppress further parsing
    (,(concat "\\b=[^"
              muse-regexp-space
              "=>]")
     ?= muse-colors-verbatim)

    ;; make emphasized text appear emphasized
    ("\\*\\{1,4\\}" ?* muse-colors-emphasized)

    ;; make underlined text appear underlined
    (,(concat "_[^"
              muse-regexp-blank
              "_]")
     ?_ muse-colors-underlined)

    ("^#title" ?\# muse-colors-title)

    (muse-explicit-link-regexp ?\[ muse-colors-link)

    ;; highlight any markup tags encountered
    (muse-tag-regexp ?\< muse-colors-custom-tags)
    )
  "Expressions to highlight an Emacs Muse buffer.
These are arranged in a rather special fashion, so as to be as quick as
possible.

Each element of the list is itself a list, of the form:

  (LOCATE-REGEXP TEST-CHAR MATCH-FUNCTION)

LOCATE-REGEXP is a partial regexp, and should be the smallest possible
regexp to differentiate this rule from other rules.  It may also be a
symbol containing such a regexp.  The buffer region is scanned only
once, and LOCATE-REGEXP indicates where the scanner should stop to
look for highlighting possibilities.

TEST-CHAR is a char or t.  The character should match the beginning
text matched by LOCATE-REGEXP.  These chars are used to build a vector
for fast MATCH-FUNCTION calling.

MATCH-FUNCTION is the function called when a region has been
identified.  It is responsible for adding the appropriate text
properties to change the appearance of the buffer.

This markup is used to modify the appearance of the original text to
make it look more like the published HTML would look (like making some
markup text invisible, inlining images, etc).

font-lock is used to apply the markup rules, so that they can happen
on a deferred basis.  They are not always accurate, but you can use
\\[font-lock-fontifty-block] near the point of error to force
fontification in that area.

Lastly, none of the regexp should contain grouping elements that will
affect the match data results."
  :type '(repeat
          (list :tag "Highlight rule"
                (choice (regexp :tag "Locate regexp")
                        (symbol :tag "Regexp symbol"))
                (choice (character :tag "Confirm character")
                        (const :tag "Default rule" t))
                function))
  :set 'muse-configure-highlighting
  :group 'muse-colors)

;; XEmacs users don't have `font-lock-multiline'.
(unless (boundp 'font-lock-multiline)
  (defvar font-lock-multiline nil))

(defun muse-use-font-lock ()
  (muse-add-to-invisibility-spec 'muse)
  (set (make-local-variable 'font-lock-multiline) 'undecided)
  (set (make-local-variable 'font-lock-defaults)
       `(nil t nil nil 'beginning-of-line
         (font-lock-fontify-region-function . muse-colors-region)
         (font-lock-unfontify-region-function
          . muse-unhighlight-region)))
  (set (make-local-variable 'font-lock-fontify-region-function)
       'muse-colors-region)
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'muse-unhighlight-region)
  (muse-make-faces)
  (muse-configure-highlighting 'muse-colors-markup muse-colors-markup)
  (font-lock-mode t))

(defun muse-colors-buffer ()
  "Re-highlight the entire Muse buffer."
  (interactive)
  (muse-colors-region (point-min) (point-max) t))

(defun muse-colors-region (beg end &optional verbose)
  "Apply highlighting according to `muse-colors-markup'.
Note that this function should NOT change the buffer, nor should any
of the functions listed in `muse-colors-markup'."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        (modified-p (buffer-modified-p))
        deactivate-mark)
    (unwind-protect
        (save-excursion
          (save-restriction
            (widen)
            ;; check to see if we should expand the beg/end area for
            ;; proper multiline matches
            (when (and font-lock-multiline
                       (> beg (point-min))
                       (get-text-property (1- beg) 'font-lock-multiline))
              ;; We are just after or in a multiline match.
              (setq beg (or (previous-single-property-change
                             beg 'font-lock-multiline)
                            (point-min)))
              (goto-char beg)
              (setq beg (muse-line-beginning-position)))
            (when font-lock-multiline
              (setq end (or (text-property-any end (point-max)
                                               'font-lock-multiline nil)
                            (point-max))))
            (goto-char end)
            (setq end (muse-line-beginning-position 2))
            ;; Undo any fontification in the area.
            (font-lock-unfontify-region beg end)
            ;; And apply fontification based on `muse-colors-markup'
            (let ((len (float (- end beg)))
                  (case-fold-search nil)
                  markup-func)
              (goto-char beg)
              (while (re-search-forward muse-colors-regexp end t)
                (if verbose
                    (message "Highlighting buffer...%d%%"
                             (* (/ (float (- (point) beg)) len) 100)))
                (setq markup-func
                      (aref muse-colors-vector
                            (char-after (match-beginning 0))))
                (when markup-func (funcall markup-func)))
              (run-hook-with-args 'muse-colors-buffer-hook
                                  beg end verbose)
              (if verbose (message "Highlighting buffer...done")))))
      (set-buffer-modified-p modified-p))))

(defcustom muse-colors-tags
  '(("example" t nil muse-colors-example-tag))
  "A list of tag specifications for specially highlighting text.
XML-style tags are the best way to add custom highlighting to Muse.
This is easily accomplished by customizing this list of markup tags.

For each entry, the name of the tag is given, whether it expects
a closing tag and/or an optional set of attributes, and a
function that performs whatever action is desired within the
delimited region.

The function is called with three arguments, the beginning and
end of the region surrounded by the tags. If properties are
allowed, they are passed as a third argument in the form of an
alist. The `end' argument to the function is the last character
of the enclosed tag or region.

Functions should not modify the contents of the buffer."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       function))
  :group 'muse-colors)

(defsubst muse-colors-tag-info (tagname &rest args)
  (assoc tagname muse-colors-tags))

(defun muse-colors-custom-tags ()
  "Highlight `muse-colors-tags'."
  (let ((tag-info (muse-colors-tag-info (match-string 4))))
    (when tag-info
      (let ((closed-tag (match-string 3))
            (start (match-beginning 0))
            end attrs)
        (when (nth 2 tag-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match (concat "\\([^"
                                              muse-regexp-space
                                              "=]+\\)\\(=\""
                                              "\\([^\"]+\\)\"\\)?")
                                      attrstr))
              (let ((attr (cons (downcase
                                 (muse-match-string-no-properties 1 attrstr))
                                (muse-match-string-no-properties 3 attrstr))))
                (setq attrstr (replace-match "" t t attrstr))
                (if attrs
                    (nconc attrs (list attr))
                  (setq attrs (list attr)))))))
        (if (and (cadr tag-info) (not closed-tag))
            (if (search-forward (concat "</" (car tag-info) ">") nil t)
                (setq end (match-end 0))
              (setq tag-info nil)))
        (when tag-info
          (let ((args (list start end)))
            (if (nth 2 tag-info)
                (nconc args (list attrs)))
            (apply (nth 3 tag-info) args)))))))

(defun muse-colors-example-tag (beg end)
  "Strip properties from stuff in example."
  (set-text-properties beg end nil)
  (goto-char end))

(defun muse-unhighlight-region (begin end &optional verbose)
  "Remove all visual highlights in the buffer (except font-lock)."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        (modified-p (buffer-modified-p))
        deactivate-mark)
    (unwind-protect
        (remove-text-properties
         begin end '(face nil font-lock-multiline nil
                          invisible nil intangible nil display nil
                          mouse-face nil keymap nil help-echo nil))
      (set-buffer-modified-p modified-p))))

(defvar muse-mode-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'muse-follow-name-at-point)
    (define-key map [(control ?m)] 'muse-follow-name-at-point)
    (define-key map [(shift return)] 'muse-follow-name-at-point-other-window)
    (if (featurep 'xemacs)
        (progn
          (define-key map [(button2)] 'muse-follow-name-at-mouse)
          (define-key map [(shift button2)]
            'muse-follow-name-at-mouse-other-window))
      (define-key map [(shift control ?m)]
        'muse-follow-name-at-point-other-window)
      (define-key map [mouse-2] 'muse-follow-name-at-mouse)
      (define-key map [(shift mouse-2)]
        'muse-follow-name-at-mouse-other-window)
      (unless (eq emacs-major-version 21)
        (set-keymap-parent map muse-mode-map)))
    map)
  "Local keymap used by Muse while on a link.")

(defvar muse-keymap-property
  (if (or (featurep 'xemacs)
          (>= emacs-major-version 21))
      'keymap
    'local-map))

(defsubst muse-link-properties (help-str &optional face)
  (append (if face
              (list 'face face 'rear-nonsticky t
                    muse-keymap-property muse-mode-local-map)
            (list 'invisible 'muse 'intangible t 'rear-nonsticky t
                  muse-keymap-property muse-mode-local-map))
          (list 'mouse-face 'highlight
                'help-echo help-str
                muse-keymap-property muse-mode-local-map)))

(defun muse-link-face (link-name &optional explicit)
  "Return the type of LINK-NAME as a face symbol.
For EXPLICIT links, this is either a normal link or a bad-link
face.  For implicit links, it is either colored normally or
ignored."
  (save-match-data
    (let ((link (if explicit
                    (muse-handle-explicit-link link-name)
                  (muse-handle-implicit-link link-name))))
      (when link
        (if (or (not explicit)
                (string-match muse-file-regexp link)
                (string-match muse-url-regexp link))
            'muse-link-face
          (if (not (featurep 'muse-project))
              'muse-link-face
            (if (string-match "#" link)
                (setq link (substring link 0 (match-beginning 0))))
            (if (or (and (muse-project-of-file)
                         (muse-project-page-file
                          link-name muse-current-project t))
                    (file-exists-p link))
                'muse-link-face
              (when explicit
                'muse-bad-link-face))))))))

(defun muse-colors-link ()
  (when (eq ?\[ (char-after (match-beginning 0)))
    ;; remove flyspell overlays
    (when (fboundp 'flyspell-unhighlight-at)
      (let ((cur (match-beginning 0)))
        (while (> (match-end 0) cur)
          (flyspell-unhighlight-at cur)
          (setq cur (1+ cur)))))
    (let* (;; FIXME: link is not used
           (link (muse-match-string-no-properties 2))
           (desc (muse-match-string-no-properties 3))
           (props (muse-link-properties
                   desc (muse-link-face (match-string 2) t)))
           (invis-props (append props (muse-link-properties desc))))
      (if desc
          (progn
            ;; we put the normal face properties on the invisible
            ;; portion too, since emacs sometimes will position
            ;; the cursor on an intangible character
            (add-text-properties (match-beginning 0)
                                 (match-beginning 3) invis-props)
            (add-text-properties (match-beginning 3) (match-end 3) props)
            (add-text-properties (match-end 3) (match-end 0) invis-props))
        (add-text-properties (match-beginning 0)
                             (match-beginning 2) invis-props)
        (add-text-properties (match-beginning 2) (match-end 0) props)
        (add-text-properties (match-end 2) (match-end 0) invis-props)))
    (goto-char (match-end 0))
    (add-text-properties
     (match-beginning 0) (match-end 0)
     (muse-link-properties (muse-match-string-no-properties 0)
                           (muse-link-face (match-string 2) t)))
    (goto-char (match-end 0))))

(defun muse-colors-title ()
  (add-text-properties (+ 7 (match-beginning 0))
                       (muse-line-end-position)
                       '(face muse-header-1)))

(provide 'muse-colors)

;;; muse-colors.el ends here
