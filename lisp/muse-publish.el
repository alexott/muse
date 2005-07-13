;;; muse-publish.el --- Base publishing implementation.

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

;; Yann Hodique (yann DOT hodique AT gmail DOT com) fixed an
;; unnecessary URL description transform in `muse-publish-url'.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse)
(require 'muse-regexps)

(defgroup muse-publish nil
  "Options controlling the general behavior of Muse publishing.
See `muse-publish' for more information."
  :group 'muse)

(defcustom muse-before-publish-hook nil
  "A hook run in the buffer to be published, before it is done."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-after-publish-hook nil
  "A hook run in the buffer to be published, after it is done."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-publish-url-transforms '(muse-publish-prepare-url)
  "A list of functions used to prepare URLs for publication.
Each is passed the URL and expects a URL to be returned."
  :type 'hook
  :options '(muse-publish-prepare-url)
  :group 'muse-publish)

(defcustom muse-publish-report-threshhold 100000
  "If a file is this size or larger, report publishing progress."
  :type 'integer
  :group 'muse-publish)

(defcustom muse-publish-markup-regexps
  `(;; Remove leading and trailing whitespace from the file
    (1000 "\\(\\`\n+\\|\n+\\'\\)" 0 "")

    ;; Remove trailing whitespace from all lines
    (1100 ,(concat "[" muse-regexp-blank "]+$") 0 "")

    ;; Handle any leading #directives
    (1200 "\\`#\\([a-zA-Z]+\\)\\s-+\\(.+\\)\n+" 0 directive)

    ;; markup tags
    (1300 muse-tag-regexp 0 tag)

    ;; commented lines
    (1350 "^;\\s-+\\(.+\\)" 0 comment)

    ;; define anchor points
    (1400 "^#\\(\\S-+\\)\\s-*" 0 anchor)

    ;; replace links in the buffer (links to other pages)
    (1500 muse-explicit-link-regexp 0 link)

    ;; emphasized or literal text
    (1600 ,(concat
            "\\(^\\|[-["
            muse-regexp-space
            "<('`\"]\\)\\(=[^="
            muse-regexp-space
            "]\\|_[^_"
            muse-regexp-space
            "]\\|\\*+[^*"
            muse-regexp-space
            "]\\)")
          2 word)

    ;; headings, outline-mode style
    (1700 "^\\(\\*+\\)\\s-+" 0 heading)

    ;; ellipses
    (1800 "\\.\\.\\.\\." 0 enddots)
    (1850 "\\.\\.\\." 0 dots)

    ;; horizontal rule, or section separator
    (1900 "^----+" 0 rule)

    (2000 ,(concat "\n*\\(^\\|["
                   muse-regexp-blank
                   "]+\\)--\\($\\|["
                   muse-regexp-blank
                   "]+\\)")
          0 emdash)

    ;; beginning of footnotes section
    (2100 "^Footnotes:?\\s-*" 0 fn-sep)
    ;; footnote definition/reference (def if at beginning of line)
    (2200 "\\[\\([1-9][0-9]*\\)\\]" 0 footnote)

    ;; unnumbered List items begin with a -.  numbered list items
    ;; begin with number and a period.  definition lists have a
    ;; leading term separated from the body with ::.  centered
    ;; paragraphs begin with at least six columns of whitespace; any
    ;; other whitespace at the beginning indicates a blockquote.  The
    ;; reason all of these rules are handled here, is so that
    ;; blockquote detection doesn't interfere with indented list
    ;; members.
    (2300 ,(concat "^["
                   muse-regexp-blank
                   "]+\\(-["
                   muse-regexp-blank
                   "]*\\|[0-9]+\\.["
                   muse-regexp-blank
                   "]*\\|\\(?:.+?\\)["
                   muse-regexp-blank
                   "]+::\n?\\)")
          1 list)

    (2400 ,(concat "^\\(\\(?:.+?\\)["
                   muse-regexp-blank
                   "]+::\n?\\)")
          0 list)

    (2500 ,(concat "^\\(["
                   muse-regexp-blank
                   "]+\\)")
          0 quote)

    ;; "verse" text is indicated the same way as a quoted e-mail
    ;; response: "> text", where text may contain initial whitespace
    ;; (see below).
    (2600 ,(concat "^["
                   muse-regexp-blank
                   "]*> ")
          0 verse)

    ;; simple table markup is supported, nothing fancy.  use | to
    ;; separate cells, || to separate header cells, and ||| for footer
    ;; cells
    (2700 ,(concat "^["
                   muse-regexp-blank
                   "]*\\(.+?\\(["
                   muse-regexp-blank
                   "]+|+["
                   muse-regexp-blank
                   "]+.+?\\)\\)$")
          0 table)

    ;; base URLs
    (3000 muse-url-regexp  0 url)

    ;; bare email addresses
    (3500
     "\\([^[]\\)[-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+" 0 email)
    )
  "List of markup rules for publishing a page with Muse.
The rules given in this variable are invoked first, followed by
whatever rules are specified by the current style.

Each member of the list is either a function, or a list of the form:

  (REGEXP/SYMBOL TEXT-BEGIN-GROUP REPLACEMENT-TEXT/FUNCTION/SYMBOL)

REGEXP is a regular expression, or symbol whose value is a regular
expression, which is searched for using `re-search-forward'.
TEXT-BEGIN-GROUP is the matching group within that regexp which
denotes the beginning of the actual text to be marked up.
REPLACEMENT-TEXT is a string that will be passed to `replace-match'.
If it is not a string, but a function, it will be called to determine
what the replacement text should be (it must return a string).  If it
is a symbol, the value of that symbol should be a string.

The replacements are done in order, one rule at a time.  Writing
the regular expressions can be a tricky business.  Note that case
is never ignored.  `case-fold-search' is always bound to nil
while processing the markup rules."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-publish)

(defcustom muse-publish-markup-functions
  '((directive . muse-publish-markup-directive)
    (comment   . muse-publish-markup-comment)
    (anchor    . muse-publish-markup-anchor)
    (tag       . muse-publish-markup-tag)
    (word      . muse-publish-markup-word)
    (emdash    . muse-publish-markup-emdash)
    (enddots   . muse-publish-markup-enddots)
    (dots      . muse-publish-markup-dots)
    (rule      . muse-publish-markup-rule)
    (heading   . muse-publish-markup-heading)
    (footnote  . muse-publish-markup-footnote)
    (fn-sep    . muse-publish-markup-fn-sep)
    (list      . muse-publish-markup-list)
    (quote     . muse-publish-markup-quote)
    (verse     . muse-publish-markup-verse)
    (table     . muse-publish-markup-table)
    (email     . muse-publish-markup-email)
    (link      . muse-publish-markup-link)
    (url       . muse-publish-markup-url))
  "An alist of style types to custom functions for that kind of text.

Each member of the list is of the form:

  (SYMBOL FUNCTION)

SYMBOL describes the type of text to associate with this rule.
`muse-publish-markup-regexps' maps regexps to these symbols.

FUNCTION is the function to use to mark up this kind of rule if
no suitable function is found through the :functions tag of the
current style."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-publish)

(defcustom muse-publish-markup-tags
  '(("contents" nil t   muse-publish-contents-tag)
    ("verse"    t   nil muse-publish-verse-tag)
    ("example"  t   nil muse-publish-example-tag)
    ("literal"  t   nil muse-publish-mark-read-only)
    ("verbatim" t   nil muse-publish-verbatim-tag)
    ("lisp"     t   nil muse-publish-lisp-tag)
    ("class"    t   t   muse-publish-class-tag)
    ("command"  t   t   muse-publish-command-tag)
    ("comment"  t   nil muse-publish-comment-tag))
  "A list of tag specifications, for specially marking up text.
XML-style tags are the best way to add custom markup to Muse.
This is easily accomplished by customizing this list of markup tags.

For each entry, the name of the tag is given, whether it expects
a closing tag and/or an optional set of attributes, and a
function that performs whatever action is desired within the
delimited region.

The tags themselves are deleted during publishing, before the
function is called.  The function is called with three arguments,
the beginning and end of the region surrounded by the tags.  If
properties are allowed, they are passed as a third argument in
the form of an alist.  The `end' argument to the function is
always a marker.

Point is always at the beginning of the region within the tags, when
the function is called.  Wherever point is when the function finishes
is where tag markup will resume.

These tag rules are processed once at the beginning of markup, and
once at the end, to catch any tags which may have been inserted
in-between."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       function))
  :group 'muse-publish)

(defcustom muse-publish-markup-specials nil
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-publish)

(defvar muse-publishing-p nil
  "Set to t while a page is being published.")
(defvar muse-batch-publishing-p nil
  "Set to t while a page is being batch published.")
(defvar muse-publishing-styles nil)
(defvar muse-publishing-current-file nil)
(defvar muse-publishing-current-style nil)
(defvar muse-publishing-directives nil
  "An alist of publishing directives from the top of a file.")
(defvar muse-publish-generate-contents nil
  "Non-nil if a table of contents should be generated.
If non-nil, it is a cons cell specifying (MARKER . DEPTH), to
tell where the <contents> was seen, and to what depth the
contents were requested.")

;; Functions for handling style information

(defsubst muse-style (&optional style)
  "Resolve the given STYLE into a Muse style, if it is a string."
  (if (null style)
      muse-publishing-current-style
    (if (stringp style)
        (assoc style muse-publishing-styles)
      (muse-assert (consp style))
      style)))

(defun muse-define-style (name &rest elements)
  (let ((entry (assoc name muse-publishing-styles)))
    (if entry
        (error "There is already a style named %s." name)
      (setq muse-publishing-styles
            (cons (append (list name) elements)
                  muse-publishing-styles)))))

(defun muse-derive-style (new-name base-name &rest elements)
  (let ((entry (assoc new-name muse-publishing-styles)))
    (if entry
        (error "There is already a style named %s." new-name)
      (apply 'muse-define-style new-name
             (append elements (list :base base-name))))))

(defsubst muse-get-keyword (keyword list &optional direct)
  (let ((value (cadr (memq keyword list))))
    (if (and (not direct) (symbolp value))
        (symbol-value value)
      value)))

(defsubst muse-style-element (elem &optional style direct)
  (setq style (muse-style style))
  (let ((value (muse-get-keyword elem style direct)))
    (if value
        value
      (let ((base (muse-get-keyword :base style)))
        (if base
            (muse-style-element elem base direct))))))

(defun muse-find-markup-element (keyword ident style)
  (let ((def (assq ident (muse-style-element keyword style))))
    (if def
        (cdr def)
      (let ((base (muse-style-element :base style)))
        (if base
            (muse-find-markup-element keyword ident base))))))

(defsubst muse-markup-text (ident &rest args)
  (let ((text (muse-find-markup-element :strings ident (muse-style))))
    (if (and text args)
        (apply 'format text args)
      (or text ""))))

(defun muse-find-markup-tag (keyword tagname style)
  (let ((def (assoc tagname (muse-style-element keyword style))))
    (or def
        (let ((base (muse-style-element :base style)))
          (if base
              (muse-find-markup-tag keyword tagname base))))))

(defsubst muse-markup-tag-info (tagname &rest args)
  (let ((tag-info (muse-find-markup-tag :tags tagname (muse-style))))
    (or tag-info
        (assoc (match-string 1) muse-publish-markup-tags))))

(defsubst muse-markup-function (category)
  (let ((func (muse-find-markup-element :functions category (muse-style))))
    (or func
        (cdr (assq category muse-publish-markup-functions)))))

;; Publishing routines

(defun muse-publish-markup (name rules)
  (let* ((case-fold-search nil)
         (inhibit-read-only t)
         (limit (* (length rules) (point-max)))
         (verbose (and muse-publish-report-threshhold
                       (> (point-max) muse-publish-report-threshhold)))
         (base 0))
    (while rules
      (goto-char (point-min))
      (let ((regexp (nth 1 (car rules)))
            (group (nth 2 (car rules)))
            (repl (nth 3 (car rules)))
            last-pos pos)
        (if (symbolp regexp)
            (setq regexp (symbol-value regexp)))
        (if (and verbose (not muse-batch-publishing-p))
            (message "Publishing %s...%d%%" name
                     (* (/ (float (+ (point) base)) limit) 100)))
        (while (and regexp (setq pos (re-search-forward regexp nil t)))
          (if (and verbose (not muse-batch-publishing-p))
              (message "Publishing %s...%d%%" name
                       (* (/ (float (+ (point) base)) limit) 100)))
          (unless (get-text-property (match-beginning group) 'read-only)
            (let* (func
                   (text (cond
                          ((and (symbolp repl)
                                (setq func (muse-markup-function repl)))
                           (funcall func))
                          ((functionp repl)
                           (funcall repl))
                          ((symbolp repl)
                           (symbol-value repl))
                          (t repl))))
              (if text
                  (replace-match text t))))
          (if (and last-pos (= pos last-pos))
              (if (eobp)
                  (setq regexp nil)
                (forward-char 1)))
          (setq last-pos pos)))
      (setq rules (cdr rules)
            base (+ base (point-max))))
    (if (and verbose (not muse-batch-publishing-p))
        (message "Publishing %s...done" name))))

(defun muse-insert-file-or-string (file-or-string &optional title)
  (let ((beg (point)) end)
    (if (and (not (string-equal file-or-string ""))
             (file-readable-p file-or-string))
        (setq end (+ beg (cadr (insert-file-contents file-or-string))))
      (insert file-or-string)
      (setq end (point)))
    (save-restriction
      (narrow-to-region beg end)
      (muse-publish-markup (or title "")
                           '((100 "<\\(lisp\\)>" 0
                              muse-publish-markup-tag))))))

(defun muse-style-run-hooks (keyword style &rest args)
  (let (handled)
    (while (and style (not handled))
      (setq style (muse-style style))
      (let ((func (muse-get-keyword keyword style t)))
        (if func
            (if (apply func args)
                (setq handled t))))
      (unless handled
        (setq style (muse-style-element :base style))))
    handled))

(defun muse-publish-markup-region (beg end title style)
  "Apply the given STYLE's markup rules to the given region."
  (save-restriction
    (narrow-to-region beg end)
    (muse-style-run-hooks :before style)
    (muse-publish-markup
     title
     (sort (copy-alist (append muse-publish-markup-regexps
                               (muse-style-element :regexps style)))
           (function
            (lambda (l r)
              (< (car l) (car r))))))
    (muse-style-run-hooks :before-end style)))

(defun muse-publish-markup-buffer (title style)
  "Apply the given STYLE's markup rules to the current buffer."
  (setq style (muse-style style))
  (let ((style-header (muse-style-element :header style))
        (style-footer (muse-style-element :footer style))
        (muse-publishing-current-style style)
        (muse-publishing-directives
         (list (cons "title" title)
               (cons "author" (user-full-name))
               (cons "date" (format-time-string "%B %e, %Y"))))
        (muse-publishing-p t))
    (run-hooks 'muse-before-publish-hook)
    (muse-publish-markup-region (point-min) (point-max) title style)
    (goto-char (point-min))
    (if style-header (muse-insert-file-or-string style-header title))
    (goto-char (point-max))
    (if style-footer (muse-insert-file-or-string style-footer title))
    (muse-style-run-hooks :after style)
    (run-hooks 'muse-after-publish-hook)))

(defun muse-publish-markup-string (string &optional style)
  "Markup STRING using the given STYLE's markup rules."
  (setq style (muse-style style))
  (with-temp-buffer
    (insert string)
    (let ((muse-publishing-current-style style)
          (muse-publishing-p t))
      (muse-publish-markup "*string*" (muse-style-element :rules style)))
    (buffer-string)))

;; Commands for publishing files

(defsubst muse-publish-get-style ()
  (if (= 1 (length muse-publishing-styles))
      (car muse-publishing-styles)
    (assoc (completing-read "Publish with style: "
                            muse-publishing-styles nil t)
           muse-publishing-styles)))

(defsubst muse-publish-get-output-dir (style)
  (let ((default-directory (or (muse-style-element :path style)
                               default-directory)))
    (read-file-name "Publish to directory: " nil default-directory)))

(defsubst muse-publish-get-info ()
  (let ((style (muse-publish-get-style)))
    (list style (muse-publish-get-output-dir style)
          current-prefix-arg)))

(defsubst muse-publish-output-name (&optional file style)
  (setq style (muse-style style))
  (concat (muse-style-element :prefix style)
          (muse-page-name file)
          (muse-style-element :suffix style)))

(defsubst muse-publish-output-file (file &optional output-dir style)
  (setq style (muse-style style))
  (if output-dir
      (expand-file-name (muse-publish-output-name file style) output-dir)
    (concat (file-name-directory file)
            (muse-publish-output-name file style))))

(defun muse-publish-file (file style &optional output-dir force)
  "Publish the given file in list FILES.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what."
  (interactive (cons (read-file-name "Publish file: ")
                     (muse-publish-get-info)))
  (setq style (muse-style style))
  (let* ((output-path (muse-publish-output-file file output-dir style))
         (output-suffix (muse-style-element :osuffix style))
         (muse-publishing-current-file file)
         (target (if output-suffix
                     (concat (file-name-sans-extension output-path)
                             output-suffix)
                   output-path)))
    (when (or force (file-newer-than-file-p file target))
      (if (and muse-publish-report-threshhold
               (> (nth 7 (file-attributes file))
                  muse-publish-report-threshhold))
          (message "Publishing %s ..." file))
      (with-temp-buffer
        (insert-file-contents file t)
        (muse-publish-markup-buffer (muse-page-name file) style)
        (let ((backup-inhibited t))
          (write-file output-path))
        (muse-style-run-hooks :final style file output-path target))
      t)))

(defun muse-publish-this-file (style output-dir &optional force)
  "Publish the page in the current file."
  (interactive (muse-publish-get-info))
  (unless (muse-publish-file buffer-file-name style output-dir force)
    (message (concat "The published version is up-to-date; use"
                     " C-u C-c C-t to force an update."))))

(defun muse-batch-publish-files ()
  "Publish Muse files in batch mode."
  (let ((muse-batch-publishing-p t)
        style-name style output-dir)
    (setq style-name (car command-line-args-left)
          style (muse-style style-name)
          command-line-args-left (cdr command-line-args-left)
          output-dir (car command-line-args-left)
          output-dir
          (if (string-match "\\`--output-dir=" output-dir)
              (prog1
                  (substring output-dir (match-end 0))
                (setq command-line-args-left (cdr command-line-args-left)))))
    (unless style
      (error "There is no style '%s' defined." style-name))
    (dolist (file command-line-args-left)
      (muse-publish-file file style output-dir t))))

;; Default publishing rules

(defun muse-publish-markup-directive (&optional name value)
  (unless name (setq name (match-string 1)))
  (unless value (setq value (match-string 2)))
  (let ((elem (assoc name muse-publishing-directives)))
    (if elem
        (setcdr elem value)
      (setq muse-publishing-directives
            (cons (cons name value)
                  muse-publishing-directives))))
  (delete-region (match-beginning 0) (match-end 0)))

(defun muse-publish-markup-anchor () "")
(defun muse-publish-markup-comment () "")

(defun muse-publish-markup-tag ()
  (let ((tag-info (muse-markup-tag-info (match-string 1))))
    (when (and tag-info
               (not (get-text-property (match-beginning 0) 'read-only)))
      (let ((closed-tag (match-string 3))
            (start (match-beginning 0))
            (beg (point)) end attrs)
        (when (nth 2 tag-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match (concat "\\([^"
                                              muse-regexp-space
                                              "=]+\\)\\(=\"\\"
                                              "([^\"]+\\)\"\\)?")
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
                (delete-region (match-beginning 0) (point))
              (setq tag-info nil)))
        (when tag-info
          (setq end (point-marker))
          (delete-region start beg)
          (goto-char start)
          (let ((args (list start end)))
            (if (nth 2 tag-info)
                (nconc args (list attrs)))
            (apply (nth 3 tag-info) args))))))
  nil)

(defun muse-publish-escape-specials (beg end)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if (get-text-property (point) 'read-only)
          (forward-char 1)
        (let ((repl
               (or (assoc (char-after) (muse-style-element :specials))
                   (assoc (char-after) muse-publish-markup-specials))))
          (if (null repl)
              (forward-char 1)
            (delete-char 1)
            (insert (cdr repl))))))))

(defun muse-publish-markup-word ()
  (let* ((beg (match-beginning 2))
         (end (1- (match-end 2)))
         (leader (buffer-substring-no-properties beg end))
         open-tag close-tag mark-read-only loc multi-line)
    (cond
     ((string= leader "_")
      (setq multi-line t)
      (setq open-tag (muse-markup-text 'begin-underline)
            close-tag (muse-markup-text 'end-underline)))
     ((string= leader "=")
      (setq open-tag (muse-markup-text 'begin-literal)
            close-tag (muse-markup-text 'end-literal))
      (setq mark-read-only t))
     (t
      (setq multi-line t)
      (let ((l (length leader)))
        (cond
         ((= l 1) (setq open-tag (muse-markup-text 'begin-emph)
                        close-tag (muse-markup-text 'end-emph)))
         ((= l 2) (setq open-tag (muse-markup-text 'begin-more-emph)
                        close-tag (muse-markup-text 'end-more-emph)))
         ((= l 3) (setq open-tag (muse-markup-text 'begin-most-emph)
                        close-tag (muse-markup-text 'end-most-emph)))))))
    (if (and (setq loc (search-forward leader nil t))
             (eq 0 (skip-syntax-forward "w" (1+ loc)))
             (or multi-line (= 1 (count-lines beg loc))))
        (progn
          (replace-match "")
          (delete-region beg end)
          (setq end (point-marker))
          (insert close-tag)
          (save-excursion
            (goto-char beg)
            (insert open-tag)
            (setq beg (point)))
          (when mark-read-only
            (muse-publish-escape-specials beg end)
            (muse-publish-mark-read-only (1- beg) (1+ end))))
      (backward-char))
    nil))

(defun muse-publish-markup-emdash ()
  (delete-region (match-beginning 0) (match-end 0))
  (insert (muse-markup-text 'emdash))
  (if (eq (char-after) ?\<)
      (insert ?\n)))

(defun muse-publish-markup-enddots ()
  (delete-region (match-beginning 0) (match-end 0))
  (insert (muse-markup-text 'enddots)))

(defun muse-publish-markup-dots ()
  (delete-region (match-beginning 0) (match-end 0))
  (insert (muse-markup-text 'dots)))

(defun muse-publish-markup-rule ()
  (delete-region (match-beginning 0) (match-end 0))
  (insert (muse-markup-text 'rule)))

(defun muse-publish-markup-heading ()
  (let* ((len (length (match-string 1)))
         (start (muse-markup-text
                 (cond ((= len 1) 'section)
                       ((= len 2) 'subsection)
                       ((= len 3) 'subsubsection))))
         (end   (muse-markup-text
                 (cond ((= len 1) 'section-end)
                       ((= len 2) 'subsection-end)
                       ((= len 3) 'subsubsection-end)))))
    (delete-region (match-beginning 0) (match-end 0))
    (insert start)
    (end-of-line)
    (if end (insert end))))

(defvar muse-publish-footnotes nil)

(defun muse-publish-markup-footnote ()
  "Scan ahead and snarf up the footnote body"
  (if (= (muse-line-beginning-position) (match-beginning 0))
      ""
    (let ((footnote (save-match-data
                      (string-to-number (match-string 1))))
          footnotemark)
      (delete-region (match-beginning 0) (match-end 0))
      (save-excursion
        (when (re-search-forward (format "^\\[%d\\]\\s-+" footnote) nil t)
          (let* ((start (match-beginning 0))
                 (beg (goto-char (match-end 0)))
                 (end (save-excursion
                        (if (search-forward "\n\n" nil t)
                            (copy-marker (match-beginning 0))
                          (goto-char (point-max))
                          (skip-chars-backward "\n")
                          (point-marker)))))
            (while (re-search-forward (concat "^["
                                              muse-regexp-blank
                                              "]+\\([^\n]\\)")
                                      end t)
              (replace-match "\\1" t))
            (let ((footnotemark-cmd (muse-markup-text 'footnotemark))
                  (footnotemark-end-cmd (muse-markup-text 'footnotemark-end)))
              (if (string= "" footnotemark-cmd)
                  (setq footnotemark
                        (concat (muse-markup-text 'footnote)
                                (buffer-substring-no-properties beg end)
                                (muse-markup-text 'footnote-end)))
                (setq footnotemark (format footnotemark-cmd footnote
                                           footnotemark-end-cmd))
                (unless muse-publish-footnotes
                  (set (make-local-variable 'muse-publish-footnotes)
                       (make-vector 256 nil)))
                (unless (aref muse-publish-footnotes footnote)
                  (setq footnotemark
                        (concat
                         footnotemark
                         (concat (format (muse-markup-text 'footnotetext)
                                         footnote)
                                 (buffer-substring-no-properties beg end)
                                 (muse-markup-text 'footnotetext-end))))
                  (aset muse-publish-footnotes footnote footnotemark))))
            (goto-char end)
            (skip-chars-forward "\n")
            (delete-region start (point)))))
      (insert (or footnotemark footnote)))))

(defun muse-publish-markup-fn-sep ()
  (delete-region (match-beginning 0) (match-end 0))
  (insert (muse-markup-text 'fn-sep)))

(defun muse-publish-surround-text (beg-tag end-tag move-func)
  (let ((beg (point)) end)
    (skip-chars-backward muse-regexp-space)
    (delete-region (point) beg)
    (insert "\n\n" beg-tag)
    (funcall move-func)
    (setq end (point-marker))
    (goto-char beg)
    (while (< (point) end)
      (if (looking-at "^\\s-+")
          (replace-match ""))
      (forward-line 1))
    (goto-char end)
    (setq beg (point))
    (skip-chars-backward muse-regexp-space)
    (delete-region (point) beg))
  (insert end-tag "\n"))

(defsubst muse-forward-paragraph (&optional pattern)
  (if (re-search-forward (if pattern
                             (concat "^\\(" pattern "["
                                     muse-regexp-blank
                                     "]+\\|$\\|\\'\\)")
                           "^\\s-*\\($\\|\\'\\)") nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun muse-publish-markup-list ()
  "Markup a list entry or quoted paragraph.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (let ((str (match-string 1)))
    (cond
     ((and (eq (aref str 0) ?-))
      (delete-region (match-beginning 0) (match-end 0))
      (muse-publish-surround-text
       (muse-markup-text 'begin-uli)
       (muse-markup-text 'end-uli)
       (function
        (lambda ()
          (muse-forward-paragraph (concat "["
                                          muse-regexp-blank
                                          "]+-"))))))
     ((and (>= (aref str 0) ?0)
           (<= (aref str 0) ?9))
      (delete-region (match-beginning 0) (match-end 0))
      (muse-publish-surround-text
       (muse-markup-text 'begin-oli)
       (muse-markup-text 'end-oli)
       (function
        (lambda ()
          (muse-forward-paragraph (concat "["
                                          muse-regexp-blank
                                          "]+[0-9]+\\."))))))
     (t
      (goto-char (match-beginning 1))
      (insert (muse-markup-text 'begin-ddt))
      (save-match-data
        (save-excursion
          (forward-line 1)
          (while (looking-at (concat "^\\(["
                                     muse-regexp-blank
                                     "]*\\)[^"
                                     muse-regexp-space
                                     "]"))
            (delete-region (match-beginning 1) (match-end 1))
            (forward-line 1))))
      (save-match-data
        (when (re-search-forward (concat "["
                                         muse-regexp-space
                                         "]+::["
                                         muse-regexp-space
                                         "]+")
                                 nil t)
          (replace-match (muse-markup-text 'start-dde))))
      (muse-forward-paragraph)
      (insert (muse-markup-text 'end-ddt) ?\n)))))

(defun muse-publish-markup-quote ()
  "Markup a list entry or quoted paragraph.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (let* ((ws (match-string 0))
         (centered (>= (string-width ws) 6))
         (begin-elem (if centered 'begin-center 'begin-quote))
         (end-elem (if centered 'end-center 'end-quote)))
    (muse-publish-surround-text (muse-markup-text begin-elem)
                                (muse-markup-text end-elem)
                                'muse-forward-paragraph)))

(defun muse-publish-markup-leading-space ()
  (let ((markup-space (muse-markup-text 'verse-space))
        count)
    (when (and markup-space
               (>= (setq count (skip-chars-forward " ")) 0))
      (delete-region (muse-line-beginning-position) (point))
      (while (> count 0)
        (insert markup-space)
        (setq count (- count 2))))))

(defun muse-publish-markup-verse ()
  (let ((leader (match-string 0)))
    (goto-char (match-beginning 0))
    (insert (muse-markup-text 'begin-verse))
    (while (looking-at leader)
      (replace-match "")
      (muse-publish-markup-leading-space)
      (end-of-line)
      (cond
       ((bolp)
        (let ((text (muse-markup-text 'empty-verse-line)))
          (if text (insert text))))
       ((save-excursion
          (save-match-data
            (forward-line 1)
            (or (looking-at (concat leader "["
                                    muse-regexp-blank
                                    "]*$"))
                (not (looking-at leader)))))
        (let ((text (muse-markup-text 'last-stanza-end)))
          (if text (insert text))))
       (t
        (let ((text (muse-markup-text 'end-verse-line)))
          (if text (insert text)))))
      (forward-line 1)))
  (insert (muse-markup-text 'end-verse) ?\n))

(defun muse-publish-markup-table ()
  "Style does not support tables.")

(defun muse-publish-markup-email ()
  (let* ((beg (match-end 1))
         (addr (buffer-substring-no-properties beg (match-end 0))))
    (with-temp-buffer
      (insert addr)
      (muse-publish-escape-specials (point-min) (point-max))
      (setq addr (buffer-string)))
    (goto-char beg)
    (delete-region beg (match-end 0))
    (insert (format (muse-markup-text 'email-addr) addr addr))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-escape-specials-in-string (string)
  "Escape specials in STRING using style-specific :specials."
  (save-excursion
    (apply (function concat)
           (mapcar
            (lambda (ch)
              (let ((repl
                     (or (assoc ch (muse-style-element :specials))
                         (assoc ch muse-publish-markup-specials))))
                (if (null repl)
                    (char-to-string ch)
                  (cdr repl))))
            (append string nil)))))

(defun muse-publish-url (url &optional desc explicit)
  "Resolve a URL into its final <a href> form."
  (let ((orig-url url))
    (dolist (transform muse-publish-url-transforms)
      (setq url (save-match-data (when url (funcall transform url explicit)))))
    (setq desc (when desc (muse-publish-escape-specials-in-string desc)))
    (if url
        (if (string-match muse-image-regexp url)
            (if desc
                (muse-markup-text 'image-with-desc url desc)
              (muse-markup-text 'image-link url))
          (if (and desc (string-match muse-image-regexp desc))
              (muse-markup-text 'url-with-image url desc)
            (muse-markup-text 'url-link url (or desc orig-url))))
      orig-url)))

(defun muse-publish-insert-url (url &optional desc explicit)
  "Resolve a URL into its final <a href> form."
  (delete-region (match-beginning 0) (match-end 0))
  (let ((beg (point)))
    (insert (muse-publish-url url desc explicit))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-markup-link ()
  (let (desc explicit link)
    (setq explicit (save-match-data
                     (if (string-match muse-explicit-link-regexp
                                       (match-string 0))
                         t nil)))
    (setq desc (if explicit (match-string 2) (match-string 0)))
    (setq link (if explicit
                   (muse-handle-explicit-link (match-string 1))
                 (muse-handle-implicit-link (match-string 0))))
    (when link
      (muse-publish-insert-url link desc explicit))))

(defun muse-publish-markup-url ()
  (muse-publish-insert-url (match-string 0)))

;; Default publishing tags

(defun muse-publish-contents-tag (beg end attrs)
  (set (make-local-variable 'muse-publish-generate-contents)
       (cons (copy-marker (point) t)
             (let ((depth (cdr (assoc "depth" attrs))))
               (or (and depth (string-to-number depth)) 2)))))

(defun muse-publish-verse-tag (beg end)
  (save-excursion
    (goto-char beg)
    (while (eq ?\  (char-syntax (char-after)))
      (delete-char 1))
    (while (< (point) end)
      (insert "> ")
      (forward-line))
    (if (eq ?\  (char-syntax (char-before)))
      (delete-char -1))))

(defun muse-publish-example-tag (beg end)
  (muse-publish-escape-specials beg end)
  (goto-char beg)
  (insert (muse-markup-text 'begin-example))
  (goto-char end)
  (insert (muse-markup-text 'end-example))
  (muse-publish-mark-read-only beg (point)))

(defun muse-publish-mark-read-only (beg end)
  (add-text-properties beg end '(rear-nonsticky (read-only) read-only t))
  nil)

(defun muse-publish-verbatim-tag (beg end)
  (muse-publish-escape-specials beg end)
  (muse-publish-mark-read-only beg end))

(defalias 'muse-publish-class-tag 'ignore)

(defun muse-publish-lisp-tag (beg end)
  (save-excursion
    (let ((str (muse-eval-lisp
                (prog1
                    (buffer-substring-no-properties beg end)
                  (delete-region beg end)))))
      (set-text-properties 0 (length str) nil str)
      (insert str))))

(defun muse-publish-command-tag (beg end attrs)
  (while (looking-at "\\s-*$")
    (forward-line))
  (let ((interp (cdr (assoc "interp" attrs))))
    (if (null interp)
        (shell-command
         (prog1
             (buffer-substring-no-properties (point) end)
           (delete-region beg end)) t)
      (shell-command-on-region beg end interp t t))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-comment-tag (beg end)
  (delete-region beg end))

;; Miscellaneous helper functions

(defsubst muse-publishing-directive (name)
  (cdr (assoc name muse-publishing-directives)))

(defun muse-publish-strip-tags (string)
  "Remove all tags from the string."
  (while (string-match "<.*?>" string)
    (setq string (replace-match "" nil t string)))
  string)

(defun muse-publish-markup-type (category default-func)
  (let ((rule (muse-find-markup-element :overrides category (muse-style))))
    (funcall (or rule default-func))))

(defun muse-published-buffer-contents (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((beg (and (search-forward "Emacs Muse begins here")
                    (muse-line-end-position)))
          (end (and (search-forward "Emacs Muse ends here")
                    (muse-line-beginning-position))))
      (buffer-substring-no-properties beg end))))

(defun muse-published-contents (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (muse-published-buffer-contents (current-buffer)))))

(defun muse-publish-transform-output
  (file temp-file output-path name gen-func &rest cleanup-exts)
  "Transform the given TEMP-FILE into the OUTPUT-PATH, using GEN-FUNC."
  (setq file (muse-page-name file))
  (message "Generating %s output for %s..." name file)
  (if (not (funcall gen-func temp-file output-path))
      (message "Generating %s from %s...failed" name file)
    (message "Generating %s output for %s...done" name file)
    (muse-delete-file-if-exists temp-file)
    (dolist (ext cleanup-exts)
      (muse-delete-file-if-exists
       (expand-file-name (concat file ext)
                         (file-name-directory output-path))))
    (message "Wrote %s" output-path)))

(defun muse-publish-read-only (string)
  (let ((end (1- (length string))))
    (add-text-properties 0 end
                         '(rear-nonsticky (read-only) read-only t)
                         string)
    string))

(defun muse-publish-prepare-url (target &rest ignored)
  (save-match-data
    (unless (or (string-match muse-url-regexp target)
                (string-match muse-image-regexp target)
                (string-match muse-file-regexp target))
      (setq target (if (string-match "#" target)
                       (concat (muse-publish-output-name
                                (substring target 0 (match-beginning 0)))
                               "#" (substring target (match-end 0)))
                     (muse-publish-output-name target)))))
  target)

(provide 'muse-publish)

;;; muse-publish.el ends here
