;;; muse-publish.el --- base publishing implementation

;; Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc.

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

;; Yann Hodique (yann DOT hodique AT gmail DOT com) fixed an
;; unnecessary URL description transform in `muse-publish-url'.

;; Peter K. Lee (saint AT corenova DOT com) provided the
;; `muse-style-elements-list' function.

;; Jim Ottaway (j DOT ottaway AT lse DOT ac DOT uk) provided a
;; reference implementation for nested lists.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse)
(require 'muse-regexps)

(defgroup muse-publish nil
  "Options controlling the general behavior of Muse publishing."
  :group 'muse)

(defcustom muse-before-publish-hook nil
  "A hook run in the buffer to be published, before it is done."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-after-publish-hook nil
  "A hook run in the buffer to be published, after it is done."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-publish-url-transforms
  '(muse-resolve-url)
  "A list of functions used to prepare URLs for publication.
Each is passed the URL.  The transformed URL should be returned."
  :type 'hook
  :options '(muse-resolve-url)
  :group 'muse-publish)

(defcustom muse-publish-desc-transforms nil
  "A list of functions used to prepare URL desciptions for publication.
Each is passed the description.  The modified description should
be returned."
  :type 'hook
  :group 'muse-publish)

(defcustom muse-publish-comments-p nil
  "If nil, remove comments before publishing.
If non-nil, publish comments using the markup of the current style."
  :type 'boolean
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
    (1200 "\\`#\\([a-zA-Z-]+\\)\\s-+\\(.+\\)\n+" 0 directive)

    ;; markup tags
    (1300 muse-tag-regexp 0 tag)

    ;; commented lines
    (1350 "^;\\s-+\\(.+\\)" 0 comment)

    ;; prevent emphasis characters in explicit links from being marked
    (1400 muse-explicit-link-regexp 0 muse-publish-mark-link)

    ;; emphasized or literal text
    (1600 ,(concat "\\(^\\|[-[" muse-regexp-blank
                   "<('`\"\n]\\)\\(=[^=" muse-regexp-blank
                   "\n]\\|_[^_" muse-regexp-blank
                   "\n]\\|\\*+[^*" muse-regexp-blank
                   "\n]\\)")
          2 word)

    ;; headings, outline-mode style
    (1700 "^\\(\\*+\\)\\s-+" 0 heading)

    ;; ellipses
    (1800 "\\.\\.\\.\\." 0 enddots)
    (1850 "\\.\\.\\." 0 dots)

    ;; horizontal rule, or section separator
    (1900 "^----+" 0 rule)

    ;; non-breaking space
    (1950 "~~" 0 no-break-space)

    ;; beginning of footnotes section
    (2000 "^Footnotes:?\\s-*" 0 fn-sep)
    ;; footnote definition/reference (def if at beginning of line)
    (2100 "\\[\\([1-9][0-9]*\\)\\]" 0 footnote)

    ;; unnumbered List items begin with a -.  numbered list items
    ;; begin with number and a period.  definition lists have a
    ;; leading term separated from the body with ::.  centered
    ;; paragraphs begin with at least six columns of whitespace; any
    ;; other whitespace at the beginning indicates a blockquote.  The
    ;; reason all of these rules are handled here, is so that
    ;; blockquote detection doesn't interfere with indented list
    ;; members.
    (2200 ,(format muse-list-item-regexp (concat "[" muse-regexp-blank "]*"))
          0 list)

    ;; simple table markup is supported, nothing fancy.  use | to
    ;; separate cells, || to separate header cells, and ||| for footer
    ;; cells
    (2300 ,(concat "\\(\\([" muse-regexp-blank "]*\n\\)?"
                   "\\(" muse-table-line-regexp "\\(?:\n\\|\\'\\)\\)\\)+")
          0 table)

    ;; blockquote and centered text
    (2400 ,(concat "^\\([" muse-regexp-blank "]+\\).+") 0 quote)

    ;; the emdash ("--")
    (2500 ,(concat "\\(^\\|[" muse-regexp-blank "]*\\)--\\($\\|["
                   muse-regexp-blank "]*\\)")
          0 emdash)

    ;; "verse" text is indicated the same way as a quoted e-mail
    ;; response: "> text", where text may contain initial whitespace
    ;; (see below).
    (2600 ,(concat "^[" muse-regexp-blank "]*> ") 0 verse)

    ;; define anchor points
    (2700 "^\\(\\W*\\)#\\(\\S-+\\)\\s-*" 0 anchor)

    ;; replace links in the buffer (links to other pages)
    (2900 muse-explicit-link-regexp 0 link)

    ;; bare URLs
    (3000 muse-url-regexp 0 url)

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
    (no-break-space . muse-publish-markup-no-break-space)
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
  '(("contents" nil t   nil muse-publish-contents-tag)
    ("verse"    t   nil nil muse-publish-verse-tag)
    ("example"  t   nil nil muse-publish-example-tag)
    ("code"     t   nil nil muse-publish-code-tag)
    ("quote"    t   nil t   muse-publish-quote-tag)
    ("literal"  t   nil nil muse-publish-mark-read-only)
    ("verbatim" t   nil nil muse-publish-verbatim-tag)
    ("lisp"     t   t   nil muse-publish-lisp-tag)
    ("class"    t   t   nil muse-publish-class-tag)
    ("command"  t   t   nil muse-publish-command-tag)
    ("perl"     t   t   nil muse-publish-perl-tag)
    ("python"   t   t   nil muse-publish-python-tag)
    ("ruby"     t   t   nil muse-publish-ruby-tag)
    ("comment"  t   nil nil muse-publish-comment-tag)
    ("include"  nil t   nil muse-publish-include-tag)
    ("markup"   t   t   nil muse-publish-mark-up-tag))
  "A list of tag specifications, for specially marking up text.
XML-style tags are the best way to add custom markup to Muse.
This is easily accomplished by customizing this list of markup tags.

For each entry, the name of the tag is given, whether it expects
a closing tag and/or an optional set of attributes, whether it is
nestable, and a function that performs whatever action is desired
within the delimited region.

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
                       (boolean :tag "Nestable" :value nil)
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
(defvar muse-publishing-styles nil
  "The publishing styles that Muse recognizes.
This is automatically generated when loading publishing styles.")
(defvar muse-publishing-current-file nil
  "The file that is currently being published.")
(defvar muse-publishing-current-output-path nil
  "The path where the current file will be published to.")
(defvar muse-publishing-current-style nil
  "The style of the file that is currently being published.")
(defvar muse-publishing-directives nil
  "An alist of publishing directives from the top of a file.")
(defvar muse-publish-generate-contents nil
  "Non-nil if a table of contents should be generated.
If non-nil, it is a cons cell specifying (MARKER . DEPTH), to
tell where the <contents> was seen, and to what depth the
contents were requested.")
(defvar muse-publishing-last-position nil
  "Last position of the point when publishing.
This is used to make sure that publishing doesn't get stalled.")

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
        (setcdr entry elements)
      (setq muse-publishing-styles
            (cons (append (list name) elements)
                  muse-publishing-styles)))))

(defun muse-derive-style (new-name base-name &rest elements)
  (apply 'muse-define-style new-name
         (append elements (list :base base-name))))

(defsubst muse-get-keyword (keyword list &optional direct)
  (let ((value (cadr (memq keyword list))))
    (if (and (not direct) (symbolp value))
        (symbol-value value)
      value)))

(defun muse-style-elements-list (elem &optional style)
  "Return a list all references to ELEM in STYLE, including base styles.
If STYLE is not specified, use current style."
  (let (base elements)
    (while style
      (setq style (muse-style style))
      (setq elements (append elements
                             (muse-get-keyword elem style)))
      (setq style (muse-get-keyword :base style)))
    elements))

(defun muse-style-element (elem &optional style direct)
  "Search for ELEM in STYLE, including base styles.
If STYLE is not specified, use current style."
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

(defun muse-markup-text (ident &rest args)
  "Insert ARGS into the text markup associated with IDENT.
If the markup text has sections like %N%, this will be replaced
with the N-1th argument in ARGS.  After that, `format' is applied
to the text with ARGS as parameters."
  (let ((text (muse-find-markup-element :strings ident (muse-style))))
    (if (and text args)
        (progn
          (let (start repl-text)
            (while (setq start (string-match "%\\([1-9][0-9]*\\)%" text start))
              ;; escape '%' in the argument text, since we will be
              ;; using format on it
              (setq repl-text (muse-replace-regexp-in-string
                               "%" "%%"
                               (nth (1- (string-to-number
                                         (match-string 1 text))) args)
                               t t)
                    start (+ start (length repl-text))
                    text (replace-match repl-text t t text))))
          (apply 'format text args))
      (or text ""))))

(defun muse-insert-markup (&rest args)
  (let ((beg (point)))
    (apply 'insert args)
    (muse-publish-mark-read-only beg (point))))

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
            pos)
        (setq muse-publishing-last-position nil)
        (if (symbolp regexp)
            (setq regexp (symbol-value regexp)))
        (if (and verbose (not muse-batch-publishing-p))
            (message "Publishing %s...%d%%" name
                     (* (/ (float (+ (point) base)) limit) 100)))
        (while (and regexp (setq pos (re-search-forward regexp nil t)))
          (if (and verbose (not muse-batch-publishing-p))
              (message "Publishing %s...%d%%" name
                       (* (/ (float (+ (point) base)) limit) 100)))
          (unless (and (> (- (match-end 0) (match-beginning 0)) 0)
                       (match-beginning group)
                       (get-text-property (match-beginning group) 'read-only))
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
              (if (stringp text)
                  (replace-match text t))))
          (if (and muse-publishing-last-position
                   (= pos muse-publishing-last-position))
              (if (eobp)
                  (setq regexp nil)
                (forward-char 1)))
          (setq muse-publishing-last-position pos)))
      (setq rules (cdr rules)
            base (+ base (point-max))))
    (if (and verbose (not muse-batch-publishing-p))
        (message "Publishing %s...done" name))))

(defcustom muse-publish-markup-header-footer-tags
  '(("lisp"     t   t   nil muse-publish-lisp-tag)
    ("markup"   t   t   nil muse-publish-mark-up-tag))
  "Tags used when publishing headers and footers.
See `muse-publish-markup-tags' for details."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-publish)

(defun muse-insert-file-or-string (file-or-string &optional title)
  (let ((beg (point)) end)
    (if (and (not (string-equal file-or-string ""))
             (not (string-match "\n" file-or-string))
             (file-readable-p file-or-string))
        (setq end (+ beg (cadr (insert-file-contents file-or-string))))
      (insert file-or-string)
      (setq end (point)))
    (save-restriction
      (narrow-to-region beg end)
      (remove-text-properties (point-min) (point-max)
                              '(read-only nil rear-nonsticky nil))
      (goto-char (point-min))
      (let ((muse-inhibit-style-tags t))
        (muse-publish-markup (or title "")
                             '((100 muse-tag-regexp 0
                                    muse-publish-markup-tag)))))))

(defun muse-style-run-hooks (keyword style &rest args)
  (catch 'handled
    (let ((cache nil))
      (while (and style
                  (setq style (muse-style style)))
        (let ((func (muse-style-element keyword style t)))
          (when (and func
                     (not (member func cache)))
            (setq cache (cons func cache))
            (when (apply func args)
              (throw 'handled t))))
        (setq style (muse-style-element :base style))))))

(defun muse-publish-markup-region (beg end &optional title style)
  "Apply the given STYLE's markup rules to the given region.
TITLE is used when indicating the publishing progress; it may be nil."
  (unless title (setq title ""))
  (unless style
    (or (setq style muse-publishing-current-style)
        (error "Cannot find any publishing styles to use")))
  (save-restriction
    (narrow-to-region beg end)
    (muse-style-run-hooks :before style)
    (muse-publish-markup
     title
     (sort (copy-alist (append muse-publish-markup-regexps
                               (muse-style-elements-list :regexps style)))
           (function
            (lambda (l r)
              (< (car l) (car r))))))
    (muse-style-run-hooks :before-end style)
    (muse-publish-escape-specials (point-min) (point-max) nil 'document)))

(defun muse-publish-markup-buffer (title style)
  "Apply the given STYLE's markup rules to the current buffer."
  (setq style (muse-style style))
  (let ((style-header (muse-style-element :header style))
        (style-footer (muse-style-element :footer style))
        (muse-publishing-current-style style)
        (muse-publishing-directives
         (list (cons "title" title)
               (cons "author" (user-full-name))
               (cons "date" (format-time-string
                             "%B %e, %Y"
                             (if muse-publishing-current-file
                                 (nth 5 (file-attributes
                                         muse-publishing-current-file))
                               (current-time))))))
        (muse-publishing-p t)
        (inhibit-read-only t))
    (run-hooks 'muse-update-values-hook)
    (run-hooks 'muse-before-publish-hook)
    (muse-publish-markup-region (point-min) (point-max) title style)
    (goto-char (point-min))
    (when style-header
      (muse-insert-file-or-string style-header title))
    (goto-char (point-max))
    (when style-footer
      (muse-insert-file-or-string style-footer title))
    (muse-style-run-hooks :after style)
    (run-hooks 'muse-after-publish-hook)))

(defun muse-publish-markup-string (string &optional style)
  "Markup STRING using the given STYLE's markup rules."
  (setq style (muse-style style))
  (muse-with-temp-buffer
    (insert string)
    (let ((muse-publishing-current-style style)
          (muse-publishing-p t))
      (muse-publish-markup "*string*" (muse-style-element :rules style)))
    (buffer-string)))

;; Commands for publishing files

(defun muse-publish-get-style (&optional styles)
  (unless styles (setq styles muse-publishing-styles))
  (if (= 1 (length styles))
      (car styles)
    (when (catch 'different
            (let ((first (car (car styles))))
              (dolist (style (cdr styles))
                (unless (equal first (car style))
                  (throw 'different t)))))
      (setq styles (muse-collect-alist
                    styles
                    (completing-read "Publish with style: " styles nil t))))
    (if (or (= 1 (length styles))
            (not (muse-get-keyword :path (car styles))))
        (car styles)
      (setq styles (mapcar (lambda (style)
                             (cons (muse-get-keyword :path style)
                                   style))
                           styles))
      (cdr (assoc (completing-read "Publish to directory: " styles nil t)
                  styles)))))

(defsubst muse-publish-get-output-dir (style)
  (let ((default-directory (or (muse-style-element :path style)
                               default-directory)))
    (muse-read-directory-name "Publish to directory: " nil default-directory)))

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

(defsubst muse-publish-link-name (&optional file style)
  (setq style (muse-style style))
  (concat (muse-style-element :prefix style)
          (muse-page-name file)
          (or (muse-style-element :link-suffix style)
              (muse-style-element :suffix style))))

(defun muse-publish-link-file (file &optional output-dir style)
  (setq style (muse-style style))
  (if output-dir
      (let ((link-name (expand-file-name file output-dir)))
        (if (file-exists-p link-name)
            link-name
          (expand-file-name (muse-publish-link-name file style) output-dir)))
    (if (file-exists-p file)
        file
      (concat (file-name-directory file)
              (muse-publish-link-name file style)))))

(defsubst muse-publish-link-page (page)
  (if (fboundp 'muse-project-link-page)
      (muse-project-link-page page)
    (muse-publish-link-file page)))

;;;###autoload
(defun muse-publish-file (file style &optional output-dir force)
  "Publish the given FILE in a particular STYLE to OUTPUT-DIR.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what."
  (interactive (cons (read-file-name "Publish file: ")
                     (muse-publish-get-info)))
  (let ((style-name style))
    (setq style (muse-style style))
    (unless style
      (error "There is no style '%s' defined." style-name)))
  (let* ((output-path (muse-publish-output-file file output-dir style))
         (output-suffix (muse-style-element :osuffix style))
         (muse-publishing-current-file file)
         (muse-publishing-current-output-path output-path)
         (target (if output-suffix
                     (concat (muse-path-sans-extension output-path)
                             output-suffix)
                   output-path))
         (threshhold (nth 7 (file-attributes file))))
    (if (not threshhold)
        (message "Please save %s before publishing" file)
      (when (or force (file-newer-than-file-p file target))
        (if (and muse-publish-report-threshhold
                 (> threshhold
                    muse-publish-report-threshhold))
            (message "Publishing %s ..." file))
        (muse-with-temp-buffer
          (insert-file-contents file)
          (muse-publish-markup-buffer (muse-page-name file) style)
          (let ((backup-inhibited t))
            (write-file output-path))
          (muse-style-run-hooks :final style file output-path target))
        t))))

;;;###autoload
(defun muse-publish-this-file (style output-dir &optional force)
  "Publish the currently-visited file.
Prompt for both the STYLE and OUTPUT-DIR if they are not
supplied."
  (interactive (muse-publish-get-info))
  (if buffer-file-name
      (let ((muse-current-output-style (list :base (car style)
                                             :path output-dir)))
        (unless (muse-publish-file buffer-file-name style output-dir force)
          (message (concat "The published version is up-to-date; use"
                           " C-u C-c C-T to force an update."))))
    (message "This buffer is not associated with any file")))

(defun muse-batch-publish-files ()
  "Publish Muse files in batch mode."
  (let ((muse-batch-publishing-p t)
        style output-dir)
    (setq style (car command-line-args-left)
          command-line-args-left (cdr command-line-args-left)
          output-dir (car command-line-args-left)
          output-dir
          (if (string-match "\\`--output-dir=" output-dir)
              (prog1
                  (substring output-dir (match-end 0))
                (setq command-line-args-left (cdr command-line-args-left)))))
    (setq auto-mode-alist
          (delete (cons (concat "\\." muse-file-extension "\\'")
                        'muse-mode-choose-mode)
                  auto-mode-alist))
    (dolist (file command-line-args-left)
      (muse-publish-file file style output-dir t))))

;; Default publishing rules

(defun muse-publish-section-close (depth)
  "Seach forward for the closing tag of given DEPTH."
  (let (not-end)
    (save-excursion
      (while (and (setq not-end (re-search-forward
                                 (concat "^\\*\\{1," (number-to-string depth)
                                         "\\}\\s-+")
                                 nil t))
                  (get-text-property (match-beginning 0) 'read-only)))
      (if not-end
          (forward-line 0)
        (goto-char (point-max)))
      (cond ((not (eq (char-before) ?\n))
             (insert "\n\n"))
            ((not (eq (char-before (1- (point))) ?\n))
             (insert "\n")))
      (muse-insert-markup (muse-markup-text 'section-close depth))
      (insert "\n"))))

(defun muse-publish-markup-directive (&optional name value)
  (unless name (setq name (match-string 1)))
  (unless value (setq value (match-string 2)))
  (let ((elem (assoc name muse-publishing-directives)))
    (if elem
        (setcdr elem value)
      (setq muse-publishing-directives
            (cons (cons name value)
                  muse-publishing-directives))))
  ;; Make sure we don't ever try to move the point forward (past the
  ;; beginning of buffer) while we're still searching for directives.
  (setq muse-publishing-last-position nil)
  (delete-region (match-beginning 0) (match-end 0)))

(defsubst muse-publishing-directive (name)
  (cdr (assoc name muse-publishing-directives)))

(defun muse-publish-markup-anchor ()
  (unless (get-text-property (match-end 1) 'muse-link)
    (let ((text (muse-markup-text 'anchor (match-string 2))))
      (unless (string= text "")
        (save-match-data
          (skip-chars-forward (concat muse-regexp-blank "\n"))
          (muse-insert-markup text)))
      (match-string 1))))

(defun muse-publish-markup-comment ()
  (if (null muse-publish-comments-p)
      ""
    (goto-char (match-end 0))
    (muse-insert-markup (muse-markup-text 'comment-end))
    (muse-publish-mark-read-only (match-beginning 1) (match-end 1))
    (delete-region (match-beginning 0) (match-beginning 1))
    (goto-char (match-beginning 0))
    (muse-insert-markup (muse-markup-text 'comment-begin))))

(defvar muse-inhibit-style-tags nil
  "If non-nil, do not search for style-specific tags.
This is used when publishing headers and footers.")

(defun muse-publish-markup-tag ()
  (let ((tag-info (if muse-inhibit-style-tags
                      (assoc (match-string 1) muse-publish-markup-tags)
                    (muse-markup-tag-info (match-string 1)))))
    (when (and tag-info
               (not (get-text-property (match-beginning 0) 'read-only)))
      (let ((closed-tag (match-string 3))
            (start (match-beginning 0))
            (beg (point))
            end attrs)
        (when (nth 2 tag-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match (concat "\\([^"
                                              muse-regexp-blank
                                              "=\n]+\\)\\(=\"\\"
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
            (if (muse-goto-tag-end (car tag-info) (nth 3 tag-info))
                (delete-region (match-beginning 0) (point))
              (setq tag-info nil)))
        (when tag-info
          (setq end (point-marker))
          (delete-region start beg)
          (goto-char start)
          (let ((args (list start end)))
            (if (nth 2 tag-info)
                (nconc args (list attrs)))
            (let ((muse-inhibit-style-tags nil))
              ;; remove the inhibition
              (apply (nth 4 tag-info) args)))))))
  nil)

(defun muse-publish-escape-specials (beg end &optional ignore-read-only context)
  "Escape specials from BEG to END using style-specific :specials.
If IGNORE-READ-ONLY is non-nil, ignore the read-only property.
CONTEXT is used to figure out what kind of specials to escape.

The following contexts exist in Muse.
'underline  _underlined text_
'literal    =monospaced text= or <code> region (monospaced, escaped)
'emphasis   *emphasized text*
'email      email@example.com
'url        http://example.com
'url-desc   [[...][description of an explicit link]]
'image      [[image.png]]
'example    <example> region (monospaced, block context, escaped)
'verbatim   <verbatim> region (escaped)
'document   normal text"
  (let ((specials (muse-style-element :specials nil t)))
    (cond ((functionp specials)
           (setq specials (funcall specials context)))
          ((symbolp specials)
           (setq specials (symbol-value specials))))
    (if (functionp specials)
        (funcall specials beg end ignore-read-only)
      (save-excursion
        (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (if (and (not ignore-read-only)
                   (get-text-property (point) 'read-only))
              (goto-char (or (next-single-property-change (point) 'read-only)
                             (point-max)))
            (let ((repl (or (assoc (char-after) specials)
                            (assoc (char-after)
                                   muse-publish-markup-specials))))
              (if (null repl)
                  (forward-char 1)
                (delete-char 1)
                (insert-before-markers (cdr repl)))))))))))

(defun muse-publish-markup-word ()
  (let* ((beg (match-beginning 2))
         (end (1- (match-end 2)))
         (leader (buffer-substring-no-properties beg end))
         open-tag close-tag mark-read-only loc context)
    (cond
     ((string= leader "_")
      (setq context 'underline
            open-tag (muse-markup-text 'begin-underline)
            close-tag (muse-markup-text 'end-underline)))
     ((string= leader "=")
      (setq context 'literal
            open-tag (muse-markup-text 'begin-literal)
            close-tag (muse-markup-text 'end-literal))
      (setq mark-read-only t))
     (t
      (let ((l (length leader)))
        (setq context 'emphasis)
        (cond
         ((= l 1) (setq open-tag (muse-markup-text 'begin-emph)
                        close-tag (muse-markup-text 'end-emph)))
         ((= l 2) (setq open-tag (muse-markup-text 'begin-more-emph)
                        close-tag (muse-markup-text 'end-more-emph)))
         ((= l 3) (setq open-tag (muse-markup-text 'begin-most-emph)
                        close-tag (muse-markup-text 'end-most-emph)))
         (t (setq context nil))))))
    (if (and context
             (not (get-text-property beg 'muse-link))
             (setq loc (search-forward leader nil t))
             (or (eobp) (not (eq (char-syntax (char-after loc)) ?w)))
             (not (eq (char-syntax (char-before (point))) ?\ ))
             (not (get-text-property (point) 'muse-link)))
        (progn
          (replace-match "")
          (delete-region beg end)
          (setq end (point-marker))
          (muse-insert-markup close-tag)
          (goto-char beg)
          (muse-insert-markup open-tag)
          (setq beg (point))
          (when mark-read-only
            (muse-publish-escape-specials beg end t context)
            (muse-publish-mark-read-only beg end)))
      (backward-char))
    nil))

(defun muse-publish-markup-emdash ()
  (unless (get-text-property (match-beginning 0) 'muse-link)
    (let ((prespace (match-string 1))
          (postspace (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (muse-insert-markup (muse-markup-text 'emdash prespace postspace))
      (when (eq (char-after) ?\<)
        (insert ?\n)))))

(defun muse-publish-markup-enddots ()
  (unless (get-text-property (match-beginning 0) 'muse-link)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'enddots))))

(defun muse-publish-markup-dots ()
  (unless (get-text-property (match-beginning 0) 'muse-link)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'dots))))

(defun muse-publish-markup-rule ()
  (unless (get-text-property (match-beginning 0) 'muse-link)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'rule))))

(defun muse-publish-markup-no-break-space ()
  (unless (get-text-property (match-beginning 0) 'muse-link)
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup (muse-markup-text 'no-break-space))))

(defun muse-publish-markup-heading ()
  (let* ((len (length (match-string 1)))
         (start (muse-markup-text
                 (cond ((= len 1) 'section)
                       ((= len 2) 'subsection)
                       ((= len 3) 'subsubsection)
                       (t 'section-other))
                 len))
         (end   (muse-markup-text
                 (cond ((= len 1) 'section-end)
                       ((= len 2) 'subsection-end)
                       ((= len 3) 'subsubsection-end)
                       (t 'section-other-end))
                 len)))
    (delete-region (match-beginning 0) (match-end 0))
    (muse-insert-markup start)
    (end-of-line)
    (when end
      (muse-insert-markup end))
    (muse-publish-section-close len)))

(defvar muse-publish-footnotes nil)

(defun muse-publish-markup-footnote ()
  "Scan ahead and snarf up the footnote body"
  (cond
   ((get-text-property (match-beginning 0) 'muse-link)
    nil)
   ((= (muse-line-beginning-position) (match-beginning 0))
    "")
   (t
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
            (while (re-search-forward
                    (concat "^[" muse-regexp-blank "]+\\([^\n]\\)")
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
      (muse-insert-markup (or footnotemark footnote))))))

(defun muse-publish-markup-fn-sep ()
  (delete-region (match-beginning 0) (match-end 0))
  (muse-insert-markup (muse-markup-text 'fn-sep)))

(defun muse-insert-markup-end-list (&rest args)
  (let ((beg (point)))
    (apply 'insert args)
    (add-text-properties beg (point) '(end-list t))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-determine-dl-indent (continue indent-sym determine-sym)
  ;; If the caller doesn't know how much indentation to use, figure it
  ;; out ourselves.  It is assumed that `muse-forward-list-item' has
  ;; been called just before this to set the match data.
  (when (and continue
             (symbol-value determine-sym))
    (save-match-data
      ;; snarf all leading whitespace
      (let ((indent (and (match-beginning 2)
                         (buffer-substring (match-beginning 1)
                                           (match-beginning 2)))))
        (when (and indent
                   (not (string= indent "")))
          (set indent-sym indent)
          (set determine-sym nil))))))

(defun muse-publish-surround-dl (indent post-indent)
  (let* ((beg-item (muse-markup-text 'begin-dl-item))
         (end-item (muse-markup-text 'end-dl-item))
         (beg-ddt (muse-markup-text 'begin-ddt)) ;; term
         (end-ddt (muse-markup-text 'end-ddt))
         (beg-dde (muse-markup-text 'begin-dde)) ;; definition
         (end-dde (muse-markup-text 'end-dde))
         (continue t)
         def-on-same-line beg)
    (while continue
      ;; envelope this as one term+definitions unit -- HTML does not
      ;; need this, but DocBook and Muse's custom XML format do
      (muse-insert-markup beg-item)
      (when (looking-at muse-dl-term-regexp)
        ;; find the term and wrap it with published markup
        (setq beg (point))
        (goto-char (match-end 1))
        (delete-region (point) (match-end 0))
        (muse-insert-markup-end-list end-ddt)
        ;; if definition is immediately after term, move to next line
        (unless (eq (char-after) ?\n)
          (insert ?\n))
        (save-excursion
          (goto-char beg)
          (delete-region (point) (match-beginning 1))
          (muse-insert-markup beg-ddt)))
      (setq beg (point)
            ;; move past current item
            continue (muse-forward-list-item 'dl-term indent))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char (point-min))
        ;; publish each definition that we find, defaulting to an
        ;; empty definition if none are found
        (muse-publish-surround-text beg-dde end-dde
         (lambda (indent)
           (muse-forward-list-item 'dl-entry indent))
         indent post-indent
         #'muse-publish-determine-dl-indent)
        (goto-char (point-max))
        (skip-chars-backward (concat muse-regexp-blank "\n"))
        (muse-insert-markup-end-list end-item)
        (when continue
          (goto-char (point-max)))))))

(defun muse-publish-strip-list-indentation (list-item empty-line indent post-indent)
  (let ((list-nested nil)
        (indent-found nil))
    (while (< (point) (point-max))
      (when (and (looking-at list-item)
                 (not (or (get-text-property
                           (muse-list-item-critical-point) 'read-only)
                          (get-text-property
                           (muse-list-item-critical-point) 'muse-link))))
        ;; if we encounter a list item, allow no post-indent space
        (setq list-nested t))
      (when (and (not (looking-at empty-line))
                 (looking-at (concat indent "\\("
                                     (or (and list-nested "")
                                         post-indent)
                                     "\\)")))
        ;; if list is not nested, remove indentation
        (unless indent-found
          (setq post-indent (match-string 1)
                indent-found t))
        (replace-match ""))
      (forward-line 1))))

(defun muse-publish-surround-text (beg-tag end-tag move-func &optional indent post-indent determine-indent-func list-item)
  (unless list-item
    (setq list-item (format muse-list-item-regexp
                            (concat "[" muse-regexp-blank "]*"))))
  (let ((continue t)
        (empty-line (concat "^[" muse-regexp-blank "]*\n"))
        (determine-indent (if determine-indent-func t nil))
        (new-indent indent)
        (first t)
        beg)
    (unless indent
      (setq indent (concat "[" muse-regexp-blank "]+")))
    (if post-indent
        (setq post-indent (concat " \\{0," (number-to-string post-indent)
                                  "\\}"))
      (setq post-indent ""))
    (while continue
      (muse-insert-markup beg-tag)
      (setq beg (point)
            ;; move past current item; continue is non-nil if there
            ;; are more like items to be processed
            continue (if (and determine-indent-func first)
                         (funcall move-func (concat indent post-indent))
                       (funcall move-func indent)))
      (when determine-indent-func
        (funcall determine-indent-func continue 'new-indent 'determine-indent))
      (when continue
          ;; remove list markup if we encountered another item of the
          ;; same type
          (replace-match "" t t nil 1))
      (save-restriction
        (narrow-to-region beg (point))
        ;; narrow to current item
        (goto-char (point-min))
        (forward-line 1)
        (muse-publish-strip-list-indentation list-item empty-line
                                             indent post-indent)
        (skip-chars-backward (concat muse-regexp-blank "\n"))
        (muse-insert-markup-end-list end-tag)
        (when determine-indent-func
          (setq indent new-indent))
        (when first
          (setq first nil))
        (when continue
          (goto-char (point-max)))))))

(defun muse-publish-markup-list ()
  "Markup a list entry.
This function works by marking up items of the same list level
and type, respecting the end-of-list property."
  (let* ((str (match-string 1))
         (type (muse-list-item-type str))
         (indent (buffer-substring (muse-line-beginning-position)
                                   (match-beginning 1)))
         (post-indent (length str))
         (last (match-beginning 0)))
    (cond
     ((or (get-text-property (muse-list-item-critical-point) 'read-only)
          (get-text-property (muse-list-item-critical-point) 'muse-link))
      nil)
     ((eq type 'ul)
      (unless (eq (char-after (match-end 1)) ?-)
        (delete-region (match-beginning 0) (match-end 0))
        (muse-insert-markup (muse-markup-text 'begin-uli))
        (save-excursion
          (muse-publish-surround-text
           (muse-markup-text 'begin-uli-item)
           (muse-markup-text 'end-uli-item)
           (lambda (indent)
             (muse-forward-list-item 'ul indent))
           indent post-indent)
          (muse-insert-markup-end-list (muse-markup-text 'end-uli)))
        (forward-line 1)))
     ((eq type 'ol)
      (delete-region (match-beginning 0) (match-end 0))
      (muse-insert-markup (muse-markup-text 'begin-oli))
      (save-excursion
        (muse-publish-surround-text
         (muse-markup-text 'begin-oli-item)
         (muse-markup-text 'end-oli-item)
         (lambda (indent)
           (muse-forward-list-item 'ol indent))
         indent post-indent)
        (muse-insert-markup-end-list (muse-markup-text 'end-oli)))
      (forward-line 1))
     ((not (string= (match-string 2) ""))
      ;; must have an initial term
      (goto-char (match-beginning 0))
      (muse-insert-markup (muse-markup-text 'begin-dl))
      (save-excursion
        (muse-publish-surround-dl indent post-indent)
        (muse-insert-markup-end-list (muse-markup-text 'end-dl)))
      (forward-line 1))))
  nil)

(defun muse-publish-markup-quote ()
  "Markup a quoted paragraph.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (let* ((ws (match-string 1))
         (centered (>= (string-width ws) 6))
         (begin-elem (if centered 'begin-center 'begin-quote-item))
         (end-elem (if centered 'end-center 'end-quote-item)))
    (replace-match "" t t nil 1)
    (unless centered
      (muse-insert-markup (muse-markup-text 'begin-quote)))
    (muse-publish-surround-text (muse-markup-text begin-elem)
                                (muse-markup-text end-elem)
                                (function (lambda (indent)
                                            (muse-forward-paragraph)
                                            nil)))
    (unless centered
      (muse-insert-markup (muse-markup-text 'end-quote)))))

(defun muse-publish-markup-leading-space (markup-space multiple)
  (let (count)
    (when (and markup-space
               (>= (setq count (skip-chars-forward " ")) 0))
      (delete-region (muse-line-beginning-position) (point))
      (while (> count 0)
        (muse-insert-markup markup-space)
        (setq count (- count multiple))))))

(defun muse-publish-markup-verse ()
  (let ((leader (match-string 0)))
    (goto-char (match-beginning 0))
    (muse-insert-markup (muse-markup-text 'begin-verse))
    (while (looking-at leader)
      (replace-match "")
      (muse-publish-markup-leading-space (muse-markup-text 'verse-space) 2)
      (let ((beg (point)))
        (end-of-line)
        (cond
         ((bolp)
          (let ((text (muse-markup-text 'empty-verse-line)))
            (when text (muse-insert-markup text))))
         ((save-excursion
            (save-match-data
              (forward-line 1)
              (or (looking-at (concat leader "["
                                      muse-regexp-blank
                                      "]*$"))
                  (not (looking-at leader)))))
          (let ((begin-text (muse-markup-text 'begin-last-stanza-line))
                (end-text (muse-markup-text 'end-last-stanza-line)))
            (when end-text (muse-insert-markup end-text))
            (goto-char beg)
            (when begin-text (muse-insert-markup begin-text))
            (end-of-line)))
         (t
          (let ((begin-text (muse-markup-text 'begin-verse-line))
                (end-text (muse-markup-text 'end-verse-line)))
            (when end-text (muse-insert-markup end-text))
            (goto-char beg)
            (when begin-text (muse-insert-markup begin-text))
            (end-of-line))))
        (forward-line 1))))
  (muse-insert-markup (muse-markup-text 'end-verse))
  (insert ?\n))

(defun muse-publish-table-fields (beg end)
  "Parse given region as a table, returning a cons cell.
The car is the length of the longest row.

The cdr is a list of the fields of the table, with the first
element indicating the type of the row:
  1: body, 2: header, 3: footer.

The existing region will be removed, except for initial blank lines."
  (unless (muse-publishing-directive "disable-tables")
    (let ((longest 0)
          (left 0)
          fields field-list)
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (looking-at (concat "^[" muse-regexp-blank "]*$"))
          (forward-line 1))
        (setq beg (point))
        (while (= left 0)
          (when (looking-at muse-table-line-regexp)
            (setq fields (cons (length (match-string 1))
                               (mapcar #'muse-trim-whitespace
                                       (split-string (match-string 0)
                                                     muse-table-field-regexp)))
                  field-list (cons fields field-list)
                  longest (max (length fields) longest)))
          (setq left (forward-line 1))))
      (delete-region beg end)
      (if (= longest 0)
          (cons 0 nil)
        (cons (1- longest) (nreverse field-list))))))

(defun muse-publish-markup-table ()
  "Style does not support tables.")

(defun muse-publish-escape-specials-in-string (string &optional context)
  "Escape specials in STRING using style-specific :specials.
CONTEXT is used to figure out what kind of specials to escape.

See the documentation of the `muse-publish-escape-specials'
function for the list of available contexts."
  (unless string
    (setq string ""))
  (let ((specials (muse-style-element :specials nil t)))
    (cond ((functionp specials)
           (setq specials (funcall specials context)))
          ((symbolp specials)
           (setq specials (symbol-value specials))))
    (if (functionp specials)
        (funcall specials string)
      (apply (function concat)
             (mapcar
              (lambda (ch)
                (let ((repl (or (assoc ch specials)
                                (assoc ch muse-publish-markup-specials))))
                  (if (null repl)
                      (char-to-string ch)
                    (cdr repl))))
              (append string nil))))))

(defun muse-publish-markup-email ()
  (let* ((beg (match-end 1))
         (addr (buffer-substring-no-properties beg (match-end 0))))
    (setq addr (muse-publish-escape-specials-in-string addr 'email))
    (goto-char beg)
    (delete-region beg (match-end 0))
    (if (or (eq (char-before (match-beginning 0)) ?\")
            (eq (char-after (match-end 0)) ?\"))
        (insert addr)
      (insert (format (muse-markup-text 'email-addr) addr addr)))
    (muse-publish-mark-read-only beg (point))))

(defun muse-publish-classify-url (target)
  "Transform anchors and get published name, if TARGET is a page.
The return value is a cons cell.  The car is the type of link,
the cadr is the page name, and the cddr is the anchor."
  (save-match-data
    (cond ((or (null target) (string= target ""))
           nil)
          ((string-match muse-url-regexp target)
           (cons 'url (cons target nil)))
          ((string-match muse-image-regexp target)
           (cons 'image (cons target nil)))
          ((string-match muse-file-regexp target)
           (cons 'file (cons target nil)))
          ((string-match "#" target)
           (if (eq (aref target 0) ?\#)
              (cons 'anchor-ref (cons nil (substring target 1)))
             (cons 'link-and-anchor
                   (cons (muse-publish-link-page
                          (substring target 0 (match-beginning 0)))
                         (substring target (match-end 0))))))
          (t
           (cons 'link (cons (muse-publish-link-page target) nil))))))

(defun muse-publish-url-desc (desc explicit)
  (when desc
    (dolist (transform muse-publish-desc-transforms)
      (setq desc (save-match-data
                   (when desc (funcall transform desc explicit)))))
    (setq desc (muse-link-unescape desc))
    (muse-publish-escape-specials-in-string desc 'url-desc)))

(defun muse-publish-url (url &optional desc orig-url explicit)
  "Resolve a URL into its final <a href> form."
  (let (type anchor)
    (dolist (transform muse-publish-url-transforms)
      (setq url (save-match-data (when url (funcall transform url explicit)))))
    (if desc
        (setq desc (muse-publish-url-desc desc explicit))
      (if orig-url
          (setq orig-url (muse-publish-url-desc orig-url explicit))))
    (let ((target (muse-publish-classify-url url)))
      (setq type (car target)
            url (if (eq type 'image)
                    (muse-publish-escape-specials-in-string (cadr target)
                                                            'image)
                  (muse-publish-escape-specials-in-string (cadr target) 'url))
            anchor (muse-publish-escape-specials-in-string
                    (cddr target) 'url)))
    (cond ((eq type 'anchor-ref)
           (muse-markup-text 'anchor-ref anchor (or desc orig-url)))
          ((string= url "")
           desc)
          ((eq type 'image)
           (let ((ext (or (file-name-extension url) "")))
             (setq url (muse-path-sans-extension url))
             (if desc
                 (muse-markup-text 'image-with-desc url ext desc)
               (muse-markup-text 'image url ext))))
          ((eq type 'link-and-anchor)
           (muse-markup-text 'link-and-anchor url anchor
                             (or desc orig-url)))
          ((and desc (string-match muse-image-regexp desc))
           (let ((ext (or (file-name-extension desc) "")))
             (setq desc (muse-path-sans-extension desc))
             (muse-markup-text 'image-link url desc ext)))
          ((eq type 'link)
           (muse-markup-text 'link url (or desc orig-url)))
          (t
           (or (and (or desc
                        (not (string= url orig-url)))
                    (let ((text (muse-markup-text 'url-and-desc url
                                                  (or desc orig-url))))
                      (and (not (string= text ""))
                           text)))
               (muse-markup-text 'url url (or desc orig-url)))))))

(defun muse-publish-insert-url (url &optional desc orig-url explicit)
  "Resolve a URL into its final <a href> form."
  (delete-region (match-beginning 0) (match-end 0))
  (let ((text (muse-publish-url url desc orig-url explicit)))
    (when text
      (muse-insert-markup text))))

(defun muse-publish-markup-link ()
  (let (desc explicit orig-link link)
    (setq explicit (save-match-data
                     (if (string-match muse-explicit-link-regexp
                                       (match-string 0))
                         t nil)))
    (setq orig-link (if explicit (match-string 1) (match-string 0)))
    (setq desc (when explicit (match-string 2)))
    (setq link (if explicit
                   (muse-handle-explicit-link orig-link)
                 (muse-handle-implicit-link orig-link)))
    (when (and link
               (or explicit
                   (not (or (eq (char-before (match-beginning 0)) ?\")
                            (eq (char-after (match-end 0)) ?\")))))
      ;; if explicit link has no user-provided description, treat it
      ;; as if it were an implicit link
      (when (and explicit (not desc))
        (setq explicit nil))
      (muse-publish-insert-url link desc orig-link explicit))))

(defun muse-publish-markup-url ()
  (unless (or (eq (char-before (match-beginning 0)) ?\")
              (eq (char-after (match-end 0)) ?\"))
    (let ((url (match-string 0)))
      (muse-publish-insert-url url nil url))))

;; Default publishing tags

(defcustom muse-publish-contents-depth 2
  "The number of heading levels to include with <contents> tags."
  :type 'integer
  :group 'muse-publish)

(defun muse-publish-contents-tag (beg end attrs)
  (set (make-local-variable 'muse-publish-generate-contents)
       (cons (copy-marker (point) t)
             (let ((depth (cdr (assoc "depth" attrs))))
               (or (and depth (string-to-number depth))
                   muse-publish-contents-depth)))))

(defun muse-publish-verse-tag (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (eq ?\  (char-syntax (char-after)))
        (delete-char 1))
      (while (< (point) (point-max))
        (insert "> ")
        (forward-line))
      (if (eq ?\  (char-syntax (char-before)))
          (delete-char -1)))))

(defun muse-publish-mark-read-only (beg end)
  "Add read-only properties to the given region."
  (add-text-properties beg end '(rear-nonsticky (read-only) read-only t))
  nil)

(defun muse-publish-mark-link (&optional beg end)
  "Indicate that the given region is a Muse link, so that other
markup elements respect it.  If a region is not specified, use
the 0th match data to determine it.

This is usually applied to explicit links."
  (unless beg (setq beg (match-beginning 0)))
  (unless end (setq end (match-end 0)))
  (add-text-properties beg end '(muse-link t))
  nil)

(defun muse-publish-quote-tag (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((quote-regexp "^\\(<\\(/?\\)quote>\\)"))
        (muse-insert-markup (muse-markup-text 'begin-quote))
        (while (progn
                 (unless (looking-at (concat "[" muse-regexp-blank "\n]*"
                                             "<quote>"))
                   (muse-publish-surround-text
                    (muse-markup-text 'begin-quote-item)
                    (muse-markup-text 'end-quote-item)
                    (function
                     (lambda (indent)
                       (muse-forward-paragraph)
                       (goto-char (match-end 0))
                       (and (< (point) (point-max))
                            (not (looking-at quote-regexp)))))
                    nil nil nil
                    quote-regexp))
                 (if (>= (point) (point-max))
                     t
                   (and (search-forward "<quote>" nil t)
                        (muse-goto-tag-end "quote" t)
                        (progn (forward-line 1) t)
                        (< (point) (point-max))))))
        (goto-char (point-max))
        (muse-insert-markup (muse-markup-text 'end-quote))))))

(defun muse-publish-code-tag (beg end)
  (muse-publish-escape-specials beg end nil 'literal)
  (goto-char beg)
  (insert (muse-markup-text 'begin-literal))
  (goto-char end)
  (insert (muse-markup-text 'end-literal))
  (muse-publish-mark-read-only beg (point)))

(defun muse-publish-example-tag (beg end)
  (muse-publish-escape-specials beg end nil 'example)
  (goto-char beg)
  (insert (muse-markup-text 'begin-example))
  (goto-char end)
  (insert (muse-markup-text 'end-example))
  (muse-publish-mark-read-only beg (point)))

(defun muse-publish-verbatim-tag (beg end)
  (muse-publish-escape-specials beg end nil 'verbatim)
  (muse-publish-mark-read-only beg end))

(defalias 'muse-publish-class-tag 'ignore)

(defun muse-publish-examplify-buffer ()
  "Transform the current buffer as if it were an <example> region."
  (let ((end (progn (goto-char (point-max)) (point-marker))))
    (muse-publish-example-tag (point-min) end)))

(defun muse-publish-versify-buffer ()
  "Transform the current buffer as if it were a <verse> region."
  (muse-publish-verse-tag (point-min) (point-max))
  (muse-publish-markup ""
                       `((100 ,(concat "^[" muse-regexp-blank "]*> ") 0
                              muse-publish-markup-verse)))
  (goto-char (point-min)))

(defmacro muse-publish-markup-attribute (beg end attrs reinterp &rest body)
  "Evaluate BODY within the bounds of BEG and END.
ATTRS is an alist.  Only the \"markup\" element of ATTRS is acted
on.

If it is omitted, publish the region with the normal Muse rules.
If RE-INTERP is specified, this is done immediately in a new
publishing process.  Currently, RE-INTERP is specified only by
the <include> tag.

If \"nil\", do not mark up the region at all, but prevent it from
being further interpreted by Muse.

If \"example\", treat the region as if it was surrounded by the
<example> tag.

If \"verse\", treat the region as if it was surrounded by the
<verse> tag, to preserve newlines.

Otherwise, it should be the name of a function to call in the
narrowed region after evaluating BODY."
  (let ((markup (make-symbol "markup"))
        (markup-function (make-symbol "markup-function")))
    `(let ((,markup (cdr (assoc "markup" ,attrs))))
       (save-restriction
         (narrow-to-region ,beg ,end)
         (goto-char (point-min))
         ,@body
         (if (not ,markup)
             (when ,reinterp
               (muse-publish-markup-region (point-min) (point-max))
               (muse-publish-mark-read-only (point-min) (point-max))
               (goto-char (point-max)))
           (let ((,markup-function (read ,markup)))
             (cond ((eq ,markup-function 'example)
                    (setq ,markup-function #'muse-publish-examplify-buffer))
                   ((eq ,markup-function 'verse)
                    (setq ,markup-function #'muse-publish-versify-buffer))
                   ((and ,markup-function (not (functionp ,markup-function)))
                    (error "Invalid markup function `%s'" ,markup))
                   (t nil))
             (if ,markup-function
                 (funcall ,markup-function)
               (muse-publish-mark-read-only (point-min) (point-max))
               (goto-char (point-max)))))))))

(put 'muse-publish-markup-attribute 'lisp-indent-function 4)
(put 'muse-publish-markup-attribute 'edebug-form-spec
     '(form form form form body))

(defun muse-publish-lisp-tag (beg end attrs)
  (muse-publish-markup-attribute beg end attrs nil
    (save-excursion
      (let ((str (muse-eval-lisp
                  (prog1
                      (concat "(progn "
                              (buffer-substring-no-properties (point-min)
                                                              (point-max))
                              ")")
                    (delete-region beg end)))))
        (set-text-properties 0 (length str) nil str)
        (insert str)))))

(defun muse-publish-command-tag (beg end attrs)
  (muse-publish-markup-attribute beg end attrs nil
    (while (looking-at "\\s-*$")
      (forward-line))
    (let ((interp (cdr (assoc "interp" attrs))))
      (if interp
          (shell-command-on-region (point) (point-max) interp t t)
        (shell-command
         (prog1
             (buffer-substring-no-properties (point) (point-max))
           (delete-region (point-min) (point-max)))
         t)))
    ;; make sure there is a newline at end
    (goto-char (point-max))
    (forward-line 0)
    (unless (looking-at "\\s-*$")
      (goto-char (point-max))
      (insert ?\n))
    (goto-char (point-min))))

(defun muse-publish-perl-tag (beg end attrs)
  (muse-publish-command-tag beg end
                            (cons (cons "interp" (executable-find "perl"))
                                  attrs)))

(defun muse-publish-python-tag (beg end attrs)
  (muse-publish-command-tag beg end
                            (cons (cons "interp" (executable-find "python"))
                                  attrs)))

(defun muse-publish-ruby-tag (beg end attrs)
  (muse-publish-command-tag beg end
                            (cons (cons "interp" (executable-find "ruby"))
                                  attrs)))

(defun muse-publish-comment-tag (beg end)
  (if (null muse-publish-comments-p)
      (delete-region beg end)
    (goto-char end)
    (muse-insert-markup (muse-markup-text 'comment-end))
    (muse-publish-mark-read-only beg end)
    (goto-char beg)
    (muse-insert-markup (muse-markup-text 'comment-begin))))

(defun muse-publish-include-tag (beg end attrs)
  "Include the named file at the current location during publishing.

<include file=\"...\" markup=\"...\">

The `markup' attribute controls how this file is marked up after
being inserted.  See `muse-publish-markup-attribute' for an
explanation of how it works."
  (let ((filename (cdr (assoc "file" attrs)))
        (muse-publishing-directives muse-publishing-directives))
    (if filename
        (setq filename (expand-file-name
                        filename
                        (file-name-directory muse-publishing-current-file)))
      (error "No file attribute specified in <include> tag"))
    (muse-publish-markup-attribute beg end attrs t
      (insert-file-contents filename))))

(defun muse-publish-mark-up-tag (beg end attrs)
  "Run an Emacs Lisp function on the region delimted by this tag.

<markup function=\"...\">

The optional `function' attribute controls how this section is
marked up.  If used, it should be the name of a function to call
with the buffer narrowed to the delimited region.  Note that no
further marking-up will be performed on this region.

If `function' is ommitted, use the standard Muse markup function.
This is useful for marking up content in headers and footers."
  (let ((function (cdr (assoc "function" attrs)))
        (muse-publishing-directives muse-publishing-directives))
    (if function
        (let ((markup-function (intern function)))
          (if (and markup-function (functionp markup-function))
              (save-restriction
                (narrow-to-region beg end)
                (funcall markup-function)
                (goto-char (point-max)))
            (error "Invalid markup function `%s'" function)))
      (muse-publish-markup-region beg end))
    (muse-publish-mark-read-only beg (point))))

;; Miscellaneous helper functions

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
    (muse-with-temp-buffer
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

(provide 'muse-publish)

;;; muse-publish.el ends here
