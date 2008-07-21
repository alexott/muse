;;; muse-init.el --- Use Emacs Muse to publish ikiwiki documents

;; The code in this file may be used, distributed, and modified
;; without restriction.

;;; Setup

(add-to-list 'load-path (expand-file-name "~ikiwiki/elisp/muse/lisp"))

;; Initialize
(require 'muse)          ; load generic module
(require 'muse-html)     ; load (X)HTML publishing style

;;; Settings

;; Styles
(muse-derive-style "ikiwiki" "xhtml"
                   :header ""
                   :footer "")

;; Permitted modes for <src> to colorize
(setq muse-html-src-allowed-modes
      '("ada" "apache" "asm" "awk" "c++" "c" "cc" "change-log" "context"
        "css" "diary" "diff" "dns" "domtool" "emacs-lisp" "f90" "fortran"
        "fundamental" "html" "java" "jython" "latex" "lisp" "lua" "m4"
        "makefile" "markdown" "matlab" "maxima" "message" "modula-2" "muse"
        "nroff" "octave" "org" "outline" "pascal" "perl" "ps" "python" "rst"
        "ruby" "scheme" "sgml" "sh" "slang" "sml" "sml-cm" "sml-lex" "sml-yacc"
        "sql" "tcl" "tex" "texinfo" "xml" "zone"))
;; In case someone does <src lang="muse">
(setq muse-colors-evaluate-lisp-tags nil
      muse-colors-inline-images nil)
;; In case someone does <src lang="org">
(require 'org)
(setq org-inhibit-startup t
      org-table-formula-evaluate-inline nil)

;; Don't allow dangerous tags to be published
(setq muse-publish-enable-dangerous-tags nil)

;;; Functions

(defun muse-ikiwiki-publish (file name)
  "Publish a single file for ikiwiki.
The name of the real file is NAME, and the name of the temporary
file containing the content is FILE."
  (if (not (stringp file))
      (message "Error: No file given to publish")
    (let ((muse-batch-publishing-p t)
          (title (muse-page-name name))
          (style "ikiwiki")
          (muse-publishing-current-file file)
          (muse-publishing-current-output-path file)
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
        (when (muse-write-file output-path)
          (muse-style-run-hooks :final style file output-path target))))))

;;; Custom variables

(custom-set-variables
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-publish-comments-p t)
 '(muse-publish-date-format "%b. %e, %Y"))
(custom-set-faces)

;;; Start server

(require 'server)
(server-start)

;;; muse-init.el ends here
