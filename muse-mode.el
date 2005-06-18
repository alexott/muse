;;; muse-mode.el --- Mode for editing Muse files; has font-lock support.

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

;; The Emacs Muse major mode is basically a hyped-up text-mode which
;; knows a lot more about the apparent structure of the document.

;;; Contributors:

;; Andrea Riciputi (ariciputi AT pito DOT com) gave an initial
;; implementation for tag completion by means of the
;; `muse-insert-tag' function.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Muse Major Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse)
(require 'muse-regexps)
(require 'muse-project)
(require 'muse-publish)

(autoload 'muse-use-font-lock "muse-colors")

(require 'derived)
(eval-when-compile
  (condition-case nil
      (require 'pcomplete)		; load if available
    (error nil)))

;;; Options:

(defgroup muse-mode nil
  "Options controlling the behavior of the Muse editing Mode.
See `muse-publish' for more information."
  :group 'muse)

(defcustom muse-mode-highlight-p t
  "If non-nil, highlight the content of Muse buffers."
  :type 'boolean
  :require 'muse-colors
  :group 'muse-mode)

(defcustom muse-mode-auto-p t
  "If non-nil, automagically determine when Muse mode should be activated."
  :type 'boolean
  :set (function
	(lambda (sym value)
	  (if value
	      (add-hook 'find-file-hooks 'muse-mode-maybe)
	    (remove-hook 'find-file-hooks 'muse-mode-maybe))
	  (set sym value)))
  :group 'muse-mode)

(defcustom muse-mode-hook nil
  "A hook that is run when Muse mode is entered."
  :type 'hook
  :options '(flyspell-mode footnote-mode turn-on-auto-fill
	     highlight-changes-mode)
  :group 'muse-mode)

(defvar muse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?a)] 'muse-index)
    (define-key map [(control ?c) (control ?b)] 'muse-browse-result)
    (define-key map [(control ?c) (control ?c)] 'muse-follow-name-at-point)
    (define-key map [(control ?c) (control ?e)] 'muse-edit-link-at-point)
    (define-key map [(control ?c) (control ?t)] 'muse-publish-this-file)
    (define-key map [(control ?c) (control ?v)] 'muse-follow-name-at-point)

    (define-key map [(control ?c) (control ?l)] 'font-lock-mode)

    (define-key map [(control ?c) ?=]           'muse-what-changed)

    (define-key map [tab] 'muse-next-reference)
    (define-key map [(control ?i)] 'muse-next-reference)

    (if (featurep 'xemacs)
	(progn
	  (define-key map [(button2)] 'muse-follow-name-at-mouse)
	  (define-key map [(shift button2)]
	    'muse-follow-name-at-mouse-other-window))
      (define-key map [(shift control ?m)]
	'muse-follow-name-at-point-other-window)
      (define-key map [mouse-2] 'muse-follow-name-at-mouse)
      (define-key map [(shift mouse-2)]
	'muse-follow-name-at-mouse-other-window))

    (if (featurep 'xemacs)
	(define-key map [(shift tab)] 'muse-previous-reference)
      (define-key map [(shift iso-lefttab)] 'muse-previous-reference)
      (define-key map [(shift control ?i)] 'muse-previous-reference))

    (define-key map [(control ?c) (control ?f)] 'muse-project-find-file)
    (define-key map [(control ?c) (control ?p)] 'muse-project-publish)

    (define-key map [(control ?c) tab] 'muse-insert-tag)
    (define-key map [(control ?c) (control ?i)] 'muse-insert-tag)

    (when (featurep 'pcomplete)
      (define-key map [(meta tab)] 'pcomplete)
      (define-key map [(meta control ?i)] 'pcomplete))

    map)
  "Keymap used by Emacs Muse mode.")

;; Code:

;;;###autoload
(define-derived-mode muse-mode text-mode "Muse"
  "Muse is an Emacs mode for authoring and publishing documents.
\\{muse-mode-map}"
  ;; because we're not inheriting from normal-mode, we need to
  ;; explicitly run file variables if the user wants to
  (condition-case err
      (hack-local-variables)
    (error (message "File local-variables error: %s"
		    (prin1-to-string err))))
  (if muse-mode-highlight-p
      (muse-use-font-lock))
  (setq muse-current-project (muse-project-of-file))
  (muse-project-set-variables)
  (when (featurep 'pcomplete)
    ;; if pcomplete is available, set it up!
    (set (make-variable-buffer-local 'pcomplete-default-completion-function)
	 'muse-mode-completions)
    (set (make-variable-buffer-local 'pcomplete-command-completion-function)
	 'muse-mode-completions)
    (set (make-variable-buffer-local 'pcomplete-parse-arguments-function)
	 'muse-mode-current-word)))

(defun muse-mode-maybe ()
  "Maybe turn Emacs Muse mode on for this file."
  (let ((project (muse-project-of-file)))
    (and project
	 (funcall (or (muse-get-keyword :major-mode (cadr project) t)
		      'muse-mode)))))

;;; Support page name completion using pcomplete

(defun muse-completions ()
  "Return a list of possible completions names for this buffer."
  (let ((project (muse-project-of-file)))
    (if project
	(while (pcomplete-here
		(mapcar 'car (muse-project-file-alist project)))))))

(defun muse-current-word ()
  (let ((end (point)))
    (save-restriction
      (save-excursion
	(skip-chars-backward (concat "^\\["
                                     muse-regexp-space))
	(narrow-to-region (point) end))
      (pcomplete-parse-buffer-arguments))))

;;; Navigate/visit links or URLs.  Use TAB, S-TAB and RET (or mouse-2).

(defun muse-link-at-point (&optional pos)
  "Return link text if a URL or Muse link name is at point."
  (let ((case-fold-search nil)
	(here (or pos (point))))
    (when (or (null pos)
	      (and (char-after pos)
		   (not (eq (char-syntax (char-after pos)) ?\ ))))
      (save-excursion
	(goto-char here)
	(skip-chars-backward (concat "^'\"<>{}("
                                     muse-regexp-space))
	(or (and (looking-at muse-url-regexp)
		 (match-string 0))
	    (and (or (looking-at muse-link-regexp)
		     (and (search-backward "[[" (line-beginning-position) t)
			  (looking-at muse-link-regexp)))
		 (<= here (match-end 0))
		 (match-string 1)))))))

(defun muse-make-link (link &optional name)
  "Return a link to LINK with NAME as the text."
  (if (and name
           link
           (not (string= name ""))
           (not (string= link name)))
      (concat "[[" (or link "") "][" name "]]")
    (concat "[[" (or link "") "]]")))

(defun muse-edit-link-at-point ()
  "Edit the current link.
Do not rename the page originally referred to."
  (interactive)
  (let (old-name)
    (if (muse-link-at-point)
        (replace-match
         (muse-make-link
          (read-string "Link: "
                       (match-string-no-properties 1))
          (read-string "Text: "
                       (match-string-no-properties 2)))
         t t)
      (error "There is no valid link at point"))))

(defun muse-visit-link (link &optional other-window)
  "Visit the URL or link named by LINK-NAME."
  (let ((visit-link-function
	 (muse-get-keyword :visit-link (cadr (muse-project-of-file)) t)))
    (if visit-link-function
	(funcall visit-link-function link other-window)
      (if (string-match muse-url-regexp link)
	  (browse-url link)
	(let (anchor)
	  (if (string-match "#" link)
	      (setq anchor (substring link (match-beginning 0))
		    link (substring link 0 (match-beginning 0))))
	  (let ((project (muse-project-of-file)))
	    (if project
		(muse-project-find-file link project
					(and other-window
					     'find-file-other-window))
	      (if other-window
		  (find-file-other-window link)
		(find-file link))))
	  (if anchor
	      (search-forward anchor nil t)))))))

(defun muse-browse-result (style &optional other-window)
  "Visit the current page's published result."
  (interactive (list (muse-publish-get-style) current-prefix-arg))
  (setq style (muse-style style))
  (let ((result-path
	 (muse-publish-output-file buffer-file-name
				   (muse-style-element :path style) style)))
    (if (not (file-readable-p result-path))
	(error "Cannot open output file '%s" result-path)
      (if other-window
	  (find-file-other-window result-path)
	(let ((func (muse-style-element :browser style t)))
	  (if func
	      (funcall func result-path)
	    (message "The publishing style %s does not support browsing."
		     style)))))))

(defun muse-follow-name-at-point (&optional other-window)
  "Visit the link at point, or insert a newline if none."
  (interactive "P")
  (let ((link (muse-link-at-point)))
    (if link
	(muse-visit-link link other-window)
      (error "There is no valid link at point"))))

(defun muse-follow-name-at-point-other-window ()
  "Visit the link at point in other window."
  (interactive)
  (muse-follow-name-at-point t))

(defun muse-follow-name-at-mouse (event &optional other-window)
  "Visit the link at point, or yank text if none."
  (interactive "eN")
  (save-excursion
    (cond ((fboundp 'event-window)      ; XEmacs
	   (set-buffer (window-buffer (event-window event)))
	   (and (funcall (symbol-function 'event-point) event)
		(goto-char (funcall (symbol-function 'event-point) event))))
	  ((fboundp 'posn-window)       ; Emacs
	   (set-buffer (window-buffer (posn-window (event-start event))))
	   (goto-char (posn-point (event-start event)))))
    (muse-follow-name-at-point other-window)))

(defun muse-follow-name-at-mouse-other-window (event)
  "Visit the link at point"
  (interactive "e")
  ;; throw away the old window position, since other-window will
  ;; change it anyway
  (select-window (car (cadr event)))
  (muse-follow-name-at-mouse event t))

(defun muse-next-reference ()
  "Move forward to next Muse link or URL, cycling if necessary."
  (interactive)
  (let ((case-fold-search nil)
	(cycled 0) pos)
    (save-excursion
      (if (muse-link-at-point)
	  (goto-char (match-end 0)))
      (while (< cycled 2)
	(if (re-search-forward
	     (concat "\\(" muse-link-regexp "\\|"
		     muse-url-regexp "\\)") nil t)
	    (setq pos (match-beginning 0)
		  cycled 2)
	  (goto-char (point-min))
	  (setq cycled (1+ cycled)))))
    (if pos
	(goto-char pos))))

(defun muse-previous-reference ()
  "Move backward to the next Muse link or URL, cycling if necessary.
This function is not entirely accurate, but it's close enough."
  (interactive)
  (let ((case-fold-search nil)
	(cycled 0) pos)
    (save-excursion
      (while (< cycled 2)
	(if (re-search-backward
	     (concat "\\(" muse-link-regexp "\\|"
		     muse-url-regexp "\\)") nil t)
	    (setq pos (point)
		  cycled 2)
	  (goto-char (point-max))
	  (setq cycled (1+ cycled)))))
    (if pos
	(goto-char pos))))

(defun muse-what-changed ()
  "Show the unsaved changes that have been made to the current file."
  (interactive)
  (diff-backup buffer-file-name))

;;; Generate an index of all known Muse pages

(defun muse-generate-index (&optional as-list exclude-private)
  "Generate an index of all Muse pages."
  (let ((index (muse-index-as-string as-list exclude-private)))
    (with-current-buffer (get-buffer-create "*Muse Index*")
      (erase-buffer)
      (insert index)
      (current-buffer))))

(defun muse-index ()
  "Display an index of all known Muse pages."
  (interactive)
  (message "Generating Muse index...")
  (let ((project (muse-project)))
    (with-current-buffer (muse-generate-index)
      (goto-char (point-min))
      (muse-mode)
      (setq muse-current-project project)
      (pop-to-buffer (current-buffer))))
  (message "Generating Muse index...done"))

(defun muse-index-as-string (&optional as-list exclude-private)
  "Generate an index of all Muse pages."
  (let ((files (sort (copy-alist (muse-project-file-alist))
		     (function
		      (lambda (l r)
			(string-lessp (car l) (car r))))))
	file)
    (with-temp-buffer
      (while files
	(unless (and exclude-private
		     (muse-project-private-p (cdar files)))
	  (insert (if as-list " - " "") "[[" (caar files) "]]\n"))
	(setq files (cdr files)))
      (buffer-string))))

;;; Insert tags interactively on C-c TAB

(defvar muse-tag-history nil
  "List of recently-entered tags; used by `muse-insert-tag'.
If you want a tag to start as the default, you may manually set
this variable to a list.")

(defvar muse-custom-tags nil
  "Keep track of any new tags entered in `muse-insert-tag'.
If there are (X)HTML tags that you use frequently with that
function, you might want to set this manually.")

(defun muse-insert-tag (tag)
  "Insert a tag interactively with a blank line after it."
  (interactive
   (list
    (completing-read
     (concat "Tag: "
             (when muse-tag-history
               (concat "(default: " (car muse-tag-history) ") ")))
     (nconc (mapcar 'car muse-publish-markup-tags)
            muse-custom-tags)
     nil nil nil 'muse-tag-history
     (car muse-tag-history))))
  (when (equal tag "")
    (setq tag (car muse-tag-history)))
  (let ((tag-entry (assoc tag muse-publish-markup-tags))
        (options ""))
    ;; Add to custom list if no entry exists
    (unless tag-entry
      (add-to-list 'muse-custom-tags tag))
    ;; Get option
    (when (nth 2 tag-entry)
      (setq options (read-string "Option: ")))
    (unless (equal options "")
      (setq options (concat " " options)))
    ;; Insert the tag, closing if necessary
    (when tag (insert (concat "<" tag options ">")))
    (when (nth 1 tag-entry)
      (insert (concat "\n\n</" tag ">\n"))
      (forward-line -2))))

(provide 'muse-mode)

;;; muse-mode.el ends here
