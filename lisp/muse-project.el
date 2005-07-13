;;; muse-project.el --- Handle Muse projects.

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
;; Muse Project Maintainance
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse)
(require 'muse-publish)

(defgroup muse-project nil
  "Options controlling the behavior of Muse project handling."
  :group 'muse)

(defcustom muse-before-project-publish-hook nil
  "A hook run before a project is published.
Each function is passed the project object, a cons with the format
  (PROJNAME . SETTINGS)"
  :type 'hook
  :group 'muse-project)

(defcustom muse-after-project-publish-hook nil
  "A hook run after a project is published.
Each function is passed the project object, a cons with the format
  (PROJNAME . SETTINGS)"
  :type 'hook
  :group 'muse-project)

(defun muse-project-alist-get (sym)
  "Turn `muse-project-alist' into something we can customize easily."
  (when (boundp sym)
    (let* ((val (copy-alist (symbol-value sym)))
           (head val))
      (while val
        (let ((head (car (cdar val)))
              res)
          (while head
            (cond ((stringp (car head))
                   (add-to-list 'res (car head))
                   (setq head (cdr head)))
                  ((and (symbolp (car head))
                        (or (symbolp (cadr head))
                            (stringp (cadr head))))
                   (add-to-list 'res (list (car head) (cadr head)) t)
                   (setq head (cddr head)))
                  (t
                   (setq head (cdr head)))))
          (setcdr (car val) (cons res (cdr (cdar val)))))
        (setq val (cdr val)))
      head)))

(defun muse-project-alist-set (sym val)
  "Turn customized version of `muse-project-alist' into something
Muse can make use of."
  (set sym val)
  (while val
    (let ((head (car (cdar val)))
          res)
      (while head
        (cond ((stringp (car head))
               (add-to-list 'res (car head) t))
              ((consp (car head))
               (add-to-list 'res (caar head) t)
               (add-to-list 'res (car (cdar head)) t)))
        (setq head (cdr head)))
      (setcdr (car val) (cons res (cdr (cdar val)))))
    (setq val (cdr val))))

(define-widget 'muse-project 'lazy
  "A widget that defines a Muse project."
  :tag "Project"
  :type '(cons (repeat :tag "Contents"
                    (choice
                     (string :tag "Directory")
                     (list :tag "Setting"
                      (choice :tag "Property"
                              (const :book-chapter)
                              (const :book-end)
                              (const :book-funcall)
                              (const :book-part)
                              (const :book-style)
                              (const :default)
                              (const :major-mode)
                              (const :nochapters)
                              (const :set)
                              (const :visit-link))
                      (choice :tag "Value"
                              (string) (symbol)))))
               (repeat (repeat :tag "Style"
                               (group :inline t
                                     (choice
                                      :tag "Property"
                                      (const :base)
                                      (const :base-url)
                                      (const :exclude)
                                      (const :include)
                                      (const :path))
                                     (string :tag "Value"))))))

(defcustom muse-project-alist nil
  "An alist of Muse projects.
A project defines a fileset, and a list of custom attributes for use
when publishing files in that project."
  :type '(alist
          :key-type (string :tag "Project name")
          :value-type muse-project)
  :get 'muse-project-alist-get
  :set 'muse-project-alist-set
  :group 'muse-project)

(defvar muse-project-file-alist nil
  "This variable is automagically constructed as needed.")

(defcustom muse-project-ignore-regexp
  "\\`\\(\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\)\\'"
  "A regexp matching files to be ignored in Wiki directories."
  :type 'regexp
  :group 'muse-regexp)

(defvar muse-current-project nil
  "Project we are currently visiting.")
(make-variable-buffer-local 'muse-current-project)

(defsubst muse-project (&optional project)
  "Resolve the given PROJECT into a full Muse project, if it is a string."
  (if (null project)
      (or muse-current-project
          (muse-project-of-file))
    (if (stringp project)
        (assoc project muse-project-alist)
      (muse-assert (consp project))
      project)))

(defsubst muse-project-page-file (page project &optional no-check-p)
  "Return a filename if PAGE exists within the given Muse PROJECT."
  (setq project (muse-project project))
  (cdr (assoc page (muse-project-file-alist project no-check-p))))

(defun muse-project-private-p (file)
  "Return non-nil if NAME is a private page with PROJECT."
  (unless muse-under-windows-p
    (setq file (file-truename file))
    (if (file-attributes file)  ; don't publish if no attributes exist
        (or (when (eq ?- (aref (nth 8 (file-attributes
                                       (file-name-directory file))) 7))
              (message (concat
                        "The " (file-name-directory file)
                        " directory must be readable by others"
                        " in order for its contents to be published.")))
            (eq ?- (aref (nth 8 (file-attributes file)) 7)))
      t)))

(defun muse-project-file-entries (path)
  (let* ((names (list t))
         (lnames names))
    (cond
     ((file-directory-p path)
      (dolist (file (directory-files
                     path t (when muse-file-extension
                              (concat "." muse-file-extension "\\'"))))
        (unless (or (string-match muse-project-ignore-regexp file)
                    (file-directory-p file))
          (setcdr lnames
                  (cons (cons (muse-page-name file) file) nil))
          (setq lnames (cdr lnames)))))
     ((file-readable-p path)
      (setcdr lnames
              (cons (cons (muse-page-name path) path) nil))
      (setq lnames (cdr lnames)))
     (t                                 ; regexp
      (muse-assert (file-name-directory path))
      (dolist (file (directory-files
                     (file-name-directory path) t
                     (file-name-nondirectory path)))
        (unless (string-match muse-project-ignore-regexp file)
          (setcdr lnames
                  (cons (cons (muse-page-name file) file) nil))
          (setq lnames (cdr lnames))))))
    (cdr names)))

(defun muse-project-file-alist (&optional project no-check-p)
  "Return member filenames for the given Muse PROJECT.
On UNIX, this list is only updated if one of the directories'
contents have changed.  On Windows, it is always reread from
disk."
  (setq project (muse-project project))
  (let ((file-alist (assoc (car project) muse-project-file-alist))
        last-mod)
    ;; Determine the last modified of any directory mentioned in the
    ;; project's pattern list
    (unless (or muse-under-windows-p no-check-p)
      (let ((pats (cadr project)))
        (while pats
          (if (symbolp (car pats))
              (setq pats (cddr pats))
            (let ((dir (or (and (file-directory-p (car pats)) (car pats))
                           (and (not (file-readable-p (car pats)))
                                (file-directory-p
                                 (file-name-directory (car pats)))
                                (file-name-directory (car pats))))))
              (if dir
                  (let ((mod-time (nth 5 (file-attributes dir))))
                    (if (or (null last-mod)
                            (and mod-time
                                 (muse-time-less-p last-mod mod-time)))
                        (setq last-mod mod-time)))))
            (setq pats (cdr pats))))))
    ;; Either return the currently known list, or read it again from
    ;; disk
    (if (or (and no-check-p (cadr file-alist))
            (not (or muse-under-windows-p
                     (null (cddr file-alist))
                     (null last-mod)
                     (muse-time-less-p (cddr file-alist) last-mod))))
        (cadr file-alist)
      (if file-alist
          (setcdr (cdr file-alist) last-mod)
        (setq file-alist (cons (car project) (cons nil last-mod))
              muse-project-file-alist
              (cons file-alist muse-project-file-alist)))
      ;; Read in all of the file entries
      (save-match-data
        (setcar
         (cdr file-alist)
         (let* ((names (list t))
                (pats (cadr project)))
           (while pats
             (if (symbolp (car pats))
                 (setq pats (cddr pats))
               (nconc names (muse-project-file-entries (car pats)))
               (setq pats (cdr pats))))
           (cdr names)))))))

(defun muse-project-of-file (&optional pathname)
  "Determine which project the given PATHNAME relates to.
If PATHNAME is nil, the current buffer's filename is used."
  (if (and (null pathname) muse-current-project)
      muse-current-project
    (setq pathname (or pathname
                       (and (boundp 'muse-publishing-current-file)
                            muse-publishing-current-file)
                       buffer-file-name))
    (when pathname
      (let* ((file (file-truename pathname))
             (dir  (file-name-directory file))
             (project-entry muse-project-alist)
             found)
        (save-match-data
          (while (and project-entry (not found))
            (let ((pats (car (cdar project-entry))))
              (while (and pats (not found))
                (if (symbolp (car pats))
                    (setq pats (cddr pats))
                  (let ((truename (file-truename (car pats))))
                    (if (or (string= truename file)
                            (string= truename dir)
                            (string-match truename file))
                        (setq found (car project-entry))))
                  (setq pats (cdr pats))))
              (setq project-entry (cdr project-entry)))))
        found))))

(defun muse-read-project (prompt &optional no-check-p no-assume)
  "Read a project name from the minibuffer, if it can't be figured
  out."
  (if (null muse-project-alist)
      (error "There are no Muse projects defined; see `muse-project-alist'.")
    (or (unless no-check-p
          (muse-project-of-file))
        (if (and (not no-assume)
                 (= 1 (length muse-project-alist)))
            (car muse-project-alist)
          (assoc (completing-read prompt muse-project-alist)
                 muse-project-alist)))))

(defvar muse-project-page-history nil)

(defun muse-read-project-file (project prompt &optional default)
  (let ((name (completing-read prompt (muse-project-file-alist project)
                               nil nil nil 'muse-project-page-history
                               default)))
    (cons name (muse-project-page-file name project))))

(defun muse-project-find-file (name project &optional command directory)
  "Open the Muse page given by NAME in PROJECT.
If COMMAND is non-nil, it is the function used to visit the file.
If DIRECTORY is non-nil, it is the directory in which the page
will be created if it does not already exist.  Otherwise, the
first directory within the project's fileset is used."
  (interactive
   (let* ((project (muse-read-project "Find in project: "
                                      current-prefix-arg))
          (default (muse-get-keyword :default (cadr project)))
          (entry (muse-read-project-file
                  project (if default
                              (format "Find page: (default: %s) "
                                      default)
                            "Find page: ")
                  default)))
     (list entry project)))
  (setq project (muse-project project))
  (let ((project-name (car project)))
    (unless (interactive-p)
      (setq project (muse-project project)
            name (cons name (muse-project-page-file name project))))
    ;; If we're given a relative name, open it as-is
    (if (and (car name)
             (save-match-data (string-match "\\.\\./" (car name))))
        (setcdr name (if muse-file-extension
                         (concat (car name) "." muse-file-extension)
                       (car name)))
      ;; At this point, name is (PAGE . FILE).
      (unless (cdr name)
        (let ((pats (cadr project)))
          (while (and pats (null directory))
            (if (symbolp (car pats))
                (setq pats (cddr pats))
              (if (file-directory-p (car pats))
                  (setq directory (car pats) pats nil)
                (setq pats (cdr pats))))))
        (when directory
          (let ((filename (expand-file-name
                           (if muse-file-extension
                               (concat (car name) "." muse-file-extension)
                             (car name))
                           directory)))
            (unless (file-exists-p directory)
              (make-directory directory t))
            (setcdr name filename)))))
    ;; Open the file
    (if (cdr name)
        (funcall (or command 'find-file) (cdr name))
      (error "There is no page %s in project %s."
             (car name) project-name))))

(defun muse-project-applicable-styles (file styles &optional ignore-regexp)
  "Given STYLES, return a list of the ones that are considered for FILE.
The name of a project may be used for STYLES."
  (when (stringp styles)
    (setq styles (cddr (muse-project styles))))
  (muse-assert (and file styles))
  (let (used-styles)
    (dolist (style styles)
      (let ((include-regexp (muse-style-element :include style))
            (exclude-regexp (muse-style-element :exclude style)))
        (when (and (or ignore-regexp
                       (and (null include-regexp)
                            (null exclude-regexp))
                       (if include-regexp
                           (string-match include-regexp file)
                         (not (string-match exclude-regexp file))))
                   (or (not (file-exists-p file))
                       (not (muse-project-private-p file))))
          (add-to-list 'used-styles style))))
    used-styles))

(defun muse-project-publish-file (file styles &optional force ignore-regexp)
  (setq styles (muse-project-applicable-styles file styles ignore-regexp))
  (let (published)
    (dolist (style styles)
      (let ((output-dir (muse-style-element :path style)))
        ;; ensure the publishing location is available
        (unless (file-exists-p output-dir)
          (message "Creating publishing directory %s" output-dir)
          (make-directory output-dir))
        ;; publish the member file!
        (if (muse-publish-file file style output-dir force)
            (setq published t))))
    published))

(defun muse-project-save-buffers (&optional project)
  (setq project (muse-project project))
  (map-y-or-n-p
   (function
    (lambda (buffer)
      (and (buffer-modified-p buffer)
           (not (buffer-base-buffer buffer))
           (or (buffer-file-name buffer)
               (progn
                 (set-buffer buffer)
                 (and buffer-offer-save
                      (> (buffer-size) 0))))
           (with-current-buffer buffer
             (let ((proj (muse-project-of-file)))
               (and proj (string= (car proj)
                                  (car project)))))
           (if (buffer-file-name buffer)
               (format "Save file %s? "
                       (buffer-file-name buffer))
             (format "Save buffer %s? "
                     (buffer-name buffer))))))
   (function
    (lambda (buffer)
      (set-buffer buffer)
      (save-buffer)))
   (buffer-list)
   '("buffer" "buffers" "save")
   (if (boundp 'save-some-buffers-action-alist)
       save-some-buffers-action-alist)))

(defun muse-project-publish (project &optional force)
  "Publish the pages of PROJECT that need publishing."
  (interactive (list (muse-read-project "Publish project: " nil t)
                     current-prefix-arg))
  (setq project (muse-project project))
  (let ((styles (cddr project))
        (muse-current-project project)
        published)
    ;; determine the style from the project, or else ask
    (unless styles
      (setq styles (list (muse-publish-get-style))))
    ;; prompt to save any buffers related to this project
    (muse-project-save-buffers project)
    ;; run hook before publishing begins
    (run-hook-with-args 'muse-before-project-publish-hook project)
    ;; publish all files in the project, for each style; the actual
    ;; publishing will only happen if the files are newer than the
    ;; last published output
    (dolist (pair (muse-project-file-alist project))
      (if (muse-project-publish-file (cdr pair) styles force)
          (setq published t)))
    ;; run hook after publishing ends
    (run-hook-with-args 'muse-after-project-publish-hook project)
    ;; notify the user that everything is now done
    (if published
        (message "All pages in %s have been published." (car project))
      (message "No pages in %s need publishing at this time."
               (car project)))))

(defun muse-project-batch-publish ()
  "Publish Muse files in batch mode."
  (let ((muse-batch-publishing-p t)
        force)
    (if (string= "--force" (or (car command-line-args-left) ""))
        (setq force t
              command-line-args-left (cdr command-line-args-left)))
    (if command-line-args-left
        (dolist (project command-line-args-left)
          (message "Publishing project %s ..." project)
          (muse-project-publish project force))
      (message "No projects specified."))))

(eval-when-compile
  (put 'make-local-hook 'byte-compile nil))

(defun muse-project-set-variables ()
  "Load project-specific variables."
  (let ((vars (muse-get-keyword :set (cadr muse-current-project)))
        sym custom-set var)
    (while vars
      (setq sym (car vars))
      (setq custom-set (or (get sym 'custom-set) 'set))
      (setq var (if (eq (get sym 'custom-type) 'hook)
                    (make-local-hook sym)
                  (make-local-variable sym)))
      (funcall custom-set var (car (cdr vars)))
      (setq vars (cdr (cdr vars))))))

(defun muse-project-delete-output-files (project)
  (interactive
   (list (muse-read-project "Remove all output files for project: " nil t)))
  (setq project (muse-project project))
  (let ((file-alist (muse-project-file-alist project))
        (styles (cddr project))
        output-file path)
    (dolist (entry file-alist)
      (dolist (style styles)
        (setq output-file
              (and (setq path (muse-style-element :path style))
                   (expand-file-name
                    (concat (muse-style-element :prefix style)
                            (car entry)
                            (or (muse-style-element :osuffix style)
                                (muse-style-element :suffix style)))
                    path)))
        (if output-file
            (muse-delete-file-if-exists output-file))))))

(provide 'muse-project)

;;; muse-project.el ends here
