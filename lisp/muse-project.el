;;; muse-project.el --- handle Muse projects

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

(defvar muse-project-alist-using-customize nil
  "Used internally by Muse to indicate whether `muse-project-alist'
has been modified via the customize interface.")
(make-variable-buffer-local 'muse-project-alist-using-customize)

(defmacro with-muse-project (project &rest body)
  `(progn
     (unless (muse-project ,project)
       (error "Can't find project %s" ,project))
     (with-temp-buffer
       (muse-mode)
       (setq muse-current-project (muse-project ,project))
       (muse-project-set-variables)
       ,@body)))

(put 'with-muse-project 'lisp-indent-function 0)
(put 'with-muse-project 'edebug-form-spec '(sexp body))

(defun muse-project-alist-get (sym)
  "Turn `muse-project-alist' into something we can customize easily."
  (when (boundp sym)
    (setq muse-project-alist-using-customize t)
    (let* ((val (copy-alist (symbol-value sym)))
           (head val))
      (while val
        (let ((head (car (cdar val)))
              res)
          ;; Turn settings of first part into cons cells, symbol->string
          (while head
            (cond ((stringp (car head))
                   (add-to-list 'res (car head) t)
                   (setq head (cdr head)))
                  ((symbolp (car head))
                   (add-to-list 'res (list (symbol-name (car head))
                                           (cadr head)) t)
                   (setq head (cddr head)))
                  (t
                   (setq head (cdr head)))))
          (setcdr (car val) (cons res (cdr (cdar val)))))
        (let ((styles (cdar val)))
          ;; Symbol->string in every style
          (while (cdr styles)
            (let ((head (cadr styles))
                  res)
              (while (consp head)
                (setq res (plist-put res (symbol-name (car head))
                                     (cadr head)))
                (setq head (cddr head)))
              (setcdr styles (cons res (cddr styles))))
            (setq styles (cdr styles))))
        (setq val (cdr val)))
      head)))

(defun muse-project-alist-set (sym val)
  "Turn customized version of `muse-project-alist' into something
Muse can make use of."
  (set sym val)
  (when muse-project-alist-using-customize
    ;; Make sure the unescaped version is written to .emacs
    (put sym 'saved-value (list (custom-quote val)))
    ;; Perform unescaping
    (while val
      (let ((head (car (cdar val)))
            res)
        ;; Turn cons cells into flat list, string->symbol
        (while head
          (cond ((stringp (car head))
                 (add-to-list 'res (car head) t))
                ((consp (car head))
                 (add-to-list 'res (intern (caar head)) t)
                 (add-to-list 'res (car (cdar head)) t)))
          (setq head (cdr head)))
        (setcdr (car val) (cons res (cdr (cdar val)))))
      (let ((styles (cdar val)))
        ;; String->symbol in every style
        (while (cdr styles)
          (let ((head (cadr styles))
                res)
            (while (consp head)
              (setq res (plist-put res (intern (car head))
                                   (cadr head)))
              (setq head (cddr head)))
            (setcdr styles (cons res (cddr styles))))
          (setq styles (cdr styles))))
      (setq val (cdr val)))))

(define-widget 'muse-project 'default
  "A widget that defines a Muse project."
  :format "\n%v"
  :value-create 'muse-widget-type-value-create
  :value-get 'muse-widget-child-value-get
  :value-delete 'ignore
  :match 'muse-widget-type-match
  :type '(cons :format "    %v"
               (repeat :tag "Settings" :format "%{%t%}:\n%v%i\n\n"
                       (choice
                        (string :tag "Directory")
                        (list :tag "Book function"
                              (const :tag ":book-funcall" ":book-funcall")
                              (choice (function)
                                      (sexp :tag "Unknown")))
                        (list :tag "Book part"
                              (const :tag ":book-part" ":book-part")
                              (string :tag "Name"))
                        (list :tag "Book style"
                              (const :tag ":book-style" ":book-style")
                              (string :tag "Style"))
                        (list :tag "Default file"
                              (const :tag ":default" ":default")
                              (string :tag "File"))
                        (list :tag "End of book"
                              (const :tag ":book-end" ":book-end")
                              (const t))
                        (list :tag "Force publishing"
                              (const :tag ":force-publish" ":force-publish")
                              (repeat (string :tag "File")))
                        (list :tag "Major mode"
                              (const :tag ":major-mode" ":major-mode")
                              (choice (function :tag "Mode")
                                      (sexp :tag "Unknown")))
                        (list :tag "New chapter"
                              (const :tag ":book-chapter" ":book-chapter")
                              (string :tag "Name"))
                        (list :tag "No chapters"
                              (const :tag ":nochapters" ":nochapters")
                              (const t))
                        (list :tag "Set variables"
                              (const :tag ":set" ":set")
                              (repeat (list :inline t
                                            (symbol :tag "Variable")
                                            (sexp :tag "Setting"))))
                        (list :tag "Visit links using"
                              (const :tag ":visit-link" ":visit-link")
                              (choice (function)
                                      (sexp :tag "Unknown")))))
               (repeat :tag "Styles" :format "%{%t%}:\n%v%i\n\n"
                       (set :tag "Style"
                            (list :inline t
                                  :tag "Publishing style"
                                  (const :tag ":base" ":base")
                                  (string :tag "Style"))
                            (list :inline t
                                  :tag "Base URL"
                                  (const :tag ":base-url" ":base-url")
                                  (string :tag "URL"))
                            (list :inline t
                                  :tag "Exclude matching"
                                  (const :tag ":exclude" ":exclude")
                                  (regexp))
                            (list :inline t
                                  :tag "Include matching"
                                  (const :tag ":include" ":include")
                                  (regexp))
                            (list :inline t
                                  :tag "Timestamps file"
                                  (const :tag ":timestamps" ":timestamps")
                                  (file))
                            (list :inline t
                                  :tag "Path"
                                  (const :tag ":path" ":path")
                                  (string :tag "Path"))))))

(defcustom muse-project-alist nil
  "An alist of Muse projects.
A project defines a fileset, and a list of custom attributes for use
when publishing files in that project."
  :type '(choice (const :tag "No projects defined." nil)
                 (repeat (cons :format "%{%t%}:\n\n%v"
                               :tag "Project" :indent 4
                               (string :tag "Project name")
                               muse-project))
                 (sexp :tag "Cannot parse expression"))
  :get 'muse-project-alist-get
  :set 'muse-project-alist-set
  :group 'muse-project)

;; Make it easier to specify a muse-project-alist entry

(defcustom muse-project-ignore-regexp
  (concat "\\`\\(.*\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\|,.*\\)\\'\\|"
          "/\\(CVS\\|RCS\\|\\.arch-ids\\|{arch}\\|,.*\\)\\(/\\|\\'\\)")
  "A regexp matching files to be ignored in Wiki directories."
  :type 'regexp
  :group 'muse-regexp)

(defun muse-project-recurse-directory (base)
  "Recusively retrieve all of the directories underneath BASE.
A list of these directories is returned.
Directories starting with \".\" will be ignored, as well as those
which match `muse-project-ignore-regexp'."
  (when (and (file-directory-p base)
             (not (string-match muse-project-ignore-regexp base)))
    (let (list dir)
      (dolist (file (directory-files base t "^[^.]"))
        (when (and (file-directory-p file)
                   (not (string-match muse-project-ignore-regexp file)))
          (setq dir (file-name-nondirectory file))
          (push dir list)
          (nconc list (mapcar #'(lambda (item)
                                  (concat dir "/" item))
                              (muse-project-recurse-directory file)))))
      list)))

(defun muse-project-alist-styles (entry-dir output-dir style)
  "Return a list of styles to use in a `muse-project-alist' entry.
ENTRY-DIR is the top-level directory of the project.
OUTPUT-DIR is where Muse files are published, keeping directory structure.
STYLE is the publishing style to use.

For an example of the use of this function, see
`examples/mwolson/muse-init.el' from the Muse distribution."
  (cons `(:base ,style :path ,(expand-file-name output-dir)
                :include ,(concat "/" (file-name-nondirectory entry-dir)
                                  "/[^/]+$"))
        (mapcar (lambda (dir);
                  `(:base ,style
                          :path ,(expand-file-name dir output-dir)
                          :include ,(concat "/" dir "/[^/]+$")))
                (muse-project-recurse-directory entry-dir))))

(defun muse-project-alist-dirs (entry-dir)
  "Return a list of directories to use in a `muse-project-alist' entry.
ENTRY-DIR is the top-level directory of the project.

For an example of the use of this function, see
`examples/mwolson/muse-init.el' from the Muse distribution."
  (cons (expand-file-name entry-dir)
        (mapcar (lambda (dir) (expand-file-name dir entry-dir))
                (muse-project-recurse-directory entry-dir))))

;; Constructing the file-alist

(defvar muse-project-file-alist nil
  "This variable is automagically constructed as needed.")

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
                     path t (when (and muse-file-extension
                                       (not (string= muse-file-extension "")))
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
  (let* ((file-alist (assoc (car project) muse-project-file-alist))
         (last-mod (cdr (cdr file-alist))))
    ;; Determine the last modified of any directory mentioned in the
    ;; project's pattern list
    (unless (or muse-under-windows-p no-check-p)
      (let ((pats (cadr project)))
        (while pats
          (if (symbolp (car pats))
              (setq pats (cddr pats))
            (let* ((fnd (file-name-directory (car pats)))
                   (dir (cond ((file-directory-p (car pats))
                               (car pats))
                              ((and (not (file-readable-p (car pats)))
                                    fnd
                                    (file-directory-p fnd))
                               fnd))))
              (when dir
                (let ((mod-time (nth 5 (file-attributes dir))))
                  (when (or (null last-mod)
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
    (unless pathname (setq pathname (muse-current-file)))
    (save-match-data
      (when (and pathname
                 (not (string-match muse-project-ignore-regexp pathname)))
        (let* ((file (file-truename pathname))
               (dir  (file-name-directory file))
               (project-entry muse-project-alist)
               found)
          (while (and project-entry (not found))
            (let ((pats (car (cdar project-entry))))
              (while (and pats (not found))
                (if (symbolp (car pats))
                    (setq pats (cddr pats))
                  (let ((truename (file-truename (car pats))))
                    (if (or (string= truename file)
                            (string= truename dir)
                            (string-match (regexp-quote truename) file))
                        (setq found (car project-entry))))
                  (setq pats (cdr pats))))
              (setq project-entry (cdr project-entry))))
          found)))))

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
    ;; If we're given a relative or absolute filename, open it as-is
    (if (and (car name)
             (save-match-data (string-match muse-file-regexp (car name))))
        (setcdr name (car name))
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
                           (if (and muse-file-extension
                                    (not (string= muse-file-extension "")))
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
  (when (and file styles)
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
      used-styles)))

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
    ;; last published output, or if the file is listed in
    ;; :force-publish.  Files in :force-publish will not trigger the
    ;; "All pages need to be published" message.
    (let ((forced-files (muse-get-keyword :force-publish (cadr project)))
          (file-alist (muse-project-file-alist project)))
      (dolist (pair file-alist)
        (when (muse-project-publish-file (cdr pair) styles force)
          (setq forced-files (delete (car pair) forced-files))
          (setq published t)))
      (dolist (file forced-files)
        (muse-project-publish-file (cdr (assoc file file-alist)) styles t)))
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
