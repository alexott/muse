;;; muse-init.el --- Initialize Emacs Muse

;; Author: Michael Olson

;; In order to see the scripts that I use to publish my website to a
;; remote webserver, check out
;; http://www.mwolson.org/projects/SiteScripts.html.

;;; Setup

;; Add to load path
(add-to-list 'load-path "/home/mwolson/proj/emacs/muse/mwolson/lisp")
(add-to-list 'load-path "/home/mwolson/proj/emacs/muse/mwolson/experimental")

;; Initialize
(require 'outline)       ; I like outline-style faces
(require 'muse)          ; load generic module
(require 'muse-colors)   ; load coloring/font-lock module
(require 'muse-mode)     ; load authoring mode
(require 'muse-blosxom)  ; load blosxom module
(require 'muse-docbook)  ; load DocBook publishing style
(require 'muse-html)     ; load (X)HTML publishing style
(require 'muse-latex)    ; load LaTeX/PDF publishing styles
(require 'muse-latex2png) ; publish <latex> tags
(require 'muse-texinfo)  ; load Info publishing style
(require 'muse-wiki)     ; load Wiki support
(require 'muse-xml)      ; load XML support
;;(require 'muse-message)  ; load message support (experimental)

;; Setup projects

;; Here is an example of making a customized version of your favorite
;; publisher.  All this does is run `my-muse-blosoxm-finalize' on the
;; published file immediately after saving it.
(muse-derive-style "my-blosxom" "blosxom-xhtml"
                   :final 'my-muse-blosxom-finalize)

;; This turns relative links into absolute links
(muse-derive-style "my-pdf" "pdf"
                   :before 'my-muse-pdf-prepare-buffer)

;; This uses a different header and footer than normal
(muse-derive-style "my-xhtml" "xhtml"
                   :header "~/personal-site/muse/header.html"
                   :footer "~/personal-site/muse/footer.html")

;; Here is my master project listing.

(setq muse-project-alist
      `(
        ("Website" ("~/proj/wiki/web/" "~/proj/wiki/web/testdir/"
                    :force-publish ("WikiIndex")
                    :default "WelcomePage")
         (:base "my-xhtml"
                :include "/web/[^/]+"
                :path "~/personal-site/site/web")
         (:base "my-xhtml"
                :include "/testdir/[^/]+"
                :path "~/personal-site/site/web/testdir")
         (:base "my-pdf"
                :path "~/personal-site/site/web"
                :include "/CurriculumVitae[^/]*$"))

        ("Projects" ("~/proj/wiki/projects/"
                     :force-publish ("WikiIndex")
                     :default "WelcomePage")
         (:base "my-xhtml"
                :path "~/personal-site/site/projects"))

        ("Blog" (,@(muse-project-alist-dirs "~/proj/wiki/blog")
                 :default "index")

         ;; Publish this directory and its subdirectories.  Arguments
         ;; are as follows.  The above `muse-project-alist-dirs' part
         ;; is also needed, using Argument 1.
         ;;
         ;;  1. Source directory
         ;;  2. Output directory
         ;;  3. Publishing style
         ,@(muse-project-alist-styles "~/proj/wiki/blog"
                                      "~/personal-site/site/blog"
                                      "my-blosxom"))

        ("MyNotes" ("~/proj/wiki/notes/"
                    :force-publish ("index")
                    :default "index")
         (:base "xhtml"
                :path "~/personal-site/site/notes")
         (:base "my-pdf"
                :path "~/personal-site/site/notes"))

        ("Private" ("~/Documents" "~/Documents/work-school"
                    :default "movielist")
         ,@(muse-project-alist-styles "~/Documents"
                                      "~/Documents"
                                      "pdf"))

        ("Classes" (,@(muse-project-alist-dirs "~/proj/wiki/classes")
                    :default "index")
         ,@(muse-project-alist-styles "~/proj/wiki/classes"
                                      "~/personal-site/site/classes"
                                      "xhtml"))

        ("Plans" ("~/proj/wiki/plans/"
                  :default "TaskPool"
                  :major-mode planner-mode
                  :visit-link planner-visit-link)
         (:base "planner-xhtml"
                :path "~/proj/notmine/planner-out"))
        ))

;; Wiki settings
(setq muse-wiki-interwiki-alist
      '(("PlugWiki" . "http://plug.student-orgs.purdue.edu/wiki/")
        ("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/")
        ("ArchWiki" . "http://gnuarch.org/gnuarchwiki/")
        ;; abbreviations
        ("CERIAS" . "http://www.cerias.purdue.edu/")
        ("PlannerMode" . "http://www.emacswiki.org/cgi-bin/wiki/PlannerMode")
        ("RememberMode" . "http://www.emacswiki.org/cgi-bin/wiki/RememberMode")
        ("GP2X" . "http://www.gp2x.co.uk/")
        ("UbuntuLinux" . "http://ubuntulinux.org/")
        ("PLUG" . "http://plug.student-orgs.purdue.edu/wiki/")
        ("PAC" . "http://web.ics.purdue.edu/~pac/")))

;;; Functions

;; Turn relative links into absolute ones
(defun my-muse-pdf-make-links-absolute (str &rest ignored)
  "Make relative links absolute."
  (when str
    (save-match-data
      (if (string-match "\\`[/.]+" str)
          (replace-match "http://www.mwolson.org/" nil t str)
        str))))

;; Make sure my interproject links become absolute when published in
;; PDFs
(defun my-muse-pdf-prepare-buffer ()
  (set (make-local-variable 'muse-publish-url-transforms)
       (cons 'my-muse-pdf-make-links-absolute muse-publish-url-transforms)))

;; Switch to the given project and prompt for a file
(defun my-muse-project-find-file (project)
  (interactive)
  (let ((muse-current-project (muse-project project)))
    (call-interactively 'muse-project-find-file)))

;; Determine whether we are publishing a certain kind of output
(defun my-muse-format-p (format)
  (let ((base (muse-get-keyword :base muse-publishing-current-style)))
    (when base (string-match format base))))

(defun my-muse-blosxom-finalize (file output-path target)
;;  (my-muse-prepare-entry-for-xanga output-path)
;; For now, do nothing.
  )

;; Make the current file display correctly in Xanga
;; I call this using C-c p x now.
(defun my-muse-prepare-entry-for-xanga (file)
  "Mangle FILE so that Xanga doesn't bug out, saving to X clipboard.

If FILE is not specified, use the published version of the current file."
  (interactive
   (list
    (expand-file-name (concat (muse-page-name) muse-blosxom-extension)
                      (muse-style-element
                       :path (car (muse-project-applicable-styles
                                   buffer-file-name
                                   (cddr (muse-project-of-file))))))))
  (save-match-data
    (muse-with-temp-buffer
      (insert-file-contents file)
      ;; surround first line in <h3></h3>
      (goto-char (point-min))
      (insert "<h3>")
      (end-of-line)
      (insert "</h3>")
      ;; treat example regions properly
      (let (beg end)
        (while (re-search-forward "<pre[^>]*>" nil t)
          (setq beg (match-end 0))
          (setq end (if (re-search-forward "</pre>" nil 1)
                        (match-beginning 0)
                      (point)))
          (save-restriction
            (narrow-to-region beg end)
            ;; change initial spaces to &nbsp;
            (goto-char (point-min))
            (while (re-search-forward "^ +" nil t)
              (replace-match (apply 'concat (make-list
                                             (length (match-string 0))
                                             "&nbsp;"))))
            ;; change newline to <br />
            (goto-char (point-min))
            (while (re-search-forward "\n" nil t)
              (replace-match "<br />")))))
      ;; get rid of 2 spaces together and merge lines
      (goto-char (point-min))
      (while (re-search-forward (concat "[" muse-regexp-blank "\n]+") nil t)
        (replace-match " "))
      ;; remove trailing space
      (goto-char (point-min))
      (while (re-search-forward " *</p> *" nil t)
        (replace-match "</p>"))
      ;; make relative links work
      (goto-char (point-min))
      (while (re-search-forward "href=\"[/.]+" nil t)
        (replace-match "href=\"http://www.mwolson.org/" nil t))
      ;; copy entry to clipboard
      (clipboard-kill-ring-save (point-min) (point-max))
      (message "Copied blog entry to clipboard"))))

;; Turn a word or phrase into a clickable Wikipedia link
(defun my-muse-dictize (beg end)
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (link (concat "dict:" (replace-regexp-in-string " " "_" text t t))))
    (delete-region beg end)
    (insert "[[" link "][" text "]]")))

;;; Key customizations

(global-set-key "\C-cpl" 'muse-blosxom-new-entry)
(global-set-key "\C-cpL" #'(lambda () (interactive)
                             (my-muse-project-find-file "Blog")))
(global-set-key "\C-cpi" #'(lambda () (interactive)
                             (my-muse-project-find-file "Private")))
(global-set-key "\C-cpn" #'(lambda () (interactive)
                             (my-muse-project-find-file "MyNotes")))
(global-set-key "\C-cpp" #'(lambda () (interactive)
                             (my-muse-project-find-file "Plans")))
(global-set-key "\C-cpr" #'(lambda () (interactive)
                             (my-muse-project-find-file "Projects")))
(global-set-key "\C-cps" #'(lambda () (interactive)
                             (my-muse-project-find-file "Classes")))
(global-set-key "\C-cpw" #'(lambda () (interactive)
                             (my-muse-project-find-file "Website")))
(global-set-key "\C-cpW" 'my-muse-dictize)
(global-set-key "\C-cpx" 'my-muse-prepare-entry-for-xanga)

;;; Custom variables

(custom-set-variables
 '(muse-blosxom-base-directory "~/proj/wiki/blog/")
 '(muse-colors-autogen-headings (quote outline))
 '(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-footer "~/personal-site/muse/generic-footer.html")
 '(muse-html-header "~/personal-site/muse/generic-header.html")
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />")
 '(muse-latex-header "~/personal-site/muse/header.tex")
 '(muse-mode-hook (quote (flyspell-mode footnote-mode)))
 '(muse-publish-comments-p t)
 '(muse-publish-desc-transforms (quote (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki)))
 '(muse-wiki-publish-small-title-words (quote ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page")))
 '(muse-xhtml-footer "~/personal-site/muse/generic-footer.html")
 '(muse-xhtml-header "~/personal-site/muse/generic-header.html")
 '(muse-xhtml-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />"))
(custom-set-faces
 '(muse-bad-link ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))

;;; muse-init.el ends here
