;;; muse-init.el --- Initialize muse-mode.

;; Hacked on by Michael Olson

;; In order to see the scripts that I use to publish my website to a
;; remote webserver, check out
;; http://www.mwolson.org/projects/SiteScripts.html.

;;; Setup

;; Add to load path
(when (fboundp 'debian-pkg-add-load-path-item)
  (debian-pkg-add-load-path-item "/stuff/proj/emacs/muse/mwolson/lisp"))

;; Initialize
(require 'outline)       ; I like outline-style faces
(require 'muse)          ; load generic module
(require 'muse-colors)   ; load coloring/font-lock module
(require 'muse-mode)     ; load authoring mode
(require 'muse-blosxom)  ; load blosxom module
(require 'muse-html)     ; load (X)HTML publishing style
(require 'muse-wiki)     ; load Wiki support
;;(require 'muse-message)  ; load message support (experimental)

;; Setup projects

;; Here is an example of making a customized version of your favorite
;; publisher.  All this does is run `my-muse-blosoxm-finalize' on the
;; published file immediately after saving it.

(unless (assoc "my-blosxom" muse-publishing-styles)
  (muse-derive-style "my-blosxom" "blosxom-xhtml"
                     :final 'my-muse-blosxom-finalize)

  (muse-derive-style "my-xhtml" "xhtml"
                     :header "~/personal-site/muse/header.html"
                     :footer "~/personal-site/muse/footer.html"))

;; Here is my master project listing.
;;
;; Note that I not do anything useful with the ProjectsWiki and
;; WebWiki projects; only the BlogWiki project is published.

(setq muse-project-alist
      `(
        ("Website"
         ("~/proj/wiki/web/" :default "WelcomePage")
         (:base "my-xhtml"
                :path "~/personal-site/site/web"))

        ("Projects"
         ("~/proj/wiki/projects/" :default "WelcomePage")
         (:base "my-xhtml"
                :path "~/personal-site/site/projects"))

        ("Blog"
         (,@(muse-blosxom-project-alist-dirs "~/proj/wiki/blog")
          :default "guestbook")

         ,@(muse-blosxom-project-alist-entry "~/proj/wiki/blog"
                                             "~/personal-site/site/blog"
                                             "my-blosxom"))

        ("Plans"
         ("~/proj/wiki/plans/"
          :default "TaskPool"
          :major-mode planner-mode
          :visit-link planner-visit-link)

         (:base "xhtml"
                :path "~/proj/notmine/planner-out"))
        ))

;; Wiki settings
(setq muse-wiki-interwiki-alist
      '(("PlugWiki" . "http://plug.student-orgs.purdue.edu/plugwiki/")
        ("TheEmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/")
        ("ArchWiki" . "http://wiki.gnuarch.org/")))

;;; Functions

;; Make the current file display correctly in Xanga

(defun my-muse-blosxom-finalize (file output-path target)
;;  (my-muse-prepare-entry-for-xanga output-path)
;; For now, do nothing.
  )

;; I call this using C-c p x now.

(defun my-muse-prepare-entry-for-xanga (file)
  "Mangle FILE so that Xanga doesn't bug out, saving to X clipboard.

If FILE is not specified, use the published version of the current file."
  (interactive
   (list
    (expand-file-name (concat (muse-page-name) ".txt")
                      (muse-style-element
                       :path (car (muse-project-applicable-styles
                                   buffer-file-name
                                   (cddr (muse-project-of-file))))))))
  (save-match-data
    (with-temp-buffer
      (insert-file-contents file t)
      ;; Surround first line in <h3></h3>
      (goto-char (point-min))
      (insert "<h3>")
      (end-of-line)
      (insert "</h3>")
      ;; Treat example regions properly
      (let (beg end)
        (while (re-search-forward "<pre[^>]*>" nil t)
          (setq beg (match-end 0))
          (setq end (if (re-search-forward "</pre>" nil 1)
                        (match-beginning 0)
                      (point)))
          (save-restriction
            (narrow-to-region beg end)
            ;; Change spaces to &nbsp;
            (goto-char (point-min))
            (while (re-search-forward "^ +" nil t)
              (replace-match (apply 'concat (make-list
                                             (length (match-string 0))
                                             "&nbsp;"))))
            ;; Change newline to <br />
            (goto-char (point-min))
            (while (re-search-forward "\n" nil t)
              (replace-match "<br />")))))
      ;; Get rid of 2 spaces together and merge lines
      (goto-char (point-min))
      (while (re-search-forward (concat "["
                                        muse-regexp-space
                                        "]+") nil t)
        (replace-match " "))
      ;; Remove trailing space
      (goto-char (point-min))
      (while (re-search-forward " *</p> *" nil t)
        (replace-match "</p>"))
      ;; Make relative links work
      (goto-char (point-min))
      (while (re-search-forward "href=\"/" nil t)
        (replace-match "href=\"http://www.mwolson.org/" nil t))
      ;; Copy entry to clipboard
      (clipboard-kill-ring-save (point-min) (point-max))
      ;; Don't prompt me about killing the buffer
      (set-buffer-modified-p nil))))

;;; Key customizations

(global-set-key "\C-cpl" 'muse-blosxom-new-entry)
(global-set-key "\C-cpL"
                #'(lambda ()
                    (interactive)
                    (find-file muse-blosxom-base-directory)))
(global-set-key "\C-cpx" 'my-muse-prepare-entry-for-xanga)

;;; Custom variables

(custom-set-variables
 '(muse-blosxom-base-directory "~/proj/wiki/blog/")
 '(muse-blosxom-publishing-directory "~/personal-site/site/blog")
 '(muse-colors-autogen-headings (quote outline))
 '(muse-file-extension "muse")
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />")
 '(muse-mode-auto-p nil)
 '(muse-mode-hook (quote (footnote-mode turn-on-auto-fill muse-wiki-update-custom-values)))
 '(muse-xhtml-footer "~/personal-site/muse/generic-footer.html")
 '(muse-xhtml-header "~/personal-site/muse/generic-header.html"))
(custom-set-faces
 '(muse-bad-link-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))))
 '(muse-link-face ((t (:foreground "blue" :underline "blue" :weight bold)))))

;;; muse-init.el ends here
