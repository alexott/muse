;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Journal Publishing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The module facilitates the keeping and publication of a journal.
;; When publishing to HTML, it assumes the form of a web log, or blog.
;;
;; The input format for each entry is as follows:
;;
;;   * 20040317: Title of entry
;;
;;   Text for the entry.
;;
;;   <qotd>
;;   "You know who you are. It comes down to a simple gut check: You
;;   either love what you do or you don't. Period." -- P. Bronson
;;   </qotd>
;;
;; The "qotd", or Quote of the Day, is entirely optional.  When
;; generated to HTML, this entry is rendered as:
;;
;;   <div id="entry">
;;     <div id="entry-qotd">
;;       <h3>Quote of the Day:</h3>
;;       <p>"You know who you are. It comes down to a simple gut
;;         check: You either love what you do or you don't. Period."
;;         -- P. Bronson</p>
;;     </div>
;;     <div id="entry-body">
;;       <div id="entry-head">
;;         <div id="entry-date">
;;           <span class="date">March 17, 2004</span>
;;         </div>
;;         <div id="entry-title">
;;           <h2>Title of entry</h2>
;;         </div>
;;       </div>
;;       <div id="entry-text">
;;         <p>Text for the entry.</p>
;;       </div>
;;     </div>
;;   </div>
;;
;; The plurality of "div" tags makes it possible to display the
;; entries in any form you wish, using a CSS style.
;;
;; Also, an .RDF file can be generated from your journal by publishing
;; it with the "rdf" style.  It uses the first two sentences of the
;; first paragraph of each entry as its "description", and
;; autogenerates tags for linking to the various entries.

(require 'muse-publish)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-book)

(defgroup muse-journal nil
  "Rules for transforming a journal into its final form."
  :group 'muse-publish)

(defcustom muse-journal-heading-regexp
  "\\(?:\\([0-9]+\\)\\(?:: \\)?\\)?\\(.+?\\)?"
  "A regexp that match a journal heading.
Paren group 1 is the ISO date, group 2 is the optional category,
and group 3 is the optional heading for the entry."
  :type 'regexp
  :group 'muse-journal)

(defcustom muse-journal-date-format "%a, %e %b %Y"
  "Date formats to use for journal entries."
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-html-heading-regexp
  (concat "^<h2[^>]*>" muse-journal-heading-regexp "</h2>$")
  "A regexp that match a journal heading.
Paren group 1 is the ISO date, group 2 is the optional category,
and group 3 is the optional heading for the entry."
  :type 'regexp
  :group 'muse-journal)

(defcustom muse-journal-html-entry-template
  "<div id=\"entry\">
  <a name=\"%anchor%\" style=\"text-decoration: none\">&nbsp;</a>
  <div id=\"entry-body\">
    <div id=\"entry-head\">
      <div id=\"entry-date\">
	<span class=\"date\">%date%</span>
      </div>
      <div id=\"entry-title\">
	<h2>%title%</h2>
      </div>
    </div>
    <div id=\"entry-text\">
      <div id=\"entry-qotd\">
	<p>%qotd%</p>
      </div>
%text%
    </div>
  </div>
</div>\n\n"
  ""
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-latex-section
  "\\section*{%title% \\hfill {\\normalsize %date%}}
\\addcontentsline{toc}{chapter}{%title%}"
  ""
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-latex-subsection
  "\\subsection*{%title%}
\\addcontentsline{toc}{section}{%title%}"
  ""
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-latex-markup-tags
  '(("qotd" t nil muse-journal-latex-qotd-tag))
  "See `muse-publish-markup-tags' for more info."
  :type '(repeat (list (string :tag "Markup tag")
		       (boolean :tag "Expect closing tag" :value t)
		       (boolean :tag "Parse attributes" :value nil)
		       function))
  :group 'muse-journal)

(defun muse-journal-generate-pages ()
  (let ((output-dir (muse-style-element :path)))
    (goto-char (point-min))
    (while (re-search-forward muse-journal-heading-regexp nil t)
      (let* ((date (match-string 1))
	     (category (match-string 1))
	     (category-file (concat output-dir category "/index.html"))
	     (heading (match-string 1)))
	t))))

(defcustom muse-journal-rdf-extension ".rdf"
  ""
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-rdf-base-url ""
  "The base URL of the website reference by the RDF file."
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-rdf-header
  "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
	 xmlns=\"http://purl.org/rss/1.0/\"
	 xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
  <channel rdf:about=\"<lisp>(concat muse-journal-rdf-base-url
				     (muse-publish-output-name))</lisp>\">
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>
    <link><lisp>(concat muse-journal-rdf-base-url
		       (concat (muse-page-name)
			       muse-html-extension))</lisp></link>
    <description>A Journal</description>
    <items>
      <rdf:Seq>
	<rdf:li resource=\"<lisp>
	  (concat muse-journal-rdf-base-url
		  (concat (muse-page-name)
			  muse-html-extension))</lisp>\"/>
      </rdf:Seq>
    </items>
  </channel>\n"
  ""
  :type '(choice string file)
  :group 'muse-journal)

(defcustom muse-journal-rdf-footer
  "</rdf:RDF>\n"
  ""
  :type '(choice string file)
  :group 'muse-journal)

(defcustom muse-journal-rdf-date-format
  "%Y-%m-%dT%H:%M:%S"
  ""
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-rdf-entry-template
  "  <item rdf:about=\"%link%#%anchor%\">
    <title>%title%</title>
    <description>
      %desc%
    </description>
    <link>%link%#%anchor%</link>
    <dc:date>%date%</dc:date>
    <dc:creator>%maintainer%</dc:creator>
  </item>\n"
  ""
  :type 'string
  :group 'muse-journal)

(defcustom muse-journal-rdf-markup-regexps
  '((10000 muse-link-regexp 0 "\\2"))
  "List of markup rules for publishing a Muse journal page to RDF.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
		  (list :tag "Markup rule"
			integer
			(choice regexp symbol)
			integer
			(choice string function symbol))
		  function))
  :group 'muse-journal)

(defcustom muse-journal-rdf-markup-functions
  '((email . ignore)
    (link  . ignore)
    (url   . ignore))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-journal)

(defun muse-journal-anchorize-title (title)
  (save-match-data
    (if (string-match "(" title)
	(setq title (substring title 0 (match-beginning 0))))
    (if (string-match "<[^>]+>" title)
	(setq title (replace-match "" nil nil title)))
    (downcase (replace-regexp-in-string "[^a-zA-Z0-9_]" ""
  title))))

(defun muse-journal-sort-entries (&optional direction)
  (interactive "P")
  (sort-subr
   direction
   (function
    (lambda ()
      (if (re-search-forward "^\\* [0-9]+" nil t)
	  (goto-char (match-beginning 0))
	(goto-char (point-max)))))
   (function
    (lambda ()
      (if (re-search-forward "^\\* [0-9]+" nil t)
	  (goto-char (1- (match-beginning 0)))
	(goto-char (point-max)))))
   (function
    (lambda ()
      (forward-char 2)))
   (function
    (lambda ()
      (end-of-line)))))

(defun muse-journal-html-munge-buffer ()
  (goto-char (point-min))
  (let ((heading-regexp muse-journal-html-heading-regexp)
	(inhibit-read-only t))
    (while (re-search-forward heading-regexp nil t)
      (let* ((date (match-string 1))
	     (orig-date date)
	     (title (match-string 2))
	     (clean-title title)
	     datestamp qotd text)
	(delete-region (match-beginning 0) (match-end 0))
	(if clean-title
	    (save-match-data
	      (while (string-match "\\(^<[^>]+>\\|<[^>]+>$\\)" clean-title)
		(setq clean-title (replace-match "" nil nil clean-title)))))
	(save-match-data
	  (when (and date
		     (string-match
		      (concat "\\`\\([1-9][0-9][0-9][0-9]\\)[./]?"
			      "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)") date))
	    (setq datestamp
		  (encode-time
		   0 0 0
		   (string-to-int (match-string 3 date))
		   (string-to-int (match-string 2 date))
		   (string-to-int (match-string 1 date))
		   (current-time-zone))
		  date (concat (format-time-string
				muse-journal-date-format datestamp)
			       (substring date (match-end 0))))))
	(save-restriction
	  (narrow-to-region
	   (point) (if (re-search-forward
			(concat "\\(^<hr>$\\|"
				heading-regexp "\\)") nil t)
		       (match-beginning 0)
		     (point-max)))
	  (goto-char (point-max))
	  (while (and (not (bobp))
		      (eq ?\  (char-syntax (char-before))))
	    (delete-char -1))
	  (goto-char (point-min))
	  (while (and (not (eobp))
		      (eq ?\  (char-syntax (char-after))))
	    (delete-char 1))
	  (save-excursion
	    (when (search-forward "<qotd>" nil t)
	      (let ((tag-beg (match-beginning 0))
		    (beg (match-end 0)))
		(re-search-forward "</qotd>\n*")
		(setq qotd (buffer-substring-no-properties
			    beg (match-beginning 0)))
		(delete-region tag-beg (match-end 0)))))
	  (setq text (buffer-string))
	  (delete-region (point-min) (point-max))
	  (let ((entry muse-journal-html-entry-template))
	    (while (string-match "%date%" entry)
	      (setq entry (replace-match (or date "") nil t entry)))
	    (while (string-match "%title%" entry)
	      (setq entry (replace-match (or title "&nbsp;") nil t entry)))
	    (while (string-match "%anchor%" entry)
	      (setq entry (replace-match
			   (muse-journal-anchorize-title
			    (or clean-title orig-date))
			   nil t entry)))
	    (while (string-match "%qotd%" entry)
	      (setq entry (replace-match (or qotd "") nil t entry)))
	    (while (string-match "%text%" entry)
	      (setq entry (replace-match text nil t entry)))
	    (insert entry)
	    (when (null qotd)
	      (goto-char (point-min))
	      (search-forward "<div id=\"entry-qotd\">")
	      (let ((beg (match-beginning 0)))
		(re-search-forward "</div>\n*")
		(delete-region beg (point))))))))))

(defun muse-journal-latex-munge-buffer ()
  (goto-char (point-min))
  (let ((heading-regexp
	 (concat "^" (regexp-quote (muse-markup-text 'section))
		 muse-journal-heading-regexp
		 (regexp-quote (muse-markup-text 'section-end)) "$"))
	(inhibit-read-only t))
    (when (re-search-forward heading-regexp nil t)
      (goto-char (match-beginning 0))
      (sort-subr nil
		 (function
		  (lambda ()
		    (if (re-search-forward heading-regexp nil t)
			(goto-char (match-beginning 0))
		      (goto-char (point-max)))))
		 (function
		  (lambda ()
		    (if (re-search-forward heading-regexp nil t)
			(goto-char (1- (match-beginning 0)))
		      (goto-char (point-max)))))
		 (function
		  (lambda ()
		    (forward-char 2)))
		 (function
		  (lambda ()
		    (end-of-line)))))
    (while (re-search-forward heading-regexp nil t)
      (let ((date (match-string 1))
	    (title (match-string 2))
	    qotd section)
	(save-match-data
	  (when (and date
		     (string-match
		      (concat "\\([1-9][0-9][0-9][0-9]\\)[./]?"
			      "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)") date))
	    (setq date (encode-time
			0 0 0
			(string-to-int (match-string 3 date))
			(string-to-int (match-string 2 date))
			(string-to-int (match-string 1 date))
			(current-time-zone))
		  date (format-time-string
			muse-journal-date-format date))))
	(save-match-data
	  (setq section muse-journal-latex-section)
	  (while (string-match "%title%" section)
	    (setq section (replace-match (or title "Untitled")
					 nil t section)))
	  (while (string-match "%date%" section)
	    (setq section (replace-match (or date "") nil t section))))
	(replace-match section nil t))))
  (goto-char (point-min))
  (let ((subheading-regexp
	 (concat "^" (regexp-quote (muse-markup-text 'subsection))
		 "\\([^\n}]+\\)"
		 (regexp-quote (muse-markup-text 'subsection-end)) "$"))
	(inhibit-read-only t))
    (while (re-search-forward subheading-regexp nil t)
      (let ((subsection muse-journal-latex-subsection))
	(save-match-data
	  (let ((title (match-string 1)))
	    (while (string-match "%title%" subsection)
	      (setq subsection (replace-match title nil t subsection)))))
	(replace-match subsection nil t)))))

(defun muse-journal-latex-qotd-tag (beg end)
  (goto-char beg)
  (insert (muse-markup-text 'begin-quote))
  (goto-char end)
  (insert (muse-markup-text 'end-quote)))

(defun muse-journal-rdf-munge-buffer ()
  (goto-char (point-min))
  (let ((heading-regexp (concat "^\\* " muse-journal-heading-regexp "$"))
	(inhibit-read-only t))
    (while (re-search-forward heading-regexp nil t)
      (let* ((date (match-string 1))
	     (orig-date date)
	     (title (match-string 2))
	     qotd desc)
	(save-match-data
	  (when (and date
		     (string-match
		      (concat "\\([1-9][0-9][0-9][0-9]\\)[./]?"
			      "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)") date))
	    (setq date (encode-time 0 0 0
				    (string-to-int (match-string 3 date))
				    (string-to-int (match-string 2 date))
				    (string-to-int (match-string 1 date))
				    (current-time-zone))
		  date (format-time-string
			muse-journal-rdf-date-format date))))
	(save-restriction
	  (narrow-to-region
	   (match-beginning 0)
	   (if (re-search-forward heading-regexp nil t)
	       (match-beginning 0)
	     (point-max)))
	  (goto-char (point-min))
	  (delete-region (point) (line-end-position))
	  (re-search-forward "</qotd>\n+" nil t)
	  (while (and (char-after)
		      (eq ?\  (char-syntax (char-after))))
	    (delete-char 1))
	  (let ((beg (point)))
	    (forward-sentence 2)
	    (setq desc (buffer-substring beg (point))))
	  (delete-region (point-min) (point-max))
	  (let ((entry muse-journal-rdf-entry-template))
	    (while (string-match "%date%" entry)
	      (setq entry (replace-match (or date "") nil t entry)))
	    (while (string-match "%title%" entry)
	      (setq entry (replace-match (or title "Untitled") nil t entry)))
	    (while (string-match "%desc%" entry)
	      (setq entry (replace-match desc nil t entry)))
	    (while (string-match "%link%" entry)
	      (setq entry (replace-match
			   (concat muse-journal-rdf-base-url
				   (concat (muse-page-name)
					   muse-html-extension))
			   nil t entry)))
	    (while (string-match "%anchor%" entry)
	      (setq entry (replace-match
			   (muse-journal-anchorize-title (or title orig-date))
			   nil t entry)))
	    (while (string-match "%maintainer%" entry)
	      (setq entry (replace-match
			   (or (muse-style-element :maintainer)
			       (concat "webmaster@" (system-name)))
			   nil t entry)))
	    (insert entry)))))))

(unless (assoc "journal-html" muse-publishing-styles)
  (muse-derive-style "journal-html" "html"
		     :before-end 'muse-journal-html-munge-buffer)

  (muse-derive-style "journal-latex" "latex"
		     :tags 'muse-journal-latex-markup-tags
		     :before-end 'muse-journal-latex-munge-buffer)

  (muse-derive-style "journal-pdf" "pdf"
		     :tags 'muse-journal-latex-markup-tags
		     :before-end 'muse-journal-latex-munge-buffer)

  (muse-derive-style "journal-book-latex" "book-latex"
		     ;;:nochapters
		     :tags 'muse-journal-latex-markup-tags
		     :before-end 'muse-journal-latex-munge-buffer)

  (muse-derive-style "journal-book-pdf" "book-pdf"
		     ;;:nochapters
		     :tags 'muse-journal-latex-markup-tags
		     :before-end 'muse-journal-latex-munge-buffer)

  (muse-define-style "journal-rdf"
		     :suffix	'muse-journal-rdf-extension
		     :regexps	'muse-journal-rdf-markup-regexps
		     :functions 'muse-journal-rdf-markup-functions
		     :before	'muse-journal-rdf-munge-buffer
		     :header	'muse-journal-rdf-header
		     :footer	'muse-journal-rdf-footer))

(provide 'muse-journal)

;;; muse-journal.el ends here
