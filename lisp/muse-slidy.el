;;; muse-html-slidy.el

;; Copyright (C) 2010 Fabio Arcinegas

;; Author: Fabio Arciniegas (fab DOT arciniegas AT gmail DOT com)
;; Keywords: declarative graphics
;; Version 1.0 August 07, 2010 (21:48:34) 
;; Muse 2.0 September 16, 2013 (15:26:59) 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This softwware is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Purpose

;; This is an extension to emacs muse allowing for the creation of self-contained
;; xhtml presentations.
;;
;; Essentially, the goal is to convert emacs muse files (plain text with simple 
;; wiki-like mark up) into platform-independent self-contained HTML
;; presentations that do not require M$ powerpoint
;;


;;; Usage
;; (require 'muse-html-slidy)

;;; Commentary: Code works mainly by embedded lisp calls inside the
;;; footer and headers.
;;;
;;;             There are essentially two stages: in the first, a list
;;;             of entity references is created in a temp file using
;;;             (create-entity-files). At the end of the publishing
;;;             process, using the footer callback, a replacement of
;;;             all entity definitions with the base64 content of the
;;;             files they refer to is made.
;;; 
;;;             The references to each entity are replaced using plain
;;;             &ref; style markup. This is accomplished by replacing
;;;             the string REPLACE_WITH_SEQUENTIAL_ENTITY which is left by the
;;;             muse handling of img tag
 

;;; Contributors:

;;; Code:

(require 'muse-publish)
(require 'muse-regexps)
(require 'muse-xml-common)
(require 'muse-html)

(defgroup muse-html-slidy nil
  "Options controlling the behavior of Muse HTML Slidy (presentations) publishing.
See `muse-html-slidy' for more information."
  :group 'muse-publish)

;; TODO: stop needing ending trail
(defcustom muse-html-slidy-style-dir "/Users/fabio/ss/tools/muse/html-slidy-support/"
  "Directory with the slidy source code and stylesheet. Note ending trail"
  :type 'string
  :group 'muse-html-slidy)

(setq muse-html-slidy-replace-entities-in-file nil) 

(defcustom muse-html-slidy-embed-with-shell t
  "Use shell command to post-process the file and embed the graphics.

Make nil to use a pure emacs implementation of base64 embedding.\nUsing the shell is recommended because emacs can reach max buffer insert limits with large image files resulting in errors."
  :type '(choice (const :tag "Embed graphics using a shell invocation" t)
                 (const :tag "Embed graphics using emacs only" 'nil))
  :group 'muse-html-slidy)


(defcustom muse-html-slidy-slidy-src "slidy.js"
  "Slidy javascript source. This code is embedded at the top of the output file"
  :type 'string 
  :group 'muse-html-slidy)

(defcustom muse-html-slidy-slidy-css "slidy.css"
  "Slidy source. This code is embedded at the top of the output file"
  :type 'string 
  :group 'muse-html-slidy)

(defun current-date-string ()
  "Returns current date as a string."
  (format-time-string "%B %d, %Y" (current-time)))

(defun create-tmp-entities-file ()
 (muse-html-slidy-embed-all-images)
)

(defun muse-html-slidy-embed-all-images ()
  (setq embedded-image-count 0)
  (setq tmp (make-temp-file "entities-")) 
  (with-temp-buffer
    (null (insert-file-contents-literally muse-publishing-current-file))

;; TODO: improve list of file exts
    (while (re-search-forward "\\\[\\{2\\}\\(\[^]:\]+\\)\\.\\(png\\|jpg\\|gif\\|ps\\)\\\]" nil t)
      (progn 
	(setq filename (concat (match-string 1) "." (match-string 2)))
	(when (file-writable-p tmp) 
	  (append-to-file (format "<!ENTITY IMAGE_%d @%s@>\n" 
				  (setq embedded-image-count (+ embedded-image-count 1))
				  (file-truename filename)
				  ) nil tmp))
      )
    )
  )
;; perl -pi -M"MIME::Base64" -e "local $/=undef;" -e "s/ENTITY IMAGE_(.*) \"(.*)\"/open TMP, $2;\"ENTITY IMAGE_$1 '\".encode_base64(<TMP>).\"'\"/ge" entities-1.test
;; TODO: NO BAK FILE
  tmp
)

;; TODO: make sure all extensions for appropriate files are considered
;; TODO: make right mime type
;; TODO: see if gzip is possible for embedded references
;; TODO: consider auto-resize


(defun replace-all-entity-references ()
  (setq embedded-image-count 0)
  (setq muse-html-slidy-replace-entities-in-file output-path)
  (setq muse-buffer-point (point))
  (goto-char 0)
  (while (re-search-forward "REPLACE_WITH_SEQUENTIAL_ENTITY" nil t)
    (replace-match (format "&IMAGE_%d;" (setq embedded-image-count (+ embedded-image-count 1))))
    )
  (setq tmp "")
  )

;; TODO: allow for comments on entities
(defcustom muse-html-slidy-markup-strings
  '(
    (section . "</div><div class=\"slide\"><h1>")
    (section-end . "</h1><img src=\"data:img/jpeg;base64,&IMAGE_HEAD;\" width=\"100%%\" alt=\"head\" style=\"position: absolute; top: 0px; left:0px; z-index:-1\"/><img src=\"data:img/jpeg;base64,&IMAGE_FOOT;\" width=\"100%%\" alt=\"foot\" style=\"position: absolute; bottom: 0px; right:0px; z-index:-1\"/>")
;    (image-with-desc . "<img src=\"data:img/jpg;base64,&%2%\">")
    (image-with-desc . "<img class=\"illustration\" src=\"data:img/jpg;base64,REPLACE_WITH_SEQUENTIAL_ENTITY\"/>")
    (image . "<img class=\"illustration\" src=\"data:img/jpg;base64,REPLACE_WITH_SEQUENTIAL_ENTITY\"/>")
    (subsection . "</div><div class=\"slide\"><h1>")
    (subsection-end . "</h1>"))
    "Strings used for marking up text as HTML Slidy."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-html)

;;<!ENTITY FIRST_IMAGE \"<lisp>(null (base64-encode-region (point) (+ (point) (car (cdr (insert-file-contents-literally (concat muse-html-slidy-style-dir \"first.jpg\"))))) 1))</lisp>\">


;; TODO: Customize fonts.
;; TODO: Customize colors.
;; TODO: Customize top and bottom image independently
;; TODO: make code pure emacs, no perl
;; TODO: make consistent with packaging
;; TODO: merge with muse in github
;; TODO: decide what to do with level 3 headers and below

(defcustom muse-html-slidy-header  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"
[
<!ENTITY IMAGE_FIRST @<lisp>(concat muse-html-slidy-style-dir \"first.jpg\")</lisp>@>
<!ENTITY IMAGE_HEAD @<lisp>(concat muse-html-slidy-style-dir \"head.jpg\")</lisp>@>
<!ENTITY IMAGE_FOOT @<lisp>(concat muse-html-slidy-style-dir \"foot.jpg\")</lisp>@>
<lisp>(setq entities (create-tmp-entities-file))
(insert-file-contents-literally entities)
(delete-file entities)
</lisp>
]
>
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title><lisp>
  (muse-publishing-directive \"title\")</lisp></title>
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
          content=\"<lisp>muse-html-meta-content-type</lisp>\" />
  <style type=\"text/css\" media=\"screen, projection, print\">
  <link href=\"http://fonts.googleapis.com/css?family=Cabin:regular,500\" rel=\"stylesheet\" type=\"text/css\"/>
<link href='http://fonts.googleapis.com/css?family=Droid+Serif' rel='stylesheet' type='text/css'/>
/*<![CDATA[*/
    <lisp>(insert-file (concat muse-html-slidy-style-dir muse-html-slidy-slidy-css))</lisp>
/*]]>*/
  </style>
  <script type=\"text/javascript\">
//<![CDATA[
    <lisp>(insert-file (concat muse-html-slidy-style-dir muse-html-slidy-slidy-src))</lisp>
//]]>
  </script>
  </head>
  <body>
<div class=\"slide\">
     <img src=\"data:image/gif;base64,&IMAGE_FIRST;\" width=\"100%%\" style=\"position: absolute; left:0px; bottom: 0px; z-index:-1\"/>
    <div style=\"position: relative; top: 300px;\"><br/><br/>      
      <span class=\"title-h1\"><lisp>
     (muse-publishing-directive \"title\")</lisp></span><br/><span class=\"title-author\"><lisp>
          (let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat author )))</lisp>
       <lisp>(current-date-string)</lisp></span>
      </div>
"
  "Header used for publishing Slidy files. Contains elisp code, alter with care."
  :type 'string
  :group 'muse-html)

(defcustom muse-html-slidy-footer "
<lisp>(replace-all-entity-references)</lisp>
<!-- This presentation published with Emacs Muse. RmFiaW8gQXJjaW5pZWdhcyx0aGFuayB5b3Ug-->
</div>
  </body>
</html>\n"
  "Footer used for publishing XHTML files. This may be text or a filename."
  :type 'string
  :group 'muse-html)


(defun replace-entity-filenames-with-base64-batch ()
  (if muse-html-slidy-replace-entities-in-file
      (shell-command (concat "perl -i.bak \"" (concat muse-html-slidy-style-dir "replace-entities-in-file.pl") "\" \"" muse-html-slidy-replace-entities-in-file "\"" ))
  (setq muse-html-slidy-replace-entities-in-file nil)
))

(muse-derive-style "html-slidy" "xhtml"
                   :strings 'muse-html-slidy-markup-strings
                   :header 'muse-html-slidy-header
                   :footer 'muse-html-slidy-footer)

;; Must change this so it does it really automatically on a
;; muse-publish-this-file
;; TODO: make work muse-publish-this-file
(add-hook 'after-save-hook
	  'replace-entitiy-filenames-with-base64-batch
)

(provide 'muse-html-slidy)
