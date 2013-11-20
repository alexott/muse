;; muse-slidy.el

;; Copyright (C) 2010 Fabio Arcinegas A.

;; Author: Fabio Arciniegas (fab DOT arciniegas AT gmail DOT com)
;; Keywords: muse, slidy, presentations, emacs powerpoint alternative, slide maker
;;
;; Version 1.0 August 07, 2010 (21:48:34) 
;; Version 2.0 September 16, 2013 (15:26:59) 

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
;; Convert emacs muse files (plain text with simple 
;; wiki-like mark up) into platform-independent self-contained 
;; presentations that do not require M$ powerpoint
;;
;; All sections and subsections of the muse file (headers level 1 and
;; 2) are converted to individual slides.

;; todo: different treatment for level 1 and 2

;;; Usage
;;
;; (require 'muse-slidy)
;;
;; ;; open any muse file 
;; ;; M-x muse-publish-this-file slidy
;; ;; The result is a self-containted presentation, use M-x
;; ;; customize-group html-slidy to configure look and feel  
 

;;; Configuration options
;;; 
;;; Base configurations are grouped in predefined themes. You can
;;; override specific values using the customization variables below.
;;;
;;;
;;; (defun current-date-string ()
;;;  "Returns current date as a string."
;;;  (format-time-string "%B %d, %Y" (current-time)))

(defcustom muse-slidy-theme "blue"
  "Base theme to use when publishing muse slidy presentations.

Caution: when you set a new theme, specific customization are
  preserved until set to nil. Set individual overrides to nil or use M-x
  muse-slidy-reset-look-and-feel to reset all customizations to nil." 
  :type '(choice (const :tag "Black on white classic clean look" "white")
		 (const :tag "W3C Slidy standard blue look" "blue")
		 (const :tag "Gray on white space look" "black"))
  :group 'muse-slidy
  )




;;; Muse integration
;;; 
;;; Base configurations are grouped in predefined themes. You can
;;; override specific values using the customization variables below.
;;;
;;;
;;; (defun current-date-string ()
;;;  "Returns current date as a string."
;;;  (format-time-string "%B %d, %Y" (current-time)))

(defcustom muse-slidy-markup-strings
  '(
    (section . "</div>\n<div class=\"slide\"><h1>")
    (section-end . "</h1>")
    (image . "<img class=\"illustration\" src=\"data:img/jpg\;base64,\&IMAGE_%s.%s\"/>\n")
    (image-with-desc . "<img class=\"illustration\" src=\"data\:img/jpg;base64,\&IMAGE_\"/>\n")
    (subsection . "</div><div class=\"slide\"><h1>")
    (subsection-end . "</h1>"))
    "Strings used for marking up text as HTML Slidy."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-html)

(defcustom muse-slidy-footer "
<lisp>(muse-slidy-reference-all-images)</lisp>
<!-- This presentation published with Emacs Muse slidy mode. RmFiaW8gQXJjaW5pZWdhcyx0aGFuayB5b3Ug-->
</div>
  </body>
</html>\n"
  "Footer used for publishing XHTML muse-slidy presentation files."
  :type 'string
  :group 'muse-slidy)


(defcustom muse-slidy-header  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"
[
<!ENTITY LEADER_IMAGE \"<lisp>(muse-slidy-configuration-as-base64 \"leader-image\")</lisp>\">
<!ENTITY IMAGE_HEAD \"<lisp>(muse-slidy-configuration-as-base64 \"header-image\")</lisp>\">
<!ENTITY IMAGE_FOOT \"<lisp>(muse-slidy-configuration-as-base64 \"footer-image\")</lisp>\">
<lisp>(null (setq muse-slidy-end-of-entities (point)))</lisp>
]
>
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>

    <meta name=\"copyright\" 
content=\"Copyright &#169; <lisp>(let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat author )))</lisp> <lisp>(current-year-string)</lisp>\" /> 
<!--
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
          content=\"<lisp>muse-html-meta-content-type</lisp>\" />
-->

   <lisp>(insert muse-slidy-slidy-js)</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"slidy-css\")</lisp>

  </head>
  <body>

<div class=\"background\"> 
   <lisp>(muse-slidy-configuration-as-base64 \"head-icon\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"head-logo\")</lisp>
</div> 

<div class=\"slide cover\"> 
   <lisp>(muse-slidy-configuration-as-base64 \"cover-image\")</lisp>

 <br clear=\"all\" />            
 <h1> <lisp>(muse-publishing-directive \"title\")</lisp></h1>
 <p><lisp>(let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat author )))</lisp> <lisp>(current-year-string)</lisp></p> 
</div> 
<!-- extra open div so section can start always with clossing-->
<div>
"
  "Header used for publishing Slidy files. Contains elisp code."
  :type 'string
  :group 'muse-html)



;;todo muse-slidy-reset-look-and-feel
;;todo implement background gradients


;; TODO: see if it this initialization can be grouped
(setq muse-slidy-white-theme (make-hash-table :test 'equal))
(puthash 'muse-slidy-theme-name "Standard White" muse-slidy-white-theme)
(puthash 'muse-slidy-header-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCB1jYAAAAAIAAc/INeUAAAAASUVORK5CYII=" muse-slidy-white-theme)
(puthash 'muse-slidy-footer-image "~/Desktop/IMG_2562.JPG" muse-slidy-white-theme)
(puthash 'muse-slidy-leader-image "~/Desktop/IMG_2562.JPG" muse-slidy-white-theme)
(puthash 'muse-slidy-background-color "#FFFFFF" muse-slidy-white-theme)
(puthash 'muse-slidy-foreground-color "#000000" muse-slidy-white-theme)
;; TODO: enable fonts
(puthash 'muse-slidy-header-font "font" muse-slidy-white-theme)
(puthash 'muse-slidy-content-font "font" muse-slidy-white-theme)


(setq muse-slidy-blue-theme (make-hash-table :test 'equal))
(puthash 'muse-slidy-theme-name "Standard Blue" muse-slidy-blue-theme)
(puthash 'muse-slidy-header-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAA1BMVEUAAACnej3aAAAAAXRSTlMAQObYZgAAAApJREFUCB1jYAAAAAIAAc/INeUAAAAASUVORK5CYII=" muse-slidy-blue-theme)
(puthash 'muse-slidy-footer-image "~/Desktop/IMG_2562.JPG" muse-slidy-blue-theme)
(puthash 'muse-slidy-leader-image "~/Desktop/IMG_2562.JPG" muse-slidy-blue-theme)
(puthash 'muse-slidy-background-color "#FFFFFF" muse-slidy-blue-theme)
(puthash 'muse-slidy-slidy-css "
<link rel=\"stylesheet\" href=\"http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css\" type=\"text/css\" />
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen, projection, print\"
 href=\"http://www.w3.org/Talks/Tools/Slidy2/styles/w3c-blue.css\" />
" muse-slidy-blue-theme)
(puthash 'muse-slidy-foreground-color "#000000" muse-slidy-blue-theme)
;; TODO: enable fonts
(puthash 'muse-slidy-header-font "font" muse-slidy-blue-theme)
(puthash 'muse-slidy-content-font "font" muse-slidy-blue-theme)
(puthash 'muse-slidy-head-icon "<img id=\"head-icon\" alt=\"graphic with four colored squares\" src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/icon-blue.png\"/>" muse-slidy-blue-theme)
(puthash 'muse-slidy-head-logo "<object id=\"head-logo\" data=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/w3c-logo-white.svg\" type=\"image/svg+xml\" title=\"W3C logo\"><a href=\"http://www.w3.org/\"><img alt=\"W3C logo\" id=\"head-logo-fallback\" src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/w3c-logo-white.gif\" /></a></object>" muse-slidy-blue-theme)
(puthash 'muse-slidy-cover-image " <img src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/keys2.jpg\" alt=\"Cover page images (keys)\" class=\"cover\" />" muse-slidy-blue-theme)



;; Configuration options are organized in themes, but overridable by individual 
;; variables.
(defun muse-slidy-get-configuration-option (name theme)
  "Get the current value of a configuration option (header-image,footer-image,etc).

If an override value for header,footer, or openning image have been set,
use it. Otherwise default to the one in the selected theme.
names of options include header-image, footer-image,opening-image,header-font, and content-font."
  (if (boundp (intern  (concat "muse-slidy-override-" name)))
      (eval (intern  (concat "muse-slidy-override-" name)))
      (if (boundp (intern  (concat "muse-slidy-" theme "-theme")))
	  (gethash (intern (concat "muse-slidy-" name))
		   (eval (intern (concat "muse-slidy-" theme "-theme")))))
  )
)

(defun muse-slidy-get-themed-customization (name)
  "Return the value of the customization denoted by string name"
  (muse-slidy-get-configuration-option name muse-slidy-theme)
)

(defun file-contents-as-base64 (filepath)
  "Return the contents of a file as a base64-encoded string."

    (with-temp-buffer 
      (insert-file-contents-literally filepath)
      (mark-whole-buffer)
      (base64-encode-region 1 (point-max) t)
      (buffer-string)
    )
)

(defun muse-slidy-configuration-as-base64 (option)
  "Return the value of a configuration option (header, footer, etc.) as a base64 string.

If the value of the configuration option is a valid filepath, return the contents as base64, otherwise assume the value is base64 information already and return the literal value of the configuration option"
 (setq c (muse-slidy-get-themed-customization option))
 
 (if (file-exists-p c)
     (file-contents-as-base64 c)
     c)
)

;; packages
;; muse-slidy-override-header-image

(require 'muse-publish)
(require 'muse-regexps)
(require 'muse-xml-common)
(require 'muse-html)

(defgroup muse-slidy nil
  "Options controlling the behavior of Muse Slidy publishing.
See `muse-slidy' for more information."
  :group 'muse-publish)

(defun current-date-string ()
  "Returns current date as a string."
  (format-time-string "%B %d, %Y" (current-time)))

(defun current-year-string ()
  "Returns current year as a string."
  (format-time-string "%Y" (current-time)))


(defun muse-slidy-embed-all-images ()
  (setq embedded-image-count 0)
  (setq entities "")
  (while (re-search-forward "\&IMAGE_\\(\.*\\)\"/>" nil t)
    (setq entities 
	  (concat entities (progn 
			     (setq image_filename (match-string 1))
			     (with-temp-buffer
			       (insert (format "<!ENTITY IMAGE_%d \"%s\">\n" 
					       (setq embedded-image-count (+ embedded-image-count 1))
					       (file-contents-as-base64 image_filename)))
			       (buffer-string))))))
  (setq embedded-image-count 0)
  entities)


(defun muse-slidy-reference-all-images ()
  (setq embedded-image-count 0)
  (save-excursion
    (beginning-of-buffer)
    (setq muse-slidy-entity-definitions (muse-slidy-embed-all-images))
    (beginning-of-buffer)
    (forward-char muse-slidy-end-of-entities)
    (insert muse-slidy-entity-definitions)
    (setq muse-slidy-embed-all-images nil)
    (while (re-search-forward "<img class=\"illustration\" src=\"data:img/jpg;base64,\&IMAGE_\.+\"/>" nil t)
      (replace-match (format "<img class=\"illustration\" src=\"data:img/jpg;base64,&IMAGE_%d;\"/>" (setq embedded-image-count (+ embedded-image-count 1))) nil nil)
      )
    )
)


;; TODO: make right mime type
;; TODO: see if gzip is possible for embedded references
;; TODO: consider auto-resize

;; TODO: allow for comments on images

;; TODO: decide what to do with level 3 headers and below

(setq muse-slidy-slidy-css "
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen, projection, print\"
 href=\"http://www.w3.org/Talks/Tools/Slidy2/styles/w3c-blue.css\" /> 
")

(setq muse-slidy-slidy-js "
<script src=\"http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js\" 
   charset=\"utf-8\" type=\"text/javascript\"></script> 
")



(muse-derive-style "slidy" "xhtml"
                   :strings 'muse-slidy-markup-strings
                   :header 'muse-slidy-header
                   :footer 'muse-slidy-footer)

(provide 'muse-slidy)
