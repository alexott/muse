;; muse-slidy.el

;; Copyright (C) 2010 Fabio Arcinegas A.

;; Author: Fabio Arciniegas (fab DOT arciniegas AT gmail DOT com)
;; Keywords: muse, slidy, presentations, emacs powerpoint alternative, slide maker
;;
;; uses slidy by W3C
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
 
 (if (string= "" c)
     ""
   (if (file-exists-p c)
     (file-contents-as-base64 c)
     c)
   )
 )

(defcustom muse-slidy-theme "white"
  "Base theme to use when publishing muse slidy presentations.

Caution: when you set a new theme, specific customization are
  preserved until set to nil. Set individual overrides to nil or use M-x
  muse-slidy-reset-look-and-feel to reset all customizations to nil." 
  :type '(choice (const :tag "Muse Slidy default theme" "flagship")
		 (const :tag "White basic look" "white")
		 (const :tag "W3C Slidy standard blue look" "blue")
		 )
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
    (section . "</div>\n<div class=\"slide\"><img src=\"data:img/jpeg;base64,&STATIC_HEAD;\" width=\"100%%\" style=\"position: absolute; top: 0px; left:0px; z-index:-1\"/><img src=\"data:img/jpeg;base64,&STATIC_FOOT;\" width=\"100%%\" style=\"position: absolute; bottom: 0px; right:0px; z-index:-1\"/><h1>")
    (section-end . "</h1>")
    (image . "<div class=\"image-container\"><img class=\"illustration\" src=\"data:img/jpg\;base64,\&IMAGE_%s.%s\"/></div>\n")
    (image-with-desc . "<div class=\"image-container\"><img class=\"illustration\" src=\"data:img/jpg\;base64,\&IMAGE_%s.%s\"/></div><div class=\"image-description\">%s</div>\n")
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
  "DO NOT MODIFY UNLESS YOU ARE SURE. ALL LOOK AND FEEL OF PRESENTATIONS CAN BE MODIFIED THROUGH MUSE-SLIDY-OVERRIDE-* CUSTOMIZATIONS. Footer used for publishing XHTML muse-slidy presentation files."
  :type 'string
  :group 'muse-slidy)



;;(defcustom muse-slidy-override-header-image "~/Desktop/aliens.jpg"
;;  "Override the theme's header image with a custom image."
;;  :type 'string
;;  :group 'muse-slidy)

(defcustom muse-slidy-header  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"
[
<!ENTITY STATIC_LEAD \"<lisp>(muse-slidy-configuration-as-base64 \"leader-image\")</lisp>\">
<!ENTITY STATIC_HEAD \"<lisp>(muse-slidy-configuration-as-base64 \"header-image\")</lisp>\">
<!ENTITY STATIC_FOOT \"<lisp>(muse-slidy-configuration-as-base64 \"footer-image\")</lisp>\">
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
   <lisp>(muse-slidy-configuration-as-base64 \"background-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"fonts-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"title-font-family-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"title-font-color-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"author-font-family-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"author-font-color-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"header-font-family-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"header-font-color-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"body-font-family-css\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"body-font-color-css\")</lisp>

   <script>
$(window).load(function(){
    $(\'#illustration\').imagefit();
});
</script>
  </head>
  <body>

<div class=\"background\"> 
   <lisp>(muse-slidy-configuration-as-base64 \"head-icon\")</lisp>
   <lisp>(muse-slidy-configuration-as-base64 \"head-logo\")</lisp>
</div> 

<div class=\"slide cover\"> 
     <img src=\"data:image/gif;base64,&STATIC_LEAD;\" width=\"100%%\" style=\"position: absolute; left:0px; bottom: 0px; z-index:-1\"/>
   <lisp>(muse-slidy-configuration-as-base64 \"cover-image\")</lisp>

    <div style=\"position: relative; top: 300px;\"><br/><br/>      
      <span class=\"title-h1\"><lisp>(muse-publishing-directive \"title\")</lisp></span><br/><span class=\"title-author\"><lisp>(let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat author )))</lisp> <lisp>(current-year-string)</lisp></span>
      </div>
</div> 
<!-- extra open div so section can start always with clossing-->
<div>
"
  "Header used for publishing Slidy files. Contains elisp code."
  :type 'string
  :group 'muse-html)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  DEFAULT THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq muse-slidy-flagship-theme (make-hash-table :test 'equal))

(puthash 'muse-slidy-header-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-flagship-theme)
(puthash 'muse-slidy-footer-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-flagship-theme)
(puthash 'muse-slidy-leader-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-flagship-theme)
(puthash 'muse-slidy-background-css "<style>body {background: rgb(229,229,229); background: -moz-linear-gradient(left,  rgba(229,229,229,1) 0%, rgba(255,255,255,1) 63%); background: -webkit-gradient(linear, left top, right top, color-stop(0%,rgba(229,229,229,1)), color-stop(63%,rgba(255,255,255,1))); background: -webkit-linear-gradient(left,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); background: -o-linear-gradient(left,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); background: -ms-linear-gradient(left,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); background: linear-gradient(to right,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#e5e5e5', endColorstr='#ffffff',GradientType=1 );} </style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-fonts-css "<style><link href=\"http://fonts.googleapis.com/css?family=Cabin:regular,500\" rel=\"stylesheet\" type=\"text/css\"/><link href='http://fonts.googleapis.com/css?family=Droid+Serif' rel='stylesheet' type='text/css'/></style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-title-font-family-css    "<style>.title-h1{font-family:Cabin, serif;font-size:90px;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-title-font-color-css     "<style>.title-h1{color:#000011;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-author-font-family-css   "<style>.title-author{font-family:Cabin, serif;font-size:80px;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-author-font-color-css    "<style>.title-author{color:#333333;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-header-font-family-css   "<style>h1{font-family:Cabin, serif;font-size:46px;font-style:normal;text-decoration:none;text-transform:none;text-align:center;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-header-font-color-css    "<style>h1{color:#000011;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-body-font-family-css   "<style>body{font-family:\"Droid Serif\", sans-serif;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-body-font-color-css    "<style>body{color:#111111;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-head-icon "" muse-slidy-flagship-theme)
(puthash 'muse-slidy-head-logo "" muse-slidy-flagship-theme)
;;(puthash 'muse-slidy-cover-image " <img src=\"a.jpeg\" class=\"cover\" />" muse-slidy-flagship-theme)
(puthash 'muse-slidy-cover-image "" muse-slidy-flagship-theme)
(puthash 'muse-slidy-slidy-css "<style type=\"text/css\" media=\"screen, projection, print\">/*<![CDATA[*/body{width:100%;height:100%;margin:0;padding:0;}.title-slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;background-color:transparent;border-width:0;margin:0;padding:20px 20px 0;}.title-content{position:absolute;bottom:10px;left:20px;}.title-author{font-family:Cabin, serif;font-size:50px;font-style:normal;font-weight:700;color:#000;text-shadow:2px 2px 2px #aaa;text-decoration:none;text-transform:none;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}.hidden{display:none;visibility:hidden;}div.toolbar{position:fixed;z-index:200;top:auto;bottom:0;left:0;right:0;height:1.2em;text-align:right;padding-left:1em;padding-right:1em;font-size:60%;color:gray;background:transparent;}div.background{display:none;}div.handout{margin-left:20px;margin-right:20px;}div.slide.titlepage.h1{padding-top:40%;}div.slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;line-height:120%;font-size:24pt;background-color:transparent;border-width:0;margin:0;padding:10px 20px 0;}div.slide + div[class].slide{page-break-before:always;} div.toc{position:absolute;top:auto;bottom:4em;left:4em;right:auto;width:60%;max-width:30em;height:30em;border:solid thin #000;background:#f0f0f0;color:#000;z-index:300;overflow:auto;display:block;visibility:visible;padding:1em;}div.toc-heading{width:100%;border-bottom:solid 1px #b4b4b4;margin-bottom:1em;text-align:center;}pre{font-size:80%;font-weight:700;line-height:120%;color:#00428C;background-color:#E4E5E7;border-color:#95ABD0;border-style:solid;border-width:thin thin thin 1em;padding:.2em 1em;}li pre{margin-left:0;}blockquote{font-style:italic;}. footnote{font-size:smaller;margin-left:2em;}a img{border-style:none;border-width:0;}a{color:#000;text-decoration:none;text-shadow:2px 2px 2px #00f;}.navbar a:link{color:#FFF;}.navbar a:visited{color:#FF0;}ul{list-style-type:disc;margin:.5em .5em .5em 3.5em;padding:0;}ul ul{list-style-type:square;}ul ul ul{list-style-type:circle;}ul ul ul ul{list-style-type:disc;}li{margin-left:1.5em;margin-top:.5em;}li li{font-size:85%;font-style:italic;}li li li{font-size:85%;font-style:normal;}div dt{margin-left:0;margin-top:1em;margin-bottom:.5em;font-weight:700;}div dd{margin-left:2em;margin-bottom:.5em;}p,pre,ul,ol,blockquote,h2,h3,h4,h5,h6,dl,table{margin-left:1em;margin-right:1em;}p.subhead{font-weight:700;margin-top:2em;}.bigger{font-size:130%;}td,th{padding:.2em;}ol{margin:.5em 1.5em .5em .5em;padding:0;}li ul li{font-size:85%;font-style:italic;list-style-type:disc;background:transparent;padding:0;}li li ul li{font-size:85%;font-style:normal;list-style-type:circle;background:transparent;padding:0;}li li li ul li{list-style-type:disc;background:transparent;padding:0;}ol.outline{list-style:decimal;}ol.outline ol{list-style-type:lower-alpha;}a.titleslide{font-weight:700;font-style:italic;}div.slide.titlepage,.center{text-align:center;}strong,.navbar a:active,.navbar a:hover{color:red;}p.copyright,.smaller{font-size:smaller;}a:visited,a:link{text-shadow:1px 1px 1px #ccc;}a:hover,a:active{color:red;text-decoration:underline;}li ol li,li li ol li{list-style-type:decimal;}ol.outline li:hover,ul.outline li:hover{cursor:pointer;}ol.outline li.nofold:hover,ul.outline li.nofold:hover{cursor:default;}ol.outline li.nofold,ul.outline li.nofold{background:transparent url(nofold-dim.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.unfolded,ul.outline li.unfolded{background:transparent url(fold-dim.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded,ul.outline li.folded{background:transparent url(unfold-dim.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.unfolded:hover,ul.outline li.unfolded:hover{background:transparent url(fold.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded:hover,ul.outline li.folded:hover{background:transparent url(unfold.gif) no-repeat 0 .5em;padding:0 0 0 20px;} div.image-container{  margin-left: 1em;  margin-right: 1em;  margin-top: 0;  margin-left:auto;  margin-right:auto;  align: center; overflow:auto;  max-width: 80%; text-align:center;  clear:both;}div.image-description{  clear:both;  text-align: center;  font-style: italic;}}/*]]>*/</style>" muse-slidy-flagship-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  WHITE THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq muse-slidy-white-theme (make-hash-table :test 'equal))

(puthash 'muse-slidy-header-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-white-theme)
(puthash 'muse-slidy-footer-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-white-theme)
(puthash 'muse-slidy-leader-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-white-theme)
(puthash 'muse-slidy-background-css "<style>body {background: rgb(229,229,229); background: -moz-linear-gradient(left,  rgba(229,229,229,1) 0%, rgba(255,255,255,1) 63%); background: -webkit-gradient(linear, left top, right top, color-stop(0%,rgba(229,229,229,1)), color-stop(63%,rgba(255,255,255,1))); background: -webkit-linear-gradient(left,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); background: -o-linear-gradient(left,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); background: -ms-linear-gradient(left,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); background: linear-gradient(to right,  rgba(229,229,229,1) 0%,rgba(255,255,255,1) 63%); filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#e5e5e5', endColorstr='#ffffff',GradientType=1 );} </style>" muse-slidy-white-theme)
(puthash 'muse-slidy-fonts-css "<style><link href=\"http://fonts.googleapis.com/css?family=Cabin:regular,500\" rel=\"stylesheet\" type=\"text/css\"/><link href='http://fonts.googleapis.com/css?family=Droid+Serif' rel='stylesheet' type='text/css'/></style>" muse-slidy-white-theme)
(puthash 'muse-slidy-title-font-family-css    "<style>.title-h1{font-family:Cabin, serif;font-size:90px;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-title-font-color-css     "<style>.title-h1{color:#000011;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-author-font-family-css   "<style>.title-author{font-family:Cabin, serif;font-size:80px;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-author-font-color-css    "<style>.title-author{color:#333333;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-header-font-family-css   "<style>h1{font-family:Cabin, serif;font-size:46px;font-style:normal;text-decoration:none;text-transform:none;text-align:center;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-header-font-color-css    "<style>h1{color:#000011;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-body-font-family-css   "<style>body{font-family:\"Droid Serif\", sans-serif;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-body-font-color-css    "<style>body{color:#111111;}</style>" muse-slidy-white-theme)
(puthash 'muse-slidy-head-icon "" muse-slidy-white-theme)
(puthash 'muse-slidy-head-logo "" muse-slidy-white-theme)
;;(puthash 'muse-slidy-cover-image " <img src=\"a.jpeg\" class=\"cover\" />" muse-slidy-white-theme)
(puthash 'muse-slidy-cover-image "" muse-slidy-white-theme)
(puthash 'muse-slidy-slidy-css "<style type=\"text/css\" media=\"screen, projection, print\">/*<![CDATA[*/body{width:100%;height:100%;margin:0;padding:0;}.title-slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;background-color:transparent;border-width:0;margin:0;padding:20px 20px 0;}.title-content{position:absolute;bottom:10px;left:20px;}.title-author{font-family:Cabin, serif;font-size:50px;font-style:normal;font-weight:700;color:#000;text-shadow:2px 2px 2px #aaa;text-decoration:none;text-transform:none;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}.hidden{display:none;visibility:hidden;}div.toolbar{position:fixed;z-index:200;top:auto;bottom:0;left:0;right:0;height:1.2em;text-align:right;padding-left:1em;padding-right:1em;font-size:60%;color:gray;background:transparent;}div.background{display:none;}div.handout{margin-left:20px;margin-right:20px;}div.slide.titlepage.h1{padding-top:40%;}div.slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;line-height:120%;font-size:24pt;background-color:transparent;border-width:0;margin:0;padding:10px 20px 0;}div.slide + div[class].slide{page-break-before:always;} div.toc{position:absolute;top:auto;bottom:4em;left:4em;right:auto;width:60%;max-width:30em;height:30em;border:solid thin #000;background:#f0f0f0;color:#000;z-index:300;overflow:auto;display:block;visibility:visible;padding:1em;}div.toc-heading{width:100%;border-bottom:solid 1px #b4b4b4;margin-bottom:1em;text-align:center;}pre{font-size:80%;font-weight:700;line-height:120%;color:#00428C;background-color:#E4E5E7;border-color:#95ABD0;border-style:solid;border-width:thin thin thin 1em;padding:.2em 1em;}li pre{margin-left:0;}blockquote{font-style:italic;}. footnote{font-size:smaller;margin-left:2em;}a img{border-style:none;border-width:0;}a{color:#000;text-decoration:none;text-shadow:2px 2px 2px #00f;}.navbar a:link{color:#FFF;}.navbar a:visited{color:#FF0;}ul{list-style-type:disc;margin:.5em .5em .5em 3.5em;padding:0;}ul ul{list-style-type:square;}ul ul ul{list-style-type:circle;}ul ul ul ul{list-style-type:disc;}li{margin-left:1.5em;margin-top:.5em;}li li{font-size:85%;font-style:italic;}li li li{font-size:85%;font-style:normal;}div dt{margin-left:0;margin-top:1em;margin-bottom:.5em;font-weight:700;}div dd{margin-left:2em;margin-bottom:.5em;}p,pre,ul,ol,blockquote,h2,h3,h4,h5,h6,dl,table{margin-left:1em;margin-right:1em;}p.subhead{font-weight:700;margin-top:2em;}.bigger{font-size:130%;}td,th{padding:.2em;}ol{margin:.5em 1.5em .5em .5em;padding:0;}li ul li{font-size:85%;font-style:italic;list-style-type:disc;background:transparent;padding:0;}li li ul li{font-size:85%;font-style:normal;list-style-type:circle;background:transparent;padding:0;}li li li ul li{list-style-type:disc;background:transparent;padding:0;}ol.outline{list-style:decimal;}ol.outline ol{list-style-type:lower-alpha;}a.titleslide{font-weight:700;font-style:italic;}div.slide.titlepage,.center{text-align:center;}strong,.navbar a:active,.navbar a:hover{color:red;}p.copyright,.smaller{font-size:smaller;}a:visited,a:link{text-shadow:1px 1px 1px #ccc;}a:hover,a:active{color:red;text-decoration:underline;}li ol li,li li ol li{list-style-type:decimal;}ol.outline li:hover,ul.outline li:hover{cursor:pointer;}ol.outline li.nofold:hover,ul.outline li.nofold:hover{cursor:default;}ol.outline li.nofold,ul.outline li.nofold{background:transparent url(nofold-dim.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.unfolded,ul.outline li.unfolded{background:transparent url(fold-dim.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded,ul.outline li.folded{background:transparent url(unfold-dim.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.unfolded:hover,ul.outline li.unfolded:hover{background:transparent url(fold.gif) no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded:hover,ul.outline li.folded:hover{background:transparent url(unfold.gif) no-repeat 0 .5em;padding:0 0 0 20px;} div.image-container{  margin-left: 1em;  margin-right: 1em;  margin-top: 0;  margin-left:auto;  margin-right:auto;  align: center; overflow:auto;  max-width: 80%; text-align:center;  clear:both;}div.image-description{  clear:both;  text-align: center;  font-style: italic;}}/*]]>*/</style>" muse-slidy-white-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BLUE THEME (standard w3c slidy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq muse-slidy-blue-theme (make-hash-table :test 'equal))
(puthash 'muse-slidy-theme-name "Standard Blue" muse-slidy-blue-theme)
;; Instead of leaving the header images empty, use a transparent 1x1 png
(puthash 'muse-slidy-header-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-blue-theme)
(puthash 'muse-slidy-footer-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-blue-theme)
(puthash 'muse-slidy-leader-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-blue-theme)

(puthash 'muse-slidy-background-css "" muse-slidy-blue-theme)
(puthash 'muse-slidy-fonts-css "" muse-slidy-blue-theme)
(puthash 'muse-slidy-title-font-family-css    "" muse-slidy-blue-theme)
(puthash 'muse-slidy-title-font-color-css     "" muse-slidy-blue-theme)
(puthash 'muse-slidy-author-font-family-css   "" muse-slidy-blue-theme)
(puthash 'muse-slidy-author-font-color-css    "" muse-slidy-blue-theme)
(puthash 'muse-slidy-header-font-family-css   "" muse-slidy-blue-theme)
(puthash 'muse-slidy-header-font-color-css    "" muse-slidy-blue-theme)
(puthash 'muse-slidy-body-font-family-css     "" muse-slidy-blue-theme)
(puthash 'muse-slidy-body-font-color-css      "" muse-slidy-blue-theme)
(puthash 'muse-slidy-slidy-css "
<link rel=\"stylesheet\" href=\"http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css\" type=\"text/css\" />
<link rel=\"stylesheet\" type=\"text/css\" media=\"screen, projection, print\"
 href=\"http://www.w3.org/Talks/Tools/Slidy2/styles/w3c-blue.css\" />
" muse-slidy-blue-theme)
(puthash 'muse-slidy-head-icon "<img id=\"head-icon\" alt=\"graphic with four colored squares\" src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/icon-blue.png\"/>" muse-slidy-blue-theme)
(puthash 'muse-slidy-head-logo "<object id=\"head-logo\" data=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/w3c-logo-white.svg\" type=\"image/svg+xml\" title=\"W3C logo\"><a href=\"http://www.w3.org/\"><img alt=\"W3C logo\" id=\"head-logo-fallback\" src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/w3c-logo-white.gif\" /></a></object>" muse-slidy-blue-theme)
(puthash 'muse-slidy-cover-image " <img src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/keys2.jpg\" alt=\"Cover page images (keys)\" class=\"cover\" />" muse-slidy-blue-theme)





;; packages

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
    (while (re-search-forward ";base64,\&IMAGE_\.+\"/>" nil t)
      (replace-match (format ";base64,&IMAGE_%d;\"/>" (setq embedded-image-count (+ embedded-image-count 1))) nil nil)
      )
    )
)




;; TODO: make right mime type
;; TODO: see if gzip is possible for embedded references
;; TODO: consider auto-resize

;; TODO: decide what to do with level 3 headers and below


(setq muse-slidy-slidy-js "<!-- compressed inlined version of http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js -->
<script type=\"text/javascript\" src=\"http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js.gz\"/>")


(muse-derive-style "slidy" "xhtml"
                   :strings 'muse-slidy-markup-strings
                   :header 'muse-slidy-header
                   :footer 'muse-slidy-footer)

(provide 'muse-slidy)






