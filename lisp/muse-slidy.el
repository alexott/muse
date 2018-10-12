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

;;; Usage
;;
;; (require 'muse-slidy)
;;
;; ;; open any muse file 
;; ;; M-x muse-publish-this-file slidy
;; ;; The result is a self-containted presentation

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

If an override value has been set, use it. Otherwise default to the one in the selected theme."
  (if (and (boundp (intern  (concat "muse-slidy-override-" name)))
	   (not (string= (eval (intern  (concat "muse-slidy-override-" name))) "")))
      (eval (intern  (concat "muse-slidy-override-" name)))
      (if (boundp (intern  (concat "muse-slidy-" theme "-theme")))
	  (gethash (intern (concat "muse-slidy-" name))
		   (eval (intern (concat "muse-slidy-" theme "-theme"))))
	""
	)
  )
)

(defun muse-slidy-get-themed-customization (name)
  "Return the value of the customization denoted by string name"
  (muse-slidy-get-configuration-option name muse-slidy-theme)
)

(defun file-contents-as-base64 (filepath)
  "Return the contents of a file as a base64-encoded string."

  (if (file-exists-p filepath)
    (with-temp-buffer 
      (insert-file-contents-literally filepath)
      (mark-whole-buffer)
      (base64-encode-region 1 (point-max) t)
      (buffer-string)
    )
    ;; if file doesn't exist return a 1x1 transparent png pixel 
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg=="
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

(defcustom muse-slidy-theme "flagship"
  "Base theme to use when publishing muse slidy presentations.

Caution: when you set a new theme, specific customization are
  preserved until set to nil. Set individual overrides to nil or use M-x
  muse-slidy-reset-look-and-feel to reset all customizations to nil." 
  :type '(choice (const :tag "Muse Slidy default theme" "flagship")
		 (const :tag "White basic look" "white")
		 (const :tag "Treehug Green" "green")
		 (const :tag "W3C Slidy standard blue look" "blue")
		 (const :tag "Fancy black" "black")
		 )
  :group 'muse-slidy
  )


(defcustom muse-slidy-markup-strings
  '(
    (section . "</div>\n<div class=\"slide\"><img src=\"data:img/jpeg;base64,&STATIC_HEAD;\" width=\"100%%\" style=\"position: absolute; top: 0px; left:0px; z-index:-1\"/><img src=\"data:img/jpeg;base64,&STATIC_FOOT;\" style=\"position: absolute; bottom: 0px; left:0px; z-index:-1\"/><h1>")
    (section-end . "</h1>")
    (image . "<div class=\"image-container\"><img class=\"illustration\" src=\"data:img/jpg\;base64,\&IMAGE_%s.%s\"/></div>\n")
    (image-with-desc . "<div class=\"image-container\"><img class=\"illustration\" src=\"data:img/jpg\;base64,\&IMAGE_%s.%s\"/></div><div class=\"image-description\">%s</div>\n")
    (subsection . "</div><div class=\"slide\"><h1>")
    (subsection-end . "</h1>"))
    "Strings used for marking up text as HTML Slidy. DO NOT MODIFY UNLESS YOU ARE SURE. ALL LOOK AND FEEL OF PRESENTATIONS CAN BE MODIFIED THROUGH MUSE-SLIDY-OVERRIDE-* CUSTOMIZATIONS."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-html)

;; TODO: consider making local (too big! would need to modify)
(setq muse-slidy-slidy-js "
<script src=\"http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js\"  charset=\"utf-8\" type=\"text/javascript\"></script> 
")

(defcustom muse-slidy-footer "
<lisp>(muse-slidy-reference-all-images)</lisp>
<!-- This presentation published with Emacs Muse slidy mode. RmFiaW8gQXJjaW5pZWdhcyx0aGFuayB5b3Ug-->
</div>
  </body>
</html>\n"
  "DO NOT MODIFY UNLESS YOU ARE SURE. ALL LOOK AND FEEL OF PRESENTATIONS CAN BE MODIFIED THROUGH MUSE-SLIDY-OVERRIDE-* CUSTOMIZATIONS. Footer used for publishing XHTML muse-slidy presentation files."
  :type 'string
  :group 'muse-slidy)




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
      <span class=\"title-h1\"><lisp>(muse-publishing-directive \"title\")</lisp></span><br/><span class=\"title-author\"><lisp>(muse-publishing-directive \"author\")</lisp></span>
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
;; 1x1 transparent png
(puthash 'muse-slidy-header-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-flagship-theme)
;; 1x1 transparent png
(puthash 'muse-slidy-footer-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-flagship-theme)
(puthash 'muse-slidy-leader-image "iVBORw0KGgoAAAANSUhEUgAABIAAAAE6CAYAAABuwSnuAAAACXBIWXMAAC4jAAAuIwF4pT92AAAK
T2lDQ1BQaG90b3Nob3AgSUNDIHByb2ZpbGUAAHjanVNnVFPpFj333vRCS4iAlEtvUhUIIFJCi4AU
kSYqIQkQSoghodkVUcERRUUEG8igiAOOjoCMFVEsDIoK2AfkIaKOg6OIisr74Xuja9a89+bN/rXX
Pues852zzwfACAyWSDNRNYAMqUIeEeCDx8TG4eQuQIEKJHAAEAizZCFz/SMBAPh+PDwrIsAHvgAB
eNMLCADATZvAMByH/w/qQplcAYCEAcB0kThLCIAUAEB6jkKmAEBGAYCdmCZTAKAEAGDLY2LjAFAt
AGAnf+bTAICd+Jl7AQBblCEVAaCRACATZYhEAGg7AKzPVopFAFgwABRmS8Q5ANgtADBJV2ZIALC3
AMDOEAuyAAgMADBRiIUpAAR7AGDIIyN4AISZABRG8lc88SuuEOcqAAB4mbI8uSQ5RYFbCC1xB1dX
Lh4ozkkXKxQ2YQJhmkAuwnmZGTKBNA/g88wAAKCRFRHgg/P9eM4Ors7ONo62Dl8t6r8G/yJiYuP+
5c+rcEAAAOF0ftH+LC+zGoA7BoBt/qIl7gRoXgugdfeLZrIPQLUAoOnaV/Nw+H48PEWhkLnZ2eXk
5NhKxEJbYcpXff5nwl/AV/1s+X48/Pf14L7iJIEyXYFHBPjgwsz0TKUcz5IJhGLc5o9H/LcL//wd
0yLESWK5WCoU41EScY5EmozzMqUiiUKSKcUl0v9k4t8s+wM+3zUAsGo+AXuRLahdYwP2SycQWHTA
4vcAAPK7b8HUKAgDgGiD4c93/+8//UegJQCAZkmScQAAXkQkLlTKsz/HCAAARKCBKrBBG/TBGCzA
BhzBBdzBC/xgNoRCJMTCQhBCCmSAHHJgKayCQiiGzbAdKmAv1EAdNMBRaIaTcA4uwlW4Dj1wD/ph
CJ7BKLyBCQRByAgTYSHaiAFiilgjjggXmYX4IcFIBBKLJCDJiBRRIkuRNUgxUopUIFVIHfI9cgI5
h1xGupE7yAAygvyGvEcxlIGyUT3UDLVDuag3GoRGogvQZHQxmo8WoJvQcrQaPYw2oefQq2gP2o8+
Q8cwwOgYBzPEbDAuxsNCsTgsCZNjy7EirAyrxhqwVqwDu4n1Y8+xdwQSgUXACTYEd0IgYR5BSFhM
WE7YSKggHCQ0EdoJNwkDhFHCJyKTqEu0JroR+cQYYjIxh1hILCPWEo8TLxB7iEPENyQSiUMyJ7mQ
AkmxpFTSEtJG0m5SI+ksqZs0SBojk8naZGuyBzmULCAryIXkneTD5DPkG+Qh8lsKnWJAcaT4U+Io
UspqShnlEOU05QZlmDJBVaOaUt2ooVQRNY9aQq2htlKvUYeoEzR1mjnNgxZJS6WtopXTGmgXaPdp
r+h0uhHdlR5Ol9BX0svpR+iX6AP0dwwNhhWDx4hnKBmbGAcYZxl3GK+YTKYZ04sZx1QwNzHrmOeZ
D5lvVVgqtip8FZHKCpVKlSaVGyovVKmqpqreqgtV81XLVI+pXlN9rkZVM1PjqQnUlqtVqp1Q61Mb
U2epO6iHqmeob1Q/pH5Z/YkGWcNMw09DpFGgsV/jvMYgC2MZs3gsIWsNq4Z1gTXEJrHN2Xx2KruY
/R27iz2qqaE5QzNKM1ezUvOUZj8H45hx+Jx0TgnnKKeX836K3hTvKeIpG6Y0TLkxZVxrqpaXllir
SKtRq0frvTau7aedpr1Fu1n7gQ5Bx0onXCdHZ4/OBZ3nU9lT3acKpxZNPTr1ri6qa6UbobtEd79u
p+6Ynr5egJ5Mb6feeb3n+hx9L/1U/W36p/VHDFgGswwkBtsMzhg8xTVxbzwdL8fb8VFDXcNAQ6Vh
lWGX4YSRudE8o9VGjUYPjGnGXOMk423GbcajJgYmISZLTepN7ppSTbmmKaY7TDtMx83MzaLN1pk1
mz0x1zLnm+eb15vft2BaeFostqi2uGVJsuRaplnutrxuhVo5WaVYVVpds0atna0l1rutu6cRp7lO
k06rntZnw7Dxtsm2qbcZsOXYBtuutm22fWFnYhdnt8Wuw+6TvZN9un2N/T0HDYfZDqsdWh1+c7Ry
FDpWOt6azpzuP33F9JbpL2dYzxDP2DPjthPLKcRpnVOb00dnF2e5c4PziIuJS4LLLpc+Lpsbxt3I
veRKdPVxXeF60vWdm7Obwu2o26/uNu5p7ofcn8w0nymeWTNz0MPIQ+BR5dE/C5+VMGvfrH5PQ0+B
Z7XnIy9jL5FXrdewt6V3qvdh7xc+9j5yn+M+4zw33jLeWV/MN8C3yLfLT8Nvnl+F30N/I/9k/3r/
0QCngCUBZwOJgUGBWwL7+Hp8Ib+OPzrbZfay2e1BjKC5QRVBj4KtguXBrSFoyOyQrSH355jOkc5p
DoVQfujW0Adh5mGLw34MJ4WHhVeGP45wiFga0TGXNXfR3ENz30T6RJZE3ptnMU85ry1KNSo+qi5q
PNo3ujS6P8YuZlnM1VidWElsSxw5LiquNm5svt/87fOH4p3iC+N7F5gvyF1weaHOwvSFpxapLhIs
OpZATIhOOJTwQRAqqBaMJfITdyWOCnnCHcJnIi/RNtGI2ENcKh5O8kgqTXqS7JG8NXkkxTOlLOW5
hCepkLxMDUzdmzqeFpp2IG0yPTq9MYOSkZBxQqohTZO2Z+pn5mZ2y6xlhbL+xW6Lty8elQfJa7OQ
rAVZLQq2QqboVFoo1yoHsmdlV2a/zYnKOZarnivN7cyzytuQN5zvn//tEsIS4ZK2pYZLVy0dWOa9
rGo5sjxxedsK4xUFK4ZWBqw8uIq2Km3VT6vtV5eufr0mek1rgV7ByoLBtQFr6wtVCuWFfevc1+1d
T1gvWd+1YfqGnRs+FYmKrhTbF5cVf9go3HjlG4dvyr+Z3JS0qavEuWTPZtJm6ebeLZ5bDpaql+aX
Dm4N2dq0Dd9WtO319kXbL5fNKNu7g7ZDuaO/PLi8ZafJzs07P1SkVPRU+lQ27tLdtWHX+G7R7ht7
vPY07NXbW7z3/T7JvttVAVVN1WbVZftJ+7P3P66Jqun4lvttXa1ObXHtxwPSA/0HIw6217nU1R3S
PVRSj9Yr60cOxx++/p3vdy0NNg1VjZzG4iNwRHnk6fcJ3/ceDTradox7rOEH0x92HWcdL2pCmvKa
RptTmvtbYlu6T8w+0dbq3nr8R9sfD5w0PFl5SvNUyWna6YLTk2fyz4ydlZ19fi753GDborZ752PO
32oPb++6EHTh0kX/i+c7vDvOXPK4dPKy2+UTV7hXmq86X23qdOo8/pPTT8e7nLuarrlca7nuer21
e2b36RueN87d9L158Rb/1tWeOT3dvfN6b/fF9/XfFt1+cif9zsu72Xcn7q28T7xf9EDtQdlD3YfV
P1v+3Njv3H9qwHeg89HcR/cGhYPP/pH1jw9DBY+Zj8uGDYbrnjg+OTniP3L96fynQ89kzyaeF/6i
/suuFxYvfvjV69fO0ZjRoZfyl5O/bXyl/erA6xmv28bCxh6+yXgzMV70VvvtwXfcdx3vo98PT+R8
IH8o/2j5sfVT0Kf7kxmTk/8EA5jz/GMzLdsAAAAgY0hSTQAAeiUAAICDAAD5/wAAgOkAAHUwAADq
YAAAOpgAABdvkl/FRgAA7yZJREFUeNrsnXW4XNXVh98bdw8xQhKCBAsJToK7u7sWaYu0FCnFCkWK
FwoUd3d39xAgCcFjhLi7535//Nb5Zu7cOSP3jpyZWe/zzHPvzJyxffbZe+3fXlL15ptv4jiO49SJ
BkAzoCnQBmhnt7Z2a2/3O9j9dkAroGXc37ZAQ+A54FxgeqYfvvPOO/sZcBzHKUHeeuutSvq5nYDr
gH2AlcAcYAEwP+7vbHt8pv0/y+7PsfuzgbnAEmCxvY/jOE7BKXX7u5GfQsdxnFo0RgJNe6CLGa+d
gW52vwsSdToQE3yaIyGoYR0/83hgMHA48LWfAsdxHKcM2AR4DFgj7rGOWb7HCiT8LCImCM202xS7
TQKmoU2UKUhAmg8s81PgOI4TwwUgx3EqkSZIuOkO9ABWBXoBfYCuSOxZBWiNPHwKxVrA+8BRwIt+
mhzHcZwSZl/gIZtL60NDoIXdMhGPFgPzgKlIFJoMjAXGAL8DE+02G1jqp8lxnErCBSDHccqVBsgz
Z1VgdbT7uLb93x1587Sx4/LFSjMuF6Pdy/lmlAZ/FwAL0U7lAuTevhwJVFVAdao3r7AQgmL1oWCe
bGy36jSvqbZzXW3nf7k3o+PUyT5tYONgU/ubiirk6RF4eyzHQ4SKTZWNmZfZ+WyDwp7bIyGnJRKG
WsX9bYo2XZrUc25uZrfOKebmuchraCIwGvgJ+AUJReORp5H3IcdxynKCdRzHKXWaAz2RwLMu0B9Y
E+iNdgtzJfKsQILNfOR6PiPu73S7zbDbHGJCT3BbZguTFX7KCj7XtbAFRidi4Xvt4/62swVKEM7X
3BYvLW0R0NIWJ+kEoJV2rqvtXC+w8z0/7jabWI6LWXH/zyAmBi7yfuKUOA3tOgoW/R3jrrX2cf8H
115wjTa01zQyEaF1BmN4FRJeF9ixC2y8XUQsbGhu3LU2M+7vTBu759v47qJtbqgGns6wnzSy8bZ1
3K2Vjccd7dbJbh1t3A7+trK+k034dYO4vrcOsGPCGD4DCUG/AMOB75FANN76k+M4TkkbxY7jOKVE
UyTsrA9sCgxAYk9PMyDrY6zOs0XBBGIu4r/b/SnEBJ9ZtlDwBXo0aGKLyW7WD4KQvlXtsU5xi89m
5Nfrqz4sJ+YRFixKJ1n/G5/QL2f5QsQpMs3tmupOLJy2Z9x1FyzQA4+PqNqcK5GXZiDCBtfd78A4
+zveHpuFhwzlmhXEcvzMt3bOhCAsLBDyO6L8fEFYd3y/bI9EpaoM3rcB8hzqbDbGEfb4MusHvwDf
AkOA75BQtMRPo+M4pYILQI7jRJ22wAbA5sAWyLunD3UTe6rRLvBElAtgDDDKboHIM82NucjSygz6
NYiF9K0ZZ+y3ydDAj/Kc3MZuvVL04YUop8UkFLrwq/XhMbZgne6LVCdHNEECapAjra9de6sjkaer
LcJL+bprQCy/TI8M5o7fTQT4ya69X+3x+d5dCsoKYt61v6U4rikScwJxqK/d+tgt07mjsfX71YFd
7bFlNu4OBz5FotAI5HHmOI4TSaq8DLzjOBGjDfLqGQxsAww0wy1b5qLduh+BH5AL969mvE/FvXei
viDrbAvN9YENgfXMWO+Gb16kYr71+3HASLv9iISi6d7vnRAaIqFndaCfXW/rIeGnJxJfneQsR2Ls
GLvehiHPkF/RhoLnkYl2v18FbSKsgULI17FroKfZI9kyBfgG+BD4BHkLzfWmdpzyodTLwLsA5DhO
sWmEvDi2B3ZCXj7dsnyPuWhHdgQw1P7+aka5G9/Rp40Z3AORy31/VBGtrTdNzpiFQhW+t8XJd/b/
RFwUqsRFbw+75ja027ootLa9N0/OmAP8jLxDvrTr7mfcO6QUaGB2yBrIA3lj+7sm2YtCk4DPgbeB
98xW8TxTjlPCuADkOI6TPS1sob87cqVen8y9OqqRF88wM6q+jFvIVnvTlgRdbNG5JTDIzn/3In6f
apQDZBGxim2LkDfNEhRiUIUSyC63/4OqQ3Pi3qMJEq2q4x5rg0IQgqp0jexvY2IVb5oTq/xWSBYg
oXSEXUdDkbfQTO+iZUUHJPZsDGwWt5BtUYTrbIldY8F1FlxDy4lVXVqCRP3geqiy55bGPRZcQ9V2
a4QS+lajXC9NkddS84TrrBnFDVebiAShz4AvkHfIFO+ipbFmsnlqXbuOtrB5bNUs+tRyJL6/AbwK
fIVCeh3HKSFcAHIcx8mMlrbgPwAJP72zWDT8bobSR8ileqQtXp3SWYAOBLa1W39brBWC+cj7ZRqx
kr9TUA6dqSgsag4SeYIqQEGC73x5jzVAgk+Qd6Sl/W1jbbUK8tAIbt1RSFwHe10+mWYLlM+QwPoN
yo/l4mrpLFJ72PU2COVOW5/wcti5YkncdTbR+kxwm4pExbk2bi+Muy3N83XWMO46C8qNt0Xhbqug
HEbd4v52Rl5QhQp5m40EofeAj4GvcQG21Oya9VDI+tZoY2vVLF4/FngNeNbGXLdrHKcEcAHIcRwn
nMa2CDkU2IPwxLbJFu1fI5fpd5G3jyfYLK3zvi6wHQrr2zzPC9AlSNQZRywh8igzrqfYArSUDesq
JJgFC9bVUWhCH/u7qi1oG+fhsxcgD7tPkAD7Fcox5IJQdPpGT2ATW4AOtmuvZR4+axkSTH+362yM
/R1NTFCdXeJ9o6VdZ13QJkWQMHhNYDV7vGkeP38q8gx6B3jfrr1l3s1LhlbIK2gnYAfkdZfptTgO
eAV4AolBft4dJ6K4AOQ4jlObtYHDgENsMZIJU4APgJfN8B3vzVhStEEeXruZ8bsu+Sm3PpdY1ZXv
iCVbnUDl7p62RV4ffazd17NrsA8S3nJ5HhZY23+ExNkhuMdCoemAPA12QKJPf3Ir+KxEnjxjUKWr
kSZEjLHrrFJz2LS06yxITr+B/V2duiULzuQ8fI82Ql5FwpAnEy4tVkNer3vZ30wLWnwPPAk8bteg
4zgRwgUgx3GcmHG8J3ACSuicSajKFOB14Hkk+sz2ZiwpOtkidB8757nO47Mc7Yp+i3LUfIkSaHq+
p8yFgt5IEAqS/fazRWyu8qBMt/MSeOu5x0LuaWzncAdgR5R/pFMORYaJKPfTMLuNRN5zLuxlYEfb
uLemnZfNUBXLXuS+WuFEFCr2ol1r0735S4p2yCv2AGAXMhODlto5vxd5B3mImONEABeAHMepdPoA
xwHHkFlen1m2WHzMDJvZ3oQlRSdbhB5gfzvm8L1XoNCtL1AJ3c+Rd89ib/ac0ZpYZZvNgI2QKJSL
6k8r0W71O0jY/cxFhDrTAYVz7YI86tZE+Wzqyywk9nyNhLvhdo15iG3uaGbX2BbANigEtm+Ozl/A
DLvOnrW/LgaVFu3Qpsnhdn1nMv6OBR4E7kfeeI7jFAkXgBzHqVQ2B/5oQkC68IPltqh/CHgB5Ytw
SodWyPvgUFS1LZeiz3TgUyQKfgj8gHY9ncLRBXmYbG6L1oFkV9kmjGkose0rtkgd602dkt5IVN0L
iT/1zZsVJND/xq6xIci7x6tOFZYmwDpIDNoJ5cXrlMP3n4GqSj1h15l7iZQWXYF9gaNt/E0nFC5A
wt9/za5yHKfAuADkOE4l0dAEgLNsoZIut8gUM0rvQ2E8Tmmd683QDuX+ZFfZJB2jgTdNGPjMFjBO
dGhlC9bNiVW26U39BKGFJkC8hLyDvsfD+KpQzqbdUBjlJtSvNHs1EtmGoBxNXyBB1b17okVHlC9t
T+ThtXoO33s8Cql+DHl4rfDmLikGIo/qQ0kfIrYSCX43AG/5uXacwuECkOM4lUADM1YvMMM1HUOB
/wHP4CEgpcaqwMHAUWaM5ipXzC8owfdztkD1sK7SoTnyENoahS1sinat68oyJAi/gjwCR1TQ4qUh
Cr/bF9gbJXCuT/W2yXY9vYdEn5HAIu+yJUMzu572R55fa+bofauR59fDwFPIE8wpHToABwKnoEpi
6fgMuMrG1JXefI6TX1wAchynnKkyozQT4Wc52tn/D0pQ6btRpUNjVKHkRCT0tc7R+05CCUsfNwN1
iTd1WdAW5Q7awW4DqLvnygqUePhFJAYNL8MFTAMk9OyLPH02pO75YBYi8exdu31N5VblKjeaofCw
Q62fdM3R+84zYeAeVGnTk7SXDg1tjD0DeQqmSyweCEEv4x6WjpM3XAByHKdc2Q64xP6mW5A8CdyM
h3mVGp1RiNeJtkDNBUttYfoAKl3sZYvLmypU6ngbFB66DdCzju8ViEFPIzHo+xJvl3WA/YCD7Pqq
q+gzHuXHesP+/uaLu7KnHbA7Kq6wA5lV1cyE4UgIegzl6HJKhwHAmcAhpBfc3wcus7+O4+QYF4Ac
xyk3NgD+iXarU4X/LELu5dejyj9O6dAfONUMyVwldP7N+sNDqMqQU5m0RCELuyNBaAPqVg57Gcph
E4hBY0vk9/e2sfNglEOpLr99OQqLewN4DYXUemLfyqUfEoKOJne52Gag/Hx3WF9zSoe1gb+iMO3m
KY6rtrHzHyg01HGcHOECkOM45UJn4O/AaUDTFMcFws91wM/ebCVDvCv5LuRuR/lz4HaU22eeN7OT
0Of6odCFvVBS8bqEii1EOW4eQ4JI1PKKdUChk4cjD6iWdfyNX6LQjdeRiOphtE48rVGuoNNQtahc
sBQl5L/ZrjHvc6XDWsA5pBeCFiOh7wq84ILj5AQXgBzHKXUaACcgr59uKY5biUK9/okqyzilQVNb
NJxtC/BcEOR7uhG5mHvSSScT+qAy2PsioaQuuaamoXwmD6Okx0uL9FuaoKTYRyHxpy4l2+ehkK7n
UDWfsd5FnAzn7O1sTN+duocWJvKljenPFvG6crJnHeBi5NGbqjLrJDvuHjyE1HHqhQtAjuOUMgPQ
zt82aY77AHkHfepNVjK0BI5AruJrpzhuPvI+6Ez6il/LbbF6NUo+6zh1pQfyDDoACSl1EYN+RGEs
j1O4sMN+wGEoUW+/Orx+HhKunkUi6gTvCk492Ag4z66jdOGG1UhAbQG0SnHcD0gIehivKFdKDEIJ
oDOx587CczY6Tp1xAchxnFKkORJ0zkGVR8IYY8c9ge8YlQptkEfXmSgfSRiTgbdRRaetUdLRMJbb
gvVqVFrYcXJJD5Qv6ADk2ZBtCNUS5EnzAAqhynVVrLYohO1YW1w1zfL1C5Cn3LMor4+LPk6uGQic
j0qHp/IImm0CwDzkjZeq0tho4BbgXjyZf6nQAInT/0Iel2EsRmH8V+Iin+NkjQtAjuOUGlugnC0D
0iyobrIF/2xvspKgJXASCgvoleK4UcjraxLKJbFDmvd9DbgUhQc4Tr7phUKqDrWxKttcVZNQ4uj7
qb+X2kbAcaiKV7csX7sU5cd6AoWsjfNT6xSAzW283i3Nce+aHdANbRb0TXHsOOAG4G7kLepEn7Zo
8+5MUgvW35od8Lk3meNkTqkLQA38FDpOxdAYxX9/SGrx52NgS7SbONubLfI0QcLPN0i0CxN/hqLK
RBvaa+4jtfgzDNjDbi7+OIViHHAbsK311X+g0tWZeiB2A/5sffZD4HhSe7cl0s5e86G9x5/JXPyp
tu/6D/vu29pvcfHHKRRfoLxAe9kYHsYONgc0sb56sM0RyeiFNg2+tbmmiTdz5JmDQgO3NJsujAE2
1l1sNqLjOBWAewA5TmWwBnLj3jrFMfOBS8zQ80og0acBCpm5GJXaDluQvgVcjyq99EZ5HQaneN9Z
qFrIf5EnmOMUm0bApij3zv5AzyxfPxl4ysbAb1MshE6whXDXLN9/PMqN9TgwBIVMOk6xaQr8EQmS
7VMc9zFKZj4OVYj8K7Az4TnhRqBiEM/iBQBKgYbIE+gyUud++tDGwFHeZI6TGg8Bcxwn6hyESoB2
THHMR8ApeHWvUmFbJNJsFfL8cuAFFOMfuHYfB/yH1Ml2nwL+hnssONGlNcpdcrQtUltl8drltsi5
E3jRHtsXOBnl9mmUxXvNR+LqQyiX1jw/NU5E6QVci8TNMOYBZ6DQSVD45TnAfoTnFPoYiUsfeBOX
BOvY2LdVimNmAKeiMFrHcUJwAchxnKjSCCUCPDfFMctMSLjK/neizdrA5SjRZ7IQ3qXAk2bsD7fH
mqLQsFNTvO8EtEP4jDexU0L0RAL30ciDpyqL1/5if9fM4jXVyIPoIVsgjfdT4JQQB9lc0CPFMXeg
ClGB9+cA5BF0CMlDv1bavHER8JM3ceRpDFyI8gOlCvn6tx3n3oyOkwQXgBzHiSKdgQdJnQhyHHL3
fdebK/K0QzmZ/oxK+CYSCD/XAN/FPb4q8CipQ/+eAf6EwmQcpxRpiDwWjkFhkZ1y/P7TUbjLg8ij
zkNknVKlG6rsdWCKYz4CjgB+j3tsfZRTJkwIWojCx68h91X4nNyzAwqJTVUw4nUbU6d5czlOTTwJ
tOM4UaM/CnNIJf68agsmF3+iTRXybvjWjO9E8WcFEnA2tePixZ+B1g/CxJ9FSPg5CBd/nNJmBfAJ
CmNdB4V0vY3CtOrKfOAde6917L0/wcUfp7SZZGP+nwgv/721zR0D4x77zuaYzZAYmngdtAAusLnq
SLLzxnMKz7tmA76a4pjdrB/09+ZynPLCBSDHKS92Ad4D+qU45ipgb1/0R57+KMfIgyTfpXsDJXM+
iFi4V8CO9to+Ie/9M7AdSvTsOOXEdFSu+gQk2NSVj1HerLvtPR2nnPivzQE/hzzfx+aQHRMeH4a8
hwbbHJRIb1Ro4E0XDiLPZLMFr0pxTD8khO/szeU45YMLQI5TPhyFEpt2CHl+PnA4iv32yh3RpbUZ
ZF8mMb6xx3dFu3NfJHl+b+AlwpN+v4aSQHppd6cc2QQlsv3erpO6shtKin+/vafjlBtf2lzwWsjz
HW0u2TvJc1/YNbIbqnyXyE52zBVAS2/qyLLSbMKjgAUhx3SyfnCkN5fjlAcuADlOeXAqSkzaNOT5
iWaoPe5NFWl2Bb5C+X4Sz+UY5JEwGO2uJmMv4Amgecjz/zVj3mP6nXKisfX9t1COnmPJrjpYGK3s
vT63996L1IlTHafUmGZzwh0hzze3OWWvkOffAAbZ3DQm4blmKJHwV8g72Ykuj5j9MSnk+abIs+sU
byrHKX1cAHKc0uc84PYUz49Ert6feFNFlk4o1OQ1YK2E5+YBl6FqLA8QXpVjG+AxwsWfS1DeB89h
4pQLrVGOnq/QDvVOhJesjucnVNnor2RWuaihvfdL9lkn22c7TjmwAjjN5ohkNLe5ZZuQ55fb3DTA
5qp5Cc/3QwmF7yb3Cdqd3PGJ2YrfpzjmDlJXlnUcpwTwKmCOU9qcB1yd4vnPgf3xfD9RZl9UPSVZ
np8nUWLN0WneY32U1LFzyPN/AW70pnbKhK7AiWg3umeGr1lu18jtSGgNylw3BXa3BfAOQKMM3288
8D/gHh9fnTLibOCGkOemorDk79K8R1/gSlQxLJFxwJnAC97UkaUbSvS9RYpjzgWu9aZyKhWvAuY4
TrE4ldTiz9vAnr44iSztgLuA56kt/oxALvOHkl786YDct138ccqdNYCbkFfjFWQm/sxCu9abohCH
54mJP9j/z9tzm9qxszJ43572HUbagnkNPz1OGXCjzRnJWMXmmg5p3mOUzV27UFss6mXX2502BzrR
YxIKC0xVJfbfZoM6jlOCuADkOKXJ4cBtKZ5/A3n+zPSmiiQ7osSZJyU8Ph/lTNgc5RzJhDsJr7Zy
Pi7+OKXPQBRiMgx5D3TI4DW/Ig/J9ZB3z7cZvOZbO3Y9e+2vGbymA/KaGGbfcaCfLqfEudHmjmT0
J3XIeTxvobLxF9rcFs/JKAn19t7ckWQ68k5+I8Ux/0VCn+M4JYYLQI5TmuLBPUBVyPMfoNLg872p
IkdT4F8oiXOix8BrqNrQlcCiDN/vLFSSNxk3ANd4kzslShXKOfICEkuPAVqkeU01Kt9+GLAB2qWe
VIfPnmSv3cDe6+MMXtPCvuMQ5OGwtZ9Cp4S5hvBQsEOQEJsJi2xO24Ta1cbWRCLRv4Am3uSRYz5w
cIrxrwFwLy7iOU7pGVieA8hxSop+yC23W8jzQ4A90O6NEy3WNmNpUMLjU4G/AQ9m+X4bmmGWrNrR
c0gEXOnN7pQYDVHoyLkoIWkmLAZeBP5D/pLdbw38GYVGNMvg+Gokxv8bCb6efN0pNRoAzwD7JXlu
HqpIOSLL9zwGuI7aIcsfo7xeP3uzR47OwKtIxEvGJJQ/7UdvKqdS8BxAjuMUiraoDGeY+PML2q1x
8Sd6HA58Rm3x5zEUMvJgHd7zapKLP98j93oXf5xSognyLPjEFhvbZfCaGSgn0IYoFCGflQ4/su+3
oX3mjDTHV9lveNW+10G4l4NTWqxEYcrJqkK1RuJmtjxoc95jCY9vhYpWHO7NHjmmmW05KuT5bsBD
QBtvKscpDVwAcpzS4RZg45DnZtoCaJw3U6RobuftUaB93ONT7HwdAUysw/seAeyW5PFFZrDP8KZ3
SoQWwNHIe/EJlP8qHaORh9A6KP9OIb0GfrbPXMe+w+gMXrM58JT9xqNsXHCcUmCGzSnJwpJ3s7ko
WybY6w5HHrAB7W2uvIXMvOycwjEWCeBheSU3sfPmOE4J4AKQ45QGp9kiKRkrgBOAb7yZIsXqqBLb
nxIefx4JeU/W8X2boQS1ybgceRo5TtRpYQvLb5BXQP8MXvM1cKwdey3amS4W0+w79Lfv9HUGr+mP
dsq/td/uQpBTCnxmc0syzqPuYs3jwEY2J8bzJ5s7+3rTR4qvbdyqDnn+GLNVHceJOC4AOU70GUDq
cu8Xo0SpTnTYndohX/NR2dT90Q5oXTk8ZLH8Ccqt4DhRpjVKIDsCuAtYK83x1Sjv2d7Ik+ZBYEGE
fs8C+06bEyudXJ3mNWvZbx9hbdHau4UTca4jeYhlf+oXtjXB5sTTUF6hgMHApzaXOtHhOeCiFM9f
jcJkHceJMC4AOU60aYISm4bFVj+LKmw40eEclJB2lbjHvgC2AP5Xz/duCJye5PEVwAXAMm9+J6LE
Cz83IQ+5VCxHCWi3QZUPX7bHospy+4472nd+JoPv29fawoUgJ+osA/5O8txypxJelTRT7gC2RKXh
A1axufQcb/5I8S+zPZPRBoWCeb4zx4kwLgA5TrQ5i/BywqNCxACnODQH7kNhIY3iHr/BFoQjc/AZ
O5C8Esf9KEmt40SN9jaOBcJPrzTHL0DV8jZCiZM/LsHf/LF9941tTFiY5vhe1BSC2nq3cSLIh9af
E9nM5qb6MtLmyhvjHmtkc+p9eMhklDgdGBPy3NY2jjmOE1FcAHKc6NIPeXWEcQZKJuwUnx7AG8Bx
cY9NQ+Vz/woszdHnJHO1nwdc76fAiRgdkMfA97agSyf8zCZW0etEsi8vHUWGo/xs/ZEn5+w0xwdC
0PfWdh28GzkR4waSC5qH5uj9lwB/QQJqfEXT42yO7eGnIBJMAf6c4vkLSB/e6zhOkXAByHGiy6VA
u5Dn/ovKCzvFZwDwATU9tT5B7uy5zM3UGoWXJPIo8IOfBiciBMLPSBQq0DXN8dNQgtkNUHWtUWXY
JqPQjvgGwBWkT17d3dpupC2k3CPIiQrfAw8neXwnoFUOP+cZFDYdX9Rga5trB/hpiASvmC2ajPbA
Zd5EjhNNXABynGiyC+E7ar+gxM9O8dkDeI+a1Ur+i4SaXC9kBwKrJTy2gvrnFXKcXNAahXoNJzPh
ZwJwPrCejWe/V0Ab/Y4SqK6PhJ10yeC7ohxvw61tPUeQEwXusLknnj4obDOXjEKhZbfHPdbX5tw9
/DREgovNJk3GYWbLOo4TMVwAcpxo8vcUz10AzPQmKjonoPK17ez+YuAUVMJ2SR4+b6skj72Jymg7
TrEIhJ/vUKhXuhCN0XZ8f+AailvKvVhMRdVy+ltbjE5z/GrWtt/hQpBTfL4B3slwjqovi1G+mVPs
f2zOfQ443k9F0ZlJ6lQFf/cmcpzo4QKQ40SPvYFtQ557DrlGO8XlfOAeoLHd/w25wN+Zx8/cNMlj
j/qpcIpEovCzWprjf0Li6IbAzbiIHSyebrY2+TPhO+kBLgQ5UeGRJI9tnMfPu9Pm2N/sfhOULP58
PxVF5xmzTZOxrdm0juNECBeAHCd6hJU8XYTHVEeBfwNXxd3/FO18fpLHz2wErJvw2AzgbT8dToFp
jso+Zyr8/IC85Qag8Mj53oS1mA/cijyCTiB9Tq9ACBoO/AFo4U3oFJg3bQ6KZz2gYR4/8xObaz+N
e+wqm5Od4nKZ2ajZ2LSO4xQJF4AcJ1rshsqgJuNeYJg3UdGoQvl2/hb32KPAzsD4PH92F6BzwmOf
AZP9tDgFojkSG4ahnBzphJ9vgWNRXpD7iIVvOOEstrbaCIW3DE9zfG8bk761c+Nlsp1CMRn4IuGx
zjZX5ZPxNuc+FvfY3+w6qPLTUjSG2diVjG2AXb2JHCc6uADkONHi1JDHZ6MdX6c4NAEetEVWwNXA
kSQviZtrOlO7EtAHflqcAtAYlWUfZousNdMc/y1KYL+ZXTMu/GTPYuB+YBNry2/THL+mnZthSDhq
7E3oFID3Eu63o/ZGRT5YaHPv1XGP/cHGmyZ+WorGDWarJuM0bx7HiQ4uADlOdFgXeQAl4wHKszxy
qSyAHwaOsvsrUFLKCwr4HVonGa+/8FPj5JEm1ue/Bu4mO+HnSWCZN2G9WWZtuRmZC0H32jk7yhfD
Tp4ZkmRN0aZAn11tc/AfiVUkO8rmahdAi8Mos1WTsSuwjjeR40QDF4AcJzocDjRN8vgCVHbVKTyN
UbLLg+3+QluI3V7g79Eq4f5s4Gc/PU6e+vxRwFDgIVSyPBVf4MJPvkkUgtJV/lvfzt1Q5CnRyJvQ
yQM/Utvjo9BhiLehcuOBJ+7BNme7CFQc/kdyr+hmdp4cx4kALgA5TjRoDhwU8twzZmg5hV8Ix4s/
M4A9KU4VtsQkr7/jVZSc3NIQOADllspE+PkM2AcYhAs/hSJeCNrHzkEq1kceEZ8BB5LfBL1O5TEL
mJBmrioET9vcHCSldhGoePyQwkY6CAlBjuMUGReAHCcabA30C3nubm+egpMo/kwEdgHeL9L3Say2
MtUX3E6OaGTiwOdmuKcr5fwpEh+2Al4CVnoTFpzl1vZb2bn4NM3xm9gi+XM71+4R5OSCpTYXxVOs
jYn3bY6eaPddBCoe94Q8vq7Zuo7jFBkXgBwnGuwb8vgXwEfePAXnf8TEn1+AnVBejWKxJOH+LD9F
Tj1paGLAZyYObJLm+J+A/cyAd+EnGqy0c7G1nZuf0hwfCEHuEeTkitkJ95cW8bt8jSqE/Wr3D8bD
54vBR8BXWdq6juMUEBeAHKf4NAV2DHnucW+egnMrqqQDEn/2QG7NxWQmNaspLfTT5NSRhsgV/wsy
E36WoGo7mwIv4MJPFFlp52ZT4JoMFuGBEPSF9QUXgpy6Mj/u/8UUf3Pie2B3m7sBTrA53SnsePRo
yHM7kjzXpeM4BcQFIMcpPv2BtUMMqxe9eQrKFaiqCMTEn18j8L0m414/Tv0IPH6+AJ4ifagXKLRo
MKq2M8+bMPLMA85HeZk+zeD4ja0vfIF7BDn1Z5bNVcXmV5u7AxHojza3O4XjRWqKgwH9zOZ1HKeI
uADkOMVnm5DHPwZGe/MUjLOAC+3/KIk/AHOAcXH32/rpcjKkETU9fjIRfuYCZwPbokpSTmkx1M7d
2WQm3G1MTY8gzxHkZEq7uP/H2VwVBRJFoAuBM/10FYxRZsNmY/M6jlMgXABynOKzVcjjr3rTFIzD
gRvt/1+QC/mvEfuO34QY3Y6TjCa2mP+czD1+AF63Y29CyYad0mS5ncON7ZxmQuAR9Ln1HU+g66Sj
XcgcFQV+pWY42E021zuF4dUsbV7HcQqEC0COU1xaAgNDjPd3vXkKwvbEKq2NB/ZCu1dRIz4ZeHeK
U27XiT5NgaORF0g2ws90lC8jiuKnU3cCQfskO8eZEAhBQ4Fj8JwdTrj90i1kjooKo4C9gd/t/t02
5zv5512SbyIMdPvFcYqLC0COU1xWB3omefw74EdvnryzNioV2wKVs90X+Dmi3/UTFJoDEoA6++lz
EhZjp6BKOA8C62fx2hds0X+fN2PZco+d4xeyeM0GwAPWp06xPuY4AZ2BHvb/XJujoshPwD42x7cA
HgbW8tOXd340WzaRVYG+3jyOUzxcAHKc4rJ2yHX4ObDCmyevdEBV1rqZ8XoI0XNhj+c3YjH1LbJc
4DvlS1vgL8BwVPJ43RTHLqbmjux0VPFuP+tfTnnzm53r46npDbQcWJTideta3xqG8gq186Z0kEDY
3P7/OOJjyDfAoSgnVnfgCbMBnPyxwmzZRBriApzjFBUXgBynuKwX8vin3jR5525ggC1+jgU+KIHv
/GTc/x5HX9l0Ay5GZY+vR96EYcxCeWBGE0vw+yLyCLnfm7LiuN/O/YtxC7LRwBukrjbYF7gBGGl9
r5s3ZUUTPwc9XgLf933gOCRMDCAW+u3kjzBb1jewHKeIuADkOMUlmRvsSuBbb5q8chWwv/1/OvB8
iXzvF4Ax9v+uPoZXJGugZKYjgcvQbnYY84CbgUuBLsiTYxbwBxTu6F4/lctv1gdOBWajzYgu1qf+
Q+rqYd3tuJHWF9fw5qzI9cPO9v8Y4KUS+d7PorLwmA1wlZ/KvPKt2bSJ9PGmcZziDuCO4xSPZJPg
RJSM2MkPRwLn2/9XAHeV0HefTWzXciCZJ/h1Sp/NUO6K4aiccfs0/eQqYCNgkv0/EHjH3ucub07H
+B+wufWNAcCVwAQbW662vhRGe+uLw61vbubNWTFsTKyAxd1p+kkU+/yV9v/5ZhM4+WG82bSJrO5N
4zjFwwUgxykejdCOayJjS8yYKiUGAP+1/x8CLirB33AXsYomh/kpLWsaA3sCbwKf2UKleYrjJ1if
7gncgvK2XG1z/d+AXfAKX05tfrG+cS4KB7sGuB15AvW0PjUhxeubW9/8zPrqnngJ+XInmHt+pzQF
5QuRaInZBAP8lOaF2WbTJtLFxhrHcYqAC0COUzzaAq2TPD7KmyYvtENVjtqiaiWnlejvmIbCLgIj
fBU/tWVHB+ufXwEvo1CLVPP19yisYR3k1bYvqsCyI6rgNBi4juSu+I6D9Y1rra98Y33nR1Q96Qrr
W3+0vpbKptzZ+uxX1oc90W750RU4wv6/yeakUuRUlKOmrdkG7fzU5oXRSR5r4+3tOMXDBSDHKR6t
UDWnRH7xpskLN6BdvvHAMcCCEv4ttwFDUS6O4/zUlg39kFDzvZ3j/mmO/wA4EIVi3Ia8Ch+2Wxvg
RpSo9WtvWidDhiIR6EbrQ48gb8mG1scGAgdZ36tO8T797fjvrU/386YtG05AItBQO8elygLgaLMJ
BpiN4OSeZF6nzYGW3jSOUxxcAHKc4tECaJbk8UneNDnnZFT6eLkZr6NL/PcsAs6x/88BVvVTXLI0
BnZHCb6/Bf5K8tDQ+HP/mC3St0NJTZcioecLFIozAdgDlYdf5E3s1GF8+QsK5ZoAHAV8aX1sKfCM
9b2trC+m6mNdrE9/a318dzw8rJRZFTgrbu4p9fFlNHAisMxshJP9FOecZOGjLgA5ThFxAchxinv9
JbsGp3rT5JT+xHb2/ga8XSa/633gX0BnVJLZKS26xy2MX0WhNk1THD8Zhej0R+EXQXndKuA8lMR3
TeA5YBPgNW9ip568an3pOetb79gYWmXPf2p9sb/1zckp3qup9fFXiQmd3b2JS45LbM75p81B5cBb
KP8VZiv099OcU8JCBKu9aRyneAtQx3GKQ0sUspHILG+anNEMJTNthUIZbiqz33cZErROBvby0x15
GgLbAg+gEtrXodLsqRgOnI7KdJ9LTXf6Lsir4mqUw+VM4IA0C3HHyYbJ1qfOtD72b+tz8V5qv1rf
XA/lCRqR5j3Xtb4/0q6FbfGEsKXAPsBJwBtIAConbjIboZXZDM38dOeMZDZtE29jxykeLgA5TvRY
6E2QMy4CBgE/AGeU4e9bhnIAjUOlbbv5KY8k3WwBPQTtmh9D6gSYS1Ei3T1QueXbgZkJx2yPQr72
toX01qhqk+Pkg/9YH/ve+twX1gfjmYlywmxkffdl68thtLNr4X27Ns70MSyydEdVBcehkKkVZfgb
zzBbYRClWSE0qoTlW6zypnGc4uACkOMUj/koJ008K4DF3jQ5YQfgAiSSnJxkAV0uTAD2t8XUQ2hn
zSk+jVElpQdt0XwTSqCbiinA9Xbc3iiMa3mS4y5AYQu9UPWaLVHVJcfJJ19ZX7vP+t5b1hcTWW59
d28kBt1gfTsVA+0a+d6umR3xXEFRoYmdk7Y210wo098502yFZdavd/BTnxOWUlswXIrnp3OcouEC
kOMUj2pql2VegXsA5YI2wM1oh+kfqOx7OfONLba2B+70019UeqFwmKEoPO9o0pe7HQL8AZXaPofw
UtudgReBK9Gu6kkoqfk8b3anQMy1PneSLeCuBJ4HOoUcPxLl+1nH+viQNO/fzq6Zt+0aOteuKad4
3Glzy54215Qzn5jNUGU2RBs//fVmPuXpMeY4JYsLQI5TPBZS29unyq/LnHAJsD7ahb62Qn7zu8Bu
qGLPjd4FCkoLJMA9Ywvea4ANMlhIP4Tyn2wO3EXq/F+Dgc/tc4YD2wD3eNM7ReIeFBI2HNjX+uag
FMfPsj6+ufX5h+waSMUGdi2NtGtrb1Q9yCkcN9mcshvlk/Q5HdcCr5sNcYl3gXrTkNrhXosIDw1z
HCfP+ELTcYrHfGp7+zRCbtZO3dkGlTCeiXJKVFKlibeQ2/ofbLHlMfb5ZT3kATECeeYcQPrStt+j
SkrrovwnH2bQR/+IKjCtDtyLym8P8+Z3isy31hfvBfoiEfqPaV5TbX3+GLsG/ka4x1tAS7u2XrRr
7UrSJ0936kcVEvlOtjnlrQr67dUoH9BMsyW29u5QL9pSu+DJQlwAcpyi4QKQ4xSPOdQO3agieWUw
JzOaEvP4uQD4pQLb4ENgC1QV7DWgtXeLnNIRJd5+B4VDXICEmVTMA54AdkG5Tq4jszwaLW1xfSsK
Fz0FJWD1kC8nKsyzPnmqLZxvtT7bMoPXTrBrYaBdG09k0Lf72jX3DQoTO86uSSd3tEGVvvawueTD
CmyDX4jlt7oWz61XHxpRezNqrtnAjuMUAReAHKd4LCd5Ysz23jR15kxgM1SmuJJz4YwANjRDfjgw
wLtGvWgO7IpKVv+IkuDuQPoktd8B5yFPocPQLvrSDD9zbRRycTzwMwqb8fxOTlT5H7CdLZyPB94D
1srwtUvt2jjMrpXz7NpJRROUKPq+uGtyFxSO6dSdATZntLI5ZEQFt8WdZktsbraFUzfaJXlsCp4X
yHGKhgtAjlNcRid5zHcz68aaKHnjPJQ4tNKZilzXn0HVe87DQ8KyoREKb7nJFqOvo7CVTmleNxvl
N9kReTb8Gxif5WfvA3wMbIIS7A4iffJcxyk2X1hffQHYFCXU3TPL9xhv18xAu4YesmsqFZ2QJ9Ab
SLC4ya5d96bNnCqbI4YAT9vcMdWbhfNQuP5FZmM42ZPMph3rzeI4xcMFIMcpLskmwS7eLHXichTu
dAnymHC0w3YOSp76V5SzYwtvllCaIk+ba9Eu+Ido5zddiNdK4DOU/2QdJBS9S/IS7un4hy2gOwEX
o7LLM/zUOCXCdGA/4FLrwy/bIjpblts1dIxdU3+0a2xlmtetbtfsh3YN/xvlhWvqpyaULWxu+CsK
HT4H984I+Mn6cmuzMZzsSWbTjvZmcZzi4QKQ4xSXZG7uq3mzZM3uwKGoEs2t3hy1eA2FY3xpi6jn
UaiFo5DLvYA7ULWh920BtA7pPabGAdcjb4fBwG3A5Dp+j9YoB8rlSPDZxxccTglzmfXhGcDVwGPU
PR/ZZLu2tkIhvtfbtZeKKruG/wZ8YNf27Xate5i12BAl1v7M5oa1kBeVU5NbrH0ORdXQnOzoneSx
kd4sjlM8XABynOLyM7UrAK3hzZIVjWyxAUrauMybJCmzUUWXLZHI+B3yNBlUgf1lfeB0+/0/AS+h
BMt9M3j9LFvM7otEtHOAr6lftbm1bZF6CDAUiUkveZd1SpyXkGjzDcrv8wH1C6NZadfHOXbt7WvX
4qwMXtsXJap+ya75F2wMWI/KCxULwvS+BVa1OeFk0ofaVSpLgb/b/5ehsuZO5iTatCvsGnQcp4iG
sOM4xWMU8DvQM+6xPnZtLvfmyYjjkAfGvch7w0nN5yi3zMFoZ/4TWwjchjyDppXZ722IdiA3QYmb
t0I73dnMf7NROMqzKFltLnNj7I6SS3cGHkVC1Hzvpk6Z8CNKDv0/JAJ9gsK6Xq/n+y5A3isvAqsA
O6NS8TuQPOlsPJ2Rd9I+aMPgF5Rz612UL20s5RcC1RmF5p2OEj2PtfPxFOnD6hxVfbwXOAE41v53
0tMY6JXw2AQ8BMxxikrVm2++6a3gOMXlFVRuNWAecl2f4E2TlnbAMKADsAGeWDBbmgKHoxwd/YBF
tjB7Eolpk0vwN7VCHjUDUW6LzZDXQbMs32ca8BH5EX0CzgBuRN64FyBBznHKlQuAK01wOBv4Tx4+
I14M2hoJH9mwGAlCXyKx/BvkrVCKomxXJL4dgkKXmiNB7hrkObXEu2RW9EZJxmeg8DkvY56eVYEf
bF4OeAmJr45Tsuy8884l/f3dA8hxis8n1BSAWiO3dBeA0nMKCme6CBd/6sIS4H7gEVsgnIWSDu+P
xKBPkfjxKQoZmxWh794AVRfpjQTTAUgEXAfoRvYhzitt4fcuypn0CTAzT9+9MXAzcBryLjoOhWQ4
TjlzFfA98ni7GQm1Z5HbsN2pNp49gjYGBiMvux1RKEq6caGZjSMbACfauDDJFrEjkLfkDzbfzCBa
3jPtUXjrICSCDUKiDzau3YQEfg+TrhtjUYGAy1A44TXeJGlZl5riT2DzOo5TRNwDyHGKz2Dkfh7P
381YdsJZxQzyJWg3bpY3SU5YHzjCbvGu2zOQCDTM2v1nM4inol3zfNAIaIuqiHSz77Om3dZA4l9b
6p7PbgrKKfKu3UaifA/57rcP2wLtB5RYdIR3O6eC6I8SnvdDAvOR5D/0tAnaWNnBbhtT94qbK5H3
x2/Ar0g4/gmVsJ9o48pc8hfG3czGkd4onLW/3danZsntcSis9FGSF5xwsqeDzYGNkUg4zZskJYHX
XzyDUOJxxylZSt0DyAUgxyk+rcw4i19sv0ZNryCnNpejktmno+ouTu4XGYNQKMVuJE+QvAAJQxNs
0TPZ/s5Eni1z0W7zvITXVaPws5a2MGuLwvnao5CNjvZ3FVRKuo0Z3PWh2hZnw9AOZJD7qJBu/OsD
z9ii7VWUC8VLvDuVSGfkCbQ7Cks6iMJWBmoLbIQSIA9GmwjdSV/5Lx1LbbybjsTxaXaNT0ObFLNt
zFlq4+eSJJ/Z2sa7NjYudkBiVVf728PGyJZJPn8U8vJ5FnluLvaulnP+iKqNXphE3HBq8qpd4wFj
bR5c4E3jlDIuADmOkwvuR4kFAyaj3cqZ3jRJWQV5TwSx+Iu8SfJKUxRitQOwPcqv0ynC37farp1f
geEon8cw6zPFyuWxO/L86YDKCv8FT/TuVDaNgBuAP9tYfhT1Tw5dV1qh8NGBqKjABsjLsAP1F4Xy
yXSUp+g95MX4LZ7bJ980t3mlrdlp7gWUnA5I1O0a99gDKOTZcUoazwHkOE4ueJGaAlBXlLz2dW+a
pJxoxsX5uPhTCJYAX9jtKmv79dAO+ia2cOqNPHgaFPB7LUc76r+jncUfzeD8Ge2Ez6R+5dlzxalo
x7gBSvx8i3cpx2G5XQ+/ovw0LyOPzjuL8F3mA0PsdicSfTogz8e1bLzrZ+Pcqsgzp5A29ErkQTQW
CdlfAV/beOcbRYVlEfBv6ycn4eH6YWxGTfEnsHUdxyky7gHkONGgA8oD0j3usZtRgkynJu3M6F2E
dmldACo+DZD408tuvVF+nu7IU6g9CmtojUK+mqZ4r2oUHrEUhZDNQyETs5Bn3CSUa2MCCumaZMdF
tWzz1ajK2mzk4fCKdxfHqcXewEPIq+JqlDskijREoVndbHzrAfS0+11trGtrY10bG++akNqLaAmx
0LF5NtZNt/HtNyT6jLPbLLxsexRoYTZbMyQOzvYmqcV/kHdfwESz2VywdEoe9wByHCcXzESx0ifF
Pba7GcEucNTkcDO8z/K2iQwrUQjHDLQrnUgDJPo0JpbbIoxqYglUAyGoFBc8zYG7UTLtX4CDURia
4zi1eQmVLH8aeXauhjw9o5bDZgUSYWahimaEjHeB8NPIxrtUAlCQK20ZEoNc4Ik+C5En543AYcAd
3iS15r9dEx57BRd/HCcSuAeQ40SH7VEMfzw7oNh+RzREOQ66Ind8T6DrRJGuqMrRNsBHqNLXJG8W
x0lLD7t2BgMf2OJ6sjeLE0E6obDjCSh3lAt3qe3Z7YH3vWmccqDUPYAa+Cl0nMjwPkpWG8/+3iw1
2A1VkLgTF3+caNLPDN9tgMetz7r44ziZMQHYBYlA29q11M+bxYkg04G7gP42zjsxDky4/yUSdB3H
iQAuADlOdKgG7kt4bF+UT8ARf7C/D3hTOBFkazNy1wGuQ+FfC71ZHCcrFqJQ3+vsWnrPri3HiRqB
LXKKN8X/0xbl9IrnPqJRkMFxHFwAcpyo8RRK+hiwGrCHNwugXeB9UI6In705nIixP6ratwrKT/U3
N3gdp85U2zV0NgqpfB33iHWix4/AM2abrOXNAcCeZrsGjAOe9GZxnOjgApDjRIsZ1PYCOs6bBYCD
7O/d3hROxDgNeBblqDoEVfBzHKf+3ITyADWya+xUbxInYtxjfw/xpkhqs96LJ392nEjhApDjRI+7
gKlx93cCNqnwNmkMHA2MxpNiO9HiUuA2lA9iV+TF5zhO7njCrq3pwO3Axd4kToR4x2yTo81WqWQ2
AXaMuz+VmEDmOE5EcAHIcaLHBJTkOP46Pb7C22Qr5F79KCoL7jhR4BbgEjP+t8eTXDpOvngfVcUc
A1xm157jRIGlwGNmo2xV4W1xYsLa8g6zaR3HiRAuADlOdBeWv8fdPwLoU8Htcaj9fdq7hhMBmiIx
8k/AUGA74DtvFsfJKyOQ0Pq1XXuP2rXoOMUmsE0qOQysDwrXDPgNuNW7huNEDxeAHCeaTAWuj7vf
jlgFrEqjFbAfMAQY5l3DKTJtgedRlaK3UMnq8d4sjlMQxtk1945dg88DbbxZnCLzLdoM2N9slkrk
D2arBtwATPOu4TjRwwUgx4kud6Idz4ATqVlZoVLYEuiC51Zxik8X4DVgN1TVZF88uaXjFJoZqOrS
U3Ytvoaq7zlOMXnS5ogtK/C3r2Y2asBwaqYycBwnQjTyJnCcyLIQ5Rd51u53Bk4Hzq+wdtjL/r7h
XcIpsoH7KrCeGbanAyu8WfJOY7Sj3tJube3WGnl+NLX7AB3MrqnO4H1nxr0mHVXA8rjXzAGWAHOB
eXZ/DrDAbvOBZX7q8j4/Hg7MBk5GHkF7orATxykGbwDXmM3yVoX99tPNRg24FFjkXcJxookLQI4T
bZ4DngEOtPsnAf9DiTArgYbAHii/ygjvDk6RWAd4BeU4uIbKE2HzRRMkwHQEVgV6At2A7va3Cwop
aG23VkTXc3klEn7m2W02MAWYhJKgTkR53SYgD5aZeEL7+rIChZ3MAs5FFSL3BH70pnGKwHDge+SV
1sDGhEqgLxJhA54x29VxnIjiApDjRJ8LUVnNdrZQ+hvabakE1gPWAK4ls119x8k1mwAvmChxIXCl
N0lWNLBxq4ddy33jbqvac+0p/ZD0BsgjKV0+mpVIsJiBBKFRcbdfkUA03ce7rDgPeWD9C3gbhWYO
9WZxCkw12ij4m9kulbJp9TdinpSzbZ50HCfCuADkONHnJ+AK4Dq7fxJwP/BlBfz2be3vm94NnCIw
CCWZ7QycgZeeTkdb5MWzDrCuLYLWRuJZRxRKVekEglhHVDZ6h4QF5HTkLfQzMBJ5FPyAQpvmevOF
ciXywLoZ5QTaB/jcm8UpMG8hQWQ7KkMA2gw4Ie7+FWazOo4TYarefNPXVY5TAjREIkiwWHgd2L0C
fveL9ptXw5PtOoVlZ+TG3hIlt7zXm6QGbZAXzwC79QfWRGFbUdlcWoFEkyBXz2y7zbG/IM/Ktva3
HbHcQm1s3I0Cy4HJyEtoGKo49K3dd1GoJicCd9v5PpDKy8XiFJcOqCrkG8ABFfB7g6IIAO+iCn2e
G88pfwNx551L+vu7B5DjlAYrgLOBj2xhshtwNPBQGf/m5sAWqPy7iz9OIdkN5TFoChxk/1cyjYBe
wIbA5sDGyMOnC8UP3ZqPPGbGodxoY5G3zEQkmsxBIslC0ufkaAC0sDG2LdAVeS+tBvRGOaB62WOt
Ctz+q9ot8IpciXIMfY/Cnb5A4tA4JBhVKvfYOX8cee/tj3uQOoVjJvLOHmw2TDknQj6amPgz12xU
F38cp0SMOsdxSoPhwD+A/9j9K8ywnVKmv3ctFHrznp96p4AcBDwBLEYeBC9UYBs0Q2Fcm9lCZmNg
dXs8W5agZMctqL9HzRSU4HeEjYc/IsFnqn1OfQkSOQei0g9JjmmKSo73Bvohz6f+9n99S5GvQEJV
E/ucVDRAibK7oRxxWJ8djQShT2wh+oM9Xkk8DRwMPIpyshxujzlOIfgAhYCthUTZcqQryrkV8A8b
kx3HKQFcAHKc0uIWMywOQLvSVyKX93JkoP0d4qfdKRAHAU+iUt77odLSlUAzYH1gK2BrYCOUy6cu
gs1k5JXyNdoVXtPet0+W77MChTh9CXxmf0ehBMrFZAkK8RiPPDID2qOQuM2ALe1v3yzbsCHKAfQx
8AsKRwu8rbpmeB7XtdvR1oa/2bn42L7vSCpDEHoOleN+3q7pQ3ARyCkMQX7GAZSvAPQvmyOw68rz
4zlOCeE5gByn9Ohmxvzqdv9A4Nky/J23AachoWu8n3YnzxwKPIY8MPalvMWfRmh3eiuUY2szFNqU
TThXNRJ7fgS+sYXOZ8gjp4cJEIeZGJEpk5Dnylv29xdKt1R6EyR+DUb5pAbb2J0pI1EY0yOoWlhv
JCwNQOL42kgUyiaxdiAIfYnydXxkbVzOIWM7Ii++FrgI5BSGnnad/Rf4Uxn+vgOIhUWPtnlkkp92
p5Io9RxALgA5TmmyA/AqChP4zRYGE8vsN35iC5y1qeycFk7+ORm4E5hG+VYP6mrjxM7Iy2dtoHEW
r5+PqrsMNQHhazP+58Qd0x84B3lSNc/wfX9GSe1ftvedU6Z9rC0S2vZCeTPWyvB1i5BocR01Qyza
ok2Ajex9N7Zzmk1uomV2Tj9CottnSNQrNwYjT6BOeEJ3J/80tutqgo215UQPGyd6Im/I3fEwfacC
cQHIcZxicQYqeQvajTmojH5bc7Tr/SWVUe3MKR6B+DMd2IPyCTlsggSZHVFllo1NNMiUqSjXzsco
wfAIJDInS6S8FfB3JGxk4pEyFlX4exL4itzk7yklmgKbII+UfZH3VTqqkVB2pZ2TRBqg5NQboETd
W9n/2eQlmoMEvjeRB9xwStcDK5FN0aZJJ+APwF0+9Dl55DUkzK5KeSWCfhp5nQOcSSwnpeNUFC4A
OY5TTO4ATrH//wLcWCa/a03kGXANcL6fZidPxIs/u5sYUcp0RN4OuwPb23WUaVjXVBTK9SHyvhtG
rFR6GNkIP/NtUfQA2jFe6N0PgJYor9uxdt7SefCkE4LiaYcqtw0GtkGhY5kKQitReNh7SDj5FJhR
4m29ifVBF4GcfHMNcC6wBspdVg6cDdxg//8PONVPs1OpuADkOE4xaQa8BOyEdtF3sQVcqbMNqqTh
7vpOvigX8aePXf97IEGmU4avm4MEn/ftWvuW9IJPwPrAZShRdjqB6RfgPlSRaZx3u5T0Ao4Ajkfi
XSpWokTHF6Ok25nQDuUQ2haJTgPJ3CssSE79CsofNLpE29hFIKcQnGR9axtqJosvZZvsTeS9+Daw
N5VXXdBx/h8XgBzHKTZdbUJez4zybVH4VClzDPIU2AXlpnCcXFLK4k8Du9Z3R/lkNiGzfDvLUEnw
D2y8+JLs8710Ai5EO7/pSsJ/jMIDXvKFQtY0swXWGUjUS8Ui4HbkEZSth05XFKays80b/cgsL9Qi
u2ZeRmLKSJKHBkaVeBHoeOB+73JOjtkFeAMlw3+4xH/LqmhjsQ/wHdpwmOKn2KlkSl0A8jLwjlP6
TEa5JN5ESUHvtYVhKeduCMqLTvDT6+SYQyg98acx8tbYB3n6bJDh/D0dhe68iUJ5fqbuCdWPAa5C
eWZS8SpKWPw+Cldysmcx8BTKt7E9SqwdlgutOQr/PQSF4z2U5dzxot2CynDb2+J1EOHeZM1Rctut
UTno4UhQeQF5lUU9af9X1p6vIe+0BdbejpMrgqIcvUr8dzS1a6SP2WOH4OKP45Q87gHkOOXDYLQj
284WuKeU8G/5L3A62nlyEcjJFQejxMPTUd6aoRH+ro1R4tr9kaC7FunDrVai6jPvICHmC2BmPb/H
GsAt1l6peA+4hPIId4giW6Owu+3THPc68Gfg13p+XgdgCySU7IgqjGXa/15B4WlDkOdZVNkYeWl0
tIWti0BOruiBPLFvA/5Ywr/jTuQxO9vmoU/81DqOewA5jhMdPkH5I55FuQ3Goh37UqQzMBftzDpO
LjgYeAKFyURV/GmMQnIC0WftDF6zyBbaryJPn+9yuOg+Fbia1HliPgMuQKFlAVWoClljFM7Uwh5v
QyxZdJVd30EFsHnIc2RpxEWDup7XJmZztbbHmqIE0IGXVLWNeaAE2YutHZbacx8BO6BQrauALUM+
azfk4XI+KhJQV2Zan3rVvv/6wK7IAy0s7LABsI7dzkFi0MtIDPoygud1qP2mN2xsABeBnNyw0Ma0
TiX8Gy5A4s9i4HBc/HGcssE9gByn/NgTiUBNKN38Bm+hPCdr4NWCnPqzO7FcNLtGzJBtQk1Pn0xE
n5kmCLyM8vmMzfF36mbiwT4pjlkGPAY8g4Sd9VDoZmfkPdLGboEQFIgeVQnvscIEjqVIDJprt1l2
m4K8ACfabRIKXZpH8fPONECCTldrs+526wF0AdrbLWiLptYeVUBDaubbqSYmhgXCT9AWM4FpwHiU
b2cuKsV8OKlz9jwPnEb2uZ7S0RvlAdkbeSa1z+A18WLQEKIVorwNErqa2G96w4dMp560QNW/vkM5
tkqN41Do1xLgALs+HMcxPAm04zhR5DBUdafaFgrPl9j3/8IWFf0oreSiTvTYBeU4WW6Lu/ci8J0a
ovCTfc247pfBayYhsedFlF9ner7sGjP8e6Q5bhny4GlX4LZbisIRJqKqYr8AP5rAMAqVs1+Rh/O1
CtAXCXT9UJWuXkjwaWfiQSGZjTyI0iVtHo82At7J0/fojCqK7Y1EoW4ZvOZ7m5NeQF44KyJwTW6P
ROJGSPh049ipDw1sXJpBuLdeVNkPCfvVZks+7afTcRIMJReAHMeJKPugfCdVyCvo7RL67iNsYbMO
nkjWqTuDkDdZIxS68k4Rv0sV8pI5AHn7bEhNb5iwxftrtlj+FJVuzyd/Ba5BgkcpMg8Yg6qdDQWG
oR34SVmMI1VIxFjfztHGNg71IRa+VWqsAM4Drs/z57S1a24/5HXXM83x1cC31r+ftvNWzPF+R+Tp
sAwJoZ/5EOrUY7z/wfrSBiX0vXdCObyqcc8fxwnFBSDHcaLMHmgnZxnaof2gRL73cLSj7gKQU1c2
R+JJG+AgiucFtzry9DkYhXqly703yr73SyhUrRB5sJqhZKXHl2E/mIeqnw1FYXNDUYLkIB9NYxRq
ujEKZ9oYJdxuXYZtcR9Krr+4AJ/VEhUm2BuJQX3THL8c5Ql6GnkGjS5SG+1n32Euyqf0pQ+lTl3W
V0gAWgr0L5HvvK3NO42R57iLP44TggtAjuNEna3iJvU9KQ0RaLh933VxAcjJnvVRHo/uSHgptAt7
FyS+Hm7XX/M0x49DoV3PAp8XaIEesBpwL/J+qA/VSKyahXLOTEVhatOJ5fNZYn9Xhry+pd1aIW+S
diifUAdUqSnIL9SK9KFPYSxDYWMf2v1tUDhXfd5vPrE8PTPs/5koTGuOPb/Absm8vhqgkNemxPIG
dbLbKijHUHtrm6p6nqe3gROB3wrYx5qhimIHIs/U1dIcvwj4GOWYepXCl50+BCWFnoBEoO98SHWy
XV+hUMdllIYAtC3y/Flq1+jHfgodJxwXgBzHKQX6owScHW1yfyfi33cY8txYm2glC3Wiz+rWv3sD
JyCvh0LQGoWNHGZ/26U5PgjvetaM7QVFGBPOsMVutt4ui0xAGIl2uUei0KuJSARZTH6E2yZIGFrF
bj2Rt05fO999bIzLdYXT5UjYGYMSbo9CXkXjkdA1FQk9+RirqpCA0hkJmn1QKOE69nc10guMicxD
4cH/QWJ7IWmJRNEDyCxMbDbKx/M4Eq/mFeh7Ho+E0dEoLGaMD61OlmPVT9Zfoy4A7Yy87qahQgQj
/PQ5TpqLxgUgx3FKhJ4oDGYjlIPk+Qh/169REto1CmjwO6VPN1skrgucaQvcfBv5g4FDzXBOlzh5
CvA6Cst8D3mGFJr+wEUo1CVToWQhEnk+RZ4z3yLxI2plvVsiYagP8sA6HnkM1YUZSDx8zRb/Uym8
SJeOxjauD0CeTFuifCOZCkLLbR64nMILQSBPru2RZ9BuyHMuFRPQRsZjKD9PvjcHzgRusr6/E7mv
puaUL22QUDwO2CTC33M/VJlvqNmF4/3UOU56XAByHKeUaA08hHKSnAzcHdHv+bYtaFZzo9vJkHZI
XNkcuBC4Ml/zJgoxOwTlFkpXwWs28kh6wvr1rCK1TzfgEiSKZFKxaiYKF33R/paCB0RjYDPk5bgr
EgLrGtq1FHk3vWFt8CXRE7ySsbqNnfuisI72Gf7W+4DLUMLsYtAeiSyHonDEdmmO/wEJqU+iEK18
hQr/HfgXqky5m13PjpOOrkhM+cD6dRQ5CbgLef8cjW+2OU7GuADkOE7JXfeo0s/fgEvN6I8az6Gd
qbVRqIXjpKKZLdJ3Bq6yRVuuWc365GEmMqSqlLUIJRx+AuUwKaaI2QDlfPkXCiNKx3vAA8jzZWoJ
nPsGyAPmMBRWlC7Z8AIUYhqEwe6Iqn21TPO6UShc73HkAbWyBNpmFRRmdSzytEnHNCSe3lPk39cV
eXAdihJzp/JoWoHyZj1uC9l8eDBcBZyPQtH2QbmsHCcV/ZBI+SzycIsal5j9dy2qEOi5Fh0nC1wA
chynVDnGFnoPop2gKO1u/w/4gy3o3/ZT5aQRAB5HyZ5vR1WOckVbtOt/JLBDGpFgBXKjfxIJmKMj
0Da97FraNcPjc+E51QSF9jRDHoctUThEIJi1sWPmoVxBDVAo3CKUR2cRCjlbkEaE6IoEn6NRdbVU
glywE/86EuYSEyCvZkLD7shrZtU053kI8qR8htQJihvY729hIkZb+9vKflvQRktR1ang/efa7w/a
aD71D3cKPFky4Q3gFBS+UmxWR6Eph6AKbanO83wk7D1qv2FODr/HHdYmTwBHUBoCoFM8dgLeAu60
fhMVmiCB9ygkDD/op8pxsqfUBaBGfgodp2J5ECX7ewklod2HwldbCSPwPOjtp8lJw3+R+PM48Occ
vF9jYBCq4LWvCQ2p+NmEgKeBb4jOTup+SPxZJcPjsxF/2qJ8R6vbrTfKRdMdhfK0Q0JPMxNBMqlc
VY1y0iywhfssFIY2wYSIcah8O0i83p/wMKFqOy+vI6+QL0mdv+c34BG7tUJhhHsj8W+thO/fEFW0
2gIJKs/FLaLWQKJbL2ufDtYebZEQ1CiLtliJxJ+5KOxoFkqyPR4loh5ttwkZCh3Buc1EBNrV2uwU
ip8rbjRwPXADMBCFXR5o5yWRVnbN7otC2V4gli+ovhscf7TzeJidi9N86HVSENguUQph74I8ZXsg
MfVrP02OU5m4AOQ4lc03KJ/Jw0gM2teM5WIT7Dyv76fIScEVwKkoNOME5D1RV9ZGXgaHZNDvphJL
RvsR0QsJuRi592daMvzvKMwlGW1ssT0AJTNdD4k+XZG4kyuqkPjWzm696vAe3yEh8EWUuLcuXhqB
F8k79vvWs3HxMPs/nnYop9LxOT5/VUhoamm3biHHrbQF5mj7vV/ZmP4LMY+ieK5E4lImQt8qKHzl
UuCfEejT1bZg/RqFLW+NRNq9SC5ydrOx4dS4fvEUdQ8pXmFjTEd7z2l2nTlOMoI5JCpJlbckJobv
iueycpyKxgUgx3FmmxF9NnKb/wf5r56UjkAAGuCnxwnhDOS1MsQW54vq8B4dre8fhRLnpkqOvAh4
F4k+ryHvlKjRGIUcHJfFay6jpvjTmlhVqUH2f/cSGssW2m9oS/0Tbre191oQ0QVTAzs33VFp9VOQ
UDIJ5Sn6EPgE5TwKErxehcLQLsrg/ausf/RBIblRCRNegkKD30ZeVrujMM3tSJ4vaH0kFl9sbfIw
EnBnZPm5i1Beojet/aYBt/hQ7CRhoP2NQijwGdb/LwFu9FPjOI7nAHIcJ56NUG6LkSgv0NwifY9+
9h1m2f/T/dQ4cRyBwnV+Qbl5fs/itY1tsXwUCnvslOLYauRR8TgK8xod4TZpivKT7JvFa64FzrUF
/s5oZ3gL8iP4rLCF+1ITFhYiD5ZqW7Q3sseak74CVCbMRDmZ3kTePN9lIGA0NrFgR2AXFCbRIQff
ZbaJBy1QmNsia4MG9lg1Eh+bkjrHTV2ZCHyKcpK8hSq6/RsVAsiUF0z8iHIC5NVReNhhtgBP5QE3
HXmKPYxCoLMRt1ZFYvCaSHh61IdkJ47OwE/Ie3I9+78YtEGVXtdFYbMe8uU4OcKTQDuOU240B65D
HgCnoxCXQtPCjKZVbSH2VkTaphHyCmhnRl5X5EUS5D1pac8nHW9Rro75SNiajnIuTUQhRTMojTLT
RZ93UWWtGbZQH5nh69ZC4V1HAOukOXY8Cn95DIXVrIh4mzRD4s8+WbzmF1sc7IFEn6b1+PwFyBti
AsqnMwGFJk2xvj0biTuL7VZlr1lh4ke1XVvbIq+uTUI+Z5S9V3e7/jL1Yl4J/IhyAr2ESnovihvv
4nP+9CPz0Lbl9rsn2vUfVoFsCMq982FcX4oP86q2c9jMxr52KKxpFfutPVCOpR72u1vW41wtQWG+
ryGRf80sXvsiEoEWR/x6aIgSgx+OckX1THP8SCTyPknmIWLrIRGog11Db+Gko7HNl0G/7oIE+PYo
f1NbwnOozSHmiTfLxv/Jdv3NtueXR2iOetPmkXVInX8sX2wN3Aa8j4Texd79HCeHF7kLQI7jlCm7
Aleb0X9FEcSJt1AljWtQCd5C0tIM1DVRbpi+dutpBmwHUocLZcssM2bHIk+Fb4DhtuB1wy3GAFSm
vJkZ2R+nOb69Lc6OMXEhlcgx3/rcw2a8zy+RNmmAhKpDsnzdCjJP0BxQbaLOjyhn2EhU6vg3e3xR
HX/DOnaNH2LnNlG8+QBVYXoZCUntUY6X3rYQ3wDojzyZ2mTweb/aYr8BSiDeN4PXzEUeYCPifvtY
FG41y4SbvVFy4K2pLSItts+8krp7BDS3RfNq1mbBb+9nj2d7LleSvcfRkyaslEoVrFZoE+EoGzNa
pTh2iS2YH0Iic7oQwq1trFgEbI9C7RzRzK6r/sgba327Xrva9ZsrliJvvxkmuIyy209I5J5YBAHm
alRa/U0yr8CYKxqj8MS9bUx9w7ui4+QeF4Acxyln2gKX2wLjAhRSUSiuQeEpI1BoWr5291rYwnEA
Cvnob4ZrDzOmisVyW2AOQbkuPjDDtlJZzRZnfVD57+dCjmuIEl4ejcKhuqR4z5XWpx9FHj+/lWC7
XA/8Jc998Asktn1li6pclddeEyWgPpza4twCVFntNpS4NB0NkCi0DvLoGWzjRpc6frcpKGTiE/v9
3yOxJ5Mqb5ujqlEH2vgSz2Ik2F1JrKpZLsbptW38Gmyf35v85Xm8HjinRMeQA5AX4Mak9vSagiqg
PWTnP2z+2d/GjjEoB1EpjiG5oi8S2ndCHlj57IOZsAx5I45CGypDUW6sMUhIzgeNbNzYgMJvXm1i
48oPKN/VHBzHyQsuADmOUwlsZ4bMJyhvSCG8UvaLW+QPRjksckFrM862QDu4G9rCoK65N1Yib5H5
xMJcFqKkq1X2/BzkmdDI7gfVfYKQjxZoZzqV4LTEFiIPomo2cyuo/7VBO5lboLDE25Mc05tYiNeG
ad5vAsrp84iJGitLtF1ORkmfEwlyZnWqw3tOs2vtbRSy9BO5z/vSFe2Qn0ztcKZpwH3I42dMDhZj
waJo+wxf8y4KQ/uK+ovOqyOPoOOSnIsFwF3IW2BKjtu3KRKEtkFeL4Pq2Bem2xjWMclzf7DvX4o0
MIHiSCQI9Uhz/DAbK54O6ZOnIaHyc+TxUWlj88HIy3JzUntZLoubJ4O5coHdGtj1NhcJmg2Q2Nra
5sdgrmxtY0ZdKxCuQCLdMBTe/jkSh3Ll8TnI7CSQOPh8Ac5BMxTmNRglef/ATVbHyS8uADmOUyk0
RdUkBgO32gIxn6xmhllbVGnljDq+T0NbDG2HcsZsnoHBn8hc5Er+GwoD+c1uE23BOhcJPkGOk+os
2rQ5MUEoCG1ZDZXBXhNYA4WexYcuTAL+a+eh3Hf5GtrCaz/kjRZferk1Cu841s5tixTvs9D67ENI
TJpX4u2yMQqHa53kuRnWFs0zfK9x1jYvIU+fGXn6zs2RZ8y5KJdNPBNtEX13jgSRxij87xQTQjLN
mzPfFob/Q2FAuQh97WKCyanUTrA9DXkK3EbdQ+jS0ckWpvsg74xeGb5ukV03yQSgeUhUG1ri11Fr
E22ORQnl040hb9kY8mbCGPJPFHrzPHAQ0c8bVl/aAn8C/kxNL7v5KBTrV+QtOAYl6Q9CJQPBZxGZ
C8tVxHJktbFz1tmupdXstrr97U5mYaAB1WhD4EuULP59JHrX9fz9x9pkDtpoyncZ+J3MNvrIPnsJ
juO4AOQCkOPUa+EZLKIC4yOoVtPWFhjNbWG+0oyOZnGL/2D3qnmCILASJS1cGWfcLCdWZnipGQ8r
7LGgakwgLgS7Z8UyMNdAISfLUEnRsXn8rLdtYT8R5RGYmsVCcxNU4ntnlH8gk3CuavuMH9EO4QgU
+hHkOClGkubGJgoFXkvbo93rJvadzkVeQeXKrSYa3I68f7DffxTavV81zevT7d6XIk1tobJFPd5j
igkcT9jiYWGev/M+yNtlnSTCz3+QJ9OsHHxOc+QFdjbKkZPIEuTZFFRuOhJ5AibzXBhpY9yjORJn
2psQdEYSIegH5GX5Yp7PQwv7vYcigaxLPd7rcySsl8uisw/yZjkShQKn4ndiXoRD7LHbkDfQrSYC
lCvHoipyq5i9MgR5zn2GcthNLuJcGeTIWtfmzA1RCPsqZJYja5n9hreQIP4VmXs8r4Ly93VHYtJO
efytvYGzzA64gdyFk9bVTm6RxE5uZ8+3JZYzsZ09F9jEDeyxeI+uKhtv58Wdsyo7D3Pt2Pl2zDJi
CcDj7eRFRbaTHReAXABynIgQVJloa4Z4J7t1JpbYt73d2hJzPW4eN6kVg2qb3OIntmBynE2sIkZw
C6pLTY97fg65D3PZGbl9DwXuIT8eFX9B+SZALs7XpTi2CdrlPgjYHe0IpmMlErC+RG7bQ1AVmFkR
78u90K71Ccij6V37f1yZXbPnmWjwgi2a97bFRyb5O15EwthnZWgE/s0WYNmyxPrKQ6gK1OwCfNd+
9l33Tnh8OnCzLZZz8T0C4ecc+8xEfrbF+uPUrvS0FspDdIT9n8iPNvbkSghqZ/35z9QOzXrJzm8h
Ske3s7HyGCQs16US3LkoLLicCPKIHYOEy0zyiD1gY85/kLfi+cizq5zoBdyLPKU+R6Gab5TAvNPe
rutNkQfzZiagpAsjq0abBq+hDYRPkeAVxjlx18JfTZjJNa2BE20OfIDce2I3iLOR29n4FFRr6xh3
C54PNjnjN0obkV1C+lyyKEEAmmf27yy7BQnDp9kcND3ORg6qtDqOC0COUyK0iRN0VrVbD7QTE9w6
2MSWqZFbbZP9UmIli+fHPb7IJrmgvPfiNJNetU2YTez/KhOlquyxZsR2U4L79WWBfbe5xEqOT0C7
l78h9+SpNhnWZWHTxBZNOwKvoB3RXO78rY121JqbIbZRwmKxCu3yHYa8QdbKwPCYasbrW8jz4Wfy
F3pRCAaY4TnYDMN3y+SaPtwW3CtQToMNqB02FM9SO+4hW0TPLtOxbjVbcGaT0+V34H5U4eynAn3P
5rYIuoCaYTXzUXjVteQm1KsB8ma5OInws9yu8//ZQjXdTn4zJKyeggTuxAS2PwCXoRxcuRDUu5jY
cwo1QzwXIOHz+gKOTWsjr7rjSO9VF890W4yWa/Lj9ki8PBqFEqaq/jgNeRxub3P5ESjhdzmwIwrP
/AQJWyNK+Lc0N1tha7vOt0DeO+nswZ9Rwu/H7ffHe3a3Q8mf+9g1OzDHY21jlFR+T+Rd9GgaMSrV
b+9sY8+qNp/E28tdzJ5uS+Zhs6lYbN8z8MYJ7lfH2dNV9tjsDGzoZvbdqu3W3K7JwJ5uaN+7Sdzj
mbDEbOWZZiNPSrCXf48TjCopx5fjApDjFJWGNjF1QbtQayAPj95262aTQlXIpDHfBvY5SP2faveD
HYHZxPK4zLXbYpsUAu+blcTCJFbkaAHQKE5IaWq/s5lN9k3jJuFgQm5rQlZw6xQnbLU1IyTbXdx5
cZPeGOQR8wtyKQ5i99MZGh2QB8q6KHHzK+TO4+gNlOcFVDHoKvu8A5A3yBakrzQyCuVseBl5hMwq
w2tkb1tsHIk8ZkqZPc3AzSSXw8/2ux9HnhrlTGsUCndkFq+ZjDzjChn+tg3yhIhPxr3SztGlNr7k
gm1tPNgy4fH51if+S91Lc2+IQg8Pp3bp8M9sLHo/R79jLeASJGTHeyUMQ55CHxbw3K2OPB2yCQ17
BOU3Kvfd8352jg4nuadYInNNBHqlxH/3vkg8PqwMfksy2tsYspfZGn3THL8cbSI9YILQTBsP/hVn
s+yWo+/WwObD/VE4+r32ealoajZxD7OV1zRhqrc91oHkuePSCSSziXnKzDQxZGbcLXhuLrHNxyVo
U3Cx2c1L4uzJ5Tlqn6CARwu7H3ghNSWWNyrIHdXG7OTA478DEv86xtnXrVKsJeaYTTzW5tRRKC/k
OHt8Gh525gKQC0COU6cFzqpmhK5tBtdaNiF3TbLQX2ID7hTkzRIk5p1iC59A6AmqNC0u03ZrZpNW
K5vAutotMAJ62v+dbLLLNJxtvrXlKLSb9YPdxlg7J3r7dEc5LnrZYu/tHEyGh6A8JcFi9iUTO7qm
ed1oM1afswXb4gq4fh4zI31n8p+kO9cE5d0PQ55eqdzzZ9m5fdAWx+We+LIZ8u46n+y8M0C5cG4q
0Pdsi5Lhnp4wVn+Cwvk+yaFI8S/k+RNvqM9GuYRuJXfJV3uiZLd/sIVD/GLgCVv45Upc2wqFy22Z
sNi8FQlnhUr4fhbKfZQN45FnyD0VMNY2ReLjMSiPUvsUx65EniGPozCiUgvT3Ql50T2GxKxyp5ld
f/ub8JIulDyZTXKwnev60NBEpINtfLkbbdLF09jsu95o8209E3tWRwJuqww/ayGxkKhJdi0HXjCT
zQacUYF29Cp2TrsQSzje0+53pvZm67IEezmwmceizdR5OC4AuQDkVDgtbDBdg1iivnVtEdgqwXgK
xJ1RyCtlrN3/ndiuw2Jv0vTXu7VtOxODetmtDxLYepox0TZDYWiiTW7foTCtH+0cLbYJ8iR77xdQ
DH1dd3s6op3wTCp3zTJj7FETBhZV2Dk+wRZgvyPPqAkR/77tUA6SI1FeiVTC5Aok5D2MKu1MqZBr
9iATVfqlOXY5tQXy71COqIUF+K7boFCrfgmLo4tQrpBc7Io2NXHi79T0DguEn//ksc/3QN44iULQ
HFRm/mZyI0Q2BI5HFe/iRe4fkJdNIbyBWqC8aOtl0McS+QF5Mz1N5tUQS5kuKOfPUSYcNEyz0H4P
eUwVKgdXffv850h0Ph6FkVYSzZHQdwTyDmqfwWt+N3t2Zh0/s7EJP/uarXuXzXXNzF7uh8LL1keb
pKuSWajWHBsbA1t6DBIjA8+VOSZQVONkIhQF3kKrmu3c285P3ziRqEGCzTwGeXEFBUZ+RZvWC71J
XQByAcgpRzrHCT0b2+S4ThIDfrSJCN/bwDjaRIYZLvAUhMYmtvSwSawfEuXWRGJdupwjy2zB970t
1D9GoW272ET5AvLayHSR1Bzt8P/ZDJ6wWO5q4AszTp8j80ph5cjBwJP2/z1IhIsaTZGnw+FmVKcL
NRmDcq48Qu28C+XMFii8abs0xy1EoU69kLdcPEdZu+X7fF6CctkE4sBKE4MuzeH1uBVwC8p5Ff/b
70ZeM4USO1dFiY9PoqZg+a2NVR/n6HNWQcLfyXELieUod9Jl5N/r7RgU4hLPk7Zg/COpy6WDcpH9
3cbmSqAKVQ47Eom2fdIcPwWFJD9mfSaKXox3I89DbGx5qoLn1lWQV9BxSFRPZY98bWPVk2S+CdXU
5sN9kHj0JgqV2goJi+siQThdNdPpJir8YrZYsDE3wWzpZTj5ppnZ0t2RN9Yadv762f12ccfORaL5
MFRx7jtb/0zzZow2LgA5Tk06oV2JjWyS3MgEhEZxQs+vNtgNs8lptBlDnn2/pjHQzm5BzHJQhSGo
VBbkBWqHdh0/NKNjRo6/SwP7vN42gW1ghu7aJgyl2hGeZ+d4hb1mPHCHGb1hHhwtUG6fv9jEGcYc
tMt8HxKcVnq34TRUihgzPDciGnlxqpCIdwgK81ozzfFzUR6FB9COeSXtkHVD3h/Hkt7b4gkTIyYD
w+2aDPgKiUj5zEewLhIa48vR5zpvTUsTQs6i5o7qU8i76Kcinad+9r0OjntsJQq3uxjlwMgFyfIp
fW4L8+/z+Psa2udsEvfYjzaOd0Oi26Fp3mO5XcMXIS+DSqEFSpp8NEoqni6X2S8ol8yTyJs2CiJ3
PxMyApHzVCTqVjoNTJA5Hgl9qTymf0WVwB5IMYd1QaLhKWjzbYSN+2ul6TfLTej5ycb+EXZ9jkWe
0Lm2hzoigXsbm1NmE8v3E1TYCqrQzrbHZ9ttiXeb/6cVEvKCNBcb2m2NOGFouY0JXyMB/Ws7z9O9
+VwAcgHIiQItbZLaGFUg2tQGtEY2gI23xcBQm6B+QB49Cyq83ZrbpB/EFwehV93iHu+M8iEF1Qwy
ZZotvh4v0O9YDbkiD0ClVdex35KuvOosVDHsMbRzPtPe7zhU0SqTEu4/Wr+b6Zfi/xPkACIiRvva
KDziYOsjDdMc/wPKd/IC0Q9fyzWNUXjRJaSudhYw0RbjM1D+iRcTnj+O2h4cueR4FPYUJBNdjCpX
XUPuPDa3tv67TtxjQ5HoFZVqdzuaGLJR3GPf27nMVc6jZiiH0vnEqkPOA85EAng+z/G9CY/tg8Js
O9m83i3DeekyFKZXaR4IPVA4z58S+nEygnxBTyFv1l+K+L1PQZs1AY8jr01HdLDru18Gx44GrrNr
dbG9dmMkoB5A+tCylTYf/oBCM79FniK/UZgw98OQCN05i9cE1XPn2fU/FW36TSIWijY57vFFFd6f
WtpYEYhCQbREz7g11U/AEOt3Q1EBjAV+KboA5AKQk2+62SJusBnmA9AOxTK06/ANUquH2kBVqdnw
q2xRFCRfXoNYBYaexErSZ1piczmx3ZSgykKw2zI77jbXDIWpKAFwMXYRW9nv3QR5BWxiE1qqkvbT
bVLrYQvaMEZYfxoQ99irZkD5DpNEs6+puSN5AyrFXUhWQ4lRD0U7pU3TGInVdsyvqHzy7xV47rZE
SXc3z+I1/zEBAFukHx/33M92nSzK0zV+IzXDC79CYuPQHH1GY+BCuwVeULOQx81t1K38cT5pgsKi
LqbmLu6/kDdXrubBjW1RHu+VczdK9J0PD9rmttiMr3h1H8o1BhIAz8ji/b6w7/pZBV7jqwIf2Di9
xOyEVJs7i62dnkSh0+ML/H2vR164AXOQyDnap1qaIo+tPeIe+8bGqg1SvG4YEu43JXVI/WKzob9C
Xnhf2fxYDC/5KpQIfBW0sRd4pAe3wCs9vhJtG9J7rwYsQJt4E62Pj0Vh378SS0ZdqbmJGiLRbW0b
+zdHntS9bY6ca+PzRyYKfUtleVq6AOQCkJMHGphosSlKzjrYjMBGtkD7FsWtf4F2JaZReSE4VTbx
9UI5c9YjFuO7mok8qQy8alugBTsgU23Cm2RtHFRlmG7G1yK7lVI7N7S22AS5Dg9CYSPNsniPX5FX
xKNmXH1CzRKmj6FcJ5UeAvYItau0/Bt5DuSb7ijX00Em4qTLDzKfWHW2K+062dGM3UqiLcqT86c0
BvM4WzDEi8aDUbnutkgc7Rn33AXIGyfXrG39bOM4keNaE2Zy5fXT20SG7eIee84Wo2Mjfj77INF1
v7jH3jPBJFffvRkSmuJzLg1FYST5CIe7wK7RgPE2Ds9B+Uk+SljMTbc5MYxlyMvvMgpX1SwqbAG8
gwTMv9v9A0hfrWmB9aNnUG6YiQX4rv+2PhbPo9bPKt02fpia3lBzzbYZaXPwZaQOX08m+Hxv4/mH
aENsPKW1gdoACcbNbU7qZLfOSPzsRiw58ip2S2cnLCOW02gM8voeSSyJ9ZwKFIYaWJuuZ+uzrdBm
z6rWXr+Yjfyu9aMxbhu7AOQCkJOJ4DMIqf2DzYhbZoPuJzYxfW2LkeUV1j4Niblnro9cMwOxpwPh
oS0rkXfObybqBDsbwf2JNoktrKCJrD0K8/p7hsePBB6y/vcZyn1xd8Ixj9kiq1KThV+KRLJEjkWl
0vNBZxsrDkEicbo8Fyttofooyl0zCVXC2c0WFY9W2DnbHXlQpMqHNB+4whZ9nxDLx/G9GX3LiJVp
DpiDdgnH5Pj77oM8jTra/VEoQfF7OfyMve3aXsXuT0UebA+X2Lk9CnlQxP+Ok1DoVK7YDuVfCsJl
p9sY+FKOf0sf5N0Q71m4kwkZTey5de3xRWY77Gbjeyph4xfkwfZahV33RyAR9Q0bA7ohb8nDkbCa
Lnx6rrX903bd5ytZ7LEkr/p1CRJ8K5FmSJw+LOHxE+zxLdFG19HUrqAXxpUoPGxWhbRhlQk/7dHG
UVBePfCUX9Xut0txLaxAXkNB4ZhhKCTuJ+QxVGmRB42tzTay/jfY1ieNbb32CYoK+NQFIReAXABy
sIF2K5SkcGukzi80g+59tLM3nNwnFS6FCaqrGbUbEyu52TuFQbvCDLEx1Ky+MNaEntlUZjhcIs1R
qMRfqVniOFNGmeG8h52PeF43IaHScgJdYgJQIlPMIMjlbnF3E3v2Q54+HTJ4zXi0c/2oCUCB8XGX
LYovRx4NlUIHlCcnXYW2F9AO/C+22I8XWm6z6wgUZhQvpuZjl/48W6gEBvmTwOk5nBuqrA9cQqy6
zkv2GaUaEtjTztNedr8aeQb8k9yJ/Z1QBbig+ttK5LHz7xz/lkep6fFwJQrPC/riaXHPbW/2w1rI
O2yfNO99t/WvShq3/4kSY8dXamxg9sYRwIHU9OgLY6aNC8+jHf9cj/Vfk7xKYyWKQB2QcLdbwuNj
kYi5K5nlLkxkMhKL/4vnwQFtpraz9Ulvalaj7YM2nsI2XOfb+fjO1jFDzRafTOV5CnVEKRW2Nvth
IBLextva7g0UxfG7d7m64QKQU0q0QzGku6EdvNXjBJ+3bVAYQeVV42pKLG/NlrZoXptwj4aFNsn8
SKz6ws/Iq2eud7NQdkIVcsJ2xpabKPA+sCewM5klGI1nBNpN/aFCruebUbnmZJxvQkN96WXnbl9g
W9J7+gSG2FvIc+utJGPK2ShU5mlqVlEqd/ZE4S+9UxwzFnnIPRP3WOA1EBDvMfWhGXkBe6Py0rka
G/9LrBT0YutXN+ewTdoiz6ID7P4iEzFuLpNzfiZwFTHvrWeQx8DcHH/G1cRCa+9GYYW5yo22FzU9
iz5CO81Qu1x8ojffgcjDIV2f/xPKdVMpPIVCZv+CcmrF0xqF1B5p82CrDN5vLsox9ILZc+Ny8B0T
w//iedD63ewKOFfrIK/VDbJ83UTkrfUSCnE+kfBQ35Go0uHbbiqG0gZ5uqxFrBptPxtbWqS4Ln5C
YuZnxPIpVVreyFbWZlubPbeRtdkvSER+DaX0mO3dLDNcAHKiTENbbO+CPCY2RLurI1FIwbtI/FlY
Ye3Swib0QTYYBknVkk3MS8w4HYHiab9Fws8kKq+aSV3piLwUTklxzKfIK+jzhMl+ELA/cpXvmeHn
zUTlSss1pKjKRJOrCN9xfNnarS6hmg2QALqrLfy2ILOE5SvtGnkMJcgMS1q6K/LW+hZ5C1SCwdHW
FlKnpzhmBXA78gxIbJPTTYgB7WRuYON4DyRCB55Y48wozoW40MmuocDKGY1CGz7NYbusYQvhAXb/
B1S97MsyO/+boXCaoArUt3YN/5rDzxiEBNdgTHgTCYe58NJqY/2sV9wY2x+FW2yAQjACz60/Iq+g
eNojj5HTSF0J8DbkzVYJuYHa2cJrANqUeyPkuJ5IRAtCxBpm8N4LbC592d73J+oW9tEI5d/aK+T5
0SYSPUX5elgcAdxCZt6u2Lz3mrXbJyiBccAWyNtnUIrX/w9511Wa531daYw2CvvZtbSpjUm9SV6A
YrnZ9EPt/Hxq806lrYNaWnvtYGvE9ewaHoaKq7xu7eKRCy4AOSVCBxTWtbctrjrbouAdu6C/pHJi
jeONmHVQXOwONkGsRvL44pkm9nxJrNzmOBd76sxeyOMhLCnodOAfaMc61UTTys7fgfaemXgGPWDv
OwqF55V63qrW9tvPMEMyjDdRSEg2i6jmSAjd3RYj/UmdwDyeUcir4QkkKKdaCPRFu3BN7HyOrIBr
YGtU/jpVueCRJvJ8GPJ8vAA0yQzcGSbOxE/id6O8PPVldVvABFX53kKeHpNzLFg8iUQsrP+cVsbz
U3tUwSsI15pg/+dSUOuKPDMCy3QY8qzKReWmIGQzYBfrFx1tzuyWQgAK2MaeS5Uf5Ufrwx9XwNiw
ni1ClyLv41Gp7HUbow+1ebBvhp+x1PrB63b7huxCjdqawJNqtfM58u59mdIvS93I7Oa+1t+PzeA1
E5D32rN2PlN50Te0972C8Epg4+w6qiSPuFyLQr1M4NjMbhuQXMRbiUS7z5EgGwhClZbntL21027I
W201s8/fQx5sH1N56RVcAHIizxq2cNvbDPZlyJXvJRROM64C26QrEsJ2sgXY2iTfOZtmBlFQRnGE
DXpO/cWKq0nt8fAk8vrJNga5jU1Qh9sipG2a4+ehHZ+fUTz49yhX0282oUXZFbiDTcp7orw7q6Y5
/haUNyaT39TFro1dkTDah9gufjqmol2ix1HYQSbJt1si9/YtkHfS82V+DTRC3jx/J9ztfwUK/7g0
zcIpXgAajrw5QeFYV8Udd4AJN/VhoL1HINrejELScmkQ729CRSszwP+R8DvKmQts8dfAFopH5/ha
aIRCrs6MW0zuhzYz6nvOnk34HUGluWHExMJUAlAwDlyKwkDDvFmWI6/RKypgIbafXW+fm72SiYDS
DIXjHoa8u1fJ8LOqbe57F3kGfYRyxWXyedeiML1U/Aa8aMLFlxFfMDa1+XU1m/vWtdtayHukdZrX
z0Hi+2NogzVbr8tVURh0qhDo22yMn+cmZb3pZCLQYGKe/51D5uSf7Np424SPyRXYXr1Q7qC9UeqQ
xmZ7vIg83Ea5AOQCkFOchcUAMxz2QC7Cv9mE/gpybVxcgW2yvhlQu6F8PsmEgdnWPu/bwD6MyvOI
yjebo+SWYbu8E21x8nQOPqubLUyONqGkQYavqzZDexISoCaYSPSbTfbTkBA4y8SUxXlciATlU9vb
tbyG9eWN7DrvlMF7fGELsvfSGPHrm9izM/KEa5vF95xnhu4TNtZke93ciXb2LyQ8r0S5sBbyxtk6
xTEjkcfLRxm836koPAw7xzvY/w+hilPYYmu9ehqrW5kY0RFtJJyVZjFfF05AnjCNbTw+nvIXA5Mt
+h9AYvYyFB57X44/43TkldHYxrMDqJ9XTVfrs8Eu+sM27mKCwvb2/2l2ftOxtfXpVN5AHyFviZ/L
vD/8HQledwF/yPK17ZGIfyjaGGmdxWvnoLDdN21c+S6N7bgjEmo3zeC9pyPR8Ruzs0YhT4vZyAMp
X5WIGtlc19TaJihF3tXEnt7I6zAoTd6SzDc+ViJh6yEk2k3Kwfc9CIns3VPMEyfaHO/kjvZoI2Ur
Ezo2RmGZya6Rr0z0eAttIFaad1Aza5+90KbranYtv2pz97cV2CYuADkFvQAHmxG3E9o5/RapsW/Y
4rXSaI68CfYkFsOaKAAsRy7l79jtSzLb8XLqMJ7YgvFKYslIE3kG5eeZlIfP3hjtfG+bg/erRq7z
C8wAmGMCyHy7zTMjdr71uTlmOFel+H7t0Y53E1v4tTGDo12ckdoyi+8YCDJ3mgGfGELX0IzdQWa4
b4VCe6qy+IwFyCX6SZvs61plJvBgedIWKuXM0WbQt0+xiLjVFn2ZhkscgoQ3bOERJEz+Gu1kJgpD
dWF3tJvdFol7R9k5zyV/tN8OCks62H5DJbKxXQ9Bzp4/EfPyyhV7mFDT3hbeh6MwoLoSL/R8g0Tq
oE/uZ/8far8rE1rafPEnwsX7WWjD4KEy7w9BUuj69IPuds4PsXE/m/mk2q7Jj21e+dTsyhVJBJZd
kJifreC0gNjmymy7zbXbUvusWYSHEVebbdHWxtFWZge2tv9b2f9t7dbS5tuqHJyfD1HC7qF5OPfd
kffuASHPL7b54iYqr5pVoeiCNhF3tFs/anvurjAB6A0U7vgllVm5rY+NAfugTcr5yFsqCIGsCAcE
F4CcfNLSFrOHmvhTZRfXc8iDpRI9V1rZQnZ/5MXQJ8kxc1CekdfMkPkRT2SWbzqaEHFACrHiHDsm
nzRFla/OzPD4CbaQaWITfje0Yx5FgoTkX6KdqDepKWY2RLuam6JcG1uj3FdNs/ycQPR51q6h+oaQ
bmPj1Uj7TrPL9BpojcK5TkxxzDjkIfFalu+9NbH8QA+ivBTtrU2D3CuXo1LqdWFPW4A2tz52YB6E
mfiKQkNtrPitwsfNXnadBULKeeS+hPsmyNuyly1WDqbuuUWC8uUgMXg9u54fIFaNcBsy82qLZ3fk
DdQrxTH3oLCxcg2HaWftth7ySPgwB31rd7vOshWDgvnme/tOHyFPod8TbKkuthDc2RbPvesw3xSK
ZWjj6UckNg0kln8sHTfbtZnvcPE/oCTRYVXfnrVjPEF0fmlo9uCOdg1tSXJv6TFmiz2HhNP5FdhW
7W282t/WqSvNfnwCpQdYUK4/3AUgJ9+iz1JbPD2JYsQXV2CbtLAF0IEovCtZNaiJaHfyJRt03Mun
cGyMSlSvHfL8N7Y4+K6A32k/5GmQiYH3K8pl84QtZnqbEd4Pubp2R+7j8TuK+SIITZuBxKnRZoQP
s/aLXzA3AdY0wWcLFHq3NrFy09kwxybtF3Mk+gR0RWLsKki4/aaMr4H7UYhdGE+i3f1pdXj/tVB+
siaoMteRKF/FMGK7lHvVcWEfL/4Mt2tnTB7Fn3dMhPDQ25gB/ZQtNgDORflWcsnqyFV/g3qKQHvb
GBEsqDdECVMfRd5FS+0z6hKy1dnG7ENSHDMchQyWq9fYQFtITrVFZ65yjwRi0H72vm3q8B4LUW6U
ITamD0ElpJfGHbOajYEb2vi0utlr7UzUqMpj28V77E42m/A3E3xGImG7ndnWh6FQ63RMsDH7+QL2
gQ2QoDow5PmfbPwfilMouti6bG/kZZssXG888q58BgmmCyuwnZrZ+HKwiUJNkNNCWYpBLgA5ubpo
tkEu94PNQHvLLpohVKb3SiNb2B6K3AyTefqMI+Z2+BGeKK8YHIXyPYTtLt6DvHGKMfCvgvIqnJih
4TkDuNfEoOHUjGmuIham1R7lwehELISrDRIqW9r1XG23JsijaIG9R5UtnIIQstm2EJ5hRusUu02l
tqt3T5RodWO027quGdwN69g+k+26eRGJpxPzcA5eMmHiKCQSliOnol3bFiHPz0Xeb3fV4zNamwDU
C4Vl7WkG6ftxi7N1yV64290M1uYox8R+5D7hZXzY1/OorHIlus2normJKPvZ/XyEg3W19t8cbSTt
T/bhYL2QIB309e3MsH8FhR6NswVsfebik1Eob5sUQsTZ5N+btFgcicL2XrEFZ65DfrrbInYftLHW
tY7vs8IElu+RV+pQmzfHJ64zbC7uYreuyGO4vc2dQQhX47h5s6XNk0vj5s3FNo8utDF1tt2mo/xn
s4iFl1Un2JL9TfQ5wT47HdVmu1xoc3GhaYm8jsK8SRfYvPOwD50Fp7VdNwegDelkm4xjzK4K1nDL
K7CdGsat4Xa26/xj67MfUQbODC4AOfUROLY0Y3h7m+jetgHjKyo3ZGlNtAN4CLGqIvFMIKayf0Bl
quxR4XJUvScZS8xIvz0C33MQ8j7IJjfQVOSt8pldj99b31ua5+/axAzkvnYtrEdsJ3U1wqtJZcIy
tHv4jl1Dn5J95ZJsuAyFJF2HKpOVG21QTobjUxwzBDjO+k99CfKvfIK8qY4gJqqNRDvGy7J4v62R
QNfW3nv/PPSHE1EybJCb/BFUphdrJjQzEWh/u38CuU8M3cbOww62eN6H7MK1Gtu4uF6cWPGoGfaD
qX8eqoB1kUddqmTD96Gcc3PLsC9chypk/hO4JM9j2CBipZ/Xpn4h0MtNFAo8V0cib7DRSFguxPzZ
w/rPJmZjDyTzKmmYXfl3mx+LzWkorDgsrO4KYiGZTuFphTbvU4lBw5H375PIY64SaWjX46Eoh21j
myseMxu7JAUyF4CcbOlvRvButpirdE8f0C7QnihMaNskk91MJI4F5Tbd06e4tEC7Y4eFPD8eJcL9
IELfuQHyRDnPDN5sWYryH/xuxuwY5DEzDXnvzEJi5FJiu5bxNLfrvY0tuDugncguaEe2J8rl0tWM
1VzlIZqMPDvetdsPBRpnglCRd22sW1Zm18AGKCnthimO+Y/1t1wJHsGi8HtbgJ+BdolBCSn3zuK9
NkKJLDvlUfzZD4U2NXLxJ2PiRaDlyJX++Tws+gMRaKpdn9mEZgZefSDvzv9Yn1wHecKdk8O2uMb6
eRjDbK4ZUWb9oAkS6bdHIt1LBVqkrWP9YkfkKdYlR++9zPraZJSHZ7zNn1Ns/pyJQrfmWr9P9BAM
vGmbmP3R3ubPzjZ/9kGbJKvarS5h2p9af3uZ/FUoqwvbIaE/rErYY0hod6/K4tLGrp3DkMdLh4Tn
lyCP3QeQF++cChaDNrV22smu99etj5fUOO4CkJMJPZBHy4FmcH9inf2jMlwYZcNAlMz0EGKJTOMN
hk+Ru+BLeE6fqLAK2skI86b5GlUyGRPR79/AJuk/2sKnWY7et9qMxoU20ScKQK1t4muY598XlN79
BAlwXxfB0FgdhQSsRKFqY8vsGjgchT2GhahMs/71VI4/9wDk+TjL5pTziHkHXItyx2RCX7Tx0If8
iT9bIIGpjQkYh+PiTzbCx2NIQJuLSnx/nofFSiACjTFDfHSGr/03MY++S6zvTbBF+YEoJDuXHIzC
4TqHPD8XhcM8Vmb9oA8S7xvYODq6wJ/fDgnFWyOPwwFmv+aTFXabl2R+bYrEnwbkLpfQYlt8/tfG
wpUR7Qt9ze7aKOT595F3xVScKNAFbcgchTYcEzf0Jtn5fIDyzYuYCY1tfDkCeZDORAULnrQ5JdK4
AOSE0RLlVzgKJZP9Dok+b1LGWdEzoJUZtifbBZ+4IB6DPKIeobBJg53MDNIXkPdDMl5Cu7GlsrOx
hi1MD0SJKxuW2PlYbpPkCFsgfoZ2xItZIaSxGdQ7oJwgr5VR/28CXIVKAYfxGRK18+Hq3QOFVLS1
BcEfkAgEcDqZhVt2ROLPQPIn/qxuC5Ke9hn7UpnVUeo7T75g19F45AWQawEgXgT6GlVzymTsOJ1Y
fqJrUG6rX23cXy9PhvuatljaMsUx16Nk4+W0qbY78haIgidlR+TxuCUSeDewMalRibXpCrMtn0Gi
4a8l8r3bIq/TME/PETbWjsGJEuujUNlDqZ3LdAXarLsLbZRU8jzZ0sa4w63NfrR14KtRXTO7AOQk
sqktALZFavzTNtFUujLfF+XKOIbaVbwWo3jQu9Cusef1iR7rIdfo3iHP320Lg1I0vhui3c29bBG0
IdmXzM03y5H7/Cjk4fO1/f01YtfLlbYIuwTlrygXuqHy6zulOOYW5IWTT0+XN5F7+QEmCgThMfsS
q84URhMTFXZDguHuKA9MroWLN9Cu51D7rOk+fNaJTtaWGyFv2F3zsEBoh0TaLZBwuy/p87TsSyws
7Wbkyfw0EhZ3yWN7NEPeR39OcczbaBNichn1g4tRPrWrUG6aqNACbaIMsD46wOy8rkRPFFqANkfe
NDvmW0oz5UJj4DbgpJDnx5odM9KHz8jRwubcE1FoZ6L3+XizMe4zO6+SWQVFEhyEPD8/QDnhvorS
l3QByAG5+wWJi1ua0fawD8KAXIf/hOLYE8tT/27tdD9KTutEkw1t4dgr5Plswk9KgdVQpa1d0c5N
qwIbqjPs2vjFrosf7P9xRHuHKFgYvmzXe3WZ9IetbJwK6/9zbVH6YAG+y5+Q0HSlGZRn2eObo7C7
VNyGkop+h0SkfCySH7QF+BiUR8R3o+vHGiZq9LK2PTYPn9EViTfrWx/5Y5rjN0OhSaAk6AtNmPgz
sWpv+eQYuwbapFgEH40SU5cDDWxMDcq4vxDh79rK+uqaKJ/Q2vb/qsiDqJAbK/ORB8EbSIz+rYzG
hWsJz7U1Dnnffo8TVfqhDfEjqZ04epHZUbeV0RhWH9ZDkTS7mn0cJNQueloQF4Aql4bIy+d4tPMx
0gy0t8l/pYOo08QWgGeg+M5EvkA5NJ6lPCt4lBMDzYAKy79woS1Gy5V1gatJn2B3pk3c8TkSquyx
5cgzarHdn4NEnukoFnwKsQTTQQn4UgsTDfL+LLIF4qQyOf9/sEVu85DnR5oRN6yA7fwzCjWbgNzK
l5tBmWrX8CxUTeZ3JMz8nIfv9hcUhrMACUyf+fCZEwYhz4WWKAn4DXn4jLVQgYVVra/cnOLYvsg9
vxEKSelt33FNCif4bWiL+/VCnl9kv6NcSsWvauNrE4qTD6i+tCRWCj5I1NwFeVZ2QuJQWxtnmyFP
l0Z2P34jYbo91iHN570EnF/mIsiFqApYMiYg70tPoxBt2iBv3lPRJk4iH6Ek+y/6upImyAP7GBv3
v0XeUh9QJG8+F4Aqj25IjQxKtT4FPF5GC5760MoWQ2fYwjmexcjV/D/Ah0Q32Z5TU/x4lXDPh78j
t/RKYH8UfrBGyPPjrD3eNQO2CTEBaJndFtr96jJrm4Z2be+MdmnKYVJpgnZZU1UgegrlMit0zquX
rJ1/Ql4bc+xanRhy/C4ovGch+RNmtjYBobHNj4/48JlTjkReaMuQgPdRHj5jS+QJ1ML611shx3W3
hXVbYiXh30CbPoWkLQobPzjFMTcj79RyWDztZuPsW8gbqNyqxlYhcaeFjSONiQlAS82G3NFsjp4h
7/Grne/nKmRcuIDwDTj3BCodGqBy8mfYtZ0YHva9rZ0ewfPpBevww+LG/udsfizoOrzUBaCGRx99
tHelzCambVAc9jlmhF1r9z/zC5IOyP37XjP+471FZtvjJyH38LFluAAuR1a3RWPvkOcrSfwB7Xg/
aMLAJtROGN0Oxd5XA68QK287x8aHhZRvxb9LgRNQ3p97y+D3dEE5TY4Ieb4a+IcZa0uK8P2Wm/HT
Bu2Sz0YhMcnKAPdCglEblFzxrTx8n47IZX0VW4zc5MNnzhmBqiBti4Sax8h92effbZw71Bbaz5Bc
3GyKPONa2bjXDLioCAvNJUiEXYYSWSerDLWF3cqh+MavNvacaPffL8N+HmyUzCfmKTsDiT/nmc3R
IeR1NyOh9NsKGhc+RuLYjkmea4cEoJdQ5UgnulTb2ugJEzOqkEdlIAR1NvvycCSM/piH8b+UmI/y
GN5nbbEzEn63RF6C4wvxJfr27VvawoZ7AKWkrV1wh9lF9yRSYD2ppehghuCf0a5gPFPQ7twdlEA5
P6cGq5jBvGHI8xcDl1dw+2yN4rPXD3n+J5Rr5b0KaItd0O7/a8CelL64uwny6Ayb2afZAuylIn7H
ligZYj+7P8au1cTSyY3tvOyIynZfl6fvcy8KhX4VeYGswMkHDVEowB4o6f7Jefqc81DY6ztoNzpR
uG6NQh6DijY/2nVTTIFlb+AewkOVfzU7bmiJ94Equ852s7H3rQro99ujCodrhzz/HSpA8VEFjw1B
ovBkDLO+4iXiS4seKDTsZLQpFc9EtOlzJ0o/4GjsPwqF1K0wO+4x8uihXeoeQA28zyRlHZQv4R1b
7P3T/t6Miz8g4ed8tCt5FTXFn99tsbEe2hV08ae0aIZEzjDx5zoqW/zBDM0tkKdDMsFjbSSgXYk8
hsqVrrbommaGSqmLP4eiEL4w8edb5An6UpG/5wJqelpVhRx3KRJ/7iZ/4s9hSPz5zfqAiz/5Y4W1
8XjkUXtYnj7nGuszO9rCMkyICLiX4nvXvGTX5rchz6+BBPlDS7wPVAOn2Jh7T5KFYTnR1ObQN0ku
/lTbHLwFlS3+YGuUsDF+QxQe09SH0JJigq2h1rM11e9xz3W3tdcIW4t18OZimq3bt41bs79jj63j
zZNkEncPoP+nAVLJT0IJ6l5D7mW/edP8P62QZ8PZKAYzcbC6CXn9zPGmKlmC3fxk3J/iuUplL7QL
0y3k+U9ReFQ5Vrl7AlU+zKQEedS5iNRl659EO3FRSVrf2Ra73ZHreH9qegAF+UK+QIkT8xGm3B0l
pu2BvL9e9eGgYGPOSzbnbkp+8h60QgUtNkdeQK/HPdcaGI7CgyeiIhjTItI2bcwGOSTFMeXgwbof
ChV5ktIXtZLRz+zvLUKen4S8z1/24aAG9wHHpXjuBG+ikqWt2SBnUbty2CRUgOF/eEqSeFazNcse
aOPkLuQ1mZMctO4BVPq0sYnkHZTT4SnkcnoZLv4ENEHC2AiUCDd+sRvv8XMdLv6UMucRLvC8RP5C
DkqZl81IfSfk+UEoVvngMvvdf7RF1nWUtvjTCnm8pRJ/LrFFVpQqFk5DAlwyOqPQ27m2GMiXQXi5
GaLX4OJPocecf1vb50vImG99Z671pbDQqieIjviDfd9D7ZoN4592zbcq4T7wvI29h9hYXE4cgnJr
hok/gTDp4k9tTibcQ/V4lCfFKU3m2DWfzCOomz03wtZqTby5wNbwlwHbobyOZ5qt/gdb+1c0lewB
tBpypd0BJS+8Axji10st9jUjc4OEx6eirPS34KXcy4E9zKBKFk4yGsXVzkGups3RLnBT+xtU6mhr
E09YKFAVSlg4x/5fYLcg6eNClOxxftytVEJKGqG8GX9Nccy/UdWOUq+ANxB5fnxlE+uSEv0dvVCM
+JYpFpMnoU2BKHIiCtUZS00PoPuBY20hla/vHuR+GoJCbxb7EFpQmqGwl03Ib+W9g5GXyf3ENgfi
PYBOQqFIUeRguz7CDP3PUI7HcSXaB5qiEsgbo9Lw35R4n26IQr5SiRTXo5CX5SX0m1rZrSWqcNYi
7v+WdquL/bTIxvwl9ncRygfTFnmH9UnyHiuRB+FrPoSWPG1Q/tUzUN7OeEYgr+YXvJlqsRkKpV4H
hfz/jzo6e3gZ+NJcvJyOyua+ZcbLeL8marE58C9qVxeYjxLgXke0dv6c7CePzkB7WwjfgETRZMxB
iUDbmUFTlcfvtQIJQYE4NBPl3ZqOEotPRKEPE1B1kKmowkVU8s8ci8TkZiHPv2gLqVJN3NfcFp4b
ABsBI0v0d2yJvBdSlROOetLYPZFoOxUlJJ+GvB8eR+L8mXkUHz5DoT+bIiHQKc4c/TkKBdyS/Ilw
N9si40DgWZs3vgc62WLylQi30cZ2PawR8vxvSCj9okT7wPp2/X2Hcl6UamWgDkhk3Dvk+cW2aHsg
Kmsns51WQVUQe9itO8rL1MluHYiJPC2oXT00l1Sb/TQbCUlhwudYtFH1m9lO0/BN3FKmM6pOfTq1
vRrfAS4s4fEtn/REm2g723z2X7KsIOgCUOmwI/L46Yh2fR/HYyWTsSpymTsGeTUELAceAq5AHiFO
tGloxkkPJOz0RTtCq9k5XgXtFLWoo6Gx3ISaJSbULLfrKdilWki4UFRtBkpLtCPV1BaVTeOMpcYZ
fo/ZKP55LDDKFu4/mnHzm32PQrO9jTFhCTqH2WJqVAn2q+uBvyAX2rtK9No4HOVtCgsBeQ+VgJ8c
8d+xCTGv1a3MePkVCUKDyF9i3rNQYsXLCU8S7BSGf6Kd3rNRDr580BLlMlvFhJQNgU/suVIQALsC
j9q4nIz5Np49VqJ94GQbz65DoSGlRl/gGcILT0xBwvYHRfhuLc1e6gOsZf2/L/J+64Y2xTLZEFtG
bFNrCRK0lqA0HAvMZkplL7Ug5h3UymzzlmYztbD7ddmYW4g2+KagkKLxqKrkKLOfJth84sn9o8/q
wD+Ao5Os3R5AYbFelKc2rdBm32FoU/lOwlM61MAFoGjTGJWlPd4G2HuR66MPZrVpCvwJhah0THju
bXvcd3qjKfR0N4NkXeTWGBgpXUzkqWuurzeBj01kmWW32Wi3KHA9XmZ/V1Bz9zETj5wqYmJQE5u0
gv9bmHHVFu20tUc7Hd3s1tXudw1ZyFebUfMT2h39FoUt/GK/I9+sj9yww3aexwP7U1pliXdHuV4e
NxGlFLkQidhh3G3jYCmEtfW2Pt0ahXpNR0n6N0chevkg8P6YYov/UvU4KBda2Lzc2cb/fHnlboZ2
kW+zzzrY5oH+lEYIVVPgVhSyFsY/kNdzKfKYLWASE3ZHnY1RmEqPkOd/tXnyuwJ8l/bAmtanByAv
137W35OJK/PRJsE0+zsx7n5gL80xm2mhrUGW2oJ8aZzok6mtFNDc7L6mtsYJQvHbmM0U2Evt0MbA
LnVsj5Vx4lCwufaDjf9j7ff6Wip6bIIqhO2U8PgMe/xWSjdsP99rqd1RovQmphe8ZGscF4BKiGbA
kWakTEYhGZ97/w5lR7Szn7gD86MJP897E0WC5rbo62cGyvpm9PcwAyAVy+MMlQkodKctcqtORpDf
ZUGE26OpGT2d0C5dbyR8BQLY6tR2g55ri+Yv0a7212bM5COErLddO2E7m7PMuP2gBPreKkisWmkC
w+QSu3aa2eL1+BTG7vnAtSU2HvyAQjgnIzE034vYK21OKLWFZjkTCLNXIoEzXwTi6SQkwo9FGw6l
lP/pbyhXW9imyL0oqXKp5bTqigS6KhNVSiE8f1u0SdI+5PlhqNrZ2HysfWx+3gh5S25ugk8ye2F0
nAAyyr7P70hwnxvxxXRL4H0TBZJxBxJ51jM7MthYa5TmfeeZHfk9Eue+QZttY/FNgaiwHxJ8+iW5
rs5Gns5OcrZEEUPdUKqAR5PNCS4ARYtWZuDvbYbxHfbXCTcarkJ5S+J3GOaYkXRLxAWAcqaBiRrr
m3Ey0AyU7qTO8D8fue6OQwLeL3FGy1QzWIIkxM+aAJHIEpQc/dMSb7/2KORtTWvHDa0Ne8f195U2
RnyIcoJ9Tm7LKnezxdmAkOdnoQTb70e8PR9Gono+E87mc5xLFQISVMt6rgT7+cfAYDNO5pkhn6/F
Xy/gZ5THqtyq2pU6TyFv57XIn0dOZ7RxEOQz+RjlnSk19kf5ZsJypLxjY92UEvtduyEP90eAoyL+
Xbcz+yNM/PkWFabI9Vy8Bcr5sQ0SLwMhsNpspBG2QP7ObKcgT04pF24YjJLdJrMbnzP7I7CZ2qDN
nt5oE21NExB6mS2VqnLeUuQVNAIJQp9bO06g9AtflCotUaLo89GGL3H9/QG0mTPZmymUddEmeT+U
b/Fe4lLHuAAUDTog194dUU6EO/ES7uk4Dok8iXlKHrdBYaw3UUFpbJPtJshtd1O73zLk+JVmHP1q
k+xwJPiMsgXg0jSfF+QNSMa/kCdBOdIECWv9ra0HoV3AYHJchryDXkIeDt9RfzfnbvZe/UOen42q
7X0Y0TY71hZM11J6ZWTXR/kl1gp5/leUBLZUK+jcizY9ZqC8LB+hXCD54DqUPHQ9tPPrRMtQHYk8
ec/J02f8zeamwShM/G6bR0qRgai6WViI7k/AQRQm9CiXXGvn/xiUszGKbIPCvtqFPD8MebXVV/xp
aOP/bmhTeDNiuQXnIO/fT22+/w559Swt0/HhCsK9AzPJ59cECcB9bTHc39p2TbTBEuZRtwAJaUOQ
YDzE5txlPmQXlN5os/+whMenIHHofm+ilPSy62RjtEFwDzDTBaDi0gFl8d4JhVHcjbwcnHDWRMki
90h4/HszHLw8ZIGuPZRYcCsziAbZuQlzvZ2OBJ6hdhuGkvXNq8Nnr2JGT68kz42w71JJCdLbmjEz
GHk+bUlsd3gU2ql8BoXF1VUM6ovKZvcNeX4y8q4ZHrG2WcMM5Z+tr5ZSaMQewIPUzmkW8AFK9jyx
hPvuX2zRfydyWX4D+Du5zy3VBYVC3Incx53ocaMJMn3JvffKxmhjYHdbLJ6IxMAbSri9uiPPwG1D
np+BPGlKKdSxGRKC10AbG1ErNNDfxqiuIc+PQjlr6lpopCHa2DkQebYE8+1cVLnwXWuf75AIVCm0
QmLXBkmeG4s2HKfX4X3bmLiwoY0RGyMxukPI8cuQAPQp2vD62OzYapxCsDvayFk34fFXUXGHX7yJ
0q6dTrI5421MCCrZRWiJCkAdzQDZygb0B0v5JBRQcDjdjLh4V8DFwL+BayhOxaRKorVNtLuY0LA+
yuORSDXajfrKJsovkEA3I0ff4xrCPTn2Q7tzlUxbM2R2QaW2148zTh9CXnI/1eF9N0LhU2GCxM/I
i/H3CLXFq2Y0bEJpJaw+FZVCD6sm94Ads7jE+2rglXOvzYnbIoHmIJT3K1dciKpNrYkSmDvRo6cZ
8Jci795c0QiFmN2APMwCr7NSrTqVKJjcgbwcwxasZ9gxpUJQHfA1am/0Fbt/vmNjSDJm2Jz7dR3e
e20k5h9JTPT5DnjF5tyhVJbgk4z90WZWMq5CGwe5oJMJDJujTbWBdu6TJdJeZOfpXTtPQ6jbpqaT
OS2A82wN0Czu8Tk2z9+GC3KZaBDHoLQCHyMhaEap/YhSE4DihZ93kNvaHO+LaVkduJ3a1QA+QvGh
w7yJ8sYqtijbF8W9h1W7mIK8ct5Fos9I8pN/qa8ZQ22TPPeifU8nRgMzWHdFeU8G2WOfA/9FYlk2
BsteKO4+zNPrXRNcouCKfgZwsy3yriuR81WFBM5UC9OLUQnzUmctJBK3trF8G3v8Iutfl+TQYPzJ
RIC/+JAQaW6wcWotcpeM9TIkJl4eZzdsZePeRmhHv9S52H5nGNeg0PhSWRidg8LBzkRCeLFpigSp
sDxsy02geDmL92xt9sofUW6flci75ykTE37Bc88ks/H2TvL4HLuWR+fhM1uisOFBaONzM2qnngiY
YDbQS8hD1yM68seGKM9rYh63N1E10dHeRGlpi9Kp7EgJCkGlIgC1Q/F3W6FErQ8gl04nPcejEIH4
ZHvzbJFwK17GMR90MMHgQJvwkiU6XIrCrd62AfdrlAsm39xqBlMiy9BuzRA/fSnpibyCjkM7XHNQ
6Ol/kStzNsZ5GDcjd9xish7Ki/ORTW6lQCvknRCWoHihzSOPlElffAjttLa2397P5sUGqHLFg2ZI
15dDgPvs/d37J/rj0082Pj2Zg/fbG+10HmqL6TYoFHm+2RHfEe49U2ocBfwPCZ7JeAqVCC6V8Oh3
bHE3gOLn7LoF+FOK57PZZOhjNsxJtgD7Am0Gv+LjU1o2RSJZMs/YGymMwN8OiU27oPQdG5A8QfUs
JAY9g8IGPcoj9zS06/JyalYSnoU8i+/zJsqINrbW3tFs5jspAeeUqAtAbcz42Bapa/fjHj+Z0tkW
kocnPP6uTZ4/ehPllGbIw+cIlHSwc5JjFtjk+xISfn6msDtUqyPvn3ZJnruf8BLZTnLWskXDSSjR
81PAP8ksceijSa7NeI4AHiuiUfA+CoPbqETGip4memwZ8vzv1t4fl0nf649yWuxgi9I/WH8MYvh7
ITHsJDIXJsN4yxZWJ/glXxLchxLd1zdD5epmyJ5IrLLYWkhgutP617t2zQ0vk7bb2sbdME/dz5Ag
+nsJ/JZ10MbSlzZOFGuz73Cb78J4zOa7TMa8fyCBfxLaeHkIz1tSl/HhuCSPz7b5fkwBv0sDG1N2
QmLzYJIXPpmGcnE9arbJYj+NOaUf2sTcIcm1eQZ1yw9ViQQeQYORB9uDRDiksUGEF9PHo7ClBmbE
3oyLP5myg036hyeID2ch1d3Fn9yxJsq5MAK5OB9NTfFnsS2gTkG79bsiL5wfKbx78kkkF38Wo90f
Jzt+RqEDvc146YBCcl4gebLFeM6014dxA9rpLAZ/Rd6WfymRsWITm2zDxJ+hxDYRyoW/oySaXxDL
zbR+3PPjkOfnrYR7NGQ6vg2093FKg1vsnK1Zj/dogUKHrqNmWfn17O9X1vc+Ine5Q6JAEEoZVhVw
S7vuNimB3/IDyge2jY3pxaAPqROF/2xzYSo2QJtmX9kcu7fNuRfj4k9duDFEQGmHxN5CstJsjFvN
Nl7XbOW3Er5jZ7OtXzNb+9J6jm9OTX5EGwZ/oWYu2MNtLbm9N1FGzDGt4mS0kXo72jhrFsUvGzUB
qBFS9+9C+X7+ZEaICz+Zn8+LUEhR77jHvzTD5WY85CsXNLTJ6gW083kJNcvJrkQ7b38zg3kXtGP6
WxG/c3ubQJPxHOWzg1sMlqLcBTuhHbSZwHso1ChMxJlmhm9YTomuKDl7oRmA8l28SGkkPt0PhTqE
tfMLyC23nOLZ17ExJchZ8nncuYvnVSSM3V6PzzoU5SP72i/zkuFrO2eH1OM97jCh4/Uk40N8n7vU
Fg79yqj9RtuC54UUosY7NvZEnTtsLL8myfhQCP5NeMWvapsDp6Vo50eQl9lU+/472Vy71C/zOjPc
bL5kHEPyTcJC8ZvZyruY7fw3G8/iN0vXMJt7uF2ju5pN7tSPlUgc3MLWjPHX4Vu2tmzgzZQRc0y7
OAOJ1neattEoaoJBVNjVJqs1kQp5HYpDdDKju02M/4wbDFeiXCPbINXcqR8tkXvfl2YY70NNZXcm
ckveGsVaXxehheeeKCwgGbf7qc0Z3yPvxYHI9fM9FF+dzK359TQiy0GolG2haIjcgOdR/BxEmXA2
8DQKFU7GjSgPV7ltIPwVhZIGi/AfUPLMQSELsOWkzjmViv1QmIVTWjyEkurWhWtRTrhkAvQgFP4U
eAZ+jooWlFty8Dk2doR5xraxsefsEvgtZ9mY/t8CL5QPsDksjDuoLTAGdtblNncGicZPpPh5jMqJ
MJuvJypUEQVGmw29qdnUd1MzD1Azs8FfN5v8uBA7y8mOEbZmvJaY8NbQ1pYv21rTyYyZxCq1rmlj
3q5R+XJRyAG0GUoiOMMu8N+8z2TNtmbw9Yx7bCJyQ3vVm6fetLQ++leUFyGRkUjhfZzoVi14DeUm
SuRdSifJbynSC1WPWd8WVC8mPN/djJewnBPD0Y7MogJ813PRTvGJKL9HVGloC7M/hzy/3BZm5Ri2
1MP6y0EoH0nAI3Z996Z2zHkTu/7fIDuvsvWsvw5CVQqd0qErEgn3znLhfD7y6ElWibANMBYl2433
Jt3SxJDNkBBZbvzJxpuw3dtbbLyJsnf1CahCzXkUxrO0ORIH+4c8P8H6y8SEx/exeeg7VJp8nF/K
eeMdaud8CWzFPSL6nVcBDiOWUiGR0Sj0+QHyU0W30tgDReTEiz7jUe7LD715smY1W5d3RHlXvyzm
lymmB9Dattg42Az1i3Hxpy6cgUK+4sWfN5Bq7uJP/WhqF+twtHuWKP68j3bIByJ3v6iKP2shkTAZ
nuU/v4wDTrXbITbWxZdAnUhq74z+KHdTvtnQxuPnibb40x65fYeJPzPtmizXnDUnAcOoKf4ERnsH
kofiLEWhXEfbfJEpu6HdQBd/So/Jdu52y9KWONL6ytIQm6299bV4PrM5slyThN+KvKnCPNL/bONm
+wj/hnvtO15jY30hxqn+KZ6/lpriTxckpB0cN1+6+JNfwmy/bc1mjCJTzdYeaPP8+wnPr262+nCz
3Zv6aa4Xr9pa8q24x3ra/T9682TNbyiULhjrrinmtVYMAWgVtCN+OvAsivH8wftF1jRD7mQ3Eyuh
WA1cgcJ9JnoT1Yu9UdLBO6kp/KxEO6Dbod2TF5C7fJTZHe3IJTIBFwkLxXdo1+R15BIa7wZ6D6qs
Q4oFRj5dmxuY0TSXaIc0rGkG354hz/+Mcne8UqZ9qDny/PlPkufet7Fp65DXTkchLX/PwnDbHrl8
O6XJyyTf4U/GH80uO5Dwii/bWh9LtvP7HzNom5VxW25HeOL+vZA37RoR/g1n2xh/K1CVx89pSbhA
j81198Td39XmxNeRSP2dX7oFW9wn89hrYTZjlFlqtvcOdl2+Qs08QUEFw6/MlnfqzkTrD1fGPdbE
xpE7cJGtLvxg2sezyMP0AtNGCkohBaAWSJH9J6qwcBaqIuFkTze0C3dK3GMzzXi7CE/0XB/WQRUn
XqRmVR2QZ9XWZux9QHgC36ixVwoDYKaf8oIvJM4wo+VvKJxpfsiiPl74OCiP3+lMVLbyLBTiEUW2
s4Vn2K7ye7ZALedk5nuj3CTJ8mb8jjwxUhnuP9t7XAWcluazOqBws/f9ki1Z3rdz2CHNcadZn9iH
1JUJd0X5fpKVQH/NxrFyXmwNtzHmvZDnB9gYtW1Ev/9YG+O3Ir853g4idYWm/1hfaWhz4HY2J77i
l2xBmUn4BuAeJfIbqs0W38ts88ScJuubLf8S8mB06sYK4EKU1yt+zXCKjf3dvInqxBdmf3+Linqc
SP2qtmZFIQSgKrs4r0Xl5c40A7baz32dCIyM7eIeG2GT+nPePHWmMdodH5JEMBlhj+1mBnAp0QO5
cCbjGT/tRWEWUvxnIc+blsDDpM6fka/winVRTojniW444PFmZIRVk7kPeQVNLvN+cxSpEzI/BWxO
6lCUIch1/iZSh4NtiHIo/OqXa8nyq53DVCE/Z6H8Nvta3wijvfWtp1Ic8wDhlSbLhck21twf8nw3
s2+Pi+j3vw95Tvyb/FVuSzVX/W5zXUub+2bGzYVO4QmzATej9JL9fopE6r2pXfRmL+QNdIHZ+k7d
eM7WmvHtu72tSTf05qkT1WbfngUsMa1kL/LrpQnkXwAaaD9mDeAfKFHlEj/fdWZ3lLgt3s34JbTj
5GF0dWct5L79L2qG2ixAqvemlO7u1JZA2ySPj6X0xKxy4260Y/UkSlz8SIpjtwI2ycN3uNnG5KhW
8bkC5a8ICy35hy04FpV5X+mLEgg+neKYV1CY2OA07/Uu2qW/AZXTTcbmKLm9U9qMtHOZjEtQ6M3B
hHu1BAxGrv6pQgKftj66epm36SIkSl8U8nwzJLRcEdHvfw4Kobk5D++9ic1VYTyIQuafQB7V9/gl
WlQ+JbnXbztUfKIUedls9gupmQi6FQpjepfo5jgqBX6wNedLcY+tYe26qzdPnVmCxPF/WHtehyog
5o18CUDdkTfFfsBtaLfRFf76cQzapY93577J2tjbtu7sh0InEo2Wz20CvJLSFi3DFoMfULtakFN4
nrWF+6toF2BJirE61+VZTwZ2QtXtxkSsXVojYezCkOcXoGog/6qQfnIQcheekeKYX4l5+KTjJeT1
cTHaiU/cbdoIGOqXZ8kzlNo7s1WoDPQl1gdeyuB9DrT3Gp3imBnWRw+skLa9wsagsGpDF9oY1jpi
3/tXJPjvYnNALtkrxbpiMdrAfMsW6e6xXnzmmS2YjK1KfDF9pdnwnyf5XZ9lOE86yZlFzJM4oAMK
tzvGm6febXsT0k72R1pKXrzxci0ANUOur2cjNfCSNAaDkxl/Re7VQbLnlTaBn03NxGdOdgRJuBJz
JFyPFO5ySEYYtovzlp/+yHAbMAq55aeqhLhTDj+zl/XzN4H/Raw9+thC4eCQ53+ztniigvrITsBj
GRz3GNqFyyQZ7yu2YDsdhYXEL1R7416l5cAPyHssoLWd61NRKFMmnq3NrP89ksGxj5uwUCk8YW0T
Nm4fbGNZn4h97/8Bb9sc0CuH75vq3I8HrkZJoO/wSzMyvJWl7VhKfGe2/PUJj3cgVoTIqRsrbQ36
l7h1aBNbq/7Vm6fejEJepu8hr83jyXGRhVwKQNuj3dj5KM7ycz9/OeEK5AoWsAiVar3Rm6ZeXG0L
7vid7wXWtoGLdKnTheTJGBfg4V9R41ygE6mTZ/ZHOZ1ywbUo2VzUqn5tC3xMeN6qT1Gyx0qaXza0
ufqjDI59DmhD5ru3r6GEn3uiOP5VzThuSWox0ikNfrNz2cHO7Yd2rvegdjn3MLYiJhyl4yOU3LeS
8kF8bmNS2Jy6qbXL1hH73mdZ37gmR+/Xk9qFM+JZ0+Y4X3RHr/8mC6FegyJUJsoDS82mP5Ka3npV
tga42rtAvbjR2ja+D10HXO5NkxM+s/XBfKSxbJ+rN86FANQXefqsb1/uaZTPwqkfVXYRxYdAzEYu
249789SLq4DzEh6bjHbOHy2j37ka0DHJ478S3WpPlcoMM0ZS0ZrwKljZcBDamb4A+D5CbXAiSqAa
5u76MLBzBQoTe6LQmkyqO/6Oqj8dkcX7v2btuqYtBk60z5rol2XJM9HsseOtD62BPFZey+I9jrI+
9XsGxy63PrRHhbXzb3YNPRzyfA/kaXFihL7zSBRecCiq7lNfNiB9uNs1eOXRqDGa5NX/OpsNWS48
ajZ+YrGI82xN4NSdx21tOjvusX+gjUan/ixHBRj+ZWuAS8hBrr36CECtgD+gkK8ngVt8YM8p/6Wm
G91kM6o8dKd+/Bk4P8miaTfgkzL7rWFVPr7Cq/BFkSczWHTXt3JLBxRfPISa8dvFpBFy0b6bcBfX
S1CFoYUV2C+2JLsk9PfYIj+b3CPvop2laiREVvsYURYE5/A65Ka/PekTPsfTBtiR7JL1vmJ9ttJY
aGNUWGL1pjbGXYu8pKLADSi3002krh6YCeukeX6CzXFO9MaIr0KeK7fS6Z+YrZ8oZp9vawOn7rxl
a9R4ge0clOLAyQ0zUfL+J9Gmzh+QFlMn6ioAbYfUvd9QjJrnCsgttwOnxd2fZBfWZ9409WI7ascC
z0DK9bAy/L1huzfDvStEdnB/J80xfev5GZeg3egzUDWWYtMRhZaEVSGbh7xZ/lmhfaIvCtX7KovX
vIZc3ffJ8rOGADvYfN4fVV9r45dlydIGlSvvjzz9ts+yH2F9aD7yzMuUr1Bo0eoV2u7/tDErrMjC
OShZascIfNdltvDtSbhwlc1YlYq38YIlUSXMJuxdhr91GEqum9gXrwO28a5QLz6zteqkuMdOszWt
kzt+QNrLePtbp7CwbAWgXkj4WRPlpnndz0POuRklaAyYhEIAvvGmqRctrW0bJzz+B+DrMv3NvbKc
7J3ik67yUn2qAQxGws/1RCOHzkC0IxcWLjIaeR88VsH9YRvknp9NJcJlxHaIsuUXYmEsR6N8TBv5
ZVlybGTX1pF2/2EU+pstxyH3/mzE4iX2WVtXcPs/ZmNXWBGUPez8DIzIou164EzCq4ZmQrd6zm1O
8QizCVcr09/7ldn+8TSxNUJL7w714htbs8aLQKcib0Mnt7yGwsL6IW0mq4T+mQpATVEc+LEoDu0u
tCvk5JYrbYEW4OJP7jiN2vlTbkeVAMqVriHGuef2iC7pqia2reP7NkJu/mOAyyLwOw9BeUXCXMzf
Q8lnh1R4fxiERJhsuc+MgvXruJAbisJ+NkCJg0/zS7Ok5roP7dzfZYudugjH/a0PPVCH135UTzGh
HBhiY1hYyN3aNgYeEoHvejkwzuaIRnV8j3TegmP90owsE0m+ybBKGf/mp4E7Ex4bQG1hyMmeZCLQ
2SZWOLllrq1ln0IazVFIs0lLJgLQpig+coZNEj95e+eFc1FS1oCZwN64+JMLWgInJzw2nfJP/JZM
LJgKTPEuEVmW5Ol9zwQ2sb/zivj7qmweeSLFguEOYPcE46ESaYzKR39Zh9eONRHg5Dou5KqAk5B7
cUsUx/8EquLjRJNVkOfXbXbOLrLFTAPqFsp3kvWhuizcv7S+26jCz8kkG8vuSHGtPWFjYlURv+ec
uDkiX7lQFvslGlmmmG2YSIcy/93/srVAPKfgXkC54BsUQhwfavd3vApgvvjJ5pGZ1s6bpntBKgGo
I/BHe5P/IFcjTwqZH06gZinOBcCBuMtsrhgErJXw2CMofrKcaZHksdlm7DnRZNU8vGdfFLL7KPBS
EX9bJ+B55KqajCAfxWnkTwgrJdawv7/U8fW32uIz2+SuK1AFGKzfHIZEw0OQN8mufmoix67I4+Rg
G98Ps3OHncsVWb5fe+s7t9bx+/xi9uIafmpYYmPanwkPpfuHjY3FFFhfQOF+V1L/XHPJ6O5dIbLM
IXl+pmZl/rt/s7VAPGvbmsGpP1/ZWja+eMe/bc3r5J5q4FXkybkZ8CdS5JoLE4B2AU5HMcq34Ynb
8ske1MySvgLlbnjfmyZnbJ7ksefK/Dc3oHa+I4Bp3h0iTTrVvi6VFv+NykieX8TfFeQkCUtMPBlV
57jVu8D/szEKy6grnyKR+5g6GBHtiHmNPIGSDP6EYsxfQ/H8LfwUFZ0WwI12TlYDfkTFDp6w59vY
ucx28+4463uf1sMQHY/nj4rnVhvjJoc8v4+NkcVss/PNBr2mDq9NNzdt5l0g0sxI8lgzolOxLl88
Xwc7zMmc92w+id+EuA1tMDj5YRaqJP4J2nzYJWyRGM9qqBpLO5sAvvV2zCsDgAepGa/3FxTL5+SO
xDjmKcDICm2L2d4dIkt7YK80x/ye5XvuDxxghn2xPN6OBT6gthdewBcoX8i73gVqzQ8/1/M9bka7
bY2zeM1iVEK+R9xjQ+0cPYdCVc5GicQ399NUNLa0c3CWnZNn7RzF22097FwuyuJ9G6NNqP/U8/v9
TDSSHEeJd+0cfRHy/Fo2Vh5bpO83DqUiOBDYL8vXppub9qb+pead/C4aE6mm/CM/RlI7LUJX7w45
5Sngr3H3m9rad4A3TV75Bm0Ad0DVJ2skdQ8EoEa2UDgYeAbFkS/1tssrXZDrYbx71k05MLqc2ixP
uD+Z2nG/5cZKFEqYyCLvDpHlZNKHgGVTyac1qu7yBeE5KPJJUFXjfqBVyDH3k7paTiWzDvXfhHkR
hW8dmsVrAi/BxPCdGUhM/LuNqRvYYvWflH+oQJRoZm3+np2DFXGL9kQvjOAcTs3i/Q9DiSVfrOf3
HGZ92KlJUN3w/pDnW9lzN9sYWmhuQ+GE19sckinp5qae1C0nmVMYkoVdzzdbspyZTm0BaJl3h5xz
s61xAzrZGriLN01eWYpCe582O/AALDdfAxTveKYtFm+gfi7nTmY0Au4F1o177FU8OVa+GJ/kgqgE
kiVdnOvdIZKsS80k8GGMyOI9/4YSsf6V7HOA1Jc+wFvUrGqYeA2eiTwNFvjpT7rI74ByFNSXW+w8
ZFr1MxDHw0I2rkIuxWPQTt5FyNV4sJ+2vLMVCsu6yNp+DLATcHXI8ZslnNN0NEB5A27OwXf9DXl8
NPHTVosFNvadmcIeOcPG0D4F/m4rkIff6lnapJnMTefjomBUmZ3ksUrYMKymtvj1u3eHvPA3W+vG
27334sUCCsFY4DqUj+lsoF8DFOv4MPAmnuS5UFyFcv8EjEIVN5Z70+SFxEpqHaiM/BXJqn35QBs9
WqJyze3SHDcR+C7D91wPJRa9zRbnhWRPW6RuE/L8eBT/7d6O4fSwBX4uwvaCkOIjMjx+QpzYEMZ7
KPzrabu/Ecpbd2MG/djJnnbWtu8RC6t60s7B+ylet3Xc2JEJR6Ld71yEoY9HQuaqfvpC+Y+NhWHX
+TY2lu5Z4O/1CfA/m0MyFWy+I33lxvY213mVpehRqTkjW1C72pkX4MkPy22tOyrusT0o/4rMUaEa
eB2F323WCIk/TuE4EsXiBSxBbrGTvGnyxjdmYPW0+6uiihS/lvnvTraL0da7Q+SMrvvJrOrEx2Se
BPo6tOt/aQF/SwPkmXAx4d4m7wFHx4kMTnK620J8fg7eayWKAz8XuVyn2+gJxsVNkffBmJDjpqGw
8VOAa1G4yFkod8g5KJzcqT8H2vXc2+7Ptfa9K83r+qCy3iQY3GFUoUpV/87R955nfa8HHuKZindR
PqeHULL1RLqicLx/ojK/hQrJucj63r9R/p50zAQ+QyEGqRhsc94ReKhNlGiboQ1ZjnNtvEj9Gwpf
dfLDJFvzvkYs/+05KNz9kVL6ITvvvHOpnoMpwIMNvC8WlPWp7Vp9kS2KnPwxh9hONTboVEK4wtgk
j3nlnmgZXE8DB2V4/PMZHncwqjZzHoXbweuBXHsvJVz8uR6FDrn4k541yG3C9qdRSOjxGRz7O/IY
aUlmJd//h8Si9+1+b/u8V6gZ5uxkx7rWhk8TE3/eQ2Fdd2Xw+l3tHE7McCF3vPWRp3P4G2aQn5Li
5cYEGxuvD3m+gY2tr1IzOXs+mYZE472ymKOezfC4g6yf+YZUdGie5LFKEG4HU7MQz1O2ZnDyx3u2
9o3nZlsjOwXCBaDC0RS4nZpJn19FO6dO/rmdmvlvDquA3/xjksc6eVeIBOuhBLr7ZHj8eLRjko7W
aMf2I+C+Av2WXVGi6TCxYJZdb+fgYa6Z0pPceP/EczWK/U4nAs8iVn3skAzf+ydgZ1swLrTH9kDJ
ZK9JmPec1HS0NhtCLFR8gbXtztbWmXBY3DwwK82xLaxvXJ3j37KAmOetk5rlNkYeluJ8pRtrc819
NpcEHn7peIXMw1b3sTlwAz/1kaBzhjZkuXF43P9zKU7BjErkWmrmA+po67Sm3jSFwQWgwnEpNXMq
zEAJAJ3C8As1M9DvBmxb5r/5R2rvZHTEk3IWkyrgVJTXYcMsXvcwmXmEnIu8BQqRUL4RcIUZ/WG7
0kH58Cf81GdF9wwW7dnyKirykMm8E+SN2p5YHplMFrDXIm+gd+KEhXORS/0pPvakpIm10TBrs0Co
e8fa9FoyT+a+Tdz8lkkOsDOtb7ya4980x/qykzlP2JgZloekh425V1CYnH5/y2JOmU12aSU2tP55
qs2NTvHGng5Jrt2fy/x3b09NMfUGyj81RJQ409bCAVsBl3izFAYXgArDNkkmzwt9oCk4VyYYwxeV
udExMckE3hkvu1gs1kFePLcDbbJ43XQy25VaG1US+y/aJc4nfVGFmguBhiHH3GFj3w9+6rOmC/nJ
j3EpcALpw0jeTTDSsuF75KlyKrHqUz2sP3wB7OuLvRpUWZt8YW3UI+66P8XaMttrKL76XroQ8x4o
/OviPPy2JT7f1IkfbOwMG/cb2tj7FvkPsfsCFRP4u80x6biDzKvOgTyLbre50SuEFW++6ZykD04s
499chZKcB3xC7j0gndT8auNYPOcSXkDEySEuAOWfFsjzJH6R9AbKm+AUliW2+AnyIeyISt6WMx8l
3G9H4XIIOKItSuA5hNqu+0tswZyKm8msHPjVaNfu8jz/nsNtUbBdyPNzUKLn04iFAznZ0Z78JHv9
CoVdpOsjXxBLHHwAyZPTpqLa5rgByCMgSDw9AOWyej9F/6kkdrDz8by1TdB2D9n9O8m+Ouv2xBLx
/kJ6Mfhy4EPg6zz8vpXWl53sWWhj6NGE5yTZzs7v4Xn+Lpfbd7gyg2N/A25JMzb8QO3S27vaHPlP
PDdQoelB7eqNn5T5b/6Tjb/YmuCEJH3SyT//Q1XIAxramtnzleYZF4Dyz3nEyrYCLEK79E5x+Bk4
lFh4xdVkVoGpVHkjyWMDvBsUhBZoB3848jZrmcRQPh6YnOI9hqPyz+nYC1VfOh9l+M8HbYG7gUcJ
z+nypV1PXl2yfrSvw8I/Uy4ykSDVLtsC4GX7v4q6h5tMsAXsTqgaY8A2yMvodco/FDcZ29pvf5ua
IXbfWFsdQ92SpQdhmYGH1aukFmG3sb5wUZ5+ZzXZeTs6tXnYxtQvQ57vaGPy3eRPOJlsdusBxPJS
peJGYETIc1WoEtCJ1M4X1NL6YhAy6ovAwpDMJnyzjH/vYJRnDVsLHEz5h7tFmfNtbRww0NbOTh5x
ASi/bAD8NeGxOxIMYafwfIrKmk42A+MhYPUy/a2fUbsa2EbeBfJKIPwMs+t9tYTnl9vja6ESvzuE
vM9K4C+2GE9FU5T4eQhwT55+09Zop/nEFMfcbAvb770L1JvGeXzvSbZAuyaNDfAQsaTdg6hfXql3
gc1RmfEpcQvBXVGIUqUIQYHw85799kComWJtszk1w++y5VxiGxrL7Bymsv+usb4wKY+/ublfzvXm
e+s7N6c45kQbo7fO03e4G+Ulupb0iVrnoaTiYV6MO6CCFGvZXJhYHKCXPe5CUGFItAnHmu1YjqwO
PGjj0iS0efa5d4Gi8g21w13/iieIzysuAOWXy6i56z8ZuM6bJRJ8gnY+R9qE8JwtxsuNecALCY8N
ojDJIyuNLrYAG2GT2RpJjnkT2BK59m+NhJsw/kUsmW4qTke5E84j92FDTZH7/7uE53+YgryPzkJl
pJ36s5LMqu7UlVvsM85OccxQalaeu5T6xeYvA241o+4GYp4p8ULQe8CeZTY+NbLfFPy+eOFnobXF
BtY29cn7tA01E2i+SngiYeIW6Lfk8be3JT+hjJXIYhtj9yPcy3NtG6svJ/fVdFba/LYuEmXS8Y7N
YWFchzwxTrO/yea6NWwuHWGf7fmk8jM+JXrBv2C2Y7nRDYXbrg58h4TIT70LRILrqOkN39LW0E6e
cAEof+wG7J/w2I2Ud1K1UuNHFEP/PNDfDJC+Zfg7H6Fm9Zh1UBlyJzdshJJY/oB21JN5k31hY8Ku
KA/LGsD9KRa6L9qCOx3d7bhHSJ/stS6/60OUKDHse74GbEJtkdGp/2KrQx7fvxqVnD6b5EJl/JwV
0MT67Gr1/OxpaHdvYxS6Euz+V9l4/LJdL8dSO2yylGgFHGe/5WX7bYHws9x++0Bri2n1/KxewAPE
qqxVU7PqZSJrmpjwF/IXagguAOWDF2zMfS3Fgv4fNnbn2tv3Xeu3l5NZdbdLiYWSJvueD9j48yUK
fdyD5DmrVre59Qeba92LOXesR83k28spzxDuvtZ/NwCeRR51P/rpjwwTqZ3uYH+zm5084AJQ/jg/
4f545ELrRIvpNsgEO1sfAZuV2W8cgsIO4q97H1TrRyeUv+cDa99TqZ3stJqYR8NgYvmYOgFPEZ6M
+xuUkDCThdM/gGbktoJPI7sePk5xLSwy8WBPYknVndyxkvzvdn8GPI08T8J4D3gy7n4fu5+LxL4/
Akei3edEAXEjJDaNRF4tvUro3PWy7zwSuC9hsVqNNhwG2W/PRd6JDjae9I577EmUaDuMW+zc57ta
YFdcAMoHv9vYezY1c2fEs5mN4eeSW4+6i2zO+UeG49gJKJQrGT2s73ay+6/ZXLm3jT2J4mR7m2uH
2Nx7fNxrnbqxW8Ja8HW0SVVObGHXwtp2PRwIzPRTHznupnZesPO9WfKDC0D5YW9q5zP4rw84keZa
lH9hlhnFJ5XZ77sh4f6BeCnmbGmGdinvRTuR96Kwi8RxdCHwBArx2gGFYgQeWB1tsTsg5DN+snMz
I4PvszFyn78MGJ2j39jfFo7XEJ67Y6gZ6TeRX++BSmYFcldvmOfPuQiJOqmqIV5KzSpEm6Md1HY5
+g5DUFjLtkgkrU4QUy5F7voP2/XWMILnqyHy8HnYvuul1PSUqrbfth3acBiSo89tBzwDbBr32Gxq
hoIl8kc75xcXoE26UdP71MkdgZfXoBT9qbmN5e/b2J4LRqNE46fZHJSOadbnw8TOAUgUbRs39r1s
c+c2NpcuSrJ22SZhLt7J5mgnc6rM3ojnxjL7jSehzY6ZNndd66c9ssy0tXI829qa2sn1xf/mm296
K+Set2wyCpgCbEj+qvM4uaOpGc8XAI+bkTO7TH7by2jXMGB7Uu8SOwqp2MSMpP1InSz8VxSK9QAw
JsnzHVFoV1jVuZ+A3UNem4zXgX7A+sD8HPT786zfhxnRy1Gc9mV4rp988y4S2VanbtWgsmGX/2Pv
vMPkKss3fKeTRholDQg9hYSEEAFpEQSUDoKAIEVBsSCiIgIWrCAKIgoqFhQUBKUKSG9SxAApEEiI
hBhICCEhlYRAyP7+eN7zm9nJzGybcmb2ua9rrp2yO3vmm3O+7/2et4VwsXdspvLx5Twbg6eAoyh9
BNieKDXpEPKLPZNRdNBNFRibphgaY3Aijbt9krWZvR24hNK3VR4aY/CBPN9VoWLBI1DkxAmUv8vP
kBALHgX28yVddrvl2xSP9nkHuBAJQm1td90bCZ0v0vxo4pHAP4qsoQ8CHye/82OruMaOp3jK6mwk
Jt2Eolje9alRlIk0Th2/ExVFrgf6onTBY+O8/y5u814LDASm0DgC+r6wU1LFfvvV9rLmCKDSsweN
xR9QOLbFn9pgDXAe8pZtgbpvHFEnn+37OQbRp/1156UX8j7+HLVhfyw2pPkM1yVIKDwY5ZZfQH4B
Z3AYV4XEn+dpmfhzBKondC5tF392Rx6y71JY/HkRCYbnYvGnEixC4uOQCvyve2ONuprC0TWXoZSh
bHZBKbO7lfh4/hXn965IUM31/o+La/MFFB1waFyzlZwfDo3//QISxnLFn9Vx7Lui1tmlFn92I3+6
8t8pLP50iu/4RirT4nlonMOLfTlXxG45P+boQiLuBjHHPxlzfltYgVIzDkCOkebwQtjGhbpE7hNr
ZL7aQrNjbR0da+1fY+3NZatYqx9DaWeXxZj09CmSl1NzzqF6Kbp7RJxnW4Qtfx4Wf2qFBTROOwc5
EPbw0JQWC0Cl55Scx++jiABTWzwXm/VzUNTDrShsvpZ5isYdX45AXjkjb8NRZOqOPAB8CeWM56bK
rUS1Cj6NPOrHheFaSBgZg7xsuxR4/eEwjJsr/myAPFqPAte34TP3By5HEQHjCvzOutjgJvUkTGVY
GD8rVaz968CGFO/a8/mYF7MZhjz3Xy6DPfE0ilQZG+fpopzXN0QRA7fFNftLFD3UrQzj0w2lnFwR
G4vb4n/ndmpbHJvOHePYS11Lo2OM9YM0rvkDEqs/V+Rvfxhj9vUKnVOjc85lU36Sum0/o3DtpXEx
519O2wrNXx//7yKan3r1ChJ6Hinw+i6xVhZKV3sn1trjwnY5NdbiXCdIBxQde2ZcKy8g8fNjuJMY
WWtLtnPzF5QuNbVabBm2+k/Ddv9gnjXLpJ8/sX7q8CkeltIbE6Z0bML64ZMPU7wVq0k314Yx8ixK
ofoumVz1WuR7WQtiT9QJqD3SNTaWZ6HaHDNRMcqTyN/laCmq5XMaEn0ORHUHmorsOzwM2u0KvH4V
ivxpSYTgaUiYOq8Nn/9YlEpzBoWjPp4D9kUe1ZWeCirK61mbtUqwChVr/WKcj/l4EzgamJvz/Aax
4bwHpSOWmpdiIzcq5quZeX5nc1Tb5tEQQi5BYlD3NvzfHihF4pJ4z0eQCLZZnt+dGcc2MuaUWWUY
hx1ijH+WZ8P9v/huFhX424/Gd/up+K4rwbicc9lUhpUxZ+8T520+OsXcPznWgtbyjViLTmvB37wR
5+PvC7y+XayZhzfxPgviPQ6MNfm0WKOXFpgfTkYRcjPjOjorbICu7fQ8+WrMccla//0a/iwbhm17
R9jqY8J2N7XJM6xfnuLg2GObEuEaQKXlhDyTzmeA33po6oJBYVjtgmpm/JHazDHfA0W4dEV1XfYG
nqj3uS42bruGqLE3aoVcTAR/LQzRO2MxaolI0xnVkjqvwP9YFRvGX7Xwc2wSBuw/UE2E1mwif0Lx
ug1rYtP7wwpuFk1jTkEC478pfYpVMb5OpkV7odo+O4ehnc+TvjrO6Z/EBq0cdI0N5GdQ5FyxDdwr
qH7AnTHHLWrivTeK+fGgeO9hRX733Xjvq1A9rnKtBQOBs1F0T/cCG+GDKexoGhqvXQJcXMFz6d+x
Vp4EXONLuir0QGm7X6N4lM7dcY4934r/cW2IMMORSNwSPoeiNXrkeW0d8CPkdFvbgvfcFIlfB6L0
r2JptOuQWPtI2ET/Rl2I6r25we7xmTvFvLUvtRnh2xUJeyegCPdLseBcL5wWa2s2n6BtUe8lpdZr
AFkAKi03oLDwhGUoDPpVD01dsSUKw98WhSre1EIDJQ18kUw62KNhMNVbt5aBqAXzXsiTP7qAoZkt
fExFIeP3onDo1kS+bIXaWX6owOtTkCd+cive+xLkuR0BvNyCv+sfG4EvUDwq4nGU+vasL/OqslcY
6CuQaDe3gv/7njhHPlRkThiB0qC2LfD6W8Cvw4D7XxmPdQQSQo+h6RTdxbFJuDfmvKQWySgkCO+H
0mcGNPE+s2Otv5bC9VZKwRZI5Dqdwqk6M1GkxIwCr3dCIvZqVK+lUmweYkJvFInl9NHqshNK+SpW
+2c1SnH8Efnr6xRim7gOLkMiUksZh8TusQVefzA2g63pdNkLdcjbP2ycHSmeIroKRcM8HHPEs5RP
yK4WyZywZzw+A6XP1hKdUTrfSUjAu4zmp9Cb2mCzuBazMy5ujLU+FVgAMtkbrGk09jb8E3khTH2y
DaoDMxTlHd/F+gVL08wvyLR//gbqDlLLDESRC3vEbTTF0/UagDkhejwQm5SXaZv372Qk0uTbsL2H
PFTfo3WRNcNj0/oDmt/CuXNskC8gf+pK9ub4O7Fpd9vm6pMUoO8Rhu7NFfzfg2Pj81ckdBdiEIqE
3KfI76xE0ULXxIbq7TIdc3ck4JwQP/s28fvryAioW9N0OvxS4P74HPeXcZ7vicS/E1FUT7Hi1vcD
n2xig3oZSvHZCZhfwXPoSOQYWYXS4v7nSzoVG//TUURNMZFzbvzONTTfsfV9FO06gsLt3ovRI9a0
rwBd8rz+FopM/GNb9jths+2BIl52j3m2Q5G/WRab0Mfi9gy1Lwh9A9UQBImCZ9bQsXePPdXhKEL1
96j7qqlP7qJxSvo8lN73VhoOzgKQSZhI43aKoBzjyzw0dc9QlFLzcp5zIM10ROGUH0fFFfdHnWVq
xZjdHHkPd0fF/kbQdH2m+WHEPRQG3fMl2sxtFdf6IQVenxSG1pNt+B/Xx2Z7OM3z0H4YeXMnFPmd
BuC6MApf86WcGrqiSLERwG9i41ZJ9kWRQJ9B3vlCdInN4jnNEFHmopSpe4H/xPlWjsjJvqgezhcp
XEy2uUxDQvnfYjNYajrH+vGBmH/3I38NsmzeR4V3L2hi/D6FIrAOQAJ3Jfk18FkkYo5F4rdJj71y
EUqnKCZ+TEKizv3NeM9+KArtftSqvbXshrrYFVqz/oFE6dklGIfuKLpyz7Dfx5O/A1k2y1C00xPI
cTQ55rVacZrsHfNvVyTwH0/hYuFp5ENIsL/b9kq74Muo5l3uXvuRNBycBSCT8HXWj6AYj1MpTLrp
jsIqD0aelIlIZU8bvVG6yfgwEndG3rxi6UzrwkiYioStRPBZUcLj6obSqr5F/qiDN1HEzpVt3Ozu
HsffnJpi42JTfkgTv/d0zFsP+TJIJTehSIqXQsiodBvbs1HNmL1R9E4x9kLRbeOb+d6rUMj+9NhQ
zYlr9Q1gORJl34nPnL1JbYjNS7e49nuiaIaBsXnbPDa4g2J+GNLGMZiHUgxej/tzkYi8AEXNvR3H
ugbV0sg91m6o9kqPmMM2jeMbhsS9USh1rUczj+eZMIofa8b38Qiq/XJJFdaUqTFf34S6K5p0bqYv
jrW0GP9A0aFNpSyfjup/7U7bagp2iTX1fFSTK5elKOLoihLPib2RILQHEoXGxvxRTNheHXbT08i5
83Q8XpHC73soSm3bGkVlfpzailg37Y+dWL+23TlUtpZdQSwAmYTc+j+zw2h/20NjUk53FO59VGz0
DqK6XZ86heE1OjaUu8b9wU0YY+/GpvJZ5J17KjaX5boG90ei79gCx/I7JP6UoijhQ2EM70Rhb/r2
KJLnBBRZUIj5YUD/jtqrXdWeyHYqNEeEKQfXoJD73Wi6s1VSkPNcihdPLkYDEoeyhZV8AlAiqvSk
cBe7cvN+HOcqJFYVEoASoaoHxSMuijEHpW38kaaLTW8bm9G7aF2h+LaSiE8gEfGnvpRTS2fUSv1b
FI9+WYvSPS+kcIpXl1h736R4WmhzGQx8M44vX1rYlNgMlmsT0wtF2+6CRK2dkFhbrOj8ulhfnwv7
Y1Lcn091o4R6x3ywB5lupxZ/TNrpiaJwt8rZax+bhoOzAGQSptI43Pyu2EgbUwt0QoUAT0f1jI6h
ch3O+qHWr+PC2BqLvPe9mvi7xSjs/GnUvWMyEoDKfdyjUB2fIwsYgLeiCJxpJfp/h8V7Hoq8sbkM
Q7UTTqV4RNRK5DW9mJTkUJui7ElG9KlWrYYeKHquF0qzXNzMjdNRMZd8oA2iR7lYGBvVh+PxxNjc
pa3FbANKlfs1al/dHFG+P4q+WImEmGp08cuuLbcXtZNW3J7pjwTnz4dYUIhVqO7KpUiUzOUQ4HZU
o+W2Eh3bjigC6TDyO4BuRvWDppd5jLoiAWgcckp9IOyWporGr0RRQVOQKDQZiWhLKvTddkVR3ofF
XPJFXOfP1A65dYCmUrhgfEWxAGQANkapJdkG5IUof9qYWuIcVB/gduC4MmwgNkBq/miU578z8rJt
2sTfvRMG51TkVXsa1Zd4s4JjMxRF2Hya9VvqNqD6Jt+lbeHvuSRe1YWoLks2w1CdsVOaMNrfRXV+
fogLJtYSfZGIuBlKPRpDeerQNMUwMtF0H4lrsTl0jOv76Pi7EVQ+WucdVIB4WogpT8Vanbv56hfj
OyE2dmNQgdgNKny878c434089U/T/BodG8TfjUBC+px2fM6a1rEViro5nuKRLiuAq1F9jtzz7MGw
iYtFq7aG3ZHQsx/ri8rvIGHqIipbG2ZjVOR855g7doz5sql54w0yzqskSmh2C+bW5tIT1fo5mPpo
9GHaHxfGuZuwEKVqvlntA7MAZECpF9NzjNtTY0EyptY4BIVZTkdRbAtb+T6dYyOQGEjjUfTMZuQP
6c7eBL0WBtKkEECmxYaiGsVEN0Wt0T8XG8XcY70bpTk8XIb/fSqq+bNrbF6JDd5XUZRWsSipdcg7
+kPkfTS1x3VIiAUVbb2+SsexB4pGug1FvjW0Yi4YGZu4JKVzWJ7rqbW8E5uqeci7/gISembGcy2t
FdINpaFuH8bmSOTtHxLzQamEoSWxgX4ORTE+Hsfe0tTMDihK6AgUdVOttuvHxTlLnKuf8CVck4xF
DswjKS7argiB4WdIuASJj/9GjpI/lOHYJqJopf3zHNsSVIfo8pgPKk0XVIdsDBLAJiAH19AmxvG9
sG+mh73zdNxvS6H8TYA7Y+76eNw3ptb4NCpXkG1zj4q1vapYADKg+gi5Xv/9UUSAMbXIMCQCbY68
R880YwMylEwq13jkDduCpgucLojJfHIYPtOQN6za9bMGoE42Z7J+esi7sRm+JEuYKTW9YzP4T1T8
eTdUAPaw2KAWYh0Km/0Rbes6ZqrP8aj2BqjWxQFVPJaPx5xwLaoh0dCG9+qIvOebxVyzGSrk3B+J
Qt3jPO4f80BSC2hpbDoXI2F6QWySXgcWUf7iq71RLa5BMd8NjLlhQLzWl0ztn54o1bIjqrexJB4v
AF5Fws+ryJPZlk48HYA/oZbwx6B0j2pxP5lIxWPjfDG1bdueh+qAFau/tybWw8tizbkKRf2NpHz1
BHdBjpDDWD9aaSHqJvYbmpe2Wk56osiqMcgRNo7mRT2/jUShqWF/TUFOsXnNmHsnoELPr8Sc8D+f
yqZG2Y/163x9MA22rQUgA8pPvCvnubExcRtTs/MDKg55Rvz8ddZrg5B3fBwSenZE3SV6N/Gei1Ea
0uQwaqbG4yUp+twDUZ78qXmMtGXIs305GY9nuUiKAJ+DOrbs34QRvhal7l1CadPQTPXYFEWHbByP
96R60R2Qact6BZk6L6a6XB5z9FmxAa8We5Cp97MQRXkt9NdTF+yG6swdTvEGA+tis/ZQrF1fB35S
5mMbFXPRcUCfnNcWoOiBKylNM4ZS0Q/VOdwROcvGxeOm6gktR86xaUgQmoycZ9mf7XRUo/CXqNlD
g09fU8PsyPoR7Acix2hVsQBkAD6Gwq8T3kdh4zM8NKYOOAR5/R9F3vYJYaz0beLvlgAvh7HyLBJ8
XiYFubsF2Aq1nz0pjyE2B4Wz/wF54MrN9qiWwuCYT4qFj69C9UIuw6le9chfyKTS/I3G3SarQZKT
f1mIDqZ6/AyJcheh7mvV5EZU7yk5Z0/w11N3jI3z7WiKR/Yma9Z81BGsEukaQ4BPIcfN5jmvLUZR
clcgASWNbIycaONR+tiYeNxUmuxS5ESbhKIS94pr7w6frqYOGInSubPrfh0F3FTtA7MAZJIN8u1Z
j9cgAcgFV02tsSlqIzwWeaXGhAixMcXr9iRiz1TklZqKanG8Sfo9UBOQB/Fo1u+i9SSKfLqF8qeX
dEXtvk+OOaWpaKo3UZvuX3uuqWsOyjLm16IooH9X+Zh+GxutK1B9rHX+mipKRyTAnYEiHE6r8vHs
iqJ/kuiQVHhoTdnYBkWanEgmOrEQK1D3yj8Cj1D+Lp29US2s01HkUjarkFB5JRJMUr0/i7HdDkVB
JNHWTYlC76F02HnI+TYZOYZmUZ26SMa0he2QAJS9/yjUEbeiWAAyoMKWuWH5o+OkNSatDKSx2DOa
5kX2LELevxXAD8Kom4/qW9RKuHFXVKPgDJRelR1hswy1Xb8qNtrl3tyOQlEdx6DIn6aYFhvwv8Z3
YeqbrqjO1Nh4fGtscKq9OfkLSrv4E/K8WwSqDB1RJOJJ8R18MgXz7q2oFgux4dyF6hTsN5VlI1Tr
6TTkLGqKmagu1I2Uv217RyRMfgalrmWnh72P0tR+gZo4vFsj490B1UUbjJxF30SC16r4LoqxFDmK
nqOxKLTAp7FJMTvEOZvNHqhhQlWxAGRAXXmm0zhEbS8y+fDGVNtoGIzEnTFkInu2prjY0xACw3/J
pHE9F0Zcf1QTYEhsQm6m9d0qKskglE7zKRRams1U5KW8EQla5WRLFOVzdGyWujTx+6vCUL0KpYZ5
c9W++DyKtklIQ4RFNyRCHo7CsT+Jih2b8tEdpeN+DIkux9LyDmelJrcG4udoXC/O1D9dUKrXZ5Bj
panGD+8hUftvyJP/SpmPbzBysJzM+kLVC0hQvY501QkqROe4/o9HKfmXIufb9siJl6SPbYNEoQ5F
3mspmTT9yfHzv2H/uHaQSQN707jDbgNymr5Y7QOzAGRA9UKei81lwsnIM2pMRa/pMHaSblxjwxjY
EtiwyN81hPHzEopcS4yB2Si9q5AxsDNwNurkczNwNekr/NkRdQ34FIqc6Jv12lsoveZqFMW3tozf
y3DUxekQFJrevRl/Nz0M0+srYCSb9NI3rslh8fhp5AWr9uY/WwR6EEWyLfbXVTY748bYaN9KOsSf
bjFv7hyP58S6s9RfV7tlSxQZ+InYqDXFapRq/Q/gHlQ7s1ziQ+eYN09B3U3754ghtyAx6AnSF9G4
SdgwR4Zd9tNYBwrZG/3IdB8bhyIptot9SjFRaHnYGtmFpl/CopCpDichx2zC60jorLqdYQHIJDyG
UsESLgG+5mEx5bx+ydTsSdqLNkfsWYvyw2eQCQd+IYyK5a08ltGoOORuqNjzVShEs5pG1KYowuZk
VFgx+/M/jiKXbqN8glWv+F4+jISfMazfrjYfS1BR+eviOB3tY0Atj3+a9fgbqNNOtckWgaaiAo2u
SVVatok5YUfSI/4k5+CFWY+/FraPMV3CJv5EzAn9mvE374bwcA9wfwgc5WojvwlKWzwRpYpldzeb
hOrr/Y3q1s3phASrU8OGeRIVfm9teYkNkSg0kkza/3AUyV2su1u2KDQ5vpekppBFIVNOfhq2T8Lj
cU1UHQtAJuEnNBZ8nkRRB8aUioHIgzM2a/FuKo3rHeSVnYm8OdOQ2PMq8HYZjnEoCgM/BuXZ34xy
/p+vkKHQFaVfnogibbLHZmZsov5KeepzdUVh2HugukK7xXg0h/lhTI2Lzd0NPt1NHuN9UswBxMZo
97imq022CDQfRQI97q+sJOwRYzuEdIk/Y+I77pU1v06g/MXyTe1xHHJoTEaOmcHN/LvXwpZ+CDlZ
Z1Keej07hM1yNI3r8C1FkUnXoC6olagV1CHreI5EQtr1yKn2Whn+X09gMyQKjQn7cnsUbbpBkb9b
itLHsmsKvYRrCpnS8m9UJiHhx8jxUHUsAJmED6EQ+IQ1sZl70UNjWsFGsQiPQZ6fHZCnpk+Rv1ke
C/ILyBP/HIrymU/lixx2j83g6SgK5jXUKe92VEuo1OLTyNh0HptjwL0G3BkbqCdKPA4bovpfOyOx
dwKKvurczL9/HXggNnX/RLU9toj3sVfN5OMUlKKQ8C8UYZaGIqbdgD8jb/+quPav9VfWJk5E9XS6
o2iET5IO8adrzF3ZntiTcdq7KSxqTAL+h1qUH4iib/al+WLQWhSFMinW8qfDvl5e4vP6g2FHHERj
B87MsCNuDBurlPQKO++QuA1BkdS/QlHKq6twfQ8Om3M0ijwciRyOxaLLl4XN+Xwc/7QYNzerMK1h
BBIXu+XstR9Ow8FZADIJnWNRmpD13AXAdz00pgn6xkK7Q4gJSVhuP/Lnaq8jU5z5eeR5eR6F5L6J
Im/SxLZh9B0X95fHtXIvUvdnoLSnljI4jKVPoEiIpJPXYlSU9O9IlG1rCHmH+I6GxYI0HnnJRiJv
Zodmvs97qKbPg0jweYqMt3wi8nIeROOCqsbknosPhBGUcBFwbkqOryPyVH8669jOxx3CWjOOPyTj
6fw9iqxMyzj+GPh61uMHgP2wcG0KcyByxkxEnUNBHax2QYXE90E1g7o08/0aUNTsC2EDPYMEoTko
OqWt52LvmGePjuMbEM+/jyLfrkMOrdYUju4XNt6uwP5hv/RGETTXofT0tKXRdkJt6bcNW3Vs/EyK
TXcs8B0tIVNu4OmwVWfgOmGmab4LfDvr8SQUWZ+KPY4FIJNNrnd2LooCestDY4I+KIVjFI3Fno0L
CAnvoQiexKsyhUy9nmU1ZnB3RJ6kw1EXi6RA5DthHDxDJox4Thh3q/IYTvshD92+ZLxRC4D7UMrZ
g7TcK9gBhTtvhISlzeN7Gh4Gz1ZhAHZswXu+H9/Tf+KYHg+jLt/i9XD8nOhLxDTBzijyJzs8/yjU
iSstXIyKw4MEzU/j1IDmMhAJPgdmjeU5KTq+o1EUBFnz954ULkhrTMIjYbNMLCAwbBNiyD7AB2Ld
7dSC91+HHECzkUNsRtgTc8OOWhTna0vtpg3jmI4M+2NgPL8ciZ9/Dfsj15HVAzmJhpFJ3x8fdl8y
f0+PuftWFLldS2J5h7Bpk7pCY8lEqw8mv5jXgByV2aLQ9PielvkSMUF/FP2zec4e+49pOUALQCab
zsiLnx0W/U3kyTPtj95h0OwQi/6OsTAWihp5O4SPGSh0dmrcn0v9tVfuEEbRXqht7F40DgVfGwbb
a8jD9j8k/hyEijcmPIfSTh6I+++jdIkuMf6dw4DsFc/3DoNlQHwPg8KY2yTubxRGW4dWfKaVYXQ+
g+oWTKJ5NQsOQp3IJpLxjBpTjG8C3896vBgJolNTdIxfQQUcO8S8drLP7yb5EOpIuEVsBL8KXJai
49sxbJzsgr7fAn7gr840g4k0P9I1qak3AXn9xyNnTK9W/N8G5ExaFPbEQiRIv44cTYtDfFgRttbK
sCXWxnPvxfOdQ7zZB0U1j876HwtRhNOSuH4HoRSywTROC5+HBPx7Yj6cQ/1FznWPjfvwmDPGxP1h
qOZQvu/njbB3p4YN9TxymLmmWPvk2zTOoPlXrI+pyXCwAGRymRCTetLieTkKcZ3hoalreiEvyOg4
B4qJPQ0oKuy/KGR5Six2M8MoWdsOx69rjF+SBndYjF9TLEUeve55FoauKGKnY9b9UpB4GV9BXqsk
131GGDEtXaCeQh6xg30ZmWbSBXmc98567kUkAr2eouM8GkXF9op57QKUFva+v8JGdELpXhfEZnEl
8nb+PUXHOAgJ7SOynnsIdTh0p0LTXO5AEc+7tOIa2TTsgqQ24nao7l5Lo3ObWt/fjZ/J/f/fM8X/
WY0iePo24/1moDo+SfrTbNJRs63SdEbOtu3JpJCNQE7S/gXs5GxRaBJy8s2mfJ3hTDoYHnZxEuG/
CjmJn0nTQVoAMvn4AvDLrMf3oSgH10GoDzZExfBGoBS/cWRCXnMXsfdjQzYThblOQSlcs5CnyDUT
MnQMo+ATcRsUz89CKVI9kEg0OAyGniU0+nINj1XI8/QGikT6H5mw8lfi8fISXNNJOsWuseAZ01xG
IGfDxlnPPYHqVSxP0XHuglIkhsXje4HPIs+30bj8BtUCIeaXY1HqaJrWvH/SuLPpG0iAnOmv0LSA
XVGE7NG0XeDsGOfmFkgIStK1twg7YVMU9dvaqN6mWIcit99CkT2vhO0wMY6FsP+ui9sU7wMa70FR
NOG2ZFLIRiGRaBDrp/81kCmJMDluL6LmJ8s9nHWzD7gbpVpm76mvTNuBWgAyhbgMODPrcZoKdZrm
L059wrDITuMaEYZFLmtiU/Mi8lQkKVxzKE/L9Xqge2wQPw4cQSa3/nnUfvVO5Dlbk/M3/VHa1oC4
bYy8cX2QMNQnDK3uYfw1xC0Rbdah6KHVKPR7KYrqWYyEuUVh1L1dRoOtU5YBc4xPBdMKjkLdobK5
N55PU+j8ENRKeZ94vAg4C6VvtmdOAH6GUk9BtcJOjM1kWugdG/X9c54vxQbetE9uIONAK1c0YMew
BfrH9dUvy15IbIXucT+JFN4w7L4OSMhZHc8vC1sgsRXezLIXFoatkJ2m3w1FMh+EGlXsEM8vAG5B
Tp+nqL/U/lLREwnjSQrZ6DhfhtG4I1TCG2FHZaePvULt1ck0cCGN27z/HPhyGg/UApApxq+RpzPh
S8AvPCzpvBbCMNiGTNvLMbHobJTn91egaJCk5fo0lA40D4fDN0UvVOjxKOBQJOSsQx3BbkW1AWZQ
/2kin4hN8U5x/hjTGnLrAYGiTj9GukSgrqgm0BlZz92EHCXz2tl3NiQM249lPfcL4GukKz2kd3xH
++U551zb0LSWHYFnkQB6fZ1/1k5hR34UNcDYFYlKC1EXsb+jBhFOaypOl5g3twvbfCyKGtom5qlc
FiFRKKmn+Rwqu7AYi0Jp5Qzg8qzHvwFOT+vBWgAyTfEL4Itxfx1SMi0CVZd8ucijYmHpU2QhmY4i
NqaTabnuhaR59EKdYo5DHrG+YfA8GAbQQ6jgc3uhK/JSPQZ8yqeHaSO/I9N6PeFBFFm3OGXHejxK
ke4bj99EHr+r28F82iG+px+RSd1bGjbCX1J2rANQpMI+ec6103zJmTbyB9QwZQfaV02coahW25Fx
bfWKOeAfSAz7FxaDWjKfJq3pRyFn2igUOZTPcbsMOWqTcgztvfZmmjgDZc4kZR1+SWNnUeqwAGSa
w5nAT8i0RPwB8B2cC1wp4WFzJPYkkT3DUQ2f3FDS92MhmI68BVPiftJy3bSMPmHoHAd8ODZ881ER
yFuR16u95m2fEhupUbhAvGk7XVGdnSNynp+CIu1eTtnxjgD+hArmJzyAOofVazTcjsAlMScmPBVz
wYspO9ZtkDC/Y87zt6B0VUe5mrYyPOyrU5H4215tpN1R04uDUd2ipcD9SAx6wLZnq8c1aSoyLuys
Ucjxm1tXaA2KDEra0k9DotBcLMRVgo6o29c34/F7wNkoQjbVWAAyzWV34LdkOmjcgdLD5ntoSkJn
5AlIijOPjsl/WxQ2mlsA8N2Y4F+ICX9KGOGv4LzstrAR6gpzLCqE2AuF396KPFxTsaclif65DxW3
M6YU9ETdZvbNeX4eirpJWwv2DVDq2lez5ud3UAj4RagWVz3QFzgPpYAnTod1SAz6Fo3rm6WBvVE0
0pCc5x+Ijarr2ZlScQVKL2xvUUCFbNgdUYT04XF/JWqA8VfUNn6RT5lW0wPV8xyOov7HoBSyzcMm
y6Yh1s1ZYas9R6bY9Ju2YUvGYJTmlXTAfRFFlz5eCwdvAci0hF5I2fxK3H8D5fz/BacSNfucRcX8
kqiekWSEnq1iE5TLChTFMx2JPYnC/yr2ZJaCzVB++zGooPM61OXjJlSQdo6HqBGfQ4Vfh3tsTInZ
ELiZ9UWgd1HR5StTeMz7o3p5W2Y99yryCl5Tw3N0F1TQ+TsxRybMRnUN7kvhMX8+5qbcDdEDKGXF
nXZMKRmGIi/OjI2gaTw2B6A6YUndoKdQAe1/xhxp2j5HbxZ7iTFxGxV7iXx1hd4m0w32eeRATqKF
3NW3Zfu441FNwE1jj/YzlClTM1FXFoBMa9gaOB8Vge2G6oCcg1r4mswE0RflS28bm+WRqE7PluTP
712LRLWZMTE/F5P07Hjek3Pp2A55g49A0VZLYpNwM8phX+ohykuPMHhvRREBxpSa7sC1NC4wnPCX
2GylrS5Qf+DHKB0km0nABagwfC1xIBKwds56rgGlfX4DdQ1KEwNQyP3xeV67Cfgkjow15eEXYUsM
R523zPr0RTUUj0Tifr+wb28JW2KWh6ike4+BSAQaSSZSaPsQKzrn+ZtFKHvgpdh7zIjv5LWwhb33
yPDBWOv3QNGv16GGAi/X2gexAGTawtYoBeSUmOAfQMUhH2pHE0anMD6HIKFnuzAEigk9hAE9m8Zi
z3+REu8Q9fJ8Tzui0OSDkHfqf8gTdTvq6OFoqqY5Ixa77YHXPRymTHRGRRQ/m+e1l1EUWhojUPZH
KWDb5zz/APA94NGUj/tEVMsgNwJrBhJ80zjm+wG/Cnskl1/HnOWUB1MuhqDUj/Nxg5Tm0AUVOz4U
RV5vFnbvnUgMmkr9d1CtBj1R5kHSKTgRhbZCDox8ZAtDLyJRaBZKL1vcjr6nDqjg+bmxNi5Fdb+u
oAaFn/9fOC0AmRLQB+X9fgaFes6Mi+Om2GTXw8W/IRJzhoWhuW1MnlsCW5A/3HJdTJJzySjqySQ6
l/R5seuNDVBKV+J12hCJbbcBd8fCZppPEv1zPYr4M6bcnIuaDnTMeb4BpVx8M4XzaK847rNQNFP2
enAHChN/LGXHvCdK5z44Z6xXo9D2H5E+x8SAODc+y/o18tbFd3CxLyFTAX6MmkU4CqjlbAl8BEVR
jURpmkk09lOorpop7zy6eexpRpDJWNg8XuuY529WxN7uldjvzQohZA4SjZZTH0EAW6BI5JNjv/cU
cBWqB1rzxc0tAJlyXDAfRep+ouw/itJqpqf4oumOopg2iuPeIhambeLnUApH8yxH0RCzyeTXzooJ
ch5OJ6okfVER0MPI1PP5DxJ9HvV30SaS6J/huPi7qRyHotSjjfO89hryvP+Z9HWl3CE2hgfmee1+
VD/gfqrnRe2Euht+LX7mcidK93o+ZePaETgh5qKheV5/E6Xi3e5Lx1SIwcg54iigtttve2XZbx1i
030bagJg+62y38WQ2AttjxzfW8VtEHKo5mNRrMuvoKyGV2Iv9Gq8tpT0puP2QdFRu8d5uHkc+x2x
HtZV3SoLQKbcF9MEFBW0LYqSWYZEodlILV6APLhLymAId0LCTk/kle0HbILyYwcioWezMCI3RQJP
vvzYNWFUzotjnhuTwpyYEN6Iic1hq9VhEEoDOAh5kJYCD8aE/SwO/y8FPZCn5y+xKTSmkmyFokr3
KvD646gj1UMpPPZDUATNDnlem4RS3W5BXtVK0BtFRX6RxjV+Ep5HXb/+kcKx/BDqvLZ7gdcfRSnp
s33JmApzEapBtT2OAioFXVAL9INQ+k0/FMF9B0pFdQp6degUe6VNY/80DIlEm8f9IchZ0y3P366N
vdIbSCR6NW4L4rYw9oIrUcTp6jLsqzojcWtA7B2GIXFrSDy/Ajnw/x3r87J6/SItAJlKskFMFFuh
qJrNwhjtGZP9uzFBrI2LfmXc70QmxLtT1vs1oG4ffZBXsG9sVHvFcz3i1hMJQR1z/nZlXOxvxYS0
IBaVN+LnPCT8LIlJ611/halhWxQ2vH9M4nNQxy537SoPZ6CisCPjOjGmGhuCb6HUnnxCfQNK7bwQ
RZymbe37NPD1MJRzmQv8KW7lqimwDerqdVKRY7gY+D3pS7vYM773j7B+uheoftuFKCXMtdxMNRiE
Uvy/haOAysGWyNG3f2za5yMh6G5cRDpNdEUCUT8kBA2Ja2PT+Dkw7veP/V+vnDl9HRJ+3kZC6iok
wqyM+0vjd5bFniz7b9/PsgXeDzuhV+wbO8etW/zd27H/exVFKc1Gjv12k3JoAcikgY5IpOkfk0Yv
JOb0Q2GGvZCQk7RQ75DH8E8mDmKCWBOG4JK4oJegVK0VWZPKiphUHLmTfjqjbgYHIu9vd1Q4+z6c
2lVukuif3yERyJhqshuKmtmpwOsNSAi+LOaHNM3v/VCtvC+htJFcVqG0sKvj2Ntad6dXbJpOiZ8b
5Pmd+ahw9VWxTqaFTnHMX45NX4cCv/cMEqif9KVhqswFSOh1FFB56YuiQQ9Abc9XoSjQfwLTcNR3
LdAp1qfeZJz1vWPP1y/Wqn7I8dMtvvNkv0iBfWDSyn5V7O2Wx3NL4/ES5PB/m/SljFccC0DGmDQL
Dx9AXt8xsahPQh6fKdjTWynORMV2R6BIOGOqTdcQBs5D0Z6FmAL8FjUkeCNFx58IQWcgD2k+XkWp
YTegOmbN3dR0jnnzGOAIFGmbj3koUiFtws+mwFGojs/YIr+3DKXWXYajc0062AhFAX0fiaqm/HSJ
eeKAmPc6IxHoblQ/aLWHyJj1sQBkjEkT/YA9UFHSLZFa/xjqCuGuXZWnB2oB+lsc/WPSx5ZxXh5P
/m4lCW+h2hHXo/SwtHS06ofSsr6A0lrz0RCbyn+gwsbPoAjXbLoB41HB7EOQWFsoYmYWal97DekR
fnoij/5xqOZH/yK/uw64FvgO9dFl1NQX3wFOA7bDUUDVWhP2DTtyAErtuT/syCUeHmOEBSBjTLUZ
Esb/nihn+FXg4Viw3/LwVJUvoZoGjv4xaWbn2Hgd3IzfnQvcg8SUJ1Iyx3RH4s3nY+NSTMx6mUyR
e8gUSd26yN+si/n0yvjcafCK9wc+GJ/7IxSOVMrmDiT4Pe1T3qQURwGlhwGoZMDEmF/eRA6AR3An
U9POsQBkjKkG26J27TvH5mcmEn3yebdNdegZ34ujf0ytsCdqa34g+QtF57II1Y55AIlBM6hcN65C
jEdRQYeipgkdWvk+DShC5nYU7fNMlT9Xb2A4Erg+hLqDbtyMv1uLhJ9LSV9xb2PykUQBbU96og3b
O0mU5MSYh1aj1NpHULtyY9oVFoCMMZWgM4oi2QsV7QOYGovvTDKFvE16cO0fU6vsiNqcH0WmeGRT
NKDuj9NQhMlUJAi9SmVbwXZAwsj+qMbRiFa+zwuoRs59yPNdyTm2D/K4D0f1OcajOm6DaL6gtRS4
EUUtTfUpbWqIJAroB8DPPRypoyMS5/aM+akh5stH4ntzYxhT91gAMsaUi66xEds1FtuVyAv9BCpA
atJLT1T75yoc/WNql4HAscAngXG0PJrmPSR+voZSr15FUTXzgQWoy8jbMbclrWXz2ipxTXVFnU96
om4nmwBD47YZql8xFAlAG5RoDN5BAtBrqI7aq3H/NWBhgc/QUGReyP0MA1FHsy3iM2wdn2EjVKC1
JTQAz6IaPzfEGBtTi3wHFXrfDkcBpZ0hKB11fMxrs1Bk6FRcYN7UKRaAjDGlpHtstHZD3t63UOeu
SbhVey1xJnA+MBJH/5jap1MY90eiIskjS/S+a1EqwWokFhWqrdMh5sbOqLB69zimtrAIebL7t/F9
3o/jXpX1eRqKzO9d4mfyeUrBC6jI9c3ISWAPvKl1Norz+oc4CqiW6Iu6iU1ARfrnoW5ik3FHMVNH
WAAyxrSV3sBOqJ5PP+RVnoRaMHvBrD2S6J/fAN/zcJg6o0vMVwcA+6Eoxd41cNyrULrsv1AR6CeR
sLQrKgK9J4q07FEDn2UF8q7fhwpyP4sENGPqiW8Dn8VRQLVKd5QiNgFFa76FBOpnqX6tOGPahAUg
Y0xr6BsL42hU72FeLIwv2pCveb6Eav84+se0BwYjj+9u8XM4SmuqJmtRqtYM5HmeFJuOeRSOjumE
Uhl2ig3LuPgsm1G6SJ3WsiA+y39QCvAk3IXH1D9JFNAPcEewWqdLzKcTYp5dikTsaTi63dQgFoCM
Mc2lH/KWj0aekbmoWOps1GbY1D5J56+rcPSPaZ/0BbYKY38U6li4BRKFkto8Hdr4PxqQyLME1ed5
HdUYmhW3GahGz6o2/p8eqB7P8Pgc26IaPYPis/RD4lApPk9Sa2gBqpM0EzkEZsQa4U2SaY98G9UC
ckew+qFjrBHjgWExTz+HBKElHh5TC9S6ANTZX6ExZaUfEnyGI9FnNnAbEn9M/XEqKvJ6pYfCtFOW
omibZ7Oe64AKH/dFXv2NUO2dPjFHdovXcoWUhni/9+Ln8tggLIrbsniuXFGTq1A650s5z3dBBZz7
ZH2efvFc33i92OdZE59jGRkR6814vBJ3dTQm4UoUVXsqrgVUL6xDreOT9vGbI+foJ1HR6BeQIGQx
yJgyYQHImNLTD3m+t4+NwMvA7bgjS73TFfgy8FsqlPpV6x4I025oQDUfVqDUrFrnPWBx3Gb76zWt
4b777vMgNM0i4HeoscKvcFepemQuGafoIGAH4OMoXXcGMB2LQcaUFAtAxpSG/qjmy9YovHU26sqy
0EPTbjgJef2v8FAYY4wxJeFyVAz6JORgMfXL63EDFY4eCRyOooZeRtFBb3mYjGkbFoCMaT19UGrX
9qiA6GzgDuQVNu2LrsC5qPaPi7MaY4wxpWF+rK3nAn/CUUDthYVxexgYgCKDDovvfxaqk7bMw2RM
y7EAZEzL6AWMALZBkT5zgbtwt6f2zkko9c81CowxxpjS8nNUDNpRQO2TxcAjcdso7PCDUWRQUvh/
pYfJmOZhAciYpulBpgtMF9Sh5V4c6WNEJ+Bs4Dc4+scYY4wpNfNjjT0b+AOqD2PaJ4uAf8X9Aajm
5hFkIoNm0PYOkMbUNRaAjMnPBkjwGR735wIP4Zo+Zn2OQx6pyz0UxhhjTFn4JfA54FjgLx4Ogxyx
j8b9pGbQx4DVKEVsFvCOh8mYxlgAMqbx9bANCi3dEHmcHsdRHaYwHYHzgD/7PDHGGGPKxmuoBtD5
wPUo/ceYhKRmEMBgFBk0DliOooL+C6z1MBljAciYjsCwWCgGxOIxGaV5NXh4TBMcCwwFLvVQGGOM
MWXlUuDkWHuv83CYAsyPWwdgCxQZtCuKGJoOzMEComnHWAAy7ZUhwGjkJVgKPI+6eNk7YFrC2cA1
YUwYY4wxpnzMiTX3bCwAmaZpiHNmTux5t0JRQXujdvPPAfM8TKa9YQHItCcGAGOALYE1SPR5KO4b
01KOBLaOn8YYY4wpPz9DUUBHAjd7OEwzWQu8FLduqMbnxLj/CjANN3cx7QQLQKbe6YlEn+Eo3WsG
8HeUE2xMWzgf+FsYDsYYY4wpPy8DNwDfxAKQaR1rgKlx2xBlBByO0sJmIDHobQ+TqVcsAJl6Pa9H
AmOBXrFBvwt4w0NjSsSBwHbAJzwUxhhjTEW5GHgGOAi408Nh2sBy1PDlcWBTYCcUYbYcCUQv4PIQ
pg43ysbUC8OAD6D6PguAJ1BdHxd6M6Umif6Z6aEwxhhjKspMFM19HhaATOl4A/gnKh69DTAB+DDw
KvAf1CDGmJrHApCpdQYAu6AUr7eBZ4HbcF0fUz72B8YDp3sojDHGmKpwaWzKDwDu8XCYEtIAzIpb
N1RK4iNAD5Qi9h9cL8jUMBaATC3SDYVoTgA2QMWc/wi85aExFeDrwC2oe4QxxhhjKs+0WIvPwQKQ
KR9rgElx6wfsBpwKrAKeRo5nO51NTWEByNQS2wJ7AUNRGOZdwH89LKaC7B2L/wc9FMYYY0xVuQh4
MtbmRzwcpswsib3HXShFbE8UFf4a8CiKGDIm9VgAMmmnPxJ9xqEUr8eBP2O13VSH84B7UWFAY4wx
xlSPqbEmn4cFIFNZ/hu3bsDOwBGo8/BkJAY5K8GkFgtAJq3n5c7ARKAPMAW4AljooTFVZBfk7Zno
oTDGGGNSwY+Ah2ONfsrDYSrMGjJdxDYB9gHOBpbFefk07iJmUrjRNiYtbIZCKXcA5gN3oxxvd/Ey
aeB84H5U/M8YY4wx1ec/wAOxRh/q4TBVZCHwV+BGVDh6P+AYVKv0XtRNzJiqYwHIVJsNUIrXvkAX
4DHgAqScG5MWxiJxcj8PhTHGGJMqLgbui7V6iofDVJl1cR5OQZkM+wJnAe8hsfJR4B0Pk6kWFoBM
tdgWOCR+zgb+BLzgYTEp5dxYsP/loTDGGGNSxb9ijT4XRVwYkxaWATfHbVTsfY5G7eRvx4WjTRWw
AGQqSQ8URfFhoANSwX+Dijsbk1ZGAYcBH/VQGGOMMankIuCfqIzA8x4Ok0Kmx6137Ie+DDSg8gL3
otbyxpQdC0CmEmwPHAmMRIr3r2ICNKYW+CowCXjIQ2GMMcakkgdRPaCvAJ/ycJgUswK4KW4jY490
VOyNbgZmeohMObEAZMpFd+AA4KA4z+5GnbyWe2hMDbEdcCxq72mMMcaY9PJD4BYUDfSSh8PUAC/E
bUMUaf4N1DXsTuAeYLWHyJQaC0Cm1GwdG+bxsfj+CnjWw2JqlDORR+YeD4UxxhiTau5G6V9nAl/w
cJgaYjlwQ9x2Aj4OnAg8gzqLvewhMqXCApApBV1QXZ+jkIJ9D3AasNhDY2qYzWPxPdFDYYwxxtQE
PwKuAX4MzPVwmBrk2bgNAD6GutwtBf6G6qe+5yEybcECkGkLg1C3hX2B+cC1qAvDOg+NqQPOAv6L
wsmNMcYYk35uQd1lz4qbMbXKYuAq4HfAXsBxKLLtQRQV9LqHyLQGC0CmNewCnITSvR4FzgDmeFhM
HTEQOBn4oofCGGOMqSkuBn6JooAWeDhMjbMOeDhuw4ATgD+itLA/AU95iExLsABkmktPVKX+Y8D7
wN+Bs3ELd1OfnAnMQ7nYxhhjjKkdbgDOj7X8XA+HqSPmAD+IfdlhWef334BbvS8zzcECkGmKLVA7
zb1QW8IfoTabxtQrGwGnAuegTgzGGGOMqR3WApcCFwKXAIs8JKbOeBu4Lm67xV7tVOARFB00x0Nk
CtHRQ2AKsCcKK7wGFRs7ETgdiz+m/vkCKrb3Fw+FMcYYU5NcG2u5u4GZeudJ1HznRCR+XhN7uD09
NCYfjgAy2fRAnbxOQPmmfwZuBlZ5aEw7oQ/wOeACYI2HwxhjjKlJ1qAooAuAnyMxyJh65lWUHnYp
KttxPtAh9nM3eT9nEhwBZACGAN9GVeX3j8njozFheLIw7YnTgHeR59AYY4wxtcs1SAg61UNh2hGr
Yg/3EVS64yOoffy3Y89n2jkWgNo3OwG/B25HkQ+fRNE/jwINHh7TzuiNCkZegovoGWOMMbXO2yga
4svAhh4O0w55BDgepYf1AW5DbeV38tC0XywAtT86A4cAdwJXAs+jqJ+vArM8PKYdcxLQBbjaQ2GM
McbUBVfH2n6ih8K0Y2bFXu8AYHrsAe8EDgY6eXjaFxaA2hdjgIeAs8gUB/sZsNhDYwz7Ab8Clnso
jDHGmLpgWWx29/NQGMPi2PvthVIkvwo8HHtE005wEej2xfy40J9GRZ6NMRmuA57wMBhjjDF1xdXA
TA+DMf/Pu8ANwN+B8bFHNO2E/xsAX8dTZobPat8AAAAASUVORK5CYII=" muse-slidy-flagship-theme)
(puthash 'muse-slidy-background-css "<style>body {
background: rgba(204,204,196,1);
background: -moz-linear-gradient(left, rgba(204,204,196,1) 0%, rgba(204,204,196,1) 1%, rgba(228,230,223,1) 26%, rgba(227,230,223,1) 72%, rgba(204,204,196,1) 100%);
background: -webkit-gradient(left top, right top, color-stop(0%, rgba(204,204,196,1)), color-stop(1%, rgba(204,204,196,1)), color-stop(26%, rgba(228,230,223,1)), color-stop(72%, rgba(227,230,223,1)), color-stop(100%, rgba(204,204,196,1)));
background: -webkit-linear-gradient(left, rgba(204,204,196,1) 0%, rgba(204,204,196,1) 1%, rgba(228,230,223,1) 26%, rgba(227,230,223,1) 72%, rgba(204,204,196,1) 100%);
background: -o-linear-gradient(left, rgba(204,204,196,1) 0%, rgba(204,204,196,1) 1%, rgba(228,230,223,1) 26%, rgba(227,230,223,1) 72%, rgba(204,204,196,1) 100%);
background: -ms-linear-gradient(left, rgba(204,204,196,1) 0%, rgba(204,204,196,1) 1%, rgba(228,230,223,1) 26%, rgba(227,230,223,1) 72%, rgba(204,204,196,1) 100%);
background: linear-gradient(to right, rgba(204,204,196,1) 0%, rgba(204,204,196,1) 1%, rgba(228,230,223,1) 26%, rgba(227,230,223,1) 72%, rgba(204,204,196,1) 100%);
filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#ccccc4', endColorstr='#ccccc4', GradientType=1 );
}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-fonts-css "<style><link href='http://fonts.googleapis.com/css?family=Montserrat+Alternates|Habibi' rel='stylesheet' type='text/css'/></style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-title-font-family-css    "<style>.title-h1{font-family:Montserrat Alternates, serif;font-size:90px;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-title-font-color-css     "<style>.title-h1{color:#ed9017;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-author-font-family-css   "<style>.title-author{font-family:Montserrat Alternates, serif;font-size:45px;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-author-font-color-css    "<style>.title-author{color:#333333;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-header-font-family-css   "<style>h1{font-family:Montserrat Alternates, serif;font-size:46px;font-style:normal;text-decoration:none;text-transform:none;text-align:center;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-header-font-color-css    "<style>h1{color:#ed9017;text-shadow:2px 2px 2px #aaa;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-body-font-family-css   "<style>body{font-family:\"Habibi\", sans-serif;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-body-font-color-css    "<style>body{color:#111111;}</style>" muse-slidy-flagship-theme)
(puthash 'muse-slidy-head-icon "" muse-slidy-flagship-theme)
(puthash 'muse-slidy-head-logo "" muse-slidy-flagship-theme)
;;(puthash 'muse-slidy-cover-image " <img src=\"a.jpeg\" class=\"cover\" />" muse-slidy-flagship-theme)
(puthash 'muse-slidy-cover-image "" muse-slidy-flagship-theme)
(puthash 'muse-slidy-slidy-css "<style type=\"text/css\" media=\"screen, projection, print\">/*<![CDATA[*/body{width:100%;height:100%;margin:0;padding:0;}.title-slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;background-color:transparent;border-width:0;margin:0;padding:20px 20px 0;}.title-content{position:absolute;bottom:10px;left:20px;}.title-author{font-family:Cabin, serif;font-size:50px;font-style:normal;font-weight:700;color:#000;text-shadow:2px 2px 2px #aaa;text-decoration:none;text-transform:none;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}.hidden{display:none;visibility:hidden;}div.toolbar{position:fixed;z-index:200;top:auto;bottom:0;left:0;right:0;height:1.2em;text-align:right;padding-left:1em;padding-right:1em;font-size:60%;color:gray;background:transparent;}div.background{display:none;}div.handout{margin-left:20px;margin-right:20px;}div.slide.titlepage.h1{padding-top:40%;}div.slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;line-height:120%;font-size:24pt;background-color:transparent;border-width:0;margin:0;padding:10px 20px 0;}div.slide + div[class].slide{page-break-before:always;} div.toc{position:absolute;top:auto;bottom:4em;left:4em;right:auto;width:60%;max-width:30em;height:30em;border:solid thin #000;background:#f0f0f0;color:#000;z-index:300;overflow:auto;display:block;visibility:visible;padding:1em;}div.toc-heading{width:100%;border-bottom:solid 1px #b4b4b4;margin-bottom:1em;text-align:center;}pre{font-size:50%;font-weight:300;line-height:100%;color:#999999;background-color:#E4E5E7;border-color:#444444;border-style:solid;border-width:thin thin thin 1em;padding: -10em .1em;}li pre{margin-left:0;}blockquote{font-style:italic;}. footnote{font-size:smaller;margin-left:2em;}a img{border-style:none;border-width:0;}a{color:#000;text-decoration:underlined;text-shadow:2px 2px 2px #00f;}.navbar a:link{color:#FFF;}.navbar a:visited{color:#FF0;}ul{list-style-type:disc;margin:.5em .5em .5em 3.5em;padding:0;}ul ul{list-style-type:square;}ul ul ul{list-style-type:circle;}ul ul ul ul{list-style-type:disc;}li{margin-left:1.5em;margin-top:.5em;}li li{font-size:85%;font-style:italic;}li li li{font-size:85%;font-style:normal;}div dt{margin-left:0;margin-top:1em;margin-bottom:.5em;font-weight:700;}div dd{margin-left:2em;margin-bottom:.5em;}p,pre,ul,ol,blockquote,h2,h3,h4,h5,h6,dl,table{margin-left:1em;margin-right:1em;}table{align:right;}p.subhead{font-weight:700;margin-top:2em;}.bigger{font-size:130%;}td,th{padding:.2em;}th{background:#f0f0f0}ol{margin:.5em 1.5em .5em .5em;padding:0;}li ul li{font-size:85%;font-style:italic;list-style-type:disc;background:transparent;padding:0;}li li ul li{font-size:85%;font-style:normal;list-style-type:circle;background:transparent;padding:0;}li li li ul li{list-style-type:disc;background:transparent;padding:0;}ol.outline{list-style:decimal;}ol.outline ol{list-style-type:lower-alpha;}a.titleslide{font-weight:700;font-style:italic;}div.slide.titlepage,.center{text-align:center;}strong,.navbar a:active,.navbar a:hover{color:red;}p.copyright,.smaller{font-size:smaller;}a:visited,a:link{text-shadow:1px 1px 1px #ccc;}a:hover,a:active{color:red;text-decoration:underline;}li ol li,li li ol li{list-style-type:decimal;}ol.outline li:hover,ul.outline li:hover{cursor:pointer;}ol.outline li.nofold:hover,ul.outline li.nofold:hover{cursor:default;}ol.outline li.nofold,ul.outline ol.outline li.unfolded,ul.outline li.unfolded{background:transparent no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded,ul.outline ol.outline li.unfolded:hover,ul.outline li.unfolded:hover{no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded:hover,ul.outline li.folded:hover{no-repeat 0 .5em;padding:0 0 0 20px;} div.image-container{  margin-left: 1em;  margin-right: 1em;  margin-top: 0;  margin-left:auto;  margin-right:auto;  align: center; overflow:auto;  max-width: 80%; text-align:center;  clear:both;}div.image-description{  clear:both;  text-align: center;  font-size:60%; font-style: italic;}}/*]]>*/</style>" muse-slidy-flagship-theme)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  GREEN THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq muse-slidy-green-theme (make-hash-table :test 'equal))
;; 1x1 transparent png
(puthash 'muse-slidy-header-image "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" muse-slidy-green-theme)
;; 1x1 transparent png
(puthash 'muse-slidy-footer-image "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACXBIWXMAAAsTAAALEwEAmpwYAAAADUlEQVQIHWP478vAAAAE6AFNd1XdIgAAAABJRU5ErkJggg==" muse-slidy-green-theme)
(puthash 'muse-slidy-leader-image "" muse-slidy-green-theme)
(puthash 'muse-slidy-background-css "<style>
body {
background: #d9eb95; /* Old browsers */
background: -moz-linear-gradient(left,  #d9eb95 0%, #558338 99%); /* FF3.6+ */
background: -webkit-gradient(linear, left top, right top, color-stop(0%,#d9eb95), color-stop(99%,#558338)); /* Chrome,Safari4+ */
background: -webkit-linear-gradient(left,  #d9eb95 0%,#558338 99%); /* Chrome10+,Safari5.1+ */
background: -o-linear-gradient(left,  #d9eb95 0%,#558338 99%); /* Opera 11.10+ */
background: -ms-linear-gradient(left,  #d9eb95 0%,#558338 99%); /* IE10+ */
background: linear-gradient(to right,  #d9eb95 0%,#558338 99%); /* W3C */
filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#d9eb95', endColorstr='#558338',GradientType=1 ); /* IE6-9 */
}
</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-fonts-css "<style>
<link href='http://fonts.googleapis.com/css?family=Alegreya+Sans:800|Neuton' rel='stylesheet' type='text/css'/>
</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-title-font-family-css    "<style>
.title-h1{font-family:Alegreya Sans, serif;font-size:90px;}</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-title-font-color-css     "<style>.title-h1{color:#65341d;}</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-author-font-family-css   "<style>
.title-author{font-family:Neuton, serif;font-size:45px;}
</style>
" muse-slidy-green-theme)
(puthash 'muse-slidy-author-font-color-css    "<style>.title-author{color:#ed9017;}</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-header-font-family-css   "<style>
h1{font-family:Alegreya Sans, serif, serif;font-size:46px;font-style:normal;text-decoration:none;text-transform:none;text-align:center;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-header-font-color-css    "<style>
h1{color:#65341d;text-shadow:2px 2px 2px #aaa;}
</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-body-font-family-css   "<style>
body{font-family:\"Neuton\", sans-serif;}
</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-body-font-color-css    "<style>
body{color:#111111;}
</style>" muse-slidy-green-theme)
(puthash 'muse-slidy-head-icon "" muse-slidy-green-theme)
(puthash 'muse-slidy-head-logo "" muse-slidy-green-theme)
(puthash 'muse-slidy-cover-image "" muse-slidy-green-theme)
(puthash 'muse-slidy-slidy-css "<style type=\"text/css\" media=\"screen, projection, print\">/*<![CDATA[*/body{width:100%;height:100%;margin:0;padding:0;}.title-slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;background-color:transparent;border-width:0;margin:0;padding:20px 20px 0;}.title-content{position:absolute;bottom:10px;left:20px;}.title-author{font-family:Cabin, serif;font-size:50px;font-style:normal;font-weight:700;color:#000;text-shadow:2px 2px 2px #aaa;text-decoration:none;text-transform:none;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}.hidden{display:none;visibility:hidden;}div.toolbar{position:fixed;z-index:200;top:auto;bottom:0;left:0;right:0;height:1.2em;text-align:right;padding-left:1em;padding-right:1em;font-size:60%;color:gray;background:transparent;}div.background{display:none;}div.handout{margin-left:20px;margin-right:20px;}div.slide.titlepage.h1{padding-top:40%;}div.slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;line-height:120%;font-size:24pt;background-color:transparent;border-width:0;margin:0;padding:10px 20px 0;}div.slide + div[class].slide{page-break-before:always;} div.toc{position:absolute;top:auto;bottom:4em;left:4em;right:auto;width:60%;max-width:30em;height:30em;border:solid thin #000;background:#f0f0f0;color:#000;z-index:300;overflow:auto;display:block;visibility:visible;padding:1em;}div.toc-heading{width:100%;border-bottom:solid 1px #b4b4b4;margin-bottom:1em;text-align:center;}pre{font-size:50%;font-weight:300;line-height:100%;color:#999999;background-color:#E4E5E7;border-color:#444444;border-style:solid;border-width:thin thin thin 1em;padding: -10em .1em;}li pre{margin-left:0;}blockquote{font-style:italic;}. footnote{font-size:smaller;margin-left:2em;}a img{border-style:none;border-width:0;}a{color:#000;text-decoration:underlined;text-shadow:2px 2px 2px #00f;}.navbar a:link{color:#FFF;}.navbar a:visited{color:#FF0;}ul{list-style-type:disc;margin:.5em .5em .5em 3.5em;padding:0;}ul ul{list-style-type:square;}ul ul ul{list-style-type:circle;}ul ul ul ul{list-style-type:disc;}li{margin-left:1.5em;margin-top:.5em;}li li{font-size:85%;font-style:italic;}li li li{font-size:85%;font-style:normal;}div dt{margin-left:0;margin-top:1em;margin-bottom:.5em;font-weight:700;}div dd{margin-left:2em;margin-bottom:.5em;}p,pre,ul,ol,blockquote,h2,h3,h4,h5,h6,dl,table{margin-left:1em;margin-right:1em;}table{align:right;}p.subhead{font-weight:700;margin-top:2em;}.bigger{font-size:130%;}td,th{padding:.2em;}th{background:#f0f0f0}ol{margin:.5em 1.5em .5em .5em;padding:0;}li ul li{font-size:85%;font-style:italic;list-style-type:disc;background:transparent;padding:0;}li li ul li{font-size:85%;font-style:normal;list-style-type:circle;background:transparent;padding:0;}li li li ul li{list-style-type:disc;background:transparent;padding:0;}ol.outline{list-style:decimal;}ol.outline ol{list-style-type:lower-alpha;}a.titleslide{font-weight:700;font-style:italic;}div.slide.titlepage,.center{text-align:center;}strong,.navbar a:active,.navbar a:hover{color:red;}p.copyright,.smaller{font-size:smaller;}a:visited,a:link{text-shadow:1px 1px 1px #ccc;}a:hover,a:active{color:red;text-decoration:underline;}li ol li,li li ol li{list-style-type:decimal;}ol.outline li:hover,ul.outline li:hover{cursor:pointer;}ol.outline li.nofold:hover,ul.outline li.nofold:hover{cursor:default;}ol.outline li.nofold,ul.outline ol.outline li.unfolded,ul.outline li.unfolded{background:transparent no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded,ul.outline ol.outline li.unfolded:hover,ul.outline li.unfolded:hover{no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded:hover,ul.outline li.folded:hover{no-repeat 0 .5em;padding:0 0 0 20px;} div.image-container{  margin-left: 1em;  margin-right: 1em;  margin-top: 0;  margin-left:auto;  margin-right:auto;  align: center; overflow:auto;  max-width: 80%; text-align:center;  clear:both;}div.image-description{  clear:both;  text-align: center;  font-size:60%; font-style: italic;}}/*]]>*/</style>" muse-slidy-green-theme)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  BLACK THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq muse-slidy-black-theme (make-hash-table :test 'equal))
;; 1x1 transparent png
(puthash 'muse-slidy-header-image "" muse-slidy-black-theme)
;; 1x1 transparent png
(puthash 'muse-slidy-footer-image "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
y/L/BgDPZNCIgF/CGAAAAABJRU5ErkJggg==" muse-slidy-black-theme)
(puthash 'muse-slidy-leader-image "" muse-slidy-black-theme)
(puthash 'muse-slidy-background-css "<style>
body {
background: #4c4c4c; /* Old browsers */
background: -moz-linear-gradient(left,  #4c4c4c 0%, #111111 16%, #131313 100%); /* FF3.6+ */
background: -webkit-gradient(linear, left top, right top, color-stop(0%,#4c4c4c), color-stop(16%,#111111), color-stop(100%,#131313)); /* Chrome,Safari4+ */
background: -webkit-linear-gradient(left,  #4c4c4c 0%,#111111 16%,#131313 100%); /* Chrome10+,Safari5.1+ */
background: -o-linear-gradient(left,  #4c4c4c 0%,#111111 16%,#131313 100%); /* Opera 11.10+ */
background: -ms-linear-gradient(left,  #4c4c4c 0%,#111111 16%,#131313 100%); /* IE10+ */
background: linear-gradient(to right,  #4c4c4c 0%,#111111 16%,#131313 100%); /* W3C */
filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#4c4c4c', endColorstr='#131313',GradientType=1 ); /* IE6-9 */
}
</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-fonts-css "<style>
<link href='http://fonts.googleapis.com/css?family=Rosarivo|Actor' rel='stylesheet' type='text/css'/>
</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-title-font-family-css    "<style>
.title-h1{font-family:Rosarivo, serif;font-size:90px;}</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-title-font-color-css     "<style>.title-h1{color:#dce0d9;}</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-author-font-family-css   "<style>
.title-author{font-family:Rosarivo, serif;font-size:45px;text-decoration:none;}
</style>
" muse-slidy-black-theme)
(puthash 'muse-slidy-author-font-color-css    "<style>.title-author{color:#dce0d9;}</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-header-font-family-css   "<style>
h1{font-family:Rosarivo, serif, serif;font-size:46px;font-style:normal;text-decoration:none;text-transform:none;text-align:center;line-height:0.89;}</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-header-font-color-css    "<style>
h1{
  color:#dce0d9;
}
</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-body-font-family-css   "<style>
body{font-family:\"Actor\", sans-serif;}
</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-body-font-color-css    "<style>
body{color:#dce0d9;}
</style>" muse-slidy-black-theme)
(puthash 'muse-slidy-head-icon "" muse-slidy-black-theme)
(puthash 'muse-slidy-head-logo "" muse-slidy-black-theme)
(puthash 'muse-slidy-cover-image "" muse-slidy-black-theme)
(puthash 'muse-slidy-slidy-css "<style type=\"text/css\" media=\"screen, projection, print\">/*<![CDATA[*/body{width:100%;height:100%;margin:0;padding:0;}.title-slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;background-color:transparent;border-width:0;margin:0;padding:20px 20px 0;}.title-content{position:absolute;bottom:10px;left:20px;}.hidden{display:none;visibility:hidden;}div.toolbar{position:fixed;z-index:200;top:auto;bottom:0;left:0;right:0;height:1.2em;text-align:right;padding-left:1em;padding-right:1em;font-size:60%;color:gray;background:transparent;}div.background{display:none;}div.handout{margin-left:20px;margin-right:20px;}div.slide.titlepage.h1{padding-top:40%;}div.slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;line-height:120%;font-size:24pt;background-color:transparent;border-width:0;margin:0;padding:10px 20px 0;}div.slide + div[class].slide{page-break-before:always;} div.toc{position:absolute;top:auto;bottom:4em;left:4em;right:auto;width:60%;max-width:30em;height:30em;border:solid thin #000;background:#f0f0f0;color:#000;z-index:300;overflow:auto;display:block;visibility:visible;padding:1em;}div.toc-heading{width:100%;border-bottom:solid 1px #b4b4b4;margin-bottom:1em;text-align:center;}pre{font-size:50%;font-weight:300;line-height:100%;color:#999999;background-color:#E4E5E7;border-color:#444444;border-style:solid;border-width:thin thin thin 1em;padding: -10em .1em;}li pre{margin-left:0;}blockquote{font-style:italic;}. footnote{font-size:smaller;margin-left:2em;}a img{border-style:none;border-width:0;}a{color:#FFFFFF;text-decoration:underlined;}.navbar a:link{color:#FFF;}.navbar a:visited{color:#FFF;}ul{list-style-type:disc;margin:.5em .5em .5em 3.5em;padding:0;}ul ul{list-style-type:square;}ul ul ul{list-style-type:circle;}ul ul ul ul{list-style-type:disc;}li{margin-left:1.5em;margin-top:.5em;}li li{font-size:85%;font-style:italic;}li li li{font-size:85%;font-style:normal;}div dt{margin-left:0;margin-top:1em;margin-bottom:.5em;font-weight:700;}div dd{margin-left:2em;margin-bottom:.5em;}p,pre,ul,ol,blockquote,h2,h3,h4,h5,h6,dl,table{margin-left:1em;margin-right:1em;}table{align:right;}p.subhead{font-weight:700;margin-top:2em;}.bigger{font-size:130%;}td,th{padding:.2em;}th{background:#f0f0f0;color:#000000}ol{margin:.5em 1.5em .5em .5em;padding:0;}li ul li{font-size:85%;font-style:italic;list-style-type:disc;background:transparent;padding:0;}li li ul li{font-size:85%;font-style:normal;list-style-type:circle;background:transparent;padding:0;}li li li ul li{list-style-type:disc;background:transparent;padding:0;}ol.outline{list-style:decimal;}ol.outline ol{list-style-type:lower-alpha;}a.titleslide{font-weight:700;font-style:italic;}div.slide.titlepage,.center{text-align:center;}strong,.navbar a:active,.navbar a:hover{color:red;}p.copyright,.smaller{font-size:smaller;}a:visited,a:link{}a:hover,a:active{color:red;text-decoration:underline;}li ol li,li li ol li{list-style-type:decimal;}ol.outline li:hover,ul.outline li:hover{cursor:pointer;}ol.outline li.nofold:hover,ul.outline li.nofold:hover{cursor:default;}ol.outline li.nofold,ul.outline ol.outline li.unfolded,ul.outline li.unfolded{background:transparent no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded,ul.outline ol.outline li.unfolded:hover,ul.outline li.unfolded:hover{no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded:hover,ul.outline li.folded:hover{no-repeat 0 .5em;padding:0 0 0 20px;} div.image-container{  margin-left: 1em;  margin-right: 1em;  margin-top: 0;  margin-left:auto;  margin-right:auto;  align: center; overflow:auto;  max-width: 80%; text-align:center;  clear:both;}div.image-description{  clear:both;  text-align: center;  font-size:60%; font-style: italic;}}/*]]>*/</style>" muse-slidy-black-theme)






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
(puthash 'muse-slidy-slidy-css "<style type=\"text/css\" media=\"screen, projection, print\">/*<![CDATA[*/body{width:100%;height:100%;margin:0;padding:0;}.title-slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;background-color:transparent;border-width:0;margin:0;padding:20px 20px 0;}.title-content{position:absolute;bottom:10px;left:20px;}.title-author{font-family:Cabin, serif;font-size:50px;font-style:normal;font-weight:700;color:#000;text-shadow:2px 2px 2px #aaa;text-decoration:none;text-transform:none;letter-spacing:-.015em;word-spacing:-.009em;line-height:0.89;}.hidden{display:none;visibility:hidden;}div.toolbar{position:fixed;z-index:200;top:auto;bottom:0;left:0;right:0;height:1.2em;text-align:right;padding-left:1em;padding-right:1em;font-size:60%;color:gray;background:transparent;}div.background{display:none;}div.handout{margin-left:20px;margin-right:20px;}div.slide.titlepage.h1{padding-top:40%;}div.slide{z-index:20;clear:both;top:0;bottom:0;left:0;right:0;line-height:120%;font-size:24pt;background-color:transparent;border-width:0;margin:0;padding:10px 20px 0;}div.slide + div[class].slide{page-break-before:always;} div.toc{position:absolute;top:auto;bottom:4em;left:4em;right:auto;width:60%;max-width:30em;height:30em;border:solid thin #000;background:#f0f0f0;color:#000;z-index:300;overflow:auto;display:block;visibility:visible;padding:1em;}div.toc-heading{width:100%;border-bottom:solid 1px #b4b4b4;margin-bottom:1em;text-align:center;}pre{font-size:80%;font-weight:700;line-height:120%;color:#00428C;background-color:#E4E5E7;border-color:#95ABD0;border-style:solid;border-width:thin thin thin 1em;padding:.2em 1em;}li pre{margin-left:0;}blockquote{font-style:italic;}. footnote{font-size:smaller;margin-left:2em;}a img{border-style:none;border-width:0;}a{color:#000;text-decoration:none;text-shadow:2px 2px 2px #00f;}.navbar a:link{color:#FFF;}.navbar a:visited{color:#FF0;}ul{list-style-type:disc;margin:.5em .5em .5em 3.5em;padding:0;}ul ul{list-style-type:square;}ul ul ul{list-style-type:circle;}ul ul ul ul{list-style-type:disc;}li{margin-left:1.5em;margin-top:.5em;}li li{font-size:85%;font-style:italic;}li li li{font-size:85%;font-style:normal;}div dt{margin-left:0;margin-top:1em;margin-bottom:.5em;font-weight:700;}div dd{margin-left:2em;margin-bottom:.5em;}p,pre,ul,ol,blockquote,h2,h3,h4,h5,h6,dl,table{margin-left:1em;margin-right:1em;}p.subhead{font-weight:700;margin-top:2em;}.bigger{font-size:130%;}td,th{padding:.2em;}ol{margin:.5em 1.5em .5em .5em;padding:0;}li ul li{font-size:85%;font-style:italic;list-style-type:disc;background:transparent;padding:0;}li li ul li{font-size:85%;font-style:normal;list-style-type:circle;background:transparent;padding:0;}li li li ul li{list-style-type:disc;background:transparent;padding:0;}ol.outline{list-style:decimal;}ol.outline ol{list-style-type:lower-alpha;}a.titleslide{font-weight:700;font-style:italic;}div.slide.titlepage,.center{text-align:center;}strong,.navbar a:active,.navbar a:hover{color:red;}p.copyright,.smaller{font-size:smaller;}a:visited,a:link{text-shadow:1px 1px 1px #ccc;}a:hover,a:active{color:red;text-decoration:underline;}li ol li,li li ol li{list-style-type:decimal;}ol.outline li:hover,ul.outline li:hover{cursor:pointer;}ol.outline li.nofold:hover,ul.outline li.nofold:hover{cursor:default;}ol.outline li.nofold,ul.outline li.nofold{background:transparent  no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.unfolded,ul.outline li.unfolded{background:transparent  no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded,ul.outline li.folded{background:transparent no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.unfolded:hover,ul.outline li.unfolded:hover{background:transparent  no-repeat 0 .5em;padding:0 0 0 20px;}ol.outline li.folded:hover,ul.outline li.folded:hover{background:transparent no-repeat 0 .5em;padding:0 0 0 20px;} div.image-container{  margin-left: 1em;  margin-right: 1em;  margin-top: 0;  margin-left:auto;  margin-right:auto;  align: center; overflow:auto;  max-width: 80%; text-align:center;  clear:both;}div.image-description{  clear:both;  text-align: center;  font-style: italic;}}/*]]>*/</style>" muse-slidy-white-theme)




(defcustom muse-slidy-override-header-image  ""
"top header image. It can be either a file name or base64 content. Example: ~/Desktop/greenbanner.png"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-footer-image  ""
"footer image. . Example: ~/Desktop/greenbanner.png"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-leader-image ""
"An openning image only used in the cover. Stretched to fit. It can be either a file name or base64 content. Example: ~/Desktop/greenbanner.png"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-background-css  ""
  "CSS used for the background. Leave blank to use the theme's default. Example:
<style>
body {
background: #6bba70; /* Old browsers */
background: -moz-linear-gradient(top,  #6bba70 0%, #6bba70 100%); /* FF3.6+ */
background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#6bba70), color-stop(100%,#6bba70)); /* Chrome,Safari4+ */
background: -webkit-linear-gradient(top,  #6bba70 0%,#6bba70 100%); /* Chrome10+,Safari5.1+ */
background: -o-linear-gradient(top,  #6bba70 0%,#6bba70 100%); /* Opera 11.10+ */
background: -ms-linear-gradient(top,  #6bba70 0%,#6bba70 100%); /* IE10+ */
background: linear-gradient(to bottom,  #6bba70 0%,#6bba70 100%); /* W3C */
filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#6bba70', endColorstr='#6bba70',GradientType=0 ); /* IE6-9 */
}
</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-fonts-css  ""
"Any CSS you need to embedd your fonts. BE CAREFUL TO ADD WELL FORMED LINKS (IF COPYING FROM GOOGLE FONTS ADD TRAILING / TO LINK ELEMENT) Example: 
<style>
<link href='http://fonts.googleapis.com/css?family=Sigmar+One|Neuton' rel='stylesheet' type='text/css'/>
</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-title-font-family-css  ""
"CSS to control the title font. Example: 
<style>.title-h1{font-family:Montserrat Alternates, serif;font-size:90px;}</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-title-font-color-css  ""
"CSS to control the color of the title font. Example: 
<style>.title-h1{color:#ed9017;text-shadow:2px 2px 2px #aaa;}</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-author-font-family-css  ""
"CSS to control the author font in the titles. Example: 
<style>
.title-author{font-family:Montserrat Alternates, serif;font-size:45px;}
</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-author-font-color-css  ""
"CSS to control the color of the author font in titles. Example: 
<style>.title-h1{color:#d0e009;text-shadow:2px 2px 2px #aaa;}</style>"
  :type 'string
  :group 'muse-slidy)


(defcustom muse-slidy-override-header-font-family-css  ""
"CSS to control the headers font. Example: 
<style>h1{font-family:Montserrat Alternates, serif;font-size:90px;}</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-header-font-color-css  ""
"CSS to control the color of the header font. Example: 
<style>h1{color:#ed9017;text-shadow:2px 2px 2px #aaa;}</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-body-font-family-css  ""
"CSS to control the body font. Example: 
<style>
body{font-family:\"Habibi\", sans-serif;}
</style>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-body-font-color-css  ""
"CSS to control the body font color. Example: 
<style>
body{color:#111111;}
</style>"
  :type 'string
  :group 'muse-slidy)


(defcustom muse-slidy-override-head-icon  ""
"a single header image. Note this is markup, not just the name of the file, to allow for flexibility. Example: <img src=\"~Desktop/bokeh.png\"/>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-head-logo  ""
"another single header image. Note this is markup, not just the name of the file, to allow for flexibility. Example: <img src=\"~Desktop/bokeh.png\"/>"
  :type 'string
  :group 'muse-slidy)

(defcustom muse-slidy-override-cover-image ""
"An openning background image only used in the cover in some themes. Note this is markup, not just the name of the file, to allow for flexibility. Example: <img src=\"~Desktop/bokeh.png\"/>"
  :type 'string
  :group 'muse-slidy)


(defcustom muse-slidy-override-slidy-css ""
"Override the whole code of slidy css.Use this only if you know what you are sure you know what you are doing."
  :type 'string
  :group 'muse-slidy)








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


(defcustom muse-slidy-extension ".xhtml"
  "Default file extension for publishing muse slidy files."
  :type 'string
  :group 'muse-slidy)

(muse-derive-style "slidy" "xhtml"
		   :suffix 'muse-slidy-extension
                   :strings 'muse-slidy-markup-strings
                   :header 'muse-slidy-header
                   :footer 'muse-slidy-footer)

(provide 'muse-slidy)






