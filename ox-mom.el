;;; ox-mom.el --- Groff MOM Macro Back-End for Org Export Engine

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;
;; This library implements a Groff MOM Macro back-end for Org
;; generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'mom "*Test MOM Groff*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Groff
;; export.  See ox.el for more details on how this exporter works.
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox)


;;; Define Back-End

(org-export-define-backend
 'mom
 '((bold . org-mom-bold)
   (center-block . org-mom-center-block)
   (clock . org-mom-clock)
   (code . org-mom-code)
   (drawer . org-mom-not-implemented)
   (dynamic-block . org-mom-not-implemented)
   (entity . org-mom-entity)
   (example-block . org-mom-not-implemented)
   (export-block . org-mom-not-implemented)
   (export-snippet . org-mom-not-implemnted)
   (fixed-width . org-mom-not-implemnted)
   (footnote-definition . org-mom-not-implemnted)
   (footnote-reference . org-mom-not-implemnted)
   (headline . org-mom-headline)
   (horizontal-rule . org-mom-not-implemnted)
   (inline-src-block . org-mom-inline-src-block)
   (inlinetask . org-mom-not-implemnted)
   (italic . org-mom-not-implemnted)
   (item . org-mom-item)
   (keyword . org-mom-not-implemnted)
   (line-break . org-mom-not-implemnted)
   (link . org-mom-not-implemnted)
   (node-property . org-mom-not-implemnted)
   (paragraph . org-mom-paragraph)
   (plain-list . org-mom-plain-list)
   (plain-text . org-mom-plain-text)
   (planning . org-mom-not-implemnted)
   (property-drawer . org-mom-not-implemnted)
   (quote-block . org-mom-quote-block)
   (radio-target . org-mom-not-implemnted)
   (section . org-mom-section)
   (special-block . org-mom-not-implemnted)
   (src-block . org-mom-src-block)
   (statistics-cookie . org-mom-not-implemnted)
   (strike-through . org-mom-not-implemnted)
   (subscript . org-mom-not-implemnted)
   (superscript . org-mom-not-implemnted)
   (table . org-mom-not-implemnted)
   (table-cell . org-mom-not-implemnted)
   (table-row . org-mom-not-implemnted)
   (target . org-mom-not-implemnted)
   (template . org-mom-template)
   (timestamp . org-mom-timestamp)
   (underline . org-mom-not-implemnted)
   (verbatim . org-mom-not-implemnted)
   (verse-block . org-mom-verse-block))

 :menu-entry
 '(?m "Export to MOM"
      ((?m "As MOM file" org-mom-export-to-mom))))



;;; User Configurable Variables
(defgroup org-export-mom nil
  "Options for exporting Org mode files to MOM."
  :tag "Org Export MOM"
  :group 'org-export)

(defcustom org-mom-active-timestamp-format "\\f[I]%s\\f[P]"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-mom
  :type 'string)

(defcustom org-mom-inactive-timestamp-format "\\f[I]%s\\f[P]"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-mom
  :type 'string)

(defcustom org-mom-diary-timestamp-format "\\f[I]%s\\f[P]"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-mom
  :type 'string)

;;; Preamble



;;; Internal Functions
(defun org-mom--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-mom--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s\n.br\n" label) output))))


;;; Template

(defun org-mom-template (contents info)
  "Return complete document string after MOM conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (and (plist-get info :with-title)
		    (let ((temp-title (plist-get info :title)))
		      (and temp-title (org-export-data temp-title info)))))
	(author (and (plist-get info :with-author)
		     (let ((auth (plist-get info :author)))
		       (and auth (org-export-data auth info))))))
    (concat
     (format "\\# -*-mode:nroff -*-\n")
     (when title
       (format ".TITLE \"%s\"\n" title))
     (when author
       (format ".AUTHOR \"%s\"\n" author))
     (format ".PRINTSTYLE TYPESET\n.START\n%s" contents))))


;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-mom-bold (bold contents info)
  "Transcode BOLD from Org to MOM.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "\\*[BOLDER]%s\\*[BOLDERX]" contents))

;;; Center block
(defun org-mom-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to MOM.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-mom--wrap-label
   center-block
   (format ".CENTER_BLOCK\n%s\n.CENTER_BLOCK OFF" contents)))

;;; Clock
(defun org-mom-clock (clock contents info)
  "Transcode a CLOCK element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (format "\\f[B]%s\\f[P] " org-clock-string)
   (format org-mom-inactive-timestamp-format
           (concat (org-timestamp-translate (org-element-property :value clock))
                   (let ((time (org-element-property :duration clock)))
                     (and time (format " (%s)" time)))))))
;;; Code
  "Transcode a CODE object from Org to MOM.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
(defun org-mom-code (code contents info)
  (format ".CODE\n%s\n.CODE OFF\n"
	  (org-element-property :value code)))

;;; Entity
(defun org-mom-entity (entity contents info)
  "Transcode an ENTITY object from Org to Mom.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :utf-8 entity))

;;; Headline
(defun org-mom-headline (headline contents info)
  (let* ((level (org-export-get-relative-level headline info))
	 (title (org-export-data (org-element-property :title headline) info)))
    (format ".HEADING %s \"%s\"\n%s" level title contents)))

;;; inline-src-block
(defun org-mom-inline-src-block (inline-src-block contents info)
  (format "%s" contents))

;;; item
(defun org-mom-item (item contents info)
  "Transcode an ITEM element from Org to MOM.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format ".ITEM %s\n" (org-trim contents)))

;;; Not-implemented
(defun org-mom-not-implemented (text contents info)
  (format "text %s\n" text)
  (format "contents %s\n" contents)
  (format "info %s\n" info))

;;; Plain List
(defun org-mom-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Mom.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attr (mapconcat #'identity
                          (org-element-property :attr_mom plain-list)
                          " "))
         (mom-type (cond
		    ((eq type 'ordered) ".LIST DIGIT")
                      ((eq type 'unordered) ".LIST BULLET")
                      ((eq type 'descriptive) ".LIST ALPHA"))))
    (org-mom--wrap-label
     plain-list
     (format "%s\n%s\n.LIST OFF\n" mom-type contents))))

;;; Plain Text
(defun org-mom-plain-text (text info)
  (format "%s" text))

;;; Paragraph
(defun org-mom-paragraph (paragraph contents info)
  (format ".PP\n%s" contents))

;;; Quote
(defun org-mom-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to MOM.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".QUOTE\n%s\n.QUOTE OFF" contents))

;;; Section
(defun org-mom-section (section contents info)
  (format "%s" contents))

;;; src-block
(defun org-mom-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to MOM
Contents holds the contenst of the item.  INFO is a plist
holding contextual information."
  (let* ((lang (org-element-property :language src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (num-start (org-export-get-loc src-block info))
         (retain-labels (org-element-property :retain-labels src-block)))
  (format ".QUOTE\n.CODE\n%s.QUOTE OFF\n"
	  (org-export-format-code-default src-block info))))

;;; timestamp
(defun org-mom-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-mom-plain-text
		(org-timestamp-translate timestamp) info)))
    (case (org-element-property :type timestamp)
      ((active active-range)
       (format org-mom-active-timestamp-format value))
      ((inactive inactive-range)
       (format org-mom-inactive-timestamp-format value))
      (t (format org-mom-diary-timestamp-format value)))))
;;; Verse
(defun org-mom-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to MOM.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".QUOTE\n%s\n.QUOTE OFF" contents))


;;; Interactive functions

(defun org-mom-export-to-mom
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Mom file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".mom" subtreep)))
    (org-export-to-file 'mom outfile
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-mom)
;;; ox-mom.el ends here
