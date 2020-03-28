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
   (entity . org-mom-entity)
   (headline . org-mom-headline)
   (inline-src-block . org-mom-inline-src-block)
   (paragraph . org-mom-paragraph)
   (plain-text . org-mom-plain-text)
   (section . org-mom-section)
   (template . org-mom-template))
 :menu-entry
 '(?m "Export to MOM"
      ((?m "As MOM file" org-mom-export-to-mom))))



;;; User Configurable Variables


;;; Preamble



;;; Internal Functions



;;; Template

(defun org-mom-template (contents info)
  "Return complete document string after Groff conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "\\# -*- mode:nroff -*-\n.TITLE Not Yet\n.PRINTSTYLE TYPESET\n.START\n%s" contents))



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

(defun org-mom-entity (entity contents info)
  "Transcode an ENTITY object from Org to Mom Groff.
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

;;; Plain Text
(defun org-mom-plain-text (text info)
  (format "%s" text))

;;; Paragraph
(defun org-mom-paragraph (paragraph contents info)
  (format ".PP\n%s" contents))

;;; Section
(defun org-mom-section (section contents info)
  (format "%s" contents))


;;; Interactive functions

(defun org-mom-export-to-mom
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Groff file.

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
;;; ox-groff.el ends here
