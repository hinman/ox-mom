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

(require 'cl-lib)
(require 'ox)


;;; Define Back-End

(org-export-define-backend
 'mom
 '((bold . org-mom-bold)
   (center-block . org-mom-center-block)
   (clock . org-mom-clock)
   (code . org-mom-code)
   (drawer . org-mom-drawer)
   (dynamic-block . org-mom-dynamic-block)
   (entity . org-mom-entity)
   (example-block . org-mom-example-block)
   (export-block . org-mom-export-block)
   (export-snippet . org-mom-export-snippet)
   (fixed-width . org-mom-fixed-width)
   (footnote-definition . org-mom-footnote-definition)
   (footnote-reference . org-mom-footnote-reference)
   (headline . org-mom-headline)
   (horizontal-rule . org-mom-horizontal-rule)
   (inline-src-block . org-mom-inline-src-block)
   (inlinetask . org-mom-inlinetask)
   (italic . org-mom-italic)
   (item . org-mom-item)
   (keyword . org-mom-keyword)
   (line-break . org-mom-line-break)
   (link . org-mom-link)
   (node-property . org-mom-node-property)
   (paragraph . org-mom-paragraph)
   (plain-list . org-mom-plain-list)
   (plain-text . org-mom-plain-text)
   (planning . org-mom-planning)
   (property-drawer . org-mom-property-drawer)
   (quote-block . org-mom-quote-block)
   (radio-target . org-mom-radio-target)
   (section . org-mom-section)
   (special-block . org-mom-special-block)
   (src-block . org-mom-src-block)
   (statistics-cookie . org-mom-statistics-cookie)
   (strike-through . org-mom-strike-through)
   (subscript . org-mom-subscript)
   (superscript . org-mom-superscript)
   (table . org-mom-table)
   (table-cell . org-mom-table-cell)
   (table-row . org-mom-table-row)
   (target . org-mom-target)
   (template . org-mom-template)
   (timestamp . org-mom-timestamp)
   (underline . org-mom-underline)
   (verbatim . org-mom-verbatim)
   (verse-block . org-mom-verse-block))

 :menu-entry
 '(?m "Export to MOM"
      ((?b "As MOM buffer" org-mom-export-as-mom)
       (?f "As MOM file" org-mom-export-to-mom)
       (?p "As PDF file" org-mom-export-to-pdf)
       (?o "As PDF file and open"
	   (lambda (a s v b)
	     (if a (org-mom-export-to-pdf t s v b)
	       (org-open-file (org-mom-export-to-pdf nil s v b)))))))

 :options-alist
 '((:mom-doctype  "MOM_DOCTYPE"  nil nil nil)
   (:mom-to       "MOM_TO"       nil nil newline)
   (:mom-from     "MOM_FROM"     nil nil newline)
   (:mom-cc       "MOM_CC"       nil nil newline)
   (:mom-subject  "MOM_SUBJECT"  nil nil nil)
   (:mom-greeting "MOM_GREETING" nil nil nil)
   (:mom-closing  "MOM_CLOSING"  nil nil nil)))



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

(defcustom org-mom-pdf-process
  '("pdfmom -t -K utf-8 %f > %b.pdf"
    "pdfmom -t -K utf-8 %f > %b.pdf"
    "pdfmom -t -K utf-8 %f > %b.pdf")
  "Commands to process a MOM file to a PDF file.
%f is replaced by the full file name, %b by the file base name
\(without extension), and %o by the base directory of the file.
-t invokes the tbl preprocessor (required for tables).
-K utf-8 sets the input encoding for Unicode characters."
  :group 'org-export-mom
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of pdfmom"
                 ("pdfmom -t -K utf-8 %f > %b.pdf"
                  "pdfmom -t -K utf-8 %f > %b.pdf"))
          (const :tag "3 runs of pdfmom"
                 ("pdfmom -t -K utf-8 %f > %b.pdf"
                  "pdfmom -t -K utf-8 %f > %b.pdf"
                  "pdfmom -t -K utf-8 %f > %b.pdf"))
	  (function)))

(defcustom org-mom-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as MOM logfiles."
  :group 'org-export-mom
  :type '(repeat (string :tag "Extension")))

(defcustom org-mom-paper "LETTER"
  "Paper size passed to MOM's .PAPER macro.
Common values: LETTER, LEGAL, A4, A5."
  :group 'org-export-mom
  :type 'string)

(defcustom org-mom-font-family "T"
  "Font family passed to MOM's .FAMILY macro.
T=Times, H=Helvetica, P=Palatino, C=Courier."
  :group 'org-export-mom
  :type 'string)

(defcustom org-mom-pt-size 12
  "Point size passed to MOM's .PT_SIZE macro."
  :group 'org-export-mom
  :type 'integer)

(defcustom org-mom-doctype "DEFAULT"
  "Argument passed to MOM's .DOCTYPE macro.
Common values: DEFAULT, CHAPTER, LETTER.  Use NAMED followed by a
name string for named document types, e.g. \"NAMED \\\"Report\\\"\"."
  :group 'org-export-mom
  :type 'string)

(defcustom org-mom-format-headline-function nil
  "Function to format headline text in MOM output.
When non-nil, called with five arguments: TODO, TODO-TYPE,
PRIORITY, TEXT, TAGS, and should return a formatted string.
When nil the default formatting is used."
  :group 'org-export-mom
  :type '(choice (const nil) function))

(defcustom org-mom-format-drawer-function nil
  "Function to format drawer contents in MOM output.
When non-nil, called with two arguments NAME and CONTENTS and
should return a string.  When nil, drawer contents are passed
through as-is."
  :group 'org-export-mom
  :type '(choice (const nil) function))

(defcustom org-mom-tables-centered t
  "When non-nil, tables are exported with the tbl \"center\" option."
  :group 'org-export-mom
  :type 'boolean)

(defcustom org-mom-inline-image-rules
  '(("file" . "\\.\\(jpg\\|png\\|pdf\\|ps\\|eps\\|pic\\)\\'")
    ("fuzzy" . "\\.\\(jpg\\|png\\|pdf\\|ps\\|eps\\|pic\\)\\'"))
  "Rules characterizing image files that can be inlined into MOM.
Each rule is an alist entry whose key is the link type and value
is a regexp matched against the link path."
  :group 'org-export-mom
  :type '(alist :key-type (string :tag "Type")
                :value-type (regexp :tag "Path")))

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
  (let* ((title (and (plist-get info :with-title)
		     (let ((temp-title (plist-get info :title)))
		       (and temp-title (org-export-data temp-title info)))))
	 (author (and (plist-get info :with-author)
		      (let ((auth (plist-get info :author)))
			(and auth (org-export-data auth info)))))
	 (doctype     (or (plist-get info :mom-doctype) org-mom-doctype))
	 (letter-p    (string= (upcase doctype) "LETTER"))
	 (mom-date     (and letter-p
			    (let ((d (plist-get info :date)))
			      (and d (org-export-data d info)))))
	 (mom-to       (and letter-p (plist-get info :mom-to)))
	 (mom-from     (and letter-p (plist-get info :mom-from)))
	 (mom-cc       (and letter-p (plist-get info :mom-cc)))
	 (mom-subject  (and letter-p (plist-get info :mom-subject)))
	 (mom-greeting (and letter-p (plist-get info :mom-greeting)))
	 (mom-closing  (and letter-p (plist-get info :mom-closing))))
    (concat
     (format "\\# -*-mode:nroff -*-\n")
     (format ".PAPER %s\n" org-mom-paper)
     (format ".FAMILY %s\n" org-mom-font-family)
     (format ".PT_SIZE %s\n" org-mom-pt-size)
     (format ".DOCTYPE %s\n" doctype)
     (format ".PRINTSTYLE TYPESET\n")
     (when title
       (format ".TITLE \"%s\"\n" title))
     (when author
       (format ".AUTHOR \"%s\"\n" author))
     ".START\n"
     (when mom-date    (format ".DATE\n%s\n" mom-date))
     (when mom-to      (format ".TO\n%s\n" mom-to))
     (when mom-cc      (format ".CC\n%s\n" mom-cc))
     (when mom-from    (format ".FROM\n%s\n" mom-from))
     (when mom-subject (format ".SUBJECT \"%s\"\n" mom-subject))
     (when mom-greeting (format ".GREETING\n%s\n" mom-greeting))
     contents
     (when mom-closing (format ".CLOSING\n%s\n" mom-closing)))))


;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-mom-bold (bold contents info)
  "Transcode BOLD from Org to MOM.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "\\f[B]%s\\f[P]" contents))

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
(defun org-mom-code (code _contents _info)
  "Transcode a CODE object from Org to MOM.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (format "\\*[CODE]%s\\*[CODE OFF]"
	  (org-element-property :value code)))

;;; Entity

(defconst org-mom--entity-alist
  ;; Maps Org entity names to groff \[name] character escapes.
  '(;; Greek uppercase
    ("Alpha" . "\\[*A]") ("Beta"  . "\\[*B]") ("Gamma"  . "\\[*G]")
    ("Delta" . "\\[*D]") ("Epsilon" . "\\[*E]") ("Zeta"  . "\\[*Z]")
    ("Eta"   . "\\[*Y]") ("Theta"  . "\\[*H]") ("Iota"  . "\\[*I]")
    ("Kappa" . "\\[*K]") ("Lambda" . "\\[*L]") ("Mu"    . "\\[*M]")
    ("Nu"    . "\\[*N]") ("Xi"     . "\\[*C]") ("Pi"    . "\\[*P]")
    ("Rho"   . "\\[*R]") ("Sigma"  . "\\[*S]") ("Tau"   . "\\[*T]")
    ("Upsilon" . "\\[*U]") ("Phi"  . "\\[*F]") ("Chi"   . "\\[*X]")
    ("Psi"   . "\\[*Q]") ("Omega"  . "\\[*W]")
    ;; Greek lowercase
    ("alpha" . "\\[*a]") ("beta"  . "\\[*b]") ("gamma" . "\\[*g]")
    ("delta" . "\\[*d]") ("epsilon" . "\\[*e]") ("zeta" . "\\[*z]")
    ("eta"   . "\\[*y]") ("theta"  . "\\[*h]") ("iota" . "\\[*i]")
    ("kappa" . "\\[*k]") ("lambda" . "\\[*l]") ("mu"   . "\\[*m]")
    ("nu"    . "\\[*n]") ("xi"     . "\\[*c]") ("pi"   . "\\[*p]")
    ("rho"   . "\\[*r]") ("sigma"  . "\\[*s]") ("tau"  . "\\[*t]")
    ("upsilon" . "\\[*u]") ("phi"  . "\\[*f]") ("chi"  . "\\[*x]")
    ("psi"   . "\\[*q]") ("omega"  . "\\[*w]")
    ;; Math
    ("infin" . "\\[if]") ("plusmn" . "\\[+-]") ("times"  . "\\[mu]")
    ("divide" . "\\[di]") ("ne"    . "\\[!=]") ("le"    . "\\[<=]")
    ("ge"    . "\\[>=]") ("sim"   . "\\[ap]")  ("cong"  . "\\[~=]")
    ("sum"   . "\\[sum]") ("prod"  . "\\[product]")
    ("part"  . "\\[pd]")  ("nabla" . "\\[nb]")  ("int"  . "\\[is]")
    ("radic" . "\\[sr]")  ("prop"  . "\\[pt]")
    ("and"   . "\\[AN]")  ("or"    . "\\[OR]")
    ("cap"   . "\\[ca]")  ("cup"   . "\\[cu]")
    ("sub"   . "\\[su]")  ("sup"   . "\\[sp]")
    ("sube"  . "\\[ib]")  ("supe"  . "\\[ip]")
    ("isin"  . "\\[mo]")  ("notin" . "\\[nm]")
    ("empty" . "\\[es]")  ("exist" . "\\[te]")  ("forall" . "\\[fa]")
    ("ang"   . "\\[ag]")  ("perp"  . "\\[pp]")
    ("uarr"  . "\\[ua]")  ("darr"  . "\\[da]")
    ("rarr"  . "\\[->]")  ("larr"  . "\\[<-]")  ("harr" . "\\[<>]")
    ;; Typography
    ("mdash" . "\\[em]")  ("ndash" . "\\[en]")
    ("laquo" . "\\[Fo]")  ("raquo" . "\\[Fc]")
    ("lsquo" . "\\[oq]")  ("rsquo" . "\\[cq]")
    ("ldquo" . "\\[lq]")  ("rdquo" . "\\[rq]")
    ("bull"  . "\\[bu]")  ("dagger" . "\\[dg]") ("Dagger" . "\\[dd]")
    ("deg"   . "\\[de]")  ("sect"  . "\\[sc]")  ("para"  . "\\[ps]")
    ("copy"  . "\\[co]")  ("reg"   . "\\[rg]")  ("trade" . "\\[tm]")
    ("pound" . "\\[Po]")  ("euro"  . "\\[Eu]")  ("yen"   . "\\[Ye]")
    ("cent"  . "\\[ct]")  ("curren" . "\\[Cs]")
    ;; Accented Latin
    ("aelig" . "\\[ae]")  ("AElig" . "\\[AE]")
    ("oelig" . "\\[oe]")  ("OElig" . "\\[OE]")
    ("szlig" . "\\[ss]"))
  "Alist mapping Org entity names to groff named character escapes.")

(defun org-mom-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to MOM.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let* ((name (org-element-property :name entity))
         (groff (cdr (assoc name org-mom--entity-alist))))
    (or groff (org-element-property :utf-8 entity))))

;;; Headline
(defun org-mom-headline (headline contents info)
  "Transcode a HEADLINE element from Org to MOM.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level    (org-export-get-relative-level headline info))
	 (text     (org-export-data (org-element-property :title headline) info))
	 (todo     (and (plist-get info :with-todo-keywords)
			(let ((kw (org-element-property :todo-keyword headline)))
			  (and kw (org-export-data kw info)))))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (tags     (and (plist-get info :with-tags)
			(org-export-get-tags headline info)))
	 ;; Escape double quotes so they don't break the .HEADING argument.
	 (safe-text (replace-regexp-in-string "\"" "\\\\(dq" text))
	 (full-text
	  (if (functionp org-mom-format-headline-function)
	      (funcall org-mom-format-headline-function
		       todo
		       (org-element-property :todo-type headline)
		       priority safe-text tags)
	    (concat
	     (when todo     (format "\\f[B]%s\\f[P] " todo))
	     (when priority (format "[\\#%c] " priority))
	     safe-text
	     (when tags
	       (format " \\f[I]%s\\f[P]" (org-make-tag-string tags))))))
	 (low-level-p (or (> level 6)
			  (org-export-low-level-p headline info)))
	 (numberedp   (org-export-numbered-headline-p headline info)))
    (cond
     ;; Footnote section: skip entirely.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Deep headline: render as a list item.
     (low-level-p
      (concat
       (when (org-export-first-sibling-p headline info)
	 (if numberedp ".LIST DIGIT\n" ".LIST BULLET\n"))
       ".ITEM\n" full-text "\n"
       (or contents "")
       (when (org-export-last-sibling-p headline info)
	 ".LIST OFF\n")))
     ;; Standard heading.
     (t
      (format ".HEADING %s \"%s\"\n%s"
	      level full-text (or contents ""))))))

;;; inline-src-block
(defun org-mom-inline-src-block (inline-src-block contents info)
  (format "%s" contents))

;;; italic
(defun org-mom-italic (italic contents info)
  "Transcode italic from Org to MOM.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "\\f[I]%s\\f[P]" contents))

;;; Keyword
(defun org-mom-keyword (keyword _contents _info)
  "Transcode a KEYWORD element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key   (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "MOM") value)
     ((string= key "TOC") ".TOC\n")
     (t nil))))

;;; item
(defun org-mom-item (item contents info)
  "Transcode an ITEM element from Org to MOM.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property
                :type (org-element-property :parent item)))
         (checkbox (cl-case (org-element-property :checkbox item)
                     (on    "\\o'\\(sq\\(mu' ")
                     (off   "\\(sq ")
                     (trans "\\o'\\(sq\\(mi' ")))
         (tag  (let ((raw (org-element-property :tag item)))
                 (and raw (org-export-data raw info)))))
    (if (and tag (eq type 'descriptive))
        (format ".ITEM \"%s\"\n%s\n" tag (org-trim contents))
      (format ".ITEM\n%s%s\n" (or checkbox "") (org-trim contents)))))

;;; Not-implemented
(defun org-mom-not-implemented (_text _contents _info)
  nil)

;;; Plain List
(defun org-mom-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Mom.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (mom-type
          (cond
           ((eq type 'ordered)   ".LIST DIGIT")
           ((eq type 'unordered) ".LIST BULLET")
           ((eq type 'descriptive)
            ;; Use VARIABLE so each .ITEM "tag" gets a hanging label.
            ;; Pass the widest tag text so MOM can size the label column.
            (let ((widest
                   (or (car (sort
                             (org-element-map plain-list 'item
                               (lambda (i)
                                 (let ((tag (org-element-property :tag i)))
                                   (when tag (org-export-data tag info))))
                               info)
                             (lambda (a b) (> (length a) (length b)))))
                       "")))
              (format ".LIST VARIABLE \"%s\"" widest))))))
    (org-mom--wrap-label
     plain-list
     (format "%s\n%s\n.LIST OFF\n" mom-type contents))))

;;; Plain Text
(defun org-mom-plain-text (text info)
  "Transcode a TEXT string from Org to MOM.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Escape backslashes first, before any other substitutions.
    (setq output (replace-regexp-in-string "\\\\" "\\\\e" output))
    ;; Prevent lines starting with . or ' from being interpreted as macros.
    ;; \& is groff's zero-width non-printing character.
    (setq output (replace-regexp-in-string "^[.']" "\\\\&\\&" output))
    ;; Activate smart quotes when requested.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :utf-8 info text)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(\\\\\\\\\\)?[ \t]*\n" ".br\n" output)))
    output))

;;; Paragraph
(defun org-mom-paragraph (paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to MOM.
CONTENTS is the paragraph text.  INFO is a plist holding
contextual information."
  (let ((parent-type (org-element-type (org-element-property :parent paragraph))))
    (if (memq parent-type '(item quote-block verse-block footnote-definition))
	contents
      (concat ".PP\n" contents))))

;;; Planning
(defun org-mom-planning (planning _contents info)
  "Transcode a PLANNING element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   (mapconcat
    #'identity
    (delq nil
          (list
           (let ((closed (org-element-property :closed planning)))
             (when closed
               (concat
                (format "\\f[B]%s\\f[P] " org-closed-string)
                (format org-mom-inactive-timestamp-format
                        (org-timestamp-translate closed)))))
           (let ((deadline (org-element-property :deadline planning)))
             (when deadline
               (concat
                (format "\\f[B]%s\\f[P] " org-deadline-string)
                (format org-mom-active-timestamp-format
                        (org-timestamp-translate deadline)))))
           (let ((scheduled (org-element-property :scheduled planning)))
             (when scheduled
               (concat
                (format "\\f[B]%s\\f[P] " org-scheduled-string)
                (format org-mom-active-timestamp-format
                        (org-timestamp-translate scheduled)))))))
    " ")
   "\n"))

;;; Quote
(defun org-mom-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to MOM.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".BLOCKQUOTE\n%s\n.BLOCKQUOTE OFF" contents))

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
  (format ".QUOTE\n.CODE\n%s.CODE OFF\n.QUOTE OFF\n"
	  (org-export-format-code-default src-block info))))

;;; strike-through
(defun org-mom-strike-through (strike-through contents info)
  (format "\n.UNDERSCORE -0.3m \"%s\"\n" contents))

;;; superscript
(defun org-mom-subscript (subscript contents info)
  (format "\n.SUPERSCRIPT_RAISE_AMOUNT -0.3m\n\\*[SUP]%s\\*[SUPX]\n.SUPERSCRIPT_RAISE_AMOUNT 0.3m\n" contents))

;;; superscript
(defun org-mom-superscript (superscript contents info)
  (format "\\*[SUP]%s\\*[SUPX]" contents))

;;; timestamp
(defun org-mom-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-mom-plain-text
		(org-timestamp-translate timestamp) info)))
    (cl-case (org-element-property :type timestamp)
      ((active active-range)
       (format org-mom-active-timestamp-format value))
      ((inactive inactive-range)
       (format org-mom-inactive-timestamp-format value))
      (t (format org-mom-diary-timestamp-format value)))))

;;; Underline
(defun org-mom-underline (underline contents info)
  (format "\n.UNDERSCORE \"%s\"\n" contents))

;;; Verbatim
(defun org-mom-verbatim (verbatim _contents _info)
  "Transcode a verbatim element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "\\*[CODE]%s\\*[CODE OFF]"
	  (org-element-property :value verbatim)))
  
;;; Verse
(defun org-mom-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to MOM.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".VERSE\n%s\n.VERSE OFF" contents))


;;; Drawer
(defun org-mom-drawer (drawer contents _info)
  "Transcode a DRAWER element from Org to MOM.
CONTENTS holds the contents of the drawer.  INFO is a plist holding
contextual information."
  (if (functionp org-mom-format-drawer-function)
      (funcall org-mom-format-drawer-function
	       (org-element-property :drawer-name drawer)
	       contents)
    contents))

;;; Example Block
(defun org-mom-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-mom--wrap-label
   example-block
   (format ".CODE\n%s\n.CODE OFF\n"
	   (org-export-format-code-default example-block info))))

;;; Fixed Width
(defun org-mom-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format ".CODE\n%s\n.CODE OFF\n"
	  (org-trim (org-remove-indentation
		     (org-element-property :value fixed-width)))))

;;; Inlinetask
(defun org-mom-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to MOM.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let* ((title (org-export-data (org-element-property :title inlinetask) info))
	 (todo  (and (plist-get info :with-todo-keywords)
		     (let ((kw (org-element-property :todo-keyword inlinetask)))
		       (and kw (org-export-data kw info)))))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority inlinetask)))
	 (tags  (and (plist-get info :with-tags)
		     (org-export-get-tags inlinetask info)))
	 (full-title
	  (concat
	   (when todo     (format "\\f[B]%s\\f[P] " todo))
	   (when priority (format "[\\#%c] " priority))
	   title
	   (when tags
	     (format " \\f[C]%s\\f[P]"
		     (mapconcat #'identity tags ":"))))))
    (format "\n.DS I\n%s\n.SP\n%s\n.DE\n" full-title (or contents ""))))

;;; Special Block
(defun org-mom-special-block (special-block contents _info)
  "Transcode a SPECIAL-BLOCK element from Org to MOM.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (org-mom--wrap-label special-block (or contents "")))

;;; Dynamic Block
(defun org-mom-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to MOM.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  contents)

;;; Export Block
(defun org-mom-export-block (export-block _contents _info)
  "Transcode an EXPORT-BLOCK element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "MOM")
    (org-remove-indentation (org-element-property :value export-block))))

;;; Export Snippet
(defun org-mom-export-snippet (export-snippet _contents _info)
  "Transcode an EXPORT-SNIPPET object from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'mom)
    (org-element-property :value export-snippet)))

;;; Footnote Definition
(defun org-mom-footnote-definition (_footnote-definition _contents _info)
  "Transcode a FOOTNOTE-DEFINITION element from Org to MOM.
Definitions are emitted inline at their reference site."
  nil)

;;; Footnote Reference
(defun org-mom-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information.

MOM footnotes are emitted inline: \\c ends the current text run,
then .FOOTNOTE/.FOOTNOTE OFF wraps the definition, and text after
.FOOTNOTE OFF continues the paragraph."
  (let* ((raw (org-export-get-footnote-definition footnote-reference info))
	 (data (org-trim (org-export-data raw info))))
    (format "\\c\n.FOOTNOTE\n%s\n.FOOTNOTE OFF\n" data)))

;;; Horizontal Rule
(defun org-mom-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a HORIZONTAL-RULE element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".RULE\n")

;;; Line Break
(defun org-mom-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".br\n")

;;; Link

(defun org-mom-link--inline-image (link info)
  "Return MOM markup for an inline image LINK.
INFO is a plist used as a communication channel."
  (let* ((path (let ((raw (org-element-property :path link)))
                 (if (file-name-absolute-p raw) (expand-file-name raw) raw)))
         (attr (org-export-read-attribute :attr_mom link))
         (width  (or (plist-get attr :width)  ""))
         (height (or (plist-get attr :height) "")))
    (format "\n.PDF_IMAGE \"%s\" %s %s\n" path width height)))

(defun org-mom-link (link desc info)
  "Transcode a LINK object from Org to MOM.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((type     (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (desc     (and (not (string= desc "")) desc))
         (imagep   (org-export-inline-image-p link org-mom-inline-image-rules))
         (path     (cond
                    ((member type '("http" "https" "ftp" "mailto"))
                     (concat type ":" raw-path))
                    ((string= type "file") (org-export-file-uri raw-path))
                    (t raw-path))))
    (cond
     ;; Custom protocol.
     ((org-export-custom-protocol-maybe link desc 'mom info))
     ;; Inline image.
     (imagep (org-mom-link--inline-image link info))
     ;; Include a raw MOM file.
     ((and (string= type "file") (string-match "\\.mom\\'" raw-path))
      (format ".so %s\n" raw-path))
     ;; Internal link: fuzzy / id / custom-id.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          (`plain-text
           ;; ID link pointing to an external file.
           (if desc (format "%s \\fBat\\fP \\fIfile://%s\\fP" desc destination)
             (format "\\fIfile://%s\\fP" destination)))
          (`nil
           ;; Unresolvable fuzzy link: just italicise whatever we have.
           (format "\\fI%s\\fP" (or desc raw-path)))
          (`headline
           (format "\\fI%s\\fP"
                   (or desc
                       (org-export-data
                        (org-element-property :title destination) info))))
          (_
           (format "\\fI%s\\fP"
                   (or desc (org-export-get-reference destination info)))))))
     ;; External link with description.
     ((and path desc) (format "%s \\fBat\\fP \\fI%s\\fP" desc path))
     ;; External link without description.
     (path (format "\\fI%s\\fP" path))
     ;; No path — use description if available.
     (t (format "\\fI%s\\fP" (or desc ""))))))

;;; Node Property
(defun org-mom-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "%s:%s"
	  (org-element-property :key node-property)
	  (let ((value (org-element-property :value node-property)))
	    (if value (concat " " value) ""))))

;;; Property Drawer
(defun org-mom-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to MOM.
CONTENTS holds the contents of the drawer.  INFO is a plist holding
contextual information."
  (and (org-string-nw-p contents)
       (format ".CODE\n%s\n.CODE OFF\n" contents)))

;;; Radio Target
(defun org-mom-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to MOM.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "%s" text))

;;; Statistics Cookie
(defun org-mom-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))

;;; Target
(defun org-mom-target (target _contents info)
  "Transcode a TARGET object from Org to MOM.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format ".PDF_TARGET \"%s\"\n"
	  (org-export-get-reference target info)))

;;; Table

(defvar orgtbl-exp-regexp)

(defun org-mom-table--align-string (divider table info)
  "Return a tbl column-format string for TABLE.
DIVIDER is the inter-column separator (\"|\", or \" \").
INFO is a plist used as a communication channel."
  (let (alignment)
    (org-element-map
	(org-element-map table 'table-row
	  (lambda (row)
	    (and (eq (org-element-property :type row) 'standard) row))
	  info 'first-match)
	'table-cell
      (lambda (cell)
	(let* ((borders   (org-export-table-cell-borders cell info))
	       (raw-width (org-export-table-cell-width cell info))
	       (width     (if raw-width
			      (format "w(%dc)" (max 1 (/ raw-width 5)))
			    "")))
	  (when (and (memq 'left borders) (not alignment))
	    (push "|" alignment))
	  (push
	   (cl-case (org-export-table-cell-alignment cell info)
	     (left   (concat "l" width divider))
	     (right  (concat "r" width divider))
	     (center (concat "c" width divider)))
	   alignment)
	  (when (memq 'right borders) (push "|" alignment))))
      info)
    (apply #'concat (reverse alignment))))

(defun org-mom-table (table contents info)
  "Transcode a TABLE element from Org to MOM.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let* ((attr      (org-export-read-attribute :attr_mom table))
	 (divider   (if (plist-get attr :divider) "|" " "))
	 (alignment (org-mom-table--align-string divider table info))
	 (title-line (plist-get attr :title-line))
	 (options
	  (mapconcat #'identity
		     (delq nil
			   (list
			    (when (or (plist-get attr :center)
				      org-mom-tables-centered)
			      "center")
			    (or (plist-get attr :boxtype) "box")))
		     ","))
	 (lines      (org-split-string contents "\n"))
	 (first-line (when lines (org-split-string (car lines) "\t"))))
    ;; Verbatim: just reproduce the raw Org table as monospace text.
    (if (plist-get attr :verbatim)
	(format ".DS L\n\\fC%s\\fP\n.DE"
		(org-trim
		 (org-element-interpret-data
		  `(table nil ,@(org-element-contents table)))))
      ;; Standard tbl table.
      (concat
       ".TS\n"
       options ";\n"
       ;; Format spec: optional bold header row, then data alignment.
       (let ((fmt ""))
	 (when title-line
	   (dotimes (_ (length first-line))
	     (setq fmt (concat fmt "cb" divider)))
	   (setq fmt (concat fmt "\n")))
	 (concat fmt
		 (or alignment
		     (mapconcat (lambda (_) (concat "l" divider))
				first-line ""))
		 ".\n"))
       ;; Data rows (already assembled by table-row / table-cell).
       (mapconcat #'identity (delq nil lines) "\n")
       "\n.TE\n"))))

(defun org-mom-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to MOM.
CONTENTS is the tab-separated cell data.  INFO is a plist."
  (when (eq (org-element-property :type table-row) 'standard)
    (let ((borders (org-export-table-cell-borders
		    (car (org-element-contents table-row)) info)))
      (concat
       (when (and (memq 'top borders) (memq 'above borders)) "_\n")
       contents
       (cond
	((and (memq 'bottom borders) (memq 'below borders)) "\n_")
	((memq 'below borders) "\n_"))))))

(defun org-mom-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to MOM.
CONTENTS is the cell text.  INFO is a plist."
  (concat
   (if (and contents
	    (bound-and-true-p orgtbl-exp-regexp)
	    (string-match orgtbl-exp-regexp contents))
       (format "%sE%s" (match-string 1 contents) (match-string 2 contents))
     contents)
   (when (org-export-get-next-element table-cell info) "\t")))


;;; Interactive functions

;;;###autoload
(defun org-mom-export-as-mom
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a MOM GROFF buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org MOM Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'mom "*Org MOM Export*"
    async subtreep visible-only body-only ext-plist (lambda () (nroff-mode))))

;;;###autoload
(defun org-mom-export-to-mom
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a MOM file.

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

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".mom" subtreep)))
    (org-export-to-file 'mom outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-mom-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to MOM then process through to PDF.

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

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".mom" subtreep)))
    (org-export-to-file 'mom outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-mom-compile file)))))

(defun org-mom-compile (file)
  "Compile a MOM file.

FILE is the name of the file being compiled.  Processing is done
through the command specified in `org-mom-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (message "Processing MOM file %s..." file)
  (let ((output (org-compile-file file org-mom-pdf-process "pdf")))
    (message "Process completed.")
    output))

(provide 'ox-mom)
;;; ox-mom.el ends here
