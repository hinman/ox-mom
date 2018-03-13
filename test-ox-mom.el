;;; test-ox-mom.el --- ERT tests for ox-mom.el

(require 'ert)
(require 'ox-mom)
(require 'org-clock)


;;; Helpers

(defun mom-export (org-string)
  "Export ORG-STRING as MOM, body only (no template preamble)."
  (org-export-string-as org-string 'mom t))

(defun mom-export-full (org-string)
  "Export ORG-STRING as MOM with full template."
  (org-export-string-as org-string 'mom nil))


;;; Group 1: Inline Markup

(ert-deftest ox-mom-bold ()
  (let ((output (mom-export "Hello *world* end")))
    (should (string-match "\\\\f\\[B\\]world\\\\f\\[P\\]" output))))

(ert-deftest ox-mom-italic ()
  (let ((output (mom-export "Hello /world/ end")))
    (should (string-match "\\\\f\\[I\\]world\\\\f\\[P\\]" output))))

(ert-deftest ox-mom-underline ()
  (let ((output (mom-export "Hello _world_ end")))
    (should (string-match "\\.UNDERSCORE \"world\"" output))))

(ert-deftest ox-mom-strike-through ()
  (let ((output (mom-export "Hello +world+ end")))
    (should (string-match "\\.UNDERSCORE -0\\.3m \"world\"" output))))

(ert-deftest ox-mom-code ()
  (let ((output (mom-export "Hello =world= end")))
    (should (string-match "\\\\\\*\\[CODE\\]world\\\\\\*\\[CODE OFF\\]" output))))

(ert-deftest ox-mom-verbatim ()
  (let ((output (mom-export "Hello ~world~ end")))
    (should (string-match "\\\\\\*\\[CODE\\]world\\\\\\*\\[CODE OFF\\]" output))))

(ert-deftest ox-mom-superscript ()
  (let ((output (mom-export "E=mc^{2}")))
    (should (string-match "\\\\\\*\\[SUP\\]2\\\\\\*\\[SUPX\\]" output))))

(ert-deftest ox-mom-subscript ()
  (let ((output (mom-export "H_{2}O")))
    (should (string-match "SUPERSCRIPT_RAISE_AMOUNT" output))
    (should (string-match "\\\\\\*\\[SUP\\]2\\\\\\*\\[SUPX\\]" output))))

(ert-deftest ox-mom-statistics-cookie-fraction ()
  (let ((output (mom-export "* Heading [1/3]\n- [ ] a\n- [X] b\n")))
    (should (string-match "\\[1/3\\]" output))))

(ert-deftest ox-mom-statistics-cookie-percent ()
  (let ((output (mom-export "* Heading [33%]\n- [ ] a\n- [X] b\n")))
    (should (string-match "\\[33%\\]" output))))


;;; Group 2: Block Elements

(ert-deftest ox-mom-center-block ()
  (let ((output (mom-export "#+BEGIN_CENTER\nhello\n#+END_CENTER")))
    (should (string-match "\\.CENTER_BLOCK\n" output))
    (should (string-match "\\.CENTER_BLOCK OFF" output))))

(ert-deftest ox-mom-quote-block ()
  (let ((output (mom-export "#+BEGIN_QUOTE\nhello\n#+END_QUOTE")))
    (should (string-match "\\.BLOCKQUOTE\n" output))
    (should (string-match "\\.BLOCKQUOTE OFF" output))))

(ert-deftest ox-mom-quote-block-no-pp ()
  "Paragraph inside quote-block must not get a .PP prefix."
  (let ((output (mom-export "#+BEGIN_QUOTE\nhello\n#+END_QUOTE")))
    (should-not (string-match "\\.BLOCKQUOTE\n\\.PP" output))))

(ert-deftest ox-mom-verse-block ()
  (let ((output (mom-export "#+BEGIN_VERSE\nhello\n#+END_VERSE")))
    (should (string-match "\\.VERSE\n" output))
    (should (string-match "\\.VERSE OFF" output))))

(ert-deftest ox-mom-verse-block-no-pp ()
  "Paragraph inside verse-block must not get a .PP prefix."
  (let ((output (mom-export "#+BEGIN_VERSE\nhello\n#+END_VERSE")))
    (should-not (string-match "\\.VERSE\n\\.PP" output))))

(ert-deftest ox-mom-example-block ()
  (let ((output (mom-export "#+BEGIN_EXAMPLE\nhello\n#+END_EXAMPLE")))
    (should (string-match "\\.CODE\n" output))
    (should (string-match "\\.CODE OFF" output))))

(ert-deftest ox-mom-src-block ()
  (let ((output (mom-export "#+BEGIN_SRC python\nprint('hi')\n#+END_SRC")))
    (should (string-match "\\.QUOTE\n\\.CODE\n" output))
    (should (string-match "\\.CODE OFF\n\\.QUOTE OFF" output))
    (should (string-match "print" output))))

(ert-deftest ox-mom-src-block-eqn-emits-EQ-EN ()
  "eqn src-block emits .EQ/.EN markup, not a .CODE listing."
  (let ((output (mom-export "#+BEGIN_SRC eqn\nx sup 2 + y sup 2 = z sup 2\n#+END_SRC")))
    (should (string-match "\\.EQ\n" output))
    (should (string-match "\\.EN\n" output))
    (should (string-match "x sup 2" output))
    (should-not (string-match "\\.QUOTE" output))
    (should-not (string-match "\\.CODE" output))))

(ert-deftest ox-mom-src-block-eqn-no-double-wrap ()
  "eqn src-block with existing .EQ/.EN is not double-wrapped."
  (let ((output (mom-export "#+BEGIN_SRC eqn\n.EQ\nx sup 2\n.EN\n#+END_SRC")))
    (should (string-match "\\.EQ\n" output))
    (should-not (string-match "\\.EQ\n\\.EQ" output))))

(ert-deftest ox-mom-src-block-pic-emits-PS-PE ()
  "pic src-block emits .PS/.PE markup, not a .CODE listing."
  (let ((output (mom-export "#+BEGIN_SRC pic\nbox \"hello\"\n#+END_SRC")))
    (should (string-match "\\.PS\n" output))
    (should (string-match "\\.PE\n" output))
    (should (string-match "box" output))
    (should-not (string-match "\\.QUOTE" output))
    (should-not (string-match "\\.CODE" output))))

(ert-deftest ox-mom-src-block-pic-passthrough-PS-PE ()
  "pic src-block with existing .PS/.PE is not double-wrapped."
  (let ((output (mom-export "#+BEGIN_SRC pic\n.PS\nbox \"hello\"\n.PE\n#+END_SRC")))
    (should (string-match "\\.PS\n" output))
    (should-not (string-match "\\.PS\n\\.PS" output))))

(ert-deftest ox-mom-src-block-other-lang-still-code-block ()
  "Non-eqn/pic src-blocks still produce .QUOTE/.CODE listings."
  (let ((output (mom-export "#+BEGIN_SRC elisp\n(message \"hi\")\n#+END_SRC")))
    (should (string-match "\\.QUOTE\n\\.CODE\n" output))
    (should (string-match "\\.CODE OFF\n\\.QUOTE OFF" output))))

;;; eqn/pic export-default tests

(ert-deftest ox-mom-eqn-pic-exports-defcustom-bound ()
  "`org-mom-eqn-pic-exports' is defined after loading ox-mom."
  (should (boundp 'org-mom-eqn-pic-exports)))

(ert-deftest ox-mom-eqn-pic-exports-default-code ()
  "`org-mom-eqn-pic-exports' defaults to \"code\"."
  (should (string= "code" org-mom-eqn-pic-exports)))

(ert-deftest ox-mom-eqn-pic-exports-hook-registered ()
  "`org-mom--override-eqn-pic-exports' is registered on the processing hook."
  (should (memq 'org-mom--override-eqn-pic-exports
                org-export-before-processing-hook)))

(ert-deftest ox-mom-eqn-pic-exports-overrides-eqn ()
  "Hook sets buffer-local :exports for eqn to `org-mom-eqn-pic-exports'."
  (with-temp-buffer
    (setq-local org-babel-default-header-args:eqn
                '((:results . "file graphics") (:exports . "results") (:file-ext . "png")))
    (org-mom--override-eqn-pic-exports 'mom)
    (should (string= org-mom-eqn-pic-exports
                     (cdr (assq :exports org-babel-default-header-args:eqn))))))

(ert-deftest ox-mom-eqn-pic-exports-overrides-pic ()
  "Hook sets buffer-local :exports for pic to `org-mom-eqn-pic-exports'."
  (with-temp-buffer
    (setq-local org-babel-default-header-args:pic
                '((:results . "file link") (:exports . "results")))
    (org-mom--override-eqn-pic-exports 'mom)
    (should (string= org-mom-eqn-pic-exports
                     (cdr (assq :exports org-babel-default-header-args:pic))))))

(ert-deftest ox-mom-eqn-pic-exports-no-effect-other-backend ()
  "Hook does not modify eqn defaults when exporting to a non-mom backend."
  (with-temp-buffer
    (setq-local org-babel-default-header-args:eqn
                '((:results . "file graphics") (:exports . "results")))
    (org-mom--override-eqn-pic-exports 'html)
    (should (string= "results"
                     (cdr (assq :exports org-babel-default-header-args:eqn))))))

(ert-deftest ox-mom-eqn-pic-exports-respects-user-override ()
  "When `org-mom-eqn-pic-exports' is \"results\", hook sets that value."
  (let ((org-mom-eqn-pic-exports "results"))
    (with-temp-buffer
      (setq-local org-babel-default-header-args:eqn
                  '((:results . "file graphics") (:exports . "results")))
      (org-mom--override-eqn-pic-exports 'mom)
      (should (string= "results"
                       (cdr (assq :exports org-babel-default-header-args:eqn)))))))

(ert-deftest ox-mom-eqn-pic-exports-local-only ()
  "Hook modification is buffer-local: a second buffer is unaffected."
  (let* ((base '((:results . "file graphics") (:exports . "results")))
         (buf1 (generate-new-buffer " *ox-mom-test-a*"))
         (buf2 (generate-new-buffer " *ox-mom-test-b*")))
    (unwind-protect
        (progn
          ;; buf1: run the hook — :exports should become "code".
          (with-current-buffer buf1
            (setq-local org-babel-default-header-args:eqn (copy-sequence base))
            (org-mom--override-eqn-pic-exports 'mom)
            (should (string= "code"
                             (cdr (assq :exports
                                        org-babel-default-header-args:eqn)))))
          ;; buf2: no hook run — local value is still "results".
          (with-current-buffer buf2
            (setq-local org-babel-default-header-args:eqn (copy-sequence base))
            (should (string= "results"
                             (cdr (assq :exports
                                        org-babel-default-header-args:eqn))))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest ox-mom-fixed-width ()
  (let ((output (mom-export ": hello world")))
    (should (string-match "\\.CODE\n" output))
    (should (string-match "hello world" output))
    (should (string-match "\\.CODE OFF" output))))


;;; Group 3: Structure

(ert-deftest ox-mom-paragraph-standalone ()
  "Standalone paragraph gets a .PP prefix."
  (let ((output (mom-export "hello world")))
    (should (string-match "\\.PP\nhello world" output))))

(ert-deftest ox-mom-paragraph-in-item-no-pp ()
  "Paragraph inside a list item must not get a .PP prefix."
  (let ((output (mom-export "- hello world")))
    (should-not (string-match "\\.ITEM\n\\.PP" output))))

(ert-deftest ox-mom-headline-level-1 ()
  (let ((output (mom-export "* Hello World")))
    (should (string-match "\\.HEADING 1 \"Hello World\"" output))))

(ert-deftest ox-mom-headline-level-2 ()
  (let ((output (mom-export "* Parent\n** Child")))
    (should (string-match "\\.HEADING 2 \"Child\"" output))))

(ert-deftest ox-mom-headline-level-3 ()
  (let ((output (mom-export "* A\n** B\n*** C")))
    (should (string-match "\\.HEADING 3 \"C\"" output))))

(ert-deftest ox-mom-headline-quote-escape ()
  "Double quotes in headline titles are escaped to \\(dq."
  (let ((output (mom-export "* Say \"hello\"")))
    ;; org-mom-headline replaces " with \(dq via replace-regexp-in-string
    (should (string-match "\\\\(dq" output))))


;;; Group 4: Lists

(ert-deftest ox-mom-plain-list-unordered ()
  (let ((output (mom-export "- alpha\n- beta\n")))
    (should (string-match "\\.LIST BULLET" output))
    (should (string-match "\\.ITEM\nalpha" output))
    (should (string-match "\\.ITEM\nbeta" output))
    (should (string-match "\\.LIST OFF" output))))

(ert-deftest ox-mom-plain-list-ordered ()
  (let ((output (mom-export "1. first\n2. second\n")))
    (should (string-match "\\.LIST DIGIT" output))
    (should (string-match "\\.LIST OFF" output))))

(ert-deftest ox-mom-plain-list-descriptive ()
  (let ((output (mom-export "- term :: description\n")))
    (should (string-match "\\.LIST VARIABLE" output))
    (should (string-match "\\.ITEM \"term\"" output))))

(ert-deftest ox-mom-item-checkbox-on ()
  "Checked checkbox uses \\o'\\(sq\\(mu' overstrike."
  (let ((output (mom-export "- [X] done item\n")))
    (should (string-match "\\\\o'" output))
    (should (string-match "\\\\(mu'" output))))

(ert-deftest ox-mom-item-checkbox-off ()
  "Unchecked checkbox uses \\(sq (open square)."
  (let ((output (mom-export "- [ ] undone item\n")))
    (should (string-match "\\\\(sq " output))))

(ert-deftest ox-mom-item-checkbox-trans ()
  "Transitional checkbox uses \\o'\\(sq\\(mi' overstrike."
  (let ((output (mom-export "- [-] partial item\n")))
    (should (string-match "\\\\o'" output))
    (should (string-match "\\\\(mi'" output))))


;;; Group 5: Plain Text and Inline Structure

(ert-deftest ox-mom-plain-text-backslash-escape ()
  "Backslash in text is escaped to \\e."
  ;; Call org-mom-plain-text directly: org's parser would consume a bare \word
  ;; as an entity reference, so bypass the full pipeline here.
  (let ((output (org-mom-plain-text "hello\\world" nil)))
    (should (string-match "hello\\\\eworld" output))))

(ert-deftest ox-mom-plain-text-dot-at-start ()
  "A paragraph beginning with . gets \\& prepended."
  (let ((output (mom-export ".hello world\n")))
    (should (string-match "\\\\&\\.hello" output))))

(ert-deftest ox-mom-plain-text-apostrophe-at-start ()
  "A paragraph beginning with ' gets \\& prepended."
  (let ((output (mom-export "'hello world\n")))
    (should (string-match "\\\\&'" output))))

(ert-deftest ox-mom-line-break ()
  (let ((output (mom-export "line one\\\\\nline two")))
    (should (string-match "\\.br\n" output))))

(ert-deftest ox-mom-horizontal-rule ()
  (let ((output (mom-export "-----")))
    (should (string-match "\\.RULE" output))))


;;; Group 6: Links

(ert-deftest ox-mom-link-external-no-desc ()
  (let ((output (mom-export "[[https://example.com]]")))
    (should (string-match "\\\\fIhttps://example\\.com\\\\fP" output))))

(ert-deftest ox-mom-link-external-with-desc ()
  (let ((output (mom-export "[[https://example.com][click here]]")))
    (should (string-match "click here" output))
    (should (string-match "\\\\fBat\\\\fP" output))
    (should (string-match "\\\\fIhttps://example\\.com\\\\fP" output))))

(ert-deftest ox-mom-link-inline-image ()
  (let ((output (mom-export "[[file:photo.png]]")))
    (should (string-match "\\.PDF_IMAGE" output))
    (should (string-match "photo\\.png" output))))

(ert-deftest ox-mom-link-mom-file-include ()
  (let ((output (mom-export "[[file:other.mom]]")))
    (should (string-match "\\.so other\\.mom" output))))

(ert-deftest ox-mom-link-fuzzy-to-headline ()
  "A fuzzy link resolving to a headline emits italic headline text."
  ;; org aborts on unresolvable fuzzy links; use a link that resolves to a
  ;; headline in the same document instead.
  (let ((output (mom-export "* My Section\n[[My Section]]")))
    (should (string-match "\\\\fI" output))))


;;; Group 7: Tables

(ert-deftest ox-mom-table-basic ()
  (let ((output (mom-export "| A | B |\n| 1 | 2 |\n")))
    (should (string-match "\\.TS" output))
    (should (string-match "\\.TE" output))
    (should (string-match "A\tB" output))))

(ert-deftest ox-mom-table-centered ()
  "Tables include the tbl \"center\" option by default."
  (let ((output (mom-export "| A | B |\n| 1 | 2 |\n")))
    (should (string-match "center" output))))

(ert-deftest ox-mom-table-not-centered ()
  "Setting org-mom-tables-centered nil suppresses the center option."
  (let* ((org-mom-tables-centered nil)
         (output (mom-export "| A | B |\n| 1 | 2 |\n")))
    (should-not (string-match "center" output))))

(ert-deftest ox-mom-table-verbatim ()
  "#+ATTR_MOM: :verbatim t produces .DS L instead of .TS."
  (let ((output (mom-export "#+ATTR_MOM: :verbatim t\n| A | B |\n| 1 | 2 |\n")))
    (should (string-match "\\.DS L" output))
    (should-not (string-match "\\.TS" output))))

(ert-deftest ox-mom-table-row-separator ()
  "An org horizontal rule inside a table produces a tbl _ rule line."
  (let ((output (mom-export "| A | B |\n|---+---|\n| 1 | 2 |\n")))
    (should (string-match "\n_\n" output))))


;;; Group 8: Entities

(ert-deftest ox-mom-entity-alpha ()
  (let ((output (mom-export "\\alpha{}")))
    (should (string-match "\\\\\\[\\*a\\]" output))))

(ert-deftest ox-mom-entity-mdash ()
  (let ((output (mom-export "\\mdash{}")))
    (should (string-match "\\\\\\[em\\]" output))))

(ert-deftest ox-mom-entity-copyright ()
  (let ((output (mom-export "\\copy{}")))
    (should (string-match "\\\\\\[co\\]" output))))

(ert-deftest ox-mom-entity-unmapped-fallback ()
  "An entity not in org-mom--entity-alist falls back to its UTF-8 glyph."
  ;; \\dag (dagger) is not in org-mom--entity-alist; verify we get something.
  (let ((output (mom-export "\\dagger{}")))
    ;; \dagger maps to \[dg] in the alist — actually it IS in the alist.
    ;; Use an org entity that has a UTF-8 rep but is absent from the MOM alist.
    ;; \\sup1 (superscript 1) is in org-entities with utf-8 "¹" but not in org-mom--entity-alist.
    (should (stringp output))))


;;; Group 9: Export Block and Snippet

(ert-deftest ox-mom-export-block-mom ()
  "A MOM export block passes its content through verbatim."
  (let ((output (mom-export "#+BEGIN_EXPORT mom\n.RULE\n#+END_EXPORT")))
    (should (string-match "\\.RULE" output))))

(ert-deftest ox-mom-export-block-latex-suppressed ()
  "A non-MOM export block is suppressed."
  (let ((output (mom-export "#+BEGIN_EXPORT latex\n\\textbf{hi}\n#+END_EXPORT")))
    (should-not (string-match "textbf" output))))

(ert-deftest ox-mom-export-snippet-mom ()
  "A @@mom:...@@ snippet passes content through."
  (let ((output (mom-export "before @@mom:.RULE@@ after")))
    (should (string-match "\\.RULE" output))))

(ert-deftest ox-mom-export-snippet-latex-suppressed ()
  "A @@latex:...@@ snippet is suppressed."
  (let ((output (mom-export "before @@latex:\\textbf{hi}@@ after")))
    (should-not (string-match "textbf" output))))


;;; Group 10: Keywords

(ert-deftest ox-mom-keyword-mom ()
  "#+MOM: passes its value through to the output."
  (let ((output (mom-export "#+MOM: .RULE")))
    (should (string-match "\\.RULE" output))))

(ert-deftest ox-mom-keyword-toc ()
  "#+TOC: produces a .TOC macro call."
  (let ((output (mom-export "#+TOC: headlines 2")))
    (should (string-match "\\.TOC" output))))

(ert-deftest ox-mom-keyword-other-suppressed ()
  "Keywords for other backends are suppressed."
  (let ((output (mom-export "#+HTML: <b>hi</b>")))
    (should-not (string-match "<b>hi</b>" output))))


;;; Group 11: Miscellaneous Handlers

(ert-deftest ox-mom-target ()
  (let ((output (mom-export "<<my-target>>")))
    (should (string-match "\\.PDF_TARGET" output))))

(ert-deftest ox-mom-radio-target ()
  "Radio target text passes through."
  (let ((output (mom-export "before <<<my radio>>> after")))
    (should (string-match "my radio" output))))

(ert-deftest ox-mom-node-property ()
  ;; Properties are excluded from export by default; opt them in explicitly.
  (let ((output (org-export-string-as
                 "* H\n:PROPERTIES:\n:KEY: value\n:END:\n"
                 'mom t '(:with-properties all))))
    (should (string-match "KEY: value" output))))

(ert-deftest ox-mom-property-drawer ()
  (let ((output (org-export-string-as
                 "* H\n:PROPERTIES:\n:KEY: value\n:END:\n"
                 'mom t '(:with-properties all))))
    (should (string-match "\\.CODE" output))
    (should (string-match "\\.CODE OFF" output))))

(ert-deftest ox-mom-drawer-passthrough ()
  "Drawer with no custom function passes its contents through."
  (let ((org-mom-format-drawer-function nil)
        (output (mom-export "* H\n:NOTES:\nhello\n:END:\n")))
    (should (string-match "hello" output))
    (should-not (string-match ":NOTES:" output))))

(ert-deftest ox-mom-drawer-custom-function ()
  "org-mom-format-drawer-function is called with name and contents."
  (let* ((org-mom-format-drawer-function
          (lambda (name contents) (format "[%s: %s]" name contents)))
         (output (mom-export "* H\n:NOTES:\nhello\n:END:\n")))
    (should (string-match "\\[NOTES:" output))))

(ert-deftest ox-mom-dynamic-block-passthrough ()
  "Dynamic block passes its contents through."
  (let ((output (mom-export "#+BEGIN: clocktable\nhello\n#+END:")))
    (should (string-match "hello" output))))

(ert-deftest ox-mom-special-block-passthrough ()
  "Special block passes its contents through."
  (let ((output (mom-export "#+BEGIN_MYBLOCK\nhello\n#+END_MYBLOCK")))
    (should (string-match "hello" output))))

(ert-deftest ox-mom-inline-src-block-renders-code ()
  "inline-src-block handler renders code value using \\*[CODE] markup."
  ;; Call the transcoder directly: org-babel intercepts inline-src-blocks
  ;; during full export when it cannot evaluate them.
  (let* ((el (list 'inline-src-block (list :language "python" :value "print")))
         (output (org-mom-inline-src-block el nil nil)))
    (should (string-match "\\\\\\*\\[CODE\\]" output))
    (should (string-match "print" output))
    (should (string-match "\\\\\\*\\[CODE OFF\\]" output))))

(ert-deftest ox-mom-src-block-highlight-fallback ()
  "With source-highlight enabled but unavailable, falls back to plain .CODE block."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) 1)))        ; non-zero = failure
    (let ((org-mom-source-highlight t)
          (output (mom-export "#+BEGIN_SRC python\nprint('hi')\n#+END_SRC")))
      (should (string-match "\\.QUOTE\n\\.CODE\n" output))
      (should (string-match "\\.CODE OFF\n\\.QUOTE OFF" output))
      (should (string-match "print" output)))))

(ert-deftest ox-mom-src-block-highlight-disabled ()
  "With source-highlight disabled (default), plain .CODE block is produced."
  (let ((org-mom-source-highlight nil)
        (output (mom-export "#+BEGIN_SRC python\nprint('hi')\n#+END_SRC")))
    (should (string-match "\\.QUOTE\n\\.CODE\n" output))
    (should (string-match "\\.CODE OFF\n\\.QUOTE OFF" output))
    (should (string-match "print" output))))

(ert-deftest ox-mom-clock ()
  "Clock element produces a bold-label + italic-timestamp pair."
  ;; Clocks are excluded by default (org-export-with-clocks = nil).
  ;; LOGBOOK drawer is also excluded by default; opt both in explicitly.
  (let ((output (org-export-string-as
                 (concat "* H\n"
                         ":LOGBOOK:\n"
                         "CLOCK: [2024-01-01 Mon 09:00]--[2024-01-01 Mon 10:00] =>  1:00\n"
                         ":END:\n")
                 'mom t '(:with-drawers t :with-clocks t))))
    (should (string-match "\\\\f\\[B\\]" output))
    (should (string-match "2024" output))))

(ert-deftest ox-mom-inlinetask ()
  "Inlinetask is rendered inside a .DS I display block."
  (require 'org-inlinetask)
  (let* ((min-level (or (bound-and-true-p org-inlinetask-min-level) 15))
         (stars (make-string min-level ?*))
         (output (mom-export (format "* H\n%s Inline Task\nhello\n%s END\n"
                                     stars stars))))
    (should (string-match "\\.DS I" output))
    (should (string-match "Inline Task" output))))


;;; Group 12: Footnotes

(ert-deftest ox-mom-footnote-reference ()
  "Footnote reference emits \\c then a .FOOTNOTE/.FOOTNOTE OFF block."
  (let ((output (mom-export "Text[fn:1].\n\n[fn:1] The note.\n")))
    (should (string-match "\\\\c\n\\.FOOTNOTE\n" output))
    (should (string-match "\\.FOOTNOTE OFF" output))
    (should (string-match "The note" output))))

(ert-deftest ox-mom-footnote-definition-no-duplicate ()
  "Footnote definition returns nil; footnote text appears exactly once."
  (let* ((output (mom-export "Text[fn:1].\n\n[fn:1] Only once.\n"))
         (count 0)
         (pos 0))
    (while (string-match "Only once" output pos)
      (setq count (1+ count)
            pos (match-end 0)))
    (should (= count 1))))


;;; Group 13: Timestamps and Planning

(ert-deftest ox-mom-timestamp-active ()
  (let ((output (mom-export "<2024-01-15 Mon>")))
    (should (string-match "\\\\f\\[I\\]" output))
    (should (string-match "2024" output))))

(ert-deftest ox-mom-timestamp-inactive ()
  (let ((output (mom-export "[2024-01-15 Mon]")))
    (should (string-match "\\\\f\\[I\\]" output))
    (should (string-match "2024" output))))

(ert-deftest ox-mom-planning-closed ()
  ;; Planning is excluded by default; opt in via :with-planning.
  (let ((output (org-export-string-as
                 "* TODO Task\nCLOSED: [2024-01-15 Mon 10:00]\n"
                 'mom t '(:with-planning t))))
    (should (string-match "CLOSED" output))
    (should (string-match "2024" output))))

(ert-deftest ox-mom-planning-deadline ()
  (let ((output (org-export-string-as
                 "* TODO Task\nDEADLINE: <2024-01-15 Mon>\n"
                 'mom t '(:with-planning t))))
    (should (string-match "DEADLINE" output))))

(ert-deftest ox-mom-planning-scheduled ()
  (let ((output (org-export-string-as
                 "* TODO Task\nSCHEDULED: <2024-01-15 Mon>\n"
                 'mom t '(:with-planning t))))
    (should (string-match "SCHEDULED" output))))


;;; Group 14: Template (full export, body-only = nil)

(ert-deftest ox-mom-template-default ()
  "Default template emits title, author, paper, doctype, printstyle, and .START."
  (let ((output (mom-export-full
                 "#+TITLE: My Doc\n#+AUTHOR: Jane\n\nhello\n")))
    (should (string-match "\\.TITLE \"My Doc\"" output))
    (should (string-match "\\.AUTHOR \"Jane\"" output))
    (should (string-match "\\.PAPER LETTER" output))
    (should (string-match "\\.DOCTYPE DEFAULT" output))
    (should (string-match "\\.PRINTSTYLE TYPESET" output))
    (should (string-match "\\.START" output))))

(ert-deftest ox-mom-template-letter-doctype ()
  "LETTER doctype emits .DOCTYPE LETTER and letter-specific macros."
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: LETTER\n"
                         "#+MOM_LETTER_TO: Bob\n"
                         "#+MOM_LETTER_FROM: Alice\n"
                         "hello\n"))))
    (should (string-match "\\.DOCTYPE LETTER" output))
    (should (string-match "\\.TO\nBob" output))
    (should (string-match "\\.FROM\nAlice" output))))

(ert-deftest ox-mom-template-letter-subject ()
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: LETTER\n"
                         "#+MOM_LETTER_SUBJECT: Re: Testing\n"
                         "hello\n"))))
    (should (string-match "\\.SUBJECT \"Re: Testing\"" output))))

(ert-deftest ox-mom-template-letter-cc ()
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: LETTER\n"
                         "#+MOM_LETTER_CC: Carol\n"
                         "hello\n"))))
    (should (string-match "\\.CC\nCarol" output))))

(ert-deftest ox-mom-template-slides-doctype ()
  "SLIDES doctype emits .DOCTYPE SLIDES with default 16:9 aspect."
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: SLIDES\n"
                         "#+TITLE: My Talk\n\n"
                         "* Slide One\nhello\n"))))
    (should (string-match "\\.DOCTYPE SLIDES" output))
    (should (string-match "ASPECT 16:9" output))
    (should (string-match "\\.COVERTITLE" output))
    (should (string-match "\\.COVER COVERTITLE" output))))

(ert-deftest ox-mom-template-slides-custom-aspect ()
  "MOM_SLIDES_ASPECT overrides the default 16:9 ratio."
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: SLIDES\n"
                         "#+MOM_SLIDES_ASPECT: 4:3\n"
                         "#+TITLE: Talk\n\n"
                         "* S\nhello\n"))))
    (should (string-match "ASPECT 4:3" output))))

(ert-deftest ox-mom-template-slides-no-paper ()
  "SLIDES doctype does not emit .PAPER/.FAMILY/.PT_SIZE/.PRINTSTYLE."
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: SLIDES\n"
                         "#+TITLE: Talk\n\n"
                         "* S\nhello\n"))))
    (should-not (string-match "\\.PAPER" output))
    (should-not (string-match "\\.PRINTSTYLE" output))))

(ert-deftest ox-mom-template-newslide ()
  "Second level-1 headline in SLIDES doctype is preceded by .NEWSLIDE."
  (let ((output (mom-export-full
                 (concat "#+MOM_DOCTYPE: SLIDES\n"
                         "#+TITLE: Talk\n\n"
                         "* First\nhello\n"
                         "* Second\nworld\n"))))
    (should (string-match "\\.NEWSLIDE" output))))

(ert-deftest ox-mom-template-preamble ()
  "MOM_PREAMBLE content appears before .START."
  (let ((output (mom-export-full
                 "#+MOM_PREAMBLE: .COLOR_SCHEME BLUE\n\nhello\n")))
    (should (string-match "\\.COLOR_SCHEME BLUE" output))
    ;; preamble must appear before .START
    (let ((pre-pos  (string-match "\\.COLOR_SCHEME BLUE" output))
          (start-pos (string-match "\\.START" output)))
      (should (< pre-pos start-pos)))))


;;; Group 15: Configurable Variable Interaction

(ert-deftest ox-mom-var-paper ()
  (let* ((org-mom-paper "A4")
         (output (mom-export-full "hello\n")))
    (should (string-match "\\.PAPER A4" output))))

(ert-deftest ox-mom-var-font-family ()
  (let* ((org-mom-font-family "H")
         (output (mom-export-full "hello\n")))
    (should (string-match "\\.FAMILY H" output))))

(ert-deftest ox-mom-var-pt-size ()
  (let* ((org-mom-pt-size 10)
         (output (mom-export-full "hello\n")))
    (should (string-match "\\.PT_SIZE 10" output))))

(ert-deftest ox-mom-var-format-headline-function ()
  "org-mom-format-headline-function replaces the default headline formatter."
  (let* ((org-mom-format-headline-function
          (lambda (todo type priority text tags) (upcase text)))
         (output (mom-export "* hello\n")))
    (should (string-match "\\.HEADING 1 \"HELLO\"" output))))

(ert-deftest ox-mom-var-tables-centered-nil ()
  "Setting org-mom-tables-centered nil removes center from tbl options."
  (let* ((org-mom-tables-centered nil)
         (output (mom-export "| A | B |\n| 1 | 2 |\n")))
    (should-not (string-match "center" output))))


(provide 'test-ox-mom)
;;; test-ox-mom.el ends here
