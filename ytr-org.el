;; -*- lexical-binding: t; -*-
;;; ytr-org.el --- Org-mode integration for ytr

;; Copyright (C) 2010-2023 Martin Puttke

;; Author: Martin Puttke <m.s.p@posteo.de>
;; This file is not part of GNU Emacs.

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
;; Org-mode integration for YouTrack.
;; This module handles conversion between org-mode and YouTrack markdown,
;; issue display in org buffers, and org-mode specific operations.

;;; Code:

(require 'org)
(require 'diff)
(require 'ox-gfm nil t)  ;; Optional, for markdown export
(require 'ytr-api)

;;;; Configuration variables (defined in main ytr.el, declared here)
(defvar ytr-baseurl)
(defvar ytr-new-comment-behavior)
(defvar ytr-update-node-behavior)
(defvar ytr-export-base-heading-level)
(defvar ytr-capture-key)
(defvar ytr-capture-issue-code)
(defvar ytr-capture-summary)
(defvar ytr-issue-code-pattern)
(defvar ytr-issue-mandatory-node-code-pattern)

;;;; Forward declarations
(declare-function ytr-issue-node-code-action "ytr")
(declare-function ytr-parse-issue-node-code "ytr")
(declare-function ytr-guess-or-read-issue-code "ytr")
(declare-function ytr-guess-issue-node-cons "ytr")
(declare-function ytr-add-issue-to-history "ytr")
(declare-function ytr-issue-node-cons-from-org-property "ytr")
(declare-function ytr-issue-node-code-buttonize-buffer "ytr-ui")
(declare-function ytr-read-query-consult "ytr-ui")
(declare-function ytr-issue-url "ytr")
(declare-function ytr-issue-property-id "ytr-ui")
(declare-function ytr-issue-property-summary "ytr-ui")

;;;; Property name constants
(defconst ytr-org-issue-code-property-name "YTR_ISSUE_CODE" "Name of the property to store the org issue code.")
(defconst ytr-org-node-type-property-name "YTR_NODE_TYPE" "Name of the property to store the node type.")
(defconst ytr-org-local-content-hash-property-name "YTR_LOCAL_CONTENT_HASH" "Name of the property to store the local content hash.")
(defconst ytr-org-remote-content-hash-property-name "YTR_REMOTE_CONTENT_HASH" "Name of the property to store the remote content hash.")
(defconst ytr-org-author-property-name "YTR_AUTHOR" "Name of the property to store the author.")
(defconst ytr-org-created-at-property-name "YTR_CREATED_AT" "Name of the property to store the creation timestamp.")
(defconst ytr-org-updated-at-property-name "YTR_UPDATED_AT" "Name of the property to store the update timestamp.")

;;;; Customization

(defcustom ytr-save-import-diff-inline nil "Control whether an inline code block is written to each imported node." :type 'boolean :group 'ytr)
(defcustom ytr-save-import-diff-inline-when-empty nil "Control whether an inline code block is written even if the diff is empty." :type 'boolean :group 'ytr)
(defcustom ytr-import-diff-switches "--ignore-space-change" "Diff Switches used to create the import diff." :type 'string :group 'ytr)
(defcustom ytr-org-file "~/ytr.org" "File path used for persisting downloaded issues." :type 'file :group 'ytr)

;;;; Buffer-local variables
(defvar-local ytr-buffer-position nil "Buffer local var to store position.")
(defvar-local ytr-buffer-text nil "Buffer local var to store text.")
(defvar-local ytr-buffer-wconf nil "Buffer local var to store wconf.")
(defvar-local ytr-buffer-curlevel nil "Buffer local var to store curlevel.")
(defvar-local ytr-buffer-issue-code nil "Buffer local var to store issue-code.")
(defvar-local ytr-buffer-node-code nil "Buffer local var to store node-code.")
(defvar-local ytr-buffer-node-type nil "Buffer local var to store node-type.")
(defvar-local ytr-buffer-commit-type nil "Buffer local var to store commit-type.")
(defvar-local ytr-buffer-local-content-hash nil "Buffer local var to store local (org) content hash.")

;;;; Helper functions

(defun ytr-align-all-org-tables-in-buffer ()
  "Align all org tables in the current buffer.
Calls `org-table-align' once per table. Preserves point."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-table-dataline-regexp nil t)
      (org-table-align)
      (goto-char (org-table-end)))))

(defun ytr-trim-blank-lines-leading-and-trailing (content)
  "Return CONTENT with all blank lines at the beginning and end trimmed."
  (replace-regexp-in-string "\n*[ \t\r\n]*\\'" "" (replace-regexp-in-string "\\`[ \t\r\n]*\n*" "" content)))

(defun ytr-max-heading-level ()
  "Determine the highest Heading in the buffer.
Return nil if no heading found."
  (save-excursion
    (goto-char (point-min))
    (let ((lowest-level 6))
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        (setq lowest-level (min lowest-level (length (match-string 1)))))
      lowest-level)))

(defun ytr-first-heading-level ()
  "Determine the level of the first heading in buffer."
  (org-fold-show-all)
  (goto-char (point-min))
  (when (not (org-at-heading-p))
    (org-next-visible-heading 1))
  (org-current-level))

(defun ytr-demote-org-headings (level)
  "Demote all headings in the current buffer so the new max level is LEVEL."
  (let ((base-level (ytr-max-heading-level)))
    (when base-level
      (ytr-demote-org-headings-by (- level base-level)))))

(defun ytr-demote-org-headings-by (level)
  "Demote all headings in the current buffer by the specified LEVEL."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\) " nil t)
      (let* ((heading-level (length (match-string 1)))
             (new-level (min (+ heading-level level) 12))
             (new-heading (concat (make-string new-level ?*) " ")))
        (replace-match new-heading)))))

;;;; Markdown to Org conversion

(defun ytr-md-to-org (input level &optional diff-file)
  "Convert a markdown string INPUT to org mode using pandoc.

LEVEL indicates the level of top level headings in org and defaults to 3.
If DIFF-FILE is given, a diff file is written, that contains possible
conversion loss."
  (save-excursion
    (let ((input-md-buffer (get-buffer-create "*ytr-input-md*"))
          (pandoc-org-buffer (get-buffer-create "*ytr-pandoc-org*"))
          (org-export-gfm-buffer (get-buffer-create "*ytr-org-export-gfm*"))
          (diff-md-buffer (get-buffer-create "*ytr-diff-md*"))
          process-exit-status)
      (with-current-buffer pandoc-org-buffer
        (erase-buffer)
        (setq process-exit-status (call-process-region input nil "pandoc" nil t nil "--wrap=preserve" "-f" "gfm" "-t" "org"))
        (unless (= 0 process-exit-status)
          (user-error "pandoc exited with error: %s" process-exit-status))
        (let ((inhibit-message t))
          (replace-string-in-region "☒" "[X]" (point-min) (point-max))
          (replace-string-in-region "☐" "[ ]" (point-min) (point-max)))
        (goto-char (point-min))
        (flush-lines " *:[A-Z_]+:.*$") ; remove properties
        (goto-char (point-max))
        (insert "\n")
        (org-mode)
        (ytr-demote-org-headings (or level 3))
        (org-unindent-buffer)
        (ytr-align-all-org-tables-in-buffer)
        (with-current-buffer input-md-buffer ;; store for diff
          (erase-buffer)
          (insert input))
        (when (or ytr-save-import-diff-inline diff-file)
          (let ((org-export-show-temporary-export-buffer nil))
            (org-export-to-buffer 'gfm org-export-gfm-buffer))
          (with-current-buffer org-export-gfm-buffer (ytr-perform-markdown-replacements ""))
          (let ((diff-switches ytr-import-diff-switches))
            (diff-no-select input-md-buffer org-export-gfm-buffer nil 'no-async diff-md-buffer ))
          (with-current-buffer diff-md-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (delete-line)
              (goto-char (- (point-max) 1))
              (delete-line)
              (goto-char (- (point-max) 1))
              (delete-line)))
          (when diff-file
            (with-current-buffer diff-md-buffer
              (write-region (point-min) (point-max) diff-file)))
          (let ((diff (with-current-buffer diff-md-buffer (buffer-string))))
            (when (and ytr-save-import-diff-inline
                       (or (not (string= "" diff))
                           ytr-save-import-diff-inline-when-empty))
              (save-excursion
                (goto-char (point-min))
                (insert "#+name: ytr_import_diff\n")
                (insert "#+begin_src diff\n")
                (insert diff)
                (insert "#+end_src\n")
                (insert "\n")))))
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;; Org insertion functions

(defun ytr-org-perform-attachment-replacements-import (org-content attachments)
  "Replace links in ORG-CONTENT to ATTACHMENTS found at the remote issue.
Returns the resulting content."
  (mapc (lambda (attachment-alist)
          (let-alist attachment-alist
            (setq org-content (string-replace (format "[[file:%s]]" .name)
                                              (format "[[%s%s&forceDownload=true&ytr_name=%s][%s]]" ytr-baseurl .url .name .name)
                                              org-content))
            (setq org-content (string-replace (format "\\[\\[file:%s]\\[\\(.*\\)]]" .name)
                                              (format "[[%s%s&forceDownload=true&ytr_name=%s][\\1]]" ytr-baseurl .url .name)
                                              org-content))))
        attachments)
  org-content)

(defun ytr-org-insert-node (content level type issue-node-cons author created updated attachments deleted)
  "Insert a node at point.

CONTENT is the markdown content.
LEVEL is the heading level.
TYPE is the node type (description, comment).
ISSUE-NODE-CONS is (issue-code . node-code).
AUTHOR is a string.
CREATED is a timestamp (milliseconds).
UPDATED is a timestamp (milliseconds) or nil.
ATTACHMENTS is a list of attachment alists.
DELETED is t if the node is deleted."
  (let* ((start (point))
         (type-string (format "%s" type))
         (remote-content (or content ""))
         (local-content (ytr-trim-blank-lines-leading-and-trailing
                         (ytr-org-perform-attachment-replacements-import (ytr-md-to-org remote-content (+ 1 level))
                                                                         attachments))))
    (open-line 1)  ;; need this to ensure props go to correct heading
    (insert (format "%s %s by %s\n\n"
                    (make-string level ?*)
                    (format-time-string "%Y-%m-%d" (/ created 1000))
                    author))
    (save-excursion
      (goto-char start)
      (org-set-tags (list (capitalize type-string)))
      (when (eq deleted t)
        (org-set-tags (append (org-get-tags) '("YTR_DETELED"))))
      (when (/= (length attachments) 0)
        (org-set-tags (append (org-get-tags) '("YTR_ATTACH")))))
    (org-set-property ytr-org-remote-content-hash-property-name (sha1 remote-content))
    (org-set-property ytr-org-local-content-hash-property-name (sha1 local-content))
    (org-set-property ytr-org-issue-code-property-name (ytr-issue-node-code-action issue-node-cons))
    (org-set-property ytr-org-node-type-property-name type-string)
    (org-set-property ytr-org-created-at-property-name (format-time-string "%Y-%m-%d %H:%M" (/ created 1000)))
    (when updated (org-set-property ytr-org-updated-at-property-name (format-time-string "%Y-%m-%d %H:%M" (/ updated 1000))))
    (org-set-property ytr-org-author-property-name author)
    (kill-whole-line)  ;; kill line we just opened
    (unless (string= local-content "")
      (insert local-content)
      (insert "\n\n"))))

(defun ytr-insert-issue-alist-as-org (issue-alist level)
  "Insert the issue given by ISSUE-ALIST as org at point with heading LEVEL."
  (let-alist issue-alist
    (insert (format "%s %s: %s\n\n" (make-string level ?*) .idReadable .summary))
    (open-line 1)  ;; need this to ensure props go to correct heading
    (org-set-property ytr-org-issue-code-property-name .idReadable)
    (org-set-property ytr-org-node-type-property-name "issue")
    (kill-whole-line)  ;; kill line we just opened
    (insert (format "%s Links\n\n" (make-string (+ 1 level) ?*)))
    (mapc (lambda (link-alist)
            (let-alist link-alist
              (unless (equal (length .issues) 0)
                (insert (format "%s %s\n\n"
                                (make-string (+ 2 level) ?*)
                                (cond ((string= .direction "BOTH") (alist-get 'sourceToTarget .linkType))
                                      ((string= .direction "INWARD") (alist-get 'targetToSource .linkType))
                                      ((string= .direction "OUTWARD") (alist-get 'sourceToTarget .linkType))
                                      (t (message "Unknown link direction: %s" .direction)))))
                (mapc (lambda (issue-alist)
                        (let-alist issue-alist
                          (insert (format " - *%s*: %s\n" .idReadable .summary))))
                      .issues)
                (insert "\n"))))
          .links)
    (unless (eq .attachments '[])
      (insert (format "%s Attachments\n\n" (make-string (+ 1 level) ?*)))
      (mapc (lambda (attachment-alist)
              (let-alist attachment-alist
                (insert (format "- [[%s%s][%s]] %s %s (%s)\n"
                                ytr-baseurl
                                .url
                                .name
                                .mimeType
                                (file-size-human-readable .size)
                                (if .comment
                                    (format "Comment %s" (alist-get 'id .comment))
                                  "Issue")))))
            .attachments)
      (insert "\n"))
    ;; do the description
    (ytr-org-insert-node .description (+ 1 level) 'description (cons .idReadable nil) (alist-get 'fullName .reporter) .created .updated .attachments nil)
    ;; do the comments
    (let ((issue-code .idReadable))
      (mapc (lambda (comment-alist)
              (let-alist comment-alist
                (when (not (eq .deleted t))
                  (ytr-org-insert-node .text (+ 1 level) 'comment (cons issue-code .id) (alist-get 'fullName .author) .created .updated .attachments .deleted))))
            .comments))
    ;; postprocess
    (org-unindent-buffer)))

(defun ytr-issue-alist-to-org-buffer (issue-alist)
  "Convert ISSUE-ALIST into an org buffer with proper headings."
  (let-alist issue-alist
    (let ((bufname (format "*ytr-org-%s*" (downcase .idReadable))))
      (set-buffer (get-buffer-create bufname))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (concat "#+Title: " .idReadable ": " .summary "\n\n"))
        (org-set-property ytr-org-issue-code-property-name (format "%s" .idReadable))
        (insert (format "#+COLUMNS: %%50ITEM(Title) %%16%s(Created) %%16%s(Updated) %%20%s(Author) %%12%s(Type)\n"
                        ytr-org-created-at-property-name ytr-org-updated-at-property-name ytr-org-author-property-name ytr-org-node-type-property-name))
        (ytr-insert-issue-alist-as-org issue-alist 1)
        (switch-to-buffer bufname)))))

;;;; Node finding and navigation

(defun ytr-find-node (&optional type-wanted)
  "Find the parent heading with a YTR_NODE_TYPE property.

Sets the point and returns the type. If property is not found in
all higher headings returns nil and restore point. If TYPE-WANTED
is not nil search for that node type."
  (let ((saved-point (point)))
    (when (or (org-at-heading-p) (org-back-to-heading))
      (let ((type-found (org-entry-get (point) ytr-org-node-type-property-name)))
        (if (and type-found (or (not type-wanted)
                                (eq type-wanted (intern type-found))))
            (intern type-found)
          (or (and (org-up-heading-safe) (ytr-find-node type-wanted))
              (and (goto-char saved-point) nil)))))))

(defun ytr-org-content-start (&optional ensure-content)
  "Return the start of the real content of a heading.
Skips the headline, properties and blank lines. When no content
was found, return nil if ENSURE-CONTENT is not nil, or the
beginning of the next heading."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((level (org-current-level)))
      (forward-line)
      (while (looking-at-p "\\s-*\\(:.*\\)?$") (forward-line))
      (unless (and ensure-content (org-at-heading-p) (<= (org-current-level) level))
        (point)))))

(defun ytr-org-skip-to-content ()
  "Skip lines forward to content (not heading, property, or blank line)."
  (when (org-at-heading-p) ; skip only one heading
    (forward-line))
  (let ((max-line (line-number-at-pos (point-max))))
    (while (and (< (line-number-at-pos) max-line)
                (looking-at-p "\\s-*\\(:.*\\)?$"))
      (forward-line))))

(defun ytr-org-mark-inner-subtree ()
  "Mark current subtree but exclude heading and properties.

Special cases:
- No content - works
- Immediate subheading - works
- End of buffer - works"
  (let* ((subtree-start (progn
                          (or (org-at-heading-p) (org-back-to-heading t))
                          (point)))
         (start (progn
                  (goto-char subtree-start)
                  (ytr-org-skip-to-content)
                  (point)))
         (end (progn
                (goto-char subtree-start)
                (org-end-of-subtree t))))
    (when (< start end)
      (set-mark end)
      (goto-char start)
      (cons start end))))

;;;; Hash comparison functions

(defun ytr-node-locally-edited-p (&optional false_if_no_node)
  "Return whether the hash of the current subtree differs from the local hash in property.
If FALSE_IF_NO_NODE is non-nil, return nil instead of error when no node found."
  (save-mark-and-excursion
    (if (not (member (ytr-find-node) (list 'comment 'description)))
        (when (not false_if_no_node)
          (user-error "No hashable node found"))
      (let* ((saved-local-hash (org-entry-get (point) ytr-org-local-content-hash-property-name t))
             (actual-local-hash (progn
                                  (ytr-org-mark-inner-subtree)
                                  (sha1 (ytr-trim-blank-lines-leading-and-trailing (buffer-substring-no-properties (region-beginning) (region-end)))))))
        (not (string= saved-local-hash actual-local-hash))))))

(defun ytr-node-remotely-edited-p (&optional remote-content false_if_no_node)
  "Return whether the hash of REMOTE-CONTENT differs from the remote hash in property.
If REMOTE-CONTENT is nil it will be retrieved according to point (only works for comments!).
If FALSE_IF_NO_NODE is non-nil, return nil instead of error when no node found."
  (unless remote-content
    (setq remote-content (alist-get 'text (ytr-retrieve-issue-comment-alist (ytr-guess-issue-node-cons)))))
  (save-mark-and-excursion
    (if (not (member (ytr-find-node) (list 'comment 'description)))
        (when (not false_if_no_node)
          (user-error "No hashable node found"))
      (let* ((saved-remote-hash (org-entry-get (point) ytr-org-remote-content-hash-property-name t))
             (actual-remote-hash (sha1 remote-content)))
        (not (string= saved-remote-hash actual-remote-hash))))))

;;;; Commit modes and functions

(defvar-keymap ytr-commit-new-comment-mode-map "C-c C-c" #'ytr-commit-new-comment "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-new-comment-mode markdown-mode "ytr-commit-new-comment-mode" "Mode for editing markdown exports from org before sending them to youtrack.")

(defvar-keymap ytr-commit-update-node-mode-map "C-c C-c" #'ytr-commit-update-node "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-update-node-mode markdown-mode "ytr-commit-update-node-mode" "Mode for editing markdown exports from org before sending them to youtrack.")

(defun ytr-cancel-commit ()
  "Cancel committing something to youtrack."
  (interactive)
  (let ((buffer (buffer-name)))
    (set-window-configuration ytr-buffer-wconf)
    (kill-buffer buffer)))

(defun ytr-perform-markdown-replacements (attach-dir)
  "Perform markdown replacements for export.
ATTACH-DIR is the org attachment directory."
  (replace-regexp-in-region "^#" (make-string ytr-export-base-heading-level ?#) (point-min) (point-max))
  (replace-regexp-in-region (format "\\[\\(.*\\)\\](%s.*&ytr_name=\\(.*\\(?:png\\|jpeg\\|jpg\\)\\))" ytr-baseurl) "![](\\1)" (point-min) (point-max))
  (replace-regexp-in-region (format "\\[\\(.*\\)\\](%s.*&ytr_name=\\(.*\\))" ytr-baseurl) "[\\1](\\2)" (point-min) (point-max))
  (replace-regexp-in-region (format "\\([^[#a-zA-Z0-9-]\\|^\\)%s\\([^]#a-zA-Z0-9-]\\|$\\)" ytr-issue-mandatory-node-code-pattern)
                            (format "\\1[\\2#\\3](%s/issue/\\2#focus=Comments-\\3.0-0)\\4" ytr-baseurl) (point-min) (point-max))
  (replace-regexp-in-region (format "](file://%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
  (replace-regexp-in-region (format "](%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
  (replace-regexp-in-region ":.*:$" "" (point-min) (point-max))
  (replace-string-in-region "\n\n\n" "\n\n" (point-min) (point-max))
  (whitespace-cleanup))

(defun ytr-commit-new-comment ()
  "Commit buffer content as a new comment."
  (interactive)
  (let ((new-node-code (or (alist-get 'id (ytr-send-new-comment-alist ytr-buffer-issue-code `((text . ,(buffer-string)))))
                           (user-error "No node code retrieved")))
        (issue-code ytr-buffer-issue-code) ;; These vars are buffer local and we are going to switch buffer
        (curlevel ytr-buffer-curlevel)
        (text ytr-buffer-text)
        (position ytr-buffer-position)
        (buffer (buffer-name))
        (local-content-hash ytr-buffer-local-content-hash))
    (message "New comment created on %s with node code %s." issue-code new-node-code)
    (set-window-configuration ytr-buffer-wconf)
    (kill-buffer buffer)
    (kill-new (ytr-issue-node-code-action (cons issue-code new-node-code)))
    (ytr-add-issue-to-history issue-code)
    (when position ;; use position as flag for source buffer
      (save-mark-and-excursion
        (if (derived-mode-p 'org-mode)
            (ytr-send-attachments-action (cons issue-code new-node-code)))
        (goto-char position)
        (set-mark (+ position (length text)))
        (cl-case ytr-new-comment-behavior
          (keep (deactivate-mark))
          (keep-content
           (let-alist (ytr-retrieve-issue-comment-alist (cons issue-code new-node-code))
             (deactivate-mark)
             (when (looking-at-p "\*+ ")
               (insert "*"))
             (goto-char position)
             (ytr-org-insert-node nil curlevel 'comment (cons issue-code new-node-code) (alist-get 'fullName .author) .created .updated .attachments .deleted)
             (setq local-content-hash (sha1 (ytr-trim-blank-lines-leading-and-trailing (buffer-substring-no-properties (point) (org-end-of-subtree)))))
             (goto-char position)
             (org-set-property ytr-org-remote-content-hash-property-name (if .text (sha1 .text) ""))
             (org-set-property ytr-org-local-content-hash-property-name local-content-hash)))
          (kill (kill-region (point) (mark)))
          (fetch
           (let-alist (ytr-retrieve-issue-comment-alist (cons issue-code new-node-code))
             (kill-region (point) (mark))
             (ytr-org-insert-node .text curlevel 'comment (cons issue-code new-node-code) (alist-get 'fullName .author) .created .updated .attachments .deleted)
             (goto-char position))))
        (ytr-issue-node-code-buttonize-buffer)))))

(defun ytr-commit-update-node ()
  "Commit the buffer to youtrack to update a node."
  (interactive)
  (let ((issue-code ytr-buffer-issue-code)
        (node-code ytr-buffer-node-code)
        (node-type ytr-buffer-node-type)
        (position ytr-buffer-position)
        (buffer (buffer-name))
        (local-content-hash ytr-buffer-local-content-hash))
    (cl-case ytr-buffer-node-type
      (description (ytr-send-issue-alist ytr-buffer-issue-code `((description . ,(buffer-string)))))
      (comment (ytr-send-issue-comment-alist (cons ytr-buffer-issue-code ytr-buffer-node-code) `((text . ,(buffer-string)))))
      (t (user-error "Wrong node type %s" ytr-buffer-node-type)))
    (message "Node successfully updated")
    (set-window-configuration ytr-buffer-wconf)
    (ytr-add-issue-to-history issue-code)
    (kill-buffer buffer)
    (kill-new (ytr-issue-node-code-action (cons issue-code node-code)))
    (when position ;; use position as flag for source buffer
      (save-excursion
        (goto-char position)
        (ytr-send-attachments-action (cons issue-code (when (eq node-type 'comment) node-code)))
        (cl-case ytr-update-node-behavior
          (keep)
          (keep-content
           (org-set-property ytr-org-remote-content-hash-property-name
                             (let ((content (cl-case node-type
                                              (description (alist-get 'description (ytr-retrieve-issue-alist issue-code)))
                                              (comment (alist-get 'text (ytr-retrieve-issue-comment-alist (cons issue-code node-code)))))))
                               (if content (sha1 content) "")))
           (org-set-property ytr-org-local-content-hash-property-name local-content-hash))
          (kill
           (org-mark-subtree)
           (kill-region (point) (mark)))
          (fetch
           (ytr-fetch-remote-node)))
        (ytr-issue-node-code-buttonize-buffer)))))

;;;; Interactive editing functions

(defun ytr-new-comment-editable ()
  "Send the current subtree or region as comment to a ticket."
  (interactive)
  (save-mark-and-excursion
    (unless (region-active-p)
      (org-mark-subtree)
      (or (org-at-heading-p) (org-back-to-heading)))
    (when (> (point) (mark)) (exchange-point-and-mark))
    (let ((position (point))
          (text (buffer-substring-no-properties (region-beginning) (region-end)))
          (wconf (current-window-configuration))
          (issue-code (ytr-guess-or-read-issue-code))
          (curlevel (+ (org-current-level) (if (org-at-heading-p) 0 1)))
          (attach-dir (or (org-attach-dir) "")))
      (org-gfm-export-as-markdown nil nil)
      (ytr-perform-markdown-replacements attach-dir)
      (ytr-commit-new-comment-mode)
      (message "Create new comment on issue %s. C-c to submit, C-k to cancel" issue-code)
      (setq-local ytr-buffer-position position
                  ytr-buffer-text text
                  ytr-buffer-wconf wconf
                  ytr-buffer-curlevel curlevel
                  ytr-buffer-issue-code issue-code
                  ytr-buffer-node-type 'comment
                  ytr-buffer-commit-type 'create
                  ytr-buffer-local-content-hash (sha1 (ytr-trim-blank-lines-leading-and-trailing text))))))

(defun ytr-quick-comment-action (issue-node-cons)
  "Open a markdown buffer to write a quick comment for ISSUE-NODE-CONS."
  (let ((issue-code (car issue-node-cons))
        (wconf (current-window-configuration)))
    (switch-to-buffer-other-window (get-buffer-create "*YTR Compose Comment*"))
    (ytr-commit-new-comment-mode)
    (message "Create new comment on issue %s. C-c to submit, C-k to cancel" issue-code)
    (setq-local ytr-buffer-position nil ;; position is used as flag for source buffer
                ytr-buffer-text ""
                ytr-buffer-wconf wconf
                ytr-buffer-curlevel 0
                ytr-buffer-issue-code issue-code
                ytr-buffer-node-type 'comment
                ytr-buffer-commit-type 'create
                ytr-buffer-local-content-hash nil)))

(defun ytr-quick-node-edit-action (issue-node-cons)
  "Open a markdown buffer to edit a node for ISSUE-NODE-CONS."
  (let ((issue-code (car issue-node-cons))
        (node-code (cdr issue-node-cons))
        (wconf (current-window-configuration)))
    (switch-to-buffer-other-window (get-buffer-create "*YTR Edit Comment*"))
    (insert
     (if node-code
         (alist-get 'text (ytr-retrieve-issue-comment-alist (cons issue-code node-code)))
       (alist-get 'description (ytr-retrieve-issue-alist issue-code))))
    (ytr-commit-update-node-mode)
    (message "Edit node %s. C-c to submit, C-k to cancel" (ytr-issue-node-code-action issue-node-cons))
    (setq-local ytr-buffer-position nil ;; position is used as flag for source buffer
                ytr-buffer-wconf wconf
                ytr-buffer-commit-type 'update
                ytr-buffer-node-type (if node-code 'comment 'description)
                ytr-buffer-issue-code issue-code
                ytr-buffer-node-code node-code
                ytr-buffer-local-content-hash nil)))

(defun ytr-new-issue ()
  "Use the current subtree to create a new issue."
  (interactive)
  (org-back-to-heading)
  (let* ((heading-start (point))
         (heading-end (ytr-org-content-start))
         (heading (buffer-substring-no-properties heading-start heading-end))
         (title (org-get-heading t t t t)))
    (org-mark-subtree)
    (kill-region (point) (mark))
    (with-temp-buffer
      (org-mode)
      (insert (pop kill-ring))
      (goto-char (point-min))
      (kill-whole-line)
      (save-window-excursion
        (org-gfm-export-as-markdown nil nil)
        (ytr-perform-markdown-replacements "")
        (whitespace-cleanup)
        (ytr-browse-new-issue title (buffer-string))))
    (insert heading)
    (insert "/(Issue created from deleted content)/\n\n")))

(defun ytr-update-remote-node-editable ()
  "Update a node on remote side after editing locally."
  (interactive)
  (let* ((type (or (ytr-find-node) (user-error "Could not find a node to update")))
         (issue-node-cons (ytr-issue-node-cons-from-org-property))
         (issue-code (car issue-node-cons))
         (node-code (cdr issue-node-cons))
         (content (cl-case type
                    (description (alist-get 'description (ytr-retrieve-issue-alist issue-code)))
                    (comment (alist-get 'text (ytr-retrieve-issue-comment-alist (cons issue-code node-code))))
                    (t (user-error (format "Unknown node type: %s" type)))))
         (current-remote-hash (if content (sha1 content) ""))
         (import-remote-hash (org-entry-get (point) ytr-org-remote-content-hash-property-name t))
         (position (point))
         (wconf (current-window-configuration))
         (attach-dir (or (org-attach-dir) ""))
         (text (save-mark-and-excursion
                 (buffer-substring-no-properties (ytr-org-content-start) (org-end-of-subtree)))))
    (when (not (string= import-remote-hash current-remote-hash))
      (user-error "Aborted! Remote Node was edited since last fetch: %s, %s" import-remote-hash current-remote-hash))
    (org-gfm-export-as-markdown nil t)
    (ytr-perform-markdown-replacements attach-dir)
    (ytr-commit-update-node-mode)
    (message "Update %s%s on issue %s" type (if (eq type 'comment) (format " with ID %s" node-code) "") issue-code)
    (setq-local ytr-buffer-position position
                ytr-buffer-wconf wconf
                ytr-buffer-commit-type 'update
                ytr-buffer-node-type type
                ytr-buffer-issue-code issue-code
                ytr-buffer-node-code node-code
                ytr-buffer-local-content-hash (sha1 (ytr-trim-blank-lines-leading-and-trailing text)))))

(defun ytr-send-node ()
  "Create a new comment or update the node, depending on context."
  (interactive)
  (if (member (org-entry-get (point) ytr-org-node-type-property-name t) (list "comment" "description"))
      (ytr-update-remote-node-editable)
    (ytr-new-comment-editable)))

(defun ytr-fetch-remote-node (&optional node-alist trust-hash)
  "Update a local node at point with its remote content.
If NODE-ALIST is not given, it will be retrieved.
If TRUST-HASH is non-nil, skip fetching if hashes match."
  (interactive)
  (save-excursion
    (let* ((node-type (or (ytr-find-node) (user-error "Could not find a node to fetch")))
           (issue-node-cons (ytr-issue-node-cons-from-org-property))
           (issue-code (car issue-node-cons))
           (node-code (cdr issue-node-cons))
           (curlevel (org-current-level)))
      (when (or (not (ytr-node-locally-edited-p t))
                (and (y-or-n-p (format "Node %s was edited locally! Fetch anyway?" (ytr-issue-node-code-action issue-node-cons)))
                     (or (setq trust-hash nil) t))) ; in this case, fetch even when remote hash is current, in order to restore remote content
        (unless node-alist
          (setq node-alist (cl-case node-type
                             (comment (ytr-retrieve-issue-comment-alist (cons issue-code node-code)))
                             (t (ytr-retrieve-issue-alist issue-code)))))
        (if (and trust-hash
                 (not (ytr-node-remotely-edited-p (or (alist-get (cl-case node-type
                                                                   (comment 'text)
                                                                   (description 'description)
                                                                   (t (user-error "Bad node type %s" node-type)))
                                                                 node-alist)
                                                      ""))))
            (message "Node %s already up-to-date" (ytr-issue-node-code-action issue-node-cons))
          (let ((inhibit-read-only t))
            (org-mark-subtree)
            (kill-region (point) (mark))
            (cl-case node-type
              (description (let-alist node-alist
                             (ytr-org-insert-node .description curlevel 'description (cons issue-code node-code) (alist-get 'fullName .reporter) .created .updated .attachments nil)))
              (comment (let-alist node-alist
                         (ytr-org-insert-node .text curlevel 'comment (cons issue-code node-code) (alist-get 'fullName .author) .created .updated .attachments .deleted)))
              (issue (ytr-insert-issue-alist-as-org node-alist curlevel))
              (t (user-error "Bad node type %s" node-type)))))))))

;;;; Org actions

(defun ytr-org-action (issue-node-cons)
  "Display ISSUE-NODE-CONS in an org buffer."
  (let* ((issue-code (car issue-node-cons))
         (node-code (cdr issue-node-cons)))
    (ytr-issue-alist-to-org-buffer (ytr-retrieve-issue-alist issue-code))
    (org-mode)
    (read-only-mode 1)
    (ytr-issue-node-code-buttonize-buffer)
    (goto-char (point-min))
    (when node-code
      (search-forward-regexp (format ":%s: *%s" ytr-org-issue-code-property-name (ytr-issue-node-code-action (cons issue-code node-code))) nil t)
      (org-back-to-heading))))

(defun ytr-org-link-heading-action (issue-node-cons)
  "Set Property YTR_ISSUE_CODE on org heading and append tag YTR.
ISSUE-NODE-CONS is (issue-code . node-code)."
  (save-excursion
    (org-set-property ytr-org-issue-code-property-name (car issue-node-cons))
    (org-back-to-heading)
    (let ((tags (org-get-tags)))
      (if (member "YTR" tags) nil (org-set-tags (append (list "YTR") tags))))))

(defun ytr-org-query (query)
  "Convert all issues given by QUERY to org and collect them in a buffer."
  (interactive (list (ytr-read-query-consult)))
  (let ((issues-alist (ytr-retrieve-query-issues-alist query))
        (bufname "*ytr-org-query*"))
    (set-buffer (get-buffer-create bufname))
    (erase-buffer)
    (org-mode)
    (insert "#+Title: Org Issues from Query\n\n")
    (mapc (lambda (item)
            (ytr-insert-issue-alist-as-org (ytr-retrieve-issue-alist (alist-get 'idReadable item)) 1 ))
          issues-alist)
    (switch-to-buffer bufname)))

(defun ytr-send-attachments-action (issue-node-cons)
  "Send the attachments at org heading to ISSUE-NODE-CONS and remove them locally."
  (when (and (org-attach-dir) (org-attach-file-list (org-attach-dir)))
    (let* ((paths (mapcar (lambda (filename)
                            (file-name-concat (expand-file-name (org-attach-dir)) filename))
                          (org-attach-file-list (org-attach-dir))))
           (size (apply '+ (mapcar (lambda (path) (nth 7 (file-attributes path))) paths))))
      (if (y-or-n-p (format "Upload %s files with total size %s? " (length paths) (file-size-human-readable size)))
          (progn
            (ytr-send-as-attachments paths (car issue-node-cons) (cdr issue-node-cons))
            (when (y-or-n-p "Delete attachments after uploading? ")
              (org-attach-delete-all t)
              (message "Attachments deleted.")))
        (message "Canceled by user.")))))

(defun ytr-find-org-node-action (issue-node-cons)
  "Find the first node with ISSUE-NODE-CONS in ytr org file."
  (with-current-buffer (find-file-noselect ytr-org-file)
    (let ((initial-point (point))
          (regexp (format "^[   ]*:%s:[   ]*%s$" ytr-org-issue-code-property-name (ytr-issue-node-code-action issue-node-cons))))
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (progn
            (goto-char (match-beginning 0))
            (org-back-to-heading)
            (switch-to-buffer (current-buffer)))
        (goto-char initial-point)
        (when (and (not (string= "" ytr-capture-key))
                   (y-or-n-p "Issue Code not found. Call Capture?"))
          (ytr-capture-action issue-node-cons))))))

;;;; Query to org table

(defun ytr-data-to-org-table (data)
  "Convert DATA (a list of lists) to an org mode table string."
  (let* ((header (car data))
         (rows (cdr data))
         (format-row (lambda (row)
                       (concat "| " (mapconcat 'identity row " | ") " |")))
         (header-row (funcall format-row header))
         (separator-row
          (concat "|" (mapconcat (lambda (_) "---") header "|") "|"))
         (data-rows (mapconcat format-row rows "\n")))
    (concat header-row "\n" separator-row "\n" data-rows "\n")))

(defun ytr-insert-and-align-org-table (data)
  "Insert DATA as an org mode table and align it."
  (interactive)
  (let ((table (ytr-data-to-org-table data)))
    (insert table)
    (forward-line (- 1 (length (split-string table "\n"))))
    (org-table-align)))

(defun ytr-query-to-org-table (query &optional no-query-keyword issue-properties)
  "Execute QUERY and insert it as an org mode table.
If NO-QUERY-KEYWORD is non-nil, don't insert the query keyword.
ISSUE-PROPERTIES overrides the default properties to display."
  (interactive (list (ytr-read-query-consult)))
  (let* ((issues-alist (ytr-retrieve-query-issues-alist query))
         (issue-properties (cons ytr-issue-property-id (cons ytr-issue-property-summary (or issue-properties ytr-issue-properties))))
         (table-data (cons
                      (mapcar (lambda (issue-property)
                                (format "*%s*" (car issue-property)))
                              issue-properties)
                      (mapcar (lambda (issue-alist)
                                (let-alist issue-alist
                                  (mapcar (lambda (issue-property)
                                            (funcall (cdr issue-property) issue-alist))
                                          issue-properties)))
                              issues-alist))))
    (when (not no-query-keyword)
      (insert (concat "#+ytr-query: " query "\n")))
    (ytr-insert-and-align-org-table table-data)))

(defun ytr-update-org-query-table ()
  "Update the query table under point according to ytr-query keyword."
  (interactive)
  (let ((my-point (point))
        (key-values))
    (when (org-at-table-p)
      (goto-char (org-table-begin))
      (forward-line -1))
    (when (not (org-at-keyword-p))
      (goto-char my-point)
      (user-error "No table keywords found."))
    (while (and (not (= (point) (point-min))) (org-at-keyword-p))
      (forward-line -1))
    (when (not (org-at-keyword-p))
      (forward-line))
    (while (org-at-keyword-p)
      (when (not (org-match-line org-keyword-regexp))
        (goto-char my-point)
        (user-error "Did not understand keyword line"))
      (push (cons (match-string-no-properties 1) (match-string-no-properties 2)) key-values)
      (forward-line))
    (let ((query (alist-get "ytr-query" key-values nil nil #'string=))
          (columns (alist-get "ytr-columns" key-values nil nil #'string=)))
      (when (not query)
        (goto-char my-point)
        (user-error "No ytr-query found"))
      (let ((properties (if columns
                            (mapcar (lambda (property-name)
                                      (if (boundp (intern property-name))
                                          (symbol-value (intern property-name))
                                        (goto-char my-point)
                                        (user-error "Did not find property symbol %s" property-name)))
                                    (split-string columns "\\s-+"))
                          ytr-issue-properties)))
        (delete-region (org-table-begin) (org-table-end))
        (ytr-query-to-org-table query t properties)
        (goto-char my-point)))))

(defun ytr-update-org-query-table-hook ()
  "Hook function to update org query table on C-c C-c."
  (condition-case nil
      (progn (ytr-update-org-query-table) t)
    (user-error nil)))

(add-to-list 'org-ctrl-c-ctrl-c-hook #'ytr-update-org-query-table-hook)

;;;; Merge operations

(define-error 'ytr-key-error "Key not found" 'error)

(defun ytr-merge-issue-node ()
  "Merge the issue node at point with remote data.
Find the issue node at point, call fetch for all subnodes that
are not locally edited and append missing nodes. Skip subheadings
that are no ytr nodes."
  (interactive)
  (save-mark-and-excursion
    (when (ytr-find-node 'issue)
      (let* ((issue-code (car (ytr-issue-node-cons-from-org-property)))
             (issue-alist (ytr-retrieve-issue-alist issue-code))
             (processed-node-codes '()))
        (let* ((level (org-current-level))
               (end (save-excursion (org-end-of-subtree t t))))
          (while (and (< (point) end)
                      (re-search-forward org-heading-regexp end t))
            (when (= (org-current-level) (1+ level))
              (let* ((current-point (point))
                     (issue-node-code (org-entry-get (point) ytr-org-issue-code-property-name))
                     (node-type (org-entry-get (point) ytr-org-node-type-property-name))
                     node-code)
                (when (and issue-node-code node-type)
                  (message "Found %s %s at %s" node-type issue-node-code (point))
                  (condition-case err
                      (ytr-fetch-remote-node (cl-case (intern node-type)
                                               (description issue-alist)
                                               (comment (setq node-code (cdr (ytr-parse-issue-node-code issue-node-code)))
                                                        (push node-code processed-node-codes)
                                                        (or (cl-find-if (lambda (comment-alist)
                                                                          (string= (alist-get 'id comment-alist) node-code))
                                                                        (alist-get 'comments issue-alist))
                                                            (signal 'ytr-key-error issue-node-code)))
                                               (t (user-error "Bad note type %s" node-type)))
                                             'trust-hash)
                    (ytr-key-error (message "Ignoring unknown node %s" (cdr err))))
                  (goto-char current-point) ; for some reason save-excursion does not work reliably here (maybe because buffer shrinks)
                  ))))
          (goto-char end)
          (mapcar (lambda (comment-alist)
                    (let-alist comment-alist
                      (ytr-org-insert-node .text (+ 1 level) 'comment (cons issue-code .id) (alist-get 'fullName .author) .created .updated .attachments .deleted)))
                  (seq-filter (lambda (comment-alist)
                                (not (or (member (alist-get 'id comment-alist) processed-node-codes)
                                         (eq (alist-get 'deleted comment-alist) t))))
                              (alist-get 'comments issue-alist))))))))

;;;; Capture

(defun ytr-capture-action (issue-node-cons)
  "Capture a proxy org task that references ISSUE-NODE-CONS on ytr."
  (let-alist (ytr-retrieve-issue-alist (car issue-node-cons))
    (setq ytr-capture-summary .summary)
    (setq ytr-capture-issue-code .idReadable)
    (org-capture nil ytr-capture-key)
    (ytr-org-link-heading-action issue-node-cons)))

;;;; Browser integration

(defun ytr-browse-new-issue (title &optional description)
  "Open web browser to create a new issue with TITLE and optional DESCRIPTION."
  (interactive "sSummary: ")
  (browse-url (concat (format "%s/newIssue?summary=%s" ytr-baseurl title)
                      (if description
                          (format "&description=%s" (url-hexify-string description))
                        ""))))

(provide 'ytr-org)
;;; ytr-org.el ends here
