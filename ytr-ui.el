;; -*- lexical-binding: t; -*-
;;; ytr-ui.el --- UI layer for ytr (completion, embark, consult, preview)

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
;; UI layer for YouTrack integration.
;; This module handles completion frameworks, embark integration,
;; consult integration, marginalia annotations, and preview windows.

;;; Code:

(require 'embark)
(require 'consult)
(require 'marginalia)
(require 'ytr-api)

;;;; Configuration variables (defined in main ytr.el, declared here)
(defvar ytr-baseurl)
(defvar ytr-queries)
(defvar ytr-user-full-name)
(defvar ytr-use-saved-queries)
(defvar ytr-only-own-saved-queries)
(defvar ytr-issue-history)
(defvar ytr-query-history)
(defvar ytr-issues-alist)
(defvar ytr-queries-alist)
(defvar ytr-issue-code-pattern)
(defvar ytr-node-code-pattern)
(defvar ytr-issue-node-code-pattern)

;;;; Forward declarations for functions defined in other modules
(declare-function ytr-parse-issue-node-code "ytr")
(declare-function ytr-delim-pattern "ytr")
(declare-function ytr-add-issue-to-history "ytr")
(declare-function ytr-issue-node-code-action "ytr")

;;;; Marginalia setup

(add-to-list 'marginalia-annotators '(ytr-issue-code ytr-annotate-issue-code builtin none))
(add-to-list 'marginalia-annotators '(ytr-query ytr-annotate-query builtin none))

(defun ytr-completing-read-categorised (prompt choices category)
  "Like `completing-read' but puts a CATEGORY on the CHOICES.
PROMPT is the prompt string."
  (completing-read prompt (lambda (str pred flag)
                            (pcase flag
                              ('metadata `(metadata (category . ,category)))
                              (_
                               (all-completions str choices pred))))))

(defun ytr-get-issue-activity (issue-alist)
  "Return the most recent timestamp of an issue with an info string as cons (info . ts).
ISSUE-ALIST is the issue data."
  (let-alist issue-alist
    (if .resolved (cons "resolved" .resolved)
      (if (and .updated (> (- .updated 10000) .created)) (cons "updated" .updated)
        (cons "created" .created)))))

(defun ytr-activity-string (activity)
  "Format ACTIVITY cons cell as a human-readable string."
  (let ((info (car activity))
        (timestamp (cdr activity)))
    (format "%s %s" info (format-time-string "%Y-%m-%d %H:%M" (/ timestamp 1000)))))

(defun ytr-annotate-issue-code (cand)
  "Annotate issue-code CAND with some info."
  (let* ((issue-code (car (split-string cand ":")))
         (issue-alist (cl-find-if (lambda (elem)
                                    (string= (alist-get 'idReadable elem) issue-code))
                                  ytr-issues-alist))) ;; ytr-issues-alist comes from ytr-read-issue-code-annotated via dynamic binding!
    (let-alist issue-alist
      (marginalia--fields
       ((ytr-get-customField-value issue-alist "Priority") :format "Pr: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "Type") :format "Ty: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "State") :format "St: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "Assignee") :format "As: %s" :truncate .2 :face 'marginalia-documentation)
       ((length (alist-get 'comments issue-alist)) :format "Re: %s" :truncate 7 :face 'marginalia-documentation)
       ((ytr-activity-string (ytr-get-issue-activity issue-alist)) :format "%s" :truncate 26 :face 'marginalia-documentation)
       ))))

(defun ytr-annotate-query (cand)
  "Annotate query CAND with some info."
  (let* ((query-alist (cl-find-if (lambda (elem) (string= (alist-get 'query elem) cand)) ytr-queries-alist))
         (name (alist-get 'name query-alist))
         (total (alist-get 'issues query-alist))
         (unresolved (seq-drop-while (lambda (issue) (alist-get 'resolved issue)) total))
         (mine (seq-take-while (lambda (issue) (string= ytr-user-full-name (ytr-get-customField-value issue "Assignee"))) unresolved))
         ) ;; queries-alist comes from ytr-read-issue-code-annotated via dynamic binding!
    (marginalia--fields
     (name :truncate .5 :face 'marginalia-documentation)
     ((length total) :format "Total: %s" :truncate .2 :face 'marginalia-type)
     ((length unresolved) :format "Open: %s" :truncate .2 :face 'marginalia-type)
     ((length mine) :format "Mine: %s" :truncate .2 :face 'marginalia-type)
     )))

;;;; Basic reading functions

(defun ytr-read-issue-code-basic ()
  "Return an issue code from a query using basic completion."
  (let* ((query (completing-read "Query: " ytr-queries nil nil))
         (issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          issues-alist))
         (choice (completing-read "Issue: " choices)))
    (car (split-string choice ":"))))

(defun ytr-read-issue-code-annotated ()
  "Return an issue-code from a query with marginalia annotations."
  (interactive)
  (let ((query (completing-read "Query: " ytr-queries nil nil)))
    (if (string-match-p "^[a-zA-Z]+-[0-9]+$" query) query ;; if query is already an issue-code
      (let ((ytr-issues-alist (ytr-retrieve-query-issues-alist query)))
        (if (length> ytr-issues-alist 0)
            (ytr-completing-read-categorised "Issue: "
                                             (mapcar (lambda (item) (alist-get 'idReadable item)) ytr-issues-alist)
                                             'ytr-issue-code)
          (user-error "Query returned empty results."))))))

;;;; Consult integration

(defun ytr-consult-state-function (action cand)
  "Consult state function for issue preview.
ACTION is the consult action, CAND is the candidate."
  (cl-case action
    (preview (ytr-sneak-window-issue
              (cl-find-if (lambda (elem) (string= (alist-get 'idReadable elem) (car (split-string cand ":")))) ytr-issues-alist)))
    (exit (quit-window))))

(defun ytr-read-issue-code-from-query-consult (query)
  "Use consult to get the issue code from a given QUERY."
  (let* ((ytr-issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          ytr-issues-alist)))
    (car (split-string (consult--read choices
                                      :category 'ytr-issue-node-code
                                      :state 'ytr-consult-state-function
                                      :require-match t
                                      :sort nil
                                      :history 'ytr-issue-history
                                      )
                       ":"))))

(defun ytr-read-query-var-consult ()
  "Use consult to get a query from custom var."
  (consult--read ytr-queries
                 :sort nil
                 :history 'ytr-query-history))

(defun ytr-read-query-saved-consult ()
  "Use consult to get a query from saved queries."
  (let* ((ytr-queries-alist (ytr-retrieve-saved-queries-alist))
         (queries-alist-filtered (seq-filter (lambda (elem)
                                               (string= (alist-get 'fullName (alist-get 'owner elem))
                                                        ytr-user-full-name))
                                             ytr-queries-alist))
         (choices (mapcar (lambda (item) (alist-get 'query item)) queries-alist-filtered)))
    (consult--read choices
                   :sort nil
                   :category 'ytr-query
                   :history 'ytr-query-history)))

(defun ytr-read-query-consult ()
  "Use consult to read a query."
  (if ytr-use-saved-queries (ytr-read-query-saved-consult) (ytr-read-query-var-consult)))

(defun ytr-read-issue-code-consult ()
  "Use consult to read a issue code."
  (ytr-read-issue-code-from-query-consult (if ytr-use-saved-queries (ytr-read-query-saved-consult) (ytr-read-query-var-consult))))

;;;; Embark integration

(defvar-keymap embark-ytr-issue-node-code-actions
  :doc "Keymap for actions for ytr issue node code"
  :parent embark-general-map
  "w" #'ytr-embark-browse
  "o" #'ytr-embark-org
  "p" #'ytr-embark-sneak
  "u" #'ytr-embark-copy-url
  "y" #'ytr-embark-copy-issue-node-code
  "i" #'ytr-embark-insert-issue-node-code
  "m" #'ytr-embark-message-issue-node-code
  "l" #'ytr-embark-org-link-heading
  "c" #'ytr-embark-org-capture)

(add-to-list 'embark-keymap-alist '(ytr-issue-node-code . embark-ytr-issue-node-code-actions))

(defvar-keymap embark-ytr-query-actions
  :doc "Keymap for actions for ytr queries"
  :parent embark-general-map
  "w" #'ytr-browse-query)

(add-to-list 'embark-keymap-alist '(ytr-query . embark-ytr-query-actions))

(defun ytr-embark-issue-node-code-target-finder ()
  "Find issue code and node code for embark."
  (save-excursion
    (let* ((start (progn (skip-chars-backward "[:alnum:]-#") (point)))
           (end (progn (skip-chars-forward "[:alnum:]-#") (point)))
           (str (buffer-substring-no-properties start end)))
      (save-match-data
        (when (string-match-p (ytr-delim-pattern ytr-issue-node-code-pattern) str)
          `(ytr-issue-node-code
            ,str
            ,start . ,end))))))

(add-to-list 'embark-target-finders 'ytr-embark-issue-node-code-target-finder)

;;;; Preview faces

(defface ytr-preview-field-name-face '((t . (:inherit font-lock-variable-name-face))) "Font used for field names in ytr preview window.")
(defface ytr-preview-field-value-face '((t . (:inherit font-lock-warning-face :weight bold))) "Font used for field values in ytr preview window.")
(defface ytr-preview-issue-code-face '((t . (:inherit org-todo :inverse-video t :weight bold))) "Font used for issue codes in ytr preview window.")
(defface ytr-preview-summary-face '((t . (:inherit org-todo :weight bold))) "Font used for summaries in ytr preview window.")

;;;; Issue property definitions for preview/tables

(defconst ytr-issue-property-id
  '("ID" . (lambda (issue-alist) (alist-get 'idReadable issue-alist)))
  "Field Definition for Sneak Preview to show the ID.")
(defconst ytr-issue-property-summary
  '("Summary" . (lambda (issue-alist) (alist-get 'summary issue-alist)))
  "Field Definition for Sneak Preview to show the summary.")
(defconst ytr-issue-property-created
  '("created" . (lambda (issue-alist) (let-alist issue-alist (if .created (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000)) "-"))))
  "Field Definition for Sneak Preview to show the created.")
(defconst ytr-issue-property-updated
  '("updated" . (lambda (issue-alist) (let-alist issue-alist (if .updated (format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) "-"))))
  "Field Definition for Sneak Preview to show the updated.")
(defconst ytr-issue-property-resolved
  '("resolved" . (lambda (issue-alist) (let-alist issue-alist (if .resolved (format-time-string "%Y-%m-%d %H:%M" (/ .resolved 1000)) "-"))))
  "Field Definition for Sneak Preview to show the Resolved.")
(defconst ytr-issue-property-reporter
  '("Reporter" . (lambda (issue-alist) (alist-get 'fullName (alist-get 'reporter issue-alist))))
  "Field Definition for Sneak Preview to show the Reporter.")
(defconst ytr-issue-property-type
  '("Type" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Type")))
  "Field Definition for Sneak Preview to show the Type.")
(defconst ytr-issue-property-priority
  '("Priority" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Priority")))
  "Field Definition for Sneak Preview to show the Priority.")
(defconst ytr-issue-property-state
  '("State" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "State")))
  "Field Definition for Sneak Preview to show the State.")
(defconst ytr-issue-property-assignee
  '("Assignee" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Assignee")))
  "Field Definition for Sneak Preview to show the Assignee.")
(defconst ytr-issue-property-comments
  '("Comments" . (lambda (issue-alist) (format "%i" (length (alist-get 'comments issue-alist)))))
  "Field Definition for Sneak Preview to show the Comments.")
(defconst ytr-issue-property-activity
  '("Activity" . (lambda (issue-alist)
                   (ytr-activity-string (ytr-get-issue-activity issue-alist))))
  "Field Definition for Sneak Preview to show the recent activity.")
(defconst ytr-issue-property-subsystem
  '("Subsystem" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Subsystem")))
  "Field Definition for Sneak Preview to show the Subsystem.")
(defconst ytr-issue-property-fixversions
  '("Fix versions" . (lambda (issue-alist) (ytr-get-customField-list-value issue-alist "Fix versionss")))
  "Field Definition for Sneak Preview to show the Fix versions.")

(defcustom ytr-issue-properties
  (list ytr-issue-property-fixversions ytr-issue-property-subsystem ytr-issue-property-priority ytr-issue-property-type ytr-issue-property-state ytr-issue-property-reporter ytr-issue-property-assignee ytr-issue-property-comments ytr-issue-property-activity)
  "List of fields to print in sneak window for issues.

Each entry is a cons of a format definition and a function to compute the value,
which receives as argument den issue-alist."
  :type '(repeat (cons string function)) :group 'ytr)

;;;; Preview window functions

(defun ytr-sneak-window-issue (issue-alist)
  "Display a side window with the description and basic information on issue.
ISSUE-ALIST is the issue data."
  (let ((buf (get-buffer-create "*ytr-describe-issue*")))
    (with-current-buffer buf
      (erase-buffer)
      (let-alist issue-alist
        (insert (format "%s %s\n"
                        (propertize (format " %s ".idReadable) 'face 'ytr-preview-issue-code-face)
                        (propertize .summary 'face 'ytr-preview-summary-face)))
        (insert (string-join (mapcar (lambda (field)
                                       (concat (propertize (format "%s: " (car field)) 'face 'ytr-preview-field-name-face)
                                               (propertize (format "%s" (funcall (cdr field) issue-alist)) 'face 'ytr-preview-field-value-face)))
                                     ytr-issue-properties) "  "))
        (unless (= (length .attachments) 0)
          (insert "\nAttachments:")
          (mapc (lambda (attachment-alist)
                  (let-alist attachment-alist
                    (insert (format " [\"%s\" %s %s]" .name .mimeType (file-size-human-readable .size)))))
                .attachments))
        (insert (propertize "\n------------------------\n" 'face 'shadow))
        (if .description (insert .description))
        (goto-char (point-min))))
    (display-buffer buf)))

(defun ytr-sneak-window-comment (issue-alist node-code)
  "Display a side window with the description and basic information on the comment.
ISSUE-ALIST is the issue data, NODE-CODE is the comment id."
  (with-output-to-temp-buffer "*ytr-describe-comment*"
    (let-alist (cl-find-if (lambda (elem)
                             (string= (alist-get 'id elem) node-code))
                           (alist-get 'comments issue-alist))
      (princ (format "Comment for %s: %s\n" (alist-get 'idReadable issue-alist) (alist-get 'summary issue-alist)))
      (princ (format "Author: %s, created: %s, updated: %s"
                     (alist-get 'fullName .author)
                     (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000))
                     (if .updated (format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) "-")))
      (unless (= (length .attachments) 7)
        (princ "\nAttachments:")
        (mapc (lambda (attachment-alist)
                (let-alist attachment-alist
                  (princ (format " [\"%s\" %s %s]" .name .mimeType (file-size-human-readable .size)))))
              .attachments))
      (princ "\n------------------------\n")
      (princ .text))))

(defun ytr-sneak-action (issue-node-cons)
  "Display a side window with some basic information on issue and comment.
ISSUE-NODE-CONS is a cons cell (issue-code . node-code)."
  (let* ((issue-code (car issue-node-cons))
         (node-code (cdr issue-node-cons))
         (issue-alist (ytr-retrieve-issue-alist issue-code)))
    (ytr-sneak-window-issue issue-alist)
    (when node-code (ytr-sneak-window-comment issue-alist node-code))))

;;;; Issue buttons

(define-button-type 'ytr-issue-node-code-button
  'follow-link t
  'action #'ytr-on-issue-node-code-button)

(defun ytr-on-issue-node-code-button (button)
  "Handle click on issue BUTTON."
  (let ((issue-node-cons (ytr-parse-issue-node-code (buffer-substring (button-start button) (button-end button)))))
    (ytr-dart-browse issue-node-cons)))

(defun ytr-issue-node-code-buttonize-buffer ()
  "Turn all issue-node-codes into buttons."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (format "\\b\\(%s\\)\\b" ytr-issue-node-code-pattern)))
      (while (re-search-forward pattern nil t)
        (make-button (match-beginning 2) (match-end 2) :type 'ytr-issue-node-code-button)))))

(add-hook 'org-mode-hook 'ytr-issue-node-code-buttonize-buffer)

;;;; Helm support (optional)

(if (require 'helm nil t)
    (progn
      (defun ytr-helm (&optional defaultquery)
        "Use Helm to select an issue from a query and open it.
DEFAULTQUERY is the initial query string."
        (interactive)
        (let* ((query (completing-read "Query: " ytr-queries nil nil defaultquery))
               (issues-alist (ytr-retrieve-query-issues-alist query))
               (choices (mapcar (lambda (issue-alist)
                                  (let-alist issue-alist
                                    (cons (format "%s: %s" .idReadable .summary) issue-alist))
                                  )
                                issues-alist))
               )
          (if choices
              (helm :sources (helm-build-sync-source
                                 "ytr-issues"
                               :candidates choices
                               :action '(("Open in browser" . (lambda (issue-alist)
                                                                (let ((issue-code (alist-get 'idReadable issue-alist)))
                                                                  (ytr-add-issue-to-history issue-code)
                                                                  (browse-url (ytr-issue-url issue-code)))))
                                         ("Open in org buffer" . (lambda (issue-alist)
                                                                   (let ((issue-code (alist-get 'idReadable issue-alist)))
                                                                     (ytr-add-issue-to-history issue-code)
                                                                     (ytr-dart-org issue-code)))))
                               :must-match 'ignore
                               :persistent-action 'ytr-sneak-window-issue
                               :keymap (let ((map (make-sparse-keymap)))
                                         (set-keymap-parent map helm-map)
                                         (define-key map (kbd "M-q") (lambda () (interactive) (helm-run-after-exit 'ytr-helm-query query)))
                                         (define-key map (kbd "M-Q") (lambda () (interactive) (helm-run-after-exit 'ytr-helm-query)))
                                         (define-key map (kbd "M-w") (lambda () (interactive) (helm-run-after-exit 'browse-url (ytr-query-url query))))
                                         map)
                               :cleanup (lambda () (kill-matching-buffers "*ytr-describe-issue*" nil t))
                               )
                    :buffer "*helm ytr*")
            (message "No Issues found."))))

      (defun ytr-helm-history ()
        "Use Helm to select an issue from issue history and open it."
        (interactive)
        (let* ((issues-alist (ytr-retrieve-history-issues-alist))
               (choices (mapcar (lambda (issue-alist)
                                  (let-alist issue-alist
                                    (cons (format "%s: %s" .idReadable .summary) issue-alist))
                                  )
                                issues-alist))
               )
          (if choices
              (helm :sources (helm-build-sync-source "ytr-issues"
                               :candidates choices
                               :action '(("Open in browser" . (lambda (issue-alist) (browse-url (ytr-issue-url (alist-get 'idReadable issue-alist)))))
                                         ("Open in org buffer" . (lambda (issue-alist) (ytr-dart-org (alist-get 'idReadable issue-alist)))))
                               :must-match 'ignore
                               :persistent-action 'ytr-sneak-window-issue
                               :cleanup (lambda () (kill-matching-buffers "*ytr-describe-issue*" nil t))
                               )
                    :buffer "*helm ytr*")
            (message "No Issues found."))
          ))
      ))

(provide 'ytr-ui)
;;; ytr-ui.el ends here
