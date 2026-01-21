;; -*- lexical-binding: t; -*-
;;; ytr.el --- Youtrack integration into emacs

;; Copyright (C) 2010-2023 Martin Puttke

;; Author: Martin Puttke <m.s.p@posteo.de>
;; Created: 07 Mar 2023
;; Keywords: convenience
;; URL: https://github.com/matlantis/emacs-ytr
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (request "0.3.0") (embark "0.22") (consult "0.35") (marginalia "1.0"))

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
;; The Package provides useful functions to interact with a YouTrack
;; instance from within Emacs.
;;
;; Essential Commands:
;; - ytr-query: browse your issues
;;
;; This is the main entry point that loads all ytr modules:
;; - ytr-api: HTTP/REST API layer
;; - ytr-ui: Completion, embark, consult, marginalia, preview
;; - ytr-org: Org-mode integration

;;; Change Log:
;;
;; 0.1 - Initial release
;; 0.2 - Modular architecture (split into ytr-api, ytr-ui, ytr-org)

;;; Code:

;;;; Dependencies

(require 'ffap)

;;;; Load ytr modules
(require 'ytr-api)
(require 'ytr-ui)
(require 'ytr-org)

;;;; Customization group

(defgroup ytr nil "Youtrack integration into emacs." :group 'external)

;;;; Configuration variables

(defcustom ytr-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server." :type 'string :group 'ytr)

(defcustom ytr-access-token "" "Your access token. May be a string or function." :type '(choice function string) :group 'ytr)

(defcustom ytr-user-full-name "" "Your full name, used to identify your issues." :type 'string :group 'ytr)

(defcustom ytr-queries () "Define your Queries here." :type '(repeat string) :group 'ytr)

(defcustom ytr-request-debug nil "Print request debug messages if non nil." :type 'boolean :group 'ytr)

(defcustom ytr-use-saved-queries t "Whether to use saved queries or user defined queries." :type 'boolean :group 'ytr)

(defcustom ytr-new-comment-behavior 'kill
  "Define what will be done with the submitted region after comment creation.

One of `kill', `fetch', `keep' or `keep-content'."
  :type '(choice (const kill) (const fetch) (const keep) (const keep-content)) :group 'ytr)

(defcustom ytr-update-node-behavior 'keep-content
  "Define what will be done with the submitted region after a node update.

One of `kill', `fetch', `keep' or `keep-content'."
  :type '(choice (const kill) (const fetch) (const keep) (const keep-content)) :group 'ytr)

(defcustom ytr-export-base-heading-level 2 "Highest Heading Level in exported markdown." :type 'integer :group 'ytr)

(defcustom ytr-only-own-saved-queries t "Filter out saved queries from others." :type 'boolean :group 'ytr)

(defcustom ytr-read-issue-code-function 'ytr-read-issue-code-basic
  "Select Input Method to read an issue-code from user."
  :type 'function :group 'ytr
  :options '(ytr-read-issue-code-basic ytr-read-issue-code-annotated ytr-read-issue-code-consult))

;;;; History and state variables

(defvar ytr-issue-history '() "History for issues.")
(defvar ytr-query-history '() "History for query.")

(defvar ytr-issues-alist '() "Issues alist used for annotations.")
(defvar ytr-queries-alist '() "Queries alist used for annotations.")

(defvar ytr-capture-issue-code "" "Issue Code for org capture template.")
(defvar ytr-capture-summary "" "Summary for org capture template.")

;;;; URL builders

(defun ytr-issue-url (issue-code)
  "Return the URL for an issue given by ISSUE-CODE."
  (concat ytr-baseurl "/issue/" issue-code))

(defun ytr-issue-comment-url (issue-node-cons)
  "Return URL for an issue with focus on comment.
ISSUE-NODE-CONS is (issue-code . node-code)."
  (if (cdr issue-node-cons)
      (concat ytr-baseurl "/issue/" (car issue-node-cons) "#focus=Comments-" (cdr issue-node-cons) ".0-0")
    (ytr-issue-url (car issue-node-cons))))

(defun ytr-query-url (query)
  "Return the URL for a QUERY."
  (concat ytr-baseurl "/issues?q=" (url-hexify-string query)))

;;;; Pattern constants

(defconst ytr-issue-code-pattern "[a-zA-Z]+-[0-9]+"
  "Regex pattern for YouTrack issue codes (e.g., PROJ-123).")

(defconst ytr-node-code-pattern "#\\([0-9-]+\\)"
  "Regex pattern for node codes (e.g., #4-70).")

(defconst ytr-issue-node-code-pattern (format "\\(%s\\)\\(?:%s\\)?" ytr-issue-code-pattern ytr-node-code-pattern)
  "Regex pattern for issue codes with optional node codes.")

(defconst ytr-issue-mandatory-node-code-pattern (format "\\(%s\\)%s" ytr-issue-code-pattern ytr-node-code-pattern)
  "Regex pattern for issue codes with mandatory node codes.")

;;;; Pattern helpers

(defun ytr-delim-pattern (pattern)
  "Add string begin and string end delimiters to PATTERN."
  (format "^%s$" pattern))

(defun ytr-surrounded-pattern (pattern)
  "Add word boundary surroundings to PATTERN."
  (format "\\b\\(%s\\)\\b" pattern))

;;;; Issue code recognition

(add-to-list 'ffap-string-at-point-mode-alist '(ytr "0-9a-zA-Z#-" "" ""))

(defun ytr-parse-issue-node-code (candidate)
  "Parse CANDIDATE string for an issue and a node code if present.
Return a cons (issue-code . node-code)."
  (if (string-match (ytr-delim-pattern ytr-issue-node-code-pattern) candidate)
      (cons (match-string 1 candidate) (match-string 2 candidate))
    nil))

(defun ytr-issue-node-code-from-point ()
  "Return the issue-node-code or issue-code at point or nil if there is none."
  (ytr-parse-issue-node-code (ffap-string-at-point 'ytr)))

(defun ytr-issue-code-from-point ()
  "Return the issue code at point or nil if there is none."
  (car (ytr-issue-node-code-from-point)))

(defun ytr-issue-node-cons-from-org-property ()
  "Return the issue code defined by an org property YTR_ISSUE_CODE or nil."
  (if (derived-mode-p 'org-mode)
      (let ((issue-code (org-entry-get (point) ytr-org-issue-code-property-name t)))
        (when issue-code (ytr-parse-issue-node-code issue-code)))
    nil))

(defun ytr-issue-code-from-branch ()
  "Return the issue code from the name of the current git branch."
  (let ((branch-name (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (if (string-match (format "^\\([a-zA-Z]+/\\)?\\(%s\\)[-_].*$" ytr-issue-code-pattern) branch-name)
        (match-string 2 branch-name))))

(defun ytr-issue-node-code-from-line ()
  "Return the first issue code in current line."
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if (string-match (ytr-surrounded-pattern ytr-issue-node-code-pattern) line)
        (match-string 0 line)
      nil)))

(defun ytr-issue-node-cons-from-line ()
  "Return issue-node cons from current line."
  (let ((code (ytr-issue-node-code-from-line)))
    (when code (ytr-parse-issue-node-code code))))

;;;; Guessing functions

(defun ytr-guess-issue-code ()
  "Return an issue code from current context."
  (interactive)
  (let ((issue-code (ytr-issue-code-from-point)))
    (if issue-code issue-code
      (let ((issue-code (car (ytr-issue-node-cons-from-line))))
        (if issue-code issue-code
          (let ((issue-node-cons (ytr-issue-node-cons-from-org-property)))
            (if issue-node-cons (car issue-node-cons)
              (let ((issue-code (ytr-issue-code-from-branch)))
                (if issue-code issue-code nil)))))))))

(defun ytr-guess-issue-node-cons ()
  "Return a cons cell from issue code and node code from current context."
  (interactive)
  (let ((issue-node-cons (ytr-issue-node-code-from-point)))
    (if issue-node-cons issue-node-cons
      (let ((issue-node-cons (ytr-issue-node-cons-from-line)))
        (if issue-node-cons issue-node-cons
          (let ((issue-node-cons (ytr-issue-node-cons-from-org-property)))
            (if issue-node-cons issue-node-cons
              (let ((issue-code (ytr-issue-code-from-branch)))
                (if issue-code (cons issue-code nil) nil)))))))))

(defun ytr-guess-or-read-issue-code ()
  "Guess issue code from context or start a query."
  (let ((guess (ytr-guess-issue-code)))
    (if guess guess (ytr-read-issue-code))))

(defun ytr-guess-or-read-issue-node-cons ()
  "Guess issue code with node code from context or start a query."
  (let ((guess (ytr-guess-issue-node-cons)))
    (if guess guess (cons (ytr-read-issue-code) nil))))

;;;; History functions

(defun ytr-retrieve-history-issues-alist ()
  "Retrieve issue alists for all issues in history."
  (let ((result))
    (dolist (elt (reverse ytr-issue-history) result)
      (setq result (cons (ytr-retrieve-issue-alist elt) result)))))

(defun ytr-add-issue-to-history (issue-node-code)
  "Add ISSUE-NODE-CODE to history."
  (delete issue-node-code ytr-issue-history)
  (push issue-node-code ytr-issue-history))

;;;; Reading functions wrapper

(defun ytr-read-issue-code ()
  "Wrapper function to read an issue code from user."
  (funcall ytr-read-issue-code-function))

;;;; URL and code actions

(defun ytr-url (issue-node-cons)
  "Get the URL for ISSUE-NODE-CONS."
  (let* ((issue-code (car issue-node-cons))
         (node-code (cdr issue-node-cons)))
    (if node-code
        (ytr-issue-comment-url (cons issue-code node-code))
      (ytr-issue-url issue-code))))

(defun ytr-copy-url-action (issue-node-cons)
  "Copy URL for ISSUE-NODE-CONS to kill ring."
  (let ((url (ytr-url issue-node-cons)))
    (message url)
    (kill-new url)))

(defun ytr-issue-node-code-action (issue-node-cons)
  "Return a string representing ISSUE-NODE-CONS."
  (if (cdr issue-node-cons)
      (format "%s#%s" (car issue-node-cons) (cdr issue-node-cons))
    (car issue-node-cons)))

(defun ytr-copy-issue-node-code-action (issue-node-cons)
  "Put a string for ISSUE-NODE-CONS on kill ring and print it as message."
  (let ((issue-node-code (ytr-issue-node-code-action issue-node-cons)))
    (kill-new issue-node-code)
    (message issue-node-code)))

(defun ytr-insert-issue-node-code-action (issue-node-cons)
  "Insert a string for ISSUE-NODE-CONS."
  (insert (ytr-issue-node-code-action issue-node-cons)))

(defalias 'ytr-message-issue-node-code-action 'ytr-copy-issue-node-code-action
  "Put a string for issue and node code on kill ring and print it as message.")

;;;; Browser actions

(defun ytr-browse-action (issue-node-cons)
  "Open ISSUE-NODE-CONS in web browser."
  (browse-url (ytr-url issue-node-cons)))

(defun ytr-read-refine-browse ()
  "Edit a predefined query to find an issue and open it in the browser."
  (interactive)
  (let* ((query-orig (completing-read "Query: " ytr-queries nil t))
         (query (read-string "Refine query: " query-orig nil))
         (issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          issues-alist))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
    (browse-url (ytr-issue-url choice-id))))

(defun ytr-browse-query (query)
  "Open web browser to execute QUERY."
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-query-consult))))
  (browse-url (concat ytr-baseurl "/issues?q=" query)))

;;;; Action macros

(defmacro ytr-define-dart-action (name action)
  "Define a dart form action NAME that calls ACTION.
Dart form uses simple prompt for issue-node-code."
  `(defun ,(intern (format "ytr-dart-%s" name)) (issue-node-code)
     ,(format "Dart form of %s." name)
     (interactive "sIssue Code (may also have Node Code): ")
     (funcall ,action (ytr-parse-issue-node-code issue-node-code))))

(defmacro ytr-define-embark-action (name action)
  "Define an embark form action NAME that calls ACTION.
Embark form extracts issue code from candidate."
  `(defun ,(intern (format "ytr-embark-%s" name)) (cand)
     ,(format "Embark form of %s." name)
     (funcall ,action (ytr-parse-issue-node-code (car (split-string cand ":"))))))

(defmacro ytr-define-base-action (name action)
  "Define a base form action NAME that calls ACTION.
Base form retrieves an issue from query."
  `(defun ,(intern (format "ytr-%s" name)) (issue-code)
     ,(format "Base form of %s." name)
     (interactive (list
                   (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                     (ytr-read-issue-code))))
     (ytr-add-issue-to-history issue-code)
     (funcall ,action (cons issue-code nil))))

(defmacro ytr-define-smart-action (name action)
  "Define a smart form action NAME that calls ACTION.
Smart form guesses issue from context."
  `(defun ,(intern (format "ytr-smart-%s" name)) (issue-node-code)
     ,(format "Smart form of %s." name)
     (interactive (list (ytr-guess-or-read-issue-node-cons)))
     (ytr-add-issue-to-history (car issue-node-code))
     (funcall ,action issue-node-code)))

(defmacro ytr-define-action (name action)
  "Define all four action forms for NAME calling ACTION."
  `(progn
     (ytr-define-dart-action ,name ,action)
     (ytr-define-embark-action ,name ,action)
     (ytr-define-base-action ,name ,action)
     (ytr-define-smart-action ,name ,action)))

;;;; Action definitions

(defun ytr-message-action (issue-node-cons)
  "Display ISSUE-NODE-CONS as message."
  (message "Issue Code %s, Node Code %s" (car issue-node-cons) (cdr issue-node-cons)))

(ytr-define-action "message-issue-node-code" 'ytr-message-issue-node-code-action)
(ytr-define-action "copy-issue-node-code" 'ytr-copy-issue-node-code-action)
(ytr-define-action "insert-issue-node-code" 'ytr-insert-issue-node-code-action)
(ytr-define-action "browse" 'ytr-browse-action)
(ytr-define-action "org" 'ytr-org-action)
(ytr-define-action "sneak" 'ytr-sneak-action)
(ytr-define-action "copy-url" 'ytr-copy-url-action)
(ytr-define-action "org-link-heading" 'ytr-org-link-heading-action)
(ytr-define-action "org-capture" 'ytr-capture-action)
(ytr-define-action "send-attachments" 'ytr-send-attachments-action)
(ytr-define-action "quick-comment" 'ytr-quick-comment-action)
(ytr-define-action "quick-node-edit" 'ytr-quick-node-edit-action)
(ytr-define-action "find-org-node" 'ytr-find-org-node-action)

;;;; Provide

(provide 'ytr)
;;; ytr.el ends here
