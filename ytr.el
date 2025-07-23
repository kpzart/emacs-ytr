;; -*- lexical-binding: t; -*-
;;; ytr.el --- Youtrack integration into emacs

;; Copyright (C) 2010-2023 Martin Puttke

;; Author: Martin Puttke <m.s.p@posteo.de>
;; Created: 07 Mar 2023
;; Keywords: convenience
;; URL: https://github.com/matlantis/emacs-ytr
;; Version: 0.1
;; Package-Requires: ()
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
;; The Package provides some usefull functions to interact with a youtrack
;; instance from within emacs.
;;
;; Essential Commands:
;; - ytr-query: browse your issues

;;; Change Log:
;;
;; empty

;;; Code:

;;;; Basic

(require 'ffap)
(require 'request)
(require 'embark)
(require 'consult)
(require 'marginalia)
(require 'diff)
(require 'org)


(defgroup ytr nil "Youtrack integration into emacs")

(defcustom ytr-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server" :type 'string :group 'ytr)

(defcustom ytr-access-token "" "Your access token. Maybe a string or function" :type '(choice function string) :group 'ytr)

(defcustom ytr-user-full-name "" "Your login name, used to identify your issues" :type 'string :group 'ytr)

(defcustom ytr-queries () "Define your Queries here" :type '(repeat string) :group 'ytr)

(defcustom ytr-request-debug nil "Print request debug messages if non nil" :type 'boolean :group 'ytr)

(defcustom ytr-use-saved-queries t "Wether to use saved queries of user defined queries" :type 'boolean :group 'ytr)

(defcustom ytr-new-comment-behavior 'kill
  "Define what will be done with the submitted region after comment creation.

One of \='kill\=, \='fetch\=, \='keep\= or \='keep-content.\="
  :type '(choice (const kill) (const fetch) (const keep) (const keep-content)) :group 'ytr)

(defcustom ytr-update-node-behavior 'keep-content
  "Define what will be done with the submitted region after a node update.

One of \='kill\=, \='fetch\=, \='keep\= or \='keep-content.\="
  :type '(choice (const kill) (const fetch) (const keep) (const keep-content)) :group 'ytr)

(defcustom ytr-export-base-heading-level 2 "Highest Heading Level in exported markdown" :type 'integer :group 'ytr)

(defvar ytr-issue-history '() "History for issues")
(defvar ytr-query-history '() "History for query")

(defvar ytr-issues-alist '() "Issues alist used for annotations")
(defvar ytr-queries-alist '() "Queries alist used for annotations")

(defvar ytr-capture-issue-code "" "Issue Code for org capture template")
(defvar ytr-capture-summary "" "Summary for org capture template")

(defvar-local ytr-buffer-position nil "Buffer local var to store position")
(defvar-local ytr-buffer-text nil "Buffer local var to store text")
(defvar-local ytr-buffer-wconf nil "Buffer local var to store wconf")
(defvar-local ytr-buffer-curlevel nil "Buffer local var to store curlevel")
(defvar-local ytr-buffer-issue-code nil "Buffer local var to store issue-code")
(defvar-local ytr-buffer-node-code nil "Buffer local var to store node-code")
(defvar-local ytr-buffer-node-type nil "Buffer local var to store node-type")
(defvar-local ytr-buffer-commit-type nil "Buffer local var to store commit-type")
(defvar-local ytr-buffer-local-content-hash nil "Buffer local var to store local (org) content hash")

;;;; urls
(defun ytr-issue-url (issue-code)
  "Return the URL for a issue given by issue-code"
  (concat ytr-baseurl "/issue/" issue-code))

(defun ytr-issue-comment-url (issue-node-cons)
  "Return URL for a issue given by issue-code with focus on comment with node-code"
  (if (cdr issue-node-cons)
      (concat ytr-baseurl "/issue/" (car issue-node-cons) "#focus=Comments-" (cdr issue-node-cons) ".0-0")
    (ytr-issue-url (car issue-node-cons))))

(defun ytr-query-url (query)
  "Return the URL for a query"
  (concat ytr-baseurl "/issues?q=" (url-hexify-string query)))

;;;; read issue node codes
(defun ytr-read-issue-code-basic ()
  "Return an issue code from a query"
  (let* ((query (completing-read "Query: " ytr-queries nil nil))
         (issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          issues-alist))
         (choice (completing-read "Issue: " choices)))
    (car (split-string choice ":"))))

;;;;; marginalia

(add-to-list 'marginalia-annotator-registry '(ytr-issue-code ytr-annotate-issue-code builtin none))
(add-to-list 'marginalia-annotator-registry '(ytr-query ytr-annotate-query builtin none))

(defun ytr-completing-read-categorised (prompt choices category)
  "Like completing-read but puts a category on the choices."
  (completing-read prompt (lambda (str pred flag)
                            (pcase flag
                              ('metadata `(metadata (category . ,category)))
                              (_
                               (all-completions str choices pred))))))

(defun ytr-get-issue-activity (issue-alist)
  "Return the most recent timestamp of an issue with an info string as cons (info . ts)"
  (let-alist issue-alist
    (if .resolved (cons "resolved" .resolved)
      (if (and .updated (> (- .updated 10000) .created)) (cons "updated" .updated)
        (cons "created" .created)))))

(defun ytr-activity-string (activity)
  (let ((info (car activity))
        (timestamp (cdr activity)))
    (format "%s %s" info (format-time-string "%Y-%m-%d %H:%M" (/ timestamp 1000)))))

(defun ytr-annotate-issue-code (cand)
  "Annotate issue-code with some info"
  (let* ((issue-code (car (split-string cand ":")))
         (issue-alist (cl-find-if (lambda (elem)
                                    (string= (alist-get 'idReadable elem) issue-code))
                                  ytr-issues-alist))) ;; ytr-issues-alist comes from ytr-read-issue-code-annotated via dynamic binding!
    (let-alist issue-alist
      (marginalia--fields
       ;; (:left .summary :format " %s" :face 'marginalia-type)
       ((ytr-get-customField-value issue-alist "Priority") :format "Pr: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "Type") :format "Ty: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "State") :format "St: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "Assignee") :format "As: %s" :truncate .2 :face 'marginalia-documentation)
       ((length (alist-get 'comments issue-alist)) :format "Re: %s" :truncate 7 :face 'marginalia-documentation)
       ((ytr-activity-string (ytr-get-issue-activity issue-alist)) :format "%s" :truncate 26 :face 'marginalia-documentation)
       ;; ((if .created (format-time-string "%Y-%m-%d" (/ .created 1000)) "-") :format "ct: %s" :truncate 20 :face 'marginalia-documentation)
       ;; ((if .updated (format-time-string "%Y-%m-%d" (/ .updated 1000)) "-") :format "ut: %s" :truncate 20 :face 'marginalia-documentation)
       ))))

(defun ytr-annotate-query (cand)
  "Annotate queries with some info"
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

(defconst ytr-issue-code-pattern "[a-zA-Z]+-[0-9]+")
(defconst ytr-node-code-pattern "#\\([0-9-]+\\)")
(defconst ytr-issue-node-code-pattern (format "\\(%s\\)\\(?:%s\\)?" ytr-issue-code-pattern ytr-node-code-pattern))
(defconst ytr-issue-mandatory-node-code-pattern (format "\\(%s\\)%s" ytr-issue-code-pattern ytr-node-code-pattern))

(defun ytr-delim-pattern (pattern)
  "Add string begin and string end delimiters to pattern"
  (format "^%s$" pattern))

(defun ytr-surrounded-pattern (pattern)
  "Add surroundings to pattern"
  (format "\\b\\(%s\\)\\b" pattern))

(defun ytr-read-issue-code-annotated ()
  "Return an issue-code from a query"
  (interactive)
  (let ((query (completing-read "Query: " ytr-queries nil nil)))
    (if (string-match-p "^[a-zA-Z]+-[0-9]+$" query) query ;; if query is already an issue-code
      (let ((ytr-issues-alist (ytr-retrieve-query-issues-alist query)))
        (if (length> ytr-issues-alist 0)
            (ytr-completing-read-categorised "Issue: "
                                             (mapcar (lambda (item) (alist-get 'idReadable item)) ytr-issues-alist)
                                             'ytr-issue-code)
          (user-error "Query returned empty results."))))))

;;;;; consult
(defun ytr-consult-state-function (action cand)
  ""
  (cl-case action
    (preview (ytr-sneak-window-issue
              (cl-find-if (lambda (elem) (string= (alist-get 'idReadable elem) (car (split-string cand ":")))) ytr-issues-alist)))
    (exit (quit-window))))

(defun ytr-read-issue-code-from-query-consult (query)
  "Use consult to get the issue code from a given QUERY"
  (let* ((ytr-issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          ytr-issues-alist)))
    (car (split-string (consult--read choices
                                      :category 'ytr-issue-code
                                      :state 'ytr-consult-state-function
                                      :require-match t
                                      :sort nil
                                      :history 'ytr-issue-history
                                      ;; :add-history (list (ytr-guess-issue-code)) ;; klappt noch nicht
                                      ;; :keymap tobedone
                                      )
                       ":"))))

(defun ytr-read-query-var-consult ()
  "Use consult to get a query from custom var"
  (consult--read ytr-queries
                 :sort nil
                 :history 'ytr-query-history))

(defcustom ytr-only-own-saved-queries t "Filter out saved queries from others" :type 'boolean :group 'ytr)

(defun ytr-read-query-saved-consult ()
  "Use consult to get a query from saved queries"
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
  "Use consult to read a query"
  (if ytr-use-saved-queries (ytr-read-query-saved-consult) (ytr-read-query-var-consult)))

(defun ytr-read-issue-code-consult ()
  "Use consult to read a issue code."
  (ytr-read-issue-code-from-query-consult (if ytr-use-saved-queries (ytr-read-query-saved-consult) (ytr-read-query-var-consult))))

;;;;; customvar

(defcustom ytr-read-issue-code-function 'ytr-read-issue-code-basic "Select Input Method to read an issue-code from user" :type 'function :group 'ytr :options '(ytr-read-issue-code-basic ytr-read-issue-code-annotated ytr-read-issue-code-consult))

(defun ytr-read-issue-code ()
  "Wrapper function to read an issue code from user"
  (funcall ytr-read-issue-code-function))

;;;; Recognize issue node codes
(add-to-list 'ffap-string-at-point-mode-alist '(ytr "0-9a-zA-Z#-" "" ""))

(defun ytr-parse-issue-node-code (candidate)
  "Parse string for an issue and a node code if present and return a cons
(issue-code . node-code)."
  (if (string-match (ytr-delim-pattern ytr-issue-node-code-pattern) candidate)
      (cons (match-string 1 candidate) (match-string 2 candidate))
    nil))

(defun ytr-issue-node-code-from-point ()
  "Return the issue-node-code or issue-code at point or nil if there is none."
  (ytr-parse-issue-node-code (ffap-string-at-point 'ytr)))

(defun ytr-issue-code-from-point ()
  "Return the issue code at point or nil if there is none"
  (car (ytr-issue-node-code-from-point)))

(defun ytr-issue-node-cons-from-org-property ()
  "Return the issue code defined by an org property YTR_ISSUE_CODE or nil"
  (if (derived-mode-p 'org-mode)
      (let ((issue-code (org-entry-get (point) ytr-org-issue-code-property-name t)))
        (when issue-code (ytr-parse-issue-node-code issue-code)))
    nil))

(defun ytr-issue-code-from-branch ()
  "Return the issue code from the name of the current git branch"
  (let ((branch-name (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (if (string-match (format "^\\([a-zA-Z]+/\\)?\\(%s\\)[-_].*$" ytr-issue-code-pattern) branch-name)
        (match-string 2 branch-name))))

(defun ytr-issue-node-code-from-line ()
  "Return the first issue code in current line"
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if (string-match (ytr-surrounded-pattern ytr-issue-node-code-pattern) line)
        (match-string 0 line)
      nil)))

(defun ytr-issue-node-cons-from-line ()
  (let ((code (ytr-issue-node-code-from-line)))
    (when code (ytr-parse-issue-node-code code))))

(defun ytr-guess-issue-code ()
  "Return a issue code from current context."
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
  "Guess issue code on context or start a query."
  (let ((guess (ytr-guess-issue-code)))
    (if guess guess (ytr-read-issue-code)))
  )

(defun ytr-guess-or-read-issue-node-cons ()
  "Guess issue code with node code on context or start a query."
  (let ((guess (ytr-guess-issue-node-cons)))
    (if guess guess (cons (ytr-read-issue-code) nil))))

;;;; embark
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
  "Find issue code and node code for embark"
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

;;;; history
(defun ytr-retrieve-history-issues-alist ()
  (let ((result))
    (dolist (elt (reverse ytr-issue-history) result)
      (setq result (cons (ytr-retrieve-issue-alist elt) result))))
  )

(defun ytr-add-issue-to-history (issue-node-code)
  (delete issue-node-code ytr-issue-history)
  (push issue-node-code ytr-issue-history)
  )

;;;; api
(defun ytr-request (method url &optional body)
  "Generic request method."
  (let ((request-timeout 20)
        (ytr-request-response)
        (counter 0)
        (ytr-request-failure))
    (with-local-quit
      (request url
        :type method
        :headers `(("Authorization" . ,(concat "Bearer "
                                               (if (functionp ytr-access-token)
                                                   (funcall ytr-access-token)
                                                 (format "%s" ytr-access-token))))
                   ("Accept" . "application/json")
                   ("Content-Type" . "application/json"))
        :data body
        :parser 'json-read
        :success (cl-function (lambda (&rest args &key response &allow-other-keys)
                                (setq ytr-request-response response)
                                (when ytr-request-debug (message "Done: %s" (request-response-status-code response)))))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (setq ytr-request-response error-thrown)
                              (setq ytr-request-failure t)
                              (message "Got error: %S" error-thrown)))))
    (while (not (or ytr-request-failure ytr-request-response (> counter (+ request-timeout 1))))
      (setq counter (+ 1 counter))
      (sleep-for 0 1000))
    (cond (ytr-request-failure (user-error "Request failed"))
          (ytr-request-response (let ((response-status (request-response-status-code ytr-request-response))
                                      (response-data (request-response-data ytr-request-response)))
                                  (if (or (not response-status) (< response-status 200) (> response-status 299))
                                      (user-error "Request failed with status %s and message %s" response-status ytr-request-response)
                                    response-data)))
          (t (user-error "Request did not return")))))

(defun ytr-request-upload (url &optional paths)
  "Generic request method for uploading a list of files."
  (let* ((request-timeout 10)
         (response (request url
                     :type "POST"
                     :headers `(("Authorization" . ,(concat "Bearer "
                                                            (if (functionp ytr-access-token)
                                                                (funcall ytr-access-token)
                                                              (format "%s" ytr-access-token))))
                                ("Accept" . "application/json")
                                ("Content-Type" . "multipart/form-data"))
                     :files (mapcar (lambda (path) (cons (file-name-nondirectory path) path)) paths)
                     :sync t
                     :parser 'json-read
                     :complete (cl-function (lambda (&key response &allow-other-keys)
                                              (when ytr-request-debug (message "Done: %s" (request-response-status-code response)))))
                     ))
         (response-status (request-response-status-code response))
         (response-data (request-response-data response)))
    (if (or (< response-status 200) (> response-status 299))
        (user-error "Request failed with status %s and message %s" response-status response)
      response-data)))

(defun ytr-retrieve-query-issues-alist (query)
  "Retrieve list of issues by query"
  (ytr-request "GET" (concat ytr-baseurl "/api/issues?fields=idReadable,summary,description,created,updated,resolved,reporter(login,fullName),customFields(name,value(name)),comments&query=" (url-hexify-string query))))

(defun ytr-retrieve-saved-queries-alist ()
  "Retrieve list of saved queries"
  (ytr-request "GET" (concat ytr-baseurl "/api/savedQueries?fields=name,query,owner(fullName),issues(resolved,customFields(name,value(name)))")))

(defun ytr-retrieve-issue-alist (issue-code)
  "Retrieve information concering the given issue and return an alist."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues/" issue-code "?fields=id,idReadable,summary,description,comments(id,text,created,updated,author(login,fullName),attachments(name,url,size,mimeType)),created,updated,resolved,reporter(login,fullName),links(direction,linkType(name,sourceToTarget,targetToSource),issues(idReadable,summary)),customFields(name,value(name)),attachments(name,url,size,mimeType,comment(id))")))

(defun ytr-retrieve-issue-comment-alist (issue-node-cons)
  "Retrieve information concering the given issue and return an alist."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues/" (car issue-node-cons) "/comments/" (cdr issue-node-cons) "?fields=id,text,created,updated,author(login,fullName),attachments(name,url,size,mimeType)")))

(defun ytr-send-new-comment-alist (issue-code alist)
  "Send the information in ALIST as a new comment for ticket with id ISSUE-CODE"
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue-code "/comments/") (json-encode alist)))

(defun ytr-send-issue-comment-alist (issue-node-cons alist)
  "Send information in ALIST for a remote update of an issue comment with id ISSUE"
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" (car issue-node-cons) "/comments/" (cdr issue-node-cons) "?fields=text") (json-encode alist)))

(defun ytr-send-issue-alist (issue alist)
  "Send the information in ALIST for a remote update of issue with id ISSUE"
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue "?fields=description") (json-encode alist)))

(defun ytr-send-as-attachments (paths issue-code &optional node-code)
  "Attach the file to the ticket with id ISSUE-CODE"
  (ytr-request-upload (concat ytr-baseurl "/api/issues/" issue-code (if node-code (format "/comments/%s" node-code) "") "/attachments?fields=id,name") paths))


(defun ytr-get-customField-value (issue-alist field-name)
  (let* ((field-alist (cl-find-if (lambda (alist) (equal (cdr (assoc 'name alist)) field-name)) (alist-get 'customFields issue-alist)))
         (value-alist (assoc 'value field-alist))
         (value-name (cdr (assoc 'name (cdr value-alist)))))
    (if value-name value-name "-")))

(defun ytr-get-customField-list-value (issue-alist field-name)
  (let* ((field-alist (cl-find-if (lambda (alist) (equal (cdr (assoc 'name alist)) field-name)) (alist-get 'customFields issue-alist)))
         (values-alist (assoc 'value field-alist))
         (value-name (mapconcat (lambda (value-alist)
                                  (cdr (assoc 'name value-alist)))
                                (cdr values-alist) ", ")))
    (if (< 0 (length value-name)) value-name "-")))

;;;; org mode conversion
(defconst ytr-org-issue-code-property-name "YTR_ISSUE_CODE" "Name of the property to store the org issue code")
(defconst ytr-org-node-type-property-name "YTR_NODE_TYPE" "Name of the property to store the node type")
(defconst ytr-org-local-content-hash-property-name "YTR_LOCAL_CONTENT_HASH" "Name of the property to store the local content hash")
(defconst ytr-org-remote-content-hash-property-name "YTR_REMOTE_CONTENT_HASH" "Name of the property to store the remote content hash")
(defconst ytr-org-author-property-name "YTR_AUTHOR" "Name of the property to store the remote content hash")
(defconst ytr-org-created-at-property-name "YTR_CREATED_AT" "Name of the property to store the remote content hash")
(defconst ytr-org-updated-at-property-name "YTR_UPDATED_AT" "Name of the property to store the remote content hash")

(defun ytr-align-all-org-tables-in-buffer ()
  "Align all org tables in the current buffer, calling `org-table-align` once per table.
Preserves point."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-table-dataline-regexp nil t)
      (org-table-align)
      ;; Move point to the end of the current table to avoid realigning the same table
      (goto-char (org-table-end)))))

(defcustom ytr-save-import-diff-inline nil "Control wether an inline code block is written to each imported node." :type 'boolean :group 'ytr)
(defcustom ytr-save-import-diff-inline-when-empty nil "Control wether an inline code block is written even if the diff is empty." :type 'boolean :group 'ytr)

(defcustom ytr-import-diff-switches "--ignore-space-change" "Diff Switches used to create the import diff." :type 'string :group 'ytr)

(defun ytr-trim-blank-lines-leading-and-trailing (content)
  "Return a string where all blank lines at the beginning and the end of CONTENT are trimmed."
  (replace-regexp-in-string "\n*[ \t\r\n]*\\'" "" (replace-regexp-in-string "\\`[ \t\r\n]*\n*" "" content)))

(defun ytr-md-to-org (input level &optional diff-file)
  "Convert a markdown string to org mode using pandoc.

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
          (replace-string "☒" "[X]" t (point-min) (point-max))
          (replace-string "☐" "[ ]" t (point-min) (point-max)))
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
        (let ((org-export-show-temporary-export-buffer nil))
          (org-export-to-buffer 'gfm org-export-gfm-buffer))
        (with-current-buffer org-export-gfm-buffer (ytr-perform-markdown-replacements ""))
        (when (or ytr-save-import-diff-inline diff-file)
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

(defun ytr-insert-issue-alist-as-org (issue-alist level)
  "Insert the issue given by ISSUE-ALIST as org at point"
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
    (ytr-org-insert-node .description (+ 1 level) 'description (cons .idReadable nil) (alist-get 'fullName .reporter) .created .updated .attachments)
    ;; do the comments
    (let ((issue-code .idReadable))
      (mapc (lambda (comment-alist)
              (let-alist comment-alist
                (ytr-org-insert-node .text (+ 1 level) 'comment (cons issue-code .id) (alist-get 'fullName .author) .created .updated .attachments)))
            .comments))
    ;; postprocess
    (org-unindent-buffer)))

(defun ytr-issue-alist-to-org-buffer (issue-alist)
  "Convert an alist of markdown code into an org buffer with proper headings"
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

(defun ytr-max-heading-level ()
  "Determine the highest Heading in the buffer. Return nil if no heading found."
  (save-excursion
    (goto-char (point-min))
    (let ((lowest-level 6)) ;; Set initial lowest level to the maximum heading level (e.g., 6 for `org-mode`)
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        (setq lowest-level (min lowest-level (length (match-string 1)))))
      lowest-level)))

(defun ytr-first-heading-level ()
  "Determine the level of the first heading in buffer"
  (org-fold-show-all)
  (goto-char (point-min))
  (when (not (org-at-heading-p))
    (org-next-visible-heading 1)
    )
  (org-current-level))

(defun ytr-demote-org-headings (level)
  "Demote all headings in the current buffer so the new max level it LEVEL."
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

(defun ytr-org-insert-node (content level type issue-node-cons author created updated attachments)
  "Insert a node at point.

Level is that of the node, type is generic, author is a string, created is a
long value"
  (let* ((start (point))
         (type-string (format "%s" type))
         (remote-content (or content ""))
         (local-content (ytr-md-to-org remote-content (+ 1 level))))
    (open-line 1)  ;; need this to ensure props go to correct heading
    (insert (format "%s %s by %s\n\n"
                    (make-string level ?*)
                    (format-time-string "%Y-%m-%d" (/ created 1000))
                    author))
    (save-excursion
      (goto-char start)
      (org-set-tags (list (capitalize type-string))))
    (org-set-property ytr-org-remote-content-hash-property-name (sha1 remote-content))
    (org-set-property ytr-org-local-content-hash-property-name (sha1 (ytr-trim-blank-lines-leading-and-trailing local-content)))
    (org-set-property ytr-org-issue-code-property-name (ytr-issue-node-code-action issue-node-cons))
    (org-set-property ytr-org-node-type-property-name type-string)
    (org-set-property ytr-org-created-at-property-name (format-time-string "%Y-%m-%d %H:%M" (/ created 1000)))
    (when updated (org-set-property ytr-org-updated-at-property-name (format-time-string "%Y-%m-%d %H:%M" (/ updated 1000))))
    (org-set-property ytr-org-author-property-name author)
    (kill-whole-line)  ;; kill line we just opened
    (insert local-content)
    (when (/= (length attachments) 0)
      (save-excursion
        (goto-char start)
        (org-set-tags (append (org-get-tags) '("YTR_ATTACH")))
        ))
    (mapcar (lambda (attachment-alist)
              (let-alist attachment-alist
                (replace-string-in-region (format "[[file:%s]]" .name)
                                          (format "[[%s%s&forceDownload=true&ytr_name=%s][%s]]" ytr-baseurl .url .name .name)
                                          (point-min) (point-max))
                (replace-regexp-in-region (format "\\[\\[file:%s]\\[\\(.*\\)]]" .name)
                                          (format "[[%s%s&forceDownload=true&ytr_name=%s][\\1]]" ytr-baseurl .url .name)
                                          (point-min) (point-max))))
            attachments)))

(defun ytr-find-node (&optional type-wanted)
  "Find the parent heading with a YTR_NODE_TYPE property.

Sets the point and returns the type. If property is not found in all higher headings returns
nil and restore point. If TYPE-wanted is not nil search for that node type."
  (let ((saved-point (point)))
    (when (or (org-at-heading-p) (org-back-to-heading))
      (let ((type-found (org-entry-get (point) ytr-org-node-type-property-name)))
        (if (and type-found (or (not type-wanted)
                                (eq type-wanted (intern type-found))))
            (intern type-found)
          (or (and (org-up-heading-safe) (ytr-find-node type-wanted))
              (and (goto-char saved-point) nil)))))))

;;;; org interactive
(defvar-keymap ytr-commit-new-comment-mode-map "C-c C-c" #'ytr-commit-new-comment "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-new-comment-mode markdown-mode "ytr-commit-new-comment-mode" "Mode for editing markdown exports from org before sending them to youtrack")

(defvar-keymap ytr-commit-update-node-mode-map "C-c C-c" #'ytr-commit-update-node "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-update-node-mode markdown-mode "ytr-commit-update-node-mode" "Mode for editing markdown exports from org before sending them to youtrack")

(defun ytr-cancel-commit ()
  "Cancel committing something to youtrack"
  (interactive)
  (let ((buffer (buffer-name)))
    (set-window-configuration ytr-buffer-wconf)
    (kill-buffer buffer)))

(defun ytr-commit-new-comment ()
  "Commit buffer content as a new comment"
  (interactive)
  (let ((new-node-code (alist-get 'id (ytr-send-new-comment-alist ytr-buffer-issue-code `((text . ,(buffer-string))))))
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
    (when position ;; use position as flag for source buffer
      (goto-char position)
      (set-mark (+ position (length text)))
      (ytr-add-issue-to-history issue-code)
      (unless new-node-code (user-error "No node code retrieved"))
      (if (derived-mode-p 'org-mode)
          (ytr-send-attachments-action (cons issue-code new-node-code)))
      (cl-case ytr-new-comment-behavior
        (keep (deactivate-mark))
        (keep-content
         (let-alist (ytr-retrieve-issue-comment-alist (cons issue-code new-node-code))
           (deactivate-mark)
           (when (looking-at-p "\*+ ")
             (insert "*")
             (forward-char -1))
           (ytr-org-insert-node nil curlevel 'comment (cons issue-code new-node-code) (alist-get 'fullName .author) .created .updated .attachments)
           (goto-char position)
           (org-set-property ytr-org-remote-content-hash-property-name (if .text (sha1 .text) ""))
           (org-set-property ytr-org-local-content-hash-property-name local-content-hash)))
        (kill (kill-region (point) (mark)))
        (fetch
         (let-alist (ytr-retrieve-issue-comment-alist (cons issue-code new-node-code))
           (kill-region (point) (mark))
           (ytr-org-insert-node .text curlevel 'comment (cons issue-code new-node-code) (alist-get 'fullName .author) .created .updated .attachments)
           (goto-char position))))
      (ytr-issue-node-code-buttonize-buffer))))

(defun ytr-commit-update-node ()
  "Commit the buffer to youtrack to update a node"
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
        (kill (org-cut-subtree))
        (fetch
         (ytr-fetch-remote-node)))
      (ytr-issue-node-code-buttonize-buffer))))

(defun ytr-perform-markdown-replacements (attach-dir)
  (replace-regexp-in-region "^#" (make-string ytr-export-base-heading-level ?#) (point-min) (point-max))
  (replace-regexp-in-region (format "\\[\\(.*\\)\\](%s.*&ytr_name=\\(.*\\(?:png\\|jpeg\\|jpg\\)\\))" ytr-baseurl) "![](\\1)" (point-min) (point-max))
  (replace-regexp-in-region (format "\\[\\(.*\\)\\](%s.*&ytr_name=\\(.*\\))" ytr-baseurl) "[\\1](\\2)" (point-min) (point-max))
  (replace-regexp-in-region (format "\\([^[#a-zA-Z0-9-]\\|^\\)%s\\([^]#a-zA-Z0-9-]\\|$\\)" ytr-issue-mandatory-node-code-pattern)
                            (format "\\1[\\2#\\3](%s/issue/\\2#focus=Comments-\\3.0-0)\\4" ytr-baseurl) (point-min) (point-max))
  (replace-regexp-in-region (format "](file://%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
  (replace-regexp-in-region (format "](%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
  (replace-regexp-in-region ":.*:$" "" (point-min) (point-max))
  (replace-string-in-region "\n\n\n" "\n\n" (point-min) (point-max))
  (whitespace-cleanup)
  )

(defun ytr-new-comment-editable ()
  "Send the current subtree or region as comment to a ticket"
  (interactive)
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
                ytr-buffer-local-content-hash (sha1 (ytr-trim-blank-lines-leading-and-trailing text)))))

(defun ytr-quick-comment-action (issue-node-cons)
  "Open a markdown buffer to write a quick comment."
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
  "Open a markdown buffer to write a quick comment."
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
  "Use the current subtree to create a new issue"
  (interactive)
  (org-back-to-heading)
  (let* ((heading-start (point))
         (heading-end (progn
                        (forward-line)
                        (while (looking-at-p "\\s-*\\(:.*\\)?$") (forward-line))
                        (point)))
         (heading (buffer-substring heading-start heading-end))
         (title (org-get-heading t t t t)))
    (org-cut-subtree)
    (with-temp-buffer
      (org-mode)
      (insert (pop kill-ring))
      (goto-char (point-min))
      (kill-whole-line)
      (save-window-excursion
        (org-gfm-export-as-markdown nil nil)
        (replace-regexp "^#" "##" nil (point-min) (point-max))
        (whitespace-cleanup)
        (ytr-browse-new-issue title (buffer-string))
        ))
    (insert heading)
    (insert "/(Issue created from deleted content)/\n\n")))

(defun ytr-update-remote-node-editable ()
  "Update a node on remote side after editing locally"
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
                 (org-mark-subtree)
                 (buffer-substring-no-properties (region-beginning) (region-end)))))
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
  "Update a local node at point withs its remote content. If NODE-ALIST ist not given, it will be retrieved."
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
                 (not (ytr-node-remotely-edited-p (alist-get (cl-case node-type
                                                               (comment 'text)
                                                               (description 'description)
                                                               (t (user-error "Bad node type %s" node-type)))
                                                             node-alist))))
            (message "Node %s already up-to-date" (ytr-issue-node-code-action issue-node-cons))
          (let ((inhibit-read-only t)
                ;; (inhibit-message t)
                )
            (org-cut-subtree)
            (cl-case node-type
              (description (let-alist node-alist
                             (ytr-org-insert-node .description curlevel 'description (cons issue-code node-code) (alist-get 'fullName .reporter) .created .updated .attachments)))
              (comment (let-alist node-alist
                         (ytr-org-insert-node .text curlevel 'comment (cons issue-code node-code) (alist-get 'fullName .author) .created .updated .attachments)))
              (issue (ytr-insert-issue-alist-as-org node-alist curlevel))
              (t (user-error "Bad node type %s" node-type)))))))))

(defun ytr-org-action (issue-node-cons)
  ""
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
  "Set Property YTR_ISSUE_CODE on org heading and append tag YTR"
  (save-excursion
    (org-set-property ytr-org-issue-code-property-name (car issue-node-cons))
    (org-back-to-heading)
    (let ((tags (org-get-tags)))
      (if (member "YTR" tags) nil (org-set-tags (append (list "YTR") tags))))))

(defun ytr-org-query (query)
  "Convert all issues given by query to org and collect them in a buffer."
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
  "Send the attachments at org heading to issue or node and remove them locally."
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

(defcustom ytr-org-file "~/ytr.org" "File path used for persisting downloaded issues." :type 'file)

(defun ytr-find-org-node-action (issue-node-cons)
  "Find the first node with given ids in ytr org file"
  (with-current-buffer (find-file-noselect ytr-org-file)
    ;; do something with the buffer
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

(defun ytr-org-skip-to-content ()
  "Skip lines forward to a line that is neither the heading nor a property or blank line (beginning with :)."
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
- imidiate subheading - works
- end of buffer - works"

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


(defun ytr-node-locally-edited-p (&optional false_if_no_node)
  "Returns wether the hash of the current subtree differs from the local hash in property."
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
  "Returns wether the hash of REMOTE-CONTENT differs from the remote hash in property. If REMOTE-CONTENT is nil it will be retrieved according to point (only works for comments!)"
  (unless remote-content
    (setq remote-content (alist-get 'text (ytr-retrieve-issue-comment-alist (ytr-guess-issue-node-cons)))))
  (save-mark-and-excursion
    (if (not (member (ytr-find-node) (list 'comment 'description)))
        (when (not false_if_no_node)
          (user-error "No hashable node found"))
      (let* ((saved-remote-hash (org-entry-get (point) ytr-org-remote-content-hash-property-name t))
             (actual-remote-hash (sha1 remote-content)))
        (not (string= saved-remote-hash actual-remote-hash))))))

;;;;; query to org table
(defun ytr-data-to-org-table (data)
  "Convert DATA (a list of lists) to an org mode table string."
  (let* ((header (car data))
         (rows (cdr data))
         ;; Helper function to format a single row
         (format-row (lambda (row)
                       (concat "| " (mapconcat 'identity row " | ") " |")))
         ;; Create header row
         (header-row (funcall format-row header))
         ;; Create separator row (e.g., |---|---|---|)
         (separator-row
          (concat "|" (mapconcat (lambda (_) "---") header "|") "|"))
         ;; Create data rows
         (data-rows (mapconcat format-row rows "\n")))
    ;; Combine all parts
    (concat header-row "\n" separator-row "\n" data-rows "\n")))

(defun ytr-insert-and-align-org-table (data)
  "Insert DATA as an org mode table and align it."
  (interactive)
  (let ((table (ytr-data-to-org-table data)))
    (insert table)
    ;; Move cursor to the beginning of the inserted table
    (forward-line (- 1 (length (split-string table "\n"))))
    ;; Align the table
    (org-table-align)))

(defun ytr-query-to-org-table (query &optional no-query-keyword issue-properties)
  "Execute QUERY and insert it as an org mode table."
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
    ;; Gehe zur ersten keyword Zeile
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
    ;; Parse die keywords
    (while (org-at-keyword-p)
      (when (not (org-match-line org-keyword-regexp))
        (goto-char my-point)
        (user-error "Did not understand keyword line"))
      (push (cons (match-string-no-properties 1) (match-string-no-properties 2)) key-values)
      (forward-line))
    ;; Suche nach einer Query Definition
    (let ((query (alist-get "ytr-query" key-values nil nil #'string=))
          (columns (alist-get "ytr-columns" key-values nil nil #'string=)))
      (when (not query)
        (goto-char my-point)
        (user-error "No ytr-query found"))
      ;; Baue die properties
      (let ((properties (if columns
                            (mapcar (lambda (property-name)
                                      (if (boundp (intern property-name))
                                          (symbol-value (intern property-name))
                                        (goto-char my-point)
                                        (user-error "Did not find property symbol %s" property-name)))
                                    (split-string columns "\\s-+"))
                          ytr-issue-properties)))
        ;; Lösche die Tabelle
        (delete-region (org-table-begin) (org-table-end))
        ;; Neue Tabelle
        (ytr-query-to-org-table query t properties)))))

(defun ytr-update-org-query-table-hook ()
  (condition-case nil
      (progn (ytr-update-org-query-table) t)
    (user-error nil)))

(add-to-list 'org-ctrl-c-ctrl-c-hook #'ytr-update-org-query-table-hook)

(define-error 'ytr-key-error "Key not found" 'error)

(defun ytr-merge-issue-node ()
  "Find the issue node at point, call fetch for all subnodes, that are not locally edited and append missing nodes. Skip subheadings that are no ytr nodes."
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
                      (ytr-org-insert-node .text (+ 1 level) 'comment (cons issue-code .id) (alist-get 'fullName .author) .created .updated .attachments)))
                  (seq-filter (lambda (comment-alist)
                                (not (member (alist-get 'id comment-alist) processed-node-codes)))
                              (alist-get 'comments issue-alist))))))))


;;;; preview
(defface ytr-preview-field-name-face '((t . (:inherit font-lock-variable-name-face))) "Font used for field values in ytr preview window")
(defface ytr-preview-field-value-face '((t . (:inherit font-lock-warning-face :weight bold))) "Font used for field values in ytr preview window")
(defface ytr-preview-issue-code-face '((t . (:inherit org-todo :inverse-video t :weight bold))) "Font used for field values in ytr preview window")
(defface ytr-preview-summary-face '((t . (:inherit org-todo :weight bold))) "Font used for field values in ytr preview window")

(defconst ytr-issue-property-id
  '("ID" . (lambda (issue-alist) (alist-get 'idReadable issue-alist)))
  "Field Definition for Sneak Preview to show the ID")
(defconst ytr-issue-property-summary
  '("Summary" . (lambda (issue-alist) (alist-get 'summary issue-alist)))
  "Field Definition for Sneak Preview to show the summary")
(defconst ytr-issue-property-created
  '("created" . (lambda (issue-alist) (let-alist issue-alist (if .created (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000)) "-"))))
  "Field Definition for Sneak Preview to show the created")
(defconst ytr-issue-property-updated
  '("updated" . (lambda (issue-alist) (let-alist issue-alist (if .updated (format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) "-"))))
  "Field Definition for Sneak Preview to show the updated")
(defconst ytr-issue-property-resolved
  '("resolved" . (lambda (issue-alist) (let-alist issue-alist (if .resolved (format-time-string "%Y-%m-%d %H:%M" (/ .resolved 1000)) "-"))))
  "Field Definition for Sneak Preview to show the Resolved")
(defconst ytr-issue-property-reporter
  '("Reporter" . (lambda (issue-alist) (alist-get 'fullName (alist-get 'reporter issue-alist))))
  "Field Definition for Sneak Preview to show the Reporter")
(defconst ytr-issue-property-type
  '("Type" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Type")))
  "Field Definition for Sneak Preview to show the Priority")
(defconst ytr-issue-property-priority
  '("Priority" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Priority")))
  "Field Definition for Sneak Preview to show the Priority")
(defconst ytr-issue-property-state
  '("State" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "State")))
  "Field Definition for Sneak Preview to show the State")
(defconst ytr-issue-property-assignee
  '("Assignee" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Assignee")))
  "Field Definition for Sneak Preview to show the Assignee")
(defconst ytr-issue-property-comments
  '("Comments" . (lambda (issue-alist) (format "%i" (length (alist-get 'comments issue-alist)))))
  "Field Definition for Sneak Preview to show the Comments")
(defconst ytr-issue-property-activity
  '("Activity" . (lambda (issue-alist)
                   (ytr-activity-string (ytr-get-issue-activity issue-alist))))
  "Field Definition for Sneak Preview to show the recent activity")
(defconst ytr-issue-property-subsystem
  '("Subsystem" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Subsystem")))
  "Field Definition for Sneak Preview to show the Subsystem")
(defconst ytr-issue-property-fixversions
  '("Fix versions" . (lambda (issue-alist) (ytr-get-customField-list-value issue-alist "Fix versionss")))
  "Field Definition for Sneak Preview to show the Subsystem")

(defcustom ytr-issue-properties
  (list ytr-issue-property-fixversions ytr-issue-property-subsystem ytr-issue-property-priority ytr-issue-property-type ytr-issue-property-state ytr-issue-property-reporter ytr-issue-property-assignee ytr-issue-property-comments ytr-issue-property-activity)
  "List of fields to print in sneak window for issues.

Each entry is a cons of a format definition and a function to compute the value,
which receives as argument den issue-alist."
  :type '(repeat (cons string function)) :group 'ytr)

(defun ytr-sneak-window-issue (issue-alist)
  "Display a side window with the description and same basic information on issue."
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
  "Display a side window with the description and basic information on the comment."
  (with-output-to-temp-buffer "*ytr-describe-comment*"
    (let-alist (cl-find-if (lambda (elem)
                             (string= (alist-get 'id elem) node-code))
                           (alist-get 'comments issue-alist))
      ;; comments(id,text,created,author(login,fullName))
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
  "Display a side window with some basic information on issue and comment."
  (let* ((issue-code (car issue-node-cons))
         (node-code (cdr issue-node-cons))
         (issue-alist (ytr-retrieve-issue-alist issue-code)))
    (ytr-sneak-window-issue issue-alist)
    (when node-code (ytr-sneak-window-comment issue-alist node-code))))

;;;; Copy URL

(defun ytr-url (issue-node-cons)
  "Get the url for issue with comment code"
  (let* ((issue-code (car issue-node-cons))
         (node-code (cdr issue-node-cons)))
    (if node-code
        (ytr-issue-comment-url (cons issue-code node-code))
      (ytr-issue-url issue-code))))

(defun ytr-copy-url-action (issue-node-cons)
  ""
  (let ((url (ytr-url issue-node-cons)))
    (message url)
    (kill-new url)))

;;;; Get Issue Node Codes
(defun ytr-issue-node-code-action (issue-node-cons)
  "Return a string representing issue and node code."
  (if (cdr issue-node-cons)
      (format "%s#%s" (car issue-node-cons) (cdr issue-node-cons))
    (car issue-node-cons)))

(defun ytr-copy-issue-node-code-action (issue-node-cons)
  "Put a string for issue and node code on kill ring."
  (kill-new (ytr-issue-node-code-action issue-node-cons)))

(defun ytr-insert-issue-node-code-action (issue-node-cons)
  "Insert a string for issue and node code."
  (insert (ytr-issue-node-code-action issue-node-cons)))

(defun ytr-message-issue-node-code-action (issue-node-cons)
  "Echo issue and node code in message area."
  (message (ytr-issue-node-code-action issue-node-cons)))

;;;; Open in browser

(defun ytr-browse-action (issue-node-cons)
  ""
  (browse-url (ytr-url issue-node-cons)))

(defun ytr-read-refine-browse ()
  "Edit a predefined query to find an issue and open it in the browser"
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

(defun ytr-browse-new-issue (title &optional description)
  "Open web browser to create a new issue"
  (interactive "sSummary: ")
  (browse-url (concat (format "%s/newIssue?summary=%s" ytr-baseurl title)
                      (if description
                          (format "&description=%s" (url-hexify-string description))
                        ""))))

(defun ytr-browse-query (query)
  "Open web browser to execute a query"
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-query-consult))))
  (browse-url (concat ytr-baseurl "/issues?q=" query)))

;;;; Capture
(defcustom ytr-capture-key "" "Key for you special template to capture ytr proxy issues" :type 'string :group 'ytr)

(defun ytr-capture-action (issue-node-cons)
  "Capture a proxy org task that references an issue on ytr"
  (let-alist (ytr-retrieve-issue-alist (car issue-node-cons))
    (setq ytr-capture-summary .summary)
    (setq ytr-capture-issue-code .idReadable)
    (org-capture nil ytr-capture-key)
    (ytr-org-link-heading-action issue-node-cons)))

;;;; actions
(defmacro ytr-define-dart-action (name action)
  `(defun ,(intern (format "ytr-dart-%s" name)) (issue-node-code)
     ,(format "Like ytr-%s but with simple prompt for issue-node-code.\n It may have a node code." name)
     (interactive "sIssue Code (may also have Node Code): ")
     (funcall ,action (ytr-parse-issue-node-code issue-node-code))))

(defmacro ytr-define-embark-action (name action)
  `(defun ,(intern (format "ytr-embark-%s" name)) (cand)
     ,(format "Like ytr-dart-%s but cand consists of issue code and summary" name)
     (funcall ,action (ytr-parse-issue-node-code (car (split-string cand ":"))))))

(defmacro ytr-define-base-action (name action)
  `(defun ,(intern (format "ytr-%s" name)) (issue-code)
     ,(format "Retrieve an issue from user input and call the %s action on it" name)
     (interactive (list
                   (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                     (ytr-read-issue-code))))
     (ytr-add-issue-to-history issue-code)
     (funcall ,action (cons issue-code nil))))

(defmacro ytr-define-smart-action (name action)
  `(defun ,(intern (format "ytr-smart-%s" name)) (issue-node-code)
     ,(format "Guess an issue by context and call the %s action on it" name)
     (interactive (list (ytr-guess-or-read-issue-node-cons)))
     (ytr-add-issue-to-history (car issue-node-code))
     (funcall ,action issue-node-code)))

(defmacro ytr-define-action (name action)
  `(progn
     (ytr-define-dart-action ,name ,action)
     (ytr-define-embark-action ,name ,action)
     (ytr-define-base-action ,name ,action)
     (ytr-define-smart-action ,name ,action)))

(defun ytr-message-action (issue-node-cons) (message "Issue Code%s, Node Code %s" (car issue-node-cons) (cdr issue-node-cons)) )
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

;;;; Issue buttons

(define-button-type 'ytr-issue-node-code-button
  'follow-link t
  'action #'ytr-on-issue-node-code-button)

(defun ytr-on-issue-node-code-button (button)
  (let ((issue-node-cons (ytr-parse-issue-node-code (buffer-substring (button-start button) (button-end button)))))
    (ytr-dart-browse issue-node-cons)))

(defun ytr-issue-node-code-buttonize-buffer ()
  "Turn all issue-node-codes into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (ytr-surrounded-pattern ytr-issue-node-code-pattern) nil t)
      (make-button (match-beginning 2) (match-end 2) :type 'ytr-issue-node-code-button))))

(add-hook 'org-mode-hook 'ytr-issue-node-code-buttonize-buffer)

;;;; helm
(if (require 'helm nil t)
    (progn
      (defun ytr-helm (&optional defaultquery)
        "Use Helm to select an issue from a query and open it."
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

;;;; provide
(provide 'ytr)
;;; ytr.el ends here
