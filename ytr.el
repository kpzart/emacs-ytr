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


(defgroup ytr nil "Youtrack integration into emacs")

(defcustom ytr-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server" :type 'string :group 'ytr)

(defcustom ytr-access-token "" "Your access token. Maybe a string or function" :type '(choice function string) :group 'ytr)

(defcustom ytr-user-full-name "" "Your login name, used to identify your issues" :type 'string :group 'ytr)

(defcustom ytr-queries () "Define your Queries here" :type '(repeat string) :group 'ytr)

(defcustom ytr-request-debug nil "Print request debug messages if non nil" :type 'boolean :group 'ytr)

(defcustom ytr-use-saved-queries t "Wether to use saved queries of user defined queries" :type 'boolean :group 'ytr)

(defcustom ytr-make-new-comment-behavior 'link "What should be done with the region from which a comment was created? One of 'kill, 'fetch (buggy), 'link or nil." :type '(choice (const kill) (const fetch) (const link)) :group 'ytr)

(defvar ytr-issue-history '() "History for issues")
(defvar ytr-query-history '() "History for query")

;;;; urls
(defun ytr-issue-url (shortcode)
  "Return the URL for a issue given by shortcode"
  (concat ytr-baseurl "/issue/" shortcode))

(defun ytr-issue-comment-url (shortcode comment-id)
  "Return the URL for a issue given by shortcode with focus on comment with comment-id"
  (concat ytr-baseurl "/issue/" shortcode "#focus=Comments-" comment-id ".0-0"))

(defun ytr-query-url (query)
  "Return the URL for a query"
  (concat ytr-baseurl "/issues?q=" (url-hexify-string query)))

;;;; read shortcode
(defun ytr-read-shortcode-basic ()
  "Return a shortcode from a query"
  (let* ((query (completing-read "Query: " ytr-queries nil nil))
         (issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          issues-alist))
         (choice (completing-read "Issue: " choices)))
    (car (split-string choice ":"))))

;;;;; marginalia

(add-to-list 'marginalia-annotator-registry '(ytr-shortcode ytr-annotate-shortcode builtin none))
(add-to-list 'marginalia-annotator-registry '(ytr-query ytr-annotate-query builtin none))

(defun ytr-completing-read-categorised (prompt choices category)
  "Like completing-read but puts a category on the choices."
  (completing-read prompt (lambda (str pred flag)
                            (pcase flag
                              ('metadata `(metadata (category . ,category)))
                              (_
                               (all-completions str choices pred))))))

(defun ytr-annotate-shortcode (cand)
  "Annotate issue shortcode with some info"
  (let* ((shortcode (car (split-string cand ":")))
         (issue-alist (cl-find-if (lambda (elem)
                                    (string= (alist-get 'idReadable elem) shortcode))
                                  issues-alist))) ;; issues-alist comes from ytr-read-shortcode-annotated via lexical binding!
    (let-alist issue-alist
      (marginalia--fields
       ;; (:left .summary :format " %s" :face 'marginalia-type)
       ((ytr-get-customField-value issue-alist "Priority") :format "Pr: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "State") :format "St: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "Assignee") :format "As: %s" :truncate .2 :face 'marginalia-documentation)
       ((length (alist-get 'comments issue-alist)) :format "Re: %s" :truncate 7 :face 'marginalia-documentation)
       ((format-time-string "%Y-%m-%d %H:%M" (/ .created 1000)) :format "ct: %s" :truncate 20 :face 'marginalia-documentation)
       ((format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) :format "ut: %s" :truncate 20 :face 'marginalia-documentation)
       ))))

(defun ytr-annotate-query (cand)
  "Annotate queries with some info"
  (let* ((query-alist (cl-find-if (lambda (elem) (string= (alist-get 'query elem) cand)) queries-alist))
         (name (alist-get 'name query-alist))
         (total (alist-get 'issues query-alist))
         (unresolved (seq-drop-while (lambda (issue) (alist-get 'resolved issue)) total))
         (mine (seq-take-while (lambda (issue) (string= ytr-user-full-name (ytr-get-customField-value issue "Assignee"))) unresolved))
         ) ;; queries-alist comes from ytr-read-shortcode-annotated via lexical binding!
    (marginalia--fields
     (name :truncate .5 :face 'marginalia-documentation)
     ((length total) :format "Total: %s" :truncate .2 :face 'marginalia-type)
     ((length unresolved) :format "Open: %s" :truncate .2 :face 'marginalia-type)
     ((length mine) :format "Mine: %s" :truncate .2 :face 'marginalia-type)
     )))

(defconst ytr-issue-shortcode-pattern "[a-zA-Z]+-[0-9]+")
(defconst ytr-comment-shortcode-pattern "#\\([0-9-]+\\)")
(defconst ytr-issue-comment-shortcode-pattern (format "\\(%s\\)\\(?:%s\\)?" ytr-issue-shortcode-pattern ytr-comment-shortcode-pattern))
(defconst ytr-issue-mandatory-comment-shortcode-pattern (format "\\(%s\\)%s" ytr-issue-shortcode-pattern ytr-comment-shortcode-pattern))

(defun ytr-delim-pattern (pattern)
  "Add string begin and string end delimiters to pattern"
  (format "^%s$" pattern))

(defun ytr-surrounded-pattern (pattern)
  "Add surroundings to pattern"
  (format "\\([^a-zA-Z0-9-#]\\)\\(%s\\)\\([^a-zA-Z0-9-#]\\)" pattern))

(defun ytr-read-shortcode-annotated ()
  "Return a shortcode from a query"
  (interactive)
  (let ((query (completing-read "Query: " ytr-queries nil nil)))
    (if (string-match-p "^[a-zA-Z]+-[0-9]+$" query) query ;; if query is already a shortcode
      (let ((issues-alist (ytr-retrieve-query-issues-alist query)))
        (if (length> issues-alist 0)
            (ytr-completing-read-categorised "Issue: "
                                             (mapcar (lambda (item) (alist-get 'idReadable item)) issues-alist)
                                             'ytr-shortcode)
          (user-error "Query returned empty results."))))))

;;;;; consult
(defun ytr-consult-state-function (action cand)
  ""
  (cl-case action
    (preview (ytr-sneak-window-issue
              (cl-find-if (lambda (elem) (string= (alist-get 'idReadable elem) (car (split-string cand ":")))) issues-alist)))
    (exit (quit-window))))

(defun ytr-read-shortcode-from-query-consult (query)
  "use consult to get the shortcode from a given query"
  (let* ((issues-alist (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          issues-alist)))
    (car (split-string (consult--read choices
                                      :category 'ytr-shortcode
                                      :state 'ytr-consult-state-function
                                      :require-match t
                                      :history 'ytr-issue-history
                                      ;; :add-history (list (ytr-guess-shortcode)) ;; klappt noch nicht
                                      ;; :keymap tobedone
                                      )
                       ":"))))

(defun ytr-read-query-var-consult ()
  "Use consult to get a query from custom var"
  (consult--read ytr-queries
                 :history 'ytr-query-history))

(defcustom ytr-only-own-saved-queries t "Filter out saved queries from others" :type 'boolean :group 'ytr)

(defun ytr-read-query-saved-consult ()
  "Use consult to get a query from saved queries"
  (let* ((queries-alist (ytr-retrieve-saved-queries-alist))
         (queries-alist-filtered (seq-filter (lambda (elem)
                                               (string= (alist-get 'fullName (alist-get 'owner elem))
                                                        ytr-user-full-name))
                                             queries-alist))
         (choices (mapcar (lambda (item) (alist-get 'query item)) queries-alist-filtered)))
    (consult--read choices
                   :category 'ytr-query
                   :history 'ytr-query-history)))

(defun ytr-read-query-consult ()
  "Use consult to read a query"
  (if ytr-use-saved-queries (ytr-read-query-saved-consult) (ytr-read-query-var-consult)))

(defun ytr-read-shortcode-consult ()
  "Use consult to read a shortcode"
  (ytr-read-shortcode-from-query-consult (if ytr-use-saved-queries (ytr-read-query-saved-consult) (ytr-read-query-var-consult))))

;;;;; customvar

(defcustom ytr-read-shortcode-function 'ytr-read-shortcode-basic "Select Input Method to read a shortcode from user" :type 'function :group 'ytr :options '(ytr-read-shortcode-basic ytr-read-shortcode-annotated ytr-read-shortcode-consult))

(defun ytr-read-shortcode ()
  "Wrapper function to read shortcode from user"
  (funcall ytr-read-shortcode-function))

;;;; recognize shortcode
(add-to-list 'ffap-string-at-point-mode-alist '(ytr "0-9a-zA-Z#-" "" ""))

(defun ytr-parse-shortcode-and-node-id (candidate)
  "Parse string for and issue shortcode and a comment id if present"
  (if (string-match (ytr-delim-pattern ytr-issue-comment-shortcode-pattern) candidate)
      (cons (match-string 1 candidate) (match-string 2 candidate))
    nil))

(defun ytr-shortcode-and-comment-id-from-point ()
  "Return a cons with the shortcode and optioinal the item id at point or nil if there is none"
  (ytr-parse-shortcode-and-node-id (ffap-string-at-point 'ytr)))

(defun ytr-shortcode-from-point ()
  "Return the shortcode at point or nil if there is none"
  (car (ytr-shortcode-and-comment-id-from-point)))

(defun ytr-shortcode-and-comment-id-from-org-property ()
  "Return the shortcode defined by an org property YTR_SHORTCODE or nil"
  (let ((shortcode (org-entry-get (point) "YTR_SHORTCODE" t)))
    (when shortcode (ytr-parse-shortcode-and-node-id shortcode))))

(defun ytr-shortcode-from-branch ()
  "Return the shortcode from the name of the current git branch"
  (let ((branch-name (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (if (string-match (format "^\\([a-zA-Z]+/\\)?\\(%s\\)[-_].*$" ytr-issue-shortcode-pattern) branch-name)
        (match-string 2 branch-name))))

(defun ytr-guess-shortcode ()
  "Return a shortcode string from current context."
  (interactive)
  (let ((issue (ytr-shortcode-from-point)))
    (if issue issue
      (let ((issue (ytr-shortcode-and-comment-id-from-org-property)))
        (if issue (car issue)
          (let ((issue (ytr-shortcode-from-branch)))
            (if issue issue nil)))))))

(defun ytr-guess-shortcode-and-comment-id ()
  "Return a cons cell from shortcode and comment id from current context."
  (let ((issue-comment-ids (ytr-shortcode-and-comment-id-from-point)))
    (if issue-comment-ids issue-comment-ids
      (let ((issue (ytr-shortcode-and-comment-id-from-org-property)))
        (if issue issue
          (let ((issue (ytr-shortcode-from-branch)))
            (if issue (cons issue nil) nil)))))))

(defun ytr-guess-or-read-shortcode ()
  "Guess shortcode on context or start a query."
  (let ((guess (ytr-guess-shortcode)))
    (if guess guess (ytr-read-shortcode)))
  )

(defun ytr-guess-or-read-shortcode-and-comment-id ()
  "Guess shortcode on context or start a query."
  (let ((guess (ytr-guess-shortcode-and-comment-id)))
    (if guess guess (cons (ytr-read-shortcode) nil))))

;;;; embark
(defvar-keymap embark-ytr-shortcode-actions
  :doc "Keymap for actions for ytr shortcodes"
  :parent embark-general-map
  "w" #'ytr-embark-browse
  "o" #'ytr-embark-org
  "p" #'ytr-embark-sneak
  "u" #'ytr-embark-copy-url
  "y" #'ytr-embark-copy-shortcode
  "i" #'ytr-embark-insert-shortcode
  "m" #'ytr-embark-message-shortcode
  "l" #'ytr-embark-org-link-heading
  "c" #'ytr-embark-org-capture)

(add-to-list 'embark-keymap-alist '(ytr-shortcode . embark-ytr-shortcode-actions))

(defvar-keymap embark-ytr-query-actions
  :doc "Keymap for actions for ytr queries"
  :parent embark-general-map
  "w" #'ytr-browse-query)

(add-to-list 'embark-keymap-alist '(ytr-query . embark-ytr-query-actions))

(defun ytr-embark-shortcode-target-finder ()
  "Find Shortcodes for embark"
  (save-excursion
    (let* ((start (progn (skip-chars-backward "[:alnum:]-#") (point)))
           (end (progn (skip-chars-forward "[:alnum:]-#") (point)))
           (str (buffer-substring-no-properties start end)))
      (save-match-data
        (when (string-match-p (ytr-delim-pattern ytr-issue-comment-shortcode-pattern) str)
          `(ytr-shortcode
            ,str
            ,start . ,end))))))

(add-to-list 'embark-target-finders 'ytr-embark-shortcode-target-finder)

;;;; history
(defun ytr-retrieve-history-issues-alist ()
  (let ((result))
    (dolist (elt (reverse ytr-issue-history) result)
      (setq result (cons (ytr-retrieve-issue-alist elt) result))))
  )

(defun ytr-add-issue-to-history (shortcode)
  (delete shortcode ytr-issue-history)
  (push shortcode ytr-issue-history)
  )

;;;; api
(defun ytr-request (method url &optional body)
  "Generic request method."
  (let* ((request-timeout 10)
         (ytr-request-response))
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
        :sync t
        :parser 'json-read
        :complete (cl-function (lambda (&key response &allow-other-keys)
                                 (when ytr-request-debug (message "Done: %s" (request-response-status-code response)))
                                 (setq ytr-request-response response)))
        ))
    (cl-loop until ytr-request-response
             do (sleep-for 0 100))
    (setq response-status (request-response-status-code ytr-request-response))
    (setq response-data (request-response-data ytr-request-response))
    (if (or (not response-status) (< response-status 200) (> response-status 299))
        (user-error "Request failed with status %s and message %s" response-status ytr-request-response)
      response-data)))

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

(defun ytr-retrieve-issue-alist (issue-id)
  "Retrieve information concering the given issue and return an alist."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues/" issue-id "?fields=id,idReadable,summary,description,comments(id,text,created,updated,author(login,fullName),attachments(name,url,size,mimeType)),created,updated,resolved,reporter(login,fullName),links(direction,linkType(name,sourceToTarget,targetToSource),issues(idReadable,summary)),customFields(name,value(name)),attachments(name,url,size,mimeType,comment(id))")))

(defun ytr-retrieve-issue-comment-alist (issue-id comment-id)
  "Retrieve information concering the given issue and return an alist."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=id,text,created,updated,author(login,fullName),attachments(name,url,size,mimeType)")))

(defun ytr-send-new-comment-alist (issue-id alist)
  "Send the information in ALIST as a new comment for ticket with id ISSUE-ID"
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue-id "/comments/") (json-encode alist)))

(defun ytr-send-issue-comment-alist (issue-id comment-id alist)
  "Send the information in ALIST for a remote update of an issue comment with id ISSUE"
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=text") (json-encode alist)))

(defun ytr-send-issue-alist (issue alist)
  "Send the information in ALIST for a remote update of issue with id ISSUE"
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue "?fields=description") (json-encode alist)))

(defun ytr-send-as-attachments (paths issue-id &optional comment-id)
  "Attach the file to the ticket with id ISSUE-ID"
  (ytr-request-upload (concat ytr-baseurl "/api/issues/" issue-id (if comment-id (format "/comments/%s" comment-id) "") "/attachments?fields=id,name") paths))


(defun ytr-get-customField-value (issue-alist field-name)
  (let* ((field-alist (cl-find-if (lambda (alist) (equal (cdr (assoc 'name alist)) field-name)) (alist-get 'customFields issue-alist)))
         (value-alist (assoc 'value field-alist))
         (value-name (cdr (assoc 'name value-alist))))
    (if value-name value-name "-")))

;;;; org mode conversion
(defun ytr-md-to-org (input level)
  "Convert a markdown string to org mode using pandoc. LEVEL indicates the level of top level headings in org and defaults to 3."
  (save-current-buffer
    (set-buffer (get-buffer-create "*ytr-convert*"))
    (erase-buffer)
    (insert input)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc --wrap=preserve -f gfm -t org")
                             nil t "*ytr-convert-error*")
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
    (buffer-string)))

(defun ytr-insert-issue-alist-as-org (issue-alist level)
  "Insert the issue given by ISSUE-ALIST as org at point"
  (let-alist issue-alist
    (insert (format "%s %s: %s\n\n" (make-string level ?*) .idReadable .summary))
    (open-line 1)  ;; need this to ensure props go to correct heading
    (org-set-property "YTR_SHORTCODE" .idReadable)
    (org-set-property "YTR_NODE_TYPE" "issue")
    (kill-whole-line)  ;; kill line we just opened
    (insert (format "%s Links\n\n" (make-string (+ 1 level) ?*)))
    (mapcar (lambda (link-alist)
              (let-alist link-alist
                (unless (equal (length .issues) 0)
                  (insert (format "%s %s\n\n"
                                  (make-string (+ 2 level) ?*)
                                  (cond ((string= .direction "BOTH") (alist-get 'sourceToTarget .linkType))
                                        ((string= .direction "INWARD") (alist-get 'targetToSource .linkType))
                                        ((string= .direction "OUTWARD") (alist-get 'sourceToTarget .linkType))
                                        (t (message "Unknown link direction: %s" .direction)))))
                  (mapcar (lambda (issue-alist)
                            (let-alist issue-alist
                              (insert (format " - *%s*: %s\n" .idReadable .summary))))
                          .issues)
                  (insert "\n"))))
            .links)
    (unless (eq .attachments '[])
      (insert (format "%s Attachments\n\n" (make-string (+ 1 level) ?*)))
      (mapcar (lambda (attachment-alist)
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
    (ytr-org-insert-node .description (+ 1 level) 'description .idReadable .id (alist-get 'fullName .reporter) .created .attachments)
    ;; do the comments
    (let ((shortcode .idReadable))
      (mapcar (lambda (comment-alist)
                (let-alist comment-alist
                  (ytr-org-insert-node .text (+ 1 level) 'comment shortcode .id (alist-get 'fullName .author) .created .attachments)))
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
        (org-set-property "YTR_SHORTCODE" (format "%s" .idReadable))
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
             (new-level (min (+ heading-level level) 6))
             (new-heading (concat (make-string new-level ?*) " ")))
        (replace-match new-heading)))))

(defun ytr-capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun ytr-org-insert-node (content level type issue-id node-id author created attachments)
  "Insert a node at point, level is that of the node, type is generic, author is a string, created is a long value"
  (let ((start (point)))
    (open-line 1)  ;; need this to ensure props go to correct heading
    (insert (format "%s %s %s by %s\n\n"
                    (make-string level ?*)
                    (format-time-string "%Y-%m-%d %H:%M" (/ created 1000))
                    (ytr-capitalize-first-char (format "%s" type))
                    author))
    (org-set-property "YTR_CONTENT_HASH" (if content (sha1 content) ""))
    (org-set-property "YTR_SHORTCODE" (format "%s#%s" issue-id node-id))
    (org-set-property "YTR_NODE_TYPE" (format "%s" type))
    (kill-whole-line)  ;; kill line we just opened
    (when content (insert (ytr-md-to-org content (+ 1 level))))
    ;; (unless (string= issue-id (org-entry-get (point) "YTR_SHORTCODE" t))  ;; dont see the sense of those lines, keep a while commented
    ;;   (org-set-property "YTR_SHORTCODE" issue-id))
    (when (/= (length attachments) 0)
      (save-excursion
        (goto-char start)
        (org-set-tags '("YTR_ATTACH"))))
    (mapcar (lambda (attachment-alist)
              (let-alist attachment-alist
                (replace-string-in-region (format "[[file:%s]]" .name)
                                          (format "[[%s%s&forceDownload=true&ytr_name=%s][%s]]" ytr-baseurl .url .name .name)
                                          (point-min) (point-max))
                (replace-regexp-in-region (format "\\[\\[file:%s]\\[\\(.*\\)]]" .name)
                                          (format "[[%s%s&forceDownload=true&ytr_name=%s][\\1]]" ytr-baseurl .url .name)
                                          (point-min) (point-max))))
            attachments)))

(defun ytr-find-node ()
  "Find the parent heading with a YTR_NODE_TYPE property, sets the point and returns the type. If property is not found in buffer returns nil."
  (when (or (org-at-heading-p) (org-back-to-heading))
    (let ((type (org-entry-get (point) "YTR_NODE_TYPE")))
      (if type (intern type)
        (when (org-up-heading-safe) (ytr-find-node))))))

;;;; org interactive
(defvar-keymap ytr-commit-new-comment-mode-map "C-c C-c" #'ytr-commit-new-comment "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-new-comment-mode markdown-mode "ytr-commit-new-comment-mode" "Mode for editing markdown exports from org before sending them to youtrack")

(defvar-keymap ytr-commit-update-node-mode-map "C-c C-c" #'ytr-commit-update-node "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-update-node-mode markdown-mode "ytr-commit-update-node-mode" "Mode for editing markdown exports from org before sending them to youtrack")

(defun ytr-cancel-commit ()
  "Cancel committing something to youtrack"
  (interactive)
  (let ((node-type ytr-buffer-node-type))
    (set-window-configuration ytr-buffer-wconf)
    (cl-case node-type
      (issue (undo))))
  (widen))

(defun ytr-remove-all-but-heading ()
  "Remove all line except the first, if its an org heading"
  (goto-char (point-min))
  (when (org-at-heading-p)
    (forward-line)
    (while (and (>= (line-number-at-pos) (line-number-at-pos (window-end)))
                (looking-at-p "\\s-*\\(:.*\\)?$"))
      (forward-line)))
  (kill-region (point) (point-max)))

(defun ytr-commit-new-comment ()
  "Commit buffer content as a new comment"
  (interactive)
  (let ((new-node-id (alist-get 'id (ytr-send-new-comment-alist ytr-buffer-issue-id `((text . ,(buffer-string))))))
        (issue-id ytr-buffer-issue-id) ;; These vars are buffer local and we are going to switch buffer
        (curlevel ytr-buffer-curlevel))
    (set-window-configuration ytr-buffer-wconf)
    (when new-node-id
      (ytr-add-issue-to-history issue-id)
      (message "New comment created on %s with node id %s." issue-id new-node-id)
      (ytr-send-attachments-action (cons issue-id new-node-id))
      (cond ((eq ytr-make-new-comment-behavior 'kill) (kill-region (point-min) (point-max)))
            ((eq ytr-make-new-comment-behavior 'link)
             (ytr-remove-all-but-heading)
             (insert (format "%s#%s\n" issue-id new-node-id)))
            ((eq ytr-make-new-comment-behavior 'fetch)
             (kill-region (point-min) (point-max))
             (ytr-insert-remote-comment issue-id new-node-id curlevel))))
    (widen)
    (ytr-shortcode-buttonize-buffer)))

(defun ytr-commit-update-node ()
  "Commit the buffer to youtrack to update a node"
  (interactive)
  (let ((issue-id ytr-buffer-issue-id)
        (node-id ytr-buffer-node-id)
        (node-type ytr-buffer-node-type))
    (cl-case ytr-buffer-node-type
      (description (ytr-send-issue-alist ytr-buffer-issue-id `((description . ,(buffer-string)))))
      (comment (ytr-send-issue-comment-alist ytr-buffer-issue-id ytr-buffer-node-id `((text . ,(buffer-string)))))
      (t (user-error "Wrong node type %s" ytr-buffer-node-type)))
    (set-window-configuration ytr-buffer-wconf)
    (message "Node successfully updated")
    (ytr-send-attachments-action (cons issue-id (when (eq node-type 'comment) node-id)))
    (ytr-fetch-remote-node)))

(defun ytr-new-comment-editable ()
  "Send the current subtree or region as comment to a ticket"
  (interactive)
  (cond ((region-active-p) (narrow-to-region (mark) (point)))
        (t (org-narrow-to-subtree)))
  (goto-char (point-min))
  (let ((wconf (current-window-configuration))
        (issue-id (ytr-guess-or-read-shortcode))
        (curlevel (ytr-max-heading-level))
        (attach-dir (or (org-attach-dir) "")))
    (org-gfm-export-as-markdown nil nil)
    (replace-regexp-in-region "^#" (make-string ytr-export-base-heading-level ?#) (point-min) (point-max))
    (whitespace-cleanup)
    (replace-regexp-in-region (format "\\([^[#a-zA-Z0-9-]\\|^\\)%s\\([^]#a-zA-Z0-9-]\\|$\\)" ytr-issue-mandatory-comment-shortcode-pattern)
                              (format "\\1[\\2#\\3](%s/issue/\\2#focus=Comments-\\3.0-0)\\4" ytr-baseurl) (point-min) (point-max))

    (replace-regexp-in-region (format "](file://%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
    (replace-regexp-in-region (format "](%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))

    (ytr-commit-new-comment-mode)
    (message "Create new comment on issue %s. C-c to submit, C-k to cancel" issue-id)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-curlevel curlevel
                ytr-buffer-issue-id issue-id
                ytr-buffer-node-type 'comment
                ytr-buffer-commit-type 'create)))

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

(defcustom ytr-export-base-heading-level 2 "Highest Heading Level in exported markdown" :type 'integer :group 'ytr)

(defun ytr-update-remote-node-editable ()
  "Update a node on remote side after editing locally"
  (interactive)
  (let* ((type (or (ytr-find-node) (user-error "Could not find a node to update")))
         (issue-node-ids (ytr-shortcode-and-comment-id-from-org-property))
         (issue-id (car issue-node-ids))
         (node-id (cdr issue-node-ids))
         (content (cl-case type
                    (description (alist-get 'description (ytr-retrieve-issue-alist issue-id)))
                    (comment (alist-get 'text (ytr-retrieve-issue-comment-alist issue-id node-id)))
                    (t (user-error (format "Unknown node type: %s" type)))))
         (remote-hash (if content (sha1 content) ""))
         (local-hash (org-entry-get (point) "YTR_CONTENT_HASH" t))
         (wconf (current-window-configuration))
         (attach-dir (or (org-attach-dir) "")))
    (ytr-add-issue-to-history issue-id)
    (when (not (string= local-hash remote-hash))
      (user-error "Aborted! Remote Node was edited since last fetch: %s %s" local-hash remote-hash))

    (org-gfm-export-as-markdown nil t)
    (replace-regexp-in-region "^#" (make-string ytr-export-base-heading-level ?#) (point-min) (point-max))
    (replace-regexp-in-region (format "\\[\\(.*\\)\\](%s.*&ytr_name=\\(.*\\(?:png\\|jpeg\\|jpg\\)\\))" ytr-baseurl) "![](\\1)" (point-min) (point-max))
    (replace-regexp-in-region (format "\\[\\(.*\\)\\](%s.*&ytr_name=\\(.*\\))" ytr-baseurl) "[\\1](\\2)" (point-min) (point-max))
    (replace-regexp-in-region (format "\\([^[#a-zA-Z0-9-]\\|^\\)%s\\([^]#a-zA-Z0-9-]\\|$\\)" ytr-issue-mandatory-comment-shortcode-pattern)
                              (format "\\1[\\2#\\3](%s/issue/\\2#focus=Comments-\\3.0-0)\\4" ytr-baseurl) (point-min) (point-max))
    (replace-regexp-in-region (format "](file://%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
    (replace-regexp-in-region (format "](%s/\\(.*\\))" (expand-file-name attach-dir)) "](\\1)" (point-min) (point-max))
    (whitespace-cleanup)
    (ytr-commit-update-node-mode)
    (message "Update %s%s on issue %s" type (if (eq type 'comment) (format " with ID %s" node-id) "") issue-id)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-commit-type 'update
                ytr-buffer-node-type type
                ytr-buffer-issue-id issue-id
                ytr-buffer-node-id node-id)))

(defun ytr-send-node ()
  "Create a new comment or update the node, depending on context."
  (interactive)
  (if (org-entry-get (point) "YTR_NODE_TYPE" t)
      (ytr-update-remote-node-editable)
    (ytr-new-comment-editable)))


(defun ytr-insert-remote-issue-description (issue-id node-id level)
  "Insert a remote node in org format"
  (let-alist (ytr-retrieve-issue-alist issue-id)
    (ytr-org-insert-node .description level 'description issue-id node-id (alist-get 'fullName .reporter) .created .attachments)))

(defun ytr-insert-remote-comment (issue-id node-id level)
  "Insert a remote node in org format"
  (let-alist (ytr-retrieve-issue-comment-alist issue-id node-id)
    (ytr-org-insert-node .text level 'comment issue-id node-id (alist-get 'fullName .author) .created .attachments)))

(defun ytr-insert-remote-issue (issue-id level)
  "Insert a remote issue in org format"
  (ytr-insert-issue-alist-as-org (ytr-retrieve-issue-alist issue-id) level))

(defun ytr-fetch-remote-node ()
  "Update a local node withs its remote content"
  (interactive)
  (let* ((type (or (ytr-find-node) (user-error "Could not find a node to fetch")))
         (issue-node-ids (ytr-shortcode-and-comment-id-from-org-property))
         (issue-id (car issue-node-ids))
         (node-id (cdr issue-node-ids))
         (curlevel (org-current-level)))
    (let ((inhibit-read-only t)
          (inhibit-message t))
      (org-cut-subtree)
      (cl-case type
        (description (ytr-insert-remote-issue-description issue-id node-id curlevel))
        (comment (ytr-insert-remote-comment issue-id node-id curlevel))
        (issue (ytr-insert-remote-issue issue-id curlevel))
        (t (user-error "Bad node type %s" type))))))

(defun ytr-org-action (issue-node-ids)
  ""
  (let* ((shortcode (car issue-node-ids))
         (comment-id (cdr issue-node-ids)))
    (ytr-issue-alist-to-org-buffer (ytr-retrieve-issue-alist shortcode))
    (org-mode)
    (read-only-mode 1)
    (ytr-shortcode-buttonize-buffer)
    (goto-char (point-min))
    (when comment-id
      (search-forward-regexp (format ":YTR_SHORTCODE: *%s#%s" shortcode comment-id) nil t)
      (org-back-to-heading))))

(defun ytr-org-link-heading-action (issue-comment-ids)
  "Set Property YTR_SHORTCODE on org heading and append tag YTR"
  (save-excursion
    (org-set-property "YTR_SHORTCODE" (car issue-comment-ids))
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
    (mapcar (lambda (item)
              (ytr-insert-issue-alist-as-org (ytr-retrieve-issue-alist (alist-get 'idReadable item))))
            issues-alist)
    (switch-to-buffer bufname)))

(defun ytr-send-attachments-action (issue-node-ids)
  "Send the attachments at org heading to issue or node and remove them locally (will ask)."
  (when (and (org-attach-dir) (org-attach-file-list (org-attach-dir)))
    (let* ((paths (mapcar (lambda (filename)
                            (file-name-concat (expand-file-name (org-attach-dir)) filename))
                          (org-attach-file-list (org-attach-dir))))
           (size (apply '+ (mapcar (lambda (path) (nth 7 (file-attributes path))) paths))))
      (if (y-or-n-p (format "Upload %s files with total size %s? " (length paths) (file-size-human-readable size)))
          (progn
            (ytr-send-as-attachments paths (car issue-node-ids) (cdr issue-node-ids))
            (when (y-or-n-p "Delete attachments after uploading? ")
              (org-attach-delete-all t)
              (message "Attachments deleted.")))
        (message "Canceled by user.")))))

;;;; preview
(defconst ytr-sneak-field-created
  '("created: %s" . (lambda (issue-alist) (format-time-string "%Y-%m-%d %H:%M" (/ (alist-get 'created issue-alist) 1000))))
  "Field Definition for Sneak Preview to show the created")
(defconst ytr-sneak-field-updated
  '("updated: %s" . (lambda (issue-alist) (let-alist issue-alist (if .updated (format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) "-"))))
  "Field Definition for Sneak Preview to show the updated")
(defconst ytr-sneak-field-resolved
  '("resolved: %s" . (lambda (issue-alist) (let-alist issue-alist (if .resolved (format-time-string "%Y-%m-%d %H:%M" (/ .resolved 1000)) "-"))))
  "Field Definition for Sneak Preview to show the Resolved")
(defconst ytr-sneak-field-reporter
  '("Reporter: %s" . (lambda (issue-alist) (alist-get 'fullName (alist-get 'reporter issue-alist))))
  "Field Definition for Sneak Preview to show the Reporter")
(defconst ytr-sneak-field-priority
  '("Priority: %s" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Priority")))
  "Field Definition for Sneak Preview to show the Priority")
(defconst ytr-sneak-field-state
  '("State: %s" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "State")))
  "Field Definition for Sneak Preview to show the State")
(defconst ytr-sneak-field-assignee
  '("Assignee: %s" . (lambda (issue-alist) (ytr-get-customField-value issue-alist "Assignee")))
  "Field Definition for Sneak Preview to show the Assignee")
(defconst ytr-sneak-field-comments
  '("Comments: %s" . (lambda (issue-alist) (length (alist-get 'comments issue-alist))))
  "Field Definition for Sneak Preview to show the Comments")

(defcustom ytr-sneak-fields-issue
  (list ytr-sneak-field-created ytr-sneak-field-updated ytr-sneak-field-resolved ytr-sneak-field-reporter ytr-sneak-field-priority ytr-sneak-field-state ytr-sneak-field-assignee ytr-sneak-field-comments)
  "List of fields to print in sneak window for issues. Each entry is a cons of a format definition and a function to compute the value, which receives as argument den issue-alist."
  :type '(repeat (cons string function)) :group 'ytr)

(defun ytr-sneak-window-issue (issue-alist)
  "Display a side window with the description and same basic information on issue."
  (with-output-to-temp-buffer "*ytr-describe-issue*"
    (let-alist issue-alist
      (princ (format "%s: %s\n" .idReadable .summary))
      (princ (string-join (mapcar (lambda (field)
                                    (format (car field) (funcall (cdr field) issue-alist)))
                                  ytr-sneak-fields-issue) ", "))
      (unless (= (length .attachments) 0)
        (princ "\nAttachments:")
        (mapcar (lambda (attachment-alist)
                  (let-alist attachment-alist
                    (princ (format " [\"%s\" %s %s]" .name .mimeType (file-size-human-readable .size)))))
                .attachments))
      (princ "\n------------------------\n")
      (princ .description))))

(defun ytr-sneak-window-comment (issue-alist comment-id)
  "Display a side window with the description and same basic information on the comment."
  (with-output-to-temp-buffer "*ytr-describe-comment*"
    (let-alist (cl-find-if (lambda (elem)
                             (string= (alist-get 'id elem) comment-id))
                           (alist-get 'comments issue-alist))
      ;; comments(id,text,created,author(login,fullName))
      (princ (format "Comment for %s: %s\n" (alist-get 'idReadable issue-alist) (alist-get 'summary issue-alist)))
      (princ (format "Author: %s, created: %s, updated: %s"
                     (alist-get 'fullName .author)
                     (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000))
                     (if .updated (format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) "-")))
      (unless (= (length .attachments) 7)
        (princ "\nAttachments:")
        (mapcar (lambda (attachment-alist)
                  (let-alist attachment-alist
                    (princ (format " [\"%s\" %s %s]" .name .mimeType (file-size-human-readable .size)))))
                .attachments))
      (princ "\n------------------------\n")
      (princ .text))))

(defun ytr-sneak-action (issue-node-ids)
  "Display a side window with the description and same basic information on issue and comment id cons cell."
  (let* ((shortcode (car issue-node-ids))
         (comment-id (cdr issue-node-ids))
         (issue-alist (ytr-retrieve-issue-alist shortcode)))
    (ytr-sneak-window-issue issue-alist)
    (when comment-id (ytr-sneak-window-comment issue-alist comment-id))))

;;;; Copy URL

(defun ytr-url (issue-node-ids)
  "Get the url for issue with comment id"
  (let* ((shortcode (car issue-node-ids))
         (comment-id (cdr issue-node-ids)))
    (if comment-id
        (ytr-issue-comment-url shortcode comment-id)
      (ytr-issue-url shortcode))))

(defun ytr-copy-url-action (issue-node-ids)
  ""
  (let ((url (ytr-url issue-node-ids)))
    (message url)
    (kill-new url)))

;;;; Get Shortcode
(defun ytr-shortcode-action (issue-node-ids)
  "Return a string representing shortcode and comment id."
  (if (cdr issue-node-ids)
      (format "%s#%s" (car issue-node-ids) (cdr issue-node-ids))
    (car issue-node-ids)))

(defun ytr-copy-shortcode-action (issue-node-ids)
  "Put a string for shortcode and comment id on kill ring."
  (kill-new (ytr-shortcode-action issue-node-ids)))

(defun ytr-insert-shortcode-action (issue-node-ids)
  "Insert a string for shortcode and comment id."
  (insert (ytr-shortcode-action issue-node-ids)))

(defun ytr-message-shortcode-action (issue-node-ids)
  "Echo shortcode and comment id in message area."
  (message (ytr-shortcode-action issue-node-ids)))
;;;; Open in browser

(defun ytr-browse-action (issue-node-ids)
  ""
  (browse-url (ytr-url issue-node-ids)))

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

(defun ytr-capture-action (issue-comment-ids)
  "Capture a proxy org task that references an issue on ytr"
  (let-alist (ytr-retrieve-issue-alist (car issue-comment-ids))
    (setq ytr-summary .summary)
    (setq ytr-shortcode .idReadable)
    (org-capture nil ytr-capture-key)
    (ytr-org-link-heading-action issue-comment-ids)))

;;;; actions
(defmacro ytr-define-dart-action (name action)
  `(defun ,(intern (format "ytr-dart-%s" name)) (shortcode-node)
     ,(format "Like ytr-%s but offers a simple prompt for entering the shortcode with no completions. It may have a node id." name)
     (interactive "sShortcode (may also have comment id): ")
     (funcall ,action (ytr-parse-shortcode-and-node-id shortcode-node))))

(defmacro ytr-define-embark-action (name action)
  `(defun ,(intern (format "ytr-embark-%s" name)) (cand)
     ,(format "Like ytr-dart-%s but cand consists of shortcode and summary" name)
     (funcall ,action (ytr-parse-shortcode-and-node-id (car (split-string cand ":"))))))

(defmacro ytr-define-base-action (name action)
  `(defun ,(intern (format "ytr-%s" name)) (shortcode)
     ,(format "Retrieve an issue from user input and call the %s action on it" name)
     (interactive (list
                   (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                     (ytr-read-shortcode))))
     (ytr-add-issue-to-history shortcode)
     (funcall ,action (cons shortcode nil))))

(defmacro ytr-define-smart-action (name action)
  `(defun ,(intern (format "ytr-smart-%s" name)) (issue-comment-ids)
     ,(format "Guess an issue by context and call the %s action on it" name)
     (interactive (list (ytr-guess-or-read-shortcode-and-comment-id)))
     (ytr-add-issue-to-history (car issue-comment-ids))
     (funcall ,action issue-comment-ids)))

(defmacro ytr-define-action (name action)
  `(progn
     (ytr-define-dart-action ,name ,action)
     (ytr-define-embark-action ,name ,action)
     (ytr-define-base-action ,name ,action)
     (ytr-define-smart-action ,name ,action)))

(defun ytr-message-action (ids) (message "Issue %s, Node ID %s" (car ids) (cdr ids)) )
(ytr-define-action "message-shortcode" 'ytr-message-shortcode-action)
(ytr-define-action "copy-shortcode" 'ytr-copy-shortcode-action)
(ytr-define-action "insert-shortcode" 'ytr-insert-shortcode-action)
(ytr-define-action "browse" 'ytr-browse-action)
(ytr-define-action "org" 'ytr-org-action)
(ytr-define-action "sneak" 'ytr-sneak-action)
(ytr-define-action "copy-url" 'ytr-copy-url-action)
(ytr-define-action "org-link-heading" 'ytr-org-link-heading-action)
(ytr-define-action "org-capture" 'ytr-capture-action)
(ytr-define-action "send-attachments" 'ytr-send-attachments-action)

;;;; Issue buttons

(define-button-type 'shortcode-button
  'follow-link t
  'action #'ytr-on-shortcode-button)

(defun ytr-on-shortcode-button (button)
  (let ((issue-comment-ids (ytr-parse-shortcode-and-node-id (buffer-substring (button-start button) (button-end button)))))
    (ytr-dart-browse (car issue-comment-ids) (cdr issue-comment-ids))))

(defun ytr-shortcode-buttonize-buffer ()
  "turn all issue shortcodes into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (ytr-surrounded-pattern ytr-issue-comment-shortcode-pattern) nil t)
      (make-button (match-beginning 2) (match-end 2) :type 'shortcode-button))))

(add-hook 'org-mode-hook 'ytr-shortcode-buttonize-buffer)

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
                                                               (let ((shortcode (alist-get 'idReadable issue-alist)))
                                                                 (ytr-add-issue-to-history shortcode)
                                                                 (browse-url (ytr-issue-url shortcode)))))
                                        ("Open in org buffer" . (lambda (issue-alist)
                                                                  (let ((shortcode (alist-get 'idReadable issue-alist)))
                                                                    (ytr-add-issue-to-history shortcode)
                                                                    (ytr-dart-org shortcode)))))
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
