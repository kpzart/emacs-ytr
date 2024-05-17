;;; ytr.el --- Youtrack integration into emacs

;; Copyright (C) 2010-2023 Martin Puttke

;; Author: Martin Puttke <m.s.p@posteo.de>
;; Created: 07 Mar 2023
;; Keywords: convenience
;; URL: https://github.com/matlantis/emacs-ytr
;; Version: 0.1
;; Package-Requires: (plz)
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
(require 'plz)
(require 'embark)
(require 'consult)
(require 'marginalia)


(defgroup ytr nil "Youtrack integration into emacs")

(defcustom ytr-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server" :type 'string :group 'ytr)

(defcustom ytr-access-token "" "Your access token. Maybe a string or function" :type '(choice function string) :group 'ytr)

(defcustom ytr-user-login "" "Your login name, used to identify your issues" :type 'string :group 'ytr)

(defcustom ytr-queries () "Define your Queries here" :type '(repeat string) :group 'ytr)

(defcustom ytr-plz-debug nil "Print plz debug messages if non nil" :type 'bool :group 'ytr)

(defcustom ytr-use-saved-queries t "Wether to use saved queries of user defined queries" :type 'bool :group 'ytr)

(defcustom ytr-make-new-comment-behavior 'link "What should be done with the region from which a comment was created? One of 'kill, 'fetch (buggy), 'link or nil." :type 'symbol :group 'ytr)

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
  (let ((issue-alist (cl-find-if (lambda (elem)
                                (string= (alist-get 'idReadable elem) (car (split-string cand ":"))))
                              issues-alist))) ;; issues-alist comes from ytr-read-shortcode-annotated via lexical binding!
    (let-alist issue-alist
      (marginalia--fields
       ;; (:left .summary :format " %s" :face 'marginalia-type)
       ((ytr-get-customField-value issue-alist "Priority") :format "P: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "State") :format "S: %s" :truncate .2 :face 'marginalia-documentation)
       ((ytr-get-customField-value issue-alist "Assignee") :format "A: %s" :truncate .2 :face 'marginalia-documentation)
       ((format-time-string "%Y-%m-%d %H:%M" (/ .created 1000)) :format "c: %s" :truncate 20 :face 'marginalia-documentation)
       ((format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000)) :format "u: %s" :truncate 20 :face 'marginalia-documentation)
       ))))

(defun ytr-annotate-query (cand)
  "Annotate queries with some info"
  (let* ((query-alist (cl-find-if (lambda (elem) (string= (alist-get 'query elem) cand)) queries-alist))
         (name (alist-get 'name query-alist))
         (total (alist-get 'issues query-alist))
         (unresolved (seq-drop-while (lambda (issue) (alist-get 'resolved issue)) total))
         (mine (seq-take-while (lambda (issue) (string= ytr-user-login (ytr-get-customField-value issue "Assignee"))) unresolved))
         ) ;; queries-alist comes from ytr-read-shortcode-annotated via lexical binding!
      (marginalia--fields
       (name :truncate .5 :face 'marginalia-documentation)
       ((length total) :format "Total: %s" :truncate .2 :face 'marginalia-type)
       ((length unresolved) :format "Open: %s" :truncate .2 :face 'marginalia-type)
       ((length mine) :format "Mine: %s" :truncate .2 :face 'marginalia-type)
       )))

(defun ytr-read-shortcode-annotated ()
  "Return a shortcode from a query"
  (interactive)
  (let ((query (completing-read "Query: " ytr-queries nil nil)))
    (if (string-match-p "^[A-z]+-[0-9]+$" query) query ;; if query is already a shortcode
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

(defun ytr-read-query-saved-consult ()
  "Use consult to get a query from saved queries"
  (let* ((queries-alist (ytr-retrieve-saved-queries-alist))
         (choices (mapcar (lambda (item) (alist-get 'query item)) queries-alist)))
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
(add-to-list 'ffap-string-at-point-mode-alist '(ytr "0-9A-z-#" "" ""))

(defun ytr-parse-shortcode-and-node-id (candidate)
  "Parse string for and issue shortcode and a comment id if present"
  (if (string-match "^\\([A-z]+-[0-9]+\\)\\(#\\([0-9-]+\\)\\)?$" candidate)
      (cons (match-string 1 candidate) (match-string 3 candidate))
    nil))

(defun ytr-shortcode-and-comment-id-from-point ()
  "Return a cons with the shortcode and optioinal the item id at point or nil if there is none"
  (ytr-parse-shortcode-and-node-id (ffap-string-at-point 'ytr)))

(defun ytr-shortcode-from-point ()
  "Return the shortcode at point or nil if there is none"
  (car (ytr-shortcode-and-comment-id-from-point)))

(defun ytr-shortcode-from-org-property ()
  "Return the shortcode defined by an org property YTR_SHORTCODE or nil"
  (org-entry-get (point) "YTR_SHORTCODE" t))

(defun ytr-comment-id-from-org-property ()
  "Return the comment id defined by an org property YTR_ID or nil. Returns also nil if this is not a comment"
  (if (string= "comment" (org-entry-get (point) "YTR_TYPE" t))
      (org-entry-get (point) "YTR_ID" t)))

(defun ytr-shortcode-from-branch ()
  "Return the shortcode from the name of the current git branch"
  (let ((branch-name (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (if (string-match "^\\([A-z]+/\\)?\\([A-z]+-[0-9]+\\)[-_].*$" branch-name)
        (match-string 2 branch-name))))

(defun ytr-guess-shortcode ()
  "Return a shortcode from current context."
  (interactive)
  (let ((issue (ytr-shortcode-from-point)))
    (if issue issue
      (let ((issue (ytr-shortcode-from-org-property)))
        (if issue issue
          (let ((issue (ytr-shortcode-from-branch)))
            (if issue issue nil)))))))

(defun ytr-guess-shortcode-and-comment-id ()
  "Return a shortcode from current context."
  (let ((issue-comment-ids (ytr-shortcode-and-comment-id-from-point)))
    (if issue-comment-ids issue-comment-ids
      (let ((issue (ytr-shortcode-from-org-property)))
        (if issue (cons issue (ytr-comment-id-from-org-property))
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
  "o" #'ytr-embark-org
  "p" #'ytr-embark-sneak
  "y" #'ytr-embark-copy-url
  "w" #'ytr-embark-browse)

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
        (when (string-match-p "^[A-z]+-[0-9]+\\(#[0-9-]+\\)?$" str)
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
(defun ytr-plz (method request &optional body)
  "Generic plz request method"
  (let* ((response (plz method request
                     :headers `(("Authorization" . ,(concat "Bearer "
                                                            (if (functionp ytr-access-token)
                                                                (funcall ytr-access-token)
                                                              (format "%s" ytr-access-token))))
                                ("Accept" . "application/json")
                                ("Content-Type" . "application/json"))
                     :as #'json-read
                     :body body
                     :connect-timeout 10
                     )))
    (cond (ytr-plz-debug
           (message "YTR Request: %s" request)
           (message "YTR Response: %s" response)))
    response))

(defun ytr-retrieve-query-issues-alist (query)
  "Retrieve list of issues by query"
  (ytr-plz 'get (concat ytr-baseurl "/api/issues?fields=idReadable,summary,description,created,updated,resolved,reporter(login,fullName),customFields(name,value(name))&query=" (url-hexify-string query))))

(defun ytr-retrieve-saved-queries-alist ()
  "Retrieve list of saved queries"
  (ytr-plz 'get (concat ytr-baseurl "/api/savedQueries?fields=name,query,issues(resolved,customFields(name,value(name)))")))

(defun ytr-retrieve-issue-alist (issue-id)
  "Retrieve information concering the given issue and return an alist."
  (ytr-plz 'get (concat ytr-baseurl "/api/issues/" issue-id "?fields=id,idReadable,summary,description,comments(id,text,created,author(login,fullName)),created,updated,resolved,reporter(login,fullName),links(direction,linkType(name,sourceToTarget,targetToSource),issues(idReadable,summary)),customFields(name,value(name))")))

(defun ytr-retrieve-issue-comment-alist (issue-id comment-id)
  "Retrieve information concering the given issue and return an alist."
  (ytr-plz 'get (concat ytr-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=id,text,created,author(login,fullName)")))

(defun ytr-send-new-comment-alist (issue-id alist)
  "Send the information in ALIST as a new comment for ticket with id ISSUE-ID"
  (ytr-plz 'post (concat ytr-baseurl "/api/issues/" issue-id "/comments/") (json-encode alist)))

(defun ytr-send-issue-comment-alist (issue-id comment-id alist)
  "Send the information in ALIST for a remote update of an issue comment with id ISSUE"
  (ytr-plz 'post (concat ytr-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=text") (json-encode alist)))

(defun ytr-send-issue-alist (issue alist)
  "Send the information in ALIST for a remote update of issue with id ISSUE"
  (ytr-plz 'post (concat ytr-baseurl "/api/issues/" issue "?fields=description") (json-encode alist)))

(defun ytr-get-customField-value (issue-alist field-name)
  (let-alist issue-alist
    (cdr (assoc 'name (assoc 'value
                             (cl-find-if (lambda (alist) (equal (cdr (assoc 'name alist)) field-name)) .customFields))))))

;;;; org mode conversion
(defun ytr-md-to-org (input level)
  "Convert a markdown string to org mode using pandoc. LEVEL indicates the level of top level headings in org and defaults to 3."
  (save-current-buffer
    (set-buffer (get-buffer-create "*ytr-convert*"))
    (erase-buffer)
    (insert input)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f gfm -t org")
                             nil t "*ytr-convert-error*")
    (let ((inhibit-message t))
      (replace-string "☒" "[X]" t (point-min) (point-max))
     (replace-string "☐" "[ ]" t (point-min) (point-max)))
    (goto-char (point-min))
    (flush-lines " *:[A-Z_]+:.*$")
    (org-mode)
    (ytr-demote-org-headings (or level 3))
    (org-unindent-buffer)
    (buffer-string)))

(defun ytr-issue-alist-to-org (input shortcode)
  "Convert an alist of markdown code into an org buffer with proper headings"
  (let ((bufname (format "*ytr-org-%s*" (downcase shortcode))))
    (set-buffer (get-buffer-create bufname))
    (erase-buffer)
    (org-mode)
    (let-alist input
      ;; title and description
      (insert (concat "#+Title: " .idReadable ": " .summary "\n\n"))
      (org-set-property "YTR_SHORTCODE" shortcode)
      (org-set-property "YTR_ID" .id)
      (insert (concat "* ".idReadable ": " .summary "\n\n"))
      (insert "** Links\n\n")
      (mapcar (lambda (link-alist)
                (let-alist link-alist
                  (unless (equal (length .issues) 0)
                    (insert (format "*** %s\n\n" (cond ((string= .direction "BOTH") (alist-get 'sourceToTarget .linkType))
                                                       ((string= .direction "INWARD") (alist-get 'targetToSource .linkType))
                                                       ((string= .direction "OUTWARD") (alist-get 'sourceToTarget .linkType))
                                                       (t (message "Unknown link direction: %s" .direction)))))
                    (mapcar (lambda (issue-alist)
                              (let-alist issue-alist
                                (insert (format " - *%s*: %s\n" .idReadable .summary))))
                            .issues)
                    (insert "\n")
                    )
                  ))
              .links)
      ;; do the comments
      (mapcar (lambda (comment-alist)
                (let-alist comment-alist
                  (ytr-org-insert-node .text 2 'comment shortcode .id (alist-get 'fullName .author) .created)))
              .comments)
      ;; do the description
      (ytr-org-insert-node .description 2 'description shortcode .id (alist-get 'fullName .reporter) .created)
      ;; postprocess
      (org-unindent-buffer)
      (switch-to-buffer bufname))))

(defun ytr-max-heading-level ()
  "Determine the highest Heading in the buffer. Return nil if no heading found."
  (save-excursion
    (goto-char (point-min))
    (let ((lowest-level 6)) ;; Set initial lowest level to the maximum heading level (e.g., 6 for `org-mode`)
      (while (re-search-forward "^\\(\\*+\\)" nil t)
        (setq lowest-level (min lowest-level (length (match-string 1)))))
      lowest-level)))

(defun ytr-first-heading-level ()
  "Determine the level of the first heading in buffer"
  (org-fold-show-all)
  (goto-char (point-min))
  (when (not (org-at-heading-p))
    (org-next-visible-heading 1)
    )
  (org-current-level)
  )

(defun ytr-demote-org-headings (level)
  "Demote all headings in the current buffer so the new max level it LEVEL."
  (let ((base-level (ytr-max-heading-level)))
    (when base-level
      (ytr-demote-org-headings-by (- level base-level))))
  )

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

(defun ytr-org-insert-node (content level type issue-id node-id author created)
  "Insert a node at point, level is that of the node, type is generic, author is a string, created is a long value"
  (let ((curpoint (point)))
    (insert (format "%s %s %s by %s\n\n"
                    (make-string level ?*)
                    (format-time-string "%Y-%m-%d %H:%M" (/ created 1000))
                    (ytr-capitalize-first-char (format "%s" type))
                    author))
    (previous-line)
    (org-set-property "YTR_CONTENT_HASH" (if content (sha1 content) ""))
    (org-set-property "YTR_ID" node-id)
    (org-set-property "YTR_TYPE" (format "%s" type))
    (insert "\n")
    (when content (insert (ytr-md-to-org content (+ 1 level))))
    (unless (string= issue-id (org-entry-get (point) "YTR_SHORTCODE" t))
      (org-set-property "YTR_SHORTCODE" issue-id))
    (goto-char curpoint)))

(defun ytr-find-node ()
  "Find the parent heading with a YTR_TYPE property, sets the point and returns the type. If property is not found in buffer returns nil."
  (when (or (org-at-heading-p) (org-back-to-heading))
    (let ((type (org-entry-get (point) "YTR_TYPE")))
      (if (or (string= type "comment") (string= type "description"))
          type
        (if (org-up-heading-safe)
            (ytr-find-node)
          nil))))
  )

;;;; org interactive
(defvar-keymap ytr-commit-new-node-mode-map "C-c C-c" #'ytr-commit-new-node "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-new-node-mode markdown-mode "ytr-commit-new-node-mode" "Mode for editing markdown exports from org before sending them to youtrack")

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
    (when (looking-at-p "\\s-*$") (forward-line))) ; enough for the moment, but this kills the properties
  (kill-region (point) (point-max)))

(defun ytr-commit-new-comment ()
  "Commit buffer content as a new comment"
  (setq new-node-id (alist-get 'id (ytr-send-new-comment-alist ytr-buffer-issue-id `((text . ,(buffer-string))))))
  (ytr-add-issue-to-history ytr-buffer-issue-id)
  (let ((issue-id ytr-buffer-issue-id) ;; These vars are buffer local and we are going to switch buffer
        (curlevel ytr-buffer-curlevel))
    (set-window-configuration ytr-buffer-wconf)
    (cond (new-node-id
           (message "New comment created on %s with node id %s." issue-id new-node-id)
           (cond ((eq ytr-make-new-comment-behavior 'kill) (kill-region (point-min) (point-max)))
                 ((eq ytr-make-new-comment-behavior 'link)
                  (ytr-remove-all-but-heading)
                  (insert (format "%s#%s\n" issue-id new-node-id)))
                 ((eq ytr-make-new-comment-behavior 'fetch)
                  (kill-region (point-min) (point-max))
                  (ytr-get-insert-remote-node issue-id new-node-id 'comment curlevel)
                  ))))))

(defun ytr-commit-new-issue ()
  "Commit buffer content as a new issue"
  (ytr-browse-new-issue ytr-new-issue-title (buffer-string))
  (set-window-configuration ytr-buffer-wconf)
  (undo) ; kill-whole-line
  (ytr-remove-all-but-heading)
  (insert "/(Issue created from deleted content)/\n"))

(defun ytr-commit-new-node ()
  "Commit the buffer to youtrack to create a new node"
  (interactive)
  (cl-case ytr-buffer-node-type
    (comment (ytr-commit-new-comment))
    (issue (ytr-commit-new-issue))
    (t (user-error "Bad node type %s" ytr-buffer-node-type)))
  (widen)
  (ytr-shortcode-buttonize-buffer))

(defun ytr-commit-update-node ()
  "Commit the buffer to youtrack to update a node"
  (interactive)
  (cl-case ytr-buffer-node-type
    (description (ytr-send-issue-alist ytr-buffer-issue-id `((description . ,(buffer-string)))))
    (comment (ytr-send-issue-comment-alist ytr-buffer-issue-id ytr-buffer-node-id `((text . ,(buffer-string)))))
    (t (user-error "Wrong node type %s" ytr-buffer-node-type)))
  (set-window-configuration ytr-buffer-wconf)
  (message "Node successfully updated")
  (ytr-fetch-remote-node))


(defun ytr-new-comment-editable ()
  "Send the current subtree or regio as comment to a ticket"
  (interactive)
  (cond ((region-active-p) (narrow-to-region (mark) (point)))
        (t (org-narrow-to-subtree)))
  (goto-char (point-min))
  (let ((wconf (current-window-configuration))
        (issue-id (ytr-guess-or-read-shortcode))
        (curlevel (ytr-max-heading-level)))
    (org-gfm-export-as-markdown nil nil)
    (replace-regexp "^#" "##" nil (point-min) (point-max))
    (ytr-commit-new-node-mode)
    (message "Create new comment on issue %s. C-c to submit, C-k to cancel" issue-id)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-curlevel curlevel
                ytr-buffer-issue-id issue-id
                ytr-buffer-node-type 'comment
                ytr-buffer-commit-type 'create)))

(defun ytr-new-issue ()
  "Use the current subtree to create a new issue"
  (interactive)
  (org-narrow-to-subtree)
  (goto-char (point-min))
  (setq title (org-get-heading t t t t))
  (kill-whole-line)
  (let ((wconf (current-window-configuration))
        (curlevel (ytr-max-heading-level)))
    (org-gfm-export-as-markdown nil nil)
    (replace-regexp "^#" "##" nil (point-min) (point-max))
    (ytr-commit-new-node-mode)
    (message "Create new issue with title: %s. C-c to submit, C-k to cancel" title)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-curlevel curlevel
                ytr-buffer-node-type 'issue
                ytr-buffer-commit-type 'create
                ytr-new-issue-title title)))

(defun ytr-new-comment ()
  "Send the current subtree as comment to a ticket"
  (interactive)
  (cond ((region-active-p) (narrow-to-region (mark) (point)))
        (t (org-narrow-to-subtree)))
  (goto-char (point-min))
  (let* ((curlevel (ytr-max-heading-level))
         (issue-id (ytr-guess-or-read-shortcode))
         (new-node-id (save-window-excursion
                        (org-gfm-export-as-markdown nil nil)
                        (markdown-mode)
                        (let ((inhibit-message t))
                          (replace-regexp "^#" "##" nil (point-min) (point-max)))
                        (when (y-or-n-p (format "Create new comment for ticket %s from this content?" issue-id))
                          (alist-get 'id (ytr-send-new-comment-alist issue-id `((text . ,(buffer-string)))))))))
    (ytr-add-issue-to-history issue-id)
    (cond (new-node-id
           (message "New comment created on %s with node id %s." issue-id new-node-id)
           (cond ((eq ytr-make-new-comment-behavior 'kill) (kill-region (point-min) (point-max)))
                 ((eq ytr-make-new-comment-behavior 'link)
                  (kill-region (point-min) (point-max))
                  (insert (format "%s#%s\n" issue-id new-node-id)))
                 ((eq ytr-make-new-comment-behavior 'fetch)
                  (kill-region (point-min) (point-max))
                  (ytr-get-insert-remote-node issue-id new-node-id 'comment curlevel)
                  ;; (goto-char (point-min))
                  ;; (unless (org-entry-get (point) "YTR_SHORTCODE" t)
                  ;;   (org-set-property "YTR_SHORTCODE" issue-id))
                  )))))
  (widen)
  (ytr-shortcode-buttonize-buffer))

(defun ytr-update-remote-node-editable ()
  "Update a node on remote side after editing locally"
  (interactive)
  (let* ((type-str (ytr-find-node))
         (type (if (string= type-str "description")
                   'description
                 (if (string= type-str "comment")
                     'comment
                   (user-error (format "Unknown node type: %s" type-str)))))
         (issue-id (org-entry-get (point) "YTR_SHORTCODE" t))
         (node-id (org-entry-get (point) "YTR_ID" t))
         (content (cl-case type
                    (description (alist-get 'description (ytr-retrieve-issue-alist issue-id)))
                    (comment (alist-get 'text (ytr-retrieve-issue-comment-alist issue-id node-id)))))
         (remote-hash (if content (sha1 content) ""))
         (local-hash (org-entry-get (point) "YTR_CONTENT_HASH" t))
         (wconf (current-window-configuration)))
    (ytr-add-issue-to-history issue-id)
    (when (not (string= local-hash remote-hash))
        (user-error "Aborted! Remote Node was edited since last fetch: %s %s" local-hash remote-hash))

    (org-gfm-export-as-markdown nil t)
    (replace-regexp "^#" "##" nil (point-min) (point-max))
    (ytr-commit-update-node-mode)
    (message "Update %s with ID %s on issue %s" type node-id issue-id)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-commit-type 'update
                ytr-buffer-node-type type
                ytr-buffer-issue-id issue-id
                ytr-buffer-node-id node-id)))

(defun ytr-update-remote-node ()
  "Update a node on remote side after editing locally"
  (interactive)
  (let* ((type-str (ytr-find-node))
         (type (if (string= type-str "description")
                   'description
                 (if (string= type-str "comment")
                     'comment
                   (user-error (format "Unknown node type: %s" type-str)))))
         (issue-id (org-entry-get (point) "YTR_SHORTCODE" t))
         (node-id (org-entry-get (point) "YTR_ID" t))
         (content (if (eq type 'description)
                      (alist-get 'description (ytr-retrieve-issue-alist issue-id))
                    (alist-get 'text (ytr-retrieve-issue-comment-alist issue-id node-id))))
         (remote-hash (if content (sha1 content) ""))
         (local-hash (org-entry-get (point) "YTR_CONTENT_HASH" t))
         )
    (ytr-add-issue-to-history issue-id)
    (when (not (string= local-hash remote-hash))
        (user-error "Aborted! Remote Node was edited since last fetch: %s %s" local-hash remote-hash))

    (when (save-window-excursion
            (org-gfm-export-as-markdown nil t)
            (markdown-mode)
            (let ((inhibit-message t))
              (replace-regexp "^#" "##" nil (point-min) (point-max)))
            (when (y-or-n-p (format "Update %s with id %s of ticket %s with this content?" type node-id issue-id))
              (if (eq type 'description)
                  (ytr-send-issue-alist issue-id `((description . ,(buffer-string))))
                (ytr-send-issue-comment-alist issue-id node-id `((text . ,(buffer-string)))))
              (message "Node successfully updated")))
        (ytr-fetch-remote-node))))

(defun ytr-send-node ()
  "Create a new comment or update the node, depending on context."
  (interactive)
  (if (org-entry-get (point) "YTR_TYPE" t)
      (ytr-update-remote-node-editable)
    (ytr-new-comment-editable))
  )

(defun ytr-get-insert-remote-node (issue-id node-id type level)
  "Insert a remote node in org format"
  (let* ((node-alist
          (if (eq type 'description)
              (ytr-retrieve-issue-alist issue-id)
            (ytr-retrieve-issue-comment-alist issue-id node-id)))
         (created (alist-get 'created node-alist))
         (author (alist-get 'fullName
                            (if (eq type 'description)
                                (alist-get 'reporter node-alist)
                              (alist-get 'author node-alist))))
         (content
          (if (eq type 'description)
              (alist-get 'description node-alist)
            (alist-get 'text node-alist))))
    (ytr-org-insert-node content level type issue-id node-id author created)))

(defun ytr-fetch-remote-node ()
  "Update a local node withs its remote content"
  (interactive)
  (let* ((type-str (ytr-find-node))
         (type (if (string= type-str "description")
                   'description
                 (if (string= type-str "comment")
                     'comment
                   (user-error "Cannot fetch node of type %s" type-str))))
         (issue-id (org-entry-get (point) "YTR_SHORTCODE" t))
         (node-id (org-entry-get (point) "YTR_ID" t))
         (curlevel (org-current-level)))
    (let ((inhibit-message t))
      (org-cut-subtree))
    (condition-case err
     (ytr-get-insert-remote-node issue-id node-id type curlevel)
     (error (undo)
            (signal (car err) (cdr err))))))

(defun ytr-org1 (issue-node-ids)
  ""
  (let* ((shortcode (car issue-node-ids))
         (comment-id (cdr issue-node-ids)))
    (ytr-issue-alist-to-org (ytr-retrieve-issue-alist "DEMO-21") shortcode)
    (org-mode)
    (ytr-shortcode-buttonize-buffer)
    (goto-char (point-min))
    (when comment-id
      (search-forward comment-id nil t)
      (org-back-to-heading))))

(defun ytr-dart-org (shortcode-node)
  "Like ytr-org but offers a simple prompt for entering the shortcode with no completions. It may have a node id."
  (interactive "sShortcode: ")
  (ytr-org1 (ytr-parse-shortcode-and-node-id shortcode-node)))

(defun ytr-embark-org (cand)
  "Like ytr-dart-org but cand consists of shortcode and summary"
  (ytr-dart-org (car (split-string cand ":"))))

(defun ytr-org (shortcode)
  "Retrieve an issue and convert it to a temporary org buffer"
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-shortcode))))
  (ytr-add-issue-to-history shortcode)
  (ytr-dart-org shortcode))

(defun ytr-smart-org (issue-comment-ids)
  "Retrieve an issue and convert it to a temporary org buffer"
  (interactive (list (ytr-guess-or-read-shortcode-and-comment-id)))
  (ytr-add-issue-to-history (car issue-comment-ids))
  (ytr-org1 issue-comment-ids))

(defun ytr-org-heading-set-shortcode ()
  "Set Property YTR_SHORTCODE on org heading and append tag YTR"
  (interactive)
  (org-set-property "YTR_SHORTCODE" (org-read-property-value "YTR_SHORTCODE"))
  (let ((tags (org-get-tags)))
    (if (member "YTR" tags) nil (org-set-tags (append (list "YTR") tags)))))

;;;; preview

(defun ytr-sneak-window-issue (issue-alist)
  (with-output-to-temp-buffer "*ytr-describe-issue*"
    (let-alist issue-alist
    (princ (format "%s: %s\n" .idReadable .summary))
    (princ (format "Reporter: %s, created: %s, updated: %s, Resolved: %s, Priority: %s, State: %s, Assignee: %s\n"
                   (alist-get 'fullName .reporter)
                   (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000))
                   (format-time-string "%Y-%m-%d %H:%M" (/ .updated 1000))
                   (if .resolved (format-time-string "%Y-%m-%d %H:%M" (/ .resolved 1000)) "-")
                   (ytr-get-customField-value issue-alist "Priority")
                   (ytr-get-customField-value issue-alist "State")
                   (ytr-get-customField-value issue-alist "Assignee")))
    (princ "------------------------\n")
    (princ .description))))

(defun ytr-sneak1 (issue-node-ids)
  "Display a side window with the description and same basic information on issue and comment id cons cell."
  (let* ((shortcode (car issue-node-ids)))
    (ytr-sneak-window-issue (ytr-retrieve-issue-alist shortcode))))

(defun ytr-dart-sneak (shortcode-node)
  "Like ytr-sneak but offers a simple prompt for entering the shortcode with no completions. It may have a node id."
  (interactive "sShortcode: ")
  (ytr-sneak1 (ytr-parse-shortcode-and-node-id shortcode-node)))

(defun ytr-embark-sneak (cand)
  "Like ytr-dart-sneak but cand consists of shortcode and summary"
  (ytr-dart-sneak (car (split-string cand ":"))))

(defun ytr-sneak (shortcode)
  "Display a side window with the description and same basic information on issue with SHORTCODE"
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-shortcode))))
  (ytr-dart-sneak shortcode))

(defun ytr-smart-sneak (issue-comment-ids)
  "Display a side window with the description and same basic information on issue with SHORTCODE"
  (interactive (list (ytr-guess-or-read-shortcode-and-comment-id)))
  (ytr-sneak1 issue-comment-ids))

;;;; Copy URL

(defun ytr-url (issue-node-ids)
  "Get the url for issue with comment id"
  (let* ((shortcode (car issue-node-ids))
         (comment-id (cdr issue-node-ids)))
    (if comment-id
      (ytr-issue-comment-url shortcode comment-id)
    (ytr-issue-url shortcode))))

(defun ytr-copy-url1 (issue-node-ids)
  ""
  (let ((url (ytr-url issue-node-ids)))
    (message url)
    (kill-new url)))

(defun ytr-dart-copy-url (shortcode-node)
  "Like ytr-copy-url but offers a simple prompt for entering the shortcode with no completions. It may have a node id."
  (interactive "sShortcode: ")
  (ytr-copy-url1 (ytr-parse-shortcode-and-node-id shortcode-node)))

(defun ytr-embark-copy-url (cand)
  "Like ytr-dart-copy-url but cand consists of shortcode and summary"
  (ytr-dart-copy-url (car (split-string cand ":"))))

(defun ytr-copy-url (shortcode)
  "Copy the url to an issue to kill ring"
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-shortcode))))
  (ytr-add-issue-to-history shortcode)
  (ytr-dart-copy-url shortcode))

(defun ytr-smart-copy-url (issue-comment-ids)
  "Open an issue in the webbrowser"
  (interactive (list (ytr-guess-or-read-shortcode-and-comment-id)))
  (ytr-add-issue-to-history (car issue-comment-ids))
  (ytr-copy-url1 issue-comment-ids))

;;;; Open in browser

(defun ytr-browse1 (issue-node-ids)
  ""
  (browse-url (ytr-url issue-node-ids)))

(defun ytr-dart-browse (shortcode-node)
  "Like ytr-browser but offers a simple prompt for entering the shortcode with no completions. It may have a node id."
  (interactive "sShortcode: ")
  (ytr-browse1 (ytr-parse-shortcode-and-node-id shortcode-node)))

(defun ytr-embark-browse (cand)
  "Like ytr-dart-browse but cand consists of shortcode and summary"
  (ytr-dart-browse (car (split-string cand ":"))))

(defun ytr-browse (shortcode)
  "Open an issue in the webbrowser"
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-shortcode))))
  (ytr-add-issue-to-history shortcode)
  (ytr-dart-browse shortcode))

(defun ytr-smart-browse (issue-comment-ids)
  "Open an issue in the webbrowser"
  (interactive (list (ytr-guess-or-read-shortcode-and-comment-id)))
  (ytr-add-issue-to-history (car issue-comment-ids))
  (ytr-browse1 issue-comment-ids))

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
    (browse-url (ytr-issue-url choice-id))
    )
  )

(defun ytr-browse-new-issue (title description)
  "Open web browser to create a new issue"
  (browse-url (concat ytr-baseurl "/newIssue?description=" description "&summary=" title)))

(defun ytr-browse-query (query)
  "Open web browser to execute a query"
  (interactive (list
                (let ((ytr-use-saved-queries (and ytr-use-saved-queries (not current-prefix-arg))))
                  (ytr-read-query-consult))))
  (browse-url (concat ytr-baseurl "/issues?q=" query)))

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
    (while (re-search-forward "[^A-z0-9-]\\([A-z]+-[0-9]+\\(#[0-9-]+\\)?\\)[^A-z0-9-]" nil t)
      (make-button (match-beginning 1) (match-end 1) :type 'shortcode-button))))

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
