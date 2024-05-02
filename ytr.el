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

(require 'ffap)
(require 'plz)

(defgroup ytr nil "Youtrack integration into emacs")

(defcustom ytr-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server" :type 'string :group 'ytr)

(defcustom ytr-access-token "" "Your access token. Maybe a string or function" :type '(choice function string) :group 'ytr)

(defcustom ytr-queries () "Define your Queries here" :type '(repeat string) :group 'ytr)

(defcustom ytr-plz-debug nil "Print plz debug messages if non nil" :type 'bool :group 'ytr)

(defcustom ytr-make-new-comment-behavior 'link "What should be done with the region from which a comment was created? One of 'kill, 'fetch (buggy), 'link or nil." :type 'symbol :group 'ytr)

(defvar ytr-issue-history '() "History for issues")

;; * urls and shortcodes
(defun ytr-issue-url (shortcode)
  "Return the URL for a issue given by shortcode"
  (concat ytr-baseurl "/issue/" shortcode))

(defun ytr-issue-comment-url (shortcode comment-id)
  "Return the URL for a issue given by shortcode with focus on comment with comment-id"
  (concat ytr-baseurl "/issue/" shortcode "#focus=Comments-" comment-id ".0-0"))

(defun ytr-query-url (query)
  "Return the URL for a query"
  (concat ytr-baseurl "/issues?q=" (url-hexify-string query)))

(defun ytr-completing-read-annotated (prompt choices-annotated)
  "Like competing-read but receives a sequence of (choice . annotations) cons cells."
  ;; (completing-read prompt choices-annotated)
  (completing-read prompt (lambda (str pred flag)
                            (pcase flag
                              ('metadata `(metadata (annotation-function . ,(lambda (choice)
                                                                              (let ((annotation (cdr (assoc choice choices-annotated))))
                                                                                (marginalia--fields (annotation)))
                                                                              ))))
                              (_
                               (all-completions str choices-annotated pred))))))

;; Test Code
;; (ytr-completing-read-annotated "Test: " '(("A" . "Attr A1") ("BB" . "Attr B1"))) ;; works
;; (ytr-completing-read-annotated "Test: " '(("A" . ("Attr A1" "Attr A2")) ("B" . ("Attr B1" "Attr B2")))) ;; doesnt work yet

(defun ytr-query-shortcode ()
  "Return a shortcode from a query"
  (let* ((query (completing-read "Query: " ytr-queries nil nil))
         (result (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          result))
         (choice (completing-read "Issue: " choices)))
    (car (split-string choice ":")))
  )

(defun ytr-query-shortcode-annoted ()
  "Return a shortcode from a query"
  (let* ((query (completing-read "Query: " ytr-queries nil nil))
         (result (ytr-retrieve-query-issues-alist query))
         (choices-annotated (mapcar (lambda (item)
                                      (cons
                                       (concat (alist-get 'idReadable item) ": " (alist-get 'summary item))
                                       (concat "Shortcode: " (alist-get 'idReadable item)))) ; this is a dummy
                                    result))
         (choice (ytr-completing-read-annotated "Issue: " choices-annotated)))
    (car (split-string choice ":"))))

(add-to-list 'ffap-string-at-point-mode-alist '(ytr "0-9A-z-#" "" ""))

(defun ytr-parse-shortcode-and-comment-id (candidate)
  "Parse string for and issue shortcode and a comment id if present"
  (if (string-match "^\\([A-z]+-[0-9]+\\)\\(#\\([0-9-]+\\)\\)?$" candidate)
      (cons (match-string 1 candidate) (match-string 3 candidate))
    nil))

(defun ytr-shortcode-and-comment-id-from-point ()
  "Return a cons with the shortcode and optioinal the item id at point or nil if there is none"
  (ytr-parse-shortcode-and-comment-id (ffap-string-at-point 'ytr)))

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
        (match-string 2 branch-name)))
  )

(defun ytr-guess-shortcode ()
  "Return a shortcode from current context."
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

(defun ytr-guess-or-query-shortcode ()
  "Guess shortcode on context or start a query."
  (let ((guess (ytr-guess-shortcode)))
    (if guess guess (ytr-query-shortcode)))
  )

(defun ytr-guess-or-query-shortcode-and-comment-id ()
  "Guess shortcode on context or start a query."
  (let ((guess (ytr-guess-shortcode-and-comment-id)))
    (if guess guess (cons ytr-query-shortcode nil))))

;; history
(defun ytr-retrieve-history-issues-alist ()
  (let ((result))
    (dolist (elt (reverse ytr-issue-history) result)
      (setq result (cons (ytr-retrieve-issue-alist elt) result))))
  )

(defun ytr-add-issue-to-history (shortcode)
  (delete shortcode ytr-issue-history)
  (push shortcode ytr-issue-history)
)

;; * api
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
  (ytr-plz 'get (concat ytr-baseurl "/api/issues?fields=idReadable,summary,description,reporter,created,resolved&query=" (url-hexify-string query))))

(defun ytr-retrieve-issue-alist (issue-id)
  "Retrieve information concering the given issue and return an alist."
  (ytr-plz 'get (concat ytr-baseurl "/api/issues/" issue-id "?fields=id,idReadable,summary,description,comments(id,text,created,author(login)),created,resolved,reporter(login),links(direction,linkType(name,sourceToTarget,targetToSource),issues(idReadable,summary)),customFields(name,value(name))")))

(defun ytr-retrieve-issue-comment-alist (issue-id comment-id)
  "Retrieve information concering the given issue and return an alist."
  (ytr-plz 'get (concat ytr-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=id,text,created,author(login)")))

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

;; * org mode conversion
(defun ytr-md-to-org (input level)
  "Convert a markdown string to org mode using pandoc. LEVEL indicates the level of top level headings in org and defaults to 3."
  (save-current-buffer
    (set-buffer (get-buffer-create "*ytr-convert*"))
    (erase-buffer)
    (insert input)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f gfm -t org")
                             nil t "*ytr-convert-error*")
    (replace-string "☒" "[X]" t (point-min) (point-max))
    (replace-string "☐" "[ ]" t (point-min) (point-max))
    (goto-char (point-min))
    (flush-lines " *:[A-Z_]+:.*$")
    (org-mode)
    (ytr-demote-org-headings (or level 3))
    (org-unindent-buffer)
    (buffer-string)
    )
)

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
      (ytr-org-insert-node .description 2 'description shortcode .id (alist-get 'login .reporter) .created)
      ;; do the comments
      (mapcar (lambda (comment-alist)
                (let-alist comment-alist
                  (ytr-org-insert-node .text 2 'comment shortcode .id (alist-get 'login .author) .created)))
              .comments)

      ;; postprocess
      (org-unindent-buffer)
      (switch-to-buffer bufname)
      ))
  )

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
  "Insert a comment node at point, level is that of the node, type is generic, author is a string, created is a long value"
  (let ((curpoint (point)))
    (insert (format "%s %s %s by %s\n\n"
                    (make-string level ?*)
                    (format-time-string "%Y-%m-%d %H:%M" (/ created 1000))
                    (ytr-capitalize-first-char (format "%s" type))
                    author
                    ))
    (previous-line)
    (org-set-property "YTR_CONTENT_HASH" (if content (sha1 content) ""))
    (org-set-property "YTR_ID" node-id)
    (org-set-property "YTR_TYPE" (format "%s" type))
    (insert "\n")
    (when content (insert (ytr-md-to-org content (+ 1 level))))
    (unless (string= issue-id (org-entry-get (point) "YTR_SHORTCODE" t))
      (org-set-property "YTR_SHORTCODE" issue-id))
    (goto-char curpoint)

    )
  )

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

;; * org interactive
(defvar-keymap ytr-commit-new-node-mode-map "C-c C-c" #'ytr-commit-new-node)
(defvar-keymap ytr-commit-new-node-mode-map "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-new-node-mode markdown-mode "ytr-commit-new-node-mode" "Mode for editing markdown exports from org before sending them to youtrack")

(defvar-keymap ytr-commit-update-node-mode-map "C-c C-c" #'ytr-commit-update-node)
(defvar-keymap ytr-commit-update-node-mode-map "C-c C-k" #'ytr-cancel-commit)

(define-derived-mode ytr-commit-update-node-mode markdown-mode "ytr-commit-update-node-mode" "Mode for editing markdown exports from org before sending them to youtrack")

(defun ytr-cancel-commit ()
  "Cancel committing something to youtrack"
  (interactive)
  (set-window-configuration ytr-buffer-wconf)
  (widen)
  )

(defun ytr-commit-new-comment ()
  "Commit buffer content as a new comment"
  (setq new-node-id (alist-get 'id (ytr-send-new-comment-alist ytr-buffer-issue-id `((text . ,(buffer-string))))))
  (ytr-add-issue-to-history ytr-buffer-issue-id)
  (set-window-configuration ytr-buffer-wconf)
  (cond (new-node-id
         (message "New comment created on %s with node id %s." ytr-buffer-issue-id new-node-id)
         (cond ((eq ytr-make-new-comment-behavior 'kill) (kill-region (point-min) (point-max)))
               ((eq ytr-make-new-comment-behavior 'link)
                (kill-region (point-min) (point-max))
                (insert (format "%s#%s\n" ytr-buffer-issue-id new-node-id)))
               ((eq ytr-make-new-comment-behavior 'fetch)
                (kill-region (point-min) (point-max))
                (ytr-get-insert-remote-node ytr-buffer-issue-id new-node-id 'comment ytr-buffer-curlevel)
                )))))

(defun ytr-commit-new-issue ()
  "Commit buffer content as a new issue"
  (ytr-browse-new-issue ytr-new-issue-title (buffer-string))
  (set-window-configuration ytr-buffer-wconf)
  (kill-region (point-min) (point-max))
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
  (if (eq ytr-buffer-commit-type 'description)
      (ytr-send-issue-alist ytr-buffer-issue-id `((description . ,(buffer-string))))
    (ytr-send-issue-comment-alist ytr-buffer-issue-id ytr-buffer-node-id `((text . ,(buffer-string)))))
  (message "Node successfully updated")
  (set-window-configuration ytr-buffer-wconf)
  (ytr-fetch-remote-node))


(defun ytr-new-node-editable (type)
  "Use the current subtree or region to create a new comment or issue depending on TYPE to be either 'comment or 'issue"
  (cond ((and (eq type 'comment) (region-active-p)) (narrow-to-region (mark) (point)))
        (t (org-narrow-to-subtree)))
  (goto-char (point-min))
  (cond ((eq type 'issue)
         (setq title (org-get-heading t t t t))
         (kill-whole-line)))
  (let ((wconf (current-window-configuration))
        (issue-id (if (eq type 'comment)
                      (ytr-guess-or-query-shortcode)))
        (curlevel (ytr-max-heading-level)))
    (org-gfm-export-as-markdown nil nil)
    (markdown-mode)
    (replace-regexp "^#" "##" nil (point-min) (point-max))
    (ytr-commit-new-node-mode)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-curlevel curlevel
                ytr-buffer-issue-id issue-id
                ytr-buffer-node-type type
                ytr-new-issue-title title)))

(defun ytr-new-comment-editable ()
  "Send the current subtree or regio as comment to a ticket"
  (interactive)
  (ytr-new-node-editable 'comment))

(defun ytr-new-comment ()
  "Send the current subtree as comment to a ticket"
  (interactive)
  (cond ((region-active-p) (narrow-to-region (mark) (point)))
        (t (org-narrow-to-subtree)))
  (goto-char (point-min))
  (let* ((curlevel (ytr-max-heading-level))
         (issue-id (ytr-guess-or-query-shortcode))
         (new-node-id (save-window-excursion
                        (org-gfm-export-as-markdown nil nil)
                        (markdown-mode)
                        (replace-regexp "^#" "##" nil (point-min) (point-max))
                        (when (y-or-n-p (format "Create new comment for ticket %s from this content?" issue-id))
                          (alist-get 'id (ytr-send-new-comment-alist issue-id `((text . ,(buffer-string)))))
                          )
                        )))
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
         (content (if (eq type 'description)
                      (alist-get 'description (ytr-retrieve-issue-alist issue-id))
                    (alist-get 'text (ytr-retrieve-issue-comment-alist issue-id node-id))))
         (remote-hash (if content (sha1 content) ""))
         (local-hash (org-entry-get (point) "YTR_CONTENT_HASH" t))
         (wconf (current-window-configuration))
         )
    (ytr-add-issue-to-history issue-id)
    (when (not (string= local-hash remote-hash))
        (user-error "Aborted! Remote Node was edited since last fetch: %s %s" local-hash remote-hash))

    (org-gfm-export-as-markdown nil t)
    (replace-regexp "^#" "##" nil (point-min) (point-max))
    (ytr-commit-update-node-mode)
    (setq-local ytr-buffer-wconf wconf
                ytr-buffer-commit-type type
                ytr-buffer-issue-id issue-id
                ytr-buffer-node-id node-id))
  )

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
            (replace-regexp "^#" "##" nil (point-min) (point-max))
            (when (y-or-n-p (format "Update %s with id %s of ticket %s with this content?" type node-id issue-id))
              (if (eq type 'description)
                  (ytr-send-issue-alist issue-id `((description . ,(buffer-string))))
                (ytr-send-issue-comment-alist issue-id node-id `((text . ,(buffer-string)))))
              (message "Node successfully updated"))
            )
        (ytr-fetch-remote-node)))
  )

(defun ytr-send-node ()
  "Create a new comment or update the node, depending on context."
  (interactive)
  (if (org-entry-get (point) "YTR_TYPE" t)
      (ytr-update-remote-node-editable)
    (ytr-new-comment-editable))
  )

(defun ytr-get-insert-remote-node (issue-id node-id type level)
  "Insert a remote node in org format"
  (let* (
        (node-alist
         (if (eq type 'description)
             (ytr-retrieve-issue-alist issue-id)
           (ytr-retrieve-issue-comment-alist issue-id node-id))
         )
        (created (alist-get 'created node-alist))
        (author (alist-get 'login
                           (if (eq type 'description)
                               (alist-get 'reporter node-alist)
                             (alist-get 'author node-alist))))
        (content
         (if (eq type 'description)
             (alist-get 'description node-alist)
           (alist-get 'text node-alist)))
        )
    (ytr-org-insert-node content level type issue-id node-id author created))
  )

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
         (curlevel (org-current-level))
         )
    ;; markiere den subtree und ersetze ihn durch das geholte, setze die properties
    (org-cut-subtree)
    (ytr-get-insert-remote-node issue-id node-id type curlevel)
    )
  )

(defun ytr-org (shortcode)
  "Retrieve an issue and convert it to a temporary org buffer"
  (ytr-issue-alist-to-org (ytr-retrieve-issue-alist shortcode) shortcode)
  (org-mode)
  (ytr-shortcode-buttonize-buffer)
  (goto-char (point-min))
  )

(defun ytr-query-org ()
  "Retrieve an issue and convert it to a temporary org buffer"
  (interactive)
  (let ((shortcode (ytr-query-shortcode)))
    (ytr-add-issue-to-history shortcode)
    (ytr-org shortcode))
  )

(defun ytr-smart-query-org ()
  "Retrieve an issue and convert it to a temporary org buffer"
  (interactive)
  (let ((shortcode (ytr-guess-or-query-shortcode)))
    (ytr-add-issue-to-history shortcode)
    (ytr-org shortcode))
  )

;; * preview

(defun ytr-sneak (issue-alist)
  (let-alist issue-alist
    (princ (format "%s: %s\n" .idReadable .summary))
    (princ (format "Reporter: %s, Created: %s, Resolved: %s, State: %s, Assignee: %s\n"
                   (alist-get 'login .reporter)
                   (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000))
                   (if .resolved (format-time-string "%Y-%m-%d %H:%M" (/ .resolved 1000)) "-")
                   (ytr-get-customField-value issue-alist "State")
                   (ytr-get-customField-value issue-alist "Assignee")))
    (princ "------------------------\n")
    (princ .description)))

(defun ytr-sneak-window (issue-alist)
  (with-output-to-temp-buffer "*ytr-describe-issue*"
    (ytr-sneak issue-alist)))

(defun ytr-smart-query-sneak ()
  "Display a side window with the description and same basic information on issue with SHORTCODE"
  (interactive)
  (ytr-sneak-window (ytr-retrieve-issue-alist (ytr-guess-or-query-shortcode)))
  )

;; * Issue buttons

(define-button-type 'shortcode-button
  'follow-link t
  'action #'ytr-on-shortcode-button)

(defun ytr-browse (shortcode &optional comment-id)
  "Open an issue in browser and focus a comment, if comment-id is given."
  (browse-url (if comment-id
                  (ytr-issue-comment-url shortcode comment-id)
                (ytr-issue-url shortcode))))

(defun ytr-on-shortcode-button (button)
  (let ((issue-comment-ids (ytr-parse-shortcode-and-comment-id (buffer-substring (button-start button) (button-end button)))))
    (ytr-browse (car issue-comment-ids) (cdr issue-comment-ids))))

(defun ytr-shortcode-buttonize-buffer ()
  "turn all issue shortcodes into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^A-z0-9-]\\([A-z]+-[0-9]+\\(#[0-9-]+\\)?\\)[^A-z0-9-]" nil t)
      (make-button (match-beginning 1) (match-end 1) :type 'shortcode-button))))

(add-hook 'org-mode-hook 'ytr-shortcode-buttonize-buffer)

;; * interactive
(defun ytr-heading-set-shortcode ()
  "Set Property YTR_SHORTCODE on headline and append tag YTR"
  (interactive)
  (org-set-property "YTR_SHORTCODE" (org-read-property-value "YTR_SHORTCODE"))
  (let ((tags (org-get-tags)))
    (if (member "YTR" tags) nil (org-set-tags (append (list "YTR") tags)))))

(defun ytr-query-browse ()
  "Open an issue in the webbrowser"
  (interactive)
  (let* ((shortcode (ytr-query-shortcode)))
    (ytr-add-issue-to-history shortcode)
    (browse-url (ytr-issue-url shortcode))
    )
  )

(defun ytr-smart-query-browse ()
  "Open an issue in the webbrowser"
  (interactive)
  (let* ((issue-comment-ids (ytr-guess-or-query-shortcode-and-comment-id)))
    (ytr-add-issue-to-history (car issue-comment-ids))
    (ytr-browse (car issue-comment-ids) (cdr issue-comment-ids))))

(defun ytr-query-refine-browse ()
  "Edit a predefined query to find an issue and open it in the browser"
  (interactive)
  (let* ((query-orig (completing-read "Query: " ytr-queries nil t))
         (query (read-string "Refine query: " query-orig nil))
         (result (ytr-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          result))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
    (browse-url (ytr-issue-url choice-id))
    )
  )

(defun ytr-new-issue ()
  "Use the current subtree to create a new issue"
  (interactive)
  (ytr-new-node-editable 'issue))

(defun ytr-browse-new-issue (title description)
  "Open web browser to create a new issue"
  (browse-url (concat ytr-baseurl "/newIssue?description=" description "&summary=" title)))


;; * helm
(if (require 'helm nil t)
    (progn
      (defun ytr-helm-query (&optional defaultquery)
        "Use Helm to select an issue from a query and open it."
        (interactive)
        (let* ((query (completing-read "Query: " ytr-queries nil nil defaultquery))
               (result (ytr-retrieve-query-issues-alist query))
               (choices (mapcar (lambda (issue-alist)
                                  (let-alist issue-alist
                                    (cons (format "%s: %s" .idReadable .summary) issue-alist))
                                  )
                                result))
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
                                                                    (ytr-org shortcode)))))
                              :must-match 'ignore
                              :persistent-action 'ytr-sneak-window
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
        (let* ((result (ytr-retrieve-history-issues-alist))
               (choices (mapcar (lambda (issue-alist)
                                  (let-alist issue-alist
                                    (cons (format "%s: %s" .idReadable .summary) issue-alist))
                                  )
                                result))
               )
          (if choices
              (helm :sources (helm-build-sync-source "ytr-issues"
                                                     :candidates choices
                                                     :action '(("Open in browser" . (lambda (issue-alist) (browse-url (ytr-issue-url (alist-get 'idReadable issue-alist)))))
                                                               ("Open in org buffer" . (lambda (issue-alist) (ytr-org (alist-get 'idReadable issue-alist)))))
                                                     :must-match 'ignore
                                                     :persistent-action 'ytr-sneak-window
                                                     :cleanup (lambda () (kill-matching-buffers "*ytr-describe-issue*" nil t))
                                                     )
                    :buffer "*helm ytr*")
            (message "No Issues found."))
          ))
  ))

(provide 'ytr)
;;; ytr.el ends here
