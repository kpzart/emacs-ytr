;;; yt.el --- Youtrack integration into emacs

;; Copyright (C) 2010-2023 Martin Puttke

;; Author: Martin Puttke <m.s.p@posteo.de>
;; Created: 07 Mar 2023
;; Keywords: convenience
;; URL: https://github.com/matlantis/emacs-yt
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
;; - kpz/yt-query: browse your issues

;;; Change Log:
;;
;; empty

;;; Code:

(require 'ffap)
(require 'plz)
(require 'helm)

(defgroup yt nil "Youtrack integration into emacs")

(defcustom yt-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server" :type 'string :group 'yt)

(defcustom yt-access-token "" "Your access token. Maybe a string or function" :type '(choice function string) :group 'yt)

(defcustom yt-queries () "Define your Queries here" :type '(repeat string) :group 'yt)

(defcustom yt-plz-debug nil "Print plz debug messages if non nil" :type 'bool :group 'yt)

;; * urls and shortcodes
(defun kpz/yt-issue-url (shortcode)
  "Return the URL for a issue given by shortcode"
  (concat yt-baseurl "/issue/" shortcode))

(defun kpz/yt-query-url (query)
  "Return the URL for a query"
  (concat yt-baseurl "/issues?q=" (url-hexify-string query)))

(defun kpz/yt-query-shortcode ()
  "Return a shortcode from a query"
  (let* ((query (completing-read "Query: " yt-queries nil nil))
         (result (kpz/yt-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          result))
         (choice (completing-read "Issue: " choices)))
    (car (split-string choice ":")))
  )

(add-to-list 'ffap-string-at-point-mode-alist '(yt "0-9A-z-" "" ""))

(defun kpz/yt-shortcode-from-point ()
  "Return the shortcode at point or nil if there is none"
  (let ((candidate (ffap-string-at-point 'yt)))
    (if (string-match-p "[A-z]+-[0-9]+" candidate)
        candidate
      nil))
  )

(defun kpz/yt-shortcode-from-org-property ()
  "Return the shortcode defined by an org property YT_SHORTCODE or nil"
  (org-entry-get (point) "YT_SHORTCODE" t))

(defun kpz/yt-shortcode-from-branch ()
  "Return the shortcode from the name of the current git branch"
  (let ((branch-name (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
    (if (string-match "^\\([A-z]+/\\)?\\([A-z]+-[0-9]+\\)-.*$" branch-name)
        (match-string 2 branch-name)))
  )

(defun kpz/yt-guess-shortcode ()
  "Return a shortcode from current context."
  (let ((issue (kpz/yt-shortcode-from-point)))
    (if issue issue
      (let ((issue (kpz/yt-shortcode-from-org-property)))
        (if issue issue
          (let ((issue (kpz/yt-shortcode-from-branch)))
            (if issue issue nil)))))))

(defun kpz/yt-guess-or-query-shortcode ()
  "Guess shortcode on context or start a query."
  (let ((guess (kpz/yt-guess-shortcode)))
    (if guess guess (kpz/yt-query-shortcode)))
  )

;; * api
(defun kpz/yt-plz (method request &optional body)
  "Generic plz request method"
  (let* ((response (plz method request
                     :headers `(("Authorization" . ,(concat "Bearer "
                                                            (if (functionp yt-access-token)
                                                                (funcall yt-access-token)
                                                              (format "%s" yt-access-token))))
                                ("Accept" . "application/json")
                                ("Content-Type" . "application/json"))
                     :as #'json-read
                     :body body
                     :connect-timeout 10
                     )))
    (cond (yt-plz-debug
           (message "YT Request: %s" request)
           (message "YT Response: %s" response)))
    response))

(defun kpz/yt-retrieve-query-issues-alist (query)
  "Retrieve list of issues by query"
  (kpz/yt-plz 'get (concat yt-baseurl "/api/issues?fields=idReadable,summary,description,reporter,created,resolved&query=" (url-hexify-string query))))

(defun kpz/yt-retrieve-issue-alist (issue-id)
  "Retrieve information concering the given issue and return an alist."
  (kpz/yt-plz 'get (concat yt-baseurl "/api/issues/" issue-id "?fields=id,idReadable,summary,description,comments(id,text,created,author(login)),created,resolved,reporter(login),links(direction,linkType(name,sourceToTarget,targetToSource),issues(idReadable,summary))")))

(defun kpz/yt-retrieve-issue-comment-alist (issue-id comment-id)
  "Retrieve information concering the given issue and return an alist."
  (kpz/yt-plz 'get (concat yt-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=id,text,created,author(login)")))

(defun kpz/yt-send-new-comment-alist (issue-id alist)
  "Send the information in ALIST a new comment for ticket with id ISSUE-ID"
  (kpz/yt-plz 'post (concat yt-baseurl "/api/issues/" issue-id "/comments/") (json-encode alist)))

(defun kpz/yt-send-issue-comment-alist (issue-id comment-id alist)
  "Send the information in ALIST for a remote update of an issue comment with id ISSUE"
  (kpz/yt-plz 'post (concat yt-baseurl "/api/issues/" issue-id "/comments/" comment-id "?fields=text") (json-encode alist)))

(defun kpz/yt-send-issue-alist (issue alist)
  "Send the information in ALIST for a remote update of issue with id ISSUE"
  (kpz/yt-plz 'post (concat yt-baseurl "/api/issues/" issue "?fields=description") (json-encode alist)))

;; * org mode conversion
(defun kpz/yt-md-to-org (input level)
  "Convert a markdown string to org mode using pandoc. LEVEL indicates the level of top level headings in org and defaults to 3."
  (save-current-buffer
    (set-buffer (get-buffer-create "*yt-convert*"))
    (setf (buffer-string) "")
    (insert input)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f gfm -t org")
                             nil t "*yt-convert-error*")
    (replace-string "☒" "[X]" t (point-min) (point-max))
    (replace-string "☐" "[ ]" t (point-min) (point-max))
    (org-mode)
    (kpz/yt-demote-org-headings (or level 3))
    (org-unindent-buffer)
    (buffer-string)
    )
)

(defun kpz/yt-issue-alist-to-org (input shortcode)
  "Convert an alist of markdown code into an org buffer with proper headings"
  (let ((bufname (format "*kpz/yt-org-%s*" (downcase shortcode))))
    (set-buffer (get-buffer-create bufname))
    (setf (buffer-string) "")
    (org-mode)
    (let-alist input
      ;; title and description
      (insert (concat "#+Title: " .idReadable ": " .summary "\n\n"))
      (org-set-property "YT_SHORTCODE" shortcode)
      (org-set-property "YT_ID" .id)
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
      (kpz/yt-org-insert-node .description 2 'description shortcode .id (alist-get 'login .reporter) .created)
      ;; do the comments
      (mapcar (lambda (comment-alist)
                (let-alist comment-alist
                  (kpz/yt-org-insert-node .text 2 'comment shortcode .id (alist-get 'login .author) .created)))
              .comments)

      ;; postprocess
      (org-unindent-buffer)
      (switch-to-buffer bufname)
      ))
  )

(defun kpz/yt-max-heading-level ()
  "Determine the highest Heading in the buffer. Return nil if no heading found."
  (save-excursion
    (goto-char (point-min))
    (let ((lowest-level 6)) ;; Set initial lowest level to the maximum heading level (e.g., 6 for `org-mode`)
      (while (re-search-forward "^\\(\\*+\\)" nil t)
        (setq lowest-level (min lowest-level (length (match-string 1)))))
      lowest-level)))

(defun kpz/yt-first-heading-level ()
  (org-fold-show-all)
  (goto-char (point-min))
  (when (not (org-at-heading-p))
    (org-next-visible-heading 1)
    )
  (org-current-level)
  )

(defun kpz/yt-demote-org-headings (level)
  (setq base-level (kpz/yt-max-heading-level))
  (when base-level
    (if (< level base-level)
        (progn
          (setq dir -1)
          (goto-char (point-max)))
      (progn
        (setq dir 1)
        (goto-char (point-min))))

    (setq last-point 0)
    (when (not (org-at-heading-p))
      (org-next-visible-heading dir))
    (while (and (/= (point) last-point) (org-at-heading-p))
      (when (= (org-current-level) base-level)
        (let ((levelsteps (- level (org-current-level))))
          (if (> levelsteps 0)
              (dotimes (i levelsteps)
                (org-demote-subtree))
            (dotimes (i (- 0 levelsteps))
              (org-promote-subtree))
            )))
      (setq last-point (point))
      (org-next-visible-heading dir)))
  )

(defun kpz/yt-capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun kpz/yt-org-insert-node (content level type issue-id node-id author created)
  "Insert a comment node at point, level is that of the node, type is generic, author is a string, created is a long value"
  (let ((curpoint (point)))
    (insert (format "%s %s %s by %s\n\n"
                    (make-string level ?*)
                    (format-time-string "%Y-%m-%d %H:%M" (/ created 1000))
                    (kpz/yt-capitalize-first-char (format "%s" type))
                    author
                    ))
    (previous-line)
    (org-set-property "YT_CONTENT_HASH" (if content (sha1 content) ""))
    (org-set-property "YT_ID" node-id)
    (org-set-property "YT_TYPE" (format "%s" type))
    (insert "\n")
    (when content (insert (kpz/yt-md-to-org content (+ 1 level))))
    (unless (string= issue-id (org-entry-get (point) "YT_SHORTCODE" t))
      (org-set-property "YT_SHORTCODE" issue-id))
    (goto-char curpoint)

    )
  )

(defun kpz/yt-find-node ()
  "Find the parent heading with a YT_TYPE property, sets the point and returns the type. If property is not found in buffer returns nil."
  (when (or (org-at-heading-p) (org-back-to-heading))
    (let ((type (org-entry-get (point) "YT_TYPE")))
      (if (or (string= type "comment") (string= type "description"))
          type
        (if (org-up-heading-safe)
            (kpz/yt-find-node)
          nil))))
  )

;; * org interactive
(defun kpz/yt-new-comment ()
  "Send the current subtree as comment to a ticket"
  (interactive)
  (unless (region-active-p) (org-mark-subtree))
  (let* ((issue-id (kpz/yt-guess-or-query-shortcode))
         (new-node-id (save-window-excursion
                        (org-gfm-export-as-markdown nil t)
                        (markdown-mode)
                        (replace-regexp "^#" "##" nil (point-min) (point-max))
                        (when (y-or-n-p (format "Create new comment for ticket %s from this content?" issue-id))
                          (alist-get 'id (kpz/yt-send-new-comment-alist issue-id `((text . ,(buffer-string)))))
                          )
                        )))
    (cond (new-node-id
           (kill-region (mark) (point))
           (insert (format "%s Comment\n\n" (make-string (+ (org-current-level) 1) ?*)))
           (org-set-property "YT_ID" new-node-id)
           (org-set-property "YT_TYPE" "comment")
           (unless (org-entry-get (point) "YT_SHORTCODE" t) (org-set-property "YT_SHORTCODE" issue-id))
           (kpz/yt-fetch-remote-node)
           (message "New comment created on %s with node id %s." issue-id new-node-id)
           ))
    )
  )

(defun kpz/yt-update-remote-node ()
  "Update a node on remote side after editing locally"
  (interactive)
  (let* ((type-str (kpz/yt-find-node))
         (type (if (string= type-str "description")
                   'description
                 (if (string= type-str "comment")
                     'comment
                   (user-error (format "Unknown node type: %s" type-str)))))
         (issue-id (org-entry-get (point) "YT_SHORTCODE" t))
         (node-id (org-entry-get (point) "YT_ID" t))
         (content (if (eq type 'description)
                      (alist-get 'description (kpz/yt-retrieve-issue-alist issue-id))
                    (alist-get 'text (kpz/yt-retrieve-issue-comment-alist issue-id node-id))))
         (remote-hash (if content (sha1 content) ""))
         (local-hash (org-entry-get (point) "YT_CONTENT_HASH" t))
         )
    (when (not (string= local-hash remote-hash))
        (user-error "Aborted! Remote Node was edited since last fetch: %s %s" local-hash remote-hash))

    (when (save-window-excursion
            (org-gfm-export-as-markdown nil t)
            (markdown-mode)
            (replace-regexp "^#" "##" nil (point-min) (point-max))
            (when (y-or-n-p (format "Update %s with id %s of ticket %s with this content?" type node-id issue-id))
              (if (eq type 'description)
                  (kpz/yt-send-issue-alist issue-id `((description . ,(buffer-string))))
                (kpz/yt-send-issue-comment-alist issue-id node-id `((text . ,(buffer-string)))))
              (message "Node successfully updated"))
            )
        (kpz/yt-fetch-remote-node)))
  )

(defun kpz/yt-send-node ()
  "Create a new comment or update the node, depending on context."
  (interactive)
  (if (org-entry-get (point) "YT_TYPE" t)
      (kpz/yt-update-remote-node)
    (kpz/yt-new-comment))
  )

(defun kpz/yt-fetch-remote-node ()
  "Update a local node withs its remote content"
  (interactive)
  (let* ((type-str (kpz/yt-find-node))
         (type (if (string= type-str "description")
                   'description
                 (if (string= type-str "comment")
                     'comment
                   (user-error "Cannot fetch node of type %s" type-str))))
         (issue-id (org-entry-get (point) "YT_SHORTCODE" t))
         (node-id (org-entry-get (point) "YT_ID" t))
         (node-alist
          (if (eq type 'description)
              (kpz/yt-retrieve-issue-alist issue-id)
            (kpz/yt-retrieve-issue-comment-alist issue-id node-id))
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
         (curlevel (org-current-level))
         )
    ;; markiere den subtree und ersetze ihn durch das geholte, setze die properties
    (org-cut-subtree)
    (kpz/yt-org-insert-node content curlevel type issue-id node-id author created)
    )
  )

(defun kpz/yt-org (shortcode)
  "Retrieve an issue and convert it to a temporary org buffer"
  (kpz/yt-issue-alist-to-org (kpz/yt-retrieve-issue-alist shortcode) shortcode)
  (org-mode)
  (kpz/yt-shortcode-buttonize-buffer)
  (goto-char (point-min))
  )

(defun kpz/yt-query-org ()
  "Retrieve an issue and convert it to a temporary org buffer"
  (interactive)
  (kpz/yt-org (kpz/yt-query-shortcode))
  )

(defun kpz/yt-smart-query-org ()
  "Retrieve an issue and convert it to a temporary org buffer"
  (interactive)
  (kpz/yt-org (kpz/yt-guess-or-query-shortcode))
  )

;; * preview

(defun kpz/yt-sneak (issue-alist)
  (let-alist issue-alist
    (princ (format "%s: %s\n" .idReadable .summary))
    (princ (format "Reporter: %s, Created: %s, Resolved: %s\n"
                   (alist-get 'login .reporter)
                   (format-time-string "%Y-%m-%d %H:%M" (/ .created 1000))
                   (if .resolved (format-time-string "%Y-%m-%d %H:%M" (/ .resolved 1000)) "-")))
    (princ "------------------------\n")
    (princ .description)))

(defun kpz/yt-sneak-window (issue-alist)
  (with-output-to-temp-buffer "*yt-describe-issue*"
    (kpz/yt-sneak issue-alist)))

(defun kpz/yt-smart-query-sneak ()
  "Display a side window with the description and same basic information on issue with SHORTCODE"
  (interactive)
  (kpz/yt-sneak-window (kpz/yt-retrieve-issue-alist (kpz/yt-guess-or-query-shortcode)))
  )

;; * Issue buttons

(define-button-type 'shortcode-button
  'follow-link t
  'action #'kpz/yt-on-shortcode-button)

(defun kpz/yt-on-shortcode-button (button)
  (browse-url (kpz/yt-issue-url (buffer-substring (button-start button) (button-end button)))))

(defun kpz/yt-shortcode-buttonize-buffer ()
  "turn all issue shortcodes into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^A-z0-9-]\\([A-z]+-[0-9]+\\)[^A-z0-9-]" nil t)
      (make-button (match-beginning 1) (match-end 1) :type 'shortcode-button))))

(add-hook 'org-mode-hook 'kpz/yt-shortcode-buttonize-buffer)

;; * interactive
(defun kpz/yt-query-browse ()
  "Open an issue in the webbrowser"
  (interactive)
  (let* ((choice-id (kpz/yt-query-shortcode)))
    (browse-url (kpz/yt-issue-url choice-id))
    )
  )

(defun kpz/yt-smart-query-browse ()
  "Open an issue in the webbrowser"
  (interactive)
  (let* ((choice-id (kpz/yt-guess-or-query-shortcode)))
    (browse-url (kpz/yt-issue-url choice-id))
    )
  )

(defun kpz/yt-query-refine-browse ()
  "Present a list of resolved issues in the minibuffer"
  (interactive)
  (let* ((query-orig (completing-read "Query: " yt-queries nil t))
         (query (read-string "Refine query: " query-orig nil))
         (result (kpz/yt-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          result))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
    (browse-url (kpz/yt-issue-url choice-id))
    )
  )

;; * helm
(defun kpz/yt-helm-query (&optional defaultquery)
  "Return a shortcode using helm"
  (interactive)
  (let* ((query (completing-read "Query: " yt-queries nil nil defaultquery))
         (result (kpz/yt-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (issue-alist)
                            (let-alist issue-alist
                              (cons (format "%s: %s" .idReadable .summary) issue-alist))
                              )
                          result))
         )
    (if choices
        (helm :sources (helm-build-sync-source "yt-issues"
                         :candidates choices
                         :action '(("Open in browser" . (lambda (issue-alist) (browse-url (kpz/yt-issue-url (alist-get 'idReadable issue-alist)))))
                                   ("Open in org buffer" . (lambda (issue-alist) (kpz/yt-org (alist-get 'idReadable issue-alist)))))
                         :must-match 'ignore
                         :persistent-action 'kpz/yt-sneak-window
                         :keymap (let ((map (make-sparse-keymap)))
                                   (set-keymap-parent map helm-map)
                                   (define-key map (kbd "M-q") (lambda () (interactive) (helm-run-after-exit 'kpz/yt-helm-query query)))
                                   (define-key map (kbd "M-Q") (lambda () (interactive) (helm-run-after-exit 'kpz/yt-helm-query)))
                                   (define-key map (kbd "M-w") (lambda () (interactive) (helm-run-after-exit 'browse-url (kpz/yt-query-url query))))
                                   map)
                         :cleanup (lambda () (kill-matching-buffers "*yt-describe-issue*" nil t))
                         )
              :buffer "*helm yt*")
      (message "No Issues found."))
    )
  )

(provide 'yt)
;;; yt.el ends here
