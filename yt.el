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

(require 'plz)

(defgroup yt nil "Youtrack integration into emacs")

(defcustom yt-baseurl "https://somewhere.youtrack.cloud" "Base url of your youtrack server" :type 'string :group 'yt)

(defcustom yt-access-token "" "Your access token" :type 'string :group 'yt)

(defcustom yt-queries () "Define your Queries here" :type '(repeat string) :group 'yt)

(defun kpz/yt-retrieve-query-issues-alist (query)
  "Retrieve list of issues by query"
  (plz 'get (concat yt-baseurl "/api/issues?fields=idReadable,summary&query=" (url-hexify-string query))
    :headers `(("Authorization" . ,(concat "Bearer " yt-access-token))
               ("Accept" . "application/json")
               ("Content-Type" . "application/json"))
    :as #'json-read
    ))

(defun kpz/yt-issue-url (shortcode)
  "Return the URL for a issue given by shortcode"
  (concat yt-baseurl "/issue/" shortcode))

(defun kpz/yt-query-browse ()
  "Present a list of resolved issues in the minibuffer"
  (interactive)
  (let* ((query (completing-read "Query: " yt-queries nil nil))
         (result (kpz/yt-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                           (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                         result))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
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

;;  org mode conversion
(defun kpz/yt-retrieve-issue-alist (issue)
  "Retrieve information concering the given issue and return an alist."
  (plz 'get (concat "https://matlantis.youtrack.cloud/api/issues/" issue "?fields=id,idReadable,summary,description,comments(id,text)")
    :headers '(("Authorization" . "Bearer perm:cm9vdA==.NDctMA==.4yaPBDqQTSnPMdhzK6C6K8yMenpT7D")
               ("Accept" . "application/json")
               ("Content-Type" . "application/json"))
    :as #'json-read
    ))

(defun kpz/yt-md-to-org (input)
  "Convert a markdown string to org mode using pandoc"
  (save-current-buffer
    (set-buffer (get-buffer-create "*yt-convert*"))
    (setf (buffer-string) "")
    (insert input)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f gfm -t org")
                             nil t "*yt-convert-error*")
    (org-mode)
    (kpz/yt-demote-org-headings 3)
    (buffer-string)
    )
)

(defun kpz/yt-issue-alist-to-org (input shortcode)
  "Convert an alist of markdown code into an org buffer with proper headings"
  (set-buffer (get-buffer-create "*kpz/yt-org*"))
  (setf (buffer-string) "")
  (org-mode)
  (let-alist input
    ;; title and description
    (insert (concat "#+Title: " .idReadable ": " .summary "\n\n"))
    (org-set-property "YT_SHORTCODE" shortcode)
    (org-set-property "YT_ID" .id)
    (org-set-property "YT_TYPE" "ticket")
    (insert (concat "* ".idReadable ": " .summary "\n\n"))
    (insert "** Description\n\n")
    (org-set-property "YT_CONTENT_HASH" (sha1 .description))
    (org-set-property "YT_TYPE" "description")
    (insert (kpz/yt-md-to-org .description))

    ;; do the comments
    (mapcar (lambda (comment-alist)
              (insert (concat "*** Comment\n\n"))
              (let-alist comment-alist
                (org-set-property "YT_CONTENT_HASH" (sha1 .text))
                (org-set-property "YT_ID" .id)
                (org-set-property "YT_TYPE" "comment")
                (insert (kpz/yt-md-to-org .text))
                ))
            .comments)

    ;; postprocess
    (org-unindent-buffer)
    (switch-to-buffer "*kpz/yt-org*")
    )
  )

(defun kpz/yt-query-org ()
  "Start a query to retrieve an issue and convert it to a temporary org buffer"
  (interactive)
  (let* ((query (completing-read "Query: " yt-queries nil nil))
         (result (kpz/yt-retrieve-query-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          result))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
    (kpz/yt-issue-alist-to-org (kpz/yt-retrieve-issue-alist choice-id) choice-id)
    (org-fold-show-all)
    (goto-char (point-min))
    )
  )

(defun kpz/yt-max-heading-level ()
  (org-fold-show-all)
  (setq base-level nil)
  (goto-char (point-min))
  (when (not (org-at-heading-p))
    (org-next-visible-heading 1)
    )
  (setq last-point 0)
  (while (and (org-at-heading-p) (/= (point) last-point))
    (setq base-level (if (and base-level (< base-level (org-current-level))) base-level (org-current-level)))
    (org-next-visible-heading 1)
    )
  base-level
  )

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

;; Issue buttons

(define-button-type 'issue-button
  'follow-link t
  'action #'kpz/yt-on-issue-button)

(defun kpz/yt-on-issue-button (button)
  (browse-url (kpz/yt-issue-url (buffer-substring (button-start button) (button-end button)))))

(defun kpz/yt-issue-buttonize-buffer ()
  "turn all issue shortcodes into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[A-z]+-[0-9]+" nil t)
      (make-button (match-beginning 0) (match-end 0) :type 'issue-button))))

(add-hook 'org-mode-hook 'kpz/yt-issue-buttonize-buffer)

(provide 'yt)
;;; yt.el ends here
