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

(defun kpz/yt-retrieve-search-issues-alist (query)
  "Retrieve list of issues by query"
  (plz 'get (concat yt-baseurl "/api/issues?fields=idReadable,summary&query=" (url-hexify-string query))
    :headers `(("Authorization" . ,(concat "Bearer " yt-access-token))
               ("Accept" . "application/json")
               ("Content-Type" . "application/json"))
    :as #'json-read
    ))

(defun kpz/yt-query ()
  "Present a list of resolved issues in the minibuffer"
  (interactive)
  (let* ((query (completing-read "Query: " yt-queries nil nil))
         (result (kpz/yt-retrieve-search-issues-alist query))
         (choices (mapcar (lambda (item)
                           (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                         result))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
    (browse-url (concat yt-baseurl "/issue/" choice-id))
    )
  )

(defun kpz/yt-query-refine ()
  "Present a list of resolved issues in the minibuffer"
  (interactive)
  (let* ((query-orig (completing-read "Query: " yt-queries nil t))
         (query (read-string "Refine query: " query-orig nil))
         (result (kpz/yt-retrieve-search-issues-alist query))
         (choices (mapcar (lambda (item)
                            (concat (alist-get 'idReadable item) ": " (alist-get 'summary item)))
                          result))
         (choice (completing-read "Issue: " choices))
         (choice-id (car (split-string choice ":"))))
    (browse-url (concat yt-baseurl "/issue/" choice-id))
    )
  )

(provide 'yt)
;;; yt.el ends here
