;; -*- lexical-binding: t; -*-
;;; ytr-api.el --- YouTrack API layer for ytr

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
;; HTTP/REST API layer for YouTrack integration.
;; This module handles all communication with the YouTrack server.

;;; Code:

(require 'request)
(require 'json)

;;;; Configuration variables (defined in main ytr.el, declared here)
(defvar ytr-baseurl)
(defvar ytr-access-token)
(defvar ytr-request-debug)

;;;; Generic request functions

(defun ytr-request (method url &optional body)
  "Generic request method.
METHOD is the HTTP method (GET, POST, etc.).
URL is the full URL to request.
BODY is optional JSON data to send."
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
  "Generic request method for uploading a list of files.
URL is the upload endpoint.
PATHS is a list of file paths to upload."
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

;;;; Data retrieval functions

(defun ytr-retrieve-query-issues-alist (query)
  "Retrieve list of issues by QUERY."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues?fields=idReadable,summary,description,created,updated,resolved,reporter(login,fullName),customFields(name,value(name)),comments&query=" (url-hexify-string query))))

(defun ytr-retrieve-saved-queries-alist ()
  "Retrieve list of saved queries."
  (ytr-request "GET" (concat ytr-baseurl "/api/savedQueries?fields=name,query,owner(fullName),issues(resolved,customFields(name,value(name)))")))

(defun ytr-retrieve-issue-alist (issue-code)
  "Retrieve information concerning the given ISSUE-CODE and return an alist."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues/" issue-code "?fields=id,idReadable,summary,description,comments(id,text,created,updated,author(login,fullName),attachments(name,url,size,mimeType),deleted),created,updated,resolved,reporter(login,fullName),links(direction,linkType(name,sourceToTarget,targetToSource),issues(idReadable,summary)),customFields(name,value(name)),attachments(name,url,size,mimeType,comment(id))")))

(defun ytr-retrieve-issue-comment-alist (issue-node-cons)
  "Retrieve information concerning the given ISSUE-NODE-CONS and return an alist.
ISSUE-NODE-CONS is a cons cell (issue-code . comment-id)."
  (ytr-request "GET" (concat ytr-baseurl "/api/issues/" (car issue-node-cons) "/comments/" (cdr issue-node-cons) "?fields=id,text,created,updated,author(login,fullName),attachments(name,url,size,mimeType),deleted")))

;;;; Data sending functions

(defun ytr-send-new-comment-alist (issue-code alist)
  "Send the information in ALIST as a new comment for ticket with id ISSUE-CODE."
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue-code "/comments/") (json-encode alist)))

(defun ytr-send-issue-comment-alist (issue-node-cons alist)
  "Send information in ALIST for a remote update of an issue comment.
ISSUE-NODE-CONS is a cons cell (issue-code . comment-id)."
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" (car issue-node-cons) "/comments/" (cdr issue-node-cons) "?fields=text") (json-encode alist)))

(defun ytr-send-issue-alist (issue alist)
  "Send the information in ALIST for a remote update of issue with id ISSUE."
  (ytr-request "POST" (concat ytr-baseurl "/api/issues/" issue "?fields=description") (json-encode alist)))

(defun ytr-send-as-attachments (paths issue-code &optional node-code)
  "Attach the files at PATHS to the ticket with id ISSUE-CODE.
If NODE-CODE is provided, attach to that specific comment."
  (ytr-request-upload (concat ytr-baseurl "/api/issues/" issue-code (if node-code (format "/comments/%s" node-code) "") "/attachments?fields=id,name") paths))

;;;; Custom field accessors

(defun ytr-get-customField-value (issue-alist field-name)
  "Get the value of custom field FIELD-NAME from ISSUE-ALIST."
  (let* ((field-alist (cl-find-if (lambda (alist) (equal (cdr (assoc 'name alist)) field-name)) (alist-get 'customFields issue-alist)))
         (value-alist (assoc 'value field-alist))
         (value-name (cdr (assoc 'name (cdr value-alist)))))
    (if value-name value-name "-")))

(defun ytr-get-customField-list-value (issue-alist field-name)
  "Get the list value of custom field FIELD-NAME from ISSUE-ALIST."
  (let* ((field-alist (cl-find-if (lambda (alist) (equal (cdr (assoc 'name alist)) field-name)) (alist-get 'customFields issue-alist)))
         (values-alist (assoc 'value field-alist))
         (value-name (mapconcat (lambda (value-alist)
                                  (cdr (assoc 'name value-alist)))
                                (cdr values-alist) ", ")))
    (if (< 0 (length value-name)) value-name "-")))

(provide 'ytr-api)
;;; ytr-api.el ends here
