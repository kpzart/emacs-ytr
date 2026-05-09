;;; ytr-org-test.el --- Tests for ytr-org -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for ytr-org.

;;; Code:

(require 'ert)
(require 'ytr-org)

(defvar ytr-baseurl nil
  "YouTrack base URL dynamically bound in tests.")

(defconst ytr-org-test-baseurl "https://youtrack.example"
  "Base URL used in ytr-org tests.")

(defconst ytr-org-test-attachment
  '((name . "test.png")
    (url . "/api/files/123"))
  "Default attachment used in ytr-org tests.")

(defmacro ytr-org-test-with-buffer (content attachments &rest body)
  "Insert CONTENT in a temp buffer, perform attachment replacement, then run BODY.
ATTACHMENTS is passed to `ytr-org-perform-import-to-org-attachment-replacements'."
  (declare (indent 2) (debug t))
  `(let ((ytr-baseurl ytr-org-test-baseurl))
     (with-temp-buffer
       (insert ,content)
       (goto-char (point-min))
       (ytr-org-perform-import-to-org-attachment-replacements ,attachments)
       ,@body)))

(defun ytr-org-test-link (url name &optional description)
  "Return the expected org link for attachment URL and NAME.
DESCRIPTION defaults to NAME."
  (format "[[%s%s&forceDownload=true&ytr_name=%s][%s]]"
          ytr-org-test-baseurl
          url
          name
          (or description name)))

(ert-deftest ytr-org-import-attachment-replaces-org-file-link ()
  (ytr-org-test-with-buffer "[[file:test.png]]" (list ytr-org-test-attachment)
                            (should (string= (buffer-string)
                                             (ytr-org-test-link "/api/files/123" "test.png")))))

(ert-deftest ytr-org-import-attachment-replaces-org-file-link-with-description ()
  (ytr-org-test-with-buffer "[[file:test.png][Screenshot]]" (list ytr-org-test-attachment)
                            (should (string= (buffer-string)
                                             (ytr-org-test-link "/api/files/123" "test.png" "Screenshot")))))

(ert-deftest ytr-org-import-attachment-replaces-markdown-image-empty-description ()
  (ytr-org-test-with-buffer "![](test.png)" (list ytr-org-test-attachment)
                            (should (string= (buffer-string)
                                             (ytr-org-test-link "/api/files/123" "test.png")))))

(ert-deftest ytr-org-import-attachment-replaces-markdown-image-with-description ()
  (ytr-org-test-with-buffer "![Screenshot](test.png)" (list ytr-org-test-attachment)
                            (should (string= (buffer-string)
                                             (ytr-org-test-link "/api/files/123" "test.png" "Screenshot")))))

(ert-deftest ytr-org-import-attachment-replaces-markdown-link ()
  (ytr-org-test-with-buffer "[Screenshot](test.png)" (list ytr-org-test-attachment)
                            (should (string= (buffer-string)
                                             (ytr-org-test-link "/api/files/123" "test.png" "Screenshot")))))

(ert-deftest ytr-org-import-attachment-replaces-multiple-occurrences ()
  (ytr-org-test-with-buffer "[[file:test.png]]\n\n![Bild](test.png)\n\n[test](test.png)"
                            (list ytr-org-test-attachment)
                            (should (string= (buffer-string)
                                             (concat (ytr-org-test-link "/api/files/123" "test.png")
                                                     "\n\n"
                                                     (ytr-org-test-link "/api/files/123" "test.png" "Bild")
                                                     "\n\n"
                                                     (ytr-org-test-link "/api/files/123" "test.png" "test"))))))

(ert-deftest ytr-org-import-attachment-replaces-multiple-attachments ()
  (ytr-org-test-with-buffer "[[file:a.png]]\n[[file:b.pdf][PDF]]"
                            '(((name . "a.png") (url . "/api/files/a"))
                              ((name . "b.pdf") (url . "/api/files/b")))
                            (should (string= (buffer-string)
                                             (concat (ytr-org-test-link "/api/files/a" "a.png")
                                                     "\n"
                                                     (ytr-org-test-link "/api/files/b" "b.pdf" "PDF"))))))

(ert-deftest ytr-org-import-attachment-keeps-unmatched-links ()
  (let ((content "[[file:other.png]]\n[Other](other.png)"))
    (ytr-org-test-with-buffer content (list ytr-org-test-attachment)
                              (should (string= (buffer-string) content)))))

(ert-deftest ytr-org-import-attachment-handles-regexp-special-chars-in-name ()
  (ytr-org-test-with-buffer "[[file:a+b (1).png]]"
                            '(((name . "a+b (1).png") (url . "/api/files/special")))
                            (should (string= (buffer-string)
                                             (ytr-org-test-link "/api/files/special" "a+b (1).png")))))

(ert-deftest ytr-org-import-attachment-does-not-confuse-prefix-names ()
  (ytr-org-test-with-buffer "[[file:test.png]]\n[[file:test.png.bak]]"
                            '(((name . "test.png") (url . "/api/files/test"))
                              ((name . "test.png.bak") (url . "/api/files/bak")))
                            (should (string= (buffer-string)
                                             (concat (ytr-org-test-link "/api/files/test" "test.png")
                                                     "\n"
                                                     (ytr-org-test-link "/api/files/bak" "test.png.bak"))))))

(ert-deftest ytr-org-import-attachment-empty-attachment-list-does-nothing ()
  (let ((content "[[file:test.png]]\n![Screenshot](test.png)\n[Screenshot](test.png)"))
    (ytr-org-test-with-buffer content nil
                              (should (string= (buffer-string) content)))))

(ert-deftest ytr-org-import-attachment-preserves-point ()
  (let ((ytr-baseurl ytr-org-test-baseurl))
    (with-temp-buffer
      (insert "abc\n[[file:test.png]]\ndef")
      (goto-char 5)
      (let ((point-before (point)))
        (ytr-org-perform-import-to-org-attachment-replacements
         (list ytr-org-test-attachment))
        (should (= (point) point-before))))))

(provide 'ytr-org-test)
;;; ytr-org-test.el ends here
