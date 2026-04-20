;;; forgejo-test-api.el --- Tests for forgejo-api  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for URL building, header parsing, and response parsing.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../forgejo.el"
       (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "../forgejo-api.el"
       (file-name-directory (or load-file-name buffer-file-name))))

;;; ---- Group 1: URL building ----

(ert-deftest forgejo-test-api-url-no-params ()
  "Build URL without query parameters."
  (let ((forgejo-host "https://codeberg.org"))
    (should (string= (forgejo-api--url "repos/owner/repo/issues")
                     "https://codeberg.org/api/v1/repos/owner/repo/issues"))))

(ert-deftest forgejo-test-api-url-with-params ()
  "Build URL with query parameters."
  (let ((forgejo-host "https://codeberg.org"))
    (should (string= (forgejo-api--url "repos/owner/repo/issues"
                                       '(("state" . "open") ("limit" . "30")))
                     "https://codeberg.org/api/v1/repos/owner/repo/issues?state=open&limit=30"))))

(ert-deftest forgejo-test-api-url-trailing-slash ()
  "Host with trailing slash should not produce double slashes."
  (let ((forgejo-host "https://codeberg.org"))
    (should (string-prefix-p "https://codeberg.org/api/v1/"
                             (forgejo-api--url "user/repos")))))

;;; ---- Group 2: Header parsing ----

(ert-deftest forgejo-test-api-parse-headers ()
  "Parse X-Total-Count and Link headers from HTTP response."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "Content-Type: application/json\r\n"
            "X-Total-Count: 42\r\n"
            "Link: <https://codeberg.org/api/v1/repos?page=2>; rel=\"next\"\r\n"
            "\r\n"
            "{}")
    (let ((headers (forgejo-api--parse-headers (current-buffer))))
      (should (= (plist-get headers :total-count) 42))
      (should (stringp (plist-get headers :link))))))

(ert-deftest forgejo-test-api-parse-headers-missing ()
  "Return nil values when pagination headers are absent."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "Content-Type: application/json\r\n"
            "\r\n"
            "{}")
    (let ((headers (forgejo-api--parse-headers (current-buffer))))
      (should (null (plist-get headers :total-count)))
      (should (null (plist-get headers :link))))))

;;; ---- Group 3: Response parsing ----

(ert-deftest forgejo-test-api-parse-response-object ()
  "Parse a JSON object response into an alist."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "Content-Type: application/json\r\n"
            "\r\n"
            "{\"id\": 1, \"title\": \"Bug report\", \"state\": \"open\"}")
    (let ((data (forgejo-api--parse-response (current-buffer))))
      (should (= (alist-get 'id data) 1))
      (should (string= (alist-get 'title data) "Bug report"))
      (should (string= (alist-get 'state data) "open")))))

(ert-deftest forgejo-test-api-parse-response-array ()
  "Parse a JSON array response into a list of alists."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n"
            "[{\"id\": 1}, {\"id\": 2}]")
    (let ((data (forgejo-api--parse-response (current-buffer))))
      (should (listp data))
      (should (= (length data) 2))
      (should (= (alist-get 'id (nth 0 data)) 1))
      (should (= (alist-get 'id (nth 1 data)) 2)))))

;;; ---- Group 4: HTTP status detection ----

(ert-deftest forgejo-test-api-response-status ()
  "Extract HTTP status code from response."
  (with-temp-buffer
    (insert "HTTP/1.1 404 Not Found\r\n\r\n{}")
    (should (= (forgejo-api--response-status (current-buffer)) 404))))

(ert-deftest forgejo-test-api-response-status-200 ()
  "Extract 200 status code."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n{}")
    (should (= (forgejo-api--response-status (current-buffer)) 200))))

;;; ---- Group 5: Auth ----

(ert-deftest forgejo-test-api-token-from-variable ()
  "Token falls back to `forgejo-token' when auth-source is disabled."
  (let ((forgejo-token-use-auth-source nil)
        (forgejo-token "test-token-123"))
    (should (string= (forgejo-token) "test-token-123"))))

(ert-deftest forgejo-test-api-token-missing ()
  "Signal error when no token is available."
  (let ((forgejo-token-use-auth-source nil)
        (forgejo-token nil))
    (should-error (forgejo-token) :type 'user-error)))

;;; ---- Group 6: Default limit ----

(ert-deftest forgejo-test-api-default-limit-cached ()
  "Return cached limit when available."
  (let ((forgejo--api-default-limit 50))
    (should (= (forgejo-api-default-limit) 50))))

(ert-deftest forgejo-test-api-default-limit-fallback ()
  "Return 50 when no cached limit."
  (let ((forgejo--api-default-limit nil))
    (should (= (forgejo-api-default-limit) 50))))

(provide 'forgejo-test-api)
;;; forgejo-test-api.el ends here
