;;; forgejo-test-api.el --- Tests for forgejo-api  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for URL building, header parsing, and response parsing.

;;; Code:

(require 'ert)
(require 'cl-lib)

(setq forgejo-markdown-mode 'text-mode)
(require 'forgejo)
(require 'forgejo-api)

;;; Group 1: URL building

(ert-deftest forgejo-test-api-url-no-params ()
  "Build URL without query parameters."
  (should (string= (forgejo-api--url "https://codeberg.org"
                                      "repos/owner/repo/issues")
                   "https://codeberg.org/api/v1/repos/owner/repo/issues")))

(ert-deftest forgejo-test-api-url-with-params ()
  "Build URL with query parameters."
  (should (string= (forgejo-api--url "https://codeberg.org"
                                      "repos/owner/repo/issues"
                                      '(("state" . "open") ("limit" . "30")))
                   "https://codeberg.org/api/v1/repos/owner/repo/issues?state=open&limit=30")))

(ert-deftest forgejo-test-api-url-trailing-slash ()
  "Host with trailing slash should not produce double slashes."
  (should (string-prefix-p "https://codeberg.org/api/v1/"
                           (forgejo-api--url "https://codeberg.org"
                                              "user/repos"))))

;;; Group 2: Header parsing

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

;;; Group 3: Response parsing

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

;;; Group 4: HTTP status detection

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

(ert-deftest forgejo-test-api-response-status-304 ()
  "Extract 304 status code for conditional requests."
  (with-temp-buffer
    (insert "HTTP/1.1 304 Not Modified\r\n\r\n")
    (should (= (forgejo-api--response-status (current-buffer)) 304))))

(ert-deftest forgejo-test-api-parse-headers-etag ()
  "Parse ETag and Last-Modified headers."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "ETag: \"abc123\"\r\n"
            "Last-Modified: Tue, 01 Apr 2026 12:00:00 GMT\r\n"
            "\r\n{}")
    (let ((headers (forgejo-api--parse-headers (current-buffer))))
      (should (string= (plist-get headers :etag) "\"abc123\""))
      (should (string-match-p "Tue" (plist-get headers :last-modified))))))

;;; Group 5: Auth

(ert-deftest forgejo-test-api-token-from-variable ()
  "Token falls back to `forgejo-token' when auth-source is disabled."
  (let ((forgejo-hosts '(("https://codeberg.org")))
        (forgejo-token-use-auth-source nil)
        (forgejo-token "test-token-123"))
    (should (string= (forgejo-token "https://codeberg.org") "test-token-123"))))

(ert-deftest forgejo-test-api-token-missing ()
  "Signal error when no token is available."
  (let ((forgejo-hosts '(("https://codeberg.org")))
        (forgejo-token-use-auth-source nil)
        (forgejo-token nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (should-error (forgejo-token "https://codeberg.org") :type 'user-error))))

;;; Group 6: Header parsing with rate limits

(ert-deftest forgejo-test-api-parse-headers-rate-limit ()
  "Parse rate limit headers from HTTP response."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "X-Total-Count: 10\r\n"
            "X-RateLimit-Remaining: 42\r\n"
            "X-RateLimit-Limit: 300\r\n"
            "X-RateLimit-Reset: 1700000000\r\n"
            "\r\n{}")
    (let ((headers (forgejo-api--parse-headers (current-buffer))))
      (should (= (plist-get headers :total-count) 10))
      (should (= (plist-get headers :rate-limit-remaining) 42))
      (should (= (plist-get headers :rate-limit-limit) 300))
      (should (= (plist-get headers :rate-limit-reset) 1700000000)))))

;;; Group 7: Rate limit helpers

(ert-deftest forgejo-test-api-format-reset-time-future ()
  "Format a future reset timestamp as relative minutes."
  (let ((future (+ (float-time) 300)))
    (should (string-match-p "in [0-9]+ min"
                            (forgejo-api--format-reset-time future)))))

(ert-deftest forgejo-test-api-format-reset-time-past ()
  "Format a past reset timestamp as now."
  (let ((past (- (float-time) 10)))
    (should (string= "now" (forgejo-api--format-reset-time past)))))

(ert-deftest forgejo-test-api-format-reset-time-nil ()
  "Format nil timestamp as unknown."
  (should (string= "unknown" (forgejo-api--format-reset-time nil))))

;;; Group 8: Error plist construction

(ert-deftest forgejo-test-api-error-plist ()
  "Build a structured error plist from components."
  (let ((plist (forgejo-api--error-plist 404 "GET" "repos/x/y" "Not Found" nil)))
    (should (= (plist-get plist :status) 404))
    (should (string= (plist-get plist :method) "GET"))
    (should (string= (plist-get plist :endpoint) "repos/x/y"))
    (should (string= (plist-get plist :message) "Not Found"))
    (should (null (plist-get plist :data)))))

(ert-deftest forgejo-test-api-error-plist-network ()
  "Error plist for network errors has nil status."
  (let ((plist (forgejo-api--error-plist nil "POST" "repos/x/y/issues"
                                         "connection refused" nil)))
    (should (null (plist-get plist :status)))
    (should (string= (plist-get plist :message) "connection refused"))))

;;; Group 9: Default limit

(ert-deftest forgejo-test-api-default-limit-cached ()
  "Return cached limit when available."
  (let ((forgejo--api-default-limit 50))
    (should (= (forgejo-api-default-limit) 50))))

(ert-deftest forgejo-test-api-default-limit-fallback ()
  "Return 50 when no cached limit."
  (let ((forgejo--api-default-limit nil))
    (should (= (forgejo-api-default-limit) 50))))

;;; Group 10: Response classification

(ert-deftest forgejo-test-api-classify-success ()
  "Classify a 200 response as success with parsed data and headers."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "X-Total-Count: 5\r\n"
            "\r\n"
            "{\"id\": 1}")
    (let ((result (forgejo-api--classify-response nil (current-buffer))))
      (should (eq (plist-get result :kind) 'success))
      (should (= (alist-get 'id (plist-get result :data)) 1))
      (should (= (plist-get (plist-get result :headers) :total-count) 5)))))

(ert-deftest forgejo-test-api-classify-http-error ()
  "Classify a 404 response as http-error with message from JSON body."
  (with-temp-buffer
    (insert "HTTP/1.1 404 Not Found\r\n"
            "\r\n"
            "{\"message\": \"repo not found\"}")
    (let ((result (forgejo-api--classify-response nil (current-buffer))))
      (should (eq (plist-get result :kind) 'http-error))
      (should (= (plist-get result :status) 404))
      (should (string= (plist-get result :message) "repo not found")))))

(ert-deftest forgejo-test-api-classify-http-error-bad-json ()
  "Classify a 500 with malformed body as http-error with nil message."
  (with-temp-buffer
    (insert "HTTP/1.1 500 Internal Server Error\r\n"
            "\r\n"
            "not json")
    (let ((result (forgejo-api--classify-response nil (current-buffer))))
      (should (eq (plist-get result :kind) 'http-error))
      (should (= (plist-get result :status) 500))
      (should (null (plist-get result :message))))))

(ert-deftest forgejo-test-api-classify-net-error ()
  "Classify a network error from url-retrieve status."
  (with-temp-buffer
    (insert "")
    (let ((result (forgejo-api--classify-response
                   '(:error (error connection-failed)) (current-buffer))))
      (should (eq (plist-get result :kind) 'net-error))
      (should (stringp (plist-get result :message))))))

(ert-deftest forgejo-test-api-classify-not-modified ()
  "Classify a 304 response as not-modified with headers."
  (with-temp-buffer
    (insert "HTTP/1.1 304 Not Modified\r\n"
            "ETag: \"abc\"\r\n"
            "\r\n")
    (let ((result (forgejo-api--classify-response nil (current-buffer))))
      (should (eq (plist-get result :kind) 'not-modified))
      (should (string= (plist-get (plist-get result :headers) :etag) "\"abc\"")))))

;;; Group 11: Error reporting

(ert-deftest forgejo-test-api-report-error-calls-callback ()
  "Error callback receives a structured error plist."
  (let (received)
    (forgejo-api--report-error
     "GET" "repos/x/y"
     '(:kind http-error :status 404 :message "not found" :data nil)
     (lambda (info) (setq received info)))
    (should (= (plist-get received :status) 404))
    (should (string= (plist-get received :message) "not found"))))

(ert-deftest forgejo-test-api-report-error-messages ()
  "Without callback, report-error logs to *Messages*."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (forgejo-api--report-error
       "GET" "repos/x/y"
       '(:kind http-error :status 404 :message "not found") nil)
      (forgejo-api--report-error
       "POST" "repos/x/y"
       '(:kind net-error :message "connection refused") nil)
      (forgejo-api--report-error
       "GET" "repos/x/y"
       '(:kind timeout) nil))
    (should (= (length messages) 3))
    (should (string-match-p "404" (nth 2 messages)))
    (should (string-match-p "connection refused" (nth 1 messages)))
    (should (string-match-p "timeout" (nth 0 messages)))))

(provide 'forgejo-test-api)
;;; forgejo-test-api.el ends here
