;;; gdisco-test.el --- Tests for gdisco.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for gdisco-el Phase 0 internals.
;; Run with: emacs -Q --batch -L . -l test/gdisco-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gdisco)

;;;; Test helpers

(defvar gdisco-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test fixture files.")

(defun gdisco-test--load-fixture (name)
  "Load JSON fixture NAME from the fixtures directory."
  (let ((path (expand-file-name name gdisco-test--fixtures-dir)))
    (json-read-file path)))

(defvar gdisco-test--gmail-spec
  (gdisco-test--load-fixture "gmail-v1-discovery.json")
  "Frozen Gmail v1 Discovery spec for testing.")

(defun gdisco-test--make-fake-client ()
  "Create a gdisco-client backed by the fixture Gmail spec."
  (gdisco-client--create
   :api-name "gmail"
   :version "v1"
   :token nil
   :spec gdisco-test--gmail-spec
   :base-url (alist-get 'baseUrl gdisco-test--gmail-spec)))

;;;; Client struct tests

(ert-deftest gdisco-test-client-struct ()
  "gdisco-client struct creation and accessors."
  (let ((c (gdisco-test--make-fake-client)))
    (should (gdisco-client-p c))
    (should (equal "gmail" (gdisco-client-api-name c)))
    (should (equal "v1" (gdisco-client-version c)))
    (should (null (gdisco-client-token c)))
    (should (gdisco-client-spec c))
    (should (equal "https://gmail.googleapis.com/gmail/v1/"
                   (gdisco-client-base-url c)))))

(ert-deftest gdisco-test-client-p-rejects-non-clients ()
  "gdisco-client-p rejects non-client objects."
  (should-not (gdisco-client-p nil))
  (should-not (gdisco-client-p "gmail"))
  (should-not (gdisco-client-p '(:api-name "gmail"))))

;;;; Method resolution tests

(ert-deftest gdisco-test-resolve-method-path-single ()
  "Single-segment method name resolves correctly."
  (should (equal '(methods getProfile)
                 (gdisco--resolve-method-path "getProfile"))))

(ert-deftest gdisco-test-resolve-method-path-two-segments ()
  "Two-segment method name resolves correctly."
  (should (equal '(resources users methods getProfile)
                 (gdisco--resolve-method-path "users.getProfile"))))

(ert-deftest gdisco-test-resolve-method-path-three-segments ()
  "Three-segment method name resolves correctly."
  (should (equal '(resources users resources messages methods get)
                 (gdisco--resolve-method-path "users.messages.get"))))

(ert-deftest gdisco-test-resolve-method-returns-node ()
  "gdisco--resolve-method returns the method node from the spec."
  (let ((node (gdisco--resolve-method "users.messages.get"
                                       gdisco-test--gmail-spec)))
    (should node)
    (should (equal "GET" (alist-get 'httpMethod node)))
    (should (equal "Gets the specified message." (alist-get 'description node)))
    (should (equal "users/{userId}/messages/{id}" (alist-get 'path node)))))

(ert-deftest gdisco-test-resolve-method-post ()
  "Resolves a POST method and returns correct httpMethod."
  (let ((node (gdisco--resolve-method "users.messages.send"
                                       gdisco-test--gmail-spec)))
    (should node)
    (should (equal "POST" (alist-get 'httpMethod node)))))

(ert-deftest gdisco-test-resolve-method-delete ()
  "Resolves a DELETE method."
  (let ((node (gdisco--resolve-method "users.messages.delete"
                                       gdisco-test--gmail-spec)))
    (should node)
    (should (equal "DELETE" (alist-get 'httpMethod node)))))

(ert-deftest gdisco-test-resolve-method-returns-nil-for-missing ()
  "gdisco--resolve-method returns nil for a nonexistent operation."
  (should (null (gdisco--resolve-method "users.bogus.nope"
                                         gdisco-test--gmail-spec))))

(ert-deftest gdisco-test-resolve-method-toplevel ()
  "Resolves a method directly under a resource (not nested)."
  (let ((node (gdisco--resolve-method "users.getProfile"
                                       gdisco-test--gmail-spec)))
    (should node)
    (should (equal "GET" (alist-get 'httpMethod node)))))

;;;; Parameter extraction tests

(ert-deftest gdisco-test-extract-control-params ()
  "Extracts only control keywords from a mixed param list."
  (let ((result (gdisco--extract-control-params
                 '(:userId "me" :then my-callback :format "full"
                   :gdisco-pages 3 :else my-errback))))
    (should (equal 'my-callback (plist-get result :then)))
    (should (equal 'my-errback (plist-get result :else)))
    (should (equal 3 (plist-get result :gdisco-pages)))
    (should (null (plist-get result :userId)))
    (should (null (plist-get result :format)))))

(ert-deftest gdisco-test-extract-api-params ()
  "Extracts only API keywords, stripping control keywords."
  (let ((result (gdisco--extract-api-params
                 '(:userId "me" :then my-callback :format "full"
                   :gdisco-pages 3))))
    (should (equal "me" (plist-get result :userId)))
    (should (equal "full" (plist-get result :format)))
    (should (null (plist-get result :then)))
    (should (null (plist-get result :gdisco-pages)))))

(ert-deftest gdisco-test-extract-empty-params ()
  "Empty param list returns nil for both extractors."
  (should (null (gdisco--extract-control-params nil)))
  (should (null (gdisco--extract-api-params nil))))

(ert-deftest gdisco-test-extract-all-control ()
  "All-control param list returns nil from api extractor."
  (should (null (gdisco--extract-api-params '(:then cb :else ecb)))))

(ert-deftest gdisco-test-extract-all-api ()
  "All-api param list returns nil from control extractor."
  (should (null (gdisco--extract-control-params '(:userId "me" :format "full")))))

;;;; URL building tests

(ert-deftest gdisco-test-interpolate-path-single-param ()
  "Single path parameter interpolation."
  (should (equal "users/me/profile"
                 (gdisco--interpolate-path "users/{userId}/profile"
                                            '(:userId "me")))))

(ert-deftest gdisco-test-interpolate-path-multiple-params ()
  "Multiple path parameter interpolation."
  (should (equal "users/me/messages/abc123"
                 (gdisco--interpolate-path "users/{userId}/messages/{id}"
                                            '(:userId "me" :id "abc123")))))

(ert-deftest gdisco-test-interpolate-path-plus-prefix ()
  "Path parameters with {+param} syntax (used by some Google APIs)."
  (should (equal "users/me/resource"
                 (gdisco--interpolate-path "users/{+userId}/resource"
                                            '(:userId "me")))))

(ert-deftest gdisco-test-build-query-string-single ()
  "Single query parameter."
  (let* ((spec gdisco-test--gmail-spec)
         (param-spec (gdisco--method-parameters "users.messages.get" spec))
         (qs (gdisco--build-query-string param-spec '(:format "metadata"))))
    (should (equal "format=metadata" qs))))

(ert-deftest gdisco-test-build-query-string-multiple ()
  "Multiple query parameters."
  (let* ((spec gdisco-test--gmail-spec)
         (param-spec (gdisco--method-parameters "users.messages.list" spec))
         (qs (gdisco--build-query-string param-spec '(:userId "me" :maxResults 10 :q "from:alice"))))
    ;; Should contain maxResults and q but not userId (which is a path param)
    (should (string-match-p "maxResults=10" qs))
    (should (string-match-p "q=from" qs))
    (should-not (string-match-p "userId" qs))))

(ert-deftest gdisco-test-build-query-string-list-values ()
  "Repeated/list query parameter generates multiple key=value pairs."
  (let* ((spec gdisco-test--gmail-spec)
         (param-spec (gdisco--method-parameters "users.messages.list" spec))
         (qs (gdisco--build-query-string param-spec
                                          '(:userId "me" :labelIds ("INBOX" "UNREAD")))))
    (should (string-match-p "labelIds=INBOX" qs))
    (should (string-match-p "labelIds=UNREAD" qs))))

(ert-deftest gdisco-test-build-query-string-no-query-params ()
  "No query parameters returns nil."
  (let* ((spec gdisco-test--gmail-spec)
         (param-spec (gdisco--method-parameters "users.messages.get" spec))
         (qs (gdisco--build-query-string param-spec '(:userId "me" :id "abc"))))
    (should (null qs))))

(ert-deftest gdisco-test-build-url-simple-get ()
  "Full URL for a simple GET operation."
  (let ((client (gdisco-test--make-fake-client)))
    (should (equal "https://gmail.googleapis.com/gmail/v1/users/me/messages/abc123"
                   (gdisco--build-url client "users.messages.get"
                                       '(:userId "me" :id "abc123"))))))

(ert-deftest gdisco-test-build-url-with-query ()
  "Full URL with query parameters."
  (let* ((client (gdisco-test--make-fake-client))
         (url (gdisco--build-url client "users.messages.get"
                                  '(:userId "me" :id "abc" :format "metadata"))))
    (should (string-match-p "^https://gmail\\.googleapis\\.com/gmail/v1/users/me/messages/abc" url))
    (should (string-match-p "format=metadata" url))))

(ert-deftest gdisco-test-build-url-list-operation ()
  "Full URL for a list operation with query params."
  (let* ((client (gdisco-test--make-fake-client))
         (url (gdisco--build-url client "users.messages.list"
                                  '(:userId "me" :maxResults 5))))
    (should (string-match-p "users/me/messages\\?" url))
    (should (string-match-p "maxResults=5" url))))

;;;; Method parameters tests

(ert-deftest gdisco-test-method-parameters-merges ()
  "Method parameters include both API-level and method-level params."
  (let ((params (gdisco--method-parameters "users.messages.get"
                                            gdisco-test--gmail-spec)))
    ;; API-level params
    (should (assq 'alt params))
    (should (assq 'fields params))
    ;; Method-level params
    (should (assq 'userId params))
    (should (assq 'id params))
    (should (assq 'format params))))

;;;; Error predicate tests

(ert-deftest gdisco-test-error-p-positive ()
  "gdisco-error-p returns t for error alists."
  (should (gdisco-error-p '((gdisco-error . t) (code . 404)
                             (message . "Not found")))))

(ert-deftest gdisco-test-error-p-negative ()
  "gdisco-error-p returns nil for success alists."
  (should-not (gdisco-error-p '((messages . []) (resultSizeEstimate . 0))))
  (should-not (gdisco-error-p nil))
  (should-not (gdisco-error-p "string")))

(ert-deftest gdisco-test-error-accessors ()
  "Error code and message accessors."
  (let ((err '((gdisco-error . t) (code . 403) (message . "Forbidden"))))
    (should (= 403 (gdisco-error-code err)))
    (should (equal "Forbidden" (gdisco-error-message err)))))

;;;; Spec cache tests

(ert-deftest gdisco-test-spec-cache-in-memory ()
  "In-memory cache stores and retrieves specs."
  (let ((gdisco--spec-cache (make-hash-table :test #'equal)))
    (puthash (cons "test-api" "v1") '((name . "test")) gdisco--spec-cache)
    (should (equal '((name . "test"))
                   (gethash (cons "test-api" "v1") gdisco--spec-cache)))))

(ert-deftest gdisco-test-cache-file-path ()
  "Cache file path is constructed correctly."
  (let ((gdisco-cache-directory "/tmp/gdisco-test-cache"))
    (should (equal "/tmp/gdisco-test-cache/gmail-v1.json"
                   (gdisco--cache-file "gmail" "v1")))))

(ert-deftest gdisco-test-cache-file-valid-p-missing ()
  "Non-existent cache file is not valid."
  (should-not (gdisco--cache-file-valid-p "/tmp/nonexistent-gdisco-test-file.json")))

(ert-deftest gdisco-test-cache-file-valid-p-disabled ()
  "File cache is invalid when TTL is 0."
  (let ((gdisco-cache-ttl-seconds 0))
    (should-not (gdisco--cache-file-valid-p
                 (expand-file-name "gmail-v1-discovery.json"
                                   gdisco-test--fixtures-dir)))))

(ert-deftest gdisco-test-cache-write-and-read ()
  "Write to file cache and read it back."
  (let* ((gdisco-cache-directory (make-temp-file "gdisco-test-" t))
         (spec '((name . "test") (version . "v1"))))
    (unwind-protect
        (progn
          (gdisco--cache-write "test" "v1" spec)
          (let ((read-back (gdisco--cache-read "test" "v1")))
            (should read-back)
            (should (equal "test" (alist-get 'name read-back)))))
      (delete-directory gdisco-cache-directory t))))

;;;; Integration: gdisco--request-sync mock test

(ert-deftest gdisco-test-request-sync-mock ()
  "gdisco--request-sync can be mocked with cl-letf."
  (cl-letf (((symbol-function 'gdisco--request-sync)
             (lambda (_verb _url _body _token)
               '((messages . [((id . "msg1"))]) (resultSizeEstimate . 1)))))
    (let ((result (gdisco--request-sync "GET" "https://example.com" nil nil)))
      (should (assq 'messages result))
      (should (= 1 (alist-get 'resultSizeEstimate result))))))

;;;; Integration: full pipeline test (mocked HTTP)

(ert-deftest gdisco-test-full-pipeline ()
  "Full pipeline: client + resolve + build-url + mock request."
  (let ((client (gdisco-test--make-fake-client)))
    (cl-letf (((symbol-function 'gdisco--request-sync)
               (lambda (verb url _body _token)
                 ;; Verify the pipeline constructed the right verb and URL
                 (should (equal "GET" verb))
                 (should (string-match-p "users/me/messages" url))
                 (should (string-match-p "maxResults=5" url))
                 '((messages . [((id . "msg1"))]) (resultSizeEstimate . 1)))))
      (let* ((operation "users.messages.list")
             (spec (gdisco-client-spec client))
             (method-node (gdisco--resolve-method operation spec))
             (verb (alist-get 'httpMethod method-node))
             (api-params '(:userId "me" :maxResults 5))
             (url (gdisco--build-url client operation api-params))
             (result (gdisco--request-sync verb url nil (gdisco-client-token client))))
        (should (equal "GET" verb))
        (should (assq 'messages result))))))

;;;; Legacy compatibility tests

(ert-deftest gdisco-test-legacy-get-path-to-method-node ()
  "Legacy function produces same result as new internal."
  (should (equal (gdisco--resolve-method-path "users.messages.get")
                 (gdisco-get-path-to-method-node "users.messages.get"))))

(ert-deftest gdisco-test-legacy-traverse-path ()
  "Legacy traverse function works via new internal."
  (let ((base-url (gdisco-traverse-path-to-api-node '(baseUrl) gdisco-test--gmail-spec)))
    (should (equal "https://gmail.googleapis.com/gmail/v1/" base-url))))

(ert-deftest gdisco-test-legacy-describe-method-parameters ()
  "Legacy describe-method-parameters returns merged params."
  (let ((params (gdisco-describe-method-parameters "users.messages.get"
                                                    gdisco-test--gmail-spec)))
    (should (assq 'userId params))
    (should (assq 'id params))
    (should (assq 'format params))
    (should (assq 'alt params))))

;;; gdisco-test.el ends here
