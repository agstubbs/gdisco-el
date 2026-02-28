;;; gdisco.el --- Emacs Lisp bindings for Google APIs via Discovery  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Andrew Stubbs

;; Author: Andrew Stubbs <andy@andystubbs.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (oauth2 "0.16") (s "1.12.0"))
;; Keywords: comm, tools
;; URL: https://github.com/drstubbsy/gdisco-el

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

;; Emacs Lisp bindings for Google APIs using the Discovery API.
;; Rather than hand-coding wrappers for each endpoint, gdisco-el
;; fetches API specifications at runtime and constructs requests
;; from the spec's metadata.
;;
;; See documentation on https://github.com/drstubbsy/gdisco-el

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'oauth2)
(require 's)
(require 'url)

;;;; Customization

(defgroup gdisco nil
  "Google API bindings via the Discovery API."
  :group 'comm
  :prefix "gdisco-")

(defcustom gdisco-cache-directory
  (expand-file-name "gdisco" user-emacs-directory)
  "Directory for caching Discovery spec JSON files."
  :type 'directory
  :group 'gdisco)

(defcustom gdisco-cache-ttl-seconds (* 7 24 60 60)
  "Time-to-live for cached Discovery specs, in seconds.
Default is 7 days.  Set to 0 to disable file caching."
  :type 'integer
  :group 'gdisco)

;;;; Client struct

(cl-defstruct (gdisco-client (:constructor gdisco-client--create))
  "A Google API client backed by a Discovery spec."
  api-name
  version
  token
  spec
  base-url)

;;;; Spec cache (in-memory + file-based)

(defvar gdisco--spec-cache (make-hash-table :test #'equal)
  "In-memory cache of Discovery specs, keyed by (API-NAME . VERSION).")

(defun gdisco--cache-file (api-name version)
  "Return the file path for the cached spec of API-NAME VERSION."
  (expand-file-name (format "%s-%s.json" api-name version)
                    gdisco-cache-directory))

(defun gdisco--cache-file-valid-p (file)
  "Return non-nil if cache FILE exists and is within TTL."
  (and (> gdisco-cache-ttl-seconds 0)
       (file-exists-p file)
       (let ((age (float-time
                   (time-subtract (current-time)
                                  (file-attribute-modification-time
                                   (file-attributes file))))))
         (< age gdisco-cache-ttl-seconds))))

(defun gdisco--cache-write (api-name version spec)
  "Write SPEC to the file cache for API-NAME VERSION."
  (when (> gdisco-cache-ttl-seconds 0)
    (let ((file (gdisco--cache-file api-name version)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (json-encode spec))))))

(defun gdisco--cache-read (api-name version)
  "Read the cached spec for API-NAME VERSION from disk, or nil."
  (let ((file (gdisco--cache-file api-name version)))
    (when (gdisco--cache-file-valid-p file)
      (json-read-file file))))

(defun gdisco--fetch-spec (api-name version)
  "Fetch the Discovery spec for API-NAME VERSION from Google.
Returns the parsed JSON as an alist."
  (let ((url (format "https://www.googleapis.com/discovery/v1/apis/%s/%s/rest"
                     api-name version)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (prog1 (let ((json-object-type 'alist)
                   (json-array-type 'vector))
               (json-read))
        (kill-buffer)))))

(defun gdisco--get-spec (api-name version)
  "Get the Discovery spec for API-NAME VERSION.
Checks in-memory cache, then file cache, then fetches from network.
Populates both caches on a network fetch."
  (let ((key (cons api-name version)))
    (or (gethash key gdisco--spec-cache)
        (let ((spec (or (gdisco--cache-read api-name version)
                        (let ((fetched (gdisco--fetch-spec api-name version)))
                          (gdisco--cache-write api-name version fetched)
                          fetched))))
          (puthash key spec gdisco--spec-cache)
          spec))))

;;;; Client constructor

(defun gdisco-client (api-name version &rest args)
  "Create a client for Google API API-NAME at VERSION.
ARGS is a plist accepting :token (an oauth2 token object).

Example:
  (gdisco-client \"gmail\" \"v1\" :token my-token)"
  (let* ((token (plist-get args :token))
         (spec (gdisco--get-spec api-name version))
         (base-url (alist-get 'baseUrl spec)))
    (gdisco-client--create
     :api-name api-name
     :version version
     :token token
     :spec spec
     :base-url base-url)))

;;;; Transport abstraction

(defun gdisco--request-sync (verb url body token)
  "Make a synchronous HTTP request.
VERB is an HTTP method string (\"GET\", \"POST\", etc.).
URL is the full request URL.
BODY is a string or nil for the request body.
TOKEN is an oauth2 token object, or nil for unauthenticated requests.
Returns the parsed JSON response as an alist."
  (let ((url-request-method verb)
        (url-request-extra-headers
         (when body '(("Content-Type" . "application/json"))))
        (url-request-data body))
    (with-current-buffer
        (if token
            (oauth2-url-retrieve-synchronously token url)
          (url-retrieve-synchronously url t))
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t)
        ;; point is now at body start
        )
      (prog1 (let ((json-object-type 'alist)
                   (json-array-type 'vector))
               (condition-case nil
                   (json-read)
                 (json-error nil)))
        (kill-buffer)))))

(defun gdisco--request-async (verb url body token callback &optional errback)
  "Make an asynchronous HTTP request.
VERB is an HTTP method string.  URL is the full request URL.
BODY is a string or nil.  TOKEN is an oauth2 token object or nil.
CALLBACK receives the parsed JSON alist on success.
ERRBACK receives an error alist on failure (optional)."
  (let ((url-request-method verb)
        (url-request-extra-headers
         (when body '(("Content-Type" . "application/json"))))
        (url-request-data body)
        (handler (lambda (status)
                   (goto-char (point-min))
                   (when (re-search-forward "\r?\n\r?\n" nil t)
                     ;; point is now at body start
                     )
                   (let ((result (let ((json-object-type 'alist)
                                      (json-array-type 'vector))
                                  (condition-case nil
                                      (json-read)
                                    (json-error nil)))))
                     (kill-buffer)
                     (if (or (plist-get status :error)
                             (and result (alist-get 'error result)))
                         (when errback (funcall errback (or result status)))
                       (funcall callback result))))))
    (if token
        (oauth2-url-retrieve token url handler)
      (url-retrieve url handler))))

;;;; Method resolution

(defun gdisco--resolve-method-path (operation)
  "Convert dotted OPERATION name to a spec traversal path.
E.g. \"users.messages.get\" → (resources users resources messages methods get)"
  (let ((parts (split-string operation "\\.")))
    (gdisco--resolve-method-path-from-parts parts)))

(defun gdisco--resolve-method-path-from-parts (parts)
  "Convert a list of PARTS to a spec traversal path."
  (cond
   ((null parts) nil)
   ((null (cdr parts))
    (list 'methods (intern (car parts))))
   (t
    (append (list 'resources (intern (car parts)))
            (gdisco--resolve-method-path-from-parts (cdr parts))))))

(defun gdisco--resolve-method (operation spec)
  "Look up OPERATION in SPEC, returning the method node.
OPERATION is a dotted string like \"users.messages.get\".
Returns the alist for that method, or nil if not found."
  (let ((path (gdisco--resolve-method-path operation)))
    (gdisco--traverse-spec path spec)))

(defun gdisco--traverse-spec (path alist)
  "Walk PATH through nested ALIST, returning the final node."
  (if (null (cdr path))
      (alist-get (car path) alist)
    (gdisco--traverse-spec (cdr path) (alist-get (car path) alist))))

;;;; Parameter handling

(defvar gdisco--control-keywords '(:then :else :body :gdisco-pages :gdisco-signal)
  "Keywords reserved for gdisco control parameters (not sent to the API).")

(defun gdisco--extract-control-params (params)
  "Extract gdisco control parameters from PARAMS plist.
Returns a plist of only control keywords (:then, :else, :body,
:gdisco-pages, :gdisco-signal)."
  (let (result)
    (while params
      (let ((key (car params))
            (val (cadr params)))
        (when (memq key gdisco--control-keywords)
          (setq result (plist-put result key val))))
      (setq params (cddr params)))
    result))

(defun gdisco--extract-api-params (params)
  "Extract API parameters from PARAMS plist.
Returns a plist with control keywords removed."
  (let (result)
    (while params
      (let ((key (car params))
            (val (cadr params)))
        (unless (memq key gdisco--control-keywords)
          (setq result (plist-put result key val))))
      (setq params (cddr params)))
    result))

;;;; URL building

(defun gdisco--method-parameters (operation spec)
  "Get the combined parameter spec for OPERATION in SPEC.
Merges API-level parameters with method-level parameters."
  (let ((api-params (alist-get 'parameters spec))
        (method-node (gdisco--resolve-method operation spec)))
    (append api-params (alist-get 'parameters method-node))))

(defun gdisco--interpolate-path (path-template api-params)
  "Replace {param} placeholders in PATH-TEMPLATE with values from API-PARAMS plist."
  (let ((result path-template))
    (while (string-match "{\\+?\\([^}]+\\)}" result)
      (let* ((param-name (match-string 1 result))
             (keyword (intern (concat ":" param-name)))
             (value (plist-get api-params keyword)))
        (setq result (replace-match (or (format "%s" value) "") t t result))))
    result))

(defun gdisco--build-query-string (param-spec api-params)
  "Build a URL query string from API-PARAMS filtered by PARAM-SPEC.
Only includes parameters whose location is \"query\" in the spec."
  (let (pairs)
    (dolist (spec-entry param-spec)
      (let* ((param-name (symbol-name (car spec-entry)))
             (meta (cdr spec-entry))
             (location (alist-get 'location meta))
             (keyword (intern (concat ":" param-name)))
             (value (plist-get api-params keyword)))
        (when (and value (equal location "query"))
          (if (listp value)
              (dolist (v value)
                (push (concat (url-hexify-string param-name) "="
                              (url-hexify-string (format "%s" v)))
                      pairs))
            (push (concat (url-hexify-string param-name) "="
                          (url-hexify-string (format "%s" value)))
                  pairs)))))
    (when pairs
      (mapconcat #'identity (nreverse pairs) "&"))))

(defun gdisco--build-url (client operation api-params)
  "Build the full request URL for OPERATION on CLIENT with API-PARAMS."
  (let* ((spec (gdisco-client-spec client))
         (base-url (gdisco-client-base-url client))
         (method-node (gdisco--resolve-method operation spec))
         (path-template (alist-get 'path method-node))
         (param-spec (gdisco--method-parameters operation spec))
         (path (gdisco--interpolate-path path-template api-params))
         (query (gdisco--build-query-string param-spec api-params)))
    (concat base-url path (when query (concat "?" query)))))

;;;; Error predicates

(defun gdisco-error-p (result)
  "Return non-nil if RESULT is a gdisco error alist."
  (and (consp result)
       (alist-get 'gdisco-error result)))

(defun gdisco-error-code (result)
  "Return the HTTP status code from an error RESULT."
  (alist-get 'code result))

(defun gdisco-error-message (result)
  "Return the error message string from an error RESULT."
  (alist-get 'message result))

;;;; Legacy API (preserved for backward compatibility)

(defun gdisco-get-api (api version &rest args)
  "Fetch the Discovery JSON spec for API at VERSION.
Returns the parsed spec as an alist.

Deprecated: use `gdisco-client' instead."
  (ignore args)
  (gdisco--get-spec api version))

(defun gdisco-get-path-to-method-node (method)
  "Convert dotted METHOD name to a spec traversal path.
E.g. \"users.messages.get\" → (resources users resources messages methods get)

Deprecated: use `gdisco--resolve-method-path' instead."
  (gdisco--resolve-method-path method))

(defun gdisco-get-path-to-parameters-node (method)
  "Return the spec path to METHOD's parameters node.

Deprecated: use `gdisco--method-parameters' instead."
  (append (gdisco--resolve-method-path method) (list 'parameters)))

(defun gdisco-traverse-path-to-api-node (path api)
  "Walk PATH through nested API alist, returning the final node.

Deprecated: use `gdisco--traverse-spec' instead."
  (gdisco--traverse-spec path api))

(defun gdisco-describe-method-parameters (method api)
  "Return the merged parameter alist for METHOD in API spec.

Deprecated: use `gdisco--method-parameters' instead."
  (append (alist-get 'parameters api)
          (gdisco--traverse-spec
           (append (gdisco--resolve-method-path method) '(parameters))
           api)))

(defun gdisco-get-method-path-template-string (method)
  "Return the spec path to METHOD's URL path template.

Deprecated."
  (append (gdisco--resolve-method-path method) (list 'path)))

(defun gdisco-get-parametrized-method-path (api method params)
  "Interpolate path parameters in METHOD's URL template from API spec.
PARAMS is a plist of parameter values.

Deprecated: use `gdisco--interpolate-path' instead."
  (let ((url-template (s-replace "{" "${" (gdisco--traverse-spec
                                           (gdisco-get-method-path-template-string method)
                                           api))))
    (s-format url-template (lambda (pname) (plist-get params (intern (concat ":" pname)))))))

(defun gdisco-get-method-arguments-for-path-or-query (where param-spec params)
  "Filter PARAM-SPEC entries by location WHERE, matching against PARAMS plist.

Deprecated."
  (if (null param-spec) nil
    (let* ((this (if (eq where (intern (alist-get 'location (cdar param-spec))))
                     (intern (concat ":" (symbol-name (caar param-spec))))
                   nil))
           (value (plist-get params this))
           (next (gdisco-get-method-arguments-for-path-or-query where (cdr param-spec) params)))
      (if (or (null this) (null value)) next
        (cons (cons this value) next)))))

(defun gdisco-get-api-base-url (api)
  "Return the base URL from API spec.

Deprecated."
  (alist-get 'baseUrl api))

(defun gdisco-get-query-string (api method params)
  "Build a query string for METHOD on API from PARAMS plist.

Deprecated: use `gdisco--build-query-string' instead."
  (mapconcat (lambda (x)
               (if (listp (cdr x))
                   (mapconcat (lambda (y) (concat (substring (symbol-name (car x)) 1) "=" y)) (cdr x) "&")
                 (concat (substring (symbol-name (car x)) 1) "=" (cdr x))))
             (gdisco-get-method-arguments-for-path-or-query 'query (gdisco-describe-method-parameters method api) params) "&"))

(defun gdisco-do-get (token api method params)
  "Execute a GET request for METHOD on API with PARAMS.
TOKEN is an oauth2 token object.
API is a parsed Discovery spec alist.
PARAMS is a plist of parameter values.

Deprecated: use `gdisco-invoke' (Phase 1) instead."
  (let* ((q (gdisco-get-query-string api method params))
         (p (gdisco-get-parametrized-method-path api method params)))
    (when (> (length q) 0)
      (setq p (format "%s?%s" p q)))
    (setq p (format "%s%s" (gdisco-get-api-base-url api) p))
    (gdisco--request-sync "GET" p nil token)))

(provide 'gdisco)
;;; gdisco.el ends here
