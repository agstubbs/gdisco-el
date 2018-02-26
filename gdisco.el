;;; gdisco.el --- Emacs LISP Interface to Google's various APIs

;; Copyright (C) 2018 Andrew Stubbs

;; Author: Andrew Stubbs <andy@andystubbs.com>

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

;; Emacs LISP Interface to Google's various APIs
;;
;; See documentation on https://github.com/drstubbsy/gdisco-el

;;; Code:
(require 'json)
(require 'oauth2)
(require 's)

(defun gdisco-get-api (api version &rest args)
  ""
  (save-current-buffer
    (let*((databuf (url-retrieve-synchronously
                    (format "https://www.googleapis.com/discovery/v1/apis/%s/%s/rest" api version))))
      (set-buffer databuf)
      (goto-char url-http-end-of-headers)
      (let* ((reply-string (buffer-substring-no-properties (point) (point-max)))
             (reply-json (json-read-from-string reply-string)))
        (kill-buffer databuf)
        reply-json)))
  )

;; (setq gmail-api (gdisco-get-api "gmail" "v1"))
;; (setq adminsdk-api (gdisco-get-api "admin" "directory_v1"))

(defun gdisco-get-path-to-method-node (method)
  (if (stringp method)
      (gdisco-get-path-to-method-node (split-string method "[.]"))
    (if (null method)
        nil
      (if (null (cdr method))
          (list 'methods (intern (car method)))
        (append (list 'resources (intern (car method))) (gdisco-get-path-to-method-node (cdr method)))
        )
      )
    )
  )
(defun gdisco-get-path-to-parameters-node (method)
  (append (gdisco-get-path-to-method-node method) (list 'parameters)))

;; (gdisco-get-path-to-method-node "users.messages.get")
;; (gdisco-get-path-to-parameters-node "users.messages.get")

(defun gdisco-traverse-path-to-api-node (path api)
  (if (eq 1 (length path))
      (alist-get  (car path) api)
    (gdisco-traverse-path-to-api-node (cdr path) (alist-get (car path) api))))

(defun gdisco-describe-method-parameters (method api)
  (append (gdisco-traverse-path-to-api-node '(parameters) api)
          (gdisco-traverse-path-to-api-node (gdisco-get-path-to-parameters-node method) api))
  )

;; (gdisco-traverse-path-to-api-node (gdisco-get-path-to-parameters-node "users.messages.get") gmail-api)
;; (gdisco-describe-method-parameters "users.messages.get" gmail-api)

(defun gdisco-get-method-path-template-string (method)
  (append (gdisco-get-path-to-method-node method) (list 'path)))

;; (gdisco-traverse-path-to-api-node (gdisco-get-method-path-template-string "users.messages.get") gmail-api)

(defun gdisco-get-parametrized-method-path (api method params)
  (let ((url-template (s-replace "{" "${" (gdisco-traverse-path-to-api-node (gdisco-get-method-path-template-string method) api))))
    (s-format url-template (lambda (pname) (plist-get params (intern (concat ":" pname))))))
)

;; (gdisco-get-parametrized-method-path gmail-api "users.messages.get" '(:id "test" :userId "something"))

;; (defun gdisco-get-api-params-recurse (where param-spec)
;;   ""
;;   (if (null param-spec) nil
;;     (let ((this (if (eq where (intern (alist-get 'location (cdar param-spec))))
;;                     (intern (concat ":" (symbol-name (caar param-spec))))
;;                   nil))
;;           (next (get-api-params-recurse where (cdr param-spec))))
;;       (if (null this) next
;;         (cons this next)
;;     ))))
;; (get-api-params-recurse 'query (gdisco-describe-method-parameters "users.messages.get" gmail-api))

(defun gdisco-get-method-arguments-for-path-or-query (where param-spec params)
  ""
  (if (null param-spec) nil
    (let* ((this (if (eq where (intern (alist-get 'location (cdar param-spec))))
                    (intern (concat ":" (symbol-name (caar param-spec))))
                  nil))
          (value (plist-get params this))
          (next (gdisco-get-method-arguments-for-path-or-query where (cdr param-spec) params)))
      (if (or (null this) (null value)) next
        (cons (cons this value) next)
        ))))

;; (gdisco-get-method-arguments-for-path-or-query 'path (gdisco-describe-method-parameters "users.messages.get" gmail-api) '(:id test :tester "something" :format "blah" :userId "hohoho"))

(defun gdisco-get-api-base-url (api)
  (gdisco-traverse-path-to-api-node '(baseUrl) api)
)

;; (gdisco-get-api-base-url gmail-api)

(defun gdisco-get-query-string (api method params)
  (mapconcat (lambda (x)
               (if (listp (cdr x))
                   (mapconcat (lambda (y) (concat (substring (symbol-name (car x)) 1) "=" y)) (cdr x) "&")
                 (concat (substring (symbol-name (car x)) 1) "=" (cdr x))))
             (gdisco-get-method-arguments-for-path-or-query 'query (gdisco-describe-method-parameters method api) params) "&")
  )

;; (gdisco-get-method-arguments-for-path-or-query 'query (gdisco-describe-method-parameters "users.messages.get" gmail-api) (list :format "metadata" :metadataHeaders (list "From" "Subject")))
;; (gdisco-get-query-string gmail-api "users.messages.get" '(:id test :tester "something" :format "blah" :alt ("jeff" "sue") :userId "hohoho"))
;; (gdisco-get-query-string gmail-api "users.messages.get" (list :format "blah" :alt (list "a" "b" "c")))

(defun gdisco-do-get (token api method params)
  ""
  (let* ((q (gdisco-get-query-string api method params))
         (p (gdisco-get-parametrized-method-path api method params))
         )
    (if (> (length q) 0)
        (setq p (format "%s?%s" p q)))
    (setq p (format "%s%s" (gdisco-get-api-base-url api) p))
    (message p)
    (let*((databuf (oauth2-url-retrieve-synchronously token p)))
      (save-current-buffer
        (set-buffer databuf)
        (let* (;;(json-object-type 'hash-table)
               ;;(json-array-type 'list)
               ;;(json-key-type 'string)
               (reply-string (buffer-substring-no-properties (point) (point-max)))
               (reply-json (json-read-from-string reply-string))
               )
          (kill-buffer databuf)
          reply-json))))
  )

;; (gdisco-do-get mytoken gmail-api "users.messages.get" '(:id "......" :userId "me" :format "metadata"))
;; (gdisco-do-get mytoken gmail-api "users.messages.get" '(:id "......" :userId "me" :format "metadata" :metadataHeaders ("From" "Subject" "Return-Path")))
;; (gdisco-do-get mytoken gmail-api "users.messages.list" '(:userId "me"))
