;;; example.el --- Emacs LISP Interface to Google's various APIs

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

(defun get-oauth-token (client-secret scopes)
  ""
  (let* ((assoc-list (cdr (car (json-read-file client-secret))))
         (client-id (cdr (assoc 'client_id assoc-list)))
         (project-id (cdr (assoc 'project_id assoc-list)))
         (auth-uri (cdr (assoc 'auth_uri assoc-list)))
         (token-uri (cdr (assoc 'token_uri assoc-list)))
         (auth-provider-x509-cert-url (cdr (assoc 'auth_provider_x509_cert_url assoc-list)))
         (client-secret (cdr (assoc 'client_secret assoc-list)))
         (redirect-uris (cdr (assoc 'redirect_uris assoc-list)))
         )
    (oauth2-auth-and-store auth-uri token-uri scopes client-id client-secret)
    )
  )

(setq gmail-token (get-oauth-token "~/.credentials/emacs_client.json" "https://www.googleapis.com/auth/gmail.readonly"))
(setq admin-token (get-oauth-token "~/.credentials/emacs_client.json" "https://www.googleapis.com/auth/admin.directory.user.readonly"))

(setq gmail-api (gdisco-get-api "gmail" "v1"))
(setq adminsdk-api (gdisco-get-api "admin" "directory_v1"))

(gdisco-do-get gmail-token gmail-api "users.messages.list" '(:userId "me"))
(gdisco-do-get admin-token adminsdk-api "users.list" '(:customer "my_customer"))
