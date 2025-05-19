;;; org-roam-download.el --- Download files using org-protocol  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2025 Selwyn Simsek <selwyn.simsek@cantab.net>
;; Author: Selwyn Simsek <selwyn.simsek@cantab.net>
;; URL: https://github.com/selwynsimsek/org-roam-download
;; Keywords: org-mode, roam, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))

;;; Commentary:

;; This package installs an org-protocol custom handler that lets one download a file, store it relative to org-roam-directory, and create an ~org-link~ to the local stored copy.
;;
;;
;;

;;; Code:

(require 'org-protocol)
(require 'org-roam)
(require 'url)

(defgroup org-roam-download nil
  "Download files and store them relative to org-roam-directory using org-protocol."
  :group 'org-roam)

(defcustom org-roam-download-relative-directory "data"
  "The directory relative to org-roam-directory that files should be stored in."
  :type 'string
  :group 'org-roam-download)

(defcustom org-roam-download-create-relative-links t
  "If non-NIL, links are created relative to org-roam-directory.
If NIL, an absolute link is created instead."
  :type 'boolean
  :group 'org-roam-download)

(defcustom org-roam-download-x-focus-frame nil
  "If non-NIL, invoke (x-focus-frame nil) upon downloading a file."
  :type 'boolean
  :group 'org-roam-download)

(defcustom org-roam-download-mime-sensible-defaults
  '((".txt" . "text/plain")
    (".jpg" . "image/jpeg")) ;; TODO insert more here. take from https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types/Common_types ?
  "Sensible defaults to match MIME types to extensions."
  :type 'sexp
  :group 'org-roam-download)

(defun org-roam-download-alphanumeric-p (ch)
  "Return T if CH is alphanumeric."
  (cl-find ch "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"))

(defun org-roam-download-sanitise (title)
  "Sanitise the TITLE of some web page to contain only alphanumeric characters."
  (cl-remove-if-not #'org-roam-download-alphanumeric-p title))

(defun org-roam-download-content-type-from-url (url)
  "Perform a HEAD request to get the MIME type of the content at URL."
  (with-current-buffer
      (let ((url-request-method "HEAD"))
        (url-retrieve-synchronously url))
    (let ((string (buffer-string)))
      (string-match "Content\\-Type: \\([a-zA-Z0-9/\\+\\.\\-]*\\)" string) ; TODO test this
      (match-string 1 string))))

(defun org-roam-download-extension-from-mime (mime-type)
  "Attempt to infer a sensible file extension for the given MIME-TYPE."
  (or (cl-loop for (key . value) in (append org-roam-download-mime-sensible-defaults mailcap-mime-extensions)
               when (string= value mime-type)
               return key)
      ".unknown"))

(defun org-roam-download (info)
  "Download URL locally and add a link.

INFO is an alist containing additional information passed by the protocol URL.
It should contain the FILE key, pointing to the path of the file to open.

  Example protocol string:

org-protocol://roam-download?url=URL&title=TITLE"
  (let* ((url (plist-get info :url))
         (title (plist-get info :title))
         (current-time-string (number-to-string (time-convert (current-time) 'integer)))
         (mime-type (org-roam-download-content-type-from-url url))
         (extension (org-roam-download-extension-from-mime mime-type))
         (filename (concat current-time-string (org-roam-download-sanitise title) extension))
         (relative-path (concat "./" org-roam-download-relative-directory "/" filename))
         (absolute-path (concat org-roam-directory "/" org-roam-download-relative-directory "/" filename))
         (link-path (if org-roam-download-create-relative-links relative-path absolute-path)))
    (make-directory (concat org-roam-directory "/" org-roam-download-relative-directory) t)
    (url-copy-file url absolute-path)
    (when (boundp 'org-stored-links)
      (if (cl-search "image" mime-type)
          ;; if it's an image, don't add a description
          (push (list link-path) org-stored-links)
        (push (list link-path title) org-stored-links)))
    (kill-new link-path)
    (message "`%s' to insert new Org link, `%s' to insert %S"
             (substitute-command-keys "\\[org-insert-link]")
             (substitute-command-keys "\\[yank]")
             url)
    (when org-roam-download-x-focus-frame (x-focus-frame nil)))
  nil)

(push '("org-roam-download"  :protocol "roam-download" :function org-roam-download)
      org-protocol-protocol-alist)

(provide 'org-roam-download)
;;; org-roam-download.el ends here
