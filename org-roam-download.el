;;; org-roam-download.el --- Download files using org-protocol  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2025 Selwyn Simsek <selwyn.simsek@cantab.net>
;; Author: Selwyn Simsek <selwyn.simsek@cantab.net>
;; URL: https://github.com/selwynsimsek/org-roam-download
;; Keywords: org-mode, roam, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.3"))

;;; Commentary:

;; This package installs an org-protocol custom handler that lets one download a file, store it relative to org-roam-directory, and create an ~org-link~ to the local stored copy.

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
  ;; Mostly taken from https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types/Common_types
  '((".aac" . "audio/aac")
    (".abw" . "application/x-abiword")
    (".apng" . "image/apng")
    (".arc" . "application/x-freearc")
    (".avif" . "image/avif")
    (".avi" . "video/x-msvideo")
    (".azw" . "application/vnd.amazon.ebook")
    (".bin" . "application/octet-stream")
    (".bmp" . "image/bmp")
    (".bz" . "application/x-bzip")
    (".bz2" . "application/x-bzip2")
    (".cda" . "application/x-cdf")
    (".csh" . "application/x-csh")
    (".css" . "text/css")
    (".csv" . "text/csv")
    (".doc" . "application/msword")
    (".docx" . "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    (".eot" . "application/vnd.ms-fontobject")
    (".epub" . "application/epub+zip")
    (".gz" . "application/gzip")
    (".gif" . "image/gif")
    (".html" . "text/html")
    (".ico" . "image/vnd.microsoft.icon")
    (".ics" . "text/calendar")
    (".jar" . "application/java-archive")
    (".jpeg" . "image/jpeg")
    (".js" . "text/javascript")
    (".json" . "application/json")
    (".jsonld" . "application/ld+json")
    (".md" . "text/markdown")
    (".midi" . "audio/midi")
    (".midi" . "audio/x-midi")
    (".mjs" . "text/javascript")
    (".mp3" . "audio/mpeg")
    (".mp4" . "video/mp4")
    (".mpeg" . "video/mpeg")
    (".mpkg" . "application/vnd.apple.installer+xml")
    (".odp" . "application/vnd.oasis.opendocument.presentation")
    (".ods" . "application/vnd.oasis.opendocument.spreadsheet")
    (".odt" . "application/vnd.oasis.opendocument.text")
    (".oga" . "audio/ogg")
    (".ogv" . "video/ogg")
    (".ogx" . "application/ogg")
    (".opus" . "audio/ogg")
    (".otf" . "font/otf")
    (".png" . "image/png")
    (".pdf" . "application/pdf")
    (".php" . "application/x-httpd-php")
    (".ppt" . "application/vnd.ms-powerpoint")
    (".pptx" . "application/vnd.openxmlformats-officedocument.presentationml.presentation")
    (".rar" . "application/vnd.rar")
    (".rtf" . "application/rtf")
    (".sh" . "application/x-sh")
    (".svg" . "image/svg+xml")
    (".tar" . "application/x-tar")
    (".tif" . "image/tiff")
    (".ts" . "video/mp2t")
    (".ttf" . "font/ttf")
    (".txt" . "text/plain")
    (".vsd" . "application/vnd.visio")
    (".wav" . "audio/wav")
    (".weba" . "audio/webm")
    (".webm" . "video/webm")
    (".webp" . "image/webp")
    (".woff" . "font/woff")
    (".woff2" . "font/woff2")
    (".xhtml" . "application/xhtml+xml")
    (".xls" . "application/vnd.ms-excel")
    (".xlsx" . "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    (".xml" . "application/xml")
    (".xul" . "application/vnd.mozilla.xul+xml")
    (".zip" . "application/zip")
    (".3gp" . "video/3gpp")
    (".3g2" . "video/3gpp2")
    (".7z" . "application/x-7z-compressed")
    (".3gp" . "audio/3gpp")
    (".3g2" . "audio/3gpp2")
    (".xml" . "text/xml")
    (".xml" . "application/atom+xml")
    (".gz" . "application/x-gzip")
    (".tiff" . "image/tiff"))
  "Sensible defaults to match MIME types to extensions."
  :type '(plist :key-type string :value-type string)
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
