;;; duckduckgo.el --- Run DuckDuckGo search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://github.com/akirak/duckduckgo.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides `duckduckgo' command, which performs search using
;; DuckDuckGo. The command supports completion of bangs. See
;; <https://duckduckgo.com/bang> for details.

;;; Code:

(require 'cl-lib)

(declare-function utf7-decode "utf7")
(declare-function xml-parse-tag "xml")

(defgroup duckduckgo nil
  ""
  :group 'convenience
  :prefix "duckduckgo-")

(defconst duckduckgo-url "https://duckduckgo.com")

(defconst duckduckgo-bang-url "https://duckduckgo.com/bang_lite.html")

(defcustom duckduckgo-browse-url-function #'browse-url
  "Default function used to browse the result of DuckDuckGo."
  :type 'function
  :options '(choice browse-url
                    eww))

(defcustom duckduckgo-bang-file (locate-user-emacs-file "duckduckgo/bang.el")
  "File that stores the list of bangs."
  :type 'file)

(defvar duckduckgo-bang-alist nil)

(defvar duckduckgo-history nil)

;;;###autoload
(defun duckduckgo (query)
  "Search QUERY using DuckDuckGo."
  (interactive (list (completing-read
                      "DuckDuckGo: " (duckduckgo-bang--completion)
                      nil nil nil duckduckgo-history)))
  ;; TODO If the query is a bang, delegate call to `duckduckgo-bang--run'.
  (funcall duckduckgo-browse-url-function
           (concat duckduckgo-url "?q=" query)))

(defun duckduckgo-bang--completion ()
  "Return a completion table for bangs."
  (duckduckgo-bang-completion
   (if (or duckduckgo-bang-alist
           (duckduckgo-bang--restore))
       (mapcar #'car duckduckgo-bang-alist)
     (duckduckgo-bang--update-synchronously))))

(defun duckduckgo-bang-completion (candidates)
  "Return a completion table for bangs from CANDIDATES."
  `(lambda (string pred action)
     (if (eq action 'metadata)
         '(metadata . ((category . duckduckgo-bang)
                       (annotation-function . duckduckgo-bang-annotator)))
       (complete-with-action action ',candidates string pred))))

(defun duckduckgo-bang-annotator (input)
  (pcase (or (assoc input duckduckgo-bang-alist)
             (when-let (bang (save-match-data
                               (when (string-match (rx "!" (+ (any alnum))) input)
                                 (match-string 0 input))))
               (assoc bang duckduckgo-bang-alist)))
    (`(,_ ,description ,category)
     (concat " " (propertize description 'face 'font-lock-string-face)
             " " (propertize (string-join category " > ")
                             'face 'font-lock-comment-face)))))

;;;; Parsing the list of bangs

;;;###autoload
(defun duckduckgo-bang-update ()
  "Update the list of bangs."
  (interactive)
  (url-retrieve duckduckgo-bang-url
                (lambda (status &optional _cbargs)
                  (pcase status
                    (`(:error ,error . ,_)
                     (error "Error while fetching bang_lite.html: %s" error))
                    (`(:redirect ,url . ,_)
                     (error "Redirection is unsupported (url %s)" url))
                    (_
                     (prog1 (duckduckgo-bang--parse)
                       (kill-buffer (current-buffer))))))))

(defun duckduckgo-bang--update-synchronously ()
  "Update the list of bangs."
  (message "Retrieving the bang list synchronously...")
  (let ((buffer (url-retrieve-synchronously duckduckgo-bang-url)))
    (unwind-protect
        (with-current-buffer buffer
          (duckduckgo-bang--parse))
      (kill-buffer buffer))))

(defun duckduckgo-bang--parse ()
  (goto-char (or (bound-and-true-p url-http-end-of-headers)
                 (point-min)))
  (let ((category-alist (duckduckgo-bang--parse-category-list))
        (flat-alist (duckduckgo-bang--parse-alphabetic-list))
        result)
    (cl-flet
        ((decode-text (s) (or (ignore-errors
                                (utf7-decode s))
                              s)))
      (pcase-dolist (`(,name . ,description)
                     flat-alist)
        (push (list (decode-text name)
                    (decode-text description)
                    (mapcar #'decode-text (cdr (assoc name category-alist))))
              result)))
    (prog1 (setq duckduckgo-bang-alist (nreverse result))
      (duckduckgo-bang--persist))))

(defun duckduckgo-bang--parse-category-list ()
  (require 'xml)
  (search-forward "<h6>Here's the full list of !bang commands by category:</h6>")
  (let (ctx result)
    (while (re-search-forward (rx (or "<h4 " "<h6>"))
                              nil t)
      (goto-char (car (match-data)))
      (pcase (match-string 0)
        ("<h4 "
         (progn
           (setq ctx (list (thread-last
                             (caddr (xml-parse-tag))
                             (string-trim))))
           (search-forward "</h4>")))
        ("<h6>"
         (progn
           (setq ctx (list (car ctx)
                           (thread-last
                             (caddr (xml-parse-tag))
                             (caddr)
                             (string-remove-suffix ":"))))
           (search-forward "</h6>")
           (while (and (re-search-forward (rx (or (group "!" (+ nonl))
                                                  "</li>"))
                                          nil t)
                       (not (equal (match-string 0) "</li>")))
             (push (cons (substring-no-properties (match-string 1)) ctx)
                   result))))))
    result))

(defun duckduckgo-bang--parse-alphabetic-list ()
  (let (result)
    (search-forward "<h4>")
    (while (re-search-forward (rx "(" (group "!" (+ (any alnum)))
                                  ")<br>")
                              nil t)
      (push (cons (substring-no-properties (match-string 1))
                  (string-trim
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (1- (car (match-data))))))
            result))
    result))

(defun duckduckgo-bang--persist ()
  (unless duckduckgo-bang-alist
    (error "The variable duckduckgo-bang-alist is empty"))
  (with-temp-buffer
    (let ((print-level nil)
          (print-length nil))
      (prin1 duckduckgo-bang-alist (current-buffer)))
    (unless (file-directory-p (file-name-directory duckduckgo-bang-file))
      (make-directory (file-name-directory duckduckgo-bang-file)))
    (setq buffer-file-name duckduckgo-bang-file)
    (let ((inhibit-message t))
      (save-buffer))))

(defun duckduckgo-bang--restore ()
  (when (file-readable-p duckduckgo-bang-file)
    (with-temp-buffer
      (insert-file-contents duckduckgo-bang-file)
      (goto-char (point-min))
      (setq duckduckgo-bang-alist (read (current-buffer))))))

(provide 'duckduckgo)
;;; duckduckgo.el ends here
