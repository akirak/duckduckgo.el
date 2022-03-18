;;; duckduckgo-answer.el --- Instant Answer -*- lexical-binding: t -*-

(defgroup duckduckgo-answer nil
  ""
  :prefix "duckduckgo-answer-"
  :group 'duckduckgo)

(defcustom duckduckgo-answer-directory
  (locate-user-emacs-file "duckduckgo/answers")
  "Directory in which Instant Answer responses are cached."
  :type 'directory)

;;;###autoload
(defun duckduckgo-answer (query)
  (interactive "sInstant answer: ")

  )

(defun duckduckgo-answer--url (query)
  "Return an Instant Answer URL for a given query."
  (concat "https://api.duckduckgo.com/?format=json&q="
          (duckduckgo-answer--url-decode query)))

(defun duckduckgo-answer--url-decode (query)
  (thread-last
    (url-unhex-string query)
    (replace-regexp-in-string "_" " ")))

(defun duckduckgo-answer-async (query callback)
  (if-let (response (duckduckgo-answer--get-cache query))
      (if (equal (alist-get 'Type response) "D")
          (duckduckgo-answer-async
           (duckduckgo-answer--disambiguate response)
           callback)
        (funcall callback response))
    (url-retrieve (duckduckgo-answer--url query)
                  (apply-partially
                   (lambda (callback status &optional _cbargs)
                     (pcase status
                       (`(:error ,error . ,_)
                        (error "Error while fetching an answer: %s"
                               error))
                       (`(:redirect ,url . ,_)
                        (error "Redirection is unsupported (url %s)" url))
                       (_
                        (unwind-protect
                            (let ((response (duckduckgo-answer--process-resp query)))
                              (if (equal (alist-get 'Type response) "D")
                                  (duckduckgo-answer-async
                                   (duckduckgo-answer--disambiguate response)
                                   callback)
                                (funcall callback response)))
                          (kill-buffer (current-buffer))))))
                   callback))))

(cl-defun duckduckgo-answer-sync (query)
  (let ((response (or (duckduckgo-answer--get-cache query)
                      (with-current-buffer (url-retrieve-synchronously
                                            (duckduckgo-answer--url query))
                        (unwind-protect
                            (duckduckgo-answer--process-resp query)
                          (kill-buffer (current-buffer)))))))
    (if (equal (alist-get 'Type response) "D")
        (duckduckgo-answer-sync (duckduckgo-answer--disambiguate response))
      response)))

(defun duckduckgo-answer--process-resp (query)
  (when-let (header-end (bound-and-true-p url-http-end-of-headers))
    (delete-region (point-min) header-end))
  (goto-char (point-min))
  (let ((result (duckduckgo-answer--parse)))
    (duckduckgo-answer--save-cache query)
    result))

(defun duckduckgo-answer--parse ()
  (json-parse-buffer :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun duckduckgo-answer--disambiguate (response)
  "Return a query for a related topic in suggested in RESPONSE."
  (cl-labels
      ((make-candidate
         (group alist)
         (if-let (url (alist-get 'FirstURL alist))
             (propertize (string-remove-prefix "https://duckduckgo.com/" url)
                         'invisible t
                         'help-echo (alist-get 'Text alist)
                         'duckduckgo-topic-group group)
           (when-let (topics (alist-get 'Topics alist))
             (mapcar (apply-partially #'make-candidate
                                      (append group (list (alist-get 'Name alist))))
                     topics)))))
    (let ((alternatives (thread-last
                          (alist-get 'RelatedTopics response)
                          (mapcar (apply-partially #'make-candidate nil))
                          (flatten-list))))
      (completing-read (format "Disambiguate \"%s\": "
                               (alist-get 'Heading response))
                       `(lambda (string pred action)
                          (if (eq action 'metadata)
                              '(metadata . ((category . duckduckgo-query)
                                            (annotation-function
                                             . duckduckgo-answer--annotate-topic)))
                            (complete-with-action action ',alternatives string pred)))))))

(defun duckduckgo-answer--annotate-topic (candidate)
  (let ((text (get-char-property 0 'help-echo candidate))
        (group (get-char-property 0 'duckduckgo-topic-group candidate)))
    (put-text-property (1+ (length (duckduckgo-answer--url-decode candidate)))
                       (length text)
                       'face 'font-lock-comment-face
                       text)
    (concat " "
            (if group
                (concat (mapconcat (lambda (s) (propertize s 'face 'font-lock-constant-face))
                                   group
                                   (propertize " > " 'face 'font-lock-comment-face))
                        " ")
              "")
            text)))

;;;; Persistence

(defun duckduckgo-answer--get-cache (query)
  (let ((file (expand-file-name (concat query ".json")
                                duckduckgo-answer-directory)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (duckduckgo-answer--parse)))))

(defun duckduckgo-answer--save-cache (query)
  (let* ((file (expand-file-name (concat query ".json")
                                 duckduckgo-answer-directory))
         (dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setq buffer-file-name file)
    (let ((inhibit-message t))
      (save-buffer))))

(provide 'duckduckgo-answer)
;;; duckduckgo-answer.el ends here
