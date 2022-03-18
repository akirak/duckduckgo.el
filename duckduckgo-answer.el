;;; duckduckgo-answer.el --- Instant Answer -*- lexical-binding: t -*-

(defgroup duckduckgo-answer nil
  ""
  :prefix "duckduckgo-answer-"
  :group 'duckduckgo)

(defconst duckduckgo-answer-buffer "*DuckDuckGo Answer*")

(defconst duckduckgo-answer-url-prefixes
  '("https://duckduckgo.com/c/"
    "https://duckduckgo.com/"))

(defcustom duckduckgo-answer-directory
  (locate-user-emacs-file "duckduckgo/answers")
  "Directory in which Instant Answer responses are cached."
  :type 'directory)

(defclass duckduckgo-answer ()
  ((Abstract :initarg :Abstract)
   (AbstractSource :initarg :AbstractSource)
   (AbstractText :initarg :AbstractText)
   (AbstractURL :initarg :AbstractURL)
   (Answer :initarg :Answer)
   (AnswerType :initarg :AnswerType)
   (Definition :initarg :Definition)
   (DefinitionSource :initarg :DefinitionSource)
   (DefinitionURL :initarg :DefinitionURL)
   (Entity :initarg :Entity)
   (Heading :initarg :Heading)
   (Image :initarg :Image)
   (ImageHeight :initarg :ImageHeight)
   (ImageIsLogo :initarg :ImageIsLogo)
   (ImageWidth :initarg :ImageWidth)
   (Infobox :initarg :Infobox)
   (Redirect :initarg :Redirect)
   (RelatedTopics :initarg :RelatedTopics)
   (Results :initarg :Results)
   (Type :initarg :Type)))

(define-button-type 'duckduckgo-external
  :supertype 'help-xref
  'help-function 'browse-url
  'help-echo (purecopy "mouse-2, RET: Browse the URL"))

(define-button-type 'duckduckgo-answer
  :supertype 'help-xref
  'help-function 'duckduckgo-answer
  'help-echo (purecopy "mouse-2, RET: Browse the answer"))

;;;###autoload
(defun duckduckgo-answer (query)
  (interactive "sInstant answer: ")
  (duckduckgo-answer-async
   query
   (lambda (response)
     (if response
         (with-electric-help
          `(lambda ()
             (let ((answer (apply #'make-instance 'duckduckgo-answer ',response)))
               (when-let (heading (oref answer Heading))
                 (insert heading "\n")
                 (insert ?\n))

               (when-let (abstract (oref answer Abstract))
                 (insert "According to ")
                 (insert-text-button (oref answer AbstractSource)
                                     'type 'duckduckgo-external
                                     'help-args (list (oref answer AbstractURL)))
                 (insert ":\n" abstract "\n")
                 (insert ?\n))

               (when-let (infobox (oref answer Infobox))
                 (dolist (content infobox)
                   (insert (plist-get content :label) " :: ")
                   (pcase (plist-get content :data_type)
                     ("official_website"
                      (insert-text-button (plist-get content :value)
                                          'type 'duckduckgo-external
                                          'help-args (list (plist-get content :value))))
                     (_
                      (insert (or (plist-get content :value) ""))))
                   (insert "\n"))
                 (insert ?\n))

               (when-let (results (oref answer Results))
                 (dolist (result results)
                   (insert-text-button (plist-get result :Text)
                                       'type 'duckduckgo-external
                                       'help-args (list (plist-get result :FirstURL)))
                   (insert "\n"))
                 (insert ?\n))

               (when-let (topics (oref answer RelatedTopics))
                 (insert "Also see: \n")
                 (dolist (topic topics)
                   (let* ((text (plist-get topic :Text))
                          (url (plist-get topic :FirstURL))
                          (query (cl-some `(lambda (prefix)
                                             (when (string-prefix-p prefix ,url)
                                               (string-remove-prefix prefix ,url)))
                                          duckduckgo-answer-url-prefixes))
                          (title (duckduckgo-answer--url-decode query)))
                     (insert-text-button title
                                         'type 'duckduckgo-answer
                                         'help-args (list query))
                     (insert (string-remove-prefix title text)))
                   (insert "\n"))
                 (insert ?\n))))
          duckduckgo-answer-buffer)
       (user-error "No response for the query")))))

(defun duckduckgo-answer--url (query)
  "Return an Instant Answer URL for a given query."
  (concat "https://api.duckduckgo.com/?format=json&q="
          (duckduckgo-answer--url-decode query)))

(defun duckduckgo-answer--url-decode (query)
  (thread-last
    (url-unhex-string query)
    (replace-regexp-in-string "_" " ")))

(defun duckduckgo-answer--normalize-plist (response)
  (cl-labels
      ((normalize-plist
         (plist)
         (cl-loop for (key value) on plist by #'cddr
                  with result = nil
                  with nonnil = nil
                  with newvalue = nil
                  do (setq newvalue
                           (cond
                            ((not value)
                             nil)
                            ((and (stringp value)
                                  (string-empty-p value))
                             nil)
                            ((listp value)
                             (cond
                              ((eq key :Infobox)
                               (plist-get value :content))
                              ((keywordp (car value))
                               (normalize-plist value))
                              (t
                               (mapcar #'normalize-plist value))))
                            (t
                             value)))
                  unless (eq key :meta)
                  append (list key newvalue) into result
                  when (and newvalue (not (eq key :meta)))
                  do (setq nonnil t)
                  finally return (when nonnil result))))
    (normalize-plist response)))

;;;###autoload
(defun duckduckgo-answer-async (query callback)
  "Asynchronously retrieve an answer for QUERY and run CALLBACK."
  (if-let (response (duckduckgo-answer--get-cache query))
      (if (equal (plist-get response :Type) "D")
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
                            (let ((response (duckduckgo-answer--process-resp)))
                              (if (equal (plist-get response :Type) "D")
                                  (duckduckgo-answer-async
                                   (duckduckgo-answer--disambiguate response)
                                   callback)
                                (funcall callback response)))
                          (kill-buffer (current-buffer))))))
                   callback))))

;;;###autoload
(cl-defun duckduckgo-answer-sync (query)
  "Synchronously retrieve an Instant Answer for QUERY."
  (when-let (response (or (duckduckgo-answer--get-cache query)
                          (with-current-buffer (url-retrieve-synchronously
                                                (duckduckgo-answer--url query))
                            (unwind-protect
                                (duckduckgo-answer--process-resp)
                              (kill-buffer (current-buffer))))))
    (if (equal (plist-get response :Type) "D")
        (duckduckgo-answer-sync (duckduckgo-answer--disambiguate response))
      response)))

(defun duckduckgo-answer--process-resp ()
  (when-let (header-end (bound-and-true-p url-http-end-of-headers))
    (delete-region (point-min) header-end))
  (goto-char (point-min))
  (duckduckgo-answer--save-cache
   (duckduckgo-answer--normalize-plist
    (duckduckgo-answer--parse))))

(defun duckduckgo-answer--parse ()
  (json-parse-buffer :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun duckduckgo-answer--disambiguate (response)
  "Return a query for a related topic in suggested in RESPONSE."
  (cl-labels
      ((make-candidate
         (group alist)
         (if-let (url (plist-get alist :FirstURL))
             (propertize (string-remove-prefix "https://duckduckgo.com/" url)
                         'invisible t
                         'help-echo (plist-get alist :Text)
                         'duckduckgo-topic-group group)
           (when-let (topics (plist-get alist :Topics))
             (mapcar (apply-partially #'make-candidate
                                      (append group (list (plist-get alist :Name))))
                     topics)))))
    (let ((alternatives (thread-last
                          (plist-get response :RelatedTopics)
                          (mapcar (apply-partially #'make-candidate nil))
                          (flatten-list))))
      (completing-read (format "Disambiguate \"%s\": "
                               (plist-get response :Heading))
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

(defun duckduckgo-answer--save-cache (plist)
  (when plist
    (let* ((query (thread-last
                    (plist-get plist :Heading)
                    (replace-regexp-in-string " " "_")
                    (url-encode-url)))
           (file (expand-file-name (concat query ".json")
                                   duckduckgo-answer-directory))
           (dir (file-name-directory file)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (setq buffer-file-name file)
      (let ((inhibit-message t))
        (save-buffer)))
    plist))

(provide 'duckduckgo-answer)
;;; duckduckgo-answer.el ends here
