;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'duckduckgo)

(describe "Parsing the bang list"
  (it "retrieves categories and descriptions"
    (let ((buffer (url-retrieve-synchronously duckduckgo-bang-url)))
      (expect (with-current-buffer buffer
                (goto-char (bound-and-true-p url-http-end-of-headers))
                (duckduckgo-bang--parse-category-list))
              :to-contain
              '("!cisco" "Tech" "Companies"))
      (expect (with-current-buffer buffer
                (goto-char (bound-and-true-p url-http-end-of-headers))
                (duckduckgo-bang--parse-alphabetic-list))
              :to-contain
              '("!wolf" . "Wolfram Alpha")))))

(provide 'duckduckgo-test)
