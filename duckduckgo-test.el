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

(describe "Parsing query possibly containing a bang"
  (it "returns nil if no bang is contained"
    (expect (duckduckgo--bang-query-p "hello bang")
            :to-be nil)
    (expect (duckduckgo--bang-query-p "hello bang!")
            :to-be nil))
  (it "returns the name of a bang if it contains a bang"
    (expect (duckduckgo--bang-query-p "!hackage")
            :to-equal "!hackage")
    (expect (duckduckgo--bang-query-p "!hackage hoogle")
            :to-equal "!hackage")
    (expect (duckduckgo--bang-query-p "hoogle !hackage")
            :to-equal "!hackage")
    (expect (duckduckgo--bang-query-p "hoogle !hackage haskellwiki")
            :to-equal "!hackage")))

(provide 'duckduckgo-test)
