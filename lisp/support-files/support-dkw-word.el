;;; support-dkw-word.el --- support file for "大漢和辞典・単語一覧" file.
;;; Documentation:

;; Download site:
;; http://github.com/kawabata/kanji-database-dict/dkw-word.txt

;;; Code:

;;; Customizable Variable

(load "support-dkw2ucs")

;;; Main

(defun support-dkw-word-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (goto-char (point-min))
  (re-search-forward "^\\(DW.+\\) DP\\([XH]\\)?\\([0-9]+\\) \\(.+\\)" nil t)
  (destructuring-bind
      (num vol page word)
      (mapcar 'match-string '(1 2 3 4))
    (delete-region (point-min) (point-max))
    (insert (format "【%s】 ページ:%s%s\n" word (or vol "") page))
    (insert (support-dkw2ucs-pdf (string-to-number page) vol))))

(setq lookup-support-options
      (list :title "大漢和辞典（単語）"
            ;; :query-filter 'lookup-new-to-old-kanji
            :content-tags '("\n" . "\n")
            :entry-tags '(" " . "\n")
            :head-tags '("\n" . " ")
            :code-tags '("\n" . "DP")
            :charsets (lambda (str) (string-match "^\\cC+$" str))
            :arranges '((reference support-dkw-word-arrange-structure
                                   lookup-arrange-references-url))))

;;; support-dkw2ucs.el ends here
