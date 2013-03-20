;;; support-dkw2ucs-.el --- support file for "大漢和辞典" file.
;;; Documentation:

;; Download site:
;; http://github.com/kawabata/kanji-database-dict/dkw2ucs.txt

;;; Code:

;;; Customizable Variable

(defvar support-dkw2ucs-pdf-directory 
  (concat "file://" (expand-file-name "~/edicts/Daikanwa")))

;;; 大漢和辞典

(defvar support-dkw2ucs-page-volume-alist
  '((1 1)
    (2 1083)
    (3 2275)
    (4 3403)
    (5 4639)
    (6 5713)
    (7 6909)
    (8 8137)
    (9 9355)
    (10 10443)
    (11 11543)
    (12 12597)
    (13 15000)))

(defun support-dkw2ucs-total-to-volume-page (total-page)
  (let ((data support-dkw2ucs-page-volume-alist)
        volume page)
    (while data
      (if (< total-page (cadar data))
          (setq data nil)
        (setq volume (caar data)
              page   (cadar data)
              data   (cdr data))))
    (list volume (1+ (- total-page page)))))

(defun support-dkw2ucs-pdf (total-page &optional extra-volume)
  ;; volume: nil→main, "X"→hoi, "H"→hokan 
  (concat
   support-dkw2ucs-pdf-directory
   (pcase extra-volume
     ((pred null) (destructuring-bind
                      (volume page)
                      (support-dkw2ucs-total-to-volume-page total-page)
                    (format "/v%02d.pdf#page=%d" volume page)))
     (`"X"
      (format "/sakuin_hoi.pdf#page=%d" (- total-page 1044)))
     (`"H"
      (format "/hokan.pdf#page=%d" total-page))
     (t (format "error! total=page=%s volue=%s" total-page extra-volume)))))

;;;
;;; main
;;;

(defun support-dkw2ucs-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (goto-char (point-min))
  (re-search-forward "^\\(D.+\\) DR\\(.+\\) DS\\(.+\\) DP\\([XH]\\)?\\([0-9]+\\) \\(.+\\)" nil t)
  (destructuring-bind
      (num rad str vol page ucs)
      (mapcar 'match-string '(1 2 3 4 5 6))
    (delete-region (point-min) (point-max))
    (insert (format "【%s】（%s） 部首:%s 画数:%s ページ:%s%s\n" num
                    (replace-regexp-in-string "U\\+[0-9A-F]+"
                                              (lambda (x) 
                                                (char-to-string (string-to-number (substring x 2) 16)))
                                              ucs)
                    rad str (or vol "") page))
    (insert (support-dkw2ucs-pdf (string-to-number page) vol))))

;; Query-Filter
(defun support-dkw2ucs-query-filter (query)
  (lookup-new-query-filter
   query 
   (lambda (string)
     (format "%05X" (string-to-char string)))))

(setq lookup-support-options
      (list :title "大漢和辞典（漢字）"
            :query-filter 'support-dkw2ucs-query-filter
            :content-tags '("\n" . "\n")
            :entry-tags '("U+" . "")
            :head-tags '("\n" . " ")
            :code-tags '("\n" . "DP")
            :charsets (lambda (x) (string-match "^\\cC$" x))
            :arranges '((reference support-dkw2ucs-arrange-structure
                                   lookup-arrange-references-url))))

;;; support-dkw2ucs.el ends here
