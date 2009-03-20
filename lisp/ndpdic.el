;;; ndpdic.el --- Lookup `pdic' interface  -*- coding: utf-8 -*-
;; Copyright (C) 2009 Lookup Development Team

;; Author: KAWABATA Taichi <kawabata.taichi@gmail.com>
;; Keywords: dictionary

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Documentation:

;; This is agent program for `pdic' format.

;;; Usage
;; 
;; (setq lookup-search-agents
;;       '(
;;         ....
;;        (ndpdic "~/edicts/eijiro/Eijiro112.dic")
;;         ....
;;         ))


;;; <<Sample Function>>

;; There is a small sample function to create `index' file for PDIC.
;; To make an index, type M-x ndpdic-create-index-file.
;; (You need at least 1G memory, it would take 10 minutes for 2GHz machine.)

;; Index is XML-like style, so you can try `mksary' to create and refer
;; it.

;; BOCU Decoder

(defun bocu-read-decode-trail-char (reg)
  "BOCU trail char in REG to be decoded."
  `(read-if (,reg > #x20) (,reg -= 13) 
     (if (,reg >= #x1c) (,reg -= 12)   
       (if (,reg >= #x10) (,reg -= 10) 
         (,reg -= 1)))))               

(define-ccl-program decode-bocu
  `(4
    ((r4 = #x40)
     (r3 = ,(charset-id-internal 'unicode))
     (loop
      (read r0)
      ;; Diff calculation phase
      (if (r0 <= #x20) (r1 = r0)
        (if (r0 == #x21)
            ((r1 = -14536567)
             ,(bocu-read-decode-trail-char 'r2)
             (r1 += (r2 * 59049))
             ,(bocu-read-decode-trail-char 'r2)
             (r1 += (r2 * 243))
             ,(bocu-read-decode-trail-char 'r2)
             (r1 += r2))
          (if (r0 < #x25)
              ((r1 = (((r0 - #x25) * 59049) - 10513))
               ,(bocu-read-decode-trail-char 'r2)
               (r1 += (r2 * 243))
               ,(bocu-read-decode-trail-char 'r2)
               (r1 += r2))
            (if (r0 < #x50)
                ((r1 = (((r0 - #x50) * 243) - 64))
                 ,(bocu-read-decode-trail-char 'r2)
                 (r1 += r2))
              (if (r0 < #xd0)
                  (r1 = (r0 - #x90))
                (if (r0 < #xfb)
                    ((r1 = (((r0 - #xd0) * 243) + 64))
                     ,(bocu-read-decode-trail-char 'r2)
                     (r1 += r2))
                  (if (r0 < #xfe)
                      ((r1 = (((r0 - #xfb) * 59049) + 10513))
                       ,(bocu-read-decode-trail-char 'r2)
                       (r1 += (r2 * 243))
                       ,(bocu-read-decode-trail-char 'r2)
                       (r1 += r2))
                    (if (r0 == #xfe)
                        ((r1 = 187660)
                         ,(bocu-read-decode-trail-char 'r2)
                         (r1 += (r2 * 59049))
                         ,(bocu-read-decode-trail-char 'r2)
                         (r1 += (r2 * 243))
                         ,(bocu-read-decode-trail-char 'r2)
                         (r1 += r2)
                         ;; ignore case: `r0 = #xff'
                         )))))))))
      ;; output stage
      (if (r0 <= #x20) 
          ((if (r0 != 13) (write r0))
           (if (r0 < #x20) (r4 = #x40)))
        (if (r0 < #xff)
            ((r1 += r4)
             (if (r1 < 0) (r1 = 0)) ; error recovery
             (write-multibyte-character r3 r1)
             ;; cp renewal stage
             (if (r1 < #x20) (r4 = #x40) ; reset
               (if (r1 == #x20) (r4 = r4) ; space → keep
                 ((r5 = (r1 >= #x3040))
                  (r6 = (r1 <= #x309f))
                  (if (r5 & r6) (r4 = #x3070)
                    ((r5 = (r1 >= #x4e00))
                     (r6 = (r1 <= #x9fa5))
                     (if (r5 & r6) (r4 = #x7711)
                       ((r5 = (r1 >= #xac00))
                        (r6 = (r1 <= #xd7a3))
                        (if (r5 & r6) (r4 = #xc1d1)
                          ((r5 = (r1 & #xff))
                           (r6 = (r1 & #xffffff00))
                           (if (r5 < #x80) (r4 = (r6 + #x40))
                             (r4 = (r6 + #xc0)))))))))))))))
      (repeat)))))

(defun ndpdic-bocu-to-str (string)
  "Decode BOCU STRING to Emacs String."
  (ccl-execute-on-string 'decode-bocu '[0 0 0 0 0 0 0 0 0] string))

;;; Interface Functions

(defvar ndpdic-max-hits 150)

(put 'ndpdic :methods 'ndpdic-dictionary-methods)
(defun ndpdic-dictionary-methods (dictionary)
  "Return methods of DICTIONARY."
  '(exact prefix))

(put 'ndpdic :list 'ndpdic-list)
(defun ndpdic-list (agent)
  "Return list of dictionary (only one) of AGENT."
  (let ((location (lookup-agent-location agent)))
    (if (null (file-exists-p location))
        (error "PDIC file not found!")
      (if (> #x500 (ndpdic-file-version location))
          (error "This version of PDIC file is not supported!")))
    (list (lookup-new-dictionary 
           agent 
           (downcase (replace-regexp-in-string
                      "^.+/\\([a-zA-Z]+\\)[^/]+$" "\\1"
                      (lookup-agent-location agent)))))))

(put 'ndpdic :title 'ndpdic-title)
(defun ndpdic-title (dictionary)
  "Return title of DICTIONARY."
  (replace-regexp-in-string
   "^.*/\\([^/]+\\)$" "\\1"
   (lookup-agent-location (lookup-dictionary-agent dictionary))))

(put 'ndpdic :search 'ndpdic-dictionary-search)
(defun ndpdic-dictionary-search (dictionary query)
  "Return list of entries for DICTIONARY QUERY."
  (let* ((query-method (lookup-query-method query))
         (query-string
          (concat (lookup-query-string query)
                  (if (eq query-method 'exact) "\\(	\\|$\\)")))
         (location (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))
         (result
          (ndpdic-binary-search location query-string)))
    (when result
      (setq result
            (remove-if
             (lambda (x) (null (string-match
                                (concat "^" query-string) x)))
             (append (ndpdic-entries location (car result))
                     (ndpdic-entries location (cdr result)))))
      ;;(if (> (length result) ndpdic-max-hits)
      ;;    )
      (mapcar (lambda (x) (lookup-new-entry
                           'regular dictionary x
                           (if (string-match "	" x)
                               (substring x (match-end 0)))))
              result))))

(put 'ndpdic :content 'ndpdic-content)
(defun ndpdic-content (entry)
  "Content of ENTRY."
  (let* ((code (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (location (lookup-agent-location
                    (lookup-dictionary-agent dictionary)))
         (result (car (ndpdic-binary-search location code))))
    (ndpdic-entry-content location result code)))

;; Hash variables 
;; (in future, move them to lookup hash tables)

(defvar ndpdic-block-index-hash
  (make-hash-table :test 'equal)
  "Hash table for file -> block-index table.")

(defvar ndpdic-block-entries-hash
  (make-hash-table :test 'equal)
  "Hash table for file -> (block -> entries) table.")

;; basic functions

(defun ndpdic-file-content (file from to)
  "Unibyte string of FILE from FROM to TO."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil from to)
    (buffer-string)))

(defun ndpdic-file-byte (file point)
  "Short value of FILE at POINT."
  (string-to-char (ndpdic-file-content file point (1+ point))))

(defun ndpdic-buffer-byte ()
  "Byte value of current buffer point."
  (let ((int (char-after (point))))
    (forward-char)
    int))

(defun ndpdic-string-to-short (str)
  "Convert STR to short value."
  (let ((chs (string-to-list str)))
    (+ (* (elt chs 1) 256) (elt chs 0))))

(defun ndpdic-file-short (file point)
  "Short value of FILE at POINT."
  (ndpdic-string-to-short (ndpdic-file-content file point (+ 2 point))))

(defun ndpdic-buffer-short ()
  "Int value of current buffer point."
  (let ((int (ndpdic-string-to-short (buffer-substring (point) (+ 2 (point))))))
    (goto-char (+ 2 (point)))
    int))

(defun ndpdic-string-to-int (str)
  "Convert STR to int value."
  (let ((factor 16777216) (result 0))
    (dolist (ch (nreverse (string-to-list str)))
      (setq result (+ result (* ch factor))
            factor (/ factor 256)))
    result))

(defun ndpdic-file-int (file point)
  "Int value of FILE at POINT."
  (ndpdic-string-to-int (ndpdic-file-content file point (+ 4 point))))

(defun ndpdic-buffer-int ()
  "Int value of current buffer point."
  (let ((int (ndpdic-string-to-int (buffer-substring (point) (+ 4 (point))))))
    (goto-char (+ 4 (point)))
    int))

(defun ndpdic-file-version (file)
  "Header lowrd value for FILE."
  (ndpdic-file-short file 140))

(defun ndpdic-file-lword (file)
  "Header lowrd value for FILE."
  (ndpdic-file-short file 142))

(defun ndpdic-file-ljapa (file)
  "Header lowrd value for FILE."
  (ndpdic-file-short file 144))

(defun ndpdic-file-block-size (file)
  "Header block_size value for FILE."
  (ndpdic-file-short file 146))

(defun ndpdic-file-index-block (file)
  "Header index_block value for FILE."
  (ndpdic-file-short file 148))

(defun ndpdic-file-header-size (file)
  "Header header_size value for FILE."
  (ndpdic-file-short file 150))

(defun ndpdic-file-nword (file)
  "Header nword value for FILE."
  (ndpdic-file-int file 160))

(defun ndpdic-file-dicorder (file)
  "Header dicorder value for FILE."
  (ndpdic-file-byte file 164))

(defun ndpdic-file-dictype (file)
  "Header dictype value for FILE."
  (ndpdic-file-byte file 165))

(defun ndpdic-file-os (file)
  "Header os value for FILE."
  (ndpdic-file-byte file 167))

(defun ndpdic-file-index-blkbit (file)
  "Header index-blkbit value for FILE."
  (ndpdic-file-byte file 182))

(defun ndpdic-file-extheader (file)
  "Header extheader value for FILE."
  (ndpdic-file-int file 184))

(defun ndpdic-file-empty-block (file)
  "Header empty_block value for FILE."
  (ndpdic-file-int file 188))

(defun ndpdic-file-nindex2 (file)
  "Header nindex2 value for FILE."
  (ndpdic-file-int file 192))

(defun ndpdic-file-nblock2 (file)
  "Header nblock2 value for FILE."
  (ndpdic-file-int file 196))

(defun ndpdic-file-crypt (file)
  "Header extheader value for FILE."
  (ndpdic-file-content file 200 208))

(defun ndpdic-file-index-start (file)
  "Index start point of FILE."
  (+ (ndpdic-file-header-size file)
     (ndpdic-file-extheader file)))

(defun ndpdic-file-data-start (file &optional block)
  "Data start point in FILE of BLOCK number."
  (unless block (setq block 0))
  (+ (ndpdic-file-index-start file) ; 1024
     (* (ndpdic-file-block-size file) ; 1024
        (+ (ndpdic-file-index-block file) block))))

(defun ndpdic-forward-to-null ()
  "Forward to next point of null character.
If it encounters continuous null or eobp, then return nil.
Otherwise, return t."
  (interactive)
  (if (eobp) nil
    (if (and (equal (char-after (point)) 0)
             (progn
               (forward-char)
               (equal (char-after (point)) 0))) nil
      (while (not (or (eobp)
                      (eq (char-after (point)) 0)))
        (forward-char))
      (if (eobp) nil t))))

(defun ndpdic-block-index (file)
  "Construct Block Index of FILE.  Result will be cached."
  (or
   (gethash (expand-file-name file) ndpdic-block-index-hash)
   (let* ((blocks (make-vector (ndpdic-file-nindex2 file) nil))
          (blkbit (ndpdic-file-index-blkbit file))
          (i 0))
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert-file-contents-literally
        file nil (ndpdic-file-index-start file)
        (ndpdic-file-data-start file))
       (goto-char (point-min))
       (while (not (eobp))
         (if (< i (length blocks))
             (aset blocks i
                   (if (= blkbit 0) (ndpdic-buffer-short)
                     (ndpdic-buffer-int))))
         (setq i (1+ i))
         (if (null (ndpdic-forward-to-null)) (goto-char (point-max)))
         (unless (eobp) (forward-char))))
     (puthash (expand-file-name file) blocks ndpdic-block-index-hash)
     blocks)))

(defun ndpdic-insert-block-contents (file block)
  "Insert content of FILE's BLOCK to current buffer.
Return a size of `Field-Length' of the block."
  (let* ((start (ndpdic-file-data-start file block))
         (block-size (ndpdic-file-block-size file))
         (block-num (ndpdic-file-short file start))
         (fl-size (if (eq (logand block-num #x8000) 0) 2 4))
         (block-num (logand block-num #x7fff)))
    (insert-file-contents-literally file nil start
                                    (+ start (* block-num block-size)))
    fl-size))

(defun ndpdic-entries-next-word (prev-word-data &optional field-size-length)
  "Scan the current buffer and return the new word and misc.
Format is a list of (WORD KIND CONTENT-START-POINT WORD-DATA).
It assumes that current point is at beginning of new entry.  If
there is no more entries available in this block, then nil is
returned.  PREV-WORD-DATA will be used for decompressing new word.
Default FIELD-SIZE-LENGTH value would be 2.  If there is a word,
then it proceeds to next point."
  (if (null field-size-length) (setq field-size-length 2))
  (let ((field-size (if (eq field-size-length 2)
                        (ndpdic-buffer-short)
                      (ndpdic-buffer-int)))
        compress kind start
        content-start word-data)
    (when (/= 0 field-size)
      (setq compress (ndpdic-buffer-byte))
      (setq kind (ndpdic-buffer-byte))
      (setq start (point))
      (ndpdic-forward-to-null)
      (setq word-data (buffer-substring start (point)))
      (setq word-data (concat (substring prev-word-data 0 compress) word-data))
      (setq content-start (1+ (point)))
      (goto-char (+ start field-size))
      (list (ndpdic-bocu-to-str word-data)
            kind content-start word-data))))

(defun ndpdic-entries (file block)
  "Get all entries in FILE at BLOCK.
Return the list of entry words.  Result will be cached."
  (let ((block-entries-hash (gethash (expand-file-name file) ndpdic-block-entries-hash))
        fl-size word-spec (word-data "") words)
    (when (null block-entries-hash)
      (setq block-entries-hash (make-hash-table))
      (puthash (expand-file-name file) block-entries-hash ndpdic-block-entries-hash))
    (or
     (gethash block block-entries-hash)
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (setq fl-size (ndpdic-insert-block-contents file block))
       (goto-char (+ 2 (point-min)))
       (while (not (eobp))
         (setq word-spec (ndpdic-entries-next-word word-data fl-size))
         (if (null word-spec) (goto-char (point-max))
           (setq words (cons (car word-spec) words))
           (setq word-data (elt word-spec 3))))
       (puthash block (nreverse words) block-entries-hash)))))

(defun ndpdic-entry-content (file block entry)
  "Get content of FILE, BLOCK, and  ENTRY."
  (let* (fl-size word word-spec (word-data "") content)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq fl-size (ndpdic-insert-block-contents file block))
      (goto-char (+ 2 (point-min)))
      (while (not (eobp))
        (setq word-spec (ndpdic-entries-next-word word-data fl-size))
        (setq word (car word-spec))
        (setq word-data (elt word-spec 3))
        (if (null word-spec) (goto-char (point-max))
          (when (equal (car word-spec) entry)
            (setq content
                  (ndpdic-adjust-content
                   entry (elt word-spec 2) (point)))
            (goto-char (point-max)))))
      content)))

(defun ndpdic-adjust-content (entry from to)
  (concat
   (if (string-match "	" entry) 
       (substring entry (match-end 0)) entry)
   "\n"
   (ndpdic-bocu-to-str (buffer-substring from to))))
  

;; binary search

(defun ndpdic-compare-entry (entry entries)
  "Judege if ENTRY is larger, smaller, or inclusive of ENTRIES.
Comparison is done lexicographicaly.
If larger, t. smaller, nil.  equal, 0 will be returned."
  (if (string-lessp entry (car entries)) nil
    (if (string-lessp (car (last entries)) entry) t 0)))

(defun ndpdic-binary-search (file entry)
  "Find block in FILE which includes ENTRY.
Return value would be (block . next-block)."
  (let* ((block-index (ndpdic-block-index file))
         (start 0) (end (length block-index))
         (middle (/ end 2))
         (entries (ndpdic-entries file (aref block-index middle)))
         result)
    (while
        (progn
          (setq result (ndpdic-compare-entry entry entries))
          (not (or (and (numberp result) (= 0 result))
                   (= start middle))))
      (if result (setq start middle)
        (setq end middle))
      (setq middle (/ (+ start end) 2))
      (setq entries (ndpdic-entries file (aref block-index middle))))
    (if (and (numberp result) (= 0 result))
        (cons (aref block-index middle)
              (if (/= (1- (length block-index)) middle)
                  (aref block-index (1+ middle))))
      nil)))
        
;; Utility Function

(defun ndpdic-create-index-file (file)
  "Create XML-like index file from FILE.
FILEは `.dic' ファイルとおなじ場所に、拡張子 `.idx' で保存される。
FILEが、PDIC Unicodeの初期バージョンの場合は、下記のように出力される。
<entry><word>XXXX</word><block>ZZZZ</block></entry>
FILEが、PDIC Unicodeの英辞郎バージョンの場合は、下記の様に出力される。
<entry><word>XXXX</word><head>YYYY</head><block>ZZZZ</block></entry>."
  (interactive "fPDIC File Name:")
  (let ((index-file (concat (file-name-sans-extension file) ".idx"))
        (block-index (ndpdic-block-index file))
        buffer block block-num (total (ndpdic-file-nindex2 file)))
    (if (or (file-exists-p index-file)
            (null (y-or-n-p (format "Index file %s will be created.  OK? " index-file))))
        (error "%s can't be created!" index-file)
      (with-temp-buffer
        (dotimes (i (length block-index))
          (setq block (aref block-index i) 0)
          (if (= 0 (% i 100)) (message "%d %% done..." (/ (* 100 i) total)))
          (setq block-num (number-to-string (aref block 1)))
          (dolist (entry (ndpdic-entries file block))
            (if (string-match "	" entry)
                (insert "<entry><word>" (substring entry 0 (match-beginning 0))
                        "</word><head>" (substring entry (match-end 0))
                        "</head><block>" block-num
                        "</block></entry>\n")
              (insert "<entry><word>" entry
                      "</word><block>" block-num
                      "</block></entry>\n"))))
        (write-region (point-min) (point-max) (expand-file-name index-file))))))

(provide 'ndpdic)

;;; ndpdic.el ends here
