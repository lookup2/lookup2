;;; ndpdic.el --- Lookup `PDIC' interface -*- coding: utf-8; lexical-binding: t -*-
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

;;; Commentary:

;; This is agent program for `PDIC' format.
;;
;; PDIC specification::
;; http://homepage3.nifty.com/TaN/unicode/dic-spec.html

;;; Usage: 

;; Put the XXX.dic files into a folder and specify that folder for ndpdic agent.
;; 
;; (setq lookup-search-agents
;;       '(...
;;        (ndpdic "~/edicts/eijiro")
;;         ...))
;;
;; If dictionary option `:index' is specified via support file, then it 
;; will be possible to search the dictionary by regular expression.
;;
;; There is a small sample function to create `index' file for PDIC.
;; To make an index, type M-x ndpdic-create-index-file.
;; (You need at least 1G memory, it would take 10 minutes for 2GHz machine 
;; for huge file such as Eijiro.)

;;; Code:

(eval-when-compile (require 'cl))
(require 'lookup)
(require 'bocu)

;;;
;;; Customizable Variables
;;;

(defvar ndpdic-max-hits lookup-max-hits)

(defvar ndpdic-extension-regexp "\\.dic\\|\\.DIC\\'")

(defvar ndpdic-extended-attributes
  '((0 . (lambda (x) (concat (bocu-to-str x) "\n")))
    (1 . (lambda (x) (concat "【用例】" (bocu-to-str x) "\n")))
    (2 . (lambda (x) (concat "【発音】" (bocu-to-str x) "\n")))))

;;;
;;; Interface functions
;;;

(put 'ndpdic :methods 'ndpdic-dictionary-methods)
(defun ndpdic-dictionary-methods (ignored)
  ;; DICTIONARY is ignored
  "Return methods of DICTIONARY."
  '(exact prefix))

(put 'ndpdic :list 'ndpdic-list)
(defun ndpdic-list (agent)
  "Return dictionaries in AGENT."
  (let ((dir (lookup-agent-location agent))
        files ndpdic dict)
    (assert (file-directory-p dir) nil "ndpdic: directory %s is not found." dir)
    (setq files (directory-files (expand-file-name dir)
                                 nil ndpdic-extension-regexp))
    (loop for file in files do
          (setq dict (lookup-new-dictionary agent file))
          (setq ndpdic (ndpdic (concat dir "/" file)))
          (lookup-put-property dict :ndpdic ndpdic)
          collect dict)))

(put 'ndpdic :title 'ndpdic-title)
(defun ndpdic-title (dictionary)
  "Return title of DICTIONARY."
  (replace-regexp-in-string
   "^.*/\\([^/]+\\)$" "\\1"
   (lookup-dictionary-name dictionary)))

(put 'ndpdic :search 'ndpdic-search)
(defun ndpdic-search (dictionary query)
  "Return list of entries for DICTIONARY QUERY."
  (ndpdic-initialize dictionary)
  (let* ((query-method (lookup-query-method query))
         (query-string
          (concat (lookup-query-string query)
                  (if (eq query-method 'exact) "\\(	\\|$\\)")))
         (ndpdic (lookup-get-property dictionary :ndpdic))
         (result (ndpdic-binary-search ndpdic query-string)))
    (when result
      (setq result
            (cl-remove-if
             (lambda (x) (null (string-match
                                (concat "^" query-string) x)))
             (append (ndpdic-entries ndpdic (car result))
                     (ndpdic-entries ndpdic (cdr result)))))
      (mapcar (lambda (x) (lookup-new-entry
                           'regular dictionary x
                           ;; pdic v5 以降は タブで検索語と見出し語を区切る
                           (if (string-match "	" x)
                               (substring x (match-end 0)))))
              result))))

(put 'ndpdic :content 'ndpdic-content)
(defun ndpdic-content (entry)
  "Content of ENTRY."
  (let* ((code (lookup-entry-code entry))
         (dictionary (lookup-entry-dictionary entry))
         (ndpdic (lookup-get-property dictionary :ndpdic))
         (result (car (ndpdic-binary-search ndpdic code))))
    (ndpdic-entry-content ndpdic result code)))

;;;
;;; Initialization
;;;

(defun ndpdic-initialize (dictionary)
  (unless (lookup-get-property dictionary :ndpdic)
    (let* ((agent (lookup-dictionary-agent dictionary))
           (location (lookup-agent-location agent))
           (name (lookup-dictionary-name dictionary))
           (file (expand-file-name name location))
           (ndpdic (ndpdic file)))
      (lookup-put-property dictionary :ndpdic ndpdic))))

;;;
;;; Structure
;;;

(cl-defstruct ndpdic 
  filename headername dictitle version lword liapa block-size
  index-block header-size index-size empty-block nindex nblock
  nword dicorder dictype attrlen os olenumber dummy-lid
  index-blkbit dummy0 extheader empty-block2 nindex2 nblock2
  crypt update-count dummy00 dicident derefid dummy
  extended-header block-index block-entries-hash)

(defun ndpdic-read-chars (len)
  (prog1 (buffer-substring (point) (+ (point) len))
    (goto-char (+ (point) len))))
(defun ndpdic-read-byte ()
  (prog1 (char-after (point)) (goto-char (1+ (point)))))
(defun ndpdic-read-short ()
  (prog1 (+ (char-after (point)) (* 256 (char-after (1+ (point)))))
    (goto-char (+ (point) 2))))
(defun ndpdic-read-long ()
  (prog1 (+ (char-after (point)) (* 256 (char-after (1+ (point))))
            (* 65536 (char-after (+ 2 (point)))) 
            (* 16777216 (char-after (+ 3 (point)))))
    (goto-char (+ (point) 4))))

(defun ndpdic (file)
  "Return ndpdic object representing FILE pdic data."
  (assert (file-exists-p file) nil "%s not found!" file)
  (let ((o (make-ndpdic :filename file)))
    (cl-flet ((char (len) (ndpdic-read-chars len))
              (byte ()    (ndpdic-read-byte))
              (short ()   (ndpdic-read-short))
              (ushort ()  (ndpdic-read-short))
              (long  ()   (ndpdic-read-long))
              (ulong ()   (ndpdic-read-long)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 1024)
        (setf (ndpdic-headername o)   (char 100))
        (setf (ndpdic-dictitle o)     (char 40))
        (setf (ndpdic-version o)      (short))
        (assert (< #x400 (ndpdic-version o)) nil
                "Version of PDIC `%s' file is old (%04X) and not supported!" 
                file (ndpdic-version o))
        (setf (ndpdic-lword o)        (short))
        (setf (ndpdic-liapa o)        (short))
        (setf (ndpdic-block-size o)   (short))
        (setf (ndpdic-index-block o)  (short))
        (setf (ndpdic-header-size o)  (short))
        (setf (ndpdic-index-size o)   (short))
        (setf (ndpdic-empty-block o)  (short))
        (setf (ndpdic-nindex o)       (short))
        (setf (ndpdic-nblock o)       (short))
        (setf (ndpdic-nword o)        (ulong))
        (setf (ndpdic-dicorder o)     (byte))
        (setf (ndpdic-dictype o)      (byte))
        (setf (ndpdic-attrlen o)      (byte))
        (setf (ndpdic-os o)           (byte))
        (setf (ndpdic-olenumber o)    (long))
        (setf (ndpdic-dummy-lid o)    (char 10))
        (setf (ndpdic-index-blkbit o) (byte))
        (setf (ndpdic-dummy0 o)       (byte))
        (setf (ndpdic-extheader o)    (ulong))
        (setf (ndpdic-empty-block2 o) (long))
        (setf (ndpdic-nindex2 o)      (ulong))
        (setf (ndpdic-nblock2 o)      (ulong))
        (setf (ndpdic-crypt o)        (char 8))
        (setf (ndpdic-update-count o) (ulong))
        (setf (ndpdic-dummy00 o)      (char 4))
        (setf (ndpdic-dicident o)     (char 8))
        (setf (ndpdic-derefid o)      (char 8))
        (setf (ndpdic-dummy o)        (char 24))
        ;;
        (setf (ndpdic-block-index o) (make-vector (ndpdic-nindex2 o) nil))
        (setf (ndpdic-block-entries-hash o) (make-hash-table :test 'equal))
        )
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally
         file nil (ndpdic-index-start o)
         (ndpdic-data-start o) (ndpdic-data-start o))
        (loop for i from 0 below (ndpdic-nindex2 o) do
              (aset (ndpdic-block-index o) i
                    (if (= (ndpdic-index-blkbit o) 0) (short) (long)))
              (ndpdic-proceed-to-null)))
      o ;; single `o' char.
      )))

;;;
;;; Basic Functions
;;;

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

(defun ndpdic-index-start (ndpdic)
  "Index start point of NDPDIC."
  (+ (ndpdic-header-size ndpdic)
     (ndpdic-extheader ndpdic)))

(defun ndpdic-data-start (ndpdic &optional block)
  "Data start point in NDPDIC of BLOCK number."
  (unless block (setq block 0))
  (+ (ndpdic-index-start ndpdic) ; 1024
     (* (ndpdic-block-size ndpdic) ; 1024
        (+ (ndpdic-index-block ndpdic) block))))

(defun ndpdic-proceed-to-null ()
  "Proceed to next point of null character or eobp.
`char-before' a new point should be null character."
  (interactive)
  (search-forward " " nil t))

(defun ndpdic-proceed-to-null-string ()
  "Proceed to next point of null character or eobp.
`char-before' a new point should be null character."
  (interactive)
  (let ((start-point (point)))
    (search-forward " " nil t)
    (buffer-substring start-point (point))))

(defun ndpdic-insert-block-contents (ndpdic block)
  "Insert content of NDPDIC's BLOCK to current buffer.
Return a size of `Field-Length' of the block."
  (let* ((file (ndpdic-filename ndpdic))
         (start (ndpdic-data-start ndpdic block))
         (block-size (ndpdic-block-size ndpdic))
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
      (ndpdic-proceed-to-null)
      (setq word-data (buffer-substring start (1- (point))))
      (setq word-data (concat (substring prev-word-data 0 compress) word-data))
      (setq content-start (point))
      (goto-char (+ start field-size))
      (list (bocu-to-str word-data)
            kind content-start word-data))))

(defun ndpdic-entries (ndpdic block)
  "Get all entries in NDPDIC at BLOCK.
Return the list of entry words.  Result will be cached."
  (if (null block) nil
    (assert (integerp block))
    (let ((block-entries-hash (ndpdic-block-entries-hash ndpdic))
          fl-size word-spec (word-data "") words)
      (or
       (gethash block block-entries-hash)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (setq fl-size (ndpdic-insert-block-contents ndpdic block))
         (goto-char (+ 2 (point-min)))
         (while (not (eobp))
           (setq word-spec (ndpdic-entries-next-word word-data fl-size))
           (if (null word-spec) (goto-char (point-max))
             (setq words (cons (car word-spec) words))
             (setq word-data (elt word-spec 3))))
         (puthash block (nreverse words) block-entries-hash))))))

(defun ndpdic-entry-content (ndpdic block entry)
  "Get content of FILE, BLOCK, and  ENTRY."
  (let* (fl-size word-spec (word-data "") content)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq fl-size (ndpdic-insert-block-contents ndpdic block))
      (goto-char (+ 2 (point-min)))
      (while (not (eobp))
        (setq word-spec (ndpdic-entries-next-word word-data fl-size))
        (setq word-data (elt word-spec 3))
        (if (null word-spec) (goto-char (point-max))
          (when (equal entry (car word-spec))
            (setq content
                  (ndpdic-adjust-content
                   entry (elt word-spec 1)
                   (elt word-spec 2) (point)))
            (goto-char (point-max)))))
      content)))

(defun ndpdic-adjust-content (entry kind from to &optional field-size-length)
  "Retrieve ENTRY contents of KIND from FROM to TO buffer.
Optional argument FIELD-SIZE-LENGTH specifies size of binary data length field."
  (let (;(word-level (logand #x0f kind))
        (extended   (logand #x10 kind))
        ;(memorize   (logand #x20 kind))
        ;(modified   (logand #x40 kind))
        extended-data start ext-val data-item)
    ;; Parse Extended Data
    (when (/= extended 0)
      (save-restriction
        (unless field-size-length (setq field-size-length 2))
        (narrow-to-region from to)
        (goto-char (point-min))
        (ndpdic-proceed-to-null)
        (setq extended-data
              (list (cons 0 (buffer-substring from (1- (point))))))
        (while (not (eobp))
          (setq ext-val (ndpdic-buffer-byte))
          (setq start (point))
          (if (/= 0 (logand #x40 ext-val))
              ;; binary
              (let ((length (if (eq field-size-length 2)
                                (ndpdic-buffer-short)
                              (ndpdic-buffer-int))))
                (goto-char (+ start length))
                (setq data-item (buffer-substring start (point))))
            ;; text
            (ndpdic-proceed-to-null)
            (setq data-item (buffer-substring start (1- (point)))))
          (setq extended-data (cons (cons ext-val data-item)
                                    extended-data)))
        (setq extended-data (nreverse extended-data))))
    ;; Contents for Display
    (concat
     ;; entry part
     (if (string-match "	" entry)
         (substring entry (match-end 0)) entry)
     "\n"
     (if (= extended 0)
         (bocu-to-str (buffer-substring from to))
       (mapconcat (lambda (x)
                    (if (assq (car x) ndpdic-extended-attributes)
                        (apply (cdr (assq (car x) ndpdic-extended-attributes))
                               (list (cdr x)))))
                  extended-data "")))))

;;; binary search

(defun ndpdic-compare-entry (entry entries)
  "Check if ENTRY is larger, smaller, or inclusive of ENTRIES.
Comparison is done lexicographicaly.
If larger, t. smaller, nil.  equal, 0 will be returned."
  (assert (stringp entry) t "ENTRY `%s' must be string.")
  (if (string-lessp entry (car entries)) nil
    (if (string-lessp (car (last entries)) entry) t 0)))

(defun ndpdic-binary-search (ndpdic entry)
  "Find block in NDPDIC which includes string ENTRY.
Return value would be (block . next-block)."
  (assert (stringp entry) t "ENTRY `%s' must be string.")
  (let* ((block-index (ndpdic-block-index ndpdic))
         (start 0) (end (length block-index))
         (middle (/ end 2))
         (entries (ndpdic-entries ndpdic (aref block-index middle)))
         result)
    (while
        (progn
          (setq result (ndpdic-compare-entry entry entries))
          (not (or (and (numberp result) (= 0 result))
                   (= start middle))))
      (if result (setq start middle)
        (setq end middle))
      (setq middle (/ (+ start end) 2))
      (setq entries (ndpdic-entries ndpdic (aref block-index middle))))
    (if (and (numberp result) (= 0 result))
        (cons (aref block-index middle)
              (if (/= (1- (length block-index)) middle)
                  (aref block-index (1+ middle))))
      nil)))
        
(provide 'ndpdic)

;;; ndpdic.el ends here
