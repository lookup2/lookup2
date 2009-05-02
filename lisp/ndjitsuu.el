;;; ndjitsuu.el --- Lookup `jitsuu' interface -*- coding: utf-8 -*-

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

;; ndjitsuu.el provides the interface for 「字通」辞書.
;;
;; Usage:
;;
;; (setq lookup-search-agents
;;       '(
;;         ....
;;         (ndjitsuu "~/edicts/Jitsuu"        ; location where `DATA' directory exists.
;;            :fonts "~/edicts/Jitsuu/Fonts") ; JitsuuXX.ttf fonts directory
;;         ....
;;         ))

;;; Code:

(require 'lookup)



;;;
;;; Customizable Variables
;;;

(defvar ndjitsuu-tmp-directory (expand-file-name (concat temporary-file-directory "/ndjitsuu"))
  "Temporafy Directory for NDJitsuu file.")

(defvar ndjitsuu-convert-program "convert") ;; ImageMagick

(defvar ndjitsuu-convert-program-options 
  '("-background"  "black"  "-fill" "white" "-transparent" "black"))

(defvar ndjitsuu-font-size-offset 4)

;;;
;;; Internal variables
;;;

;; HONMON
(defvar ndjitsuu-dat-file "~/edicts/Jitsuu/DATA/DAT/HONMON.DAT")
(defvar ndjitsuu-inf-file "~/edicts/Jitsuu/DATA/DAT/HONMON.INF")
(defvar ndjitsuu-inf-header 8)
(defvar ndjitsuu-inf-entry-number 7232)
(defvar ndjitsuu-inf-table-size 8) ; start (4) length (4)

;; OYAJI
(defvar ndjitsuu-oyaji-file "~/edicts/Jitsuu/DATA/LST/OYAJI.LST")
(defvar ndjitsuu-oyaji-header-length 32)
(defvar ndjitsuu-oyaji-number 7232)
(defvar ndjitsuu-oyaji-entry-size 190)

;; JUKUGO
(defvar ndjitsuu-jukugo-file "~/edicts/Jitsuu/DATA/LST/JUKUGO.LST")
(defvar ndjitsuu-jukugo-header-length 32)
(defvar ndjitsuu-jukugo-number 126189)
(defvar ndjitsuu-jukugo-entry-size 102)

;; Fonts
(defvar ndjitsuu-font-directory "~/edicts/Jitsuu/Fonts/")

;; INI
(defvar ndjitsuu-st-code
  '(( 0 nil   0 'normal)
    ( 1 "01"  0 'normal) ; for search
    (11 "11"  0 'normal) ; for search
    (12 "12"  0 'normal) ; for search
    (13 "13"  0 'normal)
    (14 "14"  0 'normal)
    (15 "15"  0 'normal)
    (24 "14"  0 'extra)
    (25 "15"  0 'extra)
    (30 "01"  0 'large)
    (31 "01"  0 'large)
    (32 "01"  0 'large)
    (33 "11"  0 'large)
    (34 "11"  0 'large)
    (35 "11"  0 'large)
    (36 nil   2 'normal) ; gothic
    (37 nil   6 'normal) ; mincho
    (38 nil   2 'normal) ; gothic
    (39 nil   0 'large)  ; mincho
    (40 "01"  6 'normal)
    (41 "11"  6 'normal)
    (42 "12"  6 'normal)
    (43 "13"  6 'normal)
    (44 "14"  6 'normal)
    (45 "15"  6 'normal)
    (50 nil   0 'normal) ; Times Roman
    (51 "H1"  0 'normal)
    (70 nil   8 'normal) ; mincho
    (71 "01"  8 'normal)
    (72 "01"  8 'large)
    (80 nil  16 'normal) ; mincho
    (81 "01" 16 'normal)
    ))

(defvar ndjitsuu-font-size
  '((normal . 16) (large . 24) (extra . 48)))

(defvar ndjitsuu-font-face
  '(( 2 . lookup-heading-1-face)
    ( 6 . lookup-heading-4-face)
    ( 8 . lookup-heading-3-face)
    (16 . lookup-heading-2-face)))

(defvar ndjitsuu-image-links
  '((0 . "jitsu_00001.tif")
    (1 . "jitsu_00001.tif")
    (2 . "jitsu_00004.tif")
    (3 . "jitsu_00005.tif")
    (4 . "jitsu_00006.tif")
    (5 . "jitsu_00007.tif")))

(defvar ndjitsuu-gaiji-table nil)

;;;
;;; Interface functions
;;;

(put 'ndjitsuu :methods 'ndjitsuu-methods)
(defun ndjitsuu-methods (dictionary)
  '(exact prefix suffix substring))

(put 'ndjitsuu :list 'ndjitsuu-list)
(defun ndjitsuu-list (agent)
  "Return list of dictionaries of AGENT."
  (if (null (file-directory-p ndjitsuu-tmp-directory))
      (make-directory ndjitsuu-tmp-directory))
  (let* ((location (lookup-agent-location agent)))
    (setq ndjitsuu-dat-file (expand-file-name
                             (concat location "/DATA/DAT/HONMON.DAT"))
          ndjitsuu-inf-file (expand-file-name
                             (concat location "/DATA/DAT/HONMON.INF"))
          ndjitsuu-oyaji-file (expand-file-name
                               (concat location "/DATA/LST/OYAJI.LST"))
          ndjitsuu-jukugo-file (expand-file-name
                                (concat location "/DATA/LST/JUKUGO.LST"))
          ndjitsuu-font-directory (expand-file-name
                                   (lookup-agent-option agent :fonts)))
    (when (and (file-exists-p ndjitsuu-dat-file)
               (file-exists-p ndjitsuu-font-directory))
      (list (lookup-new-dictionary agent "")))))

(put 'ndjitsuu :title 'ndjitsuu-title)
(defun ndjitsuu-title (dictionary)
  "【字通】")

(put 'ndjitsuu :search 'ndjitsuu-search)
(defun ndjitsuu-search (dictionary query)
  "Return entries list of DICTIONARY for QUERY."
  (let* ((string (lookup-query-string query))
         (number (string-to-number string)))
    (when (and (< 0 number) (<= number ndjitsuu-oyaji-number))
      (list (lookup-new-entry 'regular dictionary string
                               (ndjitsuu-oyaji number))))))

(put 'ndjitsuu :content 'ndjitsuu-content)
(defun ndjitsuu-content (entry)
  "Return string content of ENTRY."
  (let* ((code (lookup-entry-code entry))
         (index (string-to-number code))
         (dictionary (lookup-entry-dictionary entry))
         (prev
          (if (/= index 1)
              (lookup-new-entry 'regular dictionary
                                (number-to-string (1- index))
                                (ndjitsuu-oyaji (1- index)))))
         (next
          (if (/= index ndjitsuu-oyaji-number)
              (lookup-new-entry 'regular dictionary
                                (number-to-string (1+ index))
                                (ndjitsuu-oyaji (1+ index))))))
    (concat
     (ndjitsuu-inf-entry index)
     (when prev
       (lookup-put-property entry :preceding prev)
       (let ((text (concat "\n前項目：" (lookup-entry-heading prev))))
         (lookup-set-link 5 (length text) prev text)
         text))
     (when next
       (lookup-put-property entry :following next)
       (let ((text (concat "\n次項目：" (lookup-entry-heading next))))
         (lookup-set-link 5 (length text) next text)
         text))
    )))

(put 'ndjitsuu :arranges
     '((replace ndjitsuu-arrange-replace)))

(put 'ndjitsuu :reference-pattern
     '("<REF,\\([0-9]+\\)>\\(.+?\\)</REF>" 2 2 1))

(put 'nducs :charsets (lambda (x) (string-match "^[0-9]+$" x)))

;;;
;;; Main Program
;;;

(define-ccl-program ndjitsuu-decode
  '(1
    ;; CCL main code
    ((loop
      (r0 = #xff)
      (r1 = #xff)
      (r2 = #xff)
      (r3 = #xff)
      (read r0)
      (read r1)
      (read r2)
      (read r3)
      (r4 = (r0 << 8))
      (r4 += r1)
      (r4 ^= #xffff)
      (r4 += #x8831)
      (r5 = (r2 << 8))
      (r5 += r3)
      (r5 ^= #xffff)
      (r5 += #xb311)
      (r4 += (r5 >> 16))
      (r0 = ((r4 >> 8) & #xff))
      (r1 = (r4 & #xff))
      (r2 = ((r5 >> 8) & #xff))
      (r3 = (r5 & #xff))
      ;; check
      (if (r0 != 0)
          ((write r0)
           (if (r1 != 0)
               ((write r1)
                (if (r2 != 0)
                    ((write r2)
                     (if (r3 != 0)
                         ((write r3)
                          (repeat)))))))))))
    ((r6 = (r0 != 0))
     (r6 &= (r1 != 0))
     (r6 &= (r2 != 0))
     (r6 &= (r3 != 0))
     (if (r6 != 0)
         (if (r0 != #xff)
             ((r0 ^= #xff)
              (write r0)
              (if (r1 != #xff)
                  ((r1 ^= #xff)
                   (write r1)
                   (if (r2 != #xff)
                       ((r2 ^= #xff)
                        (write r2)))))))))))

(defsubst ndjitsuu-file-contents-literally (file from length)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally 
     file nil from (+ from length))
    (buffer-string)))

(defun ndjitsuu-file-contents (file from length)
  "Decode coding region"
  (substring-no-properties
   (decode-coding-string
    (string-make-unibyte
     (ccl-execute-on-string 
      'ndjitsuu-decode (make-vector 9 nil)
      (ndjitsuu-file-contents-literally
       file from length))) 'cp932)))

(defun ndjitsuu-str-to-int (str &optional pos)
  (if (null pos) (setq pos 0))
  (+ (* (elt str (+ pos 3)) 16777216)
     (* (elt str (+ pos 2)) 65536)
     (* (elt str (+ pos 1)) 256)
     (elt str pos)))

(defun ndjitsuu-file-read-int (file pos)
  (ndjitsuu-str-to-int
   (ndjitsuu-file-contents-literally file pos 4)))

(defun ndjitsuu-forward-char (num &optional after-tag)
  (while (< 0 num)
    (while (looking-at "<.+?>") (goto-char (match-end 0)))
    (if (< (char-after (point)) 128) (setq num (1- num)) (setq num (- num 2)))
    (forward-char))
  (if after-tag (while (looking-at "<.+?>") (goto-char (match-end 0)))))

(defsubst ndjitsuu-inf-file-read-int (pos)
  (ndjitsuu-file-read-int ndjitsuu-inf-file pos))

(defsubst ndjitsuu-inf-file-contents (pos length)
  (ndjitsuu-file-contents-literally ndjitsuu-inf-file pos length))

(defun ndjitsuu-inf-entry (index)
  "Return content of INDEX."
  (let* ((entry-addr (+ ndjitsuu-inf-header
                       (* (1- index) ndjitsuu-inf-table-size)))
         (entry-start
          (ndjitsuu-inf-file-read-int entry-addr))
         (datinfo-start
          (ndjitsuu-inf-file-read-int entry-start))
         (fontspec-start
          (ndjitsuu-inf-file-read-int (+ entry-start 8)))
         (fontspec-length
          (ndjitsuu-inf-file-read-int (+ entry-start 12)))
         (fontspecs
          (ndjitsuu-inf-file-contents fontspec-start fontspec-length))
         (refspec-start
          (ndjitsuu-inf-file-read-int (+ entry-start 16)))
         (refspec-length
          (ndjitsuu-inf-file-read-int (+ entry-start 20)))
         (refspecs
          (ndjitsuu-inf-file-contents refspec-start refspec-length))
         (anchorspec-start
          (ndjitsuu-inf-file-read-int (+ entry-start 24)))
         (anchorspec-length
          (ndjitsuu-inf-file-read-int (+ entry-start 28)))
         (anchorspecs
          (ndjitsuu-inf-file-contents anchorspec-start anchorspec-length))
         (dat-start
          (ndjitsuu-inf-file-read-int datinfo-start))
         (dat-length
          (ndjitsuu-inf-file-read-int (+ datinfo-start 8)))
         (dat
          (ndjitsuu-file-contents ndjitsuu-dat-file dat-start dat-length))
         (char-pos 0)
         start length val val2 current)
    (with-temp-buffer
      (insert dat)

      (goto-char (point-min))
      (setq char-pos 0 current 0)
      (while (< char-pos refspec-length)
        (setq start  (ndjitsuu-str-to-int refspecs char-pos))
        (setq length (ndjitsuu-str-to-int refspecs (+ char-pos 4)))
        (setq val    (ndjitsuu-str-to-int refspecs (+ char-pos 8)))
        (setq val2   (ndjitsuu-str-to-int refspecs (+ char-pos 12)))
        (setq char-pos (+ char-pos 20))
        (ndjitsuu-forward-char (- start current))
        (if (= val 2)
            (insert (format "<REF,%d>" val2))
          (insert (format "<REF,G%d>" val2)))
        (ndjitsuu-forward-char length)
        (insert "</REF>")
        (setq current (+ start length)))

      (goto-char (point-min))
      (setq char-pos 0 current 0)
      (while (< char-pos anchorspec-length)
        (setq val    (substring  anchorspecs char-pos (+ char-pos 4)))
        (setq start  (ndjitsuu-str-to-int anchorspecs (+ char-pos 4)))
        (setq length (ndjitsuu-str-to-int anchorspecs (+ char-pos 8)))
        (setq char-pos (+ char-pos 12))
        (ndjitsuu-forward-char (- start current))
        (insert (format "<A,%s>" val))
        (ndjitsuu-forward-char length)
        (insert "</A>")
        (setq current (+ start length)))

      (goto-char (point-min))
      (setq char-pos 0 current 0)
      (while (< char-pos fontspec-length)
        (setq start  (ndjitsuu-str-to-int fontspecs char-pos))
        (setq length (ndjitsuu-str-to-int fontspecs (+ char-pos 4)))
        (setq val    (ndjitsuu-str-to-int fontspecs (+ char-pos 8)))
        (setq char-pos (+ char-pos 12))
        (ndjitsuu-forward-char (- start current) t)
        (insert (format "<ST,%d>" val))
        (ndjitsuu-forward-char length)
        (insert "</ST>")
        (setq current (+ start length)))

      (buffer-string))))

(defun ndjitsuu-oyaji (index)
  (let* ((start (+ ndjitsuu-oyaji-header-length
                   (* ndjitsuu-oyaji-entry-size (1- index)))))
    (ndjitsuu-file-contents
     ndjitsuu-oyaji-file start ndjitsuu-oyaji-entry-size)))

(defun ndjitsuu-jukugo (index)
  (let* ((start (+ ndjitsuu-jukugo-header-length
                   (* ndjitsuu-jukugo-entry-size (1- index)))))
    (ndjitsuu-file-contents
     ndjitsuu-jukugo-file start ndjitsuu-jukugo-entry-size)))

;; Font Image

(defun ndjitsuu-font-image (font-code size code)
  "Create font image for FONT-CODE, SIZE and CODE.
Returns image file path."
  (let* ((font-file
          (expand-file-name
           (concat ndjitsuu-font-directory "/JITSUU" font-code ".TTF")))
         (image-file
          (expand-file-name
           (concat ndjitsuu-tmp-directory "/"
                              (format "%02d-%s-%04X" size font-code code) ".png")))
         (args (append ndjitsuu-convert-program-options
                       (list "-size" (format "%02dx%02d" 
                                             (+ ndjitsuu-font-size-offset size)
                                             (+ ndjitsuu-font-size-offset size))
                             "-font" font-file
                             (format "label:%c" code)
                             image-file))))
    ;;(message "debug: args=%s" args)
    (if (null (file-exists-p image-file))
        (lookup-with-coding-system 'utf-8
          (apply 'call-process ndjitsuu-convert-program nil nil nil args)))
    image-file))

;; Arrange Contents

(defun ndjitsuu-arrange-replace (entry)
  "Arrange contents of ENTRY."
  (while (re-search-forward "<ST,\\([0-9]+\\)>\\(.+?\\)</ST>" nil t)
    (let* ((st-code    (string-to-number (match-string 1)))
           (string     (match-string 2))
           (st-entry   (assq st-code ndjitsuu-st-code))
           (font-code  (elt st-entry 1))
           (font-face  (cdr (assq (eval (elt st-entry 2)) ndjitsuu-font-face)))
           (font-size  (cdr (assq (eval (elt st-entry 3)) ndjitsuu-font-size)))
           (gaiji      (car (lookup-gaiji-table-ref
                             ndjitsuu-gaiji-table
                             (format "%s-%X" font-code
                                     (string-to-char string)))))
           text-props
           (start    (match-beginning 0)) end)
      (replace-match (or gaiji string) t)
      (setq end (point))
      (if (and (null gaiji)
               font-code
               (not (equal font-code "01")))
          ;; Gaiji
          (let ((image (ndjitsuu-font-image font-code font-size (elt string 0))))
            (if (/= (length string) 1) (message "ndjitsuu: warining! length /= 1!!!"))
            (add-text-properties start end (list 'ndjitsuu-font-code font-code))
            (lookup-img-file-insert image 'png start end))
        ;; Text
        (if (and font-size
                 (/= font-size 18))
            (setq text-props (list 'display `((height ,(/ font-size 16.0))))))
        (if font-face (setq text-props (append text-props
                                               (list 'face font-face))))
        (if text-props (add-text-properties start end text-props)))))
  (goto-char (point-min))
  (while (re-search-forward "<A,\\([0-9]+\\)>\\(.\\)" nil t)
    (let ((match (match-string 2)))
      (add-text-properties 0 1
                           (list 'ndjitsuu-anchor
                                 (substring-no-properties (match-string 1)))
                           match)
      (replace-match match)))
  (goto-char (point-min))
  (while (re-search-forward "</A>" nil t) (replace-match ""))
  )

(provide 'ndjitsuu)

;;; ndjitsuu.el ends here
