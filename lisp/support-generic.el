;;; support-generic.el --- Generic Lookup Support File.
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup-utils)
(require 'jisx0213) ;; You will need Mule-UCS installed.

(defun range (from to)
  "Make the list of the integers of range FROM to TO."
  (let (result) 
    (while (<= from to) (setq result (cons to result) to (1- to))) result))

(defvar combining-ucs-above
  (nconc (range #x0300 #x0315)
         (range #x031a #x031b)
         (range #x033d #x0344)
         '(#x0346)
         (range #x034a #x034c)))

(defvar combining-ucs-below
  (nconc (range #x0316 #x0319)
         (range #x031c #x0333)
         (range #x0339 #x033c)
         '(#x0345)
         (range #x0347 #x0349)
         (range #x034d #x034e)))

(defvar combining-ucs-middle
  (range #x0334 #x0338))

(mapcar*
 (lambda (x)
   (let ((ucslist (car x)) (refpoint (cdr x)))
     (mapcar*
      (lambda (y)
        (put-char-code-property (lookup-ucs-char y)
                                'reference-point refpoint))
      (eval ucslist))))
 '((combining-ucs-above . (1 . 7))
   (combining-ucs-below . (7 . 1))
   (combining-ucs-middle . (10 . 10))))

(defmacro entity-ref-search-replace (regexp body)
  "For entity-references described as REGEXP, do EXPR and replaces
them with text properties saved."
  `(let (replace-str text-prop)
     (goto-char (point-min))
     (while (re-search-forward ,regexp nil t)
       (setq text-prop (text-properties-at (match-beginning 0)))
       (setq replace-str ,body)
       (and replace-str
            (progn
              (save-match-data
                (replace-match replace-str))
              (set-text-properties 
               (match-beginning 0) 
               (+ (match-beginning 0) (length replace-str))
               text-prop))))))

(defun decode-entity-reference-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (entity-ref-search-replace 
       "&#[xX]\\([0-9A-Fa-f]+\\);"
       (let ((ucschar 
              (lookup-ucs-char (string-to-int (match-string 1) 16))))
         (if ucschar (char-to-string ucschar))))
      (entity-ref-search-replace 
       "&#\\([0-9]+\\);"
       (let ((ucschar (lookup-ucs-char (string-to-int (match-string 1)))))
         (if ucschar (char-to-string ucschar))))
      (entity-ref-search-replace 
       "&J1?-\\([0-9][0-9]\\)\\([0-9][0-9]\\);"
       (char-to-string 
        (make-char 'japanese-jisx0213-1
                   (+ 32 (string-to-int (match-string 1)))
                   (+ 32 (string-to-int (match-string 2))))))
      (entity-ref-search-replace 
       "&J2-\\([0-9][0-9]\\)\\([0-9][0-9]\\);"
       (char-to-string 
        (make-char 'japanese-jisx0213-2
                   (+ 32 (string-to-int (match-string 1)))
                   (+ 32 (string-to-int (match-string 2))))))
      (entity-ref-search-replace
       "&G0-\\([0-9][0-9]\\)\\([0-9][0-9]\\);"
       (char-to-string 
        (make-char 'chinese-gb2312
                   (+ 32 (string-to-int (match-string 1)))
                   (+ 32 (string-to-int (match-string 2))))))
      (entity-ref-search-replace
       "&C0-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\);"
       (char-to-string 
        (decode-big5-char (string-to-int (match-string 1) 16))))
      (entity-ref-search-replace 
       "&C\\([1-7]\\)-\\([0-9A-F][0-9A-F]\\)\\([0-9A-F][0-9A-F]\\);"
       (char-to-string 
        (make-char (intern (concat "chinese-cns11643-" (match-string 1)))
                   (string-to-int (match-string 2) 16)
                   (string-to-int (match-string 3) 16))))
      (entity-ref-search-replace 
       "&K0-\\([0-9][0-9]\\)\\([0-9][0-9]\\);"
       (char-to-string 
        (make-char 'korean-ksc5601
                   (+ 32 (string-to-int (match-string 1)))
                   (+ 32 (string-to-int (match-string 2)))))))))

(defun combine-characters-region (from to)
  "Combine characters in the region.  
Character with non-nil reference-point property will be combined."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (let (refpoint compchar)
        (while (< from to)
          (when (setq refpoint (get-char-code-property (char-after from)
                                                       'reference-point))
            (if (setq compchar (find-composition (1- from) nil nil t))
                (if (elt compchar 3)
                    (compose-region (elt compchar 0)
                                    (1+ from)
                                    (elt compchar 1)
                                    (nconc (elt compchar 2)
                                           (list refpoint
                                                 (char-after from)))))
              (if (and (/= from (point-min))
                       (> (char-before from) #x020))
                  (compose-region (1- from) (1+ from)
                                  (list (char-before from)
                                        refpoint
                                        (char-after from)))))
            (setq from (1+ from)))
          (setq from (1+ from)))))))

(provide 'support-generic)
