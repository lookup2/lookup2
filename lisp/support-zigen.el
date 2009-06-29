;;; support-zigen.el --- support file for "字源"
;; Copyright (C) 2009 Lookup Development Team

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

;;; Documentation:

;; This support file provides searching capability to the "字源" XML
;; dictionary file for that character.  Original files can be
;; downloaded from `http://wagang.econ.hc.keio.ac.jp/zigen'.
;;
;; Download 214 xml files (from 001.xml to 214.xml) in
;; `http://wagang.econ.hc.keio.ac.jp/zigen' directory, concatenate all
;; of them to make one single `zigen.xml' file, then create index
;; point file to be used with sary.  Following script file will do all
;; the above.
;;
;; #/bin/sh
;; i=1
;; while [ $i -le 215 ]; do
;; wget http://wagang.econ.hc.keio.ac.jp/zigen/$(printf '%02d' $i).xml
;; i=`expr $i + 1`
;; done
;; cat [0-9]*.xml > zigen.xml
;; rm [0-9]*.xml
;; mksary -c utf-8 zigen.xml

;;; Usage
;;
;;  Example:
;;   (setq lookup-search-agents
;;         '(
;;           ....
;;           (ndsary "~/edicts/zigen/")
;;           ....)

;;; Code:

(require 'lookup)

(defvar support-zigen-use-ivs-font t)

;;;
;;; Internal Variables
;;;

(defvar support-zigen-kanji-on-start "
					<音>")
(defvar support-zigen-jukugo-on-start "
				<音>")
(defvar support-zigen-on-end "</音>")

;;;
;;; Search Support
;;;

(defun support-zigen-entry-tags-list (string method)
  (if (string-match "^[ァ-ヺ]+$" string)
      (list (cons support-zigen-kanji-on-start  support-zigen-on-end)
            (cons support-zigen-jukugo-on-start support-zigen-on-end))
    (if (and (string-match "^[㐀-鿿𠀀-𮿿]$" string)
             (or (equal method 'exact) (equal method 'prefix)))
        '(("<見出字>" . "</見出字>"))
      '(("<見出字>" . "</見出字>") ("<見出語>" . "</見出語>")))))

(defun support-zigen-content-tags (x)
  (if (consp x) (setq x (car x)))
  (cond ((string-match "<見出語>" x) '("<熟語>" . "</熟語>"))
        ((string-match support-zigen-jukugo-on-start x) 
         '("<熟語>" . "</熟語>"))
        ;;((string-match "<見出字>" x) '("<漢字>" . "</漢字>"))
        ;;((string-match support-zigen-kanji-on-start x) 
        ;;                           '("<漢字>" . "</漢字>"))
        (t '("<漢字>" . "</漢字>"))))

(defun support-zigen-code-tags (x)
  (if (consp x) (setq x (car x)))
  (cond ((string-match "<見出語>" x) nil)
        ((string-match "<見出字>" x) nil)
        ((string-match support-zigen-jukugo-on-start x) 
         '("<見出語>" . "</見出語>"))
        ((string-match support-zigen-kanji-on-start x) 
         '("<見出字>" . "</見出字>"))
        (t (error "No proper CODE found!"))))

(defun support-zigen-arrange-structure (entry)
  "Arrange content of ENTRY."
  (if support-zigen-use-ivs-font
      (lookup-text-new-to-old-kanji-ivs-region (point-min) (point-max)))
  (goto-char (point-min))
  (while (search-forward "	" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\(</.+?>\\)\n" nil t) (replace-match "\\1"))
  (goto-char (point-min))
  (while (re-search-forward "<見出字>.+</見出字>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 4.0)))))
  (goto-char (point-min))
  (while (re-search-forward "<見出語>.+</見出語>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(display ((height 2.0)) face lookup-heading-2-face)))
  (goto-char (point-min))
  (while (re-search-forward "<音>.+</音>" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(face lookup-heading-3-face)))
  (goto-char (point-min))
  (while (search-forward "<標識>" nil t) (replace-match "《"))
  (goto-char (point-min))
  (while (search-forward "</標識>" nil t) (replace-match "》"))
  (goto-char (point-min))
  (while (search-forward "<音>" nil t) (replace-match "[音]"))
  (goto-char (point-min))
  (while (search-forward "<韻>" nil t) (replace-match "[韻]"))
  (goto-char (point-min))
  (while (search-forward "<字解註>" nil t) (replace-match "[字解註]"))
  (goto-char (point-min))
  (while (re-search-forward "<返点 type=\"\\(.\\)\"/>" nil t)
    (let ((char (match-string 1)))
      (add-text-properties 0 1 '(display ((raise -0.3) (height 0.8))) char)
      (replace-match char)))
  (goto-char (point-min))
  (while (re-search-forward "<.+?>" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n\n+" nil t) (replace-match "\n"))
  (goto-char (point-min))
  (if (and (looking-at "$") (not (= (point-min) (point-max))))
      (delete-region (point-min) (1+ (point-min)))))

(setq lookup-support-options
      (list :title "字源"
            :coding 'utf-8-dos
            :query-filter 'lookup-query-filter-hiragana-to-katakana
            :charsets 
            (lambda (x) (string-match "^\\([㐀-鿿𠀀-𮿿]+\\|[ァ-ヺ]+\\)$" x))
            :entry-tags-list #'support-zigen-entry-tags-list
            :content-tags 'support-zigen-content-tags
            :code-tags 'support-zigen-code-tags
            :arranges '((replace support-zigen-arrange-structure))))

;;; support-zigen.el ends here
