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

;; This agent will search the "字源" XML dictionary file for the
;; character.  Files can be downloaded from the following site:
;;
;; http://wagang.econ.hc.keio.ac.jp/zigen/
;;
;; Download 214 xml files (from 001.xml to 214.xml) in that directory,
;; then concatenate all of them to make one single `all.xml'
;; file in the directory named ".../zigen/".
;;
;; Following will make index points.
;; % mksary -c utf-8 all.xml

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

;; Internal Variables
(defvar support-zigen-kanji-content-start "<漢字>")
(defvar support-zigen-kanji-content-end   "</漢字>")
(defvar support-zigen-kanji-entry-start "<見出字>")
(defvar support-zigen-kanji-entry-end   "</見出字>")
(defvar support-zigen-kanji-on-start "
					<音>")
(defvar support-zigen-jukugo-content-start "<熟語>")
(defvar support-zigen-jukugo-content-end   "</熟語>")
(defvar support-zigen-jukugo-on-start "
				<音>")
(defvar support-zigen-on-end "</音>")
(defvar support-zigen-jukugo-entry-start "<見出語>")
(defvar support-zigen-jukugo-entry-end "</見出語>")

(defun support-zigen-arrange-structure (entry)
  "Arrange content of ENTRY."
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
  (if (looking-at "$") (delete-region (point-min) (1+ (point-min)))))

(defun support-zigen-entry-start-end-pairs (string method)
  (if (string-match "^[ァ-ヺ]+$" string)
      (list (cons support-zigen-kanji-on-start  support-zigen-on-end)
            (cons support-zigen-jukugo-on-start support-zigen-on-end))
    (if (string-match "^[㐀-鿿𠀀-𮿿]$" string)
        (list (cons support-zigen-kanji-entry-start support-zigen-kanji-entry-end))
      (list (cons support-zigen-jukugo-entry-start support-zigen-jukugo-entry-end)))))

(defun support-zigen-content-start (entry-start)
  (if (or (equal entry-start support-zigen-jukugo-entry-start)
          (equal entry-start support-zigen-jukugo-on-start))
      support-zigen-jukugo-content-start
    support-zigen-kanji-content-start))

(defun support-zigen-content-end (entry-start)
  (if (or (equal entry-start support-zigen-jukugo-entry-start)
          (equal entry-start support-zigen-jukugo-on-start))
      support-zigen-jukugo-content-end
    support-zigen-kanji-content-end))

(setq lookup-support-options
      (list :title "字源"
            :entry-start-end-pairs #'support-zigen-entry-start-end-pairs
            :coding 'utf-8-dos
            :charsets (lambda (x) (string-match "^\\([㐀-鿿𠀀-𮿿]+\\|[ァ-ヺ]+\\)$" x))
            :content-start "<漢字>" :content-end "</漢字>"
            :query-filter 'lookup-query-filter-hiragana-to-katakana
            :arranges '((replace support-zigen-arrange-structure))))

;;; support-zigen.el ends here
