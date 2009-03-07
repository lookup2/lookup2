;;; colloc.el --- support file for 『新編 英和活用大辞典』
;; Copyright (C) 2000 Keisuke Nishida <knsihida@ring.gr.jp>

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

(require 'lookup)


(defconst colloc-gaiji-table
  (lookup-new-gaiji-table
   '(
     ("ha121" . "*")
     ("ha123" "Ç")
     ("ha124" "ə")
     ("ha125" "ʃ")
     ("ha126" "ŋ")
     ("ha127" "ː")
     ("ha128" "ɛ̃")
     ("ha129" "ɑ̃")
     ("ha12a" "ɑ")
     ("ha12b" "ñ")
     ("ha12c" "〓[$]") ;; # 縦線が２つある。
     ("ha130" "ɔ́")
     ("ha131" "É")
     ("ha132" "á")
     ("ha133" "é")
     ("ha134" "í")
     ("ha135" "à")
     ("ha136" "è")
     ("ha137" "ë")
     ("ha138" "ï")
     ("ha139" "ö")
     ("ha13a" "â")
     ("ha13b" "ê")
     ("ha13c" "ô")
     ("ha13d" "ō")

     ("zb121" "[Ref.]")
     ("zb122" "☞")
     ("zb123" "✓")
     ("zb124" "嗉")
     ("zb125" "胳")
     ("zb126" "骶")
     ("zb127" "稃")
     ("zb128" "炻")
     ("zb129" "絇")
     ("zb12a" "蒴")
     ("zb12b" "©")
     ("zb12c" "☨")
     ("zb12d" "↑")
     ("zb12e" "SeeText")
     ("zb121" . "━"))))


(defun colloc-arrange-first (entry)
  (if (re-search-forward " <reference>→<gaiji=zb12e>.*" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
  (when (looking-at "【.*】")
    (forward-line)
    (if (re-search-forward "^【.*】" nil t)
	(delete-region (match-beginning 0) (point-max)))
    (goto-char (point-min)))
  (if (re-search-forward " <reference>→<gaiji=zb12d>.*" nil t)
      (let ((string (match-string 0)))
	(goto-char (point-min))
	(end-of-line)
	(insert string)))
  (while (re-search-forward " <reference>→<gaiji=zb12d>.*" nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(setq lookup-support-options
      (list :title "英和活用大辞典"
	    :arranges '((replace colloc-arrange-first))
	    :transformer 'lookup-stem-english-search))

;;; colloc.el ends here
