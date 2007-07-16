;;; chiezo.el --- support file for 『知恵蔵１９９７』
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

(defconst chiezo-gaiji-table
  (lookup-new-gaiji-table
   '(("ha621" "*") ("ha622" ",00(B") ("ha623" ",Aa") ("ha624" ",Ai") ("ha625" ",As")
     ("ha626" ",A`") ("ha627" ",Ah") ("ha628" ",Ab") ("ha629" ",Aj") ("ha62a" ",An")
     ("ha62b" ",At") ("ha62c" ",Ad") ("ha62d" ",Ak") ("ha62e" ",Av") ("ha62f" ",A|")
     ("ha630" ",Aq") ("ha634" ",Am") ("ha637" ",A{") ("ha638" ",Az")
     ("ha63d" nil "2") ("ha63e" nil "3") ("ha63f" nil "0") ("ha640" nil "1")
     ("ha641" nil "2") ("ha642" nil "3") ("ha643" nil "4") ("ha644" nil "5")
     ("ha645" nil "6") ("ha646" nil "8") ("ha647" nil "1") ("ha648" nil "4")
     ("ha649" nil "5") ("ha64a" nil "7") ("ha64b" nil "8") ("ha64c" nil "9")
     ("ha64d" nil "a") ("ha64e" nil "G") ("ha64f" nil "P") ("ha652" nil "+")
     ("ha653" nil "-") ("ha654" nil "+") ("ha657" nil "0") ("ha658" nil "6")
     ("ha659" ",AE") ("ha65a" nil "n")
     ("za423" "⇔") ("za424" nil "[TEL]") ("za428" "(R)") ("za43f" "(C)")
     ("za44a" "[海]") ("za44b" "[新]") ("za460" "√2"))))

(defconst chiezo-arrange-table
  '((structure . chiezo-arrange-structure)))

(defun chiezo-arrange-structure (entry)
  (lookup-arrange-structure entry)
  (goto-char (point-min))
  (when (re-search-forward "^<.*>$" nil t)
    (lookup-make-region-heading (match-beginning 0) (match-end 0) 2)))

(setq lookup-support-options
      (list :gaiji-table chiezo-gaiji-table
	    :arrange-table chiezo-arrange-table))

;;; chiezo.el ends here
