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
   '(("ha621" "*") ("ha622" "œ") ("ha623" "á") ("ha624" "é") ("ha625" "ó")
     ("ha626" "à") ("ha627" "è") ("ha628" "â") ("ha629" "ê") ("ha62a" "î")
     ("ha62b" "ô") ("ha62c" "ä") ("ha62d" "ë") ("ha62e" "ö") ("ha62f" "ü")
     ("ha630" "ñ") ("ha634" "í") ("ha637" "û") ("ha638" "ú")
     ("ha63d" nil "2") ("ha63e" nil "3") ("ha63f" nil "0") ("ha640" nil "1")
     ("ha641" nil "2") ("ha642" nil "3") ("ha643" nil "4") ("ha644" nil "5")
     ("ha645" nil "6") ("ha646" nil "8") ("ha647" nil "1") ("ha648" nil "4")
     ("ha649" nil "5") ("ha64a" nil "7") ("ha64b" nil "8") ("ha64c" nil "9")
     ("ha64d" nil "a") ("ha64e" nil "G") ("ha64f" nil "P") ("ha652" nil "+")
     ("ha653" nil "-") ("ha654" nil "+") ("ha657" nil "0") ("ha658" nil "6")
     ("ha659" "Å") ("ha65a" nil "n")
     ("za423" "⇔") ("za424" nil "[TEL]") ("za428" "(R)") ("za43f" "(C)")
     ("za44a" "[海]") ("za44b" "[新]") ("za460" "√2"))))

(defun chiezo-arrange-structure (entry)
  (lookup-arrange-structure entry)
  (goto-char (point-min))
  (when (re-search-forward "^<.*>$" nil t)
    (lookup-make-region-heading (match-beginning 0) (match-end 0) 2)))

(setq lookup-support-options
      (list :gaiji-table chiezo-gaiji-table
	    :arranges '((structure chiezo-arrange-structure))))

;;; chiezo.el ends here
