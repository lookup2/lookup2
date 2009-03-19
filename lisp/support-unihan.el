;;; support-unihan.el --- support file for "Unihan" file.
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

;; This support-file will search the Unihan file distributed by
;; Unicode Consortium.  You will need to make the suffix array index
;; by "mksary" program.  (-l option should be attached.)
;;
;; Download site:
;; http://www.unicode.org/Unihan
;; 

;;; Code:

(require 'lookup)

(defvar support-unihan-kangxi-url-format
  "http://kangxizidian.com/kangxi/%04d.gif")

(defun support-unihan-transformer (dictionary query)
  (let* ((query-string (upcase (lookup-query-string query))))
    (if (string-match "^[㐀-鿿𠀀-𯿼]" query-string)
        (lookup-search-multiple 
         dictionary 
         (list (format "U+%X" (car (string-to-list query-string)))))
      (lookup-search-multiple dictionary (list (lookup-query-string query-string))))))

(defun support-unihan-arrange-structure (entry)
  "Arrange contents of ENTRY."
  (delete-non-matching-lines "^U")
  (goto-char (point-min))
  (let ((code (lookup-entry-code entry)))
    (if (string-match "^U\\+\\([0-9A-F]+\\)\\'" code)
        (insert (format "【%c】 %s\n"
                        (string-to-int (match-string 1 code) 16) code))
      (insert (lookup-entry-code entry ) "\n")))
  (if (re-search-forward "kKangXi	\\([0-9]+\\)\\.[0-9][0-9]0" nil t)
      (lookup-url-set-link
       (match-beginning 0) (match-end 0)
       (format support-unihan-kangxi-url-format
               (string-to-int (match-string 1)))))
  )

(setq lookup-support-options
      (list :title "Unihan"
            :arranges '((reference support-unihan-arrange-structure))
	    :transformer 'support-unihan-transformer
            :max-hits 100 :regular t))

;;; support-unihan.el ends here
