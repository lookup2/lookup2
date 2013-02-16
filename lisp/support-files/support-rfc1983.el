;;; support-rfc1983.el --- support file for "RFC1983"
;; Copyright (C) 2000 Keisuke Nishida <knsihida@ring.gr.jp>
;; Copyright (C) 2009 Taichi KAWABATA <kawabata.taichi@gmail.com>

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

;;; Documentation

;; This reference utility should be applied to internet glossary of RFC 1983. 
;; (RFC 1983 may be applicable, too.)
;; This is based on `rfc1983.el'.  
;;
;; pattern 1: See: XXXX, YYYY.
;; pattern 2: See: xxxxx under "XXXX", yyyyy under "YYYY".

;;; Code:

(defun support-rfc1983-arrange-references (entry)
  (goto-char (point-min))
  (while (re-search-forward "See\\(?:[ \n]+also\\)?:\\([^.]+\\)\\." nil t)
    (save-restriction
      (narrow-to-region (match-beginning 1) (match-end 1))
      (goto-char (point-min))
      (while (re-search-forward "[ \n]*\\([^,]+\\)" nil t)
        (let (heading code new-entry)
          (save-match-data
            (setq heading (lookup-oneline-string (match-string 1)))
            (if (string-match "\"\\(.+\\)\"" heading)
                (setq heading (match-string 1 heading)))
            (setq new-entry
                  (lookup-new-entry 'dynamic (lookup-entry-dictionary entry) heading heading)))
          (lookup-set-link (match-beginning 1) (match-end 1) new-entry)))
      (goto-char (match-end 0)))))

(setq lookup-support-options
      (list :title "RFC1983"
            :entry-start "\n   "
            :content-end "\n\n"
            :charsets '(ascii)
	    :arranges '((reference support-rfc1983-arrange-references))))

;;; support-rfc1983.el ends here
