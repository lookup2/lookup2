;;; support-eijiro.el --- suport file for "Eijiro" -*- coding: utf-8 -*-
;; Copyright (C) 2009 KAWABATA, Taichi <kawabata.taichi@gmail.com>

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

;; Support file for text version of "英辞郎"

;;; Code:
(eval-when-compile (require 'cl))
(require 'cl-lib)

(defvar support-eijiro-option-list
  '(("/EIJI-"
     :title "英辞郎" :charsets (ascii))
    ("/REIJI" 
     :title "例辞郎" :charsets (ascii japanese-jisx0208))
    ("/RYAKU" 
     :title "略語辞典" :charsets (ascii))
    ("/WAEI" 
     :title "和英辞郎" :charsets (ascii japanese-jisx0208))))

(setq lookup-support-options
      (append 
       (cdr (cl-assoc-if (lambda (regexp) 
                        (string-match regexp lookup-support-dictionary-id))
                      support-eijiro-option-list))
       '(:content-tags ("\n" . "\n") 
         :entry-tags-list (("■" . "  ") ("■" . " : "))
         :head-tags ("■" . " : ")
         :code-tags ("■" . " : "))))

;;; support-eijiro.el ends here
