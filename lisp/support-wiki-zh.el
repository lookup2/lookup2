;;; support-wiki-zh.el --- support for Chinese "Wikipedia" Abstract XML file.
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

;; Download site:
;; http://download.wikipedia.org/zhwiki/latest/

;;; Code:

(setq lookup-support-options
      (list :title "Wikipedia (zh)"
            :charsets '(ascii chinese-gb2312 chinese-cns11643-1 chinese-cns11643-2)
            :entry-tags '("<title>Wikipedia: " . "</title>")))

;;; support-wiki-zh.el ends here
