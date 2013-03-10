;;; support-btonic-onmusic.el --- support file for "新編・音楽中辞典" file.
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

;; This support-file will provide the search functionality on 「新編・
;; 音楽中辞典」、provided by Ongaku-No-Tomo Company (音楽の友社).  
;;
;; The file is originally distributed by compressed-archived format of
;; ".exi".  You must decompress it by the btonic2xml110 tool.
;;
;; Usage:
;;
;; (setq lookup-search-agent
;;       '(
;;         ....
;;         (ndbtonic "/path_to/onmusic")
;;         ....
;;        ))

;;; Code:

(setq lookup-support-options
      (list :title "新編音楽中辞典"))

;;; support-onmusic.el ends here
