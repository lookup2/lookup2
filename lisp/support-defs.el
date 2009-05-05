;;; support-defs.el --- autoload definition of support files
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

(setq lookup-support-autoload-alist
      '(
        ;; EB/EPWING Dictionaries
        ("/IWAKOKU\\'" . "support-iwakoku")
	("/CHUJITEN\\'" . "support-chujiten")
	("/CHIEZO\\'" . "support-chiezo")
	("/MYPAEDIA\\'" . "support-mypaedia")
        ("/CROWN_DJ\\'" . "support-crown")
        ("/CROWN_FJ\\'" . "support-crown")
        ("/KOJIEN" . "support-kojien")
        ("/DAIJIRIN" . "support-daijirin")
	("/GENIUS\\'" . "support-genius")
	("/COLLOC\\'" . "support-colloc")
        ("^[^2]+/PLUS\\'" . "support-plus")
        ("V2.*/PLUS\\'" . "support-plusv2")
	("/RIKA\\'" . "support-rika")
	("/COMP\\'" . "support-comp")
	("/OXFORD\\'" . "support-oxford")
        ("/ZHONG_RI\\'" . "support-zhongri")
        ("/MWSEDIC\\'" . "support-mwsedic")
        ("/CEBD\\'" . "support-cebd")
        ("/SKP\\'" . "support-skp")
        ("/PDH\\'" . "support-pdh")
        ;; ndsary
        ("ndsary:.*/jawiki.*-abstract\\.xml" . "support-wikipedia")
        ("ndsary:.*/enwiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("ndsary:.*/frwiki.*-abstract\\.xml" . "support-wikipedia")
        ("ndsary:.*/zhwiki.*-abstract-.*\\.xml" . "support-wikipedia")
        ("ndsary:.*/swjz\\.xml" . "support-swjz")
        ("ndsary:.*/sbgy\\.xml" . "support-sbgy")
        ("ndsary:.*\\.sdic" . "support-sdic")
        ("ndsary:.*/zigen.*all\\.xml" . "support-zigen")
        ("ndsary:.*/Unihan" . "support-unihan")
        ("ndsary:.*/JMdict" . "support-jmdict")
        ("ndsary:.*/chinadat\\.csv" . "support-chinakan")
        ;; ndbtonic
        ("ndbtonic:.*/onmusic/" . "support-onmusic")
        ;; ndpdic
        ("ndpdic:.*/eijiro/" . "support-eijiro")
        ;; ndsimple
        ("ndsimple:.*/rfc4949\\.txt" . "support-rfc4949")
	("ndsimple:.*/rfc1983\\.txt" . "support-rfc1983")
        ;; ndsrd
        ("ndsrd:" . "support-srd")
        ;; ndjitsuu
        ("ndjitsuu:" . "support-jitsuu")
        ;; misc.
	("/foldoc" . "support-foldoc")
	("/jargon" . "support-jargon")
	("/vera" . "support-vera")
	("/nhd" . "support-nhd")))

;;; support-defs.el ends here
