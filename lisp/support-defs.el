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
        ("ndeb:.*/iwakoku\\'" . "support-iwakoku")
	("ndeb:.*/chujiten\\'" . "support-chujiten")
	("ndeb:.*/chiezo\\'" . "support-chiezo")
	("ndeb:.*/mypaedia\\'" . "support-mypaedia")
        ("ndeb:.*/crown_dj\\'" . "support-crown")
        ("ndeb:.*/crown_fj\\'" . "support-crown")
        ("ndeb:.*/kojien" . "support-kojien")
        ("ndeb:.*/daijirin" . "support-daijirin")
	("ndeb:.*/genius\\'" . "support-genius")
	("ndeb:.*/colloc\\'" . "support-colloc")
        ("ndeb:.*^[^2]+/plus\\'" . "support-plus")
        ("ndeb:.*v2.*/plus\\'" . "support-plusv2")
	("ndeb:.*/rika\\'" . "support-rika")
	("ndeb:.*/comp\\'" . "support-comp")
	("ndeb:.*/oxford\\'" . "support-oxford")
        ("ndeb:.*/zhong_ri\\'" . "support-zhongri")
        ("ndeb:.*/mwsedic\\'" . "support-mwsedic")
        ("ndeb:.*/cebd\\'" . "support-cebd")
        ("ndeb:.*/skp\\'" . "support-skp")
        ("ndeb:.*/pdh\\'" . "support-pdh")
        ;; ndsary
        ("ndsary:.*\\.sdic" . "support-sdic")
        ("ndsary:.*/swjz\\.xml" . "support-swjz")
        ("ndsary:.*/sbgy\\.xml" . "support-sbgy")
        ("ndsary:.*/xszd\\.txt" . "support-xszd")
        ("ndsary:.*/zigen.*\\.xml" . "support-zigen")
        ("ndsary:.*/Unihan" . "support-unihan")
        ("ndsary:.*/JMdict" . "support-jmdict")
        ("ndsary:.*/chinadat\\.csv" . "support-chinakan")
        ("ndsary:.*/waseikanji\\.html" . "support-waseikanji")
        ("ndsary:.*/JitsuuFuroku/b001.html" . "support-jtfrk")
        ("ndsary:.*/JitsuuFuroku/b002.html" . "support-jtfrk")
        ("ndsary:.*/JitsuuFuroku/f001.html" . "support-jtfrk")
        ("ndsary:.*/enwiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("ndsary:.*/frwiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("ndsary:.*/jawiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("ndsary:.*/zhwiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("ndsary:.*/Rangjung-Yeshe" . "support-rangjung")
        ;; ndbtonic
        ("ndbtonic:.*/onmusic/" . "support-onmusic")
        ;; ndpdic
        ("ndpdic:.*/eijiro/" . "support-eijiro")
        ;; ndsimple
        ("ndsimple:.*/rfc4949\\.txt" . "support-rfc4949")
	("ndsimple:.*/rfc1983\\.txt" . "support-rfc1983")
        ;; ndsrd
        ("ndsrd:" . "support-srd")
        ;; ndic
	("ndic:.*/foldoc" . "support-foldoc")
	("/jargon" . "support-jargon")
	("/vera" . "support-vera")
	("/nhd" . "support-nhd")))

;;; support-defs.el ends here
