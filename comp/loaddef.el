;;; loaddef.el --- autoload definition of complement files
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

(setq lookup-complement-autoload-alist
      '(("IWAKOKU\\'" . "iwakoku.el")
	("CHUJITEN\\'" . "chujiten.el")
	("CHIEZO\\'" . "chiezo.el")
	("MYPAEDIA\\'" . "mypaedia.el")
	("GENIUS\\'" . "genius.el")
	("COLLOC\\'" . "colloc.el")
	("RIKA\\'" . "rika.el")
	("COMP\\'" . "comp.el")
	("OXFORD\\'" . "oxford.el")
	("foldoc" . "foldoc.el")
	("jargon" . "jargon.el")
	("vera" . "vera.el")
	("rfc1983" . "rfc1983.el")
	("csrd\\'" . "srd.el")))

;;; loaddef.el ends here
