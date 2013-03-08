;;; support-defs.el --- autoload definition of support files -*- lexical-binding: t -*-

;; Add your dictionary freely!

(setq lookup-support-autoload-default-alist
      '(
        ;;; Text-based dictionaries
        ;; (for ndtext/ndbuffer/ndsary agents)

        ;; sdic (http://www.namazu.org/~tsuchiya/sdic/)
        ("\\.sdic" . "support-sdic")

        ;; 支那漢 (http://www.seiwatei.net/chinakan/)
        ("/chinadat\\.csv" . "support-chinakan")

        ;; 和製漢字の辞典 (http://homepage2.nifty.com/TAB01645/ohara/)
        ("/waseikanji\\.html" . "support-waseikanji")

        ;; RFC 1983
        ("rfc1983\\.txt" . "support-rfc1983")
        ;; RFC 4939
        ("rfc4949\\.txt" . "support-rfc4949")

        ;; 字通 CD-ROM 付録HTMLファイル
        ;; (http://www.tranradar.net/cd-jitsuu.html)
        ("/JitsuuFuroku/b001.html" . "support-jtfrk")
        ("/JitsuuFuroku/b002.html" . "support-jtfrk")
        ("/JitsuuFuroku/f001.html" . "support-jtfrk")

        ;; 字源
        ;; http://wagang.econ.hc.keio.ac.jp/zigen
        ("/zigen\\.xml" . "support-zigen")

        ;; 英辞郎（１行テキスト版）
        ("/EIJIRO/EIJI-" . "support-eijiro")
        ("/EIJIRO/REIJI" . "support-eijiro")
        ("/EIJIRO/RYAKU" . "support-eijiro")
        ("/EIJIRO/WAEI-" . "support-eijiro")

        ;; Unicode Char Database (http://www.unicode.org/Public/6.2.0/ucdxml/)
        ("/ucd\\..*flat\\.xml" . "support-ucd")

        ;; Jim Breen's JMDICT (http://www.csse.monash.edu.au/~jwb/japanese.html)
        ("/JMdict" . "support-jmdict")

        ;; RangJung Yeshe Tibetan English dictionary
        ;; (http://www.rangjung.com/ry-dic.htm)
        ("/Rangjung-Yeshe" . "support-rangjung")

        ;;; BTONIC dictionaries
        ;; (ndbtonic)
        ;; 音楽中辞典 (http://www.ongakunotomo.co.jp/useful/dic/index_cdrom.html)
        ("/onmusic/" . "support-onmusic")

        ;;; Shogakukan Random House Dictionary
        ;; (ndsrd)
        ;; (http://www.shogakukan.co.jp/books/detail/_isbn_4099068579)
        ("ndsrd:" . "support-srd")

        ;; FreePWing Dictionaries
        ;; FOLDOC (http://foldoc.org/)
        ("/foldoc" . "support-foldoc")
        ;; JARGON File (http://openlab.ring.gr.jp/edict/fpw/#jargon)
        ("/jargon" . "support-jargon")
        ;; V.E.R.A. (http://www.delorie.com/gnu/docs/vera/vera.html)
        ("/vera" . "support-vera")
        ))

;;; support-defs.el ends here
