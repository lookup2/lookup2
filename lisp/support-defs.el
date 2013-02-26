;;; support-defs.el --- autoload definition of support files -*- lexical-binding: t -*-

;; Add your dictionary freely!

(setq lookup-support-autoload-default-alist
      '(
        ;; sdic (http://www.namazu.org/~tsuchiya/sdic/)
        ("\\.sdic" . "support-sdic")

        ;; Unicode Char Database http://www.unicode.org/Public/6.2.0/ucdxml/
        ("/ucd\\..*flat\\.xml" . "support-ucd")

        ;; Jim Breen's JMDICT (http://www.csse.monash.edu.au/~jwb/japanese.html)
        ("/JMdict" . "support-jmdict")

        ;; 支那漢 (http://www.seiwatei.net/chinakan/)
        ("/chinadat\\.csv" . "support-chinakan")

        ;; 英辞郎
        ("/EIJIRO/EIJI-" . "support-eijiro")
        ("/EIJIRO/REIJI" . "support-eijiro")
        ("/EIJIRO/RYAKU" . "support-eijiro")
        ("/EIJIRO/WAEI-" . "support-eijiro")

        ;; Wasei Kanji no JIten
        ("/waseikanji\\.html" . "support-waseikanji")

        ;; RFC Dictionaries
        ("rfc4949\\.txt" . "support-rfc4949")
        ("rfc1983\\.txt" . "support-rfc1983")

        ;; 字通 CD-ROM
        ("/JitsuuFuroku/b001.html" . "support-jtfrk")
        ("/JitsuuFuroku/b002.html" . "support-jtfrk")
        ("/JitsuuFuroku/f001.html" . "support-jtfrk")

        ;; Wikipedia
        ("/frwiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("/jawiki.*-abstract.*\\.xml" . "support-wikipedia")
        ("/zhwiki.*-abstract.*\\.xml" . "support-wikipedia")

        ;; RangJung Yeshe Tibetan English dictionary
        ("/Rangjung-Yeshe" . "support-rangjung")

        ;; BTONIC
        ("/onmusic/" . "support-onmusic")

        ;; Shougakukan Random House
        ("ndsrd:" . "support-srd")

        ;; DIC files
        ("/foldoc" . "support-foldoc")
        ("/jargon" . "support-jargon")
        ("/vera" . "support-vera")

        ;; muller
        ("/muller-all\\.xml" . "support-muller")
        ))

;;; support-defs.el ends here
