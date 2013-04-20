;;; ndweb-options.el --- Web-based Dictionary Options  -*- coding: utf-8; lexical-binding:t -*-

;; Copyright (c) KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 
;; Keywords:
;; URL: http://lookup2.github.com/

;;; Commentary:

;; This file provides preset options (ndweb-site-predefined-options)
;; for each web dictionaries.
;;
;; This file is invoked from `support-ndweb.el' file.  You can set
;; your own dictionary options via ordinary
;; `lookup-dictionary-option-alist'.  This file should only provide
;; static options.  It should never invoke web sites to retrive
;; options.

;;; Code:

;;;
;;; option data
;;;

(defun ndweb-goo-suggestions (dict-kind)
  ;; Return searching function. `(cadr)' unquotes macroexpand-all output.
  (cadr (macroexpand-all
   `(lambda (query method _pattern)
      (message (concat "http://dictionary.goo.ne.jp/srch/" ,dict-kind "/"
                  query (case method
                          ('prefix "/m0u/")
                          ('exact "/m1u/")
                          ('suffix "/m2u/")
                          ('text "/m3u/")
                          ('substring "/m6u/"))))
      (ndweb-with-url
          (concat "http://dictionary.goo.ne.jp/srch/" ,dict-kind "/"
                  query (case method
                          ('prefix "/m0u/")
                          ('exact "/m1u/")
                          ('suffix "/m2u/")
                          ('text "/m3u/")
                          ('substring "/m6u/")))
        (if (re-search-forward
             ,(concat
               "<link rel=\"canonical\" href=\"http://dictionary.goo.ne.jp/leaf/"
               dict-kind "/\\([0-9]+\\)/m0u/")
             nil t)
            (list (list (match-string 1) query))
          (loop while (re-search-forward
                       ,(concat
                         "<a href=\"/leaf/" dict-kind 
                         "/\\([0-9]+\\)/m[0-6]u/.+?\">\\(.+?\\)</a>")
                       nil t)
                collect (list (match-string 1)
                              (replace-regexp-in-string
                               "&nbsp;" " " (match-string 2))))))))))

;;;
;;; Pre-defined options
;;;
(defvar ndweb-default-options-alist
  (cl-mapcan
   (lambda (set)
     (let* ((default-options (butlast set 2))
            (sites (plist-get set :sites)))
       (mapcar (lambda (x) (append x default-options))
               sites)))
   `(;; English
     (:charsets (ascii)
      :sites
      (("en.wikipedia.org"
        :self "http://en.wikipedia.org/w/opensearch_desc.php")
       ("en.wiktionary.org"
        :self "http://en.wiktionary.org/w/opensearch_desc.php")
       ("www.google.com" :title "Google (Firefox)"
        :suggestions
        "http://suggestqueries.google.com/complete/search?hl=en&client=firefox&hjson=t&&q={searchTerms}"
        :results "https://www.google.com/#q={searchTerms}&output=search")
       ("www.ldoceonline.com"
        :self "http://www.ldoceonline.com/widgets/ldoce_opensearch.xml")
       ("dictionary.cambridge.org"
        :self "http://dictionary.cambridge.org/gadgets/british/opensearch.xml")
       ("mycroft:webster" :title "Merriam-Webster Dictionary")
       ("mycroft:webster-med" :title "Merriam-Webster Medical")
       ("mycroft:webster-thsrs" :title "Merriam-Webster Thesaurus")
       ("mycroft:oald8" :start-tag "<_id id=\"main-container\">")
       ("mycroft:yahoo_dictionary")
       ("www.onelook.com" :self "http://www.onelook.com/osdf.xml")
       ;("mycroft:collins-cobuild")
       ("dictionary.reference.com"
        :self "http://dictionary.reference.com/opensearch_desc.xml")
       ("thesaurus.reference.com"
        :self "http://thesaurus.reference.com/opensearch_desc.xml")
       ("mycroft:visual-thesaurus") ; view in web browser.
       ;; English Corpus
       ("mycroft:bnc-ssearch" :title "British National Corpus")
       ("erek.ta2o.net/news" :title "EReK News Corpus"
        :results
        "http://erek.ta2o.net/news/{searchTerms}.html")
       ;; English Library
       ("www.oxfordjournals.org" 
        :self "http://www.oxfordjournals.org/resource/xml/opensearch.xml")
       ("mycroft:blaut" :title "Brigish Library Author Search")
       ("mycroft:bltit" :title "Brigish Library Title Search")
       ;; English-to-Japanese
       ("dictionary.goo.ne.jp/ej3" :title "goo 英和辞書"
         :methods (exact prefix suffix text substring)
         :start-tag "<b><span>"
         :suggestions ,(ndweb-goo-suggestions "ej3")
         :results "http://dictionary.goo.ne.jp/leaf/ej3/{searchTerms}/m0u/")
       ("mycroft:kenkwaeichujiten-cl" :title "研究社新和英中辞典 (Excite)")
       ("mycroft:yahoodicenjp")
       ("mycroft:eijiro")
       ("mycroft:eijiro-collocation" :title "英辞郎 頻度集計")
       ("mycroft:eijiro-kwic" :title "英辞郎 整列表示")
       ))
     (;; Japanese
      :charsets (ascii japanese-jisx0208)
      :sites
      (("ja.wikipedia.org" :self "http://ja.wikipedia.org/w/opensearch_desc.php")
       ("ja.wiktionary.org")
       ("kotobank.jp"
        :self "http://kotobank.jp/guide/opensearch.xml"
        :start-tag "<_id id=\"main_area\">"
        :suggestions
        (lambda (query method _pattern)
          (ndweb-with-url (concat "http://kotobank.jp/search/result?q=" query "&c=opensearch")
            (mapcar
             (lambda (x) (decode-coding-string (url-unhex-string x) 'utf-8))
             (if (re-search-forward "<link rel=\"canonical\" href=\"http://kotobank.jp/word/\\(.+?\\)\"" nil t)
                 (list (match-string 1))
               (loop while (re-search-forward "<a href=\"/word/\\(.+?\\)\"" nil t)
                     collect (match-string 1))))))
        :results "http://kotobank.jp/word/{searchTerms}")
       ("dictionary.goo.ne.jp/jn2" :title "goo 国語辞書"
        :methods (exact prefix suffix text substring)
         :start-tag "<b><span>"
         :suggestions ,(ndweb-goo-suggestions "jn2")
         :results "http://dictionary.goo.ne.jp/leaf/jn2/{searchTerms}/m0u/")
       ("dictionary.goo.ne.jp/thsrs" :title "goo 類語辞書"
         :methods (exact prefix suffix text substring)
         :start-tag "<b><span>"
         :suggestions ,(ndweb-goo-suggestions "thsrs")
         :results "http://dictionary.goo.ne.jp/leaf/thsrs/{searchTerms}/m0u/")
       ("k.hatena.ne.jp" :self "http://d.hatena.ne.jp/opensearch/keyword.xml"
        :start-tag "<b><a name=\"keywordbody\"")
       ("mycroft:shogakukadaijisen-cl" :title "小学館 大辞泉 (Yahoo)")
       ("mycroft:daijirin" :title "三省堂 大辞林 (Yahoo)" :start-tag "<_id id=\"main-d\">")
       ("mycroft:daihyakkazensho-cl" :title "小学館 日本大百科全書 (Yahoo)")
       ("mycroft:sanseidodaijirin-cl" :title "三省堂 大辞林 (Excite)")
       ("mycroft:yahoodicjp_thesaurus")
       ;; Japnese Corpus
       ("jrek.ta2o.net/news" :title "JReK Corpus"
        :results "http://jrek.ta2o.net/s/{searchTerms}.html")
       ;; Japanese Misc
       ("ml.naxos.jp" :title "Naxos Music Library"
        :results "http://ml.naxos.jp/KeywordSearch.aspx?word={searchTerms}")
       ("hanmoto.com" :title "版元ドットコム書誌検索" 
        :self "http://hanmoto.com/bd.xml")
       ("mycroft:goo-music")
       ("mycroft:goo-lyrics")
       ("reed.kuee.kyoto-u.ac.jp/cf-search" :title "格フレーム検索"
        :encoding euc-jp
        :results "http://reed.kuee.kyoto-u.ac.jp/cf-search/?text={searchTerms}")
       ;; Japanese-to-English
       ("dictionary.goo.ne.jp/je2" :title "goo 和英辞書"
         :methods (exact prefix suffix text substring)
         :start-tag "<b><span>"
         :suggestions ,(ndweb-goo-suggestions "je2")
         :results "http://dictionary.goo.ne.jp/leaf/je2/{searchTerms}/m0u/")
       ("rnnnews.jp"
        :title "RNN時事英語辞典"
        :methods (exact prefix suffix substring)
        :suggestions
        (lambda (query method pattern)
          (let ((code (concat query "&m="
                              (case method ('substring "0") ('prefix "1")
                                    ('suffix "2") ('exact "3")))))
            (ndweb-with-url 
                (concat "http://rnnnews.jp/search/result/?q=" code)
              (unless (search-forward "見つかりませんでした。" nil t)
                (list (list code pattern))))))
        :results 
        "http://rnnnews.jp/search/result/?q={searchTerms}")
       ("mycroft:kenkeiwachujiten-cl" :title "研究社新英和中辞典 (Excite)")
       ("www.excite.co.jp/dictionary"
        :self "http://www.excite.co.jp/search/opensearch/xml/dictionary/english/")
       ("home.alc.co.jp/db/ow/bdicn_sch"
        :results "home.alc.co.jp/db/ow/bdicn_sch?w={searchTerms}"
        :encoding japanese-shift_jis)
       ("mycroft:yahoodicjpen")
       ;; Japanese-to-Chinese
       ("dictionary.goo.ne.jp/jc" :title "goo 日中辞書"
         :methods (exact prefix suffix text substring)
         :start-tag "<b><span>"
         :suggestions ,(ndweb-goo-suggestions "jc")
         :results "http://dictionary.goo.ne.jp/leaf/jc/{searchTerms}/m0u/")
       ("mycroft:bitex" :title "BitEx 日中辞書")
       ("mycroft:hjenglish-j2c" :title "沪江小D中日词典")))
     (;; Chinese
      :charsets (ascii chinese-gb2312)
      :sites
      (("zh.wikipedia.org")
       ("zh.wiktionary.org")
       ("www.thefreedictionary.com"
        :self "http://www.thefreedictionary.com/_/open-search.xml")
       ("www.iciba.com"
        :self "http://res.iciba.com/dict/opensearch_desc.xml")
       ;; Chinese-to-Japanese
       ("dictionary.goo.ne.jp/cj" :title "goo 中日辞書"
         :methods (exact prefix suffix text substring)
         :start-tag "<b><span>"
         :suggestions ,(ndweb-goo-suggestions "cj")
         :results "http://dictionary.goo.ne.jp/leaf/cj/{searchTerms}/m0u/")
       ("bitex-cn.com" :title "BitEx 中日辞書"
        :results "http://www.bitex-cn.com/search_result.php?deal_type=jp2cn&amp;keywords={searchTerms}")
       ("www.excite.co.jp/dictionary/chinese_japanese" :title "三省堂 中日辞典 (Excite)"
        :results
        "http://www.excite.co.jp/dictionary/chinese_japanese/?search={searchTerms}")
       ("mycroft:hjenglish-c2j" :title "沪江小D日中词典")))
     ;;(;; Kanji/Hanzi
     ;; :charsets (han)
     ;; :sites
     ;; (("www.zdic.net"
     ;;   :title "漢典"
     ;;   :charsets (han)
     ;;   :results "http://www.zdic.net/sousou/?q={searchTerms}"
     ;;   :method "post")))
     ))
  "Pre-defined options for some searching sites.")

;;;
;;; interaface function
;;;

(defun ndweb-site-options (site agent-options)
  (append
   agent-options
   (assoc-default site ndweb-default-options-alist)))

(provide 'ndweb-options)
