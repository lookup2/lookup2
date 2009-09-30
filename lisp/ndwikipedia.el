;;; ndwikipedia.el --- "Wikipedia" Offline -*- coding: utf-8 -*-
;; Copyright (C) 2009 KAWABATA Taichi <kawabata.taichi@gmail.com>

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

;; This agent file provides the ability to instantly search the
;; wikipedia article pages.  You need to set up the wikipedia bzip file
;; and Xapian index as described by the following URLs.
;;
;; http://jsomers.net/blog/offline-wikipedia
;; http://users.softlab.ece.ntua.gr/~ttsiod/buildWikipediaOffline.html
;; http://ihome.cuhk.edu.hk/~s065928/site/mydevs/mydev.html#08202009
;;
;; 
;;; Directory Structure:
;;
;; …/offline.wikipedia/
;; …/offline.wikipedia/quickstartsearch
;; …/offline.wikipedia/en/    … split enwiki-latest-article-pages.bz2
;; …/offline.wikipedia/en/db/ … xapian index
;; …/offline.wikipedia/ja/    … split jawiki-latest-article-pages.bz2
;; …/offline.wikipedia/ja/db/ … xapian index
;; …/offline.wikipedia/...
;;
;;
;;; Set-up
;;
;; (1) Download XXwiki-latest-article-pages.xml.bz2 into XX directory.
;;     (XX may be `en', `ja', etc.)
;; (2) create index `db' directory with `quicksearchindex'
;;     % cd ja
;;     % rm -rf db 
;;     % for i in rec*.bz; do echo \#$i ; bzcat $i | grep '<title' | perl -ne 'm/<title>([^<]+)</title>/ && print $1."\n";' ; done | ../quicksearchindex
;;
;;
;;; Lookup Setup
;;
;; You should specify the directory where each language wikipedia file
;; is set-up.
;;
;; (setq lookup-search-agents
;;       '(...
;;         (ndwikipedia ".../offline.wikipedia/")
;;         ...))

;;; Code:

(require 'lookup)
(require 'lookup-utils)
(require 'w3m)

;;;
;;; Customizable variables
;;;
(defvar ndwikipedia-search-program 
  (expand-file-name "~/edicts/offline.wikipedia/quickstartsearch"))

(defvar ndwikipedia-texvc-program 
  (expand-file-name "~/edicts/offline.wikipedia/mediawiki_sa/math/texvc"))

(defvar ndwikipedia-tmp-directory 
  (expand-file-name (concat temporary-file-directory "/ndwikipedia"))
  "Temporafy Directory for `ndwikipedia' files.")

(defvar ndwikipedia-tmp-work-directory 
  (expand-file-name (concat temporary-file-directory "/ndwikipedia/work"))
  "Temporafy Working Directory for `ndwikipedia' files.")

;;;
;;; Internal variables
;;;

(defvar ndwikipedia-entity-alist
  '(
    ("nbsp"   . " ")
    ("iexcl"  . "¡")
    ("cent"   . "¢")
    ("pound"  . "£")
    ("curren" . "¤")
    ("yen"    . "¥")
    ("brvbar" . "¦")
    ("sect"   . "§")
    ("uml"    . "¨")
    ("copy"   . "©")
    ("ordf"   . "ª")
    ("laquo"  . "«")
    ("not"    . "¬")
    ("shy"    . "­")
    ("reg"    . "®")
    ("macr"   . "¯")
    ("deg"    . "°")
    ("plusmn" . "±")
    ("sup2"   . "²")
    ("sup3"   . "³")
    ("acute"  . "´")
    ("micro"  . "µ")
    ("para"   . "¶")
    ("middot" . "·")
    ("cedil"  . "¸")
    ("sup1"   . "¹")
    ("ordm"   . "º")
    ("raquo"  . "»")
    ("frac14" . "¼")
    ("frac12" . "½")
    ("frac34" . "¾")
    ("iquest" . "¿")
    ("Agrave" . "À")
    ("Aacute" . "Á")
    ("Acirc"  . "Â")
    ("Atilde" . "Ã")
    ("Auml"   . "Ä")
    ("Aring"  . "Å")
    ("AElig"  . "Æ")
    ("Ccedil" . "Ç")
    ("Egrave" . "È")
    ("Eacute" . "É")
    ("Ecirc"  . "Ê")
    ("Euml"   . "Ë")
    ("Igrave" . "Ì")
    ("Iacute" . "Í")
    ("Icirc"  . "Î")
    ("Iuml"   . "Ï")
    ("ETH"    . "Ð")
    ("Ntilde" . "Ñ")
    ("Ograve" . "Ò")
    ("Oacute" . "Ó")
    ("Ocirc"  . "Ô")
    ("Otilde" . "Õ")
    ("Ouml"   . "Ö")
    ("times"  . "×")
    ("Oslash" . "Ø")
    ("Ugrave" . "Ù")
    ("Uacute" . "Ú")
    ("Ucirc"  . "Û")
    ("Uuml"   . "Ü")
    ("Yacute" . "Ý")
    ("THORN"  . "Þ")
    ("szlig"  . "ß")
    ("agrave" . "à")
    ("aacute" . "á")
    ("acirc"  . "â")
    ("atilde" . "ã")
    ("auml"   . "ä")
    ("aring"  . "å")
    ("aelig"  . "æ")
    ("ccedil" . "ç")
    ("egrave" . "è")
    ("eacute" . "é")
    ("ecirc"  . "ê")
    ("euml"   . "ë")
    ("igrave" . "ì")
    ("iacute" . "í")
    ("icirc"  . "î")
    ("iuml"   . "ï")
    ("eth"    . "ð")
    ("ntilde" . "ñ")
    ("ograve" . "ò")
    ("oacute" . "ó")
    ("ocirc"  . "ô")
    ("otilde" . "õ")
    ("ouml"   . "ö")
    ("divide" . "÷")
    ("oslash" . "ø")
    ("ugrave" . "ù")
    ("uacute" . "ú")
    ("ucirc"  . "û")
    ("uuml"   . "ü")
    ("yacute" . "ý")
    ("thorn"  . "þ")
    ("yuml"   . "ÿ")
    ("fnof"     . "ƒ")
    ("Alpha"    . "Α")
    ("Beta"     . "Β")
    ("Gamma"    . "Γ")
    ("Delta"    . "Δ")
    ("Epsilon"  . "Ε")
    ("Zeta"     . "Ζ")
    ("Eta"      . "Η")
    ("Theta"    . "Θ")
    ("Iota"     . "Ι")
    ("Kappa"    . "Κ")
    ("Lambda"   . "Λ")
    ("Mu"       . "Μ")
    ("Nu"       . "Ν")
    ("Xi"       . "Ξ")
    ("Omicron"  . "Ο")
    ("Pi"       . "Π")
    ("Rho"      . "Ρ")
    ("Sigma"    . "Σ")
    ("Tau"      . "Τ")
    ("Upsilon"  . "Υ")
    ("Phi"      . "Φ")
    ("Chi"      . "Χ")
    ("Psi"      . "Ψ")
    ("Omega"    . "Ω")
    ("alpha"    . "α")
    ("beta"     . "β")
    ("gamma"    . "γ")
    ("delta"    . "δ")
    ("epsilon"  . "ε")
    ("zeta"     . "ζ")
    ("eta"      . "η")
    ("theta"    . "θ")
    ("iota"     . "ι")
    ("kappa"    . "κ")
    ("lambda"   . "λ")
    ("mu"       . "μ")
    ("nu"       . "ν")
    ("xi"       . "ξ")
    ("omicron"  . "ο")
    ("pi"       . "π")
    ("rho"      . "ρ")
    ("sigmaf"   . "ς")
    ("sigma"    . "σ")
    ("tau"      . "τ")
    ("upsilon"  . "υ")
    ("phi"      . "φ")
    ("chi"      . "χ")
    ("psi"      . "ψ")
    ("omega"    . "ω")
    ("thetasym" . "ϑ")
    ("upsih"    . "ϒ")
    ("piv"      . "ϖ")
    ("bull"     . "•")
    ("hellip"   . "…")
    ("prime"    . "′")
    ("Prime"    . "″")
    ("oline"    . "‾")
    ("frasl"    . "⁄")
    ("weierp"   . "℘")
    ("image"    . "ℑ")
    ("real"     . "ℜ")
    ("trade"    . "™")
    ("alefsym"  . "ℵ")
    ("larr"     . "←")
    ("uarr"     . "↑")
    ("rarr"     . "→")
    ("darr"     . "↓")
    ("harr"     . "↔")
    ("crarr"    . "↵")
    ("lArr"     . "⇐")
    ("uArr"     . "⇑")
    ("rArr"     . "⇒")
    ("dArr"     . "⇓")
    ("hArr"     . "⇔")
    ("forall"   . "∀")
    ("part"     . "∂")
    ("exist"    . "∃")
    ("empty"    . "∅")
    ("nabla"    . "∇")
    ("isin"     . "∈")
    ("notin"    . "∉")
    ("ni"       . "∋")
    ("prod"     . "∏")
    ("sum"      . "∑")
    ("minus"    . "−")
    ("lowast"   . "∗")
    ("radic"    . "√")
    ("prop"     . "∝")
    ("infin"    . "∞")
    ("ang"      . "∠")
    ("and"      . "∧")
    ("or"       . "∨")
    ("cap"      . "∩")
    ("cup"      . "∪")
    ("int"      . "∫")
    ("there4"   . "∴")
    ("sim"      . "∼")
    ("cong"     . "≅")
    ("asymp"    . "≈")
    ("ne"       . "≠")
    ("equiv"    . "≡")
    ("le"       . "≤")
    ("ge"       . "≥")
    ("sub"      . "⊂")
    ("sup"      . "⊃")
    ("nsub"     . "⊄")
    ("sube"     . "⊆")
    ("supe"     . "⊇")
    ("oplus"    . "⊕")
    ("otimes"   . "⊗")
    ("perp"     . "⊥")
    ("sdot"     . "⋅")
    ("lceil"    . "⌈")
    ("rceil"    . "⌉")
    ("lfloor"   . "⌊")
    ("rfloor"   . "⌋")
    ("lang"     . "〈")
    ("rang"     . "〉")
    ("loz"      . "◊")
    ("spades"   . "♠")
    ("clubs"    . "♣")
    ("hearts"   . "♥")
    ("diams"    . "♦")
    ("quot"    . "\"")
    ("amp"     . "&")
    ("lt"      . "<")
    ("gt"      . ">")
    ("OElig"   . "Œ")
    ("oelig"   . "œ")
    ("Scaron"  . "Š")
    ("scaron"  . "š")
    ("Yuml"    . "Ÿ")
    ("circ"    . "ˆ")
    ("tilde"   . "˜")
    ("ensp"    . " ")
    ("emsp"    . " ")
    ("thinsp"  . " ")
    ("zwnj"    . "‌")
    ("zwj"     . "‍")
    ("lrm"     . "‎")
    ("rlm"     . "‏")
    ("ndash"   . "–")
    ("mdash"   . "—")
    ("lsquo"   . "‘")
    ("rsquo"   . "’")
    ("sbquo"   . "‚")
    ("ldquo"   . "“")
    ("rdquo"   . "”")
    ("bdquo"   . "„")
    ("dagger"  . "†")
    ("Dagger"  . "‡")
    ("permil"  . "‰")
    ("lsaquo"  . "‹")
    ("rsaquo"  . "›")
    ("euro"    . "€")
    ("apos" . "'"))
  "The MediaWiki pre-defined entities.")

(defvar ndwikipedia-replace-entities-regexp
  (concat "&\\("
          (regexp-opt
           (mapcar 'car ndwikipedia-entity-alist))
          "\\);"))

(defvar ndwikipedia-reference-cache
  (make-hash-table :test 'equal))

;;;
;;; Interface functions
;;;

(put 'ndwikipedia :list 'ndwikipedia-list)
(defun ndwikipedia-list (agent)
  "Return list of dictionaries of AGENT."
  (when (null (file-directory-p ndwikipedia-tmp-directory))
    (make-directory ndwikipedia-tmp-directory))
  (when (null (file-directory-p ndwikipedia-tmp-work-directory))
    (make-directory ndwikipedia-tmp-work-directory))
  (let ((dir (lookup-agent-location agent)))
    (if (file-directory-p dir)
        (mapcar (lambda (name) (lookup-new-dictionary agent name))
                (remove-if-not (lambda (file)
                                 (file-directory-p (concat dir "/" file "/db")))
                               (directory-files dir)))
      (message "ndwikipedia: directory %s is not found." dir)
      nil)))

(put 'ndwikipedia :title 'ndwikipedia-title)
(defun ndwikipedia-title (dictionary)
  (let ((name (lookup-dictionary-name dictionary)))
    (concat "Wikipedia (" name ")")))

(put 'ndwikipedia :methods 'ndwikipedia-methods)
(defun ndwikipedia-methods (dictionary)
  '(exact keyword prefix))

(put 'ndwikipedia :search 'ndwikipedia-search)
(defun ndwikipedia-search (dictionary query)
  (let* ((method (lookup-query-method query))
	 (string (lookup-query-to-wildcard query))
         (dir (ndwikipedia-db-directory dictionary))
         (result (ndwikipedia-search-for-word dir string)))
    (mapcar (lambda (code-heading)
              (lookup-new-entry 'regular dictionary (car code-heading) (cdr code-heading)))
            result)))

(defun ndwikipedia-db-directory (dictionary)
  (concat (lookup-agent-location
           (lookup-dictionary-agent dictionary))
          "/"
          (lookup-dictionary-name dictionary)
          "/db"))

(defun ndwikipedia-search-for-word (dir string)
  (let (result)
    (with-temp-buffer
      (lookup-with-coding-system 'utf-8
        (call-process ndwikipedia-search-program
                      nil t nil (expand-file-name dir) string))
      (message "debug: %s" (buffer-string))
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]+% \\)\\[\\(rec[^:]+\\):\\(.+\\)\\]" nil t)
        (setq result (cons (cons (concat (match-string 2) ":" (match-string 3))
                                 (concat (match-string 1) "[" (match-string 3) "]"))
                           result)))
      (nreverse result))))

(put 'ndwikipedia :content 'ndwikipedia-content)
(defun ndwikipedia-content (entry)
  "Return string content of ENTRY."
  (let ((dict  (lookup-entry-dictionary entry))
        (code  (lookup-entry-code entry)))
    (when (null (string-match ":" code))
      (setq code (ndwikipedia-search-for-exact-word
                  (ndwikipedia-db-directory dict) code)))
    (if code
        (let* ((file  (progn
                        (string-match "\\([^:]+\\):\\(.+\\)" code)
                        (match-string 1 code)))
               (title (match-string 2 code))
               (agent (lookup-dictionary-agent dict))
               (file  (concat (lookup-agent-location agent)
                              "/"
                              (lookup-dictionary-name dictionary)
                              "/"
                              file)))
          (ndwikipedia-content-of-file file title))
      "This entry is not defined yet.")))

(defun ndwikipedia-search-for-exact-word (dir string)
  (let ((key (concat dir string)))
    (or
     (gethash key ndwikipedia-reference-cache)
     (with-temp-buffer
       (lookup-with-coding-system 'utf-8
         (call-process ndwikipedia-search-program
                       nil t nil (expand-file-name dir) string))
       (goto-char (point-min))
       (when (re-search-forward "\\([0-9]+% \\)\\[\\(rec[^:]+:.+\\)\\]" nil t)
         (puthash key (match-string 2) ndwikipedia-reference-cache)
         (match-string 2))))))

(defun ndwikipedia-content-of-file (file title)
  (let (start end
        (coding-system-for-read 'no-conversion)
        (search (string-as-unibyte (concat "<title>" title "</title>")))
        (case-fold-search nil))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents file)
      (message "point-max=%s, search=%s" (point-max) search)
      (goto-char (point-min))
      (if (null (search-forward search nil t))
          (error "ndwikipedia error! %s %s %s" file title search))
      (if (re-search-forward "<text.*?>" nil 1)
          (setq start (match-end 0))
        (ndwikipedia-insert-additional-file file)
        (re-search-forward "<text.*?>" nil t)
        (setq start (match-end 0)))
      (if (search-forward "</text>" nil 1)
          (setq end (match-beginning 0))
        (ndwikipedia-insert-additional-file file)
        (search-forward "</text>" nil t)
        (setq end (match-beginning 0)))
      (decode-coding-string (buffer-substring start end) 'utf-8))))

(defun ndwikipedia-insert-additional-file (file)
  (save-match-data
    (save-excursion
      (string-match "\\(.+\\)/rec\\([0-9][0-9][0-9][0-9][0-9]\\)\\(.+\\)" file)
      (goto-char (point-max))
      (insert-file-contents 
       (format "%s/rec%05d%s" (match-string 1 file) 
               (1+ (string-to-number (match-string 2 file)))
               (match-string 3 file))))))

(put 'ndwikipedia :arranges
     '((replace   ndwikipedia-arrange-replace)
       (gaiji     ndwikipedia-arrange-equation)
       (reference ndwikipedia-arrange-table
                  ndwikipedia-arrange-reference)
       (structure ndwikipedia-arrange-structure)))

;;;
;;; Main Program
;;;

(defun ndwikipedia-arrange-replace (entry)
  (while (re-search-forward ndwikipedia-replace-entities-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 1) ndwikipedia-entity-alist))))
  (goto-char (point-min))
  ;; repeat twice: &amp;minus; -> &minus; -> "-"
  (while (re-search-forward ndwikipedia-replace-entities-regexp nil t)
    (replace-match
     (cdr (assoc (match-string 1) ndwikipedia-entity-alist))))
  (goto-char (point-min))
  (while (re-search-forward "&#\\([0-9]+\\);" nil t)
    (replace-match (string (string-to-number (match-string 1)))))
  (goto-char (point-min))
  (while (re-search-forward "&#x\\([0-9a-f]+\\);" nil t)
    (replace-match (string (string-to-number (match-string 1) 16))))
  (goto-char (point-min))
  (while (re-search-forward "<sub>\\(.+?\\)</sub>" nil t)
    (replace-match (lookup-subscript-string (match-string 1))))
  (goto-char (point-min))
  (while (re-search-forward "<sup>\\(.+?\\)</sup>" nil t)
    (replace-match (lookup-superscript-string (match-string 1))))
  (goto-char (point-min))
  (while (re-search-forward "'''\\(.+?\\)'''" nil t)
    (add-text-properties (match-beginning 1) (match-end 1)
                         '(face lookup-heading-1-face))
    (replace-match "\\1"))
  (goto-char (point-min))
  (while (re-search-forward "<!--\\(.\\|\n\\)+?-->" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
                         '(face lookup-comment-face)))
  )

(defun ndwikipedia-arrange-equation (entry)
  (while (re-search-forward "<math>\\(\\(.\\|\n\\)+?\\)</math>" nil t)
    (let ((image-file 
           (save-match-data (ndwikipedia-get-math-image (match-string 1)))))
      (if image-file (lookup-img-file-insert image-file 'png 
                                             (match-beginning 0)
                                             (match-end 0)
                                             :background "white")))))

(defun ndwikipedia-get-math-image (equation)
  (let* ((md5 (md5 (concat "\"" equation "\"")))
         (file-name (concat ndwikipedia-tmp-directory "/" md5 ".png")))
    (when (null (file-exists-p file-name))
      (with-temp-buffer
        (call-process ndwikipedia-texvc-program
                      nil t nil 
                      (expand-file-name ndwikipedia-tmp-work-directory)
                      (expand-file-name ndwikipedia-tmp-work-directory)
                      equation
                      "iso-8859-1")
        (goto-char (point-min))
        (when (re-search-forward "^.\\([0-9a-f]\\{32\\}\\)" nil t)
          (rename-file 
           (concat ndwikipedia-tmp-work-directory "/" (match-string 1) ".png")
           file-name)
          (message "img file %s is generated." file-name))))
    (if (file-exists-p file-name) file-name)))

(defun ndwikipedia-arrange-reference (entry)
  (while (re-search-forward "\\[\\[\\([^]|]+\\)\\(|[^]]+\\)?\\]\\]" nil t)
    (let* ((start (match-beginning 0))
           (code  (match-string 1))
           (head  (if (match-string 2)
                      (substring (match-string 2) 1)
                    code))
           (dict  (lookup-entry-dictionary entry))
           ;;(dir   (ndwikipedia-db-directory dict))
           ;;(code  (save-match-data 
           ;;         (ndwikipedia-search-for-exact-word dir code)))
           ;;(new-entry (if code (lookup-new-entry 'regular dict code head))))
           (new-entry (lookup-new-entry 'regular dict code head)))
      (replace-match head t t)
      (if new-entry
          (lookup-set-link start (point) new-entry)))))

(defun ndwikipedia-arrange-table (entry)
  (let ((dict (lookup-entry-dictionary entry)) start end)
    (while (re-search-forward "^{|\\(.\\|\n\\)+?\n|}" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (ndwikipedia-convert-table-to-plain-text (point-min) (point-max))
    ))
    (goto-char (point-min))
    ;; process text properties
    (while (setq start (next-single-property-change (point) 'w3m-href-anchor))
      (goto-char start)
      (when (setq val (get-text-property (point) 'w3m-href-anchor))
        (setq val (substring val 10)
              end (next-single-property-change (point) 'w3m-href-anchor))
        (lookup-set-link start end
                         (lookup-new-entry 'regular dict val val))
        (goto-char end)))
    (remove-text-properties 
     (point-min) (point-max) 
     '(balloon-help mouse-face keymap help-echo w3m-anchor-sequence 
       w3m-balloon-help w3m-href-anchor w3m-safe-url-regexp))
    ))

;;
;; MediaWiki Table Drawing
;;

(defun ndwikipedia-convert-table-to-plain-text (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (let (start end val)
        (narrow-to-region from to)
        (ndwikipedia-convert-table-to-html from to)
        (w3m-region (point-min) (point-max))
        (remove-text-properties 
         (point-min) (point-max) 
         '(balloon-help face help-echo mouse-face 
           keymap help-echo w3m-anchor-sequence w3m-balloon-help
           w3m-safe-url-regexp))
      ))))

(defun ndwikipedia-convert-table-to-html (from to)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]" nil t)
        (if (match-string 2) 
            (replace-match "<a href='#\\1'>\\3</a>" t)
          (replace-match "<a href='#\\1'>\\1</a>" t)))
      (goto-char (point-min))
      (while (re-search-forward "^{|\\(.*\\)" nil t) (replace-match "<table \\1>"))
      (goto-char (point-min))
      (while (re-search-forward "^|}.*" nil t) (replace-match "</table>"))
      (goto-char (point-min))
      (while (re-search-forward "^|\\+\\(.*\\)" nil t) (replace-match "<caption>\\1</caption>"))
      (goto-char (point-min))
      (while (re-search-forward "^|-\\(.*\\)" nil t) (replace-match "<tr \\1>"))
      (goto-char (point-min))
      (while (re-search-forward "^\\([!|].+\\)" nil t) 
        (ndwikipedia-table-row-process (match-beginning 0) (match-end 0)))
      )))
    
(defun ndwikipedia-table-row-process (from to)
  (interactive "r")
  (let (mode sep start pos attr)
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (while (re-search-forward "\\(.+?\\)\\(!!\\|||\\)" nil t)
        (replace-match (ndwikipedia-table-cell-process (match-string 1)) t nil nil 1))
      (re-search-forward ".+" nil t)
      (replace-match (ndwikipedia-table-cell-process (match-string 0))))))

(defun ndwikipedia-table-cell-process (string)
  (save-match-data
  (string-match "^\\([|!]\\)[|!]?\\(\\([^|]+\\)|\\)?\\(.*\\)" string)
  (let ((cell (if (equal (match-string 1 string) "|") "td" "th"))
        (attr (match-string 3 string))
        (val  (match-string 4 string)))
    (concat "<" cell " " attr ">" val "</" cell ">"))))

(provide 'ndwikipedia)

;;; ndwikipedia.el ends here

;;; support-wikipedia.el ends here
