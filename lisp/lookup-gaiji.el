;;; lookup-gaiji.el --- generalized gaiji replacement
;; Copyright (C) 2000 Keisuke Nishida <knishida@ring.gr.jp>

;; Author: Keisuke Nishida <knishida@ring.gr.jp>
;; Keywords: dictionary

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

;;;
;;; Interface functions
;;;

;;;###autoload
(defun lookup-gaiji-concrete (spec)
  (let ((func (get (aref spec 0) 'lookup-concrete-function)))
    (if func
	(funcall func (aref spec 1))
      (error "Invalid gaiji specification: %S" spec))))

;;;
;;; Concrete functions
;;;

(put 'unicode 'lookup-concrete-function 'lookup-gaiji-concrete-unicode)
(defun lookup-gaiji-concrete-unicode (code)
  (when lookup-use-unicode
    (let ((ucs (ucs-to-char code)))
      (if ucs (list (char-to-string ucs))))))

(put 'jisx0212 'lookup-concrete-function 'lookup-gaiji-concrete-jisx0212)
(defun lookup-gaiji-concrete-jisx0212 (code)
  (lookup-gaiji-concrete-100 'japanese-jisx0212 code))

(put 'gb2312 'lookup-concrete-function 'lookup-gaiji-concrete-gb2312)
(defun lookup-gaiji-concrete-gb2312 (code)
  (lookup-gaiji-concrete-100 'chinese-gb2312 code))

(put 'cns0 'lookup-concrete-function 'lookup-gaiji-concrete-cns0)
(defun lookup-gaiji-concrete-cns0 (code)
  (list (char-to-string (decode-big5-char code))))

(put 'cns1 'lookup-concrete-function 'lookup-cns1-concrete)
(defun lookup-gaiji-concrete-cns1 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-1 code))

(put 'cns2 'lookup-concrete-function 'lookup-cns2-concrete)
(defun lookup-gaiji-concrete-cns2 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-2 code))

(put 'cns3 'lookup-concrete-function 'lookup-cns3-concrete)
(defun lookup-gaiji-concrete-cns3 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-3 code))

(put 'cns4 'lookup-concrete-function 'lookup-cns4-concrete)
(defun lookup-gaiji-concrete-cns4 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-4 code))

(put 'cns5 'lookup-concrete-function 'lookup-cns5-concrete)
(defun lookup-gaiji-concrete-cns5 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-5 code))

(put 'cns6 'lookup-concrete-function 'lookup-cns6-concrete)
(defun lookup-gaiji-concrete-cns6 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-6 code))

(put 'cns7 'lookup-concrete-function 'lookup-cns7-concrete)
(defun lookup-gaiji-concrete-cns7 (code)
  (lookup-gaiji-concrete-256 'chinese-cns11643-7 code))

(defun lookup-gaiji-concrete-100 (charset code)
  (let ((char (make-char charset (+ (/ code 100) 32) (+ (% code 100) 32))))
    (list (char-to-string char))))

(defun lookup-gaiji-concrete-256 (charset code)
  (let ((char (make-char charset (/ code 256) (% code 256))))
    (list (char-to-string char))))

;;;
;;; Internal
;;;

(defconst lookup-gaiji-intern-table
  '(;; latin-1
    ("A`" ",A@(B" "A")
    ("A'" ",AA(B" "A")
    ("A^" ",AB(B" "A")
    ("A~" ",AC(B" "A")
    ("A\"" ",AD(B" "A")
    ("A/" ",AE(B" "A")
    ("a`" ",A`(B" "a")
    ("a'" ",Aa(B" "a")
    ("a^" ",Ab(B" "a")
    ("a~" ",Ac(B" "a")
    ("a\"" ",Ad(B" "a")
    ("a/" ",Ae(B" "a")
    ("E`" ",AH(B" "E")
    ("E'" ",AI(B" "E")
    ("E^" ",AJ(B" "E")
    ("E\"" ",AK(B" "E")
    ("E/" ",AF(B" "E/")
    ("e`" ",Ah(B" "e")
    ("e'" ",Ai(B" "e")
    ("e^" ",Aj(B" "e")
    ("e\"" ",Ak(B" "e")
    ("e/" ",Af(B" "e")
    ("I`" ",AL(B" "I")
    ("i`" ",Al(B" "i")
    ("I'" ",AM(B" "I")
    ("i'" ",Am(B" "i")
    ("I^" ",AN(B" "I")
    ("i^" ",An(B" "i")
    ("I\"" ",AO(B" "I")
    ("i\"" ",Ao(B" "i")
    ("O`" ",AR(B" "O")
    ("o`" ",Ar(B" "o")
    ("O'" ",AS(B" "O")
    ("o'" ",As(B" "o")
    ("O^" ",AT(B" "O")
    ("o^" ",At(B" "o")
    ("O~" ",AU(B" "O")
    ("o~" ",Au(B" "o")
    ("O\"" ",AV(B" "O")
    ("o\"" ",Av(B" "o")
    ("O/" ",AX(B" "O")
    ("o/" ",Ax(B" "o")
    ("U`" ",AY(B" "U")
    ("u`" ",Ay(B" "u")
    ("U'" ",AZ(B" "U")
    ("u'" ",Az(B" "u")
    ("U^" ",A[(B" "U")
    ("u^" ",A{(B" "u")
    ("U\"" ",A\(B" "U")
    ("u\"" ",A|(B" "u")
    ("Y'" ",A](B" "Y")
    ("y'" ",A}(B" "y")
    ("y\"" ",A(B" "y")
    ("D/" ",AP(B" "D")
    ("d/" ",Ap(B" "d")
    ("T/" ",A^(B" "T/")
    ("t/" ",A~(B" "t/")
    ("s/" ",A_(B" "s/")
    ("C," ",AG(B" "C")
    ("c," ",Ag(B" "c")
    ("N~" ",AQ(B" "N")
    ("n~" ",Aq(B" "n")
    ("?/" ",A?(B" "?")
    ("!/" ",A!(B" "!")
    ("<<" ",A+(B" "<<")
    (">>" ",A;(B" ">>")
    ("o_" ",A:(B" "o")
    ("a_" ",A*(B" "a")
    ("//" ",A0(B" "//")
    ;; latin-2
    ("C'" ",BF(B" "C")
    ("l/" ",B3(B" "l")
    ;; other
    ("B'" nil "B")
    ("D'" nil "D")
    ("F'" nil "F")
    ("G'" nil "G")
    ("H'" nil "H")
    ("M'" nil "M")
    ("P'" nil "P")
    ("Q'" nil "Q")
    ("T'" nil "T")
    ("V'" nil "V")
    ("X'" nil "X")
    ("y`" nil "y")
    ;; IPA
    ("i" ",0 (B" "i")
    ("I" ",0!(B" "I")
    ("e" ",0"(B" "e")
    ("/3" ",0#(B" "/3")
    ("E" ",0#(B" "E")
    ("ae" ",0$(B" "ae")
    ("a" ",0%(B" "a")
    ("i-" ",0&(B" "i-")
    ("/e" ",0'(B" "/e")
    ("/a" ",0((B" "/a")
    ("/m" ",0)(B" "/m")
    ("&" ",0*(B" "&")
    ("/v" ",0+(B" "/v")
    ("A" ",0,(B" "A")
    ("o|" ",0,(B" "o|")
    ("y" ",0-(B" "y")
    ("Y" ",0.(B" "Y")
    ("o/" ",0/(B" "o/")
    ("oe" ",00(B" "oe")
    ("OE" ",01(B" "OE")
    ("u-" ",02(B" "u-")
    ("o-" ",03(B" "o-")
    ("u" ",04(B" "u")
    ("U" ",05(B" "U")
    ("o" ",06(B" "o")
    ("/c" ",07(B" "/c")
    ("/A" ",08(B" "/A")
    ("|o" ",08(B" "|o")
    ("e-" ",0:(B" "e-")
    ("e|" ",0:(B" "e|")
    ("/3~" ",0;(B" "/3~")
    ("E~" ",0;(B" "E~")
    ("A~" ",0<(B" "A~")
    ("oe~" ",0=(B" "oe~")
    ("/c~" ",0>(B" "/c~")
    ("p" ",0@(B" "p")
    ("b" ",0A(B" "b")
    ("t" ",0B(B" "t")
    ("d" ",0C(B" "d")
    ("k" ",0D(B" "k")
    ("g" ",0E(B" "g")
    ("f" ",0F(B" "f")
    ("v" ",0G(B" "v")
    ("th" ",0H(B" "th")
    ("dh" ",0I(B" "dh")
    ("s" ",0J(B" "s")
    ("z" ",0K(B" "z")
    ("sh" ",0L(B" "sh")
    ("zh" ",0M(B" "zh")
    ("3" ",0M(B" "3")
    ("c," ",0N(B" "c,")
    ("x" ",0O(B" "x")
    ("/R" ",0P(B" "/R")
    ("h" ",0Q(B" "h")
    ("m" ",0R(B" "m")
    ("n" ",0S(B" "n")
    ("gn" ",0T(B" "gn")
    ("ng" ",0U(B" "ng")
    ("r" ",0V(B" "r")
    ("R" ",0W(B" "R")
    ("/r" ",0X(B" "/r")
    ("j" ",0Y(B" "j")
    ("l" ",0Z(B" "l")
    ("/y" ",0[(B" "/y")
    ("L" ",0\(B" "L")
    ("/h" ",0](B" "/h")
    ("w" ",0^(B" "w")
    ("M" ",0_(B" "M")
    ("'" ",0p(B" "'")
    ("`" ",0q(B" "`")
    (":" ",0r(B" ":")))

(put 'intern 'lookup-concrete-function 'lookup-gaiji-concrete-intern)
(defun lookup-gaiji-concrete-intern (code)
  (lookup-assoc-get lookup-gaiji-intern-table code))

(provide 'lookup-gaiji)

;;; lookup-gaiji.el ends here
