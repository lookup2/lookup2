;;; oxford.el --- complement file for "Oxford Dictionary/Thesaurus"
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

(require 'lookup)

(defconst oxford-gaiji-table
  (lookup-new-gaiji-table
   '(("h0f20" "O/") ("h0f21" "o/") ("h0f22" "*") ("h0f23" "'a") ("h0f24" "~i")
     ("h0f25" "~a") ("h0f26" "&") ("h0f27" "`u") ("h0f28" "`u") ("h0f29" "e")
     ("h0f2a" "#oa") ("h0f2b" "oe") ("h0f2c" "oe") ("h0f2d" "(n)")
     ("h0f2e" "(+)") ("h0f2f" "(y)") ("h0f30" "[0]") ("h0f31" "[1]")
     ("h0f32" "[2]") ("h0f33" "[3]") ("h0f34" "[4]") ("h0f35" "[5]")
     ("h0f36" "[6]") ("h0f37" "[7]") ("h0f38" "[8]") ("h0f39" "[9]")
     ("h0f3a" ":") ("h0f3b" ";") ("h0f3c" "(0)") ("h0f3d" "(8)")
     ("h0f3e" "(9)") ("h0f3f" "(0)") ("h0f40" "sprt3") ("h0f41" "A")
     ("h0f42" "B") ("h0f43" "C") ("h0f44" "D") ("h0f45" "E") ("h0f46" "F")
     ("h0f47" "G") ("h0f48" "H") ("h0f49" "I") ("h0f4a" "J") ("h0f4b" "K")
     ("h0f4c" "L") ("h0f4d" "M") ("h0f4e" "N") ("h0f4f" "O") ("h0f50" "P")
     ("h0f51" "Q") ("h0f52" "R") ("h0f53" "S") ("h0f54" "T") ("h0f55" "U")
     ("h0f56" "V") ("h0f57" "W") ("h0f58" "X") ("h0f59" "Y") ("h0f5a" "Z")
     ("h0f5b" "sqrt") ("h0f5c" "'E") ("h0f5d" "(C)") ("h0f5e" "#oA")
     ("h0f5f" "~ae") ("h0f60" "\"o") ("h0f61" "a") ("h0f62" "b") ("h0f63" "c")
     ("h0f64" "d") ("h0f65" "e") ("h0f66" "f") ("h0f67" "g") ("h0f68" "h")
     ("h0f69" "i") ("h0f6a" "j") ("h0f6b" "k") ("h0f6c" "l") ("h0f6d" "m")
     ("h0f6e" "n") ("h0f6f" "o") ("h0f70" "p") ("h0f71" "q") ("h0f72" "r")
     ("h0f73" "s") ("h0f74" "t") ("h0f75" "u") ("h0f76" "v") ("h0f77" "w")
     ("h0f78" "x") ("h0f79" "y") ("h0f7a" "z") ("h0f7b" "^u") ("h0f7c" "~u")
     ("h0f7d" "v") ("h0f7e" "*") ("h0f7f" "~n") ("h0f80" "'u") ("h0f81" "\"u")
     ("h0f82" "'e") ("h0f83" "^a") ("h0f84" "\"a") ("h0f85" "`a")
     ("h0f86" "^i") ("h0f87" "c,") ("h0f88" "^e") ("h0f89" "^o") ("h0f8a" "`e")
     ("h0f8b" "\"i") ("h0f8c" "#oa") ("h0f8d" "<C") ("h0f8e" "<c")
     ("h0f8f" "'a") ("h0f90" ",") ("h0f91" "<1>") ("h0f92" "<2>")
     ("h0f93" "<3>") ("h0f94" "<4>") ("h0f95" "<5>") ("h0f96" "<6>")
     ("h0f97" "a") ("h0f98" "~a") ("h0f99" "`a") ("h0f9a" "'i") ("h0f9b" "=")
     ("h0f9c" "(R)") ("h0f9d" "[D]") ("h0f9e" "[T]") ("h0f9f" "\"o")
     ("h0fa0" ",") ("h0fa1" "(1)") ("h0fa2" "(2)") ("h0fa3" "(3)")
     ("h0fa4" "(4)") ("h0fa5" "(5)") ("h0fa6" "(6)") ("h0fa7" "(7)")
     ("h0fa8" "(8)") ("h0fa9" "(9)") ("h0faa" "(+)") ("h0fab" "(-)")
     ("h0fac" "\"e") ("h0fad" "\"A") ("h0fae" "ae") ("h0faf" "'o")
     ("h0fb0" "3") ("h0fb1" "S") ("h0fb2" "n,") ("h0fb3" "0-") ("h0fb4" "6`")
     ("h0fb5" "ae") ("h0fb6" "i") ("h0fb7" "#e") ("h0fb8" "#a") ("h0fb9" "-o-")
     ("h0fba" "#v") ("h0fbb" "3") ("h0fbc" "#c") ("h0fbd" "~#c")
     ("h0fbe" "#oA") ("h0fbf" "\"O") ("h0fc0" "`a") ("h0fc1" "a") ("h0fc2" "b")
     ("h0fc3" "c") ("h0fc4" "d") ("h0fc5" "e") ("h0fc6" "f") ("h0fc7" "g")
     ("h0fc8" "h") ("h0fc9" "i") ("h0fca" "j") ("h0fcb" "k") ("h0fcc" "l")
     ("h0fcd" "m") ("h0fce" "n") ("h0fcf" "o") ("h0fd0" "p") ("h0fd1" "q")
     ("h0fd2" "r") ("h0fd3" "s") ("h0fd4" "t") ("h0fd5" "u") ("h0fd6" "v")
     ("h0fd7" "w") ("h0fd8" "x") ("h0fd9" "y") ("h0fda" "z") ("h0fdb" "'e")
     ("h0fdc" "c,") ("h0fdd" "`e") ("h0fde" "^e") ("h0fdf" "^a")
     ("h0fe0" "\"a") ("h0fe1" "A") ("h0fe2" "B") ("h0fe3" "C") ("h0fe4" "D")
     ("h0fe5" "E") ("h0fe6" "F") ("h0fe7" "G") ("h0fe8" "H") ("h0fe9" "I")
     ("h0fea" "J") ("h0feb" "K") ("h0fec" "L") ("h0fed" "M") ("h0fee" "N")
     ("h0fef" "O") ("h0ff0" "P") ("h0ff1" "Q") ("h0ff2" "R") ("h0ff3" "S")
     ("h0ff4" "T") ("h0ff5" "U") ("h0ff6" "V") ("h0ff7" "W") ("h0ff8" "X")
     ("h0ff9" "Y") ("h0ffa" "Z") ("h0ffb" "\"u") ("h0ffc" "^i") ("h0ffd" "^o")
     ("h0ffe" "~n"))))

(defun oxford-arrange-structure (entry)
  (while (re-search-forward "\\( \\([nv]\\|adj\\)\\.\\)\\|\\[[0-9]\\]" nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (newline))))

(setq lookup-complement-options
      (list ':title "Oxford Dictionary"
	    ':coding 'iso-8859-1
	    ':stop-code "0x1f090000"
	    ':gaiji-table oxford-gaiji-table
	    ':arrange-table '((structure . oxford-arrange-structure))))

;;; oxford.el ends here
