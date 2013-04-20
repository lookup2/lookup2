;;; bocu.el --- BOCU decoding software -*- lexical-binding: t -*-
;; Copyright (C) 2009 Lookup Development Team

;; Author: KAWABATA Taichi <kawabata.taichi@gmail.com>
;; Keywords: Unicode, BOCU

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

;;; Documentation:

;; This decodes BOCU (Binary Ordered Compression for Unicode) string.

(eval-and-compile
  (defun bocu-read-decode-trail-char (reg)
    "BOCU trail char in REG to be decoded."
    `(read-if (,reg > #x20) (,reg -= 13)
	      (if (,reg >= #x1c) (,reg -= 12)   
		(if (,reg >= #x10) (,reg -= 10)
		  (,reg -= 1))))))

(define-ccl-program decode-bocu
  `(4
    ((r4 = #x40)
     (r3 = ,(charset-id-internal 'unicode))
     (loop
      (read r0)
      ;; Diff calculation phase
      (if (r0 <= #x20) (r1 = r0)
        (if (r0 == #x21)
            ((r1 = -14536567)
             ,(bocu-read-decode-trail-char 'r2)
             (r1 += (r2 * 59049))
             ,(bocu-read-decode-trail-char 'r2)
             (r1 += (r2 * 243))
             ,(bocu-read-decode-trail-char 'r2)
             (r1 += r2))
          (if (r0 < #x25)
              ((r1 = (((r0 - #x25) * 59049) - 10513))
               ,(bocu-read-decode-trail-char 'r2)
               (r1 += (r2 * 243))
               ,(bocu-read-decode-trail-char 'r2)
               (r1 += r2))
            (if (r0 < #x50)
                ((r1 = (((r0 - #x50) * 243) - 64))
                 ,(bocu-read-decode-trail-char 'r2)
                 (r1 += r2))
              (if (r0 < #xd0)
                  (r1 = (r0 - #x90))
                (if (r0 < #xfb)
                    ((r1 = (((r0 - #xd0) * 243) + 64))
                     ,(bocu-read-decode-trail-char 'r2)
                     (r1 += r2))
                  (if (r0 < #xfe)
                      ((r1 = (((r0 - #xfb) * 59049) + 10513))
                       ,(bocu-read-decode-trail-char 'r2)
                       (r1 += (r2 * 243))
                       ,(bocu-read-decode-trail-char 'r2)
                       (r1 += r2))
                    (if (r0 == #xfe)
                        ((r1 = 187660)
                         ,(bocu-read-decode-trail-char 'r2)
                         (r1 += (r2 * 59049))
                         ,(bocu-read-decode-trail-char 'r2)
                         (r1 += (r2 * 243))
                         ,(bocu-read-decode-trail-char 'r2)
                         (r1 += r2)
                         ;; ignore case: `r0 = #xff'
                         )))))))))
      ;; output stage
      (if (r0 <= #x20)
          ((if (r0 != 13) (write r0))
           (if (r0 < #x20) (r4 = #x40)))
        (if (r0 < #xff)
            ((r1 += r4)
             (if (r1 < 0) (r1 = 0)) ; error recovery
             (write-multibyte-character r3 r1)
             ;; cp renewal stage
             (if (r1 < #x20) (r4 = #x40) ; reset
               (if (r1 == #x20) (r4 = r4) ; space → keep
                 ((r5 = (r1 >= #x3040))
                  (r6 = (r1 <= #x309f))
                  (if (r5 & r6) (r4 = #x3070)
                    ((r5 = (r1 >= #x4e00))
                     (r6 = (r1 <= #x9fa5))
                     (if (r5 & r6) (r4 = #x7711)
                       ((r5 = (r1 >= #xac00))
                        (r6 = (r1 <= #xd7a3))
                        (if (r5 & r6) (r4 = #xc1d1)
                          ((r5 = (r1 & #xff))
                           ;; Warning:: some old Emacs treats #xffffff00 as float.
                           ;;(r6 = (r1 & #xffffff00)) ;; FIXME
                           (r6 = (r1 & -256))
                           (if (r5 < #x80) (r4 = (r6 + #x40))
                             (r4 = (r6 + #xc0)))))))))))))))
      (repeat)))))

;;###autoload
(defun bocu-to-str (string)
  "Decode BOCU STRING to Emacs String."
  (ccl-execute-on-string 'decode-bocu (vector 0 0 0 0 0 0 0 0 0) string))

(provide 'bocu)
