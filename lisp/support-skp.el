;;; support-skp.el --- support file for 『Super日本語大辞典』 EPWING Version.

;; If you convert the 『Super日本語大辞典』 to EPWING format by using EBStudio,
;; set `support-skp-gaiji-file' variable to your SKP2.map file.

;;; Code:

(defvar support-skp-gaiji-file nil
  "Location of EBStudio-style Gaiji File for SKP (SKP2.map).")

(if (and support-skp-gaiji-file (file-exists-p support-skp-gaiji-file))
    (setq lookup-support-options
          (list ':gaiji-table (ndeb-parse-gaiji-file support-skp-gaiji-file))))

(require 'lookup)

;;; support-skp.el ends here
