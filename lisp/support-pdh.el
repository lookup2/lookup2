;;; support-pdh.el --- support file for 「私立PDH図書館『百科辞書』」 EPWING Version.

;; If you prepare the 「私立PDH図書館『百科辞書』」, 
;; set `support-pdh-gaiji-file' variable to your pdh_gaiji.map file.

;;; Code:

(require 'ndeb)

(defvar support-pdh-gaiji-file nil
  "Location of EBStudio-style Gaiji File `pdh_gaiji.map'.")

(if (and support-pdh-gaiji-file (file-exists-p support-pdh-gaiji-file))
    (setq lookup-support-options
          (list ':gaiji-table (ndeb-parse-gaiji-file support-pdh-gaiji-file))))

;;; support-pdh.el ends here
