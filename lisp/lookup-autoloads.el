;;; lookup-autoloads.el --- definition of autoloads for Lookup
;; This file automatically generated on July 11, 1999 by install.el


;;;### (autoloads (lookup-restore-cache lookup-dump-cache) "lookup-cache"
;;;;;;  "lisp/lookup-cache.el" (14216 11664))
;;; Generated autoloads from lisp/lookup-cache.el

(autoload (quote lookup-dump-cache) "lookup-cache" nil nil nil)

(autoload (quote lookup-restore-cache) "lookup-cache" nil nil nil)

(autoload (quote lookup-gaiji-concrete) "lookup-gaiji" nil nil nil)

(autoload (quote lookup-history-display) "lookup-history" nil nil nil)

(autoload (quote lookup-record-dictionary-used) "lookup-record" nil nil nil)

(autoload (quote lookup-record-entry-displayed) "lookup-record" nil nil nil)

;;;***

;;;### (autoloads (lookup-display-menu lookup-display-entries lookup-search-session)
;;;;;;  "lookup-search" "lisp/lookup-search.el" (14217 20456))
;;; Generated autoloads from lisp/lookup-search.el

(autoload (quote lookup-search-session) "lookup-search" nil nil nil)

(autoload (quote lookup-display-entries) "lookup-search" nil nil nil)

(autoload (quote lookup-display-menu) "lookup-search" nil nil nil)

;;;***

;;;### (autoloads (lookup-select-session) "lookup-select" "lisp/lookup-select.el"
;;;;;;  (14217 18484))
;;; Generated autoloads from lisp/lookup-select.el

(autoload (quote lookup-select-session) "lookup-module" nil nil nil)

;;;***

;;;### (autoloads (lookup-use-complement lookup-set-dictionary-options
;;;;;;  lookup-set-agent-options lookup-secondary-other-frame lookup-secondary-full-screen
;;;;;;  lookup-secondary lookup-selection-other-frame lookup-selection-full-screen
;;;;;;  lookup-selection lookup-region-other-frame lookup-region-full-screen
;;;;;;  lookup-region lookup-word-other-frame lookup-word-full-screen
;;;;;;  lookup-word lookup-pattern-other-frame lookup-pattern-full-screen
;;;;;;  lookup-pattern lookup lookup-version) "lookup" "lisp/lookup.el"
;;;;;;  (14217 12396))
;;; Generated autoloads from lisp/lookup.el

(autoload (quote lookup-version) "lookup" "\
Display the version string of Lookup.
With prefix argument, insert string at point." t nil)

(autoload (quote lookup) "lookup" "\
Start Lookup and display the list of your dictionaries.
If you have already started lookup, display the last status of buffers.
With prefix arg, you can choose which module you use." t nil)

(autoload (quote lookup-pattern) "lookup" "\
Search for the PATTERN." t nil)

(autoload (quote lookup-pattern-full-screen) "lookup" "\
Search for the PATTERN in full screen.
See `lookup-pattern' for details." t nil)

(autoload (quote lookup-pattern-other-frame) "lookup" "\
Search for the PATTERN in another frame.
See `lookup-pattern' for details." t nil)

(autoload (quote lookup-word) "lookup" "\
Search for the word near the cursor." t nil)

(autoload (quote lookup-word-full-screen) "lookup" "\
Search for the word near the cursor in full screen.
See `lookup-word' for details." t nil)

(autoload (quote lookup-word-other-frame) "lookup" "\
Search for the word near the cursor in another frame.
See `lookup-word' for details." t nil)

(autoload (quote lookup-region) "lookup" "\
Search for the region." t nil)

(autoload (quote lookup-region-full-screen) "lookup" "\
Search for the region in full screen.
See `lookup-region' for details." t nil)

(autoload (quote lookup-region-other-frame) "lookup" "\
Search for the region in another frame.
See `lookup-region' for details." t nil)

(autoload (quote lookup-selection) "lookup" "\
Search for the mouse's selection." t nil)

(autoload (quote lookup-selection-full-screen) "lookup" "\
Search for the mouse's selection in full screen.
See `lookup-selection' for details." t nil)

(autoload (quote lookup-selection-other-frame) "lookup" "\
Search for the mouse's selection in another frame.
See `lookup-selection' for details." t nil)

(autoload (quote lookup-secondary) "lookup" "\
Search for the mouse's secondary selection." t nil)

(autoload (quote lookup-secondary-full-screen) "lookup" "\
Search for the mouse's secondary selection in full screen.
See `lookup-secondary' for details." t nil)

(autoload (quote lookup-secondary-other-frame) "lookup" "\
Search for the mouse's secondary selection in another frame.
See `lookup-secondary' for details." t nil)

(autoload (quote lookup-set-agent-options) "lookup" nil nil nil)

(autoload (quote lookup-set-dictionary-options) "lookup" nil nil nil)

(autoload (quote lookup-use-complement) "lookup" nil nil nil)

;;;***

;;;### (autoloads (stem-english) "stem-english" "lisp/stem-english.el"
;;;;;;  (14188 3976))
;;; Generated autoloads from lisp/stem-english.el

(autoload (quote stem-english) "stem-english" "\
活用語尾を取り除く関数
与えられた語の元の語として可能性のある語の文字列長の昇順のリストを返す" nil nil)

;;;***

;;; lookup-autoloads.el ends here
