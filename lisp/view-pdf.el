;;; view-pdf.el --- view PDF of specified page.
;;
;; Author:    Taichi Kawabata <kawabata.taichi _at_ gmail.com>
;; License:   GPL v3
;; Keywords:  pdf, applescript

;;; Commentary:

;; This elisp enables to view PDF via URL of
;; "file:///path/to/file.pdf#page=XXX".  
;;
;; This elisp changes the value of `browse-url-browse-function', so
;; that URL such that "file://path/to/file.pdf#page=XXX" will be
;; displayed by PDF viewer applications.
;;
;; Since Acrobat Reader nor Preview do not accept the page
;; specification when viewing the PDF file, `osascript' is applied.

;;; References
;; http://partners.adobe.com/public/developer/en/acrobat/PDFOpenParameters.pdf

(eval-when-compile (require 'cl))

(defvar view-pdf-mac-app 'preview
  "Either `preview' or `adobe-reader' can be specified.")

(defvar view-pdf-gnu/linux-app 'evince
  "Either 'evince' or `xpdf' can be specified.")

(defconst view-pdf-mac-params
  '((preview      "Preview"      "g" "command down, option down")
    (adobe-reader "Adobe Reader" "n" "command down, shift down")))

(defun view-pdf-mac (file page)
  "Return AppleScript that opens specified PDF FILE and PAGE."
  (let ((temp-file (concat (make-temp-file "osa-") ".applescript")))
    (with-temp-file temp-file
      (insert
       (destructuring-bind
           (appname key modifier-keys)
           (assoc-default view-pdf-mac-app view-pdf-mac-params)
         (concat "
tell application \"" appname "\"
  activate
  open \"" (file-truename file) "\" as POSIX file
end tell
tell application \"System Events\"
  tell process \"" appname "\"
    keystroke \"" key "\" using {" modifier-keys "}
    keystroke \"" (number-to-string page) "\"
    keystroke return
  end tell
end tell"))))
    (call-process "osascript" nil nil nil "-ss" temp-file)))

;; If you use xpdf, you should launch `xpdf -remote myServer&' beforehand.
(defconst view-pdf-gnu/linux-params
  '((evince (format "evince --page-index=%d \"%s\" &" page file))
    (xpdf   (format "xpdf -remote myServer -raise \"%s\" %d" file page))))

(defconst view-pdf-win-path
  "c:/Program Files/Adobe/Reader 10.0/Reader/AcroRd32.exe\" /A \"page=%d=OpenActions\" %s &")
;; "start AcroRD32.exe /a \"page=%s\" %d"

(defun view-pdf (file page)
  "Open PDF FILE with specified PAGE.
Warning: Keep your fingers off the modifier keys while invoking this command."
  (setq file (file-truename file))
  (cond 
   ((eq system-type 'darwin)
    (view-pdf-mac file page))
   ((eq system-type 'gnu/linux)
    (shell-command (eval (car (assoc-default view-pdf-gnu/linux-app
                                             view-pdf-gnu/linux-params)))))
   ((eq system-type 'windows-nt)
    (shell-command (format test page file)))
   (t
    (error "view-pdf: not supported platform"))))

;;; browse-url 設定

(when (symbolp browse-url-browser-function)
  (setq browse-url-browser-function '(("." . browse-url-browser-function))))
(add-to-list 'browse-url-browser-function
             '("^file:///.*pdf#.*page=[0-9]+" . view-pdf-url))

(defun view-pdf-url (url &rest args)
  (string-match "^file://\\(.+pdf\\)#.*page=\\([0-9]+\\)" url)
  (message "debug! url=%s file=%s" url (match-string 1 url))
  (view-pdf (match-string 1 url)
            (string-to-number (match-string 2 url))))

(provide 'view-pdf)
