;;; lookup-compile.el

(push "." load-path)

(defun lookup-bytecomp ()
  (setq lookup-byte-compiling t)
  (mapc 'load command-line-args-left)
  (mapc 'byte-compile-file command-line-args-left))

(defun lookup-autoload ()
  (require 'autoload)
  (let ((generated-autoload-file "lookup-autoloads.el")
	(make-backup-files nil))
    (with-temp-buffer
      (mapc 'generate-file-autoloads command-line-args-left)
      (write-file generated-autoload-file))))

;;; lookup-compile.el ends here
