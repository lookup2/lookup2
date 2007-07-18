;;; lookup-compile.el

(defun lookup-bytecomp ()
  (setq lookup-byte-compiling t)
  (setq load-path (nconc '("." "agent") load-path))
  (mapcar 'load command-line-args-left)
  (mapc 'byte-compile-file command-line-args-left))

(defun lookup-autoload ()
  (require 'autoload)
  (let ((generated-autoload-file "lookup-autoloads.el")
	(make-backup-files nil))
    (with-temp-buffer
      (mapc 'generate-file-autoloads command-line-args-left)
      (write-file generated-autoload-file))))

;;; lookup-compile.el ends here
