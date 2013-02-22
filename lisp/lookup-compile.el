;;; lookup-compile.el

(defun lookup-bytecomp ()
  (setq lookup-byte-compiling t)
  (push "." load-path)
  (mapc 'load command-line-args-left)
  (mapc 'byte-compile-file command-line-args-left))

(defun lookup-autoload ()
  (require 'autoload)
  (mapc (lambda (file)
          (update-file-autoloads file nil
           (expand-file-name "lookup-autoloads.el" default-directory)))
        command-line-args-left))

;;; lookup-compile.el ends here
