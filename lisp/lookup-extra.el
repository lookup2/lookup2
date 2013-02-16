
;; lookup-cache.el
;(defun lookup-dump-module-attributes--session (session)
;  (let ((type (lookup-session-type session)))
;    (cond ((eq type 'lookup-select-session) (list type))
;	  ((eq type 'lookup-search-query)
;	   (let ((query (lookup-session-query session)))
;	     (cons type (list (lookup-query-method query)
;			      (lookup-query-string query)
;			      (lookup-query-pattern query))))))))




;;;
;;; simple grep utility
;;;
(setq lookup-grep-program "grep")
(defmacro lookup-with-grep-file (pattern file options &rest body)
  (declare (indent 3))
  `(with-temp-buffer
     (let ((fname (file-truename ,file)))
       (call-process lookup-grep-program nil t nil ,@options ,pattern fname)
       ,@body)))


;;; Module Commands

(defun lookup-nth-module (n &optional module)
  (let* ((len (length lookup-module-list))
	 (pos (if module (position module lookup-module-list) 0)))
    (setq pos (% (+ pos n) len))
    (if (< pos 0) (setq pos (+ pos len)))
    (nth pos lookup-module-list)))

(defun lookup-forward-module (arg)
  "Forward current module by ARG.
New session begins if there is a session.  If called from 
`lookup-select-mode' or `lookup-modules-mode', then default
module will be changed."
  (interactive "p")
  (let ((session (lookup-current-session)) 
        module query)
    (if (or (eq major-mode 'lookup-select-mode)
            (eq major-mode 'lookup-modules-mode)
            (null session))
        (progn
          (dotimes (x (mod arg (length lookup-module-list)))
            (setq lookup-module-list (nconc (cdr lookup-module-list)
                                            (list (car lookup-module-list)))))
          (setq module (car lookup-module-list))
          (cond ((eq major-mode 'lookup-select-mode)
                 (setf (lookup-session-module session) module)
                 (lookup-select-dictionaries module))
                ((eq major-mode 'lookup-modules-mode)
                 (setf (lookup-session-module session) module)
                 (lookup-list-modules))
                (t (message "Current module in this buffer is %s" 
                            (lookup-current-module)))))
      (setq module (lookup-nth-module arg (lookup-current-module)))
      (setq query (lookup-session-query session))
      (if (not (eq (lookup-query-method query) 'reference))
          (lookup-search-query module query)
        (error
         "Current session handles `reference'.  Please exit session first")))
    (princ (lookup-module-name module))))

(defun lookup-backward-module (arg)
  (interactive "p")
  (lookup-forward-module (- arg)))

;;; 

