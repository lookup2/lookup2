
;;;
;;; Auto-Lookup
;;;

(defvar lookup-auto-lookup-mode nil)
(defvar lookup-auto-lookup-timer nil)
(defvar lookup-auto-lookup-word "")

(defvar lookup-auto-lookup-interval 1.00)
(defvar lookup-auto-lookup-open-function 'lookup-other-window)

(defun lookup-toggle-auto-lookup()
  (interactive)
  (if lookup-auto-lookup-mode 
      (lookup-deactivate-auto-lookup)
    (lookup-activate-auto-lookup)))

(defun lookup-activate-auto-lookup ()
  "Activate Auto Lookup."
  (interactive)
  (setq lookup-auto-lookup-mode nil)
  (unless (lookup-get-module "auto")
    (error "Please prepare `auto' module before using auto-lookup!"))
  (setq lookup-auto-lookup-mode t)
  (setq lookup-auto-lookup-timer
        (run-with-idle-timer
         lookup-auto-lookup-interval t
         'lookup-auto-lookup)))

(defun lookup-deactivate-auto-lookup ()
  "Deactivate Auto Lookup."
  (interactive)
  (setq lookup-auto-lookup-mode nil)
  (cancel-timer lookup-auto-lookup-timer))

(defun lookup-auto-lookup ()
  "Execute Auto Lookup."
  (when (and (not isearch-mode)
             (not executing-kbd-macro)
             lookup-auto-lookup-mode
             (not (window-minibuffer-p (selected-window)))
             (not (eq (current-buffer) (get-buffer (lookup-summary-buffer))))
             (not (eq (current-buffer) (get-buffer (lookup-content-buffer))))
             (not (eq (current-buffer) (get-buffer " *Dictionary List*")))
             (not (eq (current-buffer) (get-buffer " *Search History*")))
             (not (eq (current-buffer) (get-buffer " *Module List*"))))
    (save-selected-window
      (save-match-data
        (save-excursion
          (save-restriction
            (let ((lookup-edit-input nil)
                  (word (lookup-current-word))
                  (lookup-open-function lookup-auto-lookup-open-function))
              (when (not (equal lookup-auto-lookup-word word))
                (lookup-word word (lookup-get-module "auto"))
                (setq lookup-auto-lookup-word word)))))))))
