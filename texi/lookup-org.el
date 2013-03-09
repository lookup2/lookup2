;;; lookup-org.el --- export lookup.org to texi file.  -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp/")
(require 'ox-texinfo)

(defun lookup-texi-generate ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/Dropbox/cvs/lookup2/texi/lookup.org")
    (org-mode)
    (org-export-to-file 'texinfo "~/Dropbox/cvs/lookup2/texi/lookup.texi")))
