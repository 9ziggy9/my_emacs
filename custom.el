(defun custom/org-browse ()
  (interactive)
  (let ((default-directory (file-truename (expand-file-name "~/org/"))))
    (call-interactively #'counsel-find-file)))
