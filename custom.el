(defun custom/org-browse ()
  (interactive)
  (let ((default-directory (file-truename (expand-file-name "~/org/"))))
    (call-interactively #'counsel-find-file)))

(defun custom/scratch-toggle ()
  (interactive)
  (if (string= "*scratch*" (buffer-name))
      (previous-buffer)
    (switch-to-buffer "*scratch*")))
