(defun org-browse
    "Traverse org mode notes directory"
  (let ((default-directory (file-truename (expand-file-name "~/org"))))
    (call-interactively #'counsel-find-file)))
