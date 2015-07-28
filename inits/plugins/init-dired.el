;; dired

(defun yasuharu519/dired-down-directory ()
  "[Dired command] Go down to the directory."
  (interactive)
  (condition-case err
      (let ((path (dired-get-file-for-visit)))
        (if (file-directory-p path)
            (dired-find-file)
            (message "This is not directory!")))
    (error (message "%s" (cadr err)))))

(evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
(evil-define-key 'normal dired-mode-map (kbd "l") 'yasuharu519/dired-down-directory)
