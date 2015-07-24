;; OSX specific configuration

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
(setq x-select-enable-primary t)

(defun open-init-file-buffer (file)
  "Show an init file on current buffer"
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
	(switch-to-buffer buffer)
	(message "%s" file))
    (find-file (concat user-emacs-directory file)))
  )

(global-set-key (kbd "C-:") '(lambda () (interactive) (open-init-file-buffer "init.el")))
(global-set-key (kbd "C-\"") '(lambda () (interactive) (open-init-file-buffer "core/core-package-manager.el")))
(global-set-key (kbd "M-a") 'mark-whole-buffer)
