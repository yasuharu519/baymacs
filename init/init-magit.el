;; magit

(with-eval-after-load 'magit
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'magit-commit-mode 'motion)
  (evil-set-initial-state 'magit-log-mode 'motion)
  (evil-set-initial-state 'magit-wassup-mode 'motion)
  (evil-set-initial-state 'magit-mode 'motion)
  (evil-set-initial-state 'git-rebase-mode 'motion)

  )

(defun baymacs/magit-mode-hook ()
  )

(evil-leader/set-key
"gs" 'magit-status)

(add-hook 'magit-mode-hook 'baymacs/magit-mode-hook)
