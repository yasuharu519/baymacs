;; yasnippet

;; functions
(defun yasnippet/open-snippet-dir ()
  (interactive)
  (dired yas-snippet-dirs)
  )

(custom-set-variables
 '(yas-snippet-dirs (concat user-emacs-directory "snippets"))
 )

(with-eval-after-load 'yasnippet
  (yas-reload-all)

  (evil-define-key 'insert yas-minor-mode-map (kbd "C-k") 'yas-expand)
  (evil-define-key 'normal yas-minor-mode-map (kbd "C-SPC") 'yasnippet/open-snippet-dir)
  (define-key yas-keymap (kbd "C-k") 'yas-next-field-or-maybe-expand)
  )

(autoload 'yas-minor-mode "yasnippet" nil t)

(dolist (hook '(c-mode-hook
		c++-mode-hook
		emacs-lisp-mode-hook
		js-mode-hook
		js2-mode-hook
		org-mode-hook
		))
  (add-hook hook 'yas-minor-mode))

