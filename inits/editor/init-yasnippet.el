;; yasnippet

(custom-set-variables
 '(yas-snippet-dirs (concat user-emacs-directory "snippets"))
 )

(with-eval-after-load 'yasnippet
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-k") 'yas-expand)
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
