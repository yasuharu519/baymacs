;; hl-mode

(autoload 'hl-todo-mode "hl-todo" nil t)

(dolist (hook '(c-mode-hook
		c++-mode-hook
		emacs-lisp-mode-hook
		js-mode-hook
		js2-mode-hook
		org-mode-hook
		))
  (add-hook hook 'hl-todo-mode))
