(defun pretty-lambda ()
  (setq prettify-symbols-alist
	'(
	  ("lambda" . ?λ)
	  )))
(defun pretty-function ()
  (setq prettify-symbols-alist
	'(
	  ("function" . ?⨍)
	  )))

(add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
(add-hook 'javascript-mode-hook 'pretty-function)
(add-hook 'js-mode-hook 'pretty-function)
(add-hook 'web-mode-hook 'pretty-function)
(global-prettify-symbols-mode 1)
