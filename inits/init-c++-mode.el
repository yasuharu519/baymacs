;; c++-mode

(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))

(custom-set-variables
 '(c-default-style "bsd")
 '(c-basic-offset 2) ;; if の次とかで改行しない
 )
