(with-eval-after-load 'eww
  ;; eww mode
  (evil-define-key 'normal eww-mode-map (kbd "r") 'eww-reload)
  (evil-define-key 'normal eww-mode-map (kbd "c 0") 'eww-copy-page-url)
  (evil-define-key 'normal eww-mode-map (kbd "N") 'eww-next-url)
  (evil-define-key 'normal eww-mode-map (kbd "P") 'eww-previous-url)
  (evil-define-key 'normal eww-mode-map (kbd "C-p") 'eww-back-url)
  (evil-define-key 'normal eww-mode-map (kbd "C-n") 'eww-forward-url)
  (evil-define-key 'normal eww-mode-map (kbd "&") 'eww-browse-with-external-browser)
  (evil-define-key 'normal eww-mode-map (kbd "b") 'eww-add-bookmark)
  (evil-define-key 'normal eww-mode-map (kbd "B") 'eww-list-bookmarks)
  (evil-define-key 'normal eww-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'normal eww-bookmark-mode-map (kbd "q") 'quit-window)

  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))

  (defun eww-disable-color ()
    "eww で文字色を反映させない"
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))

  (defun eww-enable-color ()
    "eww で文字色を反映させる"
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload)) 

  (defun find-help-in-rubikich-page (package)
    (interactive "sPackageName: ")
    (eww-browse-url (format "http://rubikitch.com/tag/package:%s/" package)))

  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)

  )
