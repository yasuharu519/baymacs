;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)

(with-eval-after-load 'auto-complete
  (ac-config-default)
  )

(defvar basic-ac-sources
  '(ac-source-yasnippet,
    ac-source-abbrev,
    ac-source-dictionary,
    ac-source-words-in-same-mode-buffers))

(defun ac-c++-mode-setup ()
  (setq-default ac-sources (append '(ac-source-irony) basic-ac-sources)))

;; 追加
(add-to-list 'ac-modes 'c++-mode)

(add-hook 'c++-mode-hook 'ac-c++-mode-setup)

