;; Evil on

(with-eval-after-load 'evil
  (evil-mode 1)

  (defun move-to-splitted-window (&optional COUNT FILE)
    "Moves the point to the newly created window after splitting."
    (message "Splitted and move to created window.")
    (other-window 1)
    )

  ;; window-split の後に、そのウィンドウに移動するように
  (advice-add 'evil-window-split :after #'move-to-splitted-window)
  (advice-add 'evil-window-vsplit :after #'move-to-splitted-window)

  ;; key map
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  )

(require 'evil)
