;; Evil 関連のロード

;; - evil
;; - evil-leader
;; - evil-jumper
;; - evil-nerd-commenter

;; evil-core
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

;; evil-leader
(with-eval-after-load 'evil-leader
  (global-evil-leader-mode) ;; evil-leader
  )

;; evil-jumper
(with-eval-after-load 'evil-jumper
  (global-evil-jumper-mode)
  )

(with-eval-after-load 'evil-nerd-commenter
  (evilnc-default-hotkeys)
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-lines)
  (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
  )

(defun after-all-loads ()
  (require 'evil)
  (require 'evil-leader)
  (require 'evil-jumper)
  (require 'evil-nerd-commenter)
  )

(add-hook 'after-init-hook 'after-all-loads)
