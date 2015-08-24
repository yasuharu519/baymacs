;; Evil 関連のロード

;; - evil
;; - evil-leader
;; - evil-jumper
;; - evil-nerd-commenter

;; evil-core
(custom-set-variables
 '(evil-cross-lines t)
 )

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

  (defun open-next-line (arg)
    "Move to the next line and then opens a line.
    See also `newline-and-indent'."
    (interactive "p")
    (save-excursion
      (end-of-line)
      (open-line arg)
      (next-line 1)))

  (defun open-previous-line (arg)
    "Open a new line before the current one.
     See also `newline-and-indent'."
    (interactive "p")
    (save-excursion
      (beginning-of-line)
      (open-line arg)))

  ;; key map
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-normal-state-map (kbd "] SPC") 'open-next-line)
  (define-key evil-normal-state-map (kbd "[ SPC") 'open-previous-line)

  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
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
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  )

(require 'evil)
(require 'evil-leader)
(require 'evil-jumper)
(require 'evil-nerd-commenter)
