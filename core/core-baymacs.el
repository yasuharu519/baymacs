(require 'evil-leader)
(require 'evil)
(require 'powerline)
(require 'guide-key)
(require 'helm)
(require 'helm-utils)
(require 'projectile)
(require 'volatile-highlights)
(require 'irony)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'evil-jumper)

(require 'core-functions)


(defun core-baymacs/init-global-mode ()
    (global-evil-leader-mode) ;; evil-leader
    (guide-key-mode 1)
    (evil-mode 1)
    (helm-mode 1)
    (projectile-global-mode)
    (volatile-highlights-mode t)
    (global-anzu-mode +1)
    (ac-config-default)
    (global-evil-jumper-mode)
    )

(defun core-baymacs/init-powerline ()
    ;; パワーライン
    (powerline-center-evil-theme)
    (setq-default powerline-default-separator 'wave)
  )

(defun core-baymacs/init-ui ()
    (menu-bar-mode 0) ;; メニューバーの設定
    (tool-bar-mode 0) ;; ツールバーの設定
    (scroll-bar-mode 0) ;; スクロールバーの設定
    (global-linum-mode t) ;; 行番号表示
    (helm-gtags-mode)
    )

(defmacro baymacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "baymacs/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (evil-leader/set-key ,keys ',func-name))))

(defun core-baymacs/init-keymaps ()
  (define-key global-map (kbd "C-;") 'helm-buffers-list)
  
    ;; keymap
    (evil-leader/set-leader "SPC")
    
    (evil-leader/set-key
    "e" 'find-file
    "ad"  'dired
    "ab"  'helm-mini
    "ff"  'helm-find-files
    "fr"  'helm-recentf
    ";"   'helm-M-x)
    ;; projectile
    (require 'helm-projectile)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (setq projectile-enable-caching t)
    (evil-leader/set-key
      "pa" 'helm-projectile
      "pp" 'helm-projectile-switch-project
      "pf" 'helm-projectile-find-file
      "pr" 'helm-projectile-recentf
      "pg" 'helm-projectile-ag)
    (setq guide-key/guide-key-sequence '("SPC"))
    (setq guide-key/recursive-key-sequence-flag t)

    ;; Make <escape> quit as much as possible
    (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)
    (define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
    (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
    (define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
    (define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
    (define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
    (define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
    (define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit) 

    ;; helm 
    ; (define-key helm-map (kbd "C-h") 'delete-backward-char)
    ; (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

    ;; C++
    (evil-define-key 'insert c++-mode-map (kbd "C-l") (lambda () (interactive)
							(insert "->")))
    ;; helm-gtags
    (require 'helm-gtags)
    (setq
	helm-gtags-ignore-case t
	helm-gtags-auto-update t
	helm-gtags-use-input-at-cursor t
	helm-gtags-pulse-at-cursor t
	helm-gtags-suggested-key-mapping t
	)
    (evil-define-key 'normal helm-gtags-mode-map (kbd "C-]") 'helm-gtags-find-tag-from-here)
    (evil-define-key 'normal helm-gtags-mode-map (kbd "C-j") 'helm-gtags-find-pattern)


    ;; Enable helm-gtags-mode
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)   
    
    (baymacs||set-helm-key "hdb" describe-bindings)
    (baymacs||set-helm-key "hdc" describe-char)
    (baymacs||set-helm-key "hdf" describe-function)
    (baymacs||set-helm-key "hdk" describe-key)
    (baymacs||set-helm-key "hdm" describe-mode)
    (baymacs||set-helm-key "hdp" describe-package)
    (baymacs||set-helm-key "hdt" describe-theme)
    (baymacs||set-helm-key "hdv" describe-variable)
    (baymacs||set-helm-key "hL" helm-locate-library)
    ;; search functions -----------------------------------------------------------
    (baymacs||set-helm-key "sww" helm-wikipedia-suggest)
    (baymacs||set-helm-key "swg" helm-google-suggest)

    )

(defun core-baymacs/init-copy-paste ()
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)
    (setq x-select-enable-primary t)
  )

(defun core-baymacs/init-magit ()
  (require 'magit)
  (require 'evil)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)
    (evil-set-initial-state 'git-commit-mode 'insert)
    (evil-set-initial-state 'magit-commit-mode 'motion)
    (evil-set-initial-state 'magit-log-mode 'motion)
    (evil-set-initial-state 'magit-wassup-mode 'motion)
    (evil-set-initial-state 'magit-mode 'motion)
    (evil-set-initial-state 'git-rebase-mode 'motion)

    ;; set-key
    (evil-leader/set-key
      "gs" 'magit-status)

    )

(defun core-baymacs/init-evil ()
  (require 'evil)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

  (defun move-to-splitted-window (&optional COUNT FILE)
    "Moves the point to the newly created window after splitting."
    (message "Splitted and move to created window.")
    (other-window 1)
    )

  ;; window-split の後に、そのウィンドウに移動するように
  (advice-add 'evil-window-split :after #'move-to-splitted-window)
  (advice-add 'evil-window-vsplit :after #'move-to-splitted-window)

  )

(defun core-baymacs/init-org ()
  (require 'org-install) ;; 初期設定
  (require 'org-capture) ;; org-remember モードが変わった

  (define-key global-map (kbd "C-c r") 'org-capture)
  )

(defun core-baymacs/init ()
  (core-baymacs/init-global-mode)
  (core-baymacs/init-powerline)
  (core-baymacs/init-ui)
  (core-baymacs/init-keymaps)
  (core-baymacs/init-copy-paste)
  (core-baymacs/init-magit)
  (core-baymacs/init-evil)
  (core-baymacs/init-org)
  )

(core-baymacs/init)


(provide 'core-baymacs)
