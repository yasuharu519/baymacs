(defconst baymacs-version          "0.0.1" "Personal version.")
(defconst baymacs-emacs-min-version   "24.3" "Minimal version of Emacs.")

(defun baymacs/emacs-version-ok ()
  (version<= baymacs-emacs-min-version emacs-version))

;; evilの設定
;; http://d.hatena.ne.jp/tarao/20130304/evil_config
(setq evil-want-C-u-scroll t
      evil-search-module 'evil-search
      evil-ex-search-vim-style-regexp t)

(when (baymacs/emacs-version-ok)

  ;; ロードパスのロード
  (load (concat user-emacs-directory "core/core-load-path.el"))
  (require 'core-package-manager)
  (baymacs/core-package-manager-init)

  (load-theme 'monokai t t)
  (enable-theme 'monokai)

  ;; 環境変数を引き継ぐ
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  ;; init-loader
  (require 'init-loader)
  (custom-set-variables
    '(init-loader-byte-compile t)
    '(init-loader-show-log-after-init 'error-only) ;; エラーだけ表示
    )
  (init-loader-load (concat user-emacs-directory "init-loader"))

  (require 'core-baymacs)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
