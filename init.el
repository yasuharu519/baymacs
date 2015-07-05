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
  (load-file (concat user-emacs-directory "core/core-load-path.el"))
  (require 'core-package-manager)
  (baymacs/core-package-manager-init)
  (baymacs/core-package-manager-install-packages)

  (require 'core-baymacs)
  )


