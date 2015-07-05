(defun add-to-load-path (dir) (add-to-list 'load-path dir))

;; path
(defconst baymacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "設定ファイルのコアディレクトリ")
(defconst baymacs-banner-directory
  (expand-file-name (concat baymacs-core-directory "banners/"))
  "Baymacs banners directory.")
(defconst baymacs-banner-default-png
  (expand-file-name (concat baymacs-banner-directory "img/baymacs.png"))
  "Baymacs default banner image.")
(defconst baymacs-directory
  (expand-file-name (concat user-emacs-directory "baymacs/"))
  "Baymacs base directory.")
(defconst baymacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Baymacs storage area for persistent files")
(defconst baymacs-package-directory
  (expand-file-name (concat user-emacs-directory "el-get/"))
  "Baymacs directory which contains packages")
(defconst baymacs-package-manager-directory
  (expand-file-name (concat baymacs-package-directory "el-get/"))
  "Baymacs package manager directory")
(defconst baymacs-personal-elisps-directory
  (expand-file-name (concat baymacs-package-directory "elisps/"))
  "Baymacs package manager directory")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")
(defconst pcache-directory
  (concat baymacs-cache-directory "pcache"))
(unless (file-exists-p baymacs-cache-directory)
  (make-directory baymacs-cache-directory))

(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")

(defconst user-backup-directory
  (expand-file-name (concat user-emacs-directory "backup"))
  "User backup directory.")
(unless (file-exists-p user-backup-directory)
  (make-directory user-backup-directory))

;; バックバップの保存先変更
(setq backup-directory-alist
      (cons (cons ".*" user-backup-directory)
	    backup-directory-alist))
;; 自動保存ファイルの保存先変更
(setq auto-save-file-name-transforms
      `((".*" ,user-backup-directory t)))

(mapc 'add-to-load-path
  `(
    ,baymacs-core-directory
    ,baymacs-package-manager-directory
    ,baymacs-personal-elisps-directory
;;        ,(concat user-emacs-directory "core/libs/")
;;        ,(concat user-dropbox-directory "emacs/")
  ))
