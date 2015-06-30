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

(mapc 'add-to-load-path
  `(
    ,baymacs-core-directory
;;        ,(concat user-emacs-directory "core/libs/")
;;        ,(concat user-dropbox-directory "emacs/")
  ))
