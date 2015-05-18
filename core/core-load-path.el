(defun add-to-load-path (dir) (add-to-list 'load-path dir))

;; path
(defconst personal-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "設定ファイルのコアディレクトリ")
(defconst personal-banner-directory
  (expand-file-name (concat personal-core-directory "banners/"))
  "Personal banners directory.")
(defconst personal-banner-default-png
  (expand-file-name (concat personal-banner-directory "img/spacemacs.png"))
  "Personal default banner image.")
(defconst personal-directory
  (expand-file-name (concat user-emacs-directory "personal/"))
  "Personal base directory.")
(defconst personal-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Personal storage area for persistent files")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")
(defconst pcache-directory
  (concat personal-cache-directory "pcache"))
(unless (file-exists-p personal-cache-directory)
  (make-directory personal-cache-directory))

(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")

(mapc 'add-to-load-path
  `(
    ,personal-core-directory
;;        ,(concat user-emacs-directory "core/libs/")
;;        ,(concat user-dropbox-directory "emacs/")
  ))
