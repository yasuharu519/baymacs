
(defun baymacs/core-package-manager-init ()
    (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (setq el-get-user-package-directory (concat user-emacs-directory "init"))
  )

(defun baymacs/core-package-manager-install-packages()
    (el-get-bundle 'tarao/el-get-lock)
    (el-get-bundle 'auto-complete)
    (el-get-bundle 'evil-leader)
    (el-get-bundle 'evil)
    (el-get-bundle 'monokai-theme)
    (el-get-bundle 'helm-ag)
    (el-get-bundle 'helm)
    (el-get-bundle 'projectile)
    (el-get-bundle 'powerline)
    (el-get-bundle 'sx)
    (el-get-bundle 'guide-key)
    (el-get-bundle 'magit)
    (el-get-bundle 'git-modes)
    (el-get-bundle 'undo-tree)
    (el-get-bundle 'expand-region)
    (el-get-bundle 'volatile-highlights)
    (el-get-bundle 'anzu)
    (el-get-bundle 'rainbow-delimiters)
    (el-get-bundle 'ac-irony)
    (el-get-bundle 'evil-jumper)

    ;; programming
    ;; Macでのインストールの際は
    ;;  - brew install --HEAD ctags
    ;;  - brew install global --with-exuberant-ctags --with-pygments
    (el-get-bundle 'ggtags)

    ;; C++
    ;; irony サーバインストール方法
    ;;  - https://gist.github.com/soonhokong/7c2bf6e8b72dbc71c93b
    ;;  - brew install llvm36 --with-libcxx --with-clang --without-assertions --rtti
    ;;  - DLIBCLANG_INCLUDE_DIR=/usr/local/opt/llvm36/lib/llvm-3.6/include
    ;;  - DLIBCLANG_LIBRARY=/usr/local/opt/llvm36/lib/llvm-3.6/lib/libclang.dylib.
    (el-get-bundle 'irony)
    (el-get-bundle 'helm-gtags)
    
    (load-theme 'monokai t)

    (el-get-lock)
  )

(provide 'core-package-manager)
