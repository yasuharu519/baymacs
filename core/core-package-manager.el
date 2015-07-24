(setq personal-packages
      '(emacs-aoj
	))

(defun baymacs/core-package-manager-init ()
    (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (setq el-get-user-package-directory (concat user-emacs-directory "init"))

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
    (el-get-bundle 'git-modes)
    (el-get-bundle 'undo-tree) ;; undo tree
    (el-get-bundle 'expand-region)
    (el-get-bundle 'volatile-highlights)
    (el-get-bundle 'anzu)
    (el-get-bundle 'rainbow-delimiters) ;; 対応するカッコをみやすく
    (el-get-bundle 'ac-irony)
    (el-get-bundle 'evil-jumper)
    (el-get-bundle 'geiser) ;; Racket(scheme実装) 環境

    ;; font look
    (el-get-bundle 'rainbow-mode) ;;カラーコードを色で設定する

    ;; git
    (el-get-bundle 'magit)
    ;; (el-get-bundle 'magit-gh-pulls) <- 現状 Enterpriseには対応してなさそう

    ;; web/javascript
    (el-get-bundle 'web-mode)

    ;; open-junk-file
    (el-get-bundle 'open-junk-file)
    (setq open-junk-file-format (expand-file-name (concat user-org-memo-directory "%Y/%Y-%m-%d.org")))

    ;; programming
    ;; Macでのインストールの際は
    ;;  - brew install --HEAD ctags
    ;;  - brew install global --with-exuberant-ctags --with-pygments
    (el-get-bundle 'ggtags)
    (el-get-bundle 'highlight-symbol)
    (el-get-bundle 'indent-guide)
    (el-get-bundle 'smartparens)
    (el-get-bundle 'hl-todo) ;; TODO などをハイライト
    (el-get-bundle 'evil-nerd-commenter) ;; 簡単にコメント化

    ;; スニペット
    (el-get-bundle 'yasnippet)

    ;; C++
    ;; irony サーバインストール方法
    ;;  - https://gist.github.com/soonhokong/7c2bf6e8b72dbc71c93b
    ;;  - brew install llvm36 --with-libcxx --with-clang --without-assertions --rtti
    ;;  - DLIBCLANG_INCLUDE_DIR=/usr/local/opt/llvm36/lib/llvm-3.6/include
    ;;  - DLIBCLANG_LIBRARY=/usr/local/opt/llvm36/lib/llvm-3.6/lib/libclang.dylib.
    (el-get-bundle 'irony)
    (el-get-bundle 'helm-gtags)

    ;; init-loader
    (el-get-bundle 'init-loader)

    ;; ESUP - Emacs start up profiler
    ;; Emacsの起動時間を測る
    (el-get-bundle 'esup)

    ;; 起動時間を視覚的に理解する
    ;; http://qiita.com/yuttie/items/0f38870817c11b2166bd
    (el-get-bundle 'yuttie/initchart)

    ;; javascript
    (el-get-bundle 'js2-mode)
    (el-get-bundle 'jade-mode)
    (el-get-bundle 'less-css-mode)

    ;; visual-regexp
    (el-get-bundle 'visual-regexp)

    ;; haskell
    (el-get-bundle 'haskell-mode)
    (el-get-bundle 'hi2)

    ;; Libraries
    (el-get-bundle 'shackle)
    (el-get-bundle 'restclient)
    (el-get-bundle 'tkf/emacs-request) ;; リクエスト送信を簡単にするライブラリ

    ;; misc
    (el-get-bundle 'helm-dash) ;; Dashリファレンスをemacsから
    (el-get-bundle 'dired-plus) ;; dired の改良版
    (el-get-bundle 'git-gutter) ;; git-gutter

    (add-personal-package-to-load-path 'emacs-aoj) ;; ドキュメント検索ツール Dashとの連携
  )

(provide 'core-package-manager)
