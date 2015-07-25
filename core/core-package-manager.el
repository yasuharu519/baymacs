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

    ;; パッケージ管理
    (el-get-bundle 'tarao/el-get-lock) ;; パッケージバージョンを固定
    (el-get-bundle 'init-loader) ;; 設定を順にロード

    ;; ===============================================================================
    ;; 基本パッケージ
    ;; ===============================================================================

    ;; 基本パッケージ /補完
    (el-get-bundle 'auto-complete) ;; 補完

    ;; 基本パッケージ /evil
    (el-get-bundle 'evil) ;; evil 本体
    (el-get-bundle 'evil-leader) ;; <leader> を使用できるように
    (el-get-bundle 'evil-jumper) ;; <C-i>, <C-o> でジャンプできるように
    (el-get-bundle 'evil-nerd-commenter) ;; 簡単にコメント化

    ;; 基本パッケージ /helm
    (el-get-bundle 'helm) ;; helm 本体
    (el-get-bundle 'helm-ag) ;; helm で ag 検索
    (el-get-bundle 'helm-gtags) ;; helm で gtag 検索

    ;; 基本パッケージ /git
    (el-get-bundle 'magit)
    (el-get-bundle 'git-modes) ;; gitattributes-mode, gitconfig-mode, gitignore-mode を提供
    ;; (el-get-bundle 'magit-gh-pulls) <- 現状 Enterpriseには対応してなさそう
    (el-get-bundle 'git-gutter) ;; git-gutter

    ;; 基本パッケージ /エディタ
    (el-get-bundle 'expand-region) ;; リージョンを広げるコマンドの提供
    (el-get-bundle 'volatile-highlights) ;; 変更箇所を表示
    (el-get-bundle 'anzu) ;; 検索時に現在の位置などを表示
    (el-get-bundle 'rainbow-delimiters) ;; 対応するカッコをみやすく
    (el-get-bundle 'rainbow-mode) ;;カラーコードを色で設定する
    (el-get-bundle 'highlight-symbol) ;; シンボルをハイライト

    ;; 基本パッケージ /その他
    (el-get-bundle 'guide-key) ;; コマンドキーのガイド
    (el-get-bundle 'projectile) ;; git プロジェクト内の移動など
    (el-get-bundle 'undo-tree) ;; undo履歴

    ;; visual
    (el-get-bundle 'monokai-theme) ;; monokaiテーマ
    (el-get-bundle 'powerline) ;; パワーラインを表示

    ;; ===============================================================================
    ;; プログラミング
    ;; ===============================================================================

    ;; プログラミング /基本
    ;; Macでのインストールの際は
    ;;  - brew install --HEAD ctags
    ;;  - brew install global --with-exuberant-ctags --with-pygments
    (el-get-bundle 'ggtags) ;; gnu global タグ
    (el-get-bundle 'indent-guide) ;; インデントを縦線で表示
    (el-get-bundle 'smartparens) ;; カッコの自動挿入
    (el-get-bundle 'hl-todo) ;; TODO などをハイライト
    (el-get-bundle 'yasnippet) ;; スニペット
    (el-get-bundle 'visual-regexp) ;; 正規表現

    ;; プログラミング /C++
    ;; irony サーバインストール方法
    ;;  - https://gist.github.com/soonhokong/7c2bf6e8b72dbc71c93b
    ;;  - brew install llvm36 --with-libcxx --with-clang --without-assertions --rtti
    ;;  - DLIBCLANG_INCLUDE_DIR=/usr/local/opt/llvm36/lib/llvm-3.6/include
    ;;  - DLIBCLANG_LIBRARY=/usr/local/opt/llvm36/lib/llvm-3.6/lib/libclang.dylib.
    (el-get-bundle 'irony) ;; C++ 補完エンジン
    (el-get-bundle 'ac-irony) ;; C++ の irony の補完

    ;; プログラミング /Scheme
    (el-get-bundle 'geiser) ;; Racket(scheme実装) 環境

    ;; プログラミング /web
    (el-get-bundle 'web-mode) ;; webモード

    ;; プログラミング /Javascript
    (el-get-bundle 'js2-mode)
    (el-get-bundle 'jade-mode)
    (el-get-bundle 'less-css-mode)

    ;; プログラミング /haskell
    (el-get-bundle 'haskell-mode)
    (el-get-bundle 'hi2)

    ;; ===============================================================================
    ;; その他
    ;; ===============================================================================

    (el-get-bundle 'open-junk-file) ;; 一時ファイルを作成
    (setq open-junk-file-format (expand-file-name (concat user-org-memo-directory "%Y/%Y-%m-%d.org")))

    (el-get-bundle 'esup) ;; Emacsの起動時間を測る

    ;; 起動時間を視覚的に理解する
    ;; http://qiita.com/yuttie/items/0f38870817c11b2166bd
    (el-get-bundle 'yuttie/initchart)


    ;; その他 /Libraries
    (el-get-bundle 'shackle)
    (el-get-bundle 'restclient)
    (el-get-bundle 'tkf/emacs-request) ;; リクエスト送信を簡単にするライブラリ

    ;; テスト
    (el-get-bundle 'helm-dash) ;; Dashリファレンスをemacsから
    (el-get-bundle 'dired-plus) ;; dired の改良版

    (add-personal-package-to-load-path 'emacs-aoj) ;; aoj連携 (開発中)
  )

(provide 'core-package-manager)
