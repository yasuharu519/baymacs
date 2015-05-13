;; // defconst -> Define symbol as a constant variable.
;; // (defconst SYMBOL INITVALUE [DOCSTRING])

;; (defconst spacemacs-version          "0.101.2" "Spacemacs version.")
;; (defconst spacemacs-emacs-min-version   "24.3" "Minimal version of Emacs.")

;; // defun → 関数の定義
;; // (defun NAME ARGLIST &optional DOCSTRING DECL &rest BODY)
;; (defun 関数名 (引数...)
;;  "省略可能な関数の説明文..."
;;   (interactive 引数に関する情報) ;    省略可能
;;  本体...)

;; // version<= → バージョン比較
;; // (version<= V1 V2)

;; (defun spacemacs/emacs-version-ok ()
;;   (version<= space-emacs-min-version emacs-version))

;; // when → if 文のような。ただしelseがない
;; // (when COND BODY)
;; // COND が nil でなければ BODY の実行。

;; // load-file → FILEのロード
;; // (load-file FILE)
;; // File の ロードを行う

;; // concat → argument を全て繋げて string を返す
;; // (concat &rest SEQUENCE)
;; // SEQUENCE は string や list や 文字のベクタ

;; // require → FEATURE が ロードされてない場合はFILENAMEからロードを行う
;; // (require FEATURE &optional FILENAME NOERROR)

;; // user-emacs-directory → 変数: ~/.emacs.d/
;; // load-path → 変数: サーチパス

;; // file-exists-p → FILEが存在するかどうか
;; // (file-exists-p FILENAME)

;; // make-directory → ディレクトリの作成
;; // (make-directory DIR &optional PARENTS)

;; // mapc → 引数でわたらせたリストに対して処理を繰り返す
;; // (mapc FUNCTION SEQUENCE)

;; // defgroup → グループの作成
;; // (defgroup SYMBOL MEMBERS DOC &rest ARGS)
