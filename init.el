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

(defconst personal-version          "0.0.1" "Personal version.")
(defconst personal-emacs-min-version   "24.3" "Minimal version of Emacs.")

(defun personal/emacs-version-ok ()
  (version<= personal-emacs-min-version emacs-version))

(when (personal/emacs-version-ok)
  ;; ロードパスのロード
  (load-file (concat user-emacs-directory "core/core-load-path.el"))
  )


(defgroup personal nil
    "personal customizations."
    :group 'starter-kit
    :prefix 'personal-)


(define-derived-mode personal-mode special-mode "Personal"
  "Personal major mode for startup screen."
  :group 'personal
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  ;; motion state since this is a special mode
  (add-to-list 'evil-motion-state-modes 'personal-mode))

(defun personal/location ()
  "Return the absolute path to the spacemacs dotfile."
  (concat user-home-directory ".spacemacs"))

(defun personal/load-file ()
  "Load ~/.spacemacs if it exists."
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs) (load dotspacemacs))))

(defun personal/init ()
  "`pwersonal-mode' のバッファを作成し、初期化処理を行う"
  ;; explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8) ;; 優先する文字コード
  )


