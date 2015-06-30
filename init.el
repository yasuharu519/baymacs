(defconst baymacs-version          "0.0.1" "Personal version.")
(defconst baymacs-emacs-min-version   "24.3" "Minimal version of Emacs.")

(defun baymacs/emacs-version-ok ()
  (version<= personal-emacs-min-version emacs-version))

(when (baymacs/emacs-version-ok)
  ;; ロードパスのロード
  (load-file (concat user-emacs-directory "core/core-load-path.el"))
  )

