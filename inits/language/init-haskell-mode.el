;; haskell mode

;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

;; Dependencies
;; $ cabal update
;; $ cabal install happy
;; $ cabal install hi2
;; $ cabal install hindent
;; $ cabal install hasktags
;; $ cabal install present ← うまくうごかなかった
;; $ cabal install ghc-mod
;; -- gitからのインストール
;;   git clone git@github.com:kazu-yamamoto/ghc-mod.git
;;   cd ghc-mod
;;   cabal install -j
;; $ cabal install hlint
;; $ cabal install structured-haskell-mode

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-tags-on-save t)
 '(haskell-process-type 'cabal-repl)
 )

(with-eval-after-load 'haskell-mode
  (evil-define-key 'normal haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
  (evil-define-key 'insert haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
  (evil-define-key 'insert haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)

  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (evil-define-key 'normal haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (evil-define-key 'insert haskell-mode-map (kbd "C-k") 'shm/newline-indent)
  (evil-define-key 'normal haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
  (evil-define-key 'insert haskell-mode-map (kbd "C-L") 'shm/forward-node)
  (evil-define-key 'insert haskell-mode-map (kbd "C-H") 'shm/backward-node)
)

(with-eval-after-load 'haskell-cabal
  (evil-define-key 'normal haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (evil-define-key 'normal haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (evil-define-key 'normal haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (evil-define-key 'normal haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (evil-define-key 'normal haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
  )

;; add cabal package for PATH.
(let ((my-cabal-path (expand-file-name "~/Library/Haskell/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;; add ghc-mod
(add-to-list 'load-path "~/Library/Haskell/share/ghc-7.10.1-x86_64/ghc-mod-0")

(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal "haskell-cabal" nil t)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)

