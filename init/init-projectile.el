;; projectile

(with-eval-after-load 'projectile
  (projectile-global-mode)

  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-enable-caching t)
  (evil-leader/set-key
    "pa" 'helm-projectile
    "pp" 'helm-projectile-switch-project
    "pf" 'helm-projectile-find-file
    "pr" 'helm-projectile-recentf
    "pg" 'helm-projectile-ag)
  )

(require 'projectile)
