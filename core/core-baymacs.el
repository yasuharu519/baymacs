(menu-bar-mode 0) ;; メニューバーの設定
(tool-bar-mode 0) ;; ツールバーの設定
(scroll-bar-mode 0) ;; スクロールバーの設定

;; linum-modeのため
(global-linum-mode)
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
(run-with-idle-timer 1.0 nil #'linum-update-current))

;; 設定
(setq-default indent-tabs-mode nil) ;; インデントでタブを使わない

;; doc-view の時は linum-modeを辞める
(add-hook 'doc-view-mode-hook
	  (lambda ()
	    (linum-mode -1)
	    ))

(provide 'core-baymacs)
