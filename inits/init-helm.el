;; helm

(with-eval-after-load 'helm
  (helm-mode 1)

  (define-key global-map (kbd "C-;") 'helm-buffers-list)
  (define-key helm-map (kbd "C-h") 'delete-backward-char) ;; minibuffer で C-h
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

  ;; C-x C-f でタブ補完
  ;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action) 
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

  (evil-leader/set-key
    "aa" 'helm-resume ;; aa で helm をレジューム
    )
  )

(require 'helm)
;; (require 'helm-utils)

