;; keybindings

(defmacro baymacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "baymacs/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (evil-leader/set-key ,keys ',func-name))))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "ad"  'dired
  "ab"  'helm-mini
  "ff"  'helm-find-files
  "fr"  'helm-recentf
  ";"   'helm-M-x
  "au"  'undo-tree-visualize)

;; Make <escape> quit as much as possible
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)
(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit) 

;; helm 
;; (define-key helm-map (kbd "C-h") 'delete-backward-char)
;; (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; C++
(evil-define-key 'insert c++-mode-map (kbd "C-l") (lambda () (interactive)
						    (insert "->")))
;; Enable helm-gtags-mode
(baymacs||set-helm-key "hdb" describe-bindings)
(baymacs||set-helm-key "hdc" describe-char)
(baymacs||set-helm-key "hdf" describe-function)
(baymacs||set-helm-key "hdk" describe-key)
(baymacs||set-helm-key "hdm" describe-mode)
(baymacs||set-helm-key "hdp" describe-package)
(baymacs||set-helm-key "hdt" describe-theme)
(baymacs||set-helm-key "hdv" describe-variable)
(baymacs||set-helm-key "hL" helm-locate-library)

;; search functions -----------------------------------------------------------
(baymacs||set-helm-key "sww" helm-wikipedia-suggest)
(baymacs||set-helm-key "swg" helm-google-suggest)


