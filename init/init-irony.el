(require 'irony)

(setq irony-additional-clang-options '("-std=c++11"))

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h" ".hpp" ".ipp"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".cpp" "c"))))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)
(add-hook 'c-mode-common-hook (lambda ()
    (define-key c-mode-base-map (kbd "<f2>") 'ff-get-other-file)))


(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun baymacs/setup-ac-irony ()
  (require 'auto-complete)
  (require 'ac-irony)
  (require 'ggtags)
  (ggtags-mode 1)
  (auto-complete-mode)

  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))

(add-hook 'irony-mode-hook 'baymacs/setup-ac-irony)
