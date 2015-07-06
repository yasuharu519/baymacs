(defun baymacs/load-theme (theme)
  "Load THEME."
  (unless (memq theme (custom-available-themes))
    (cond
     ;; solarized theme, official spacemacs theme
     ((or (eq 'solarized-light theme)
          (eq 'solarized-dark theme))
      (add-to-list 'load-path (concat spacemacs-directory
                                      "extensions/solarized-theme/"))
      (require 'solarized)
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))
     ;; themes with explicitly declared package names
     ((assq theme spacemacs-theme-name-to-package)
      (let* ((pkg (cdr (assq theme spacemacs-theme-name-to-package)))
             (pkg-dir (spacemacs/load-or-install-package pkg)))
        (when (or (eq 'moe-light theme)
                  (eq 'moe-dark theme))
          (load-file (concat pkg-dir "moe-light-theme.el"))
          (load-file (concat pkg-dir "moe-dark-theme.el")))
        (add-to-list 'custom-theme-load-path pkg-dir)))
     (t
      ;; other themes
      ;; we assume that the package name is suffixed with `-theme'
      ;; if not we will handle the special themes as we get issues in the tracker.
      (let ((pkg (format "%s-theme" (symbol-name theme))))
        (spacemacs/load-or-install-package (intern pkg))))))
  (load-theme theme t))
