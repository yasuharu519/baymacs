;; guide-key

(with-eval-after-load 'guide-key
  (guide-key-mode 1)

  (setq guide-key/guide-key-sequence '("SPC"))
  (setq guide-key/recursive-key-sequence-flag t)
  )

(require 'guide-key)
