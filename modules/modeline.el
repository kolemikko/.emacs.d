;; Modeline
(use-package doom-modeline
  :after eshell
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 5))

(provide 'modeline)
