;;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(provide 'icons)
