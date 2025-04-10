;;; file-management.el -- All file manager etc. configs

(file-name-shadow-mode 1)

;; Dired
(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (setq insert-directory-program "ls" dired-use-ls-dired t
        dired-listing-switches "-agho --group-directories-first"
	dired-kill-when-opening-new-dired-buffer t
	delete-by-moving-to-trash t
	dired-omit-verbose nil
	dired-hide-details-hide-symlink-targets nil
	dired-dwim-target t
	)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "<left>") 'dired-up-directory
    (kbd "<right>") 'dired-find-file
    "p" 'dired-view-file
    "P" 'dired-display-file))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-collapse)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(when (is-mac)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq insert-directory-program "/opt/homebrew/Cellar/coreutils/9.7/libexec/gnubin/ls"))

(provide 'file-management)
;;; file-management.el ends here
