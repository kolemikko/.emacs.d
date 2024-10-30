;;; file-management.el -- All file manager etc. configs

;; Save history
(setq backup-directory-alist `(("." . "~/.emacs.d/save-hist")))

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/save-hist/\\1" t)))

(setq delete-old-versions t
      kept-new-versions 3
      kept-old-versions 3
      version-control t)

;; Dired
(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :config
  (setq insert-directory-program "ls" dired-use-ls-dired t
        dired-listing-switches "-agho --group-directories-first"
	delete-by-moving-to-trash t
	dired-omit-verbose nil
	dired-hide-details-hide-symlink-targets nil)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "<left>") 'dired-up-directory
    (kbd "<right>") 'dired-find-file
    "p" 'dired-view-file
    "P" 'dired-display-file))

(use-package dired-collapse)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(when (is-mac)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq insert-directory-program "/opt/homebrew/Cellar/coreutils/9.5/libexec/gnubin/ls"))

;; (general-define-key
;;  :prefix "SPC"
;;  :states 'normal
;;  :keymaps 'dired-mode-map
;;  "dd" '(dired-display-file :wk "display file")
;;  "dv" '(dired-view-file :wk "view file"))

;; Treemacs
(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-display-in-side-window t
	  treemacs-file-follow-delay 0.2
	  treemacs-follow-after-init t
	  treemacs-expand-after-init t
	  treemacs-indentation 2
	  treemacs-indentation-string " "
	  treemacs-no-delete-other-windows t
	  treemacs-project-follow-cleanup nil
	  treemacs-position 'left
	  treemacs-recenter-distance 0.1
	  treemacs-recenter-after-project-jump 'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-hidden-files t
	  treemacs-sorting 'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-width 30
	  treemacs-width-is-initially-locked nil)

    (treemacs-resize-icons 18)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'projectile-mode-map
 "tt" '(treemacs :wk "treemacs"))

(provide 'file-management)
;;; file-management.el ends here
