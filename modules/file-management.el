;;; file-management.el -- All file manager etc. configs

(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :config
  (setq insert-directory-program "ls" dired-use-ls-dired t
        dired-listing-switches "-al --group-directories-first"
        dired-kill-when-opening-new-dired-buffer t
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "<left>") 'dired-single-up-directory
    (kbd "<right>") 'dired-single-buffer
    "p" 'dired-view-file
    "P" 'dired-display-file
    "=" 'my/diff-marked-files))

(use-package dired-single)

(use-package dired-collapse)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(when (is-mac)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq insert-directory-program "/opt/homebrew/Cellar/coreutils/9.5/libexec/gnubin/ls"))

(defun my/diff-marked-files ()
  (interactive)
  (let ((marked-files  ())
        (here   ()))
    (dolist (buf  (mapcar #'cdr dired-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq here  (dired-get-marked-files nil nil nil t)))
        (when (or (null (cdr here))  (eq t (car here)))
          (setq here  (cdr here)))
        (setq marked-files  (nconc here marked-files))))
    (setq marked-files  (delete-dups marked-files))
    (when (= (length marked-files) 1)
      (dired-diff (nth 0 marked-files)))))

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

(provide 'file-management)
;;; file-management.el ends here
