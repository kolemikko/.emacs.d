;;; misc.el -- Random stuff

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo))

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package diminish)

(use-package shackle)
(setq shackle-rules
      '((compilation-mode :noselect t))
      shackle-default-rule
      '(:select t))

(use-package bufler
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "TAB")     'bufler-list-buffer-peek
    "D"           'bufler-list-buffer-kill))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (recentf-mode)
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recentf))

(use-package magit
  :defer t)

(use-package simple-httpd
  :defer t)

(use-package websocket
  :defer t
  :after org-roam)

(provide 'misc)
;;; misc.el
