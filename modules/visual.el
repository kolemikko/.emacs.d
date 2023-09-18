;;; visual.el -- Any configuration related to visuals

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq my/default-fixed-pitch-font-size '142)
(setq my/default-variable-pitch-font-size '130)

(when (is-mac)
  (set-face-attribute 'default nil :font "Source Code Pro" :height my/default-fixed-pitch-font-size))

(when (is-linux)
  (set-face-attribute 'default nil :font "Source Code Pro" :height my/default-fixed-pitch-font-size))

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-pitch-font-size :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :after eshell
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 5))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-items '((recents . 6)
                          (projects . 6)
                          (agenda . 9)))
  (setq
   dashboard-banner-logo-title "Emacs FTW!"
   dashboard-footer-messages '("")
   dashboard-startup-banner 'logo
   dashboard-page-separator "\n\n\n"

   dashboard-item-names '(("Agenda for the coming week:" . "Agenda:"))

   dashboard-week-agenda t
   dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
   dashboard-match-agenda-entry "TODO=\"TODO\"|TODO=\"INPROGRESS\""
   dashboard-agenda-sort-strategy '(todo-state-down)

   dashboard-center-content t
   dashboard-set-heading-icons nil
   dashboard-set-file-icons nil
   dashboard-center-content t
   dashboard-set-init-info t
   dashboard-items-default-length 30)
  (dashboard-setup-startup-hook))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package default-text-scale
  :bind
  (:map default-text-scale-mode-map
        ("C-+" . default-text-scale-increase)
        ("C--" . default-text-scale-decrease))
  :config
  (default-text-scale-mode))

(use-package eterm-256color
  :defer t
  :hook (term-mode . eterm-256color-mode))

(provide 'visual)
;;; visual.el ends here
