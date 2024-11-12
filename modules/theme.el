;;; Theme
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load-theme 'tango)

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar my/default-fixed-pitch-font-size '142)
(defvar my/default-variable-pitch-font-size '130)

(when (is-mac)
  (set-face-attribute 'default nil :font "Source Code Pro" :height my/default-fixed-pitch-font-size))

(when (is-linux)
  (set-face-attribute 'default nil :font "Source Code Pro" :height my/default-fixed-pitch-font-size))

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-pitch-font-size :weight 'regular)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package default-text-scale
  :bind
  (:map default-text-scale-mode-map
        ("C-+" . default-text-scale-increase)
        ("C--" . default-text-scale-decrease))
  :config
  (default-text-scale-mode))

;; (use-package eterm-256color
;;   :defer t
;;   :hook (term-mode . eterm-256color-mode))

(provide 'theme)
