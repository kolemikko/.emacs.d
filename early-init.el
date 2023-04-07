(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(setq inhibit-startup-message t)

(server-start)
(setq native-comp-async-report-warnings-errors 'silent)

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

(set-default-coding-systems 'utf-8)
