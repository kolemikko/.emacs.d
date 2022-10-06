(server-start)
(setq inhibit-startup-message t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on Non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package which-os
  :straight (:host github :repo "kolemikko/which-os" :branch "master"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq visible-bell 1)
(setq use-dialog-box nil)

(set-default-coding-systems 'utf-8)

(when (is-mac)
  (set-face-attribute 'default nil :font "Fira Code" :height 130))

(when (is-linux)
  (set-face-attribute 'default nil :font "Source Code Pro" :height 130))

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :after eshell
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 5))

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-items '((recents . 6)
                          (projects . 5)
                          (agenda . 5)))
  (setq
   dashboard-banner-logo-title "Emacs FTW!"
   dashboard-footer-messages '("")
   dashboard-startup-banner 'logo
   dashboard-page-separator "\n\n\n"
   dashboard-week-agenda t
   dashboard-center-content t
   dashboard-set-heading-icons nil
   dashboard-set-file-icons nil
   dashboard-week-agenda t
   dashboard-center-content t
   dashboard-set-init-info t
   dashboard-set-navigator t
   dashboard-items-default-length 30)
  (dashboard-setup-startup-hook))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq completion-ignored-extensions '(".meta"))

(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package ws-butler
:hook ((text-mode . ws-butler-mode)
        (prog-mode . ws-butler-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package diminish)

(defun my/vertigo-backward-kill (arg)
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

  (use-package vertico
    :bind
    (:map minibuffer-local-map ("<left>" . my/vertigo-backward-kill))
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode))

(use-package savehist
  :init
  (setq history-length 20)
  (savehist-mode 1))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package consult
  :demand t
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(setq evil-want-keybinding nil)

(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(dolist (mode '(flycheck-error-list-mode
                term-mode))
  (evil-set-initial-state 'help-mode 'emacs))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(defun my/kill-buffer-other-window ()
    (interactive)
    (other-window 1)
    (kill-buffer (current-buffer))
    (other-window 1))

(defun my/kill-all-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (kill-buffer buffer))
  (delete-other-windows))

(defun my/switch-recent-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package bufler
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "TAB")     'bufler-list-buffer-peek
    "D"           'bufler-list-buffer-kill))

(use-package default-text-scale
  :defer 1
  :bind
  (:map default-text-scale-mode-map
        ("C-+" . default-text-scale-increase)
        ("C--" . default-text-scale-decrease))
  :config
  (default-text-scale-mode))

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
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

(use-package dired-single
  :defer t)

(use-package dired-collapse
  :defer t)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(when (is-mac)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq insert-directory-program "/opt/homebrew/Cellar/coreutils/9.1/libexec/gnubin/ls"))

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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (recentf-mode)
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recentf))

(use-package magit)

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . my/org-mode-setup)
  :diminish org-indent-mode
  :config
  (setq org-agenda-files '("~/Org"))
  (setq org-export-coding-system 'utf-8)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-startup-folded t
        org-cycle-separator-lines 2)

  (setq org-modules
    '(org-crypt)))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("●" "○" "●" "○" "●" "○" "●")))

(require 'org-indent)

(set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'medium :height (cdr face)))

(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit 'fixed-pitch)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package org-roam
  :ensure t
  :straight nil
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Org")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%d%m%Y>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("i" "idea entry" entry
      "\n* ${title}%?"
      :if-new (file+head "Ideas.org" "#+title: Ideas\n")
      :file-name "Ideas.org"
      :unnarrowed t)
     ("j" "journal entry" entry
      "* %<%A, %B %d, %Y>\n${title}%?"
      :if-new (file+head "Journal.org" "#+title: Journal\n")
      :file-name "Journal.org"
      :unnarrowed t)
     ("w" "Work journal entry" entry
      "* %<%A, %B %d, %Y>\n${title}%?"
      :if-new (file+head "WorkJournal.org" "#+title: Work Journal\n")
      :file-name "WorkJournal.org"
      :unnarrowed t)
     ("t" "task" entry
      "* TODO ${title}%?"
      :if-new (file+head "Todo.org" "#+title: TODOlist\n")
      :file-name "Todo.org"
      :unnarrowed t
      :immediate-finish))))

(use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun my/org-present-prepare-slide ()
  (org-overview)
  (org-show-all)
  (org-show-children))

(defun my/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.8) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (org-present-read-only)
  (my/org-present-prepare-slide))

(defun my/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-present-read-write)
  (org-remove-inline-images))

(defun my/org-present-prev ()
  (interactive)
  (org-present-prev)
  (my/org-present-prepare-slide))

(defun my/org-present-next ()
  (interactive)
  (org-present-next)
  (my/org-present-prepare-slide))

(defun my/org-present-beginning()
  (interactive)
  (org-present-beginning)
  (my/org-present-prepare-slide))

(defun my/org-present-end ()
  (interactive)
  (org-present-end)
  (my/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
              ("C-<right>" . my/org-present-next)
              ("C-<left>" . my/org-present-prev)
              ("C-<" . my/org-present-beginning)
              ("C->" . my/org-present-end)
              ("C-q" . org-present-quit))
  :hook ((org-present-mode . my/org-present-hook)
         (org-present-mode-quit . my/org-present-quit-hook)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("ru" . "src rust"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(defun tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'tangle-config)))

(use-package auctex
  :defer t
  :init
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-auto-local (concat user-emacs-directory "auctex/auto/")
        TeX-style-local (concat user-emacs-directory "auctex/style/")))

(use-package company-auctex
  :after auctex
  :init
  (add-to-list 'company-backends 'company-auctex)
  (company-auctex-init))

(setq org-latex-listings t)
(setq org-latex-compiler "xelatex")

(setq tramp-default-method "ssh")

(use-package simple-httpd)

(use-package websocket
  :after org-roam)

(use-package impatient-mode
  :straight t)

(add-hook 'markdown-mode-hook 'impatient-mode)

(defun my/markdown-to-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(use-package treemacs
  :defer t
  :config
  (progn
    treemacs-display-in-side-window          t
    treemacs-file-follow-delay               0.2
    treemacs-follow-after-init               t
    treemacs-expand-after-init               t
    treemacs-indentation                     2
    treemacs-indentation-string              " "
    treemacs-no-delete-other-windows         t
    treemacs-project-follow-cleanup          nil
    treemacs-position                        'left
    treemacs-recenter-distance               0.1
    treemacs-recenter-after-project-jump     'always
    treemacs-recenter-after-project-expand   'on-distance
    treemacs-show-hidden-files               t
    treemacs-sorting                         'alphabetic-asc
    treemacs-select-when-already-in-treemacs 'move-back
    treemacs-width                           38
    treemacs-width-is-initially-locked       nil)

  (treemacs-resize-icons 18)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))

(use-package company
  :bind ((:map company-active-map
              ("<tab>" . company-complete-selection)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))

(use-package flycheck
  :defer t)

(defun my/next-error()
  (interactive)
  (unless (flycheck-next-error)
    (flycheck-first-error)))
  ;; (let (error-count (length flycheck-current-errors)))
  ;; (when (> error-count 0)
    ;; (let* ((req-n (if (numberp n) n 1))
    ;;        (curr-pos (if (> req-n 0) (- error-count 1) 0))
    ;;        (next-pos (mod (+ curr-pos req-n) error-count)))
    ;;   (apply flycheck-next-error (list (+ 1 next-pos) 'reset))
    ;;   (message "No more Flycheck errors"))

(use-package eglot)

(use-package lsp-ui
  :ensure
  :init
  (add-hook 'lsp-ui-mode-hook 'lsp-ui-doc-mode)
  :commands lsp-ui-mode
  :config
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

(use-package rustic
  :init
  (add-hook 'rustic-mode-hook 'lsp-ui-mode)
  (add-hook 'rustic-mode-hook 'my/rustic-mode-setup)
  :config
  (setq rustic-format-on-save t)
  :custom
(rustic-rustfmt-config-alist '((edition . "2018"))))

(defun my/rustic-mode-setup()
  (setq lsp-enable-snippet nil)
  (setq lsp-rust-analyzer-completion-add-call-argument-snippets nil))

(eval-after-load
    'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my/csharp-mode-setup ()
  (eglot-ensure)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(use-package csharp-mode
  :init
  (add-hook 'csharp-mode-hook 'my/csharp-mode-setup t))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

(use-package yaml-mode)

(use-package irony-eldoc
  :defer t)
(use-package irony
  :defer t)
(use-package arduino-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.ino$" . arduino-mode))

(use-package platformio-mode
  :defer t)

(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)
                           (platformio-conditionally-enable)))

(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)

            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)

            (irony-cdb-autosetup-compile-options)))

(use-package term
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(defun my/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . my/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t))
  (eshell-git-prompt-use-theme 'powerline))

(defun my/set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                          ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(my/set-exec-path-from-shell-PATH)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(when (is-mac)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer custom-keys
    :states '(normal visual motion)
    :keymaps '(override dashboard)
    :prefix "SPC")

  (custom-keys
    "b"  '(:ignore b :which-key "buffer")
    "br" '(revert-buffer-quick :which-key "revert buffer")
    "bk" '(kill-buffer-and-window :which-key "kill buffer and window")
    "bo" '(my/kill-buffer-other-window :which-key "kill buffer in other window")

    "."  '(bufler :which-key "list buffers")
    "/"  '(my/switch-recent-buffer :which-key "switch to recent buffer")

    "c"  '(:ignore c :which-key "consult")
    "cr" '(consult-ripgrep :which-key "ripgrep")
    "co" '(consult-outline :which-key "outline")
    "ch" '(consult-history :which-key "history")
    "ce" '(consult-file-externally :which-key "open file externally")

    "e"  '(:ignore e :which-key "eval")
    "eb" '(eval-buffer :which-key "buffer")
    "ee" '(eval-expression :which-key "expression")
    "er" '(eval-region :which-key "region")
    "es" '(org-babel-execute-src-block :which-key "source block")

    "f"  '(:ignore f :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fo" '(find-file-other-window :which-key "open file in new window")
    "fr" '(recentf-open-files :which-key "find from recent files")

    "d"  '(dired :which-key "dired")
    "g"  '(magit-status :which-key "magit-status")

    "o"  '(:ignore o :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-roam-capture :which-key "capture")
    "of" '(org-roam-node-find :which-key "find node")
    "oi" '(org-roam-node-insert :which-key "insert")

    "p"  '(projectile-command-map :which-key "projectile")
    "pg" '(projectile-ripgrep :which-key "projectile-ripgrep")

    "s"  '(:ignore s :which-key "shell/terminal")
    "st" '(term :which-key "term")
    "ss" '(eshell :which-key "eshell")

    "t"  '(:ignore t :which-key "toggle")
    "tT" '(toggle-truncate-lines :which-key "truncate lines")
    "tv" '(visual-line-mode :which-key "visual line mode")
    "tn" '(display-line-numbers-mode :which-key "display line numbers")
    "tR" '(read-only-mode :which-key "read only mode")

    "q"  '(:ignore q :which-key "quit")
    "qq" '(evil-quit-all :which-key "quit all")
    "qk" '(my/kill-all-buffers :which-key "kill all")

    "w"  '(:ignore w :which-key "window")
    "TAB"'(other-window :which-key "switch window")
    "wd" '(delete-window :which-key "delete window")
    "wo" '(delete-other-windows :which-key "delete other windows")
    "wb" '(split-window-below :which-key "split window below")
    "wr" '(split-window-right :which-key "split window right")))

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'omnisharp-mode-map
 "l"  '(:ignore l :which-key "lsp")
 "lu" '(omnisharp-find-usages :which-key "find usages")
 "lf" '(omnisharp-fix-code-issues-at-point :which-key "fix issue at point")
 "lF" '(omnisharp-fix-usings :which-key "fix usings")
 "lr" '(omnisharp-rename :which-key "rename")
 "lR" '(omnisharp-reload-solution :which-key "reload solution")
 "lS" '(omnisharp-start-omnisharp-server :which-key "start server")
 "le" '(omnisharp-code-format-entire-file :which-key "format file")
 "la" '(omnisharp-run-code-action-refactoring :which-key "action refactoring")
 ";"  '(my/next-error :which-key "next error")
 "tt" '(treemacs :which-key "treemacs"))

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'rustic-mode-map
 "l"  '(:ignore l :which-key "lsp")
 "lf" '(rustic-format-buffer :which-key "format buffer")
 "lr" '(lsp-rename :which-key "rename symbol")
 "ld" '(lsp-find-definition :which-key "find definition")

 "lc"  '(:ignore lc :which-key "cargo")
 "lcb" '(rustic-cargo-build :which-key "build")
 "lcc" '(rustic-cargo-clippy :which-key "clippy")
 "lcf" '(rustic-cargo-clippy-fix :which-key "clippy fix")
 "lcC" '(rustic-cargo-clean :which-key "clean")
 "lco" '(rustic-cargo-outdated :which-key "outdated")
 "lcu" '(rustic-cargo-update :which-key "update")
 "lcr" '(rustic-cargo-run :which-key "run")
 "lct" '(lsp-rust-analyzer-open-cargo-toml :which-key "open cargo toml file")

 ";"  '(my/next-error :which-key "next error")
 "tt" '(treemacs :which-key "treemacs"))

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'org-mode-map
 "o"  '(:ignore o :which-key "org")
 "oe" '(org-export-dispatch :which-key "export dispatch")
 "os" '(org-schedule :which-key "schedule")
 "od" '(org-deadline :which-key "deadline")
 "ot"  '(:ignore ot :which-key "table")
 "ots" '(org-table-sort-lines :which-key "sort lines")
 "ote" '(org-table-export :which-key "export")
 "otc" '(org-table-create :which-key "create")
 "oti"  '(:ignore oti :which-key "insert")
 "otic" '(org-table-insert-column :which-key "column")
 "otir" '(org-table-insert-row :which-key "row")
 "otih" '(org-table-insert-hline :which-key "horizontal line")
 "ob" '(:ignore ob :which-key "babel")
 "obt"'(org-babel-tangle :which-key "tangle")
 "op" '(org-present :which-key "presentation mode"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt evil-nerd-commenter company flycheck ccls visual-fill-column org-bullets evil-magit magit projectile general evil-collection evil which-key use-package rainbow-delimiters helpful doom-themes doom-modeline command-log-mode)))
(custom-set-faces)
