;;This init.el file was generated from config.org file 

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

(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil :font "Source Code Pro" :height 130)

(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :after eshell
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(defun revert-buffer-force ()
  (interactive) (revert-buffer t t))

(setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defun my/kill-all-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (kill-buffer buffer))
  (delete-other-windows))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ws-butler
:hook ((text-mode . ws-butler-mode)
        (prog-mode . ws-butler-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package diminish)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
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

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(when (is-mac)
  (setq insert-directory-program "/opt/homebrew/Cellar/coreutils/9.0/libexec/gnubin/ls"))

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
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-agenda-files '("~/Org"))
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-modules
    '(org-crypt)))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

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

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 90
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

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

(defun org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(use-package org-present
  :hook (org-present-mode-quit . org-present-quit-hook))

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

(use-package lsp-mode
    :straight t
    :commands lsp
    :init (setq lsp-keymap-prefix "C-c l")
    :config
    (lsp-enable-which-key-integration t)
    (setq lsp-ui-doc-enable nil))

  (use-package lsp-treemacs
    :after lsp)

(setenv "PATH" (concat (getenv "PATH") "/Library/Frameworks/Mono.framework/Versions/Current/Commands"))
(setq exec-path (append exec-path '("/Library/Frameworks/Mono.framework/Versions/Current/Commands")))

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
    treemacs-width                           38)

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
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind ((:map company-active-map
              ("<tab>" . company-complete-selection))
         (:map lsp-mode-map
               ("<tab>" . company-indent-or-complete-common)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.5))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package rustic
  :config
  (setq rustic-format-on-save t))

(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my/csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode))

(use-package csharp-mode
  :init
  (add-hook 'csharp-mode-hook 'my/csharp-mode-setup t))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

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

(defun my/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . my/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
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
    :states 'normal
    :keymaps 'override
    :prefix "SPC")

  (custom-keys
    "k"  '(:ignore k :which-key "kill")
    "kk" '(kill-buffer :which-key "selected buffer")
    "kK" '(my/kill-all-buffers :which-key "all buffers")
    "kq" '(kill-buffer-and-window :which-key "current buffer and window")

    "b"  '(:ignore b :which-key "buffer")
    "br" '(revert-buffer-force :which-key "revert buffer")

    "e"  '(:ignore e :which-key "evaluate")
    "eb" '(eval-buffer :which-key "evaluate current buffer")
    "ee" '(eval-expression :which-key "evaluate expression")
    "er" '(eval-region :which-key "evaluate region")

    "f"  '(:ignore f :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fo" '(find-file-other-window :which-key "open file in new window")
    "fr" '(counsel-recentf :which-key "find from recent files")

    "d"  '(dired :which-key "dired")
    "g"  '(magit-status :which-key "magit-status")

    "o"  '(:ignore o :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "os" '(org-schedule :which-key "schedule")
    "od" '(org-deadline :which-key "deadline")
    "ot" '(org-time-stamp :which-key "timestamp")
    "op" '(org-present :which-key "presentation mode")

    "oc" '(org-roam-capture :which-key "capture")
    "og" '(org-roam-graph :which-key "graph")
    "of" '(org-roam-node-find :which-key "find node")
    "oi" '(org-roam-node-insert :which-key "insert")

    "p"  '(
           projectile-command-map :which-key "projectile")
    "pg" '(counsel-projectile-grep :which-key "counsel-projectile-grep")

    "s"  '(eshell :which-key "eshell")

    "t"  '(:ignore t :which-key "toggle")
    "tf" '(flycheck-list-errors :which-key "flycheck error list")
    "tt" '(treemacs :which-key "treemacs")

    "."  '(switch-to-buffer :which-key "switch to buffer")
    "/"  '(switch-to-buffer-other-window :which-key "switch to buffer with other window")


    "w"  '(:ignore w :which-key "window")
    "TAB"'(other-window :which-key "switch window")
    "wd" '(delete-window :which-key "delete window")
    "wo" '(delete-other-windows :which-key "delete other windows")
    "wb" '(split-window-below :which-key "split window below")
    "wr" '(split-window-right :which-key "split window right")
    "wl" '(split-window-left :which-key "split window left")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-treemacs lsp-python-ms pyls dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt evil-nerd-commenter company flycheck ccls lsp-ui lsp-mode visual-fill-column org-bullets evil-magit magit counsel-projectile projectile general evil-collection evil which-key use-package rainbow-delimiters ivy-rich helpful doom-themes doom-modeline counsel command-log-mode)))
(custom-set-faces)
