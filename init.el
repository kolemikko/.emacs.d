
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; default font
(set-face-attribute 'default nil :font "Source Code Pro" :height 100)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

;; packages
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

(use-package command-log-mode)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

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


(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-modeline
  :after eshell
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish w:hich-key-mode
  :config (setq which-key-idle-delay 0.2))

(use-package evil
  :init
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
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (define-key dired-mode-map [remap dired-find-file] 'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory] 'dired-single-up-directory))

(use-package dired-single
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

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

(defun org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-directory "~/Dropbox/Org")
  (setq org-default-notes-file (concat org-directory "todo.org"))
  (setq org-agenda-files '("~/Dropbox/Org/todo.org" "~/Dropbox/Org/notes.org"))

  (setq org-deadline-warning-days 7)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "INPROGRESS(i)" "DONE(d!)")))

  (setq org-tag-alist
    '((:startgroup)
       ("home" . ?H)
       ("work" . ?W)
       ("meeting" . ?m)
       ("planning" . ?p)
       ("idea" . ?i)
       (:endgroup)))

  (setq org-capture-templates
    '(("t" "Tasks")
      ("tt" "Task" entry (file+olp "~/Dropbox/Org/todo.org") 
        "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("tn" "Note" entry (file+olp+datetree "~/Dropbox/Org/notes.org")
        "\n* %<%H:%M> - :notes:\n\n%?\n\n"
           :empty-lines 1)

      ("tm" "Meeting" entry (file+olp "~/Dropbox/Org/todo.org")
        "* %<%H:%M> - %a :meetings:\n\n%?\n\n"
           :empty-lines 1)

      ("j" "Journal")
      ("jj" "Journal Entry" entry
           (file+olp+datetree "~/Dropbox/Org/journal.org")
           "\n* %<%H:%M> - Journal :journal:\n\n%?\n\n"
           :empty-lines 1)))

(setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))

     (todo "TODO"
            ((org-agenda-overriding-header "TODO")
             (org-agenda-files org-agenda-files)))
     (todo "INPROGRESS"
            ((org-agenda-overriding-header "INPROGRESS")
             (org-agenda-files org-agenda-files)))))))
  
  (org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(defun org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

(defun org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :hook ((org-tree-slide-play . org-start-presentation)
	 (org-tree-slide-stop . org-end-presentation))
  :config
  (evil-define-key 'normal org-tree-slide-mode-map
    (kbd "q") 'org-end-presentation
    (kbd "<down>") 'org-tree-slide-move-next-tree
    (kbd "<right>") 'org-tree-slide-move-next-tree
    (kbd "<up>") 'org-tree-slide-move-previous-tree
    (kbd "<left>") 'org-tree-slide-move-previous-tree)
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message "Presentation started."
        org-tree-slide-deactivate-message "Presentation ended."
        org-tree-slide-header t
	org-tree-slide-breadcrumbs " // "))

(use-package lsp-mode
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-ui-doc-enable nil))

(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

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

(defun configure-eshell ()
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
  :hook (eshell-first-time-mode . configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'powerline))

(use-package general
  :config
  (general-create-definer custom-keys
    :keymaps '(normal visual emacs dired)
    :prefix "SPC"
    :global-prefix "SPC")
  
  ;; general key bindings
  (custom-keys
    "k"  '(kill-buffer :which-key "select and kill buffer")
    "q"  '(kill-buffer-and-window :which-key "kill current buffer and window")
    "."  '(switch-to-buffer :which-key "switch to buffer")
    "d" '(dired :which-key "dired") 
    
    "e"  '(:ignore e :which-key "evaluate")
    "eb" '(eval-buffer :which-key "evaluate current buffer")
    "ee" '(eval-expression :which-key "evaluate expression")
    "er" '(eval-region :which-key "evaluate region")

    "f"  '(:ignore f :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fo" '(find-file-other-window :which-key "open file in new window")
    "fr" '(counsel-recentf :which-key "find from recent files")

    "g"  '(:ignore g :which-key "git")
    "gs"  '(magit-status :which-key "magit-status")

    "o"  '(:ignore o :which-key "org")
    "oc" '(org-capture :which-key "capture")
    "oa" '(org-agenda :which-key "agenda")
    "os" '(org-schedule :which-key "schedule")
    "od" '(org-deadline :which-key "set deadline")
    "ot" '(org-time-stamp :which-key "set time stamp")

    "p"  '(projectile-command-map :which-key "projectile")
    "pg"  '(counsel-projectile-grep :which-key "counsel-projectile-grep")

    "s"  '(eshell :which-key "eshell")

    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")

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
   '(lsp-python-ms pyls dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt evil-nerd-commenter company flycheck ccls lsp-ui lsp-mode visual-fill-column org-bullets evil-magit magit counsel-projectile projectile general evil-collection evil which-key use-package rainbow-delimiters ivy-rich helpful doom-themes doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
