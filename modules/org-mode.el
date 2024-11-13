;;; org-mode.el -- All Org-mode related configuration

(defun my/org-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . my/org-mode-setup)
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

  (setq org-todo-keywords
        '((sequence "TODO"
                    "INPROGRESS"
                    "DONE")))

  (setq org-modules
        '(org-crypt)))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(setq calendar-week-start-day 1)
(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))

(use-package org-roam
  :defer t
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
      :if-new (file+head "Todo.org" "#+title: TODO\n")
      :file-name "Todo.org"
      :unnarrowed t
      :immediate-finish))))

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

(setq org-latex-listings t)
(setq org-latex-compiler "xelatex")

(setq tramp-default-method "ssh")
(setq remote-file-name-inhibit-cache nil)
(setq tramp-verbose 1)
(setq tramp-chunksize 500)

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'org-mode-map
 "o"  '(:ignore o :wk "org")
 "oe" '(org-export-dispatch :wk "export dispatch")
 "or" '(org-todo :wk "rotate todo state")
 "os" '(org-schedule :wk "schedule")
 "od" '(org-deadline :wk "deadline")
 "ot"  '(:ignore ot :wk "table")
 "ots" '(org-table-sort-lines :wk "sort lines")
 "ote" '(org-table-export :wk "export")
 "otc" '(org-table-create :wk "create")
 "oti"  '(:ignore oti :wk "insert")
 "otic" '(org-table-insert-column :wk "column")
 "otir" '(org-table-insert-row :wk "row")
 "otih" '(org-table-insert-hline :wk "horizontal line")
 "ob" '(:ignore ob :wk "babel")
 "obt"'(org-babel-tangle :wk "tangle")
 "op" '(org-present :wk "presentation mode"))


(provide 'org-mode)
;;; org-mode.el ends here
