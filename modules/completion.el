;;; completion.el -- All completion related configuration

(setq completion-ignored-extensions '(".meta"))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  :init
  (global-corfu-mode))

(use-package consult
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package eglot
  :defer t
  :hook
  (rustic-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (svelte-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio"))))

(setq eglot-confirm-server-initiated-edits nil)

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps '(eglot-mode-map toml-mode-map)
 "l"  '(:ignore l :wk "Eglot")
 "lf" '(eglot-code-action-quickfix :wk "quickfix")
 "la" '(eglot-code-actions :wk "code actions")
 "lr" '(eglot-rename :wk "rename symbol")
 "lR" '(eglot-reconnect :wk "reconnect Eglot")
 "ld" '(eldoc-doc-buffer :wk "show doc buffer")
 "li" '(eglot-find-implementation :wk "find implementation")
 "lu" '(xref-find-references :wk "find usages")
 ";"  '(flymake-goto-next-error :wk "next error"))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(defun my/vertigo-backward-kill (arg)
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  :ensure t
  :bind
  (:map minibuffer-local-map ("<left>" . my/vertigo-backward-kill))
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize t)
  (setq vertico-cycle t)

  (with-eval-after-load 'rfn-eshadow
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(provide 'completion)
;;; completion.el ends here
