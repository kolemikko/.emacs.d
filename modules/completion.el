;;; completion.el -- All completion related configuration

(setq completion-ignored-extensions '(".meta"))

(use-package corfu
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
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
 ";"  '(flymake-goto-next-error :wk "next error")
 "tt" '(treemacs :wk "treemacs"))

(use-package treesit-auto
  :ensure treesit-auto
  :commands
  global-treesit-auto-mode
  :defines
  treesit-auto-fallback-alist
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq major-mode-remap-alist
        '(
          (bash-mode . bash-ts-mode)
          (css-mode . css-ts-mode)
          (js2-mode . js-ts-mode)
          (svelte-mode . typescript-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
  (global-treesit-auto-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

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

(provide 'completion)
;;; completion.el ends here
