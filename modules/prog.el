;;; prog.el -- All programming related configurations

;; Rust
(use-package rustic
  :defer t
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  :custom
  (rustic-rustfmt-config-alist '((edition . "2018"))))

(defun my/rustic-build-with-arguments()
  (interactive)
  (rustic-cargo-build ""))

(defun my/rust-cargo-tree()
  (interactive)
  (shell-command "cargo tree"))

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps '(rustic-mode-map toml-mode-map)
 "lc"  '(:ignore lc :wk "cargo")
 "lcb" '(my/rustic-build-with-arguments :wk "build with arguments")
 "lcc" '(rustic-cargo-clippy :wk "clippy")
 "lcf" '(rustic-cargo-clippy-fix :wk "clippy fix")
 "lcC" '(rustic-cargo-clean :wk "clean")
 "lco" '(rustic-cargo-outdated :wk "cargo-outdated")
 "lcu" '(rustic-cargo-update :wk "update")
 "lcr" '(rustic-cargo-run :wk "run")
 "lct" '(rustic-cargo-test :wk "cargo test")
 "lcT" '(my/rust-cargo-tree :wk "cargo tree")

 "le"  '(:ignore le :wk "cargo-edit")
 "lea" '(rustic-cargo-add :wk "add crate")
 "leA" '(rustic-cargo-add-missing-dependencies :wk "add missing crates")
 "ler" '(rustic-cargo-rm :wk "remove crate")
 "let" '(rustic-open-dependency-file :wk "open cargo.toml file"))

;; Node.js modules
(use-package add-node-modules-path
  :ensure add-node-modules-path
  :commands
  add-node-modules-path)

;; Typescript
(use-package typescript-mode
  :ensure typescript-mode
  :after (tree-sitter)
  :defines
  typescript-mode-map
  :mode
  "\\.ts\\'"
  :hook
  (typescript-mode . add-node-modules-path)
  (typescript-ts-mode . add-node-modules-path)
  :custom
  (typescript-indent-level 2)
  :config
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX"))

;; Javascript
(use-package js2-mode
  :ensure js2-mode
  :diminish "js2"
  :commands
  js2-mode
  :defines
  js2-additional-externs
  js2-mode-map
  :hook
  (js2-mode . add-node-modules-path)
  :interpreter
  "node"
  :mode
  "\\.jsx?\\'"
  :custom
  (js2-global-externs '("Promise"))
  (js2-highlight-level 3)
  (js2-include-browser-externs nil)
  (js2-include-node-externs t)
  (js2-mode-assume-strict t)
  :config
  (add-to-list 'safe-local-variable-values '(js2-basic-offset . 2))
  (add-to-list 'safe-local-variable-values '(js2-basic-offset . 4))
  (setq-default js2-basic-offset 2))

;; Markdown
(use-package markdown-mode
  :defer t
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun my/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun my/markdown-mode-hook ()
    (my/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'my/markdown-mode-hook))

(use-package toml-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package irony-eldoc
  :defer t)
(use-package irony
  :defer t)
(use-package arduino-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.ino$" . arduino-mode))

(use-package platformio-mode
  :defer t)

(use-package impatient-mode
  :defer t
  :straight t)

(add-hook 'markdown-mode-hook 'impatient-mode)

(defun my/markdown-html-filter (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(defun my/preview-markdown ()
  (interactive)
  (impatient-mode)
  (httpd-start)
  (setq impatient-mode-delay 1)
  (setq imp-user-filter 'my/markdown-html-filter)
  (imp-visit-buffer))

(provide 'prog)
;;; prog.el ends here
