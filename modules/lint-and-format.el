;;; lint-and-format.el -- Linter and formatting related configuration

(use-package flymake
  :ensure flymake
  :demand t
  :hook (prog-mode . flymake-mode))

(setq flymake-wrap-around nil)

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'shfmt apheleia-formatters) '("shfmt" "-i=4" "-sr" "-kp"))
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(provide 'lint-and-format)
;;; lint-and-format.el ends here
