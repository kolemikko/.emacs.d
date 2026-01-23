;;; evil-mode.el -- All Evil-mode related configuration

(use-package evil
  :init
  ;; Set before loading evil-collection
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Arrow keys behave like hjkl in normal/visual/motion states
  (dolist (map (list evil-normal-state-map
                     evil-visual-state-map
                     evil-motion-state-map))
    (define-key map (kbd "<left>")  #'evil-backward-char)
    (define-key map (kbd "<down>")  #'evil-next-visual-line)
    (define-key map (kbd "<up>")    #'evil-previous-visual-line)
    (define-key map (kbd "<right>") #'evil-forward-char))

  ;; Set initial states
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'pdf-view-mode 'motion))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(provide 'evil-mode)
;;; evil-mode.el ends here
