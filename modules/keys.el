;;; keys.el -- All generic general/which-key bindings

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(when (is-mac)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control))

(defun my/consult-flymake-in-project ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (if project-root
        (consult-flymake project-root)
      (message "Not in a Projectile project."))
    ))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer custom-keys
    :states '(normal visual motion)
    :keymaps '(override dashboard)
    :prefix "SPC")

  (custom-keys
    "."   '(bufler :wk "list buffers")
    "/"   '(my/switch-recent-buffer :wk "switch to recent buffer")
    "SPC" '(my/switch-to-dashboard-buffer :wk "switch to dashboard buffer")

    "c"  '(:ignore c :wk "consult")
    "cr" '(consult-ripgrep :wk "ripgrep")
    "co" '(consult-outline :wk "outline")
    "ch" '(consult-history :wk "history")
    "ce" '(consult-file-externally :wk "open file externally")

    "e"  '(:ignore e :wk "eval")
    "eb" '(eval-buffer :wk "buffer")
    "ee" '(eval-expression :wk "expression")
    "er" '(eval-region :wk "region")
    "es" '(org-babel-execute-src-block :wk "source block")

    "f"  '(:ignore f :wk "file")
    "ff" '(find-file :wk "find file")
    "fr" '(consult-recent-file :wk "find from recent files")
    "fo" '(find-file-other-window :wk "open file in new window")

    "g"  '(:ignore g :wk "goto")
    "gd" '(evil-goto-definition :wk "go to definition")
    "gb" '(evil-jump-backward :wk "go to last jump position")
    "gc" '(evil-goto-last-change :wk "go to last change")
    "gl" '(consult-goto-line :wk "go to line N")

    "d"  '(:ignore d :wk "dired")
    "dj" '(dired-jump :wk "jump")

    "m"  '(magit-status :wk "magit")

    "o"  '(:ignore o :wk "org")
    "oa" '(org-agenda :wk "agenda")
    "oc" '(org-roam-capture :wk "capture")
    "of" '(org-roam-node-find :wk "find node")
    "oi" '(org-roam-node-insert :wk "insert")

    "p"  '(:ignore p :wk "projectile")
    "pf" '(projectile-find-file :wk "find file")
    "pr" '(projectile-recentf :wk "find from recent files")
    "pF" '(projectile-find-in-known-projects :wk "find file in known projects")
    "pp" '(projectile-switch-project :wk "switch project")
    "ps" '(projectile-save-project-buffers :wk "save project buffers")
    "pd" '(my/consult-flymake-in-project :wk "jump through project diagnostics")
    "pD" '(flymake-show-project-diagnostics :wk "list project diagnostics")
    "pe" '(consult-ripgrep :wk "ripgrep")
    "pb" '(consult-project-buffer :wk "switch to project buffer")

    "s"  '(:ignore s :wk "shell/terminal")
    "st" '(term :wk "term")
    "ss" '(eshell :wk "eshell")

    "t"  '(:ignore t :wk "toggle")
    "tT" '(toggle-truncate-lines :wk "truncate lines")
    "tv" '(visual-line-mode :wk "visual line mode")
    "tn" '(display-line-numbers-mode :wk "display line numbers")
    "tR" '(read-only-mode :wk "read only mode")

    "q"  '(:ignore q :wk "quit")
    "qq" '(evil-quit-all :wk "quit all")
    "qk" '(my/kill-all-buffers :wk "kill all")

    "b"  '(:ignore b :wk "buffer")
    "br" '(revert-buffer-quick :wk "revert buffer")
    "bk" '(kill-buffer-and-window :wk "kill buffer and window")
    "bo" '(my/kill-buffer-other-window :wk "kill buffer in other window")

    "w"  '(:ignore w :wk "window")
    "TAB"'(other-window :wk "switch window")
    "wf" '(make-frame :wk "open active window in new frame")
    "wd" '(delete-window :wk "delete window")
    "wo" '(delete-other-windows :wk "delete other windows")
    "wb" '(split-window-below :wk "split window below")
    "wr" '(split-window-right :wk "split window right")

    "x" '(execute-extended-command :wk "execute command")))

(provide 'keys)
;;; keys.el ends here
