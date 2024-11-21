;;; utils.el -- All kind of utility functions

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
  (if (> (length (window-list)) 1)
      (evil-window-mru)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun my/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*scratch*"))
  (revert-buffer-quick))

(provide 'utils)
;;; utils.el ends here
