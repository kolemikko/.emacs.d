;;; Dashboard
(use-package welcome-dashboard
  :straight (:host github :repo "konrad1977/welcome-dashboard" :branch "main"))
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981
        welcome-dashboard-use-nerd-icons nil
        welcome-dashboard-path-max-length 75
        welcome-dashboard-use-fahrenheit nil
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-width 200
        welcome-dashboard-max-number-of-todos 5
        welcome-dashboard-image-height 169
        welcome-dashboard-title "Welcome to Emacs!")
  (welcome-dashboard-create-welcome-hook))

(provide 'dashboard)
