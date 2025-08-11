;;; llm.el -- LLMs etc.

(use-package ellama
  :init
  (require 'llm-ollama)
  (setopt ellama-language "English")
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "codellama:latest" :embedding-model "codellama:latest"))
  :config
  (setopt ellama-auto-scroll t)
  (ellama-context-mode-line-mode +1))

(provide 'llm)
;;; llm.el ends here
