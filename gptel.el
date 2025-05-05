(use-package gptel
  :defer t
  :ensure t
  :config
  (setq gptel-backend
        (gptel-make-openai "Mistral"
          :key 'gptel-api-key
          :host "api.mistral.ai"
          :models '("mistral-large-latest" "mistral-tiny" "mistral-small" "mistral-medium")))
  :bind
  (("C-c å g s" . gptel-send)))
