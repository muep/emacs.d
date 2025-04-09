(use-package org-ai
  :ensure t
  :defer t
  :commands (org-ai-mode org-ai-global-mode)
  :config
  (setq org-ai-auto-fill t)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode))
