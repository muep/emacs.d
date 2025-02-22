; (require 'uniquify)
; (setq uniquify-buffer-name-style 'post-forward
;       uniquify-separator ":")
(use-package diminish
  :ensure t)

(use-package rg
  :ensure t)

(use-package projectile
  :diminish
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package ivy
  :diminish
  :ensure t
  :config
  (ivy-mode 1))
