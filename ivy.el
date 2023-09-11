; (require 'uniquify)
; (setq uniquify-buffer-name-style 'post-forward
;       uniquify-separator ":")

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package diminish
  :ensure t)

(use-package ivy
  :diminish
  :ensure t
  :config
  (ivy-mode 1))
