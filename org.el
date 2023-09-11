(use-package org)
(use-package org-roam
  :ensure t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(defun underscore-to-dash (txt)
  (string-replace "_" "-" txt))

(when (functionp 'org-roam-version)
  (require 'org)
  (require 'org-roam)
  (require 'org-roam-dailies)
  ;; Keybinding suggestions from
  ;; https://lucidmanager.org/productivity/taking-notes-with-emacs-org-mode-and-org-roam/
  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n r") 'org-roam-node-random)
  (global-set-key (kbd "C-c n d") 'org-roam-dailies-map)
  (define-key org-mode-map (kbd "C-c n i") 'org-roam-node-insert)
  (define-key org-mode-map (kbd "C-c n o") 'org-id-get-create)
  (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-mode-map (kbd "C-c n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "C-c n l") 'org-roam-buffer-toggle)

  (advice-add 'org-roam-node-slug :filter-return #'underscore-to-dash)

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "${slug}.org"
                      "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-directory (file-truename "~/org/zk"))
  (org-roam-db-autosync-mode))

;; Nicer auto fill for org mode
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (setq fill-column 60)))

