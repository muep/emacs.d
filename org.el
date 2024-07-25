;; Same as current org default, but maybe better be explicit
(setq org-directory (file-truename (concat (file-name-parent-directory user-emacs-directory) "/org")))
(setq org-agenda-files (list (file-truename (concat org-directory "/agenda"))))
(setq org-roam-directory (file-truename (concat org-directory "/zk")))

(setq org-roam-dailies-filename-pattern
      (if (string-match "work" user-init-file)
          "work-%<%Y-%m-%d>.org"
        "%<%Y-%m-%d>.org"))

(setq org-roam-dailies-template-source
      (if (string-match "work" user-init-file)
          "%[work-template.org]"
        "%[template.org]"))

(defun underscore-to-dash (txt)
  (string-replace "_" "-" txt))

(use-package org
  :defer t
  :bind (:map global-map
              ("C-c a" . org-agenda))
  )
(use-package org-roam
  :defer t
  :ensure t
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n l" . org-roam-buffer-toggle))
  :config 
  (advice-add 'org-roam-node-slug :filter-return #'underscore-to-dash)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "${slug}.org"
                      "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %?" :target
           (file+head
            ,org-roam-dailies-filename-pattern
            ,(concat "#+title: %<%Y-%m-%d>\n" org-roam-dailies-template-source)))))
  (org-roam-db-autosync-mode))

(use-package org-roam-dailies
  :defer t
  :bind-keymap (("C-c n d" . org-roam-dailies-map)))
