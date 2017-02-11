(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 70)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (c-mode . "manualtab")
                     (other . "gnu"))))


(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Remove the toolbar and menu bar if possible
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(defconst manualtab-cpp-style
  '("gnu"
    (tab-width . 8)
    (c-basic-offset . 8)
    (c-syntactic-indentation . nil)
    (indent-tabs-mode . t)))
(c-add-style "manualtab" manualtab-cpp-style)
