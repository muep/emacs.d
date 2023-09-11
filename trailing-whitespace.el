(setq-default show-trailing-whitespace t)

(defun disable-trailing-whitespace-display ()
  (setq show-trailing-whitespace nil))

;; Disable trailing whitespace display for special-mode children and
;; some other modes as well. This includes stuff like the git log view
;; mode and suchlike.
(add-hook 'special-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'compilation-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'diff-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'term-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'shell-mode-hook 'disable-trailing-whitespace-display)

(defun enable-autoclear-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
