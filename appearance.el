(setq modus-themes-bold-constructs t)
(setq modus-themes-paren-match '(bold))

(if (string-match "work" user-init-file)
    (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(defun select-default-font ()
  (let ((available-fonts (font-family-list))
        (preferred-fonts '(("Hack" . "Hack-10")
                           ("DejaVu Sans Mono" . "DejaVu Sans Mono-10")
                           ("Liberation Mono" . "Liberation Mono-10")
                           ("Menlo" . "Menlo-11")
                           ("Consolas" . "Consolas-10"))))
    (cdar (seq-filter (lambda (font)
                        (member (car font) available-fonts))
                      preferred-fonts))))

(let ((the-font (select-default-font)))
  (when the-font
    (set-face-attribute 'default nil :font (select-default-font))))
