;; In MacOS, is kind of makes sense to keep the menu bar
(if (fboundp 'tool-bar-mode)
    (menu-bar-mode 1))

;; Modifier keys
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
