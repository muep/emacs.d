;; init.el
;;
;; Personal emacs configuration for Joonas Sarajärvi
;; See README for more information.

;; Load the Customize data from elsewhere, to avoid making them change
;; this init file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; Set us up to use Melpa as the package repository. Emacs nowadays
;; comes with some GNU-blessed package archives preconfigured, but it
;; seems safer to stick with just one archive.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

;; A bunch of simple default overrides
(column-number-mode 1)
(setq-default fill-column 70)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq-default show-trailing-whitespace t)
(electric-indent-mode -1)

;; Add a convenient place for some local lisp code, if any
(let ((default-directory  "~/.emacs.d/lisp/"))
  (if (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))

(load-file (expand-file-name "ivy.el" user-emacs-directory))
(load-file (expand-file-name "magit.el" user-emacs-directory))
(load-file (expand-file-name "org.el" user-emacs-directory))
(load-file (expand-file-name "cc.el" user-emacs-directory))

;; Keybindings for things for which there seems to be no convenient
;; default.
(global-set-key (kbd "M--") 'xref-find-references)

;; Clojure
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

;; Cider
(defun muep-cider-eval-and-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(defun muep-cider-eval-and-test-ns ()
  (interactive)
  (cider-load-buffer)
  (cider-test-run-ns-tests nil))

(defun muep-cider-keys ()
  (define-key cider-mode-map (kbd "<f8> n") 'muep-cider-eval-and-test-ns)
  (define-key cider-mode-map (kbd "<f8> t") 'muep-cider-eval-and-test))

(add-hook 'cider-mode-hook 'muep-cider-keys)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'paredit-mode)

;; Cider REPL
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(defun disable-trailing-whitespace-display ()
  (setq show-trailing-whitespace nil))

;; Disable trailing whitespace display for special-mode children. This
;; includes stuff like the git log view mode and suchlike.
(add-hook 'special-mode-hook 'disable-trailing-whitespace-display)

;; Also some others
(add-hook 'compilation-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'diff-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'term-mode-hook 'disable-trailing-whitespace-display)
(add-hook 'shell-mode-hook 'disable-trailing-whitespace-display)

(defun setup-theme ()
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-paren-match '(bold))
  (load-theme 'modus-vivendi t))

(if window-system
    (progn
      ;; Initialization for cases when we are in some window system
      (setup-theme)
      (windmove-default-keybindings))
  ;; Could add items that are only required in terminal mode.
  )

(defun select-default-font ()
  (let ((available-fonts (font-family-list))
        (preferred-fonts '(("Hack" . "Hack-10")
                           ("DejaVu Sans Mono" . "DejaVu Sans Mono-10")
                           ("Liberation Mono" . "Liberation Mono-10"))))
    (cdar (seq-filter (lambda (font)
                        (member (car font) available-fonts))
                      preferred-fonts))))

;; Platform specific tweaks
(cond
 ;; Mostly just GNU/Linux
 ((eq window-system 'x)
  ;; Font selection
  (set-face-attribute 'default nil :font (select-default-font)))

 ;; On OS X the window system is called ns
 ((eq window-system 'ns)
  ;; Font selection
  (if (member "Menlo" (font-family-list))
      (set-face-attribute 'default nil :font "Menlo-11"))

  ;; In MacOS, is kind of makes sense to keep the menu bar
  (if (fboundp 'tool-bar-mode)
      (menu-bar-mode 1))

  ;; Modifier keys
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t))

 ;; Windows specific tweaks
 ((eq window-system 'w32)
  (if (member "Consolas" (font-family-list))
      (set-face-attribute 'default nil :font "Consolas"))))

;; Install paredit from MELPA to make use of this
(if (functionp 'enable-paredit-mode)
    (progn
      (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode)))

;; Install rainbow-delimiters from MELPA to make use of this
(if (functionp 'rainbow-delimiters-mode)
    (progn
      (add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
      (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
      (add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")


(defun enable-autoclear-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

;; Enable a bunch of functions that are disabled by default to avoid
;; confusion.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun muep-py2-boilerplate ()
  "Insert a group of future imports for Python 2"
  (interactive)
  (insert
   (mapconcat
    'identity
    '("from __future__ import absolute_import\n"
      "from __future__ import division\n"
      "from __future__ import print_function\n"
      "from __future__ import unicode_literals\n")
    "")))

(defun muep-string-trim-left (s)
  (replace-regexp-in-string "^[\s\t\n]" "" s))

(defun muep-string-trim-right (s)
  (replace-regexp-in-string "[\s\t\n]$" "" s))

;; No idea why stuff like this does not seem to be built-in
(defun muep-string-trim (s)
  (muep-string-trim-left (muep-string-trim-right s)))

(defun muep-insert-specfile-date ()
  "Insert date as expected in Fedora changelogs"
  (interactive)
  (insert
   (muep-string-trim
    (shell-command-to-string "env LC_ALL=C date \"+%a %b %d %Y\""))))

(defun muep-insert-reldate (n)
  "Insert a date relative to today"
  (interactive "Noffset:")
  (let* ((now (current-time))
         (dif (days-to-time n))
         (then (time-add now dif)))
    (insert
     (format-time-string "%Y-%m-%d" then))))

(defun muep-insert-today ()
  "Insert the date of today"
  (interactive)
  (muep-insert-reldate 0))

(defun buffer-line-count ()
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))

(defun goto-random-line ()
  "Go to a random line in this buffer."
  ; good for electrobibliomancy.
  (interactive)
  (goto-line (1+ (random (buffer-line-count)))))

;; From https://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)
