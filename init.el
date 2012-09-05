;; init.el
;;
;; Personal emacs configuration for Joonas Sarajärvi
;; See README for more information.

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
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Keybindings for things for which there seems to be no convenient
;; default.
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c f") 'describe-face)

;; Solarized setup
(let ((tgt (expand-file-name "~/.emacs.d/solarized")))
  (if (or (file-directory-p tgt)
          (file-symlink-p tgt))

      (progn
        (add-to-list 'custom-theme-load-path tgt)
        (condition-case nil
            (load-theme 'solarized-light t)
          (error nil)))))

;; Platform specific tweaks
(cond
 ;; Mostly just GNU/Linux
 ((eq window-system 'x)
  ;; Font selection
  (cond
   ((member "Terminus" (font-family-list))
    (set-face-attribute 'default nil :font "Terminus-9"))
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-9")))

  ;; Remove the toolbar from top of the X frames:
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1)))

 ;; On OS X the window system is called ns
 ((eq window-system 'ns)
  ;; Font selection
  (if (member "Menlo" (font-family-list))
      (set-face-attribute 'default nil :font "Menlo-11"))

  ;; Avoid toolbar, but menubar is ok since the space is always used
  ;; in OS X.
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))

  ;; Modifier keys
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t))

 ;; Windows specific tweaks
 ((eq window-system 'w32)
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1))
  (if (member "Consolas" (font-family-list))
      (set-face-attribute 'default nil :font "Consolas"))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; A kludge that implements "Smart tabs" similarly to how it
;; is done at http://www.emacswiki.org/SmartTabs
;;
;; Main difference is that this is easy to insert into a
;; custom CC mode style.
(defun muep-cpp-indent-hook ()
  ;; Since this function will call c-indent-line which will
  ;; cause this function to be called again, it has a
  ;; mechanism to avoid recursing infinitely.
  (if (not (boundp 'already-in-muep-cpp-indent-hook))
      (progn
        (save-excursion
          ;; Remove existing whitespace from line beginning
          (beginning-of-line)
          (while (looking-at "\t*\\( +\\)\t+")
            (replace-match "" nil nil nil 1))
          (let (;; Mark that we are already in this...
                (already-in-muep-cpp-indent-hook t)
                ;; And set tab width to a high value
                (tab-width fill-column)
                (c-basic-offset fill-column))
            ;; And perform re-indentation in that environment
            (c-indent-line)))
        ;; If point was left into the beginning of line, move
        ;; forwards.
        (while (looking-at "\t")
          (forward-char)))))

;; A customized C++ style. Likely needs some adjustment before being
;; fully useful.
(defconst muep-cpp-style
  '("gnu"
    (tab-width . 4)
    (c-basic-offset . 4)
    (cua-auto-tabify-rectangles . nil)
    (c-special-indent-hook . muep-cpp-indent-hook)
    (c-offsets-alist . ((innamespace . 0)))
    (indent-tabs-mode . t)))

(c-add-style "muep" muep-cpp-style)

;; Stuff specific to CC-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Use C-c o to jump between headers and sources
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            ;; Remove trailing whitespace automatically
            (add-hook
             'before-save-hook
             'delete-trailing-whitespace nil t)))

;; Nicer auto fill for org mode
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (setq fill-column 60)))

;; Avoid showing trailing whitespace in diffs. In the diff mode, the
;; trailing whitespace visualization does not really work very well.
(add-hook 'diff-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Enable narrow-to-region. This is disabled by default to avoid
;; confusion.
(put 'narrow-to-region 'disabled nil)

;; Seems I can not find anything similar in
;; stock emacs, so let's roll our own
(defun muep-any (predicate l)
  (if
      (consp l)
      (if (funcall predicate (car l))
          t
        (muep-any predicate (cdr l)))
    ;; Empty list does not have any
    nil))

;; Test if file name looks like a name of a C/C++ header
(defun headerishp (fname)
  (muep-any
   (lambda (ext)
     (equal (file-name-extension fname) ext))
   '("h" "hh" "hpp")))

;; Include guard generation
(defun muep-get-include-guard-id ()
  "Generate the include guard macro name based on current buffer filename."
   (replace-regexp-in-string
    "\\." "_"
    (upcase (file-name-nondirectory (buffer-file-name)))))

(defun muep-insert-include-guards ()
  "Insert a C/C++ style include guard into the current buffer"
  (interactive)
  (insert
   "#ifndef " (muep-get-include-guard-id) "\n"
   "#define " (muep-get-include-guard-id) "\n\n\n"
   "#endif\n"))

;; Other boilerplate generation
(defun muep-c-introheader ()
  (let ((fname (file-relative-name (buffer-file-name)))
        (myname (user-full-name))
        (datestr (format-time-string "%Y-%m-%d")))
    (mapconcat 'identity
               (list
                "/*\n"
                " * " fname "\n"
                " *\n"
                " * Created on " datestr " by\n"
                " *   " myname "\n"
                " *\n"
                " * DESCRIBE THE PURPOSE OF THIS FILE HERE\n"
                " */\n") "")))

(defun muep-insert-c-boilerplate ()
  "Insert common boilerplate into the current
   file as a C comment"
  (interactive)
  (insert (muep-c-introheader))
  (if (headerishp (file-name-nondirectory (buffer-file-name)))
      (muep-insert-include-guards)))
