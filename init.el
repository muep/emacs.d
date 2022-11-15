;; init.el
;;
;; Personal emacs configuration for Joonas Sarajärvi
;; See README for more information.

(when (version< emacs-version "26.3")
  ;; Seems to be required on older versions of Emacs. See
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36725
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Load the Customize data from elsewhere, to avoid making them change
;; this init file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; Set us up to use a package repository.
(when (< emacs-major-version 27)
  (package-initialize))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

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

;; Keybindings for things for which there seems to be no convenient
;; default.
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c f") 'describe-face)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(if (functionp 'magit-status)
    (global-set-key (kbd "C-x g") 'magit-status))

(when (functionp 'org-roam-version)
  (setq org-roam-directory (file-truename "~/org/zk"))
  (org-roam-db-autosync-mode))

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
  (set-face-attribute 'default nil :font (select-default-font))

  ;; Remove the toolbar from top of the X frames:
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1)))

 ;; On OS X the window system is called ns
 ((eq window-system 'ns)
  ;; Font selection
  (if (member "Menlo" (font-family-list))
      (set-face-attribute 'default nil :font "Menlo-10"))

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

(when (functionp 'projectile-mode)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (when (functionp 'counsel-projectile-mode)
      (counsel-projectile-mode)
      ;; This overrides the stock buffer change command
      (global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(require 'rg)
(rg-enable-default-bindings)

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
        (while (looking-at "\\(\t\\| \\)")
          (forward-char)))))

;; This used in multiple styles, so spelled out one
;; time here.
(defconst muep-c-offsets-alist
  '((innamespace . 0)))

;; A customized C++ style. Likely needs some adjustment before being
;; fully useful.
(defconst muep-cpp-style
  `("gnu"
    (tab-width . 4)
    (c-basic-offset . 4)
    (cua-auto-tabify-rectangles . nil)
    (c-special-indent-hook . muep-cpp-indent-hook)
    (c-offsets-alist . ,muep-c-offsets-alist)
    (indent-tabs-mode . t)))

(c-add-style "muep" muep-cpp-style)

;; A variant of the above that uses 4-space basic indentation instead
;; of one tab. Nicely this does not need the special indent hook that
;; is used in the "normal" muep style.
(defconst muep-cpp-style-4spc
  `("gnu"
    (tab-width . 8)
    (c-basic-offset . 4)
    (c-offsets-alist . ,muep-c-offsets-alist)
    (indent-tabs-mode . nil)))

(c-add-style "muep4" muep-cpp-style-4spc)

;; A simple "style" that disables the automatic indentation of emacs
;; and sets some variables so that they suit editing a file that
;; mostly uses tabs for indentation.
(defconst manualtab-cpp-style
  '("gnu"
    (tab-width . 8)
    (c-basic-offset . 8)
    (c-syntactic-indentation . nil)
    (indent-tabs-mode . t)))
(c-add-style "manualtab" manualtab-cpp-style)

;; A simple "style" that disables the automatic indentation of emacs
;; and sets some variables so that they suit editing a file that
;; mostly uses spaces for indentation.
(defconst manualspc-cpp-style
  '("gnu"
    (tab-width . 8)
    (c-basic-offset . 4)
    (c-syntactic-indentation . nil)
    (indent-tabs-mode . nil)))
(c-add-style "manualspc" manualspc-cpp-style)

(defun enable-ff-find-other-file ()
  ;; Use C-c o to jump between headers and sources
  (local-set-key (kbd "C-c o") 'ff-find-other-file))

(defun enable-autoclear-whitespace ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(defun muep-c-mode-keys ()
    (define-key
      c-mode-base-map
      (kbd "<backtab>")
      (lambda ()
        (interactive)
        (c-indent-line-or-region -1))))

(add-hook 'c-initialization-hook 'muep-c-mode-keys)
(add-hook 'c-mode-common-hook 'enable-ff-find-other-file)

;; Nicer auto fill for org mode
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (setq fill-column 60)))

;; Enable a bunch of functions that are disabled by default to avoid
;; confusion.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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
        (datestr (format-time-string "%Y-%m-%d")))
    (mapconcat 'identity
               (list
                "/*\n"
                " * " fname "\n"
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

(defun muep-surround-region (start end)
  "Insert a pair of strings so that they surround either point or
   region, depending on if region is active"
  (if (use-region-p)
      (let ((orig-rb (region-beginning))
            (orig-re (region-end)))
          (goto-char orig-re)
          (insert end)
          (goto-char orig-rb)
          (insert start)
          (goto-char orig-rb)
          (forward-line 1))
      (let ((orig-pt (point)))
        (insert start)
        (insert "\n")
        (insert end)
        (goto-char orig-pt)
        (forward-line 1))))

(defun muep-cpp-namespace-text (name)
  (let ((nsn (if (< 0 (length name)) (concat name " ") "")))
    (mapconcat 'identity
               (list
                "namespace " nsn "{\n"
                "\n"
                "} /* namespace " nsn "*/")
               "")))

(defun muep-insert-cpp-namespace (name)
  "Insert a C++ namespace block"
  (interactive "Mname:")
  (insert (muep-cpp-namespace-text name))
  (forward-line -1))

(defun muep-c-ifdef-start (name)
  (mapconcat 'identity (list "#ifdef " name "\n") ""))

(defun muep-c-ifdef-end (name)
  (mapconcat 'identity (list "#endif /* defined " name " */\n") ""))

(defun muep-insert-c-ifdef (name)
  "Insert a C ifdef block"
  (interactive "Mname:")
  (muep-surround-region (muep-c-ifdef-start name)
                        (muep-c-ifdef-end name)))

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



(defun muep-kill-orglink-here ()
  "Copy current location as an org mode link"
  (interactive)
  (let* ((path (buffer-file-name))
         (file-name-only (file-name-nondirectory path))
         (line (int-to-string (line-number-at-pos)))
         (query (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  line))
         (link-text (if (region-active-p)
                        query
                      (mapconcat 'identity (list file-name-only ":" line) ""))))
    (kill-new (mapconcat 'identity
                         (list
                          "[[file:" path "::" query "]"
                          "[" link-text "]]")
                         ""))))

(defun muep-kill-orglink-thisfile ()
  "Copy current location as an org mode link"
  (interactive)
  (let* ((path (buffer-file-name))
         (file-name-only (file-name-nondirectory path))
         (line (int-to-string (line-number-at-pos)))
         (query (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  line))
         (link-text (if (region-active-p)
                        query
                      (mapconcat 'identity (list file-name-only ":" line)))))
    (kill-new (mapconcat 'identity
                         (list
                          "[[file:" path "::" query "]"
                          "[" link-text "]]")
                         ""))))

(require 'svelte-mode)

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
