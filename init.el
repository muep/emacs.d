;; init.el
;;
;; Personal emacs configuration for Joonas Sarajärvi
;; See README for more information.

;; Load the Customize data from elsewhere, to avoid making them change
;; this init file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; Set us up to use Melpa as the package repository. At least some
;; packages expect the GNU archive to be in use, so that is included
;; as well.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; A bunch of simple default overrides
(column-number-mode 1)
(setq-default fill-column 70)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(electric-indent-mode -1)

(when window-system
  ;; Let's avoid touching it when in terminal
  (load-file (expand-file-name "appearance.el" user-emacs-directory)))

(load-file (expand-file-name "ivy.el" user-emacs-directory))
(load-file (expand-file-name "magit.el" user-emacs-directory))
(load-file (expand-file-name "org.el" user-emacs-directory))
(load-file (expand-file-name "lisps.el" user-emacs-directory))
; (load-file (expand-file-name "cc.el" user-emacs-directory))
; (load-file (expand-file-name "clojure.el" user-emacs-directory))
; (load-file (expand-file-name "trailing-whitespace.el" user-emacs-directory))

;; Keybindings for things for which there seems to be no convenient
;; default.
(global-set-key (kbd "M--") 'xref-find-references)

;; Platform specific tweaks
(cond
 ;; On OS X the window system is called ns
 ((eq window-system 'ns)
  (load-file (expand-file-name "macos.el" user-emacs-directory)))

 ;; Windows specific tweaks
 ((eq window-system 'w32)
  (load-file (expand-file-name "windows.el" user-emacs-directory))))

;; Enable a bunch of functions that are disabled by default to avoid
;; confusion.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
