;; Custom color theme
;;
;; Initially created with some emacs function but later modified by
;; hand.

(deftheme muep
  "Created 2012-08-31.")

(let ((backgnd "gray25")
      (normal "light gray")
      (dimmed "gray60")
      (little "#10679e")
      (somewhat "#0084c8")
      (bright "#00bbff"))

  (custom-theme-set-faces
   'muep
   `(default
      ((t (:background ,backgnd :foreground ,normal))))

   '(cursor
     ((((background light)) (:background "black"))
      (((background dark))  (:background "white"))))

   '(fixed-pitch
     ((t (:family "Monospace"))))

   '(variable-pitch
     ((t (:family "Sans Serif"))))

   ;; TODO where is this used?
   '(escape-glyph
     ((((background dark)) (:foreground "cyan"))
      (((type pc))         (:foreground "magenta"))
      (t                   (:foreground "brown"))))

   `(minibuffer-prompt
     ((t                   (:foreground ,bright))))

   ;; TODO where is this used?
   '(highlight
     ((((class color)
        (min-colors 88)
        (background light)) (:background "darkseagreen2"))
      (((class color)
        (min-colors 88)
        (background dark))  (:background "darkolivegreen"))
      (((class color)
        (min-colors 16)
        (background light)) (:background "darkseagreen2"))
      (((class color)
        (min-colors 16)
        (background dark)) (:background "darkolivegreen"))
      (((class color)
        (min-colors 8))    (:foreground "black" :background "green"))
      (t                   (:inverse-video t))))

   '(region
     ((t  (:background "dark slate blue"))))

   ;; TODO where is this used?
   '(shadow
     ((((class color grayscale)
        (min-colors 88)
        (background light))
              (:foreground "grey50"))
             (((class color grayscale)
               (min-colors 88)
               (background dark))
              (:foreground "grey70"))
             (((class color)
               (min-colors 8)
               (background light))
              (:foreground "green"))
             (((class color)
               (min-colors 8)
               (background dark))
              (:foreground "yellow"))))

   ;; TODO where is this used?
   '(secondary-selection
     ((((class color) (min-colors 88)
                            (background light))
       (:background "yellow1"))
      (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4"))
      (((class color) (min-colors 16) (background light)) (:background "yellow"))
      (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4"))
      (((class color) (min-colors 8)) (:foreground "black" :background "cyan"))
      (t (:inverse-video t))))

   '(trailing-whitespace
     ((((class color) (background light)) (:background "red1"))
      (((class color) (background dark)) (:background "red1"))
      (t (:inverse-video t))))

   `(font-lock-builtin-face
     ((t (:foreground ,bright))))

   '(font-lock-comment-delimiter-face
     ((default (:inherit (font-lock-comment-face)))))

   `(font-lock-comment-face
     ((t (:foreground ,dimmed :slant italic))))

   `(font-lock-constant-face
     ((t (:foreground ,normal))))

   '(font-lock-doc-face
     ((t (:inherit (font-lock-string-face)))))

   `(font-lock-function-name-face
     ((t (:foreground ,normal ))))

   `(font-lock-keyword-face
     ((t (:foreground ,bright))))

   ;; TODO where is this used?
   '(font-lock-negation-char-face
     ((t nil)))

   '(font-lock-preprocessor-face
     ((t (:inherit (font-lock-builtin-face)))))

   '(font-lock-regexp-grouping-backslash
     ((t (:inherit (bold)))))

   '(font-lock-regexp-grouping-construct
     ((t (:inherit (bold)))))

   `(font-lock-string-face
     ((t (:foreground ,somewhat))))

   `(font-lock-type-face
     ((t (:foreground ,bright))))

   `(font-lock-variable-name-face
     ((t (:foreground ,normal))))

   '(font-lock-warning-face
     ((t (:inherit error))))

   '(button ((t (:inherit link))))

   `(link ((t (:foreground ,bright))))

   '(link-visited ((t (:inherit link :foreground "magenta4"))))

   '(fringe ((t (:background "gray80"))))

   '(header-line ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box nil))))

   '(tooltip ((t (:inherit variable-pitch :background "lightyellow" :foreground "black"))))

   '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))

   '(mode-line-buffer-id ((t (:weight bold))))

   '(mode-line-emphasis ((t (:weight bold))))

   '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))

   '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))

   `(isearch ((t (:background ,bright :foreground "black"))))

   '(isearch-fail ((t (:background "RosyBrown1"))))

   `(lazy-highlight ((t (:background ,somewhat :foreground "grey85"))))

   '(match ((t (:background "yellow1"))))

   '(next-error ((t (:inherit region))))



   '(query-replace ((t (:inherit isearch))))

   ;; spec file colors
   `(rpm-spec-doc-face     ((t (:foreground ,normal))))
   `(rpm-spec-dir-face     ((t (:foreground ,somewhat))))
   `(rpm-spec-ghost-face   ((t (:foreground ,bright))))
   `(rpm-spec-macro-face   ((t (:foreground ,bright))))
   `(rpm-spec-package-face ((t (:foreground ,normal))))
   `(rpm-spec-section-face ((t (:foreground ,bright))))
   `(rpm-spec-tag-face     ((t (:foreground ,somewhat))))
   `(rpm-spec-var-face     ((t (:foreground ,somewhat))))

   `(org-level-1 ((t (:foreground ,bright))))
   `(org-level-2 ((t (:foreground ,somewhat))))
   `(org-level-3 ((t (:foreground ,somewhat))))
   `(org-level-4 ((t (:foreground ,somewhat))))
   `(org-level-5 ((t (:foreground ,somewhat))))
   `(org-level-6 ((t (:foreground ,somewhat))))
   `(org-level-7 ((t (:foreground ,somewhat))))
   `(org-level-8 ((t (:foreground ,somewhat))))
   ))

(provide-theme 'muep)
