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

(defun muep-c-mode-keys ()
    (define-key
      c-mode-base-map
      (kbd "<backtab>")
      (lambda ()
        (interactive)
        (c-indent-line-or-region -1))))

(add-hook 'c-initialization-hook 'muep-c-mode-keys)
(add-hook 'c-mode-common-hook 'enable-ff-find-other-file)

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
