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
