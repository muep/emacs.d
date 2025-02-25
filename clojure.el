(defun muep-cider-eval-and-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(defun muep-cider-eval-and-test-ns ()
  (interactive)
  (cider-load-buffer)
  (cider-test-run-ns-tests nil))

(use-package cider
  :ensure t
  :defer t
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (clojure-mode . enable-paredit-mode)
         (cider-mode . rainbow-delimiters-mode)
         (cider-mode . enable-paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-repl-mode . enable-paredit-mode))
  :bind
  (:map cider-mode-map
        ("<f8> n" . muep-cider-eval-and-test-ns)
        ("<f8> t" . muep-cider-eval-and-test)))
