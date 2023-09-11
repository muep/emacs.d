;; Code kept over from past config, but cleaned here since it's not
;; clear if I actually need this
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
