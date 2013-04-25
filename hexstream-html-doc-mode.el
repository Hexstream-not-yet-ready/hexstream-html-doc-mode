(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c v") 'hexstream-html-doc-variable)
    (define-key map (kbd "C-c i t") 'hexstream-html-doc-insert-tag)))

;; Doesn't yet work...
(defun hexstream-html-doc-variable (region-min region-max)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     ;; Won't work as intended if point is exactly at the beginning or end of s-expression.
     ;; I don't yet know how to easily determine if point is on a s-expression
     ;; and if so, what its boundaries are.
     (list (save-excursion
             (backward-sexp)
             (point))
           (save-excursion
             (forward-sexp)
             (point)))))
  (save-excursion
    (goto-char region-min)
    (insert "<var>")
    (goto-char region-max)
    (insert "</var>")))

(defun hexstream-html-doc-insert-tag (name)
  (interactive "*@sInsert tag: ")
  (insert "<" name ">")
  (save-excursion
    (insert "</" name ">")))