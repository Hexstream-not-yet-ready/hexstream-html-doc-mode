(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c w v") 'hexstream-html-doc-wrap-variable)
    (define-key map (kbd "C-c i t") 'hexstream-html-doc-insert-tag)))


(defun hexstream-html-doc-wrap-variable ()
  (interactive "*@")
  (save-excursion
    (backward-sexp)
    (insert "<var>")
    (forward-sexp)
    (insert "</var>")))

(defun hexstream-html-doc-insert-tag (name)
  (interactive "*@sInsert tag: ")
  (insert "<" name ">")
  (save-excursion
    (insert "</" name ">")))
