(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c t") 'hexstream-html-doc-tag)
    (define-key map (kbd "C-c v") 'hexstream-html-doc-variable)))

(defun hexstream-html-doc-wrap (region-min region-max insert-before insert-after)
  (interactive (hexstream-html-doc-suitable-region))
  (cond (region-min
         (save-excursion
           (goto-char region-max)
           (insert insert-after)
           (goto-char region-min)
           (insert insert-before)))
        ;; Todo: smarter handling
        (t (hexstream-html-doc-wrap (point) (point) insert-before insert-after)
           (goto-char (+ (point) (length insert-before))))))


(defun hexstream-html-doc-tag (name)
  (interactive "*@sInsert tag: ")
  (let (region-min region-max)
    ;; I don't know if it's possible to mix an interactive string spec and code...
    (let ((region (hexstream-html-doc-suitable-region)))
      (setq region-min (first region)
            region-max (second region)))
    (hexstream-html-doc-wrap region-min region-max
                             (concat  "<" name ">") (concat  "</" name ">"))))

(defun hexstream-html-doc-variable (region-min region-max)
  (interactive (hexstream-html-doc-suitable-region))
  (hexstream-html-doc-tag "var"))


(defun hexstream-html-doc-suitable-region ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (if (not (find (char-syntax (char-after)) "w_"))
        (list nil nil)
      (list (save-excursion
              (skip-syntax-backward "w_")
              (point))
            (save-excursion
              (skip-syntax-forward "w_")
              (point))))))
