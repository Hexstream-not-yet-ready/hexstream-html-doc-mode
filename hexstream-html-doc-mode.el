(require 'cl)

(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c t") 'hexstream-html-doc-tag)
    (define-key map (kbd "C-c v") 'hexstream-html-doc-variable)
    (define-key map (kbd "C-c e")
      (lambda (region-min region-max)
        (interactive (hexstream-html-doc-suitable-region))
        (hexstream-html-doc-tag region-min region-max "code"
                                :attributes '(("class" . "common-lisp")))
        (hexstream-html-doc-tag t t "pre"
                                :attributes '(("class" . "example")))))
    (define-key map (kbd "C-c c c")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp")))
    (define-key map (kbd "C-c c l l")
      (lambda (region-min region-max)
        (interactive (hexstream-html-doc-suitable-region))
        (hexstream-html-doc-tag region-min region-max "cite"
                                :attributes '(("class" . "common-lisp library")))))
    (define-key map (kbd "C-c c l p")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "package")))
    (define-key map (kbd "C-c c l f")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "function" "operator")))
    (define-key map (kbd "C-c c l m")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "macro" "operator")))
    (define-key map (kbd "C-c c l s")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "special-operator" "operator")))
    (define-key map (kbd "C-c c l t")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "type-specifier")))
    (define-key map (kbd "C-c c l r")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "marker")))
    (define-key map (kbd "C-c c l k")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "constant")))
    (define-key map (kbd "C-c c l g")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "glossary")))))

(defvar hexstream-html-doc-start-marker (make-marker))
(defvar hexstream-html-doc-end-marker (make-marker))

(defun hexstream-html-doc-wrap (region-min region-max insert-before insert-after)
  (cond (region-min
         (when (eq region-min t)
           (setf region-min (marker-position hexstream-html-doc-start-marker)))
         (when (eq region-max t)
           (setf region-max (marker-position hexstream-html-doc-end-marker)))
         (move-marker hexstream-html-doc-start-marker region-min)
         (move-marker hexstream-html-doc-end-marker region-max)
         (save-excursion
           (goto-char region-max)
           (insert insert-after)
           (goto-char region-min)
           (insert insert-before)))
        ;; Todo: smarter handling
        (t (hexstream-html-doc-wrap (point) (point) insert-before insert-after)
           (goto-char (+ (point) (length insert-before))))))


(defun* hexstream-html-doc-tag (region-min region-max name &key attributes)
  (interactive (let ((region (hexstream-html-doc-suitable-region)))
                 (list (first region) (second region)
                       (call-interactively (lambda (name)
                                             (interactive "*@sInsert tag: ")
                                             name)))))
  (hexstream-html-doc-wrap
   region-min region-max
   (with-output-to-string
     (princ "<")
     (princ name)
     (when attributes
       (dolist (attribute attributes)
         (princ " ")
         (princ (car attribute))
         (princ "=")
         (prin1 (cdr attribute))))
     (princ ">"))
   (concat  "</" name ">")))

(defun hexstream-html-doc-variable (region-min region-max)
  (interactive (hexstream-html-doc-suitable-region))
  (downcase-region region-min region-max)
  (hexstream-html-doc-tag region-min region-max "var"))

(defun* hexstream-html-doc-make-code-wrapper (css-class-list &key (downcasep t))
  (lexical-let ((class-attribute (with-output-to-string
                                   (princ (first css-class-list))
                                   (dolist (class (cdr css-class-list))
                                     (princ " ")
                                     (princ class))))
                (downcasep downcasep))
    (lambda (region-min region-max)
      (interactive (hexstream-html-doc-suitable-region))
      (when downcasep (downcase-region region-min region-max))
      (hexstream-html-doc-tag region-min region-max "code"
                              :attributes `(("class" . ,class-attribute))))))


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
