(require 'cl)

(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c m") (hexstream-html-doc-make-repeat-like
                                   'hexstream-html-doc-cycle-marker))
    (define-key map (kbd "C-c u") 'hexstream-html-doc-unfill)
    (define-key map (kbd "C-c s") 'hexstream-html-doc-strip)
    (define-key map (kbd "C-c t") 'hexstream-html-doc-tag)
    (define-key map (kbd "C-c v") 'hexstream-html-doc-variable)
    (define-key map (kbd "C-c i t") 'hexstream-html-doc-insert-table)
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
    (define-key map (kbd "C-c c l C")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "class")))
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
       '("common-lisp" "library" "glossary")))

    ;; Stupid copy-paste of above. Refactor later.
    ;; A better thing would be one command that looks up the kind from a database,
    ;; and queries the user if there are multiple kinds for that name.
    ;; Even better would be Slime integration,
    ;; for automatically marking up an entire expression.
    (define-key map (kbd "C-c c s p")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "package")))
    (define-key map (kbd "C-c c s f")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "function" "operator")))
    (define-key map (kbd "C-c c s m")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "macro" "operator")))
    (define-key map (kbd "C-c c s s")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "special-operator" "operator")))
    (define-key map (kbd "C-c c s C")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "class")))
    (define-key map (kbd "C-c c s t")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "type-specifier")))
    (define-key map (kbd "C-c c s r")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "marker")))
    (define-key map (kbd "C-c c s k")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "constant")))
    (define-key map (kbd "C-c c s g")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "glossary")))))

(defvar hexstream-html-doc-outer-start-marker (let ((m (make-marker)))
                                                (prog1 m
                                                  (set-marker-insertion-type m t))))
(defvar hexstream-html-doc-inner-start-marker (make-marker))
(defvar hexstream-html-doc-inner-end-marker (let ((m (make-marker)))
                                              (prog1 m
                                                (set-marker-insertion-type m t))))
(defvar hexstream-html-doc-outer-end-marker (make-marker))

;; Currently won't cycle correctly if the next marker is "on top of" the one we're at.
(defun hexstream-html-doc-cycle-marker ()
  (interactive)
  (let ((p (point)))
    ;; Implementation could be more clever...
    (setf (point)
          (cond ((= p hexstream-html-doc-outer-start-marker)
                 (message "<tag>|content</tag>")
                 hexstream-html-doc-inner-start-marker)
                ((= p hexstream-html-doc-inner-start-marker)
                 (message "<tag>content|</tag>")
                 hexstream-html-doc-inner-end-marker)
                ((= p hexstream-html-doc-inner-end-marker)
                 (message "<tag>content</tag>|")
                 hexstream-html-doc-outer-end-marker)
                (t (message "|<tag>content</tag>")
                   hexstream-html-doc-outer-start-marker)))))

;; C-x z (repeat)-like trick from:
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2006-04/msg00508.html
(defun hexstream-html-doc-make-repeat-like (inner-function)
  (lexical-let ((inner-function inner-function))
    (lambda ()
      (interactive)
      (let ((dont-stop t))
        (funcall inner-function)
        (while dont-stop
          (let ((last last-input-event)
                (event (read-event)))
            (if (eq event last)
                (funcall inner-function)
              (push event unread-command-events)
              (setq dont-stop nil))))))))

(defun hexstream-html-doc-unfill ()
  (interactive)
  (let ((fill-column 65535))
    (fill-paragraph)))

(defun hexstream-html-doc-strip ()
  (interactive)
  (save-excursion
    (skip-syntax-backward "^)")
    (let ((end (point)))
      (skip-syntax-backward "^(")
      (delete-region (1- (point)) end))
    (skip-syntax-forward "^(")
    (let ((start (point)))
      (skip-syntax-forward "^)")
      (delete-region start (1+ (point)))))
  nil)

(defvar hexstream-html-doc-tag-style :inline)
(defvar hexstream-html-doc-leave-point-at :inner-start)

(defun* hexstream-html-doc-wrap (region-min region-max insert-before insert-after
                                            &key (leave-point-at hexstream-html-doc-leave-point-at))
  (cond (region-min
         (when (eq region-min t)
           (setf region-min (marker-position hexstream-html-doc-outer-start-marker)))
         (when (eq region-max t)
           (setf region-max (marker-position hexstream-html-doc-outer-end-marker)))
         (goto-char region-min)
         (setf (marker-position hexstream-html-doc-inner-end-marker)
               region-max)
         (insert insert-before)
         (setf (marker-position hexstream-html-doc-outer-start-marker) region-min
               (marker-position hexstream-html-doc-inner-start-marker) (point))
         (goto-char hexstream-html-doc-inner-end-marker)
         (insert insert-after)
         (setf (marker-position hexstream-html-doc-inner-end-marker) (- (point) (length insert-after))
               (marker-position hexstream-html-doc-outer-end-marker) (point)))
        ;; Todo: smarter handling
        (t (hexstream-html-doc-wrap (point) (point) insert-before insert-after)))
  (setf (point) (ecase leave-point-at
                  (:outer-start hexstream-html-doc-outer-start-marker)
                  (:inner-start hexstream-html-doc-inner-start-marker)
                  (:inner-end hexstream-html-doc-inner-end-marker)
                  (:outer-end hexstream-html-doc-outer-end-marker)))
  nil)


(defun* hexstream-html-doc-tag (region-min region-max name
                                           &key attributes
                                           (style hexstream-html-doc-tag-style)
                                           (leave-point-at hexstream-html-doc-leave-point-at))
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
   (concat  "</" name ">")
   :leave-point-at leave-point-at)
  (when (eq style :block)
    (newline 2)
    (previous-line)
    (indent-for-tab-command)))

(defun hexstream-html-doc-variable (region-min region-max)
  (interactive (hexstream-html-doc-suitable-region))
  (downcase-region region-min region-max)
  (hexstream-html-doc-tag region-min region-max "var"))

(defun hexstream-html-doc-insert-table ()
  (interactive)
  (let ((hexstream-html-doc-tag-style :block)
        (start (point))
        (end (copy-marker (point) t)))
    (hexstream-html-doc-tag nil nil "table")
    (hexstream-html-doc-tag nil nil "thead" :leave-point-at :outer-end)
    (hexstream-html-doc-tag nil nil "tbody")
    (indent-region start end)))

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
              (skip-chars-backward ".")
              (point))))))
