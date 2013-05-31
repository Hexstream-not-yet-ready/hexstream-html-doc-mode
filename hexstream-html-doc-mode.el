(require 'cl)

(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (add-to-invisibility-spec :tags)
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c T") 'hexstream-html-doc-cycle-tags-invisibility)
    (define-key map (kbd "C-c m") (hexstream-html-doc-make-repeat-like
                                   'hexstream-html-doc-cycle-marker))
    (define-key map (kbd "C-c DEL") 'hexstream-html-doc-backward-delete-spaces)
    (define-key map (kbd "C-c u") 'hexstream-html-doc-unfill)
    (define-key map (kbd "C-M-q") 'hexstream-html-doc-indent-sexp)
    (define-key map (kbd "C-c s") 'hexstream-html-doc-strip)
    (define-key map (kbd "C-c t") 'hexstream-html-doc-tag)
    (define-key map (kbd "C-c v") (hexstream-html-doc-make-repeat-like
                                   'hexstream-html-doc-variable))
    (define-key map (kbd "C-c i t") 'hexstream-html-doc-insert-table)
    (define-key map (kbd "C-c e")
      (lambda (region-min region-max)
        (interactive (hexstream-html-doc-suitable-region))
        (hexstream-html-doc-tag region-min region-max "pre"
                                :attributes '(("class" . "example")))
        (hexstream-html-doc-tag (point) (point) "code"
                                :attributes '(("class" . "common-lisp")))))
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
    (define-key map (kbd "C-c c l v")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "library" "variable")))
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
    (define-key map (kbd "C-c c s v")
      (hexstream-html-doc-make-code-wrapper
       '("common-lisp" "standard" "variable")))
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

(defun hexstream-html-doc-cycle-tags-invisibility ()
  (interactive)
  (cond ((invisible-p :tags)
         (remove-from-invisibility-spec :tags)
         (message "Showing tags."))
        (t
         (add-to-invisibility-spec :tags)
         (message "Hiding tags.")))
  (redraw-display))

;;; overlay categories
(setf (symbol-plist 'hexstream-html-doc-opening-tag)
      '(invisible :tags intangible :opening-tag face italic evaporate t))
(setf (symbol-plist 'hexstream-html-doc-code-tag-contents)
      '(after-string "[C]" face bold evaporate t))
(setf (symbol-plist 'hexstream-html-doc-closing-tag)
      '(invisible :tags intangible :closing-tag face italic evaporate t))


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
        (call-interactively inner-function)
        (while dont-stop
          (let ((last last-input-event)
                (event (read-event)))
            (if (eq event last)
                (call-interactively inner-function)
              (push event unread-command-events)
              (setq dont-stop nil))))))))

;; Somewhat useful to delete the indentation before an expression in <pre>,
;; before doing C-M-q. But much smarter behavior would be possible.
(defun hexstream-html-doc-backward-delete-spaces ()
  (interactive)
  (delete-region (point)
                 (save-excursion
                   (skip-chars-backward " \t")
                   (point))))

(defun hexstream-html-doc-unfill ()
  (interactive)
  (let ((fill-column 65535))
    (fill-paragraph)))

(defun hexstream-html-doc-get-slime-buffer ()
  (let ((buffer (get-buffer-create " hexstream-html-doc-slime")))
    (prog1 buffer
      (with-current-buffer buffer
        (unless (eq major-mode 'lisp-mode)
          (lisp-mode)
          (slime-mode))))))

;; Requires Slime.
(defun hexstream-html-doc-indent-sexp ()
  (interactive)
  (let ((end (save-excursion (forward-sexp) (point))))
    (let ((indented (let ((sexp (buffer-substring-no-properties (point) end)))
                      (with-current-buffer (hexstream-html-doc-get-slime-buffer)
                        (erase-buffer)
                        (insert sexp)

                        ;; Naïve fixup, part 1.
                        (goto-char 1)
                        (while (re-search-forward "&amp;" nil t)
                          (replace-match "&" t t))

                        (goto-char 1)
                        (indent-sexp)

                        ;; Naïve fixup, part 2.
                        (goto-char 1)
                        (while (re-search-forward "&" nil t)
                          (replace-match "&amp;" t t))

                        (buffer-string)))))
      (delete-region (point) end)
      (insert indented))))

(defun hexstream-html-doc-strip ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (skip-syntax-backward "^)")
      (let ((end (point)))
        (skip-syntax-backward "^(")
        (delete-region (1- (point)) end))
      (skip-syntax-forward "^(")
      (let ((start (point)))
        (skip-syntax-forward "^)")
        (delete-region start (1+ (point))))))
  nil)

(defvar hexstream-html-doc-tag-style :inline)
(defvar hexstream-html-doc-leave-point-at :inner-start)

(defun* hexstream-html-doc-wrap (region-min region-max insert-before insert-after
                                            &key (leave-point-at hexstream-html-doc-leave-point-at))
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
        (marker-position hexstream-html-doc-outer-end-marker) (point))
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
  (hexstream-html-doc-tag region-min region-max "var"
                          :leave-point-at (if (= region-min region-max)
                                              :inner-start
                                            :outer-end))
  (unless (= region-min region-max)
    (skip-syntax-forward " "))
  nil)

(defun hexstream-html-doc-insert-table ()
  (interactive)
  (let ((hexstream-html-doc-tag-style :block)
        (start (point))
        (end (copy-marker (point) t)))
    (hexstream-html-doc-tag (point) (point) "table")
    (hexstream-html-doc-tag (point) (point) "thead" :leave-point-at :outer-end)
    (hexstream-html-doc-tag (point) (point) "tbody")
    (indent-region start end)))

(defun hexstream-html-doc-overlay-tag ()
  (let ((opening-overlay (make-overlay hexstream-html-doc-outer-start-marker
                                       hexstream-html-doc-inner-start-marker
                                       nil t nil))
        (content-overlay (make-overlay hexstream-html-doc-inner-start-marker
                                       hexstream-html-doc-inner-end-marker
                                       nil nil t))
        (closing-overlay (make-overlay hexstream-html-doc-inner-end-marker
                                       hexstream-html-doc-outer-end-marker
                                       nil t nil)))
    (let ((hint "Strip read-only tag with C-c s."))
      (add-text-properties (overlay-start opening-overlay)
                           (overlay-end opening-overlay)
                           `(read-only ,hint rear-nonsticky (read-only)))
      (add-text-properties (overlay-start closing-overlay)
                           (overlay-end closing-overlay)
                           `(read-only ,hint)))
    (overlay-put opening-overlay 'category 'hexstream-html-doc-opening-tag)
    (overlay-put content-overlay 'category 'hexstream-html-doc-code-tag-contents)
    (overlay-put closing-overlay 'category 'hexstream-html-doc-closing-tag)))

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
      ;; ?( and ?) really screw up emacs' parsing...
      (when (and (string= (char-to-string (char-syntax (char-before region-min)))
                          ")")
                 (string= (char-to-string (char-syntax (char-after region-max)))
                          "(")
                 (save-excursion
                   (skip-syntax-backward "^(")
                   (looking-at-p "code\\_>")))
        (let ((start (and region-min (copy-marker region-min)))
              (end (and region-max (copy-marker region-max t))))
          (hexstream-html-doc-strip)
          (setf region-min (and (markerp start) (marker-position start))
                region-max (and (markerp end) (marker-position end)))))
      (hexstream-html-doc-tag region-min region-max "code"
                              :attributes `(("class" . ,class-attribute)))
      (hexstream-html-doc-overlay-tag))))


(defun hexstream-html-doc-suitable-region ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds
          (destructuring-bind (start . end) bounds
            (list start
                  (save-excursion
                    (goto-char end)
                    (skip-chars-backward ".")
                    (point))))
        (list (point) (point))))))
