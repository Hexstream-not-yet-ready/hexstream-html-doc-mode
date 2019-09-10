(require 'cl)

(define-derived-mode hexstream-html-doc-mode html-mode "Hexstream HTML Documentation"
  (let ((map hexstream-html-doc-mode-map))
    (define-key map (kbd "C-c t") 'hexstream-html-doc-tag)
    (define-key map (kbd "C-c a") 'hexstream-html-doc-link)
    (define-key map (kbd "C-c v") 'hexstream-html-doc-variable)))


(defvar hexstream-html-doc-outer-start-marker (let ((m (make-marker)))
                                                (prog1 m
                                                  (set-marker-insertion-type m t))))
(defvar hexstream-html-doc-inner-start-marker (make-marker))
(defvar hexstream-html-doc-inner-end-marker (let ((m (make-marker)))
                                              (prog1 m
                                                (set-marker-insertion-type m t))))
(defvar hexstream-html-doc-outer-end-marker (make-marker))

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
    (indent-for-tab-command)
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

(defun hexstream-html-doc-link (region-min region-max)
  (interactive (hexstream-html-doc-suitable-region))
  (hexstream-html-doc-tag region-min region-max "a"
                          :leave-point-at :inner-start)
  (backward-char)
  (insert " href=\"\"")
  (backward-char)
  nil)

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
