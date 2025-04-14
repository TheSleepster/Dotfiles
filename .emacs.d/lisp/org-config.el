;; ORG MODE
(use-package org-modern
  :ensure t
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

(use-package org)
(use-package org-bullets)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (R . t)))

(org-indent-mode)

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 120 :weight normal))))
 '(fixed-pitch ((t ( :family "Fira Code Retina" :height 120)))))

(add-hook 'org-mode-hook 'visual-line-mode)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; FUNCTIONS
(defun fill-region-paragraphs (b e &optional justify)
  "Fill region between b and e like `fill-paragraph' for each paragraph in region
instead of `fill-region' which is implied by the original version of `fill-paragraph'.
Justify when called with prefix arg."
  (interactive "r\nP")
  (save-excursion
    (goto-char b)
    (while (< (point) e)
      (fill-paragraph justify)
      (forward-paragraph)
      )))
(global-set-key (kbd "M-f") 'fill-region-paragraphs)

(defun org-adjust-region (b e)
  "Re-adjust stuff in region according to the preceeding stuff."
  (interactive "r") ;; current region
  (save-excursion
    (let ((e (set-marker (make-marker) e))
      (_indent (lambda ()
             (insert ?\n)
             (backward-char)
             (org-indent-line)
             (delete-char 1)))
      last-item-pos)
      (goto-char b)
      (beginning-of-line)
      (while (< (point) e)
    (indent-line-to 0)
    (cond
     ((looking-at "[[:space:]]*$")) ;; ignore empty lines
     ((org-at-heading-p)) ;; just leave the zero-indent
     ((org-at-item-p)
      (funcall _indent)
      (let ((struct (org-list-struct))
        (mark-active nil))
        (ignore-errors (org-list-indent-item-generic -1 t struct)))
      (setq last-item-pos (point))
      (when current-prefix-arg
        (fill-paragraph)))
     ((org-at-block-p)
      (funcall _indent)
      (goto-char (plist-get (cadr (org-element-special-block-parser e nil)) :contents-end))
      (org-indent-line))
     (t (funcall _indent)))
    (forward-line))
      (when last-item-pos
    (goto-char last-item-pos)
    (org-list-repair)
    ))))
(define-key org-mode-map (kbd "C-=") 'org-adjust-region)



