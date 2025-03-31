(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode)
  :ensure t)
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init)
  :ensure t)
(use-package general
  :config
  (general-evil-setup)
  ;; Leader key
  (general-create-definer dt/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "M-SPC")

  ;; Utilities
  (dt/leader-keys
    "gc" '(comment-line :wk "Comment Lines"))

  (dt/leader-keys
   "b" '(:ignore t :wk "buffer")
   "bb" '(switch-to-buffer :wk "Switch to Buffer")
   "bi" '(ibuffer :wk "IBuffer")
   "bk" '(kill-this-buffer :wk "Kill this buffer")
   "bn" '(next-buffer :wk "Next buffer")
   "bp" '(previous-buffer :wk "Previous buffer")
   "br" '(reload-buffer :wk "Reload buffer"))

  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe Function")
    "hv" '(describe-variable :wk "describe Variable"))
   ;; Unbind the default action of C-d
   (general-define-key
   :states 'insert
    "C-d" nil)

  ;; Keybindings
  (general-define-key
    "C-z" 'insert-equals-braces
    "C-b" 'insert-allman-braces
    "C-s" 'insert-allman-braces-semicolon
    "C-c" 'insert-allman-braces-break
    "C-d" 'duplicate-line)
  :ensure t
 )

;; Function to insert "= {};"
(defun insert-equals-braces ()
  (interactive)
  (insert "= {};"))

;; Function to insert "{ }" in Allman Style
(defun insert-allman-braces ()
  (interactive)
  (insert "{\n\n}"))

;; Function to insert "{ };" in Allman Style
(defun insert-allman-braces-semicolon ()
  (interactive)
  (insert "{\n\n};"))

;; Function to insert "{ }break;" in Allman Style
(defun insert-allman-braces-break ()
  (interactive)
  (insert "{\n\n}break;"))

(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (save-excursion
    (let ((line (thing-at-point 'line t)))
      (move-beginning-of-line 1)
      (insert line))))



;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))



;; CONFIG

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq compilation-directory-locked nil)
(scroll-bar-mode -1)
(setq shift-select-mode nil)
(setq enable-local-variables nil)

(setq giosupports-verbose nil)
(setq-default truncate-lines t)  ;; Enable line truncation
(setq-default word-wrap nil)  ;; Disable word wrapping

(setq sleepster-todo-file "w:/Clover/code/todo.txt")

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)


(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")


(setq evil-default-cursor 'box)
(setq evil-insert-state-cursor 'box)
(setq display-line-numbers-type nil)
(setq tab-width 4
      indent-tabs-mode nil)

(setq sleepster-buildscript "build.bat")
(setq compilation-directory-locked nil)

; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)

(add-to-list 'default-frame-alist '(font . "LiterationMono Nerd Font Propo-11"))
(setq-default line-spacing 0.12)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'compile)
(abbrev-mode 1)

(defun load-todo ()
  (interactive)
  (find-file sleepster-todo-file)
)
(define-key global-map "\et" 'load-todo)


; no screwing with my middle mouse button
(global-unset-key [mouse-2])

;; Function to insert current time of day and file name into the header
(defun sleepster-header-format ()
  (interactive)
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (insert "#if !defined(")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H)\n")
  (insert "/* ========================================================================\n")
  (insert (format "   $File: %s $\n" (file-name-nondirectory buffer-file-name)))
  (insert (format "   $Date: %s $\n" (format-time-string "%a, %d %b %y: %I:%M%p")))
  (insert "   $Revision: $\n")
  (insert "   $Creator: Justin Lewis $\n")
  (insert "   ======================================================================== */\n\n")
  (insert "#define ")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H\n")
  (insert "#endif"))

;; Function to insert current time of day and file name into the source
(defun sleepster-source-format ()
  (interactive)
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (insert "/* ========================================================================\n")
  (insert (format "   $File: %s $\n" (file-name-nondirectory buffer-file-name)))
  (insert (format "   $Date: %s $\n" (format-time-string "%a, %d %b %y: %I:%M%p")))
  (insert "   $Revision: $\n")
  (insert "   $Creator: Justin Lewis $\n")
  (insert "   ======================================================================== */\n"))

 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
	 (font-lock-add-keywords
	  mode
	  '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
	fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)


(when buffer-file-name
  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "\\.hin$" buffer-file-name) (sleepster-source-format))
        ((string-match "\\.cin$" buffer-file-name) (sleepster-source-format))
        ((string-match "\\.h$" buffer-file-name) (sleepster-header-format))
        ((string-match "\\.cpp$" buffer-file-name) (sleepster-source-format))))



;; Ensure the same file opens only in one window
(defun my-find-file (filename)
  "Open a file, ensuring it's opened in only one window."
  (interactive "FOpen file: ")
  (let ((buf (find-file-noselect filename)))
    (switch-to-buffer buf)
    (when (one-window-p)
      (display-buffer buf))))

;; Bind the function to a key or use it as a default
(global-set-key (kbd "C-x C-f") 'my-find-file)



;; SILLY FUN Find File Function
  (defun sleepster-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	   (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
       (error "Unable to find a corresponding file")))
  (defun sleepster-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (sleepster-find-corresponding-file)
    (other-window -1))


;; SILLY FUN COMPILATION
  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'sleepester-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(sleepster-devenv
   "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
    2 3 nil (4)))

(setq compilation-context-lines 0)
(setq compilation-window-height nil)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
     compilation-error-regexp-alist))

(defun sleepster-big-fun-compilation-hook ()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil)
)

(add-hook 'compilation-mode-hook 'sleepster-big-fun-compilation-hook)

;; (defun setup-msvc-env ()
;;   (interactive)
;;   (shell-command "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\Auxiliary\\Build\\vcvarsall.bat x64"))

;; (add-hook 'compilation-mode-hook 'setup-msvc-env)

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p sleepster-buildscript) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun my-display-compilation-in-opposite-window (buffer &optional _)
  "Display the compilation BUFFER in the opposite window."
  (let* ((left-window (window-at 0 0))
         (right-window (next-window left-window)))
    ;; Check if the current window is the left or right one
    (if (eq (selected-window) left-window)
        (set-window-buffer right-window buffer)
      (set-window-buffer left-window buffer))
    (select-window (selected-window))))  ;; Keep focus in the original window


(setq display-buffer-overriding-action
      '((my-display-compilation-in-opposite-window)))

(defun make-without-asking ()
  "Make the current build and return to the original position."
  (interactive)
  (let ((original-buffer (current-buffer))
        (original-point (point)))
    (when (find-project-directory)
      (compile sleepster-buildscript))
    (let ((opposite-window (next-window)))
      (select-window opposite-window)
      ;; Wait for compilation to finish (if desired)
      (with-current-buffer original-buffer
        (goto-char original-point)  ;; Return to original position
        (select-window (previous-window))))))  ;; Switch back to the original window

;; KEYMAPPINGS
(define-key global-map "\em" 'make-without-asking)
(define-key global-map "\en" 'next-error)
(define-key global-map "\ep" 'previous-error)
  (define-key global-map [f12] 'sleepster-find-corresponding-file)
  (define-key global-map [M-f12] 'sleepster-find-corresponding-file-other-window)

(defun maximize-frame()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488)
)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(display-time)
(split-window-horizontally)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(custom-set-variables
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(version-control nil)
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3d39093437469a0ae165c1813d454351b16e4534473f62bc6e3df41bb00ae558" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'handmade)

(defun post-load-stuff ()
  (maximize-frame)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
)
(add-hook 'window-setup-hook 'post-load-stuff t)
