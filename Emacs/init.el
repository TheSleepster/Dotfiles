(defvar elpaca-installer-version -1.7)
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
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads"))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Install Evil and related packages
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init)
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; Leader key
  (general-create-definer dt/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; Utilities
  (dt/leader-keys
   "b" '(:ignore t :wk "buffer")
   "bb" '(switch-to-buffer :wk "Switch to Buffer")
   "bi" '(ibuffer :wk "IBuffer")
   "bk" '(kill-this-buffer :wk "Kill this buffer")
   "bn" '(next-buffer :wk "Next buffer")
   "bp" '(previous-buffer :wk "Previous buffer")
   "br" '(reload-buffer :wk "Reload buffer"))

  ;; Help
  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe Function")
    "hv" '(describe-variable :wk "Describe Variable"))

  ;; Unbind the default action of C-d
  (general-define-key
   :states 'insert
   "C-d" nil)


  ;; Keybindings for brace insertion and duplication
  (general-define-key
    "C-z" 'insert-equals-braces
    "C-b" 'insert-allman-braces
    "C-s" 'insert-allman-braces-semicolon
    "C-c" 'insert-allman-braces-break
    "C-l" 'duplicate-line)

  ;; Keybindings for commenting
  (general-define-key
   :states '(normal visual)
   "gc" '(:ignore t :wk "Comment")
   "gcc" 'comment-line      ;; Comment out current line
   "gci" 'comment-region)   ;; Comment out selected region

  ;; Keybindings for commenting
  (general-define-key
   :states '(normal visual)
   "gc" '(:ignore t :wk "Comment")
   "gcc" 'comment-line      ;; Comment out current line
   "gci" 'comment-region)   ;; Comment out selected region

  ;; Custom comment insertion with TODO, NOTE, IMPORTANT
  (dt/leader-keys
   "t" '(lambda () (interactive) (insert-comment-with-prefix "TODO(") :wk "Insert TODO")
   "n" '(lambda () (interactive) (insert-comment-with-prefix "NOTE)") :wk "Insert NOTE")
   "i" '(lambda () (interactive) (insert-comment-with-prefix "IMPORTANT(") :wk "Insert IMPORTANT"))
)

(use-package glsl-mode
    :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil     ; if nil, bold is universally disabled
        doom-themes-enable-italic nil)) ; if nil, italics is universally disabled

;; Functions for brace insertion
(defun insert-equals-braces ()
  (interactive)
  (insert "= {};"))

(defun insert-allman-braces ()
  (interactive)
  (insert "{\n\n}"))

(defun insert-allman-braces-semicolon ()
  (interactive)
  (insert "{\n\n};"))

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

;;LSP
;; Configure Clangd arguments
(setq clangd-args '("--header-insertion=never" "--all-tokens"))

(defun my-eglot-setup ()
  (interactive)
   (eglot-ensure)
   ;; Set local eglot server programs for C/C++
   (setq-local eglot-server-programs
               '((c-mode . ("clangd" "--header-insertion=never" "--all-tokens"))
                 (c++-mode . ("clangd" "--header-insertion=never" "--all-tokens"))))
   ;; Set ignored capabilities for eglot, excluding signatureHelp for parameter hints
   (setq-local eglot-ignored-server-capabilities
               '(:hover :documentHighlight :codeActionProvider
                        :documentFormattingProvider :documentRangeFormattingProvider
                        :documentSymbolProvider :documentSymbolProvide :inlayHints)))

(add-hook 'eglot-managed-mode-hook 'my-eglot-setup)

(with-eval-after-load 'eglot
  ;; Custom function to toggle eglot signature help on demand
  (defun my-eglot-toggle-signature-help ()
    "Toggle eglot signature help for function parameters."
    (interactive)
    (if eldoc-mode
        (eldoc-mode -1)  ;; Hide signature help if it's active
      (eglot-signature-help))))  ;; Show signature help if it's not active

;; GLSL Configuration with Eglot
(with-eval-after-load 'eglot
  (setq eglot-stay-out-of '(flymake))
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (add-to-list 'eglot-server-programs
               '(glsl-mode . ("C:/Users/ibjal/AppData/Local/nvim-data/mason/packages/glsl_analyzer/bin/glsl_analyzer.exe" "--stdio")))
  (add-to-list 'auto-mode-alist '("\\.\\(fs\\|vs\\|vert\\|frag\\|glsl\\|comp\\|glh\\)\\'" . glsl-mode)))

;; C# Configuration with Eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("C:/Users/ibjal/AppData/Local/nvim-data/mason/packages/omnisharp/OmniSharp.exe" "--stdio"))))

 ;; C/C++ mode setup
 (add-hook 'c-mode-hook
           (lambda ()
             (eglot-ensure)
             (c-set-offset 'substatement-open 0)))

 (add-hook 'c++-mode-hook
           (lambda ()
             (eglot-ensure)))

;; Language server configurations for Zig, Rust, OdinLang, and Golang
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(odin-mode . ("ols" "--stdio")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls"))))

(setq eglot-inlay-hints-mode nil)



;CONFIG
;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;; Suppress minibuffer messages
(setq echo-keystrokes 0.1)
(setq-default minibuffer-message-timeout 0)

;; Disable displaying help at point when idle
(setq help-at-pt-display-when-idle nil)

;; Adjust company-mode settings if needed
(setq company-tooltip-align-annotations t)
(setq company-show-numbers nil)
(setq company-idle-delay 0.5)

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq compilation-directory-locked nil)
(scroll-bar-mode -1)
(setq shift-select-mode nil)
(setq enable-local-variables nil)

(setq giosupports-verbose nil)
(setq-default truncate-lines t)  ;; Enable line truncation
(setq-default word-wrap nil)  ;; Disable word wrapping

(setq sleepster-todo-file "w:/Clover/code/todo.md")

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)


(setq evil-default-cursor 'box)
(setq evil-insert-state-cursor 'box)
(setq display-line-numbers-type nil)

(setq-default indent-tabs-mode nil)  ; Disable tabs for indentation
(setq-default tab-width 4)           ; Set tab width to 4 spaces

(setq sleepster-buildscript "build.bat")
(setq compilation-directory-locked nil)

(defun silence-all-messages (&rest args)
  "A no-op function to suppress all messages."
  nil)

(defun enable-global-silence ()
  "Globally silence all `message` calls."
  (advice-add 'message :override #'silence-all-messages))

(defun disable-global-silence ()
  "Restore normal `message` behavior."
  (advice-remove 'message #'silence-all-messages))

;;Additional style stuff
(setq casey-big-fun-c-style
      '((c-electric-pound-behavior   . nil)
        (c-tab-always-indent         . t)
        (c-comment-only-line-offset  . 0)
        (c-hanging-braces-alist      . ((class-open)
                                        (class-close)
                                        (defun-open)
                                        (defun-close)
                                        (inline-open)
                                        (inline-close)
                                        (brace-list-open)
                                        (brace-list-close)
                                        (brace-list-intro)
                                        (brace-list-entry)
                                        (block-open)
                                        (block-close)
                                        (substatement-open)
                                        (statement-case-open)
                                        (class-open)))
        (c-hanging-colons-alist      . ((inher-intro)
                                        (case-label)
                                        (label)
                                        (access-label)
                                        (access-key)
                                        (member-init-intro)))
        (c-cleanup-list              . (scope-operator
                                        list-close-comma
                                        defun-close-semi))
        (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                        (label                 . -4)
                                        (access-label          . -4)
                                        (substatement-open     . 0) ; No additional indent for opening braces
                                        (statement-block-intro . +) ; 4 spaces inside the block
                                        (statement-case-intro  . +)
                                        (case-label            . 4)
                                        (block-open            . 0) ; Align opening braces
                                        (inline-open           . 0)
                                        (topmost-intro-cont    . 0)
                                        (knr-argdecl-intro     . -4)
                                        (brace-list-open       . 0)
                                        (brace-list-intro      . +)))
        (c-echo-syntactic-information-p . t)))

(defun casey-big-fun-c-hook ()
  "Apply Casey's Big Fun C++ Style to the current buffer."
  (c-add-style "BigFun" casey-big-fun-c-style t)
  (c-toggle-auto-hungry-state -1)
  (setq tab-width 4
        indent-tabs-mode nil)
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop))))

(add-hook 'c-mode-common-hook 'casey-big-fun-c-hook)

(load-library "view")
(require 'compile)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

(abbrev-mode 1)

; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)

(defun load-todo ()
  (interactive)
  (find-file sleepster-todo-file)
)
(define-key global-map (kbd "<f2>") 'load-todo)


; no screwing with my middle mouse button
(global-unset-key [mouse-2])

;; Functions for TODO Inserts
;; Define a variable for the username
(defvar sleepster-username "Sleepster"
  "The username to be inserted in comments.")

;; Function to insert a comment with a given prefix
(defun insert-comment-with-prefix (prefix)
  "Insert a comment with PREFIX, the defined username, and a closing parenthesis."
  (interactive "sPrefix: ")  ;; Prompt for prefix
  (insert (format "// %s%s): " prefix sleepster-username)))

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


(defun sleepster-insert-header-or-source-format ()
  "Insert appropriate header or source format for new files."
  (when (and buffer-file-name (eq (point-min) (point-max))) ;; Check if the buffer is empty
    (cond 
     ((string-match "\\.hin$" buffer-file-name) (sleepster-source-format))
     ((string-match "\\.cin$" buffer-file-name) (sleepster-source-format))
     ((string-match "\\.h$" buffer-file-name) (sleepster-header-format))
     ((string-match "\\.cpp$" buffer-file-name) (sleepster-source-format)))))

(add-hook 'find-file-hook 'sleepster-insert-header-or-source-format)

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

; Abbrevation expansion
(abbrev-mode 1)
(setq debug-on-error t)

; Compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
     compilation-error-regexp-alist))

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

(defun sleepster-parse-error (line)
  "Parse a line of the compilation output to extract the file, line, and column numbers."
  (let ((regex "\\([^:]+\\)(\\([0-9]+\\)):\\([^\n]+\\)"))
    (if (string-match regex line)
        (let ((file (match-string 1 line))
              (line-num (string-to-number (match-string 2 line))))
          (list file line-num))
      nil)))

(defun sleepster-next-error ()
  "Custom function to go to the next error in the *compilation* buffer."
  (interactive)
  (let* ((compilation-buffer (get-buffer "*compilation*"))
         (lines (split-string (with-current-buffer compilation-buffer (buffer-string)) "\n"))
         (current-line (line-number-at-pos))
         (current-window (selected-window)))
    ;; Loop through the compilation buffer and find the next error
    (cl-loop for line in lines
             for line-num = (sleepster-parse-error line)
             when (and line-num (> (cadr line-num) current-line))
             return (let ((file (car line-num))
                          (line-num (cadr line-num)))
                      (my-find-file file)
                      (goto-char (point-min))
                      (forward-line (1- line-num))))))

(global-unset-key (kbd "\ee"))
(global-unset-key (kbd "\ep"))
(define-key global-map "\ee" 'sleepster-next-error)
(define-key global-map "\ep" 'previous-error)

(defun my-display-compilation-in-opposite-window (buffer &optional _)
  "Display the compilation BUFFER in the opposite window."
  (let* ((current-window (selected-window))
         (opposite-window (if (window-next-sibling current-window)
                              (window-next-sibling current-window)
                            (previous-window)))
         (buffer-to-display (get-buffer buffer)))
    (when buffer-to-display
      (set-window-buffer opposite-window buffer-to-display)
      (select-window current-window)))) ;; Keep focus in the original window

(defun make-without-asking ()
  "Make the current build and return to the original buffer and position."
  (interactive)
  (let ((original-window (selected-window))
        (original-buffer (current-buffer))
        (original-point (point))
        (window-start-point (window-start)))
    (when (find-project-directory)
      (compile sleepster-buildscript)
      (select-window original-window))
    (set-window-start original-window window-start-point)
    (goto-char original-point)))

(setq display-buffer-overriding-action
      '((my-display-compilation-in-opposite-window)))

;; KEYMAPPINGS
(define-key global-map "\em" 'make-without-asking)
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
(setq auto-save-default nil)
(setq compilation-ask-about-save nil)

(add-to-list 'default-frame-alist '(font . "LiterationMono Nerd Font-11"))
(setq-default line-spacing 0.12)

(display-time)
(split-window-horizontally)
(setq auto-save-default nil
      auto-save-interval 0
      auto-save-list-file-prefix nil
      auto-save-timeout 0
      auto-show-mode t
      delete-auto-save-files nil
      delete-old-versions 'other
      imenu-auto-rescan t
      imenu-auto-rescan-maxout 500000
      kept-new-versions 5
      kept-old-versions 5
      make-backup-file-name-function 'ignore
      make-backup-files nil
      mouse-wheel-follow-mouse nil
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(15)
      version-control nil)

;; If you use org' and don't want your org files in the default location below,
;; change org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4337503020251b87200e428a1219cdf89d42fba08bc0a1d8ab031164c7925ea2" "21d883fccc7cd1556fbc7b37d10b709189b316be4317b6d4028b335d8827d541" "4764de4e8898fafa22abf3ebe8fe71d6cd528c45f694e32684afea0b825e5ae4" "417b2e4625b6bccb49c6d0714c8d13af1a27f62102ec6d56b538d696fc5ebf19" "4c4c36513edb1edd045f8170c46563bfbb8a2bfa3a80c8c478bdde204313e8b6" "31014fae0ca149e8bbffe40826f8f5952fdb91ea534914622d614b2219e04eaf" "dc7b0ef1298429908ea0a56a9b455d952a5d54b7775e5a7809e5fa1c8e8d8df8" "01633566bc8bfca5421d3a9ad7b7d91cb10743437c2996796d83a8bcc146c85a" "9a870ed55018161c9f022bf47e0e852078ad97c8021a1a9130af9cde1880bfa4" "dca64882039075757807f5cead3cee7a9704223fab1641a9f1b7982bdbb5a0e2" "058ed73311aaaf42e1da18f6aae2ce0a7fd6e37a819064e54f423369f548359a" "b95e452d5eb81406a3f1f61f9df9c2c6ff2d5eddba9d712fcd0f640e0d7707da" "16ebbe9a60555c0f546f58469a31f2312cbd9afe759901ba0ab08fcedc8030b7" "dc0af05ccfb5fc01cf4b7d9c1b63f652f78e3de844e9a7d8e74ecad1f89c001f" "fa362af0e2ae1bda1bab47bc4f15ede63884a2966394d272839d0143a948ec5a" "a5e8a918a21f1d67110b4c2e819b60cc2de7e49b79a80f483a1923e2c74d04d5" "f6bdf7cc215cbd95e09a66ec0511e0932954eadab4c60189dd82370da3f1b3fa" "80de716bfeec860f43d35d302169385bd0698bfb06fd3c16b5378ef5e3e52a24" "3d6e1dd2683f93b0b68d2672b6d922c817817a3f33e1aa5821216cdf6870c24b" "d143a4dd9292f87b377e87c77f3459d15aa5fb56a670edd2873bd812d95dfa6c" "f03d5bc29c7b8711f8a18cc92dcc2b59b90dcac44e4997daaa4db1ae7d1b419b" "4a892742bd6f8ac795d14a72461ce49bc7eb100583024b0da9b43d79884b7c45" "6e165f5225ce7ce5ca3f2dc8adf827ff8565704bfe625afe4a3cb666a166efd9" "a819fa2e49c3307ec8a8b374f09275ef35b5a47d4fa88b76eba6df7d86bcf70c" "1107071694e48770dfaeb2042b5d2b300efa0a01bfdfe8a9526347fe6f2cc698" "4d12469f94f29f44958a3173a74985f1b6aa383f933a49735d07c3304d77c810" "400fd3e8877904b0cc738d0fded98cdbda263639a007293645f31806895eba9e" "ac4ab3921322aaa6aec49d1268337ec28f88c9ee49fa9cb284d145388fb928a8" "f33b5dfb5c5fb99b5a90feab9158cadc2588c6840211b995622a35419c450b04" "545a268abdb70a28a299242bb79daf7cf1f088ddcbe9518c9d754a6f6159feb6" "42265cac74d3656d9d0b3185d422f8bdaa0f798d842c0a0c2f0786ea387dbe7e" "b6e908ac2a3b9c8a635b36341f19eff119823fea947a0a645bedf77e17e273b8" "3d39093437469a0ae165c1813d454351b16e4534473f62bc6e3df41bb00ae558" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'handmade)
;;(load-theme 'doom-material-dark)
;;(load-theme 'doom-miraware)
;;(load-theme 'doom-nord)
;;(load-theme 'doom-wilmersdorf)



(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-for-tab-command)
(define-key global-map "\C-y" 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
(define-key global-map "	" 'indent-region)

(add-to-list 'default-frame-alist '(font . "LiterationMono Nerd Font-11"))
(set-face-attribute 'default t :font "LiterationMono Nerd Font Propo-11")
(defun highlight-function-calls ()
  "Highlight function calls in C/C++ modes."
  (font-lock-add-keywords
   nil
   '(("\\<\\(\\w+\\)\\s-*(" 1 'font-lock-function-call-face))))

;; Hook into C/C++ modes
(add-hook 'c-mode-common-hook 'highlight-function-calls)

(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (maximize-frame)
  (enable-global-silence)
)

(add-hook 'window-setup-hook 'post-load-stuff t)
