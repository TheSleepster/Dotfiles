  ; ;; Keybindings for brace insertion and duplication
  ; (general-define-key
  ;   "C-z" 'insert-equals-braces
  ;   "C-b" 'insert-allman-braces
  ;   "C-s" 'insert-allman-braces-semicolon
  ;   "C-c" 'insert-allman-braces-break
  ;   "C-l" 'duplicate-line)

  ; ;; Keybindings for commenting
  ; (general-define-key
  ;  :states '(normal visual)
  ;  "gc" '(:ignore t :wk "Comment")
  ;  "gcc" 'comment-line      ;; Comment out current line
  ;  "gci" 'comment-region)   ;; Comment out selected region

  ;   (general-define-key
  ;    :states 'normal
  ;    "<S-left>" 'evil-shift-left  ;; Shift left action in normal mode
  ;    "<S-right>" 'evil-shift-right)  ;; Shift right action in normal mode

  ;   (general-define-key
  ;    :states 'visual
  ;    "<S-left>" (lambda ()
  ;                  (interactive)
  ;                  (call-interactively 'evil-shift-left))  ;; Shift left in visual mode
  ;    "<S-right>" (lambda ()
  ;                  (interactive)
  ;                  (call-interactively 'evil-shift-right)))   ;; Shift right in visual mode

