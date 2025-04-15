;; I abused GROK to make this. It generated it from my old NVIM colorcheme.
;; turns out AI can be useful...

(deftheme nvim_dark "Nvim dark theme for Emacs 29")

(custom-theme-set-faces
 'nvim_dark
 ;; Default face (foreground and background)
 '(default ((t (:foreground "#dab98f" :background "#101010"))))
 ;; Cursor
 '(cursor ((t (:background "#00fc5f"))))
 ;; Region (visual selection, including Evil-mode visual line)
 '(region ((t (:background "#2f363d"))))                      ; Darker grey, confirmed perfect
 ;; Highlight current line
 '(hl-line ((t (:background "#191970"))))
 ;; Fringe
 '(fringe ((t (:background "#161616"))))
 ;; Mode line (active)
 '(mode-line ((t (:foreground "#dab98f" :background "#5c6370"))))
 ;; Mode line (inactive)
 '(mode-line-inactive ((t (:foreground "#5c6370" :background "#161616"))))
 ;; Minibuffer prompt
 '(minibuffer-prompt ((t (:foreground "#CD950C"))))
 ;; Syntax highlighting faces
 '(font-lock-keyword-face ((t (:foreground "#CD950C"))))       ; Keywords: if, else, while, for, case, break, struct
 '(font-lock-type-face ((t (:foreground "#CD950C"))))          ; Types: int, float, struct names, VOID, HANDLE, DWORD
 '(font-lock-string-face ((t (:foreground "#f4C430"))))        ; Strings
 '(font-lock-function-name-face ((t (:foreground "#ff5900")))) ; Functions
 '(font-lock-variable-name-face ((t (:foreground "#dab98f")))) ; Variables: Region1, DestSample
 '(font-lock-constant-face ((t (:foreground "#00FF00"))))      ; Constants: INVALID_HANDLE_VALUE
 '(font-lock-preprocessor-face ((t (:foreground "#ff7a7b"))))  ; Preprocessor directives
 '(font-lock-comment-face ((t (:foreground "#5c6370"))))       ; Comments
 '(font-lock-doc-face ((t (:foreground "#5c6370"))))           ; Doc strings
 '(font-lock-builtin-face ((t (:foreground "#ff5900"))))       ; Built-ins (e.g., sizeof)
 ;; Special characters and escapes
 '(escape-glyph ((t (:foreground "#6b8e23"))))                ; Escape sequences
 '(special-char ((t (:foreground "#f4C430"))))                ; Special characters
 ;; Flymake diagnostics
 '(flymake-error ((t (:underline (:color "#fc0505" :style wave)))))
 '(flymake-warning ((t (:underline (:color "#dbc900" :style wave)))))
 '(flymake-note ((t (:underline (:color "#41c101" :style wave)))))
 ;; Custom operator face
 '(font-lock-operator-face ((t (:foreground "#FF0000"))))      ; Operators: +, -, etc.
 ;; Custom enum member face
 '(font-lock-enum-member-face ((t (:foreground "#1BFC06"))))   ; Enum values: SL_LOG_FATAL, SDL_WINDOW_RESIZABLE
 ;; Custom number face
 '(font-lock-number-face ((t (:foreground "#CD950C"))))        ; Numbers: 1920, 1080
 )

(provide-theme 'nvim_dark)
