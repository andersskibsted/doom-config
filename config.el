;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; I    f you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;(setq gcmh-high-cons-threshold (* 256 1024 1024)) ; 256MB

(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq doom-theme 'doom-one)

;; ae, oe og aa
(add-hook 'text-mode-hook (lambda () (set-input-method "danish-postfix")))

(map! :after vertico
      :map vertico-map
      "C-j"  #'vertico-next
      "C-k" #'vertico-previous
      "C-d" #'vertico-scroll-down
      "C-u" #'vertico-scroll-up
      ;; Directory navigation (vertico-directory)
      "C-h" #'vertico-directory-delete-char
      "C-l" #'vertico-directory-enter
      "M-h" #'vertico-directory-delete-word)

;; Customize ripgrep args
(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=1000 \
       --path-separator / --smart-case --no-heading \
       --with-filename --line-number --search-zip \
       --hidden")  ;; tilføj --hidden for at søge i skjulte filer

(after! rainbow-delimiters
  ;; Antal forskellige farver (max 9)
  (setq rainbow-delimiters-max-face-count 9)

  ;; Fremhæv mismatched delimiters
  (setq rainbow-delimiters-highlight-braces-p t
        rainbow-delimiters-highlight-brackets-p t
        rainbow-delimiters-highlight-parens-p t)
  ;; Brug mere intense farver
  (custom-set-faces!
    '(rainbow-delimiters-depth-1-face :foreground "#c678dd")
    '(rainbow-delimiters-depth-2-face :foreground "#98be65")
    '(rainbow-delimiters-depth-3-face :foreground "#51afef")
    '(rainbow-delimiters-depth-4-face :foreground "#da8548")
    '(rainbow-delimiters-depth-5-face :foreground "#46d9ff")
    '(rainbow-delimiters-depth-6-face :foreground "#a9a1e1")
    '(rainbow-delimiters-depth-7-face :foreground "#ecbe7b")
    '(rainbow-delimiters-depth-8-face :foreground "#c678dd")
    '(rainbow-delimiters-depth-9-face :foreground "#98be65"))
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                      :foreground "red"
                      :weight 'bold
                      :background "#3f0d0d"))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'show-smartparens-mode)

(after! prism
  ;; Moderat mættede farver
  (prism-set-colors :num 16
    :desaturations '(0 10 20)     ; Lavt = høj mætning
    :lightens '(-10 0 10)         ; Variation i lysstyrke
    :colors '("#ff6c6b" "#51afef" "#98be65"
              "#ECBE7B" "#c678dd" "#46d9ff")))
;; Kraftige farver
;; (prism-set-colors :num 9
;;   :desaturations '(0)           ; Ingen desaturation
;;   :lightens '(0)                ; Ingen lightening
;;   :colors '("#ff0000"           ; Pure red
;;             "#00ff00"           ; Pure green
;;             "#0000ff"           ; Pure blue
;;             "#ffff00"           ; Pure yellow
;;             "#ff00ff"           ; Pure magenta
;;             "#00ffff"))        ; Pure cyan
;; Moderate farver
;; (prism-set-colors
;;   :desaturations '(40 50)
;;   :lightens '(5 10)
;;   :colors '("#51afef" "#98be65" "#da8548" "#c678dd" "#46d9ff"))
;; Brug dit theme's farver
;; (prism-set-colors :num 16
;;   :desaturations '(40 50)
;;   :lightens '(5 10)
;;   :colors (list (doom-color 'blue)
;;                 (doom-color 'green)
;;                 (doom-color 'yellow)
;;                 (doom-color 'magenta)
;;                 (doom-color 'cyan)
;;                 (doom-color 'orange)))

(add-hook 'emacs-lisp-mode #'prism-mode)
(add-hook 'lisp-mode #'prism-mode)
(add-hook 'scheme-mode #'prism-mode)
(add-hook 'clojure-mode #'prism-mode)
(add-hook 'python-mode-hook #'prism-whitespace-mode)
;;(add-hook 'prog-mode-hook #'prism-comments-mode)

(setq display-line-numbers t)
(setq org-directory "~/org/")

(setq org-capture-templates
      '(("t" "Tasks")
        ("tt" "Unscheduled task" entry (file+headline "~/Documents/org/agenda/inbox.org" "Tasks")
         "* TODO %?")
        ("tD" "Task with deadline" entry (file+headline "~/Documents/org/agenda/inbox.org" "Task with deadline")
         "* TODO %? DEADLINE: %^{Deadline date}t\n %i"
         :time-prompt t)
        ("ts" "Scheduled task" entry (file+headline "~/Documents/org/agenda/dayplanner.org" "Tasks")
         "* TODO %? SCHEDULED: %^{Schedule date}t\n  %i\n"
         :time-prompt t)
        ("td" "Scheduled task with deadline" entry (file+headline "~/Documents/org/agenda/dayplanner.org" "Tasks")
         "* TODO %? SCHEDULED: %^{Schedule date}t DEADLINE: %^{Deadline}t\n  %i\n"
         :time-prompt t)


        ("d" "Dayplanner")
        ("dd" "Dayplanner" entry
         (file+datetree "~/Documents/org/agenda/dayplanner.org")
         "** Morgen \n*** [ ] Aflever\n*** [ ] Tossefit\n** Formiddag \n %? \n** Frokost \n\n** Eftermiddag \n\n** Eftermiddag 2 \n*** [ ] Hente \n*** [ ] Tossefit \n** Aften "
         :time-prompt t)
        ("dt" "Task in dayplanner" entry
         (file+datetree "~/Documents/org/agenda/dayplanner.org")
         "TODO %?"
         :time-prompt t)
        ("dc" "Task in dayplanner with link to context" entry
         (file+datetree "~/Documents/org/agenda/dayplanner.org")
         "TODO %?\n %a"
         :time-prompt t)
        ("dn" "Add note to a day in dayplanner" entry
         (file+datetree "~/Documents/org/agenda/dayplanner.org")
         "** Note: %?"
         :time-prompt t)

        ("e" "Emails")
        ("eu" "Urgent response" entry
         (file+headline "~/Documents/org/agenda/inbox.org" "Urgent emails")
         "* TODO Respond to %? DEADLINE: %^{Deadline date}t \nSubject: ")
        ("en" "Non-urgent response" entry
         (file+headline "~/Documents/org/agenda/inbox.org" "Non-urgent emails")
         "* TODO Respond to %? \n Subject: ")))


(after! org
  (setq org-agenda-files
        (directory-files-recursively "~/Documents/org/agenda" "\\.org$")))

(after! org-roam
  (setq org-roam-directory "~/Documents/org/org-roam/"))

;; Typst / tinymist
(after! lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "tinymist")
    :major-modes '(typst-ts-mode)
    :server-id 'tinymist)))

(after! typst-ts-mode
  (add-hook 'typst-ts-mode-hook #'lsp!))

;; typst-lsp - men jeg tror at det er tinymist som er konfigureret ovenfor der den der skal bruges
;; (after! lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))

;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "typst-lsp")
;;     :major-modes '(typst-ts-mode)
;;     :server-id 'typst-lsp)))

;; python
(after! lsp-mode
  (setq lsp-disabled-clients '(ts-query-ls))
  (setq lsp-pyright-auto-import-completions t
        lsp-pyright-diagnostic-mode "workspace"))

(after! lsp-ui
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-always-show t)

  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t   ; vis fejl/warnings
        lsp-ui-sideline-show-hover nil       ; vis hover info (kan være spam)
        lsp-ui-sideline-show-code-actions t  ; vis tilgængelige code actions
        lsp-ui-sideline-delay 0.5)

  (setq ;;lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-cursor nil      ; vis når cursor er over symbol
        lsp-ui-doc-show-with-mouse t     ; vis ved mouse hover
        lsp-ui-doc-position 'bottom      ; 'top, 'bottom, 'at-point
        lsp-ui-doc-delay 0.2               ; delay før den vises
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 20))

(map! :after lsp-mode
      :map lsp-mode-map
      :leader
       ;; lsp
      "l" '(:ignore t :which-key "lsp")
      "la" #'lsp-execute-code-action        ;; code actions
      "lr" #'lsp-rename                      ;; rename symbol
      "lf" #'lsp-format-buffer               ;; format
      "lF" #'lsp-format-region               ;; format region
      "ld" #'lsp-find-definition             ;; go to definition
      "lD" #'lsp-find-declaration            ;; go to declaration
      "li" #'lsp-find-implementation         ;; go to implementation
      "lt" #'lsp-find-type-definition        ;; go to type def
      "lR" #'lsp-find-references             ;; find references
      "ls" #'lsp-describe-thing-at-point     ;; show docs
      "lh" #'lsp-ui-doc-show                 ;; show hover
      "ll" #'lsp-workspace-show-log          ;; show log
      "lq" #'lsp-workspace-restart         ;; restart
      "lK" #'lsp-ui-doc-show           ;; Hover docs
      "lgd" #'lsp-ui-peek-find-definitions
      "lgp" #'lsp-ui-peek-find-references)

(after! avy
  (setq avy-all-windows t
        avy-background t
        avy-timeout-seconds 0.5
        avy-keys '(?s ?n ?t ?h ?a ?e ?i ?r))
  )

(after! treesit
 :config
    ;; Remap major modes to tree-sitter versions
    (setq major-mode-remap-alist
          '((c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (python-mode . python-ts-mode)
            (bash-mode . bash-ts-mode)
            (css-mode . css-ts-mode)
            (javascript-mode . js-ts-mode)
            (json-mode . json-ts-mode)
            (rust-mode . rust-ts-mode)))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'treesit-language-source-alist
                 '(typst "https://github.com/uben0/tree-sitter-typst")))
    ;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
    ;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

(after! ace-window
  (setq aw-keys '(?s ?n ?t ?h ?a ?e ?i ?r)
        aw-scope 'frame
        aw-background t))
(map! :after ace-window
      :map ace-window-map
      "M-o" #'ace-window)

(use-package! lispyville
  :hook ((emacs-lisp-mode . lispyville-mode)
         (lisp-mode . lispyville-mode)
         (scheme-mode . lispyville-mode)
         (clojure-mode . lispyville-mode)
         (racket-mode . lispyville-mode))
  :config
  ;; Vælg de key themes du vil have
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     text-objects
     atom-movement
     slurp/barf-cp
     additional
     additional-movement
     commentary
     additional-wrap)))

(use-package! dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

;; Sprog-specifikke adapters
(after! dap-mode
  (require 'dap-python)    ; Python
  (require 'dap-node)      ; Node.js
  (require 'dap-go)        ; Go
  (require 'dap-lldb))     ; C/C++/Rust

(map! :after dap-mode
      :map dap-mode-map
      "<<f5>>" 'dap-debug
      "<f6>" 'dap-disconnect
      "<f9>" 'dap-breakpoint-toggle
      "<f10>" 'dap-next)

;; Typst mode setup
(use-package! typst-ts-mode
  :mode "\\.typ\\'"

  :config
  (setq typst-ts-mode-grammar-location (expand-file-name "tree-sitter/typst" doom-data-dir))

  ;; Enable LSP
  (add-hook 'typst-ts-mode-hook #'lsp!)

  ;; Optional: Enable ligatures
  (add-hook 'typst-ts-mode-hook #'rainbow-delimiters-mode))

(after! typst-ts-mode
  ;; Tillad automatisk anvendelse af local variables for typst-preview-file
  (add-to-list 'safe-local-variable-values
               '(typst-preview-file . stringp))

  ;; ;; Automatisk start preview hvis variablen er sat
  ;; (add-hook 'typst-ts-mode-hook
  ;;           (lambda ()
  ;;             (when (and (boundp 'typst-preview-file)
  ;;                        typst-preview-file
  ;;                        (not (get-buffer "*typst-preview*"))) ; Kun hvis preview ikke kører
  ;;               (typst-preview-start typst-preview-file)))))
)

(use-package! typst-preview
  :after typst-ts-mode
  :config
  (setq typst-preview-autostart t
        typst-preview-open-browser-automatically t)
  (add-hook 'typst-ts-mode-hook #'typst-preview-mode)
  :custom
  (typst-preview-browse "default")
  (typst-prieview-invert-colors "no")
  (typst-preview-executable "tinymist"))

;; LSP setup for Typst

(after! smartparens
  (sp-local-pair 'typst-ts-mode "$" "$"))

;; (after! dired
;;   (setq dired-omit-extensions
;;         (remove ".fsl" dired-omit-extensions)))

;;; RISC-V Assembly Configuration

(after! asm-mode
  ;; RISC-V syntax
  (setq asm-comment-char ?\#)

  ;; Indentation
  (setq tab-width 8)
  (setq indent-tabs-mode nil)

  ;; Tab stops for labels og instruktioner
  (setq tab-stop-list '(0 8 16 24 32 40 48))

  ;; Highlight RISC-V instruktioner
  (font-lock-add-keywords
   'asm-mode
   '(("\\<\\(add\\|addi\\|sub\\|and\\|andi\\|or\\|ori\\|xor\\|xori\\|sll\\|slli\\|srl\\|srli\\|sra\\|srai\\|slt\\|slti\\|sltu\\|sltiu\\)\\>" . font-lock-keyword-face)
     ("\\<\\(lb\\|lh\\|lw\\|lbu\\|lhu\\|sb\\|sh\\|sw\\)\\>" . font-lock-keyword-face)
     ("\\<\\(beq\\|bne\\|blt\\|bge\\|bltu\\|bgeu\\)\\>" . font-lock-keyword-face)
     ("\\<\\(jal\\|jalr\\|ret\\)\\>" . font-lock-keyword-face)
     ("\\<\\(lui\\|auipc\\)\\>" . font-lock-keyword-face)
     ("\\<\\(ecall\\|ebreak\\)\\>" . font-lock-keyword-face)
     ("\\<\\(li\\|la\\|mv\\|j\\|call\\)\\>" . font-lock-builtin-face) ; pseudo-instruktioner
     ("\\<\\([xsft]?[0-9]+\\|zero\\|ra\\|sp\\|gp\\|tp\\|[ast][0-9]\\|a[0-7]\\|s[0-9]\\|s1[01]\\|t[0-6]\\)\\>" . font-lock-variable-name-face) ; registre
     ("\\.\\(globl\\|text\\|data\\|section\\|align\\|word\\|byte\\|string\\|option\\)\\>" . font-lock-preprocessor-face)))) ; directives

;; Auto-mode for .s og .S filer
(add-to-list 'auto-mode-alist '("\\.s\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.S\\'" . asm-mode))

;; Compile keybinding (hvis du vil assemblere direkte)
(map! :map asm-mode-map
      :localleader
      "c" #'compile
      "C" (lambda ()
            (interactive)
            (compile (format "riscv32-unknown-elf-as %s -o %s.o"
                           (buffer-file-name)
                           (file-name-sans-extension (buffer-file-name))))))

(use-package! gas-mode
  :mode "\\.s\\'"
  :config
  (setq gas-comment-char ?\#))

(use-package! multi-vterm
  :after vterm
  :config
  (setq multi-vterm-dedicated-window-height-percent 30))

  ;; Keybindings
  (map! :after multi-vterm
        :map multi-vterm-mode-map
        :leader
        (:prefix ("v" . "vterm")
         :desc "New vterm" "n" #'multi-vterm
         :desc "Next vterm" "]" #'multi-vterm-next
         :desc "Previous vterm" "[" #'multi-vterm-prev
         :desc "Dedicated toggle" "t" #'multi-vterm-dedicated-toggle
         :desc "Project vterm" "p" #'multi-vterm-project))

(use-package! smalltalk-mode
  :mode "\\.st\\'"
  :config
  (setq smalltalk-indent-amount 4))

(after! smalltalk-mode
  (map! :map smalltalk-mode-map
        :localleader
        "e" #'smalltalk-eval-region
        "b" #'smalltalk-eval-buffer))

(after! info
  (add-to-list 'Info-directory-list
               "/opt/homebrew/Cellar/gnu-smalltalk/3.2.5_10/share/info"))

;; Lookup funktioner
(defun my/gst-lookup ()
  "Search in GNU Smalltalk manual"
  (interactive)
  (let ((symbol (or (thing-at-point 'symbol t)
                    (read-string "Search for: "))))
    (info "(gst)")
    (Info-goto-node "Top")
    (Info-search symbol)))

(map! :after smalltalk-mode
      :map smalltalk-mode-map
      :localleader
      "h" #'my/gst-lookup)

(after! rustic
  (setq rustic-lsp-client 'lsp-mode)
  ;;(setq rustic-format-on-save t)       ; kør rustfmt ved gem
  (setq rustic-lsp-server 'rust-analyzer)
  (setq read-process-output-max (* 1024 1024)) ; 1mb
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-eldoc-enable-hover nil)   ; henter ikke docs ved hover
  (setq lsp-signature-auto-activate nil)) ; slår signatur-popup fra

;; Inline type hints (valgfri men meget nyttigt)
(setq lsp-rust-analyzer-display-chained-hint-types t
      lsp-rust-analyzer-display-closure-return-type-hints "always"
      lsp-rust-analyzer-display-parameter-hints t)

(use-package! gptel
  :config
  (setq gptel-model 'mistral:latest
        gptel-backend (gptel-make-ollama "Ollama"
                                         :host "localhost:11434"
                                         :stream t
                                         :models '(mistral:latest
                                                   codellama:latest
                                                   llama3.2:latest
                                                   deepseek-coder-v2:latest))))
;; Aktiver gptel-mode automatisk i relevante modes
;;(add-hook 'prog-mode-hook #'gptel-mode)
;; (add-hook 'text-mode-hook #'gptel-mode)
;; (add-hook 'org-mode-hook #'gptel-mode)

;; Keybindings til gptel
(map! :leader
      :desc "GPTel chat" "v w" #'gptel
      :desc "Send region" "v s" #'gptel-send
      :desc "Brug Mistral" "v m" #'gptel-use-mistral
      :desc "Brug CodeLlama" "v c" #'gptel-use-codellama
      :desc "Brug Llama 3.2" "v l" #'gptel-use-llama3
      :desc "Brug DeepSeek Coder v2 " "v d" #'gptel-use-deepseekcoder)


(defun gptel-use-mistral ()
  "Skift til Mistral model"
  (interactive)
  (setq gptel-model "mistral:latest")
  (message "Skiftet til Mistral"))

(defun gptel-use-codellama ()
  "Skift til CodeLlama model"
  (interactive)
  (setq gptel-model "codellama:latest")
  (message "Skiftet til CodeLlama"))

(defun gptel-use-llama3 ()
  "Skift til CodeLlama model"
  (interactive)
  (setq gptel-model "llama3.2:latest")
  (message "Skiftet til Llama 3.2"))

(defun gptel-use-deepseekcoder ()
  "Skift til CodeLlama model"
  (interactive)
  (setq gptel-model "deepseek-coder-v2:latest")
  (message "Skiftet til DeepSeek Coder"))

;; Kobl TSX-filer til web-mode
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Brug tsx-ts-mode i stedet for web-mode til .tsx filer
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'tsx-ts-mode-hook #'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (after! smartparens
;;   (sp-with-modes '(tsx-ts-mode)
;;     (sp-local-pair "<" nil :actions nil)))
    ;; (sp-local-pair "</" nil :actions nil)))

;; Prettier ved gem
;;(add-hook 'typescript-mode-hook 'prettier-mode)
;;(add-hook 'web-mode-hook 'prettier-mode)

;; LSP i typescript og web-mode
;; (add-hook 'typescript-mode-hook #'lsp!)
;; (add-hook 'web-mode-hook #'lsp!)

(use-package! tldr)

;; (add-to-list 'load-path "/.config/emacs/modules/lang/fasto")
;; (require 'fasto-mode)
(use-package! bison-mode
  :mode ("\\.fsy\\'" . bison-mode)
  :mode ("\\.fsp\\'" . bison-mode))

(defun my/add-scheduled-to-string (str)
  "Insert 'SCHEDULED: ' before timestamp in STR and return the result."
  (if (string-match "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" str)
      (concat (substring str 0 (match-beginning 0))
              "SCHEDULED: "
              (substring str (match-beginning 0)))
    str))  ; Return unchanged if no timestamp found

(defun md-to-org-scheduled-todo (md-file org-file org-scheduled-file)
  "Læs en Markdown-fil og tilføj nye linjer som TODOs i en Org-fil."
  (interactive "fMarkdown file: \nfOrg file: ")
  (let ((lines (with-temp-buffer
                 (insert-file-contents md-file)
                 (split-string (buffer-string) "\n" t)))
        (existing-todos (append
                         (with-temp-buffer
                           (insert-file-contents org-file)
                           (split-string (buffer-string) "\n" t))
                         (with-temp-buffer
                           (insert-file-contents org-scheduled-file)
                           (split-string (buffer-string) "\n" t)))))
    (dolist (line lines)
      (if (string-match "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" line)
          (let ((line-scheduled (my/add-scheduled-to-string line)))
            (when (and (not (string-blank-p line))
                       (not (member (concat "* TODO " line-scheduled) existing-todos)))
              (with-temp-buffer
                (insert-file-contents org-scheduled-file)
                (goto-char (point-max))
                (insert (concat "\n* TODO " line-scheduled))
                (write-region (point-min) (point-max) org-scheduled-file))))
        (when (and (not (string-blank-p line))
                   (not (member (concat "* TODO " line) existing-todos)))
          (with-temp-buffer
            (insert-file-contents org-file)
            (goto-char (point-max))
            (insert (concat "\n* TODO " line))
            (write-region (point-min) (point-max) org-file)))))))


;; Comic sans frame
(defun my/comic-sans-frame ()
  "Toggle Comic sans in current frame"
  (interactive)
  (if (string= (face-attribute 'default :family (selected-frame)) "Comic Sans MS")
      (set-frame-font my/default-font)
    ;; (set-frame-font my/default-font)
    (set-frame-font "Comic Sans MS-12")))

(md-to-org-scheduled-todo "/Users/andersskibsted/Library/Mobile Documents/iCloud~md~obsidian/Documents/Org-agenda/Org-agenda.md" "~/Documents/org/agenda/inbox.org" "~/Documents/org/agenda/dayplanner.org")
