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
(setq doom-font (font-spec :family "Iosevka SS04" :size 20 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "SF Pro Text" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'wombat)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


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
;;
;;F# config
(after! fsharp-mode
  (remove-hook 'fsharp-mode-hook #'lsp!))
(after! company
  (global-company-mode -1))


(use-package! eglot
  :hook (fsharp-mode . eglot-ensure)
  :config
  ;; Specify the F# language server executable if needed
  (add-to-list 'eglot-server-programs '(fsharp-mode . ("dotnet" "fsautocomplete" "--background-service-enabled"))))

(use-package! corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-min-width 250
        corfu-min-height 750
        corfu-count 20
        corfu-auto t
        corfu-cycle t
        corfu-separator ?\s
        corfu-preview-current "insert"
        corfu-scroll-margin 25
        ;; enable corfu on TAB
        tab-always-indent 'complete
        ;; shows documentation after `corfu-popupinfo-delay'
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)) )

(use-package! orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; (use-package! eglot
;;   :hook (fsharp-mode . eglot-ensure))

;; (after! fsharp-mode
;;   (add-hook 'fsharp-mode-hook #'lsp-deferred)
;;   (setq lsp-fsharp-server-install-dir (expand-file-name "~/.dotnet/tools"))
;;   (setq lsp-fsharp-server-path "fsautocomplete"))

;; (after! fsharp-mode
;;  (setq lsp-fsharp-server-install-dir (expand-file-name "~/.dotnet/tools"))
;;  (add-hook 'fsharp-mode-hook #'lsp))
;;
;;
;; (use-package! lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :config (setq lsp-fsharp-server-path (expand-file-name "~/.dotnet/tools/fsautocomplete")))

;; (use-package! fsharp-mode-abbrev-table
;;   :hook (fsharp-mode . lsp))
;; Org mode
(defun my/org-open-at-point-same-window ()
  "Open Org link at point in the current window."
  (interactive)
  (let ((org-link-frame-setup '((file . find-file))))
    (org-open-at-point)))

(map! :map org-mode-map
      :localleader
      :desc "Open link in same window"
      "L" #'my/org-open-at-point-same-window)

;; Org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org/org-roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
    "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "Plade materiale noter" plain (file "~/Documents/org/org-roam/roam-templates/plade materiale noter.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
      ("u" "Plugins noter" plain (file "~/Documents/org/org-roam/roam-templates/plugin noter.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("s" "Software at checke ud" plain (file "~/Documents/org/org-roam/roam-templates/software noter.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-enable))



;; Angiv vinduets størrelse ved start
(setq initial-frame-alist '((width . 180) (height . 60)))
;;(setq python-shell-completion-native-enable 'nil)
;;


;; Org settings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(after! org
  (setq org-tag-alist
        '(;;Places
          ("@hjem" . ?H)
          ("@studiet" . ?S)
          ("@uni" . ?U)

          ;; Device
          ("@computer" . ?C)
          ("@telefon" . ?T)

          ;; Domæne
          ("@komponist" . ?K)
          ("@datalogi" . ?D)
          ("@koncertarranger" . ?K)
          ("@amager-electronic" . ?A)
          ("@administration" . ?a)))
  (setq org-agenda-files '("~/Documents/org/agenda/"))

  (setq org-agenda-custom-commands
        '(("k" "Komponist tasks for today"
           ((tags-todo "@komponist+TIMESTAMP>=\"<today>\"+TIMESTAMP<=\"<today>\""
                       ((org-agenda-overriding-header "Komponist TODOs for Today")))))
          ("w" "kompo TODOs" tags-todo "+@komponist"
           ((org-agenda-overriding-header "kompo TODOs")))))
  (setq org-link-frame-setup
      '((file . find-file-other-window)))
  
)

(map! :map org-mode-map
      :n "C-t" #'org-set-tags-command)

;; LaTex config

(setq +latex-viewers '(pdf-tools))
