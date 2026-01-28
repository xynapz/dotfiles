;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Identity (optional)
(setq user-full-name "xinapx"
      user-mail-address "xynapz@aol.com")

;; FONTS
(setq doom-font (font-spec :family "Iosevka" :size 23)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 23)
      doom-big-font (font-spec :family "Iosevka" :size 26))

;; THEME
;; (setq doom-theme 'xz-nord)  ; Your custom theme
(setq doom-theme 'xz-nord)

;; EDITOR
(setq display-line-numbers-type 'relative)

;; Org directory
(setq org-directory "~/org/")

;; LSP Configuration
(after! lsp-mode
  ;; Auto-detect project root - no more prompts
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil)

  ;; Sensible performance settings
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-file-watch-threshold 1500
        lsp-response-timeout 10)

  ;; Keep useful features, disable heavy ones
  (setq lsp-headerline-breadcrumb-enable t
        lsp-signature-auto-activate '(:on-trigger-char :after-completion))

  ;; Python - use Pyright (faster, better types)
  ;; Pyright handles: type checking, completion, go-to-definition
  ;; Formatting handled by Doom's +format (uses black/ruff)
  (setq lsp-pyright-venv-path nil)  ; Auto-detect from .venv

  ;; C/C++ clangd
  (setq lsp-clients-clangd-args
        '("--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--pch-storage=memory"
          "-j=4"
          "--log=error"))

  ;; Go
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.staticcheck" t t)))

  ;; Rust
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t)

  ;; YAML schemas
  (setq lsp-yaml-schemas
        '((kubernetes . ["/k8s/*.yaml" "/kubernetes/*.yaml"])
          (docker-compose . ["docker-compose*.yaml" "compose*.yaml"])))

  ;; Terraform
  (setq lsp-terraform-ls-enable-show-reference t
        lsp-terraform-ls-prefill-required-fields t))


;; LSP UI - keep minimal
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t))

;; CODE FORMATTING - Doom's built-in +format module
;; Uses format-all under the hood. Formatters are auto-detected.
;; To disable format-on-save for specific modes:
;; (add-hook! 'go-mode-hook (format-on-save-mode -1))

;; Set formatters per mode if needed (optional overrides)
(after! format
  ;; Use LSP formatter if available, fallback to format-all
  (setq +format-with-lsp t)

  ;; Specific formatter overrides (optional)
  ;; (set-formatter! 'black '("black" "-q" "-") :modes '(python-mode python-ts-mode))
  ;; (set-formatter! 'prettier '("prettier" "--stdin-filepath" filepath) :modes '(js-mode typescript-mode))
  )

;; LANGUAGE SETTINGS

;; Web development - 2 space indent
(setq-hook! '(js-mode-hook js-ts-mode-hook typescript-mode-hook
              typescript-ts-mode-hook tsx-ts-mode-hook
              css-mode-hook css-ts-mode-hook html-mode-hook)
  tab-width 2
  js-indent-level 2
  typescript-indent-level 2
  css-indent-offset 2)

;; Go - tabs
(setq-hook! 'go-ts-mode-hook
  tab-width 4
  indent-tabs-mode t)

;; Rust - 4 spaces
(setq-hook! 'rust-ts-mode-hook
  tab-width 4
  indent-tabs-mode nil)

;; YAML - 2 spaces
(setq-hook! 'yaml-ts-mode-hook
  tab-width 2
  indent-tabs-mode nil)

;; Bash - 2 spaces
(setq-hook! 'bash-ts-mode-hook
  tab-width 2
  indent-tabs-mode nil)

;; C/C++ SMART HEADER DETECTION
(defun xz/detect-c-or-cpp-header ()
  "Detect whether a .h file should use C or C++ mode."
  (let* ((filename (buffer-file-name))
         (base-name (file-name-sans-extension filename))
         (has-cpp-file (or (file-exists-p (concat base-name ".cc"))
                           (file-exists-p (concat base-name ".cpp"))
                           (file-exists-p (concat base-name ".cxx"))))
         (has-c-file (file-exists-p (concat base-name ".c")))
         (content-suggests-cpp
          (save-excursion
            (goto-char (point-min))
            (or (re-search-forward "\\bclass\\b" nil t)
                (re-search-forward "\\bnamespace\\b" nil t)
                (re-search-forward "\\btemplate\\b" nil t)
                (re-search-forward "::\\|std::" nil t)))))
    (cond
     (content-suggests-cpp 'c++-ts-mode)
     (has-cpp-file 'c++-ts-mode)
     (has-c-file 'c-ts-mode)
     (t 'c++-ts-mode))))

(defun xz/setup-h-file-mode ()
  "Automatically select C or C++ mode for .h files."
  (funcall (xz/detect-c-or-cpp-header)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . xz/setup-h-file-mode))

(defun xz/toggle-c-cpp-mode ()
  "Toggle between C and C++ mode."
  (interactive)
  (cond
   ((derived-mode-p 'c-ts-mode) (c++-ts-mode) (message "Switched to C++"))
   ((derived-mode-p 'c++-ts-mode) (c-ts-mode) (message "Switched to C"))
   (t (message "Not in a C/C++ buffer"))))

(map! :leader :desc "Toggle C/C++" "c t" #'xz/toggle-c-cpp-mode)

;;; PYTHON & FLASK

;; Auto-activate .venv and restart LSP
(use-package! pyvenv
  :config
  (defun xz/auto-activate-python-env ()
    "Auto-activate .venv and configure pyright to use it."
    (let* ((root (projectile-project-root))
           (venv-path (and root (expand-file-name ".venv" root))))
      (when (and venv-path (file-exists-p venv-path))
        (pyvenv-activate venv-path)
        ;; Tell pyright about the venv
        (setq-local lsp-pyright-venv-path root)
        (message "Activated venv: %s" venv-path))))

  (add-hook 'python-mode-hook #'xz/auto-activate-python-env)
  (add-hook 'python-ts-mode-hook #'xz/auto-activate-python-env))

;; Jinja2 / Flask templates (use web-mode)
(use-package! web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.jinja2?\\'" . web-mode)
         ("\\.j2\\'" . web-mode))
  :config
  ;; Jinja2 detection for Flask templates
  (setq web-mode-engines-alist
        '(("jinja" . "\\.html\\'")
          ("jinja" . "\\.jinja2?\\'")))

  ;; Indentation
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t))

;;; PROJECTILE

(after! projectile
  (setq projectile-project-search-path '("~/xynapz/")
        projectile-enable-caching t
        projectile-indexing-method 'alien))

;;; VTERM - Terminal Multiplexer Workflow
;; Philosophy: Use Emacs as the multiplexer instead of tmux
;; Named terminals for different purposes, quick keybindings

(use-package! vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project))

(set-popup-rule! "^\\*vterm" :size 0.4 :vslot -4 :select t :quit nil :ttl nil)

;; Named terminal management
(defun xz/vterm-named (name)
  "Create or switch to a named vterm buffer."
  (if-let ((buf (get-buffer name)))
      (if (eq buf (current-buffer))
          (bury-buffer)
        (pop-to-buffer buf))
    (let ((vterm-buffer-name name))
      (vterm))))

(defun xz/vterm-main ()
  "Main terminal - general purpose."
  (interactive)
  (xz/vterm-named "*vterm-main*"))

(defun xz/vterm-server ()
  "Server terminal - for running dev servers."
  (interactive)
  (xz/vterm-named "*vterm-server*"))

(defun xz/vterm-ssh ()
  "SSH terminal - for remote connections."
  (interactive)
  (xz/vterm-named "*vterm-ssh*"))

(defun xz/vterm-project ()
  "Project-scoped terminal (uses vterm-toggle)."
  (interactive)
  (vterm-toggle))

;; Quick access keybindings under SPC o (open)
(map! :leader
      :prefix ("o" . "open")
      :desc "Toggle vterm" "t" #'xz/vterm-project
      :desc "Main terminal" "T" #'xz/vterm-main
      :desc "Server terminal" "s" #'xz/vterm-server
      :desc "SSH terminal" "S" #'xz/vterm-ssh)

(use-package! nginx-mode
  :mode ("nginx\\.conf\\'" "/nginx/.*\\.conf\\'"))

(use-package! dotenv-mode
  :mode ("\\.env\\'" "\\.env\\..*\\'"))

;;; ORG MODE

(after! org
  ;; Directory structure
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

  ;; TODO Keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#BF616A" :weight bold))
          ("NEXT" . (:foreground "#EBCB8B" :weight bold))
          ("WAIT" . (:foreground "#D08770" :weight bold))
          ("DONE" . (:foreground "#A3BE8C" :weight bold))
          ("CANCELLED" . (:foreground "#4C566A" :weight bold))))

  ;; Agenda
  (setq org-agenda-files (list org-directory))

  (setq org-agenda-custom-commands
        '(("1" "Today"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-start-day "today")
                        (org-deadline-warning-days 0)
                        (org-agenda-overriding-header "📅 Today")))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))))

          ("7" "This Week"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday nil)
                        (org-deadline-warning-days 7)
                        (org-agenda-overriding-header "📅 This Week")))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))
            (todo "WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))))

          ("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Actions")))
            (todo "WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
            (tags-todo "inbox" ((org-agenda-overriding-header "📥 Inbox")))))

          ("p" "Projects"
           ((tags-todo "+project" ((org-agenda-overriding-header "🚀 Active Projects")))))

          ("l" "Learning"
           ((tags-todo "+learning" ((org-agenda-overriding-header "📚 Learning")))
            (tags-todo "+reading" ((org-agenda-overriding-header "📖 Reading List")))))))

  ;; Capture Templates
  (setq org-capture-templates
        `(("t" "Task" entry (file org-default-notes-file)
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i" :empty-lines 1)

          ("T" "Task with Dates" entry (file org-default-notes-file)
           "* TODO %?\nSCHEDULED: %^t\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i" :empty-lines 1)

          ("c" "Code Task" entry (file org-default-notes-file)
           "* TODO %?\nSCHEDULED: %^t\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %a\n:END:\n%i" :empty-lines 1)

          ("p" "Project" entry (file ,(expand-file-name "projects.org" org-directory))
           "* TODO %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Goals\n** Tasks\n*** TODO \n** Notes\n" :empty-lines 1)

          ("l" "Learning" entry (file ,(expand-file-name "learning.org" org-directory))
           "* TODO %? :learning:\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: \n:END:\n** Notes\n** Questions\n** Summary\n" :empty-lines 1)

          ("j" "Journal" entry (file+datetree ,(expand-file-name "journal.org" org-directory))
           "* %<%H:%M> %?\n" :empty-lines 1)

          ("n" "Note" entry (file org-default-notes-file)
           "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i" :empty-lines 1)))

  ;; Refile
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  ;; Archive
  (setq org-archive-location (concat (expand-file-name "archive.org" org-directory) "::* From %s"))

  ;; Babel
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)
          (latex . t)
          (shell . t)
          (C . t))))

(map! :n "C-a" #'evil-numbers/inc-at-pt
      :n "C-S-a" #'evil-numbers/dec-at-pt)

;;; TERMINAL CLIPBOARD SUPPORT
(unless (display-graphic-p)
  (cond
   ;; Wayland
   ((executable-find "wl-copy")
    (setq interprogram-cut-function
          (lambda (text &optional _)
            (start-process "wl-copy" nil "wl-copy" text)))
    (setq interprogram-paste-function
          (lambda ()
            (shell-command-to-string "wl-paste -n | tr -d \\r"))))
   ;; X11 - xclip
   ((executable-find "xclip")
    (setq interprogram-cut-function
          (lambda (text &optional _)
            (start-process "xclip" nil "xclip" "-selection" "clipboard" "-i" text)))
    (setq interprogram-paste-function
          (lambda ()
            (shell-command-to-string "xclip -selection clipboard -o"))))))
