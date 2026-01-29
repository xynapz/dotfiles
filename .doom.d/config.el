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

;; Disable prettify-symbols (e.g., -> becoming →, <= becoming ≤)
(global-prettify-symbols-mode -1)
(setq prettify-symbols-unprettify-at-point nil)

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

;; Set formatters per mode if needed (optional overrides)
(after! format
  (setq +format-with-lsp t)
  )

;; LANGUAGE SETTINGS
;; Each language sets BOTH tab-width AND its native indent variable
;; This ensures Emacs indent matches formatter output

;; GLOBAL DEFAULTS
(setq-default tab-width 4
              indent-tabs-mode nil
              standard-indent 4
              evil-shift-width 4)

;; C/C++ - 4 spaces
(setq-hook! '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook)
  tab-width 4
  c-basic-offset 4
  indent-tabs-mode nil)

;; Python - 4 spaces (PEP 8)
(setq-hook! '(python-mode-hook python-ts-mode-hook)
  tab-width 4
  python-indent-offset 4
  indent-tabs-mode nil)

;; JavaScript/TypeScript - 2 spaces
(setq-hook! '(js-mode-hook js-ts-mode-hook js2-mode-hook
              typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook)
  tab-width 2
  js-indent-level 2
  typescript-indent-level 2
  indent-tabs-mode nil)

;; Web (HTML/CSS) - 2 spaces
(setq-hook! '(html-mode-hook mhtml-mode-hook
              css-mode-hook css-ts-mode-hook scss-mode-hook)
  tab-width 2
  css-indent-offset 2
  sgml-basic-offset 2
  indent-tabs-mode nil)

;; Go - tabs (gofmt standard)
(setq-hook! '(go-mode-hook go-ts-mode-hook)
  tab-width 4
  indent-tabs-mode t)

;; Rust - 4 spaces (rustfmt)
(setq-hook! '(rust-mode-hook rust-ts-mode-hook)
  tab-width 4
  rust-indent-offset 4
  indent-tabs-mode nil)

;; Java - 4 spaces
(setq-hook! '(java-mode-hook java-ts-mode-hook)
  tab-width 4
  c-basic-offset 4
  indent-tabs-mode nil)

;; Shell/Bash - 2 spaces
(setq-hook! '(sh-mode-hook bash-ts-mode-hook)
  tab-width 2
  sh-basic-offset 2
  indent-tabs-mode nil)

;; Lua - 2 spaces
(setq-hook! '(lua-mode-hook lua-ts-mode-hook)
  tab-width 2
  lua-indent-level 2
  indent-tabs-mode nil)

;; PHP - 4 spaces (PSR-12)
(setq-hook! 'php-mode-hook
  tab-width 4
  c-basic-offset 4
  indent-tabs-mode nil)

;; Ruby - 2 spaces
(setq-hook! '(ruby-mode-hook ruby-ts-mode-hook)
  tab-width 2
  ruby-indent-level 2
  indent-tabs-mode nil)

;; YAML - 2 spaces
(setq-hook! '(yaml-mode-hook yaml-ts-mode-hook)
  tab-width 2
  yaml-indent-offset 2
  indent-tabs-mode nil)

;; JSON - 2 spaces
(setq-hook! '(json-mode-hook json-ts-mode-hook)
  tab-width 2
  js-indent-level 2
  indent-tabs-mode nil)

;; Nix - 2 spaces
(setq-hook! '(nix-mode-hook nix-ts-mode-hook)
  tab-width 2
  nix-indent-function 'nix-indent-line
  indent-tabs-mode nil)

;; Terraform - 2 spaces
(setq-hook! 'terraform-mode-hook
  tab-width 2
  indent-tabs-mode nil)

;; LaTeX - 2 spaces
(setq-hook! '(latex-mode-hook LaTeX-mode-hook)
  tab-width 2
  LaTeX-indent-level 2
  indent-tabs-mode nil)

;; Dockerfile - 4 spaces
(setq-hook! 'dockerfile-mode-hook
  tab-width 4
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
  :defer t
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
  :defer t
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
  (setq org-refile-targets '((nil :maxlevel . 2)
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

;; Decode WezTerm's C-S-a sequence for "C-S-a" evil-numbers/dec-at-pt
(unless (display-graphic-p)
  (define-key input-decode-map (kbd "\e[97;6u") (kbd "C-S-a")))

;;; TERMINAL CLIPBOARD SUPPORT
(when (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
  (executable-find "wl-copy")
  (executable-find "wl-paste")
  (defun my-wl-copy (text)
    "Copy with wl-copy if in terminal, otherwise use the original value of `interprogram-cut-function'."
    (if (display-graphic-p)
        (gui-select-text text)
      (let ((wl-copy-process
             (make-process :name "wl-copy"
                           :buffer nil
                           :command '("wl-copy")
                           :connection-type 'pipe)))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))))
  (defun my-wl-paste ()
    "Paste with wl-paste if in terminal. otherwise use the original value of `interprogram-paste-function'"
    (if (display-graphic-p)
        (gui-selection-value)
      (shell-command-to-string "wl-paste --no-newline")))
  (setq interprogram-cut-function #'my-wl-copy)
  (setq interprogram-paste-function #'my-wl-paste))
