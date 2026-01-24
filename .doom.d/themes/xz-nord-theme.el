;;; xz-nord-theme.el --- A darker Nord theme for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Based on Nord Theme by Sven Greb (MIT License)
;; Modified by Angel Dhakal - darker background variant

;;; Code:

(require 'doom-themes)

(def-doom-theme xz-nord
  "A darker variant of the Nord theme."

  ;; Color definitions
  ((bg         '("#1b1d1e"))           ; Darker than standard nord
   (bg-alt     '("#262829"))
   (base0      '("#1b1d1e"))
   (base1      '("#262829"))
   (base2      '("#303030"))
   (base3      '("#434C5E"))
   (base4      '("#4C566A"))
   (base5      '("#616e88"))           ; Comments
   (base6      '("#9099AB"))
   (base7      '("#D8DEE9"))
   (base8      '("#ECEFF4"))
   (fg         '("#D8DEE9"))
   (fg-alt     '("#E5E9F0"))

   ;; Nord Frost
   (nord7      '("#8FBCBB"))
   (nord8      '("#88C0D0"))
   (nord9      '("#81A1C1"))
   (nord10     '("#5E81AC"))

   ;; Nord Aurora
   (red        '("#BF616A"))
   (orange     '("#D08770"))
   (yellow     '("#EBCB8B"))
   (green      '("#A3BE8C"))
   (magenta    '("#B48EAD"))

   ;; Semantic colors
   (grey       base5)
   (blue       nord9)
   (dark-blue  nord10)
   (cyan       nord8)
   (teal       nord7)
   (violet     magenta)
   (dark-cyan  '("#5E81AC"))

   ;; Face categories
   (highlight      nord8)
   (vertical-bar   base2)
   (selection      base3)
   (builtin        nord9)
   (comments       base5)
   (doc-comments   base5)
   (constants      nord9)
   (functions      nord8)
   (keywords       nord9)
   (methods        nord8)
   (operators      fg)
   (type           nord7)
   (strings        green)
   (variables      fg)
   (numbers        magenta)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red))

  ;; Face overrides
  (
   ;; Base
   ((font-lock-comment-face &override) :slant 'italic)
   ((font-lock-function-name-face &override) :weight 'bold)
   ((font-lock-preprocessor-face &override) :foreground dark-blue :weight 'bold)

   ;; UI
   (hl-line :background base1)
   (line-number :foreground base4 :background bg)
   (line-number-current-line :foreground nord8 :background bg)
   (mode-line :background base3 :foreground nord8)
   (mode-line-inactive :background base1 :foreground fg)
   (show-paren-match :foreground fg :background base3 :weight 'bold)
   (show-paren-mismatch :background red)

   ;; Org mode
   (org-level-1 :foreground nord7 :weight 'extra-bold)
   (org-level-2 :foreground nord8 :weight 'bold)
   (org-level-3 :foreground nord9 :weight 'semi-bold)
   (org-level-4 :foreground nord10 :weight 'normal)
   (org-block :background base1)
   (org-block-begin-line :foreground base4)
   (org-block-end-line :foreground base4)
   (org-code :foreground nord7)
   (org-date :foreground nord8)
   (org-document-title :foreground nord8 :weight 'bold)
   (org-done :foreground green :weight 'bold)
   (org-todo :foreground yellow :weight 'bold)
   (org-link :foreground nord8 :underline t)
   (org-table :foreground nord9)
   (org-verbatim :foreground nord7)

   ;; Magit
   (magit-section-heading :foreground nord7 :weight 'bold)
   (magit-section-highlight :background base2)
   (magit-branch-local :foreground nord7 :weight 'bold)
   (magit-branch-remote :foreground green :weight 'bold)
   (magit-hash :foreground nord8)
   (magit-diff-context-highlight :background base1)
   (magit-diffstat-added :foreground green)
   (magit-diffstat-removed :foreground red)

   ;; Diffs
   (diff-added :foreground green)
   (diff-changed :foreground yellow)
   (diff-removed :foreground red)
   (diff-header :foreground nord9 :weight 'bold)

   ;; Markdown
   (markdown-header-face-1 :foreground nord8 :weight 'bold)
   (markdown-header-face-2 :foreground nord8 :weight 'bold)
   (markdown-header-face-3 :foreground nord8 :weight 'bold)
   (markdown-inline-code-face :foreground nord7)
   (markdown-link-face :foreground nord8)
   (markdown-url-face :foreground fg :underline t)

   ;; Rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground nord7)
   (rainbow-delimiters-depth-2-face :foreground nord8)
   (rainbow-delimiters-depth-3-face :foreground nord9)
   (rainbow-delimiters-depth-4-face :foreground nord10)
   (rainbow-delimiters-depth-5-face :foreground orange)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground green)
   (rainbow-delimiters-depth-8-face :foreground magenta)

   ;; Flycheck
   (flycheck-error :underline `(:style wave :color ,red))
   (flycheck-warning :underline `(:style wave :color ,yellow))
   (flycheck-info :underline `(:style wave :color ,nord8))

   ;; Git gutter
   (git-gutter:modified :foreground yellow)
   (git-gutter:added :foreground green)
   (git-gutter:deleted :foreground red)

   ;; Corfu/Company
   (corfu-default :background base2 :foreground fg)
   (corfu-current :background base3 :weight 'bold)
   (corfu-border :background base3)

   ;; Which key
   (which-key-key-face :foreground nord8)
   (which-key-separator-face :foreground base4)
   (which-key-command-description-face :foreground fg))

  ;; Extra variables
  ())

;;; xz-nord-theme.el ends here
