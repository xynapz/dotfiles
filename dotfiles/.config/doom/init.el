;;; init.el -*- lexical-binding: t; -*-

;; Doom Emacs Module Configuration
;; Run 'doom sync' after modifying this file!

(doom! :input
       ;;bidi
       ;;chinese
       ;;japanese
       ;;layout

       :completion
       (corfu +orderless +icons)
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)    ; Doom's built-in formatter
       snippets
       (whitespace +guess +trim)

       :emacs
       dired
       electric
       tramp
       undo
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       debugger
       direnv
       docker
       editorconfig
       (eval +overlay)
       lookup
       (lsp +peek)
       magit
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (cc +lsp +tree-sitter)
       data
       emacs-lisp
       ;;(go +lsp +tree-sitter)
       ;;(java +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       (latex +lsp)
       (lua +lsp +tree-sitter)
       markdown
       ;;(nix +lsp +tree-sitter)
       org
       ;;(php +lsp +tree-sitter)
       (python +lsp +pyright +tree-sitter)
       ;;(rust +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (terraform +lsp)
       (web +lsp +tree-sitter)
       (yaml +lsp)

       :email

       :app

       :config
       (default +bindings +smartparens))
