;;; git.el --- Magit Git configuration -*- lexical-bindig: t; -*-
;;; Commentary:
;; magit, gittimemachine

;;; Code:
;; Magit - Git porcelain for Emacs
(use-package magit
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-clone-default-directory "~/projects/")
  :config
  ;; Performance
  (setq magit-refresh-status-buffer nil
        magit-git-executable "git"))

;; Magit todos - Show TODOs in magit status
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1)
  (setq magit-todos-keywords-list '("TODO" "FIXME" "HACK" "NOTE" "XXX")))

;; Git timemachine - Browse historic versions
(use-package git-timemachine
  :commands git-timemachine
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal git-timemachine-mode-map
      (kbd "j") 'git-timemachine-show-next-revision
      (kbd "k") 'git-timemachine-show-previous-revision
      (kbd "p") 'git-timemachine-show-previous-revision
      (kbd "n") 'git-timemachine-show-next-revision
      (kbd "g") 'git-timemachine-show-nth-revision
      (kbd "q") 'git-timemachine-quit
      (kbd "w") 'git-timemachine-kill-abbreviated-revision
      (kbd "W") 'git-timemachine-kill-revision
      (kbd "y") 'git-timemachine-show-commit
      (kbd "?") 'git-timemachine-help)))

;; Git gutter - Show git diff in fringe
(use-package git-gutter
  :diminish
  :hook ((prog-mode text-mode conf-mode) . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.5)
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c" :foreground "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b" :foreground "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6" :foreground "#ff79c6")))))

;; Diff-hl - Alternative to git-gutter (choose one)
;; (use-package diff-hl
;;   :hook ((prog-mode text-mode conf-mode) . diff-hl-mode)
;;   :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
;;   :hook (magit-post-refresh . diff-hl-magit-post-refresh))

;; Forge - Work with GitHub/GitLab from Magit
(use-package forge
  :after magit
  :config
  (setq forge-topic-list-limit '(60 . 0)))

;; Git link - Get GitHub/GitLab links
(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-open-in-browser t))

;; Blamer - Git blame in current line (like GitLens)
(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-prettify-time-p t)
  (blamer-type 'visual)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 0.9
                    :italic t))))

(provide 'magit-config)
;;; git.el ends here
