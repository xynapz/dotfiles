;;; vterm.el --- VTerm terminal configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; vterm splits

;;; Code:
;; VTerm - Fast terminal emulator
(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  (vterm-kill-buffer-on-exit t)
  (vterm-timer-delay 0.01)
  :config
  ;; Evil keybindings
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)
    
    ;; Key translations
    (define-key vterm-mode-map (kbd "C-c C-c") 'vterm-send-C-c)
    (define-key vterm-mode-map (kbd "C-c C-d") 'vterm-send-C-d)
    (define-key vterm-mode-map (kbd "C-c C-z") 'vterm-send-C-z)
    
    ;; Allow escaping to normal mode
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-\\") 'evil-normal-state)
    
    (evil-define-key 'normal vterm-mode-map
      (kbd "i") 'evil-emacs-state
      (kbd "a") 'evil-emacs-state
      (kbd "p") 'vterm-yank
      (kbd "u") 'vterm-undo)))

;; Multi-vterm - Multiple vterm instances
(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-project multi-vterm-dedicated-toggle)
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :config
  ;; Projectile integration
  (defun my/vterm-project ()
    "Open vterm in project root."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (multi-vterm)))
  
  ;; VSCode-like split terminal at bottom
  (defun my/vterm-split-below ()
    "Open vterm in a split below."
    (interactive)
    (let ((buf (multi-vterm)))
      (set-window-dedicated-p (selected-window) nil)
      (switch-to-buffer-other-window buf)
      (evil-window-move-very-bottom)
      (evil-window-set-height 15)))
  
  ;; Quick toggle terminal (like VSCode Ctrl+`)
  (defun my/vterm-toggle ()
    "Toggle a dedicated vterm at the bottom."
    (interactive)
    (if (and (boundp 'my/vterm-toggle-buffer)
             my/vterm-toggle-buffer
             (buffer-live-p my/vterm-toggle-buffer))
        (let ((win (get-buffer-window my/vterm-toggle-buffer)))
          (if win
              (delete-window win)
            (progn
              (split-window-below -15)
              (other-window 1)
              (switch-to-buffer my/vterm-toggle-buffer))))
      (progn
        (split-window-below -15)
        (other-window 1)
        (vterm)
        (setq my/vterm-toggle-buffer (current-buffer)))))
  
  ;; Server terminal - persistent vterm for long-running processes
  (defvar my/vterm-server-buffers '()
    "List of server vterm buffers.")
  
  (defun my/vterm-new-server (name)
    "Create a new named vterm for running servers."
    (interactive "sServer name: ")
    (let* ((buffer-name (format "*vterm-server:%s*" name))
           (buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (unless (eq major-mode 'vterm-mode)
          (vterm-mode))
        (add-to-list 'my/vterm-server-buffers buf))
      (switch-to-buffer-other-window buf)
      (evil-window-set-height 15)))
  
  (defun my/vterm-list-servers ()
    "List all server vterm buffers."
    (interactive)
    (let ((servers (cl-remove-if-not #'buffer-live-p my/vterm-server-buffers)))
      (if servers
          (consult-buffer (list (cons "Server Terminals" servers)))
        (message "No server terminals running"))))
  
  (defun my/vterm-kill-server ()
    "Kill a server vterm buffer."
    (interactive)
    (let* ((servers (cl-remove-if-not #'buffer-live-p my/vterm-server-buffers))
           (buf (consult-buffer (list (cons "Kill Server Terminal" servers)))))
      (when buf
        (kill-buffer buf)
        (setq my/vterm-server-buffers (delete buf my/vterm-server-buffers))))))

;; VTerm toggle - Simpler toggle solution
(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;; Shell pop - Alternative popup terminal
(use-package shell-pop
  :commands shell-pop
  :custom
  (shell-pop-shell-type '("vterm" "*vterm-pop*" (lambda () (vterm))))
  (shell-pop-window-size 30)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))

;; Directory tracking in vterm
(defun my/vterm-directory-sync ()
  "Sync vterm directory with Emacs."
  (when (derived-mode-p 'vterm-mode)
    (let ((dir (vterm-get-pwd)))
      (when dir
        (setq default-directory dir)))))

(add-hook 'vterm-mode-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      #'my/vterm-directory-sync nil t)))

;; Better copy mode for vterm
(defun my/vterm-copy-mode ()
  "Enter copy mode in vterm (like tmux)."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (vterm-copy-mode 1)
    (evil-normal-state)))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-y") 'my/vterm-copy-mode))

;; Send region to vterm
(defun my/vterm-send-region (start end)
  "Send selected region to vterm."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (if (get-buffer vterm-buffer-name)
        (with-current-buffer vterm-buffer-name
          (vterm-send-string text)
          (vterm-send-return))
      (message "No vterm buffer found"))))

;; Clear vterm
(defun my/vterm-clear ()
  "Clear vterm buffer."
  (interactive)
  (vterm-send-C-l))

(provide 'vterm-config)
;;; vterm.el ends here
