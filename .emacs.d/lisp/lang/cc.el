;;; cpp.el --- C/C++ support -*- lexical-binding: t; -*-
;;; Commentary:
;; cpp lsp and linting

;;; Code:

(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)))

;; Make .h open as C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Google style
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "google")
            (setq c-basic-offset 2)))

;; Disable namespace indentation
(c-set-offset 'innamespace 0)
(setq-default indent-tabs-mode nil)

;; CMake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; Build helpers (F5/F6/F7/F8)
(defun my/cmake-configure ()
  (interactive)
  (let* ((project (project-root (project-current t)))
         (build (concat project "build/"))
         (default-directory build)
         (src (concat build "compile_commands.json"))
         (dst (concat project "compile_commands.json")))
    (unless (file-exists-p build) (make-directory build))
    (compile "cmake ..")
    (run-with-timer
     2 nil
     (lambda ()
       (when (file-exists-p src)
         (when (file-exists-p dst) (delete-file dst))
         (make-symbolic-link src dst t))))))

(defun my/cmake-build ()
  (interactive)
  (let* ((project (project-root (project-current t)))
         (default-directory (concat project "build/")))
    (compile "make")))

(defun my/cmake-run ()
  (interactive)
  (let* ((project (project-root (project-current t)))
         (build (concat project "build/"))
         (default-directory build)
         (exe (car (directory-files build nil "^[^.].*[^.o]$")))
         (buf (format "*run: %s*" exe)))
    (make-comint-in-buffer exe buf (concat "./" exe))
    (pop-to-buffer buf)))

(global-set-key (kbd "<f5>") #'my/cmake-configure)
(global-set-key (kbd "<f6>") #'my/cmake-build)
(global-set-key (kbd "<f7>") #'my/cmake-run)
(global-set-key (kbd "<f8>") #'recompile)

(provide 'cc)

;;; cc.el ends here
