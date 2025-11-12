;; ~/.emacs.d/init.el
;; This is the only file Emacs reads upon startup.
;; Its only job is to load our "real" config in init.org.

(require 'org)
(require 'ob-tangle)

;; Load the Org-mode configuration file
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))

;; Set custom-set-variables to be saved in a separate file
;; This keeps your init.org clean from auto-generated code
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
