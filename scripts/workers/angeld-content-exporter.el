(defun my/site-export-log (msg)
  "Append MSG to the site export log file with a timestamp."
  (let ((log-file "/home/angel/xynapz/angeld.me/logs/site-export.log"))
    (make-directory (file-name-directory log-file) t)
    (write-region (concat (format-time-string "[%Y-%m-%d %H:%M:%S] ") msg "\n") nil log-file 'append)))

(defun my/export-site-content ()
  "Export all .org files in site-content to HTML, preserving existing buffers."
  (interactive)
  (let ((files (directory-files-recursively "/home/angel/xynapz/angeld.me/site-content" "\\.org$"))
        (count 0)
        (errors 0))
    (my/site-export-log "--------------------------------------------------")
    (my/site-export-log "Starting site export...")
    (dolist (file files)
      (condition-case err
          (let ((existing-buffer (get-file-buffer file)))
            (if existing-buffer
                (with-current-buffer existing-buffer
                  (my/site-export-log (format "Exporting (from open buffer): %s" file))
                  (org-html-export-to-html))
              ;; File not open, open it, export, then kill
              (let ((buffer (find-file-noselect file)))
                (with-current-buffer buffer
                  (my/site-export-log (format "Exporting (fresh buffer): %s" file))
                  (org-html-export-to-html))
                (kill-buffer buffer)))
            (setq count (1+ count)))
        (error
         (setq errors (1+ errors))
         (my/site-export-log (format "ERROR exporting %s: %s" file err)))))
    
    (let ((summary (format "Export complete. Processed: %d, Errors: %d" count errors)))
      (my/site-export-log summary)
      summary)))
