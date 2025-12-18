;;; angeld-content-exporter.el --- site content exporter worker program -*- lexical-binding: t; -*-
;;; Commentary:
;; Batch export all site-content .org files to HTML with detailed logging.

;;; Code:

(defvar xz/export-log-file "/home/angel/xynapz/angeld/logs/site-export.log"
  "Path to the site export log file.")

(defvar xz/site-content-dir "/home/angel/xynapz/site-content"
  "Path to the site-content source directory.")

(defvar xz/export-start-time nil
  "Start time of the current export run.")

(defun xz/export-log (level msg)
  "Log MSG with LEVEL (INFO, WARN, ERROR, STAT) to the export log."
  (let ((prefix (pcase level
                  ('info "INFO ")
                  ('warn "WARN ")
                  ('error "ERROR")
                  ('stat "STAT ")
                  ('sep "═════")
                  (_ "     "))))
    (make-directory (file-name-directory xz/export-log-file) t)
    (write-region
     (format "[%s] %s  %s\n" (format-time-string "%H:%M:%S") prefix msg)
     nil xz/export-log-file 'append 'silent)))

(defun xz/export-log-header ()
  "Log the export run header with system info."
  (write-region
   (concat
    "\n"
    "╔══════════════════════════════════════════════════════════════════════╗\n"
    (format "║  SITE EXPORT RUN - %s                              ║\n" (format-time-string "%Y-%m-%d %H:%M:%S"))
    "╚══════════════════════════════════════════════════════════════════════╝\n")
   nil xz/export-log-file 'append 'silent))

(defun xz/export-log-footer (total-files open-buffers fresh-buffers errors elapsed)
  "Log the summary footer with stats."
  (write-region
   (concat
    "\n"
    "┌──────────────────────────────────────────────────────────────────────┐\n"
    "│  EXPORT SUMMARY                                                      │\n"
    "├──────────────────────────────────────────────────────────────────────┤\n"
    (format "│  Total Files:      %-5d                                             │\n" total-files)
    (format "│  From Open Buffer: %-5d                                             │\n" open-buffers)
    (format "│  Fresh Buffer:     %-5d                                             │\n" fresh-buffers)
    (format "│  Errors:           %-5d                                             │\n" errors)
    (format "│  Elapsed Time:     %.2fs                                            │\n" elapsed)
    "└──────────────────────────────────────────────────────────────────────┘\n")
   nil xz/export-log-file 'append 'silent))

(defun xz/short-path (file)
  "Return shortened path for FILE, relative to site-content."
  (if (string-prefix-p xz/site-content-dir file)
      (substring file (1+ (length xz/site-content-dir)))
    file))

(defun xz/export-site-content ()
  "Export all .org files in site-content to HTML with detailed logging."
  (interactive)
  (let* ((files (directory-files-recursively xz/site-content-dir "\\.org$"))
         (total 0)
         (open-count 0)
         (fresh-count 0)
         (errors 0)
         (start-time (float-time)))

    (xz/export-log-header)
    (xz/export-log 'info (format "Source: %s" xz/site-content-dir))
    (xz/export-log 'info (format "Files found: %d" (length files)))
    (xz/export-log 'sep "")

    (dolist (file files)
      (condition-case err
          (let ((existing-buffer (get-file-buffer file))
                (short-name (xz/short-path file)))
            (if existing-buffer
                (progn
                  (with-current-buffer existing-buffer
                    (org-html-export-to-html))
                  (xz/export-log 'info (format "[OPEN]  %s" short-name))
                  (setq open-count (1+ open-count)))
              ;; Fresh buffer
              (let ((buffer (find-file-noselect file)))
                (with-current-buffer buffer
                  (org-html-export-to-html))
                (kill-buffer buffer)
                (xz/export-log 'info (format "[FRESH] %s" short-name))
                (setq fresh-count (1+ fresh-count))))
            (setq total (1+ total)))
        (error
         (setq errors (1+ errors))
         (xz/export-log 'error (format "%s: %s" (xz/short-path file) (error-message-string err))))))

    (let ((elapsed (- (float-time) start-time)))
      (xz/export-log-footer total open-count fresh-count errors elapsed)
      ;; Return summary for notification
      (format "Exported %d files (%.1fs) | Errors: %d" total elapsed errors))))

(provide 'angeld-content-exporter)
;;; angeld-content-exporter.el ends here

