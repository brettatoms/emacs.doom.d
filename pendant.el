;;; ../../../Nextcloud/doom.d/pendant.el -*- lexical-binding: t; -*-

;; Maybe we should define a pendant minor mode that binds keys

(defun pendant-user-go ()
  (interactive)
  (cider-interactive-eval "(user/go pendant)"))

(defun pendant-reset ()
  (interactive)
  (cider-interactive-eval "(dev.pendant/reset)"))

(defun pendant-stop ()
  (interactive)
  (cider-interactive-eval "(dev.pendant/stop)"))

(defun pendant-go ()
  (interactive)
  (cider-interactive-eval "(dev.pendant/go)"))

(defun pendant-stop-and-go ()
  (interactive)
  (cider-interactive-eval "(do (dev.pendant/stop) (dev.pendant/go))"))

(defun auto-eval-sql-clj-on-hug-sql-edit ()
  "Evaluate sql.clj file in the same directory when editing .hug.sql files.
  Uses the REPL connection specific to the sql.clj file's project without switching
  buffers."
  (when (and (buffer-file-name)
             (string-suffix-p ".hug.sql" (buffer-file-name)))
    (let* ((hug-sql-file (buffer-file-name))
           (sql-clj-file (concat (file-name-directory hug-sql-file) "sql.clj")))
      (when (file-exists-p sql-clj-file)
        ;; (message "Found sql.clj file: %s" sql-clj-file)
        (save-window-excursion
          (save-excursion
            (let ((sql-buffer (find-file-noselect sql-clj-file)))
              (with-current-buffer sql-buffer
                ;; (message "CIDER available: %s" (featurep 'cider))
                ;; (message "CIDER connections bound: %s" (boundp
                ;;                                         'cider-connections))
                ;; (when (boundp 'cider-connections)
                ;;   (message "CIDER connections: %s" cider-connections))
                ;; (message "CIDER connected: %s" (and (fboundp 'cider-connected-p)
                ;;                                     (cider-connected-p)))
                (when (and (featurep 'cider)
                           (fboundp 'cider-connected-p)
                           (cider-connected-p))
                  (message "Evaluating %s..." sql-clj-file)
                  (cider-load-file sql-clj-file))))))))))

(add-hook 'after-save-hook 'auto-eval-sql-clj-on-hug-sql-edit)

(provide 'pendant)
