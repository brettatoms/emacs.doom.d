;;; banzai.el -*- lexical-binding: t; -*-

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


(defun banzai-at-work-user-go ()
  (interactive)
  (cider-interactive-eval "(user/go at-work)"))

(defun banzai-at-work-reset ()
  (interactive)
  (cider-interactive-eval "(dev.banzai-at-work/reset)"))

(defun banzai-at-work-stop ()
  (interactive)
  (cider-interactive-eval "(dev.banzai-at-work/stop)"))

(defun banzai-at-work-go ()
  (interactive)
  (cider-interactive-eval "(dev.banzai-at-work/go)"))

(defun banzai-at-work-stop-and-go ()
  (interactive)
  (cider-interactive-eval "(do (dev.banzai-at-work/stop) (dev.banzai-at-work/go))"))


(defun auto-eval-sql-clj-on-hug-sql-edit ()
  "Evaluate sql.clj file in the same directory when editing .hug.sql files.
  Uses the REPL connection specific to the sql.clj file's project without switching
  buffers.

  Use the following to make this run on save:
  (add-hook 'after-save-hook 'auto-eval-sql-clj-on-hug-sql-edit)
  "
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

(defun pendant-setup-key-bindings ()
  "Setup key bindings for pendant."
  (interactive)
  (map! :leader
        :n "r p r" 'pendant-reset
        :n "r p s" 'pendant-stop
        :n "r p g" 'pendant-go
        :n "r p R" 'pendant-stop-and-go))

(defun banzai-at-work-setup-key-bindings ()
  "Setup key bindings for pendant."
  (interactive)
  (map! :leader
        :n "r w r" 'banzai-at-work-reset
        :n "r w s" 'banzai-at-work-stop
        :n "r w g" 'banzai-at-work-go
        :n "r w R" 'banzai-at-work-stop-and-go))

(provide 'banzai)
