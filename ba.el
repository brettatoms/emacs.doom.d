;;; ../Nextcloud/doom.d/ba.el -*- lexical-binding: t; -*-

;; https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun ba/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun ba/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (ba/reload-dir-locals-for-current-buffer)))))

(defun ba/switch-to-vterm (vterm-buffer-name)
  "Switch to a vterm buffer if one exists else create one"
  (interactive)
  (let* ((vterm-buffer-name (or vterm-buffer-name "vterm<1>"))
         (buffer-names
          (mapcar 'buffer-name (buffer-list)))
         (vterm-buffer-exists (member vterm-buffer-name buffer-names))
         (vterm-window (get-buffer-window vterm-buffer-name)))
    (if vterm-window
        (progn
          (select-window vterm-window)
          (evil-insert-state 1))
      (if vterm-buffer-exists
          (progn
            (switch-to-buffer vterm-buffer-name)
            (evil-insert-state 1))
        (progn
          ;; Start vterm in the project root directory
          (vterm--set-directory (projectile-project-root))
          (vterm vterm-buffer-name))))))

;; TODO: Maybe we can do it by adding advice around  cider-popup-buffer-display
(defun ba/assoc-cider-error-buffer-to-project ())

(provide 'ba)
