;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 17)
      doom-modeline-height 20 ;; TODO: not sure this is being respected
      doom-theme 'doom-one-light)

;; Maximize frame on startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))

(setq mac-command-modifier 'ctrl
      mac-control-modifier 'meta
      mac-option-modifier 'meta
      evil-escape-key-sequence "kj")

(use-package! evil-collection
  :config
  (evil-collection-init 'package-list)
  ; (evil-collection-init)
)

(map! :leader
      ;; Open the project magit status buffer like Spacemacs
      :g "p v" 'projectile-vc

      ;; Go to the next Flycheck error like Spacemacs
      :g "e n" 'flycheck-next-error

      ;; Search in project like Spacemacs
      :g "/" '+default/search-project

      ;; Toggle line comments
      :nv ";" 'evilnc-comment-or-uncomment-lines


      ;; Toggle line comments
      :nv "r l" 'ivy-resume

      ;; SPC-; overrides the Doom default of switching workspaces but I don't
      ;; use workspaces very much
      :nv [tab] 'evil-switch-to-windows-last-buffer)


(add-hook 'web-mode-hook 'prettier-js-mode)

(defun switch-to-vterm (vterm-buffer-name)
  "Switch to a vterm buffer if one exists else create one"
  (interactive)
  (let* (
         (vterm-buffer-name (or vterm-buffer-name "vterm<1>"))
         (buffer-names
          (mapcar 'buffer-name (buffer-list)))
         (vterm-buffer-exists (member vterm-buffer-name buffer-names))
         (vterm-window (get-buffer-window vterm-buffer-name)))
    (message vterm-buffer-name)
    ;; (message vterm-buffer-exists)
    (if vterm-window
        (progn
          (select-window vterm-window)
          (evil-insert-state 1))
      (if vterm-buffer-exists
          (progn
            (switch-to-buffer vterm-buffer-name)
            (evil-insert-state 1))
        (vterm vterm-buffer-name))
      )))

(map!
 :leader
 :nv "o 1" (defun switch-to-vterm-1 () (interactive) (switch-to-vterm "vterm<1>"))
 :nv "o 2" (defun switch-to-vterm-2 () (interactive) (switch-to-vterm "vterm<2>"))
 :nv "o 3" (defun switch-to-vterm-3 () (interactive) (switch-to-vterm "vterm<3>"))
 :nv "o 4" (defun switch-to-vterm-4 () (interactive) (switch-to-vterm "vterm<4>")))
;; Switch to normal mode when the process in a vterm buffer exits. The
;; lambda is required so that the arguments don't get passed to
;; evil-normal-state
;; (add-to-list 'vterm-exit-functions (lambda (&rest r) (evil-normal-state)))

(after! vterm
  ;; Doom hides the modeline for vterm by default
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode))
