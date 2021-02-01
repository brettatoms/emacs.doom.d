;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 17)
      doom-modeline-height 20 ;; TODO: not sure this is being respected
      doom-theme 'doom-one-light
      doom-localleader-key ","

      ;; Disable workspace switching for projects to allow us to have files open
      ;; from different projects in the same frame.
      +workspaces-on-switch-project-behavior nil

      ;; TODO: trying to remove annoying messsage when killing buffers not in
      ;; perspective....maybe best we just disable perspective mode
      persp-autokill-buffer-on-remove 'dont-ask-kill-weak

      ;; The indexing method can't be 'alien if we want to sort by recently-active
      projectile-indexing-method 'hybrid
      projectile-sort-order 'recently-active)

;; (global-undo-tree-mode -1)


;; Shows uncommitted changes in the fringe
(global-diff-hl-mode 1)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Maximize frame on startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))

(setq mac-command-modifier 'ctrl
      mac-control-modifier 'meta
      mac-option-modifier 'meta
      evil-escape-key-sequence "kj")


(after! yasnippet
  (add-hook 'yas-minor-mode-hook
    (lambda ()
      (yas-load-directory "~/.emacs.snippets.d"))))

(use-package! evil-collection
  :config
  (evil-collection-init 'package-list))

(after! lsp-ui
  (add-hook 'lsp-ui-mode-hook
            (lambda ()
              (setq lsp-ui-sideline-mode nil
                    lsp-headerline-breadcrumb-enable nil))))

(use-package! lsp
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.tox\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pytest_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.expo\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.log\\'")

  ;; Fix potential lockups
  ;; https://github.com/hlissner/doom-emacs/issues/4093
  (setq! ;; lsp-enable-file-watchers nil
         ;; +format-with-lsp causes problems with undo in web-mode,
         ;; could also do (setq-hook! 'web-mode-hook +format-with-lsp nil)
         +format-with-lsp nil))

(after! eglot
  (add-to-list 'eglot-server-programs
               `(typescript-tsx-mode . ("typescript-language-server" "-stdio"))))



;; allow saving the buffer in insert mode without leaving insert mode
(map! "C-x s" 'save-buffer)

(map! :leader
      ;; Open the project magit status buffer like Spacemacs
      :g "p v" 'projectile-vc

      ;; Go to the next Flycheck error like Spacemacs
      :g "e n" 'flycheck-next-error
      :g "e l" 'flycheck-list-errors

      ;; Search in project like Spacemacs
      ;; :g "/" '+default/search-project

      ;; Toggle line comments
      :nv ";" 'evilnc-comment-or-uncomment-lines

      ;; Toggle line comments
      :nv "r l" 'ivy-resume

      :v "s" 'evil-surround-region

      :nv "j l" 'evil-avy-goto-line
      :nv "j w" 'evil-avy-goto-word-1

      ;; Spacemacs style window jumping
      :g "0" 'winum-select-window-0-or-10
      :g "1" 'winum-select-window-1
      :g "2" 'winum-select-window-2
      :g "3" 'winum-select-window-3
      :g "4" 'winum-select-window-4

      :g "5" 'winum-select-window-5

      ;; TODO: unfortunately SPC-h is used for help....we move windows a lot
      ;; more than we use help so maybe we can change the normal SPC-h prefix to
      ;; something else
      ;;
      ;; :n "h" 'evil-window-left
      ;; :n "j" 'evil-window-down
      ;; :n "k" 'evil-window-up
      ;; :n "l" 'evil-window-right

      ;; SPC-; overrides the Doom default of switching workspaces but I don't
      ;; use workspaces very much
      :nv [tab] 'evil-switch-to-windows-last-buffer

      ;; "SPC o f" causes me to open a lot of new frames on accident
      :nv "o f" nil

      :nv "b x" 'doom/switch-to-scratch-buffer
      :nv "b s" 'doom/switch-to-scratch-buffer
      )

(map! :map prog-mode-map
      :i [tab] 'company-indent-or-complete-common)

(after! magit
  :config
  (map! :map magit-mode-map
        :nv "z" 'magit-stash)
  ;; remove some hooks from the magit status screen so it loads a bit faster
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

;; (map! :map evil-magit-section-map
;;       :nv "z" 'magit-stash)

;; The web-mode packages is named "web-mode" so doing (after! web ...) here won't work.
(after! web-mode
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (prettier-js-mode)
                             (rainbow-mode)
                             (setq +format-with-lsp nil))))

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
  :config
  ;; override doom's default vterm popup rule
  (set-popup-rule! "^vterm" :ignore t)
  ;; Doom hides the modeline for vterm by default
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode))

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))


(custom-theme-set-faces! 'doom-one-light
  '(font-lock-function-name-face :foreground "steelblue")
  '(font-lock-variable-name-face :foreground "steelblue4"))

