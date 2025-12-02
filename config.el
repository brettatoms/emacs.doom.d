;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;;
;; To reenable font smoothing on Mac
;; defaults write org.gnu.Emacs CGFontRenderingFontSmoothingDisabled -bool NO
;;
;; To control anti-aliasing on OSX:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0 (none)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 1 (light)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 2 (medium)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 3 (strong)

;; Maximize frame on startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))

;; Set platform specific variables
(cond
 ((string-equal system-type "darwin")
  ;; This needs to be set for doom to use fd on Mac since doom adds some weird advice to
  ;; projectile-get-ext-command. Both of these commands should be found with
  ;; executable-find but for some reason they're nil.
  (setq doom-fd-executable "/opt/homebrew/bin/fd"
        doom-ripgrip-executable "/opt/homebrew/bin/rg"
        ;; Performance improvements from
        ;; https://readingworldmagazine.com/emacs/2021-08-11-2021-08-11-emacs-snitch-and-how-to-improve-performance/
        ns-use-srgb-colorspace t
        mac-command-modifier 'ctrl
        mac-control-modifier 'meta
        mac-option-modifier 'meta)))

(setq! doom-font "CommitMono:size=18"
       ;; doom-theme 'modus-operandi
       inhibit-startup-screen t
       doom-localleader-key ","
       doom-snippets-dir (expand-file-name "~/emacs/snippets")

       ;; Disable workspace switching for projects to allow us to have files open
       ;; from different projects in the same frame.
       +workspaces-on-switch-project-behavior nil

       ;; TODO: trying to remove annoying messsage when killing buffers not in
       ;; perspective....maybe best we just disable perspective mode
       persp-autokill-buffer-on-remove 'dont-ask-kill-weak

       ;; The indexing method can't be 'alien if we want to sort by recently-active
       projectile-indexing-method 'hybrid
       projectile-sort-order 'recently-active

       ;; ws-butler-global-mode t
       enable-local-variables t

       ;; Set jit-lock-defer-time to 0 to defer fontification until there is not
       ;; pending input.
       jit-lock-defer-time 0

       ;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
       ;; cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"

       auth-sources '("~/.authinfo")

       evil-escape-key-sequence "kj"

       ;; For faster rending in large files
       bidi-paragraph-direction 'left-to-right
       bidi-inhibit-bpa t
       global-so-long-mode 1

       ;; Always create a new buffer for an async shell comamnd if the default buffer
       ;; has a process attached.
       async-shell-command-buffer 'new-buffer

       ;; Number of lines from the edge of the buffer to start scrolling
       ;; scroll-margin 4
       )

;; allow saving the buffer in insert mode without leaving insert mode
(map! "C-x s" 'save-buffer)

(map! :leader
      ;; SPC-. finds a file in the same directory as the open file
      :g "." (lambda ()
               (interactive)
               (let ((default-directory (file-name-directory buffer-file-name)))
                 (call-interactively #'find-file)))

      ;; Open the project magit status buffer like Spacemacs
      :g "p v" 'projectile-vc
      :g "p t" 'projectile-toggle-between-implementation-and-test

      ;; TODO: unbind SPC-o-f to open a new frame

      ;; Go to the next Flycheck error like Spacemacs
      :g "e n" 'flycheck-next-error
      :g "e l" 'flycheck-list-errors

      ;; Search in project like Spacemacs
      ;; :g "/" '+default/search-project

      ;; Toggle line comments
      :nv ";" 'evilnc-comment-or-uncomment-lines

      ;; Toggle line (comments)
      ;; :nv "r l" 'ivy-resume

      ;; TODO: This conflicts with the doom search commands, e.g. SPC-s-p but it seems like
      ;; it shouldn't b/c its only when in visual mode.
      ;; :v "s" 'evil-surround-region

      :nv "j l" 'evil-avy-goto-line
      :nv "j w" 'evil-avy-goto-word-1

      ;; Spacemacs style window jumping
      ;; :g "0" 'winum-select-window-0-or-10
      :g "0" 'treemacs-select-window
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
      ;; :nv [tab] 'evil-switch-to-windows-last-buffer

      ;; "SPC o f" causes me to open a lot of new frames on accident
      :nv "o f" nil

      :nv "b x" 'doom/switch-to-scratch-buffer
      :nv "b s" 'doom/switch-to-scratch-buffer

      :nv "r s" 'replace-string
      :nv "r r" 'replace-string
      :nv "r q" 'query-replace

      :nv "f o" 'ba/xdg-open)

(after! modus-themes
  :config
  ;; Use modus-themes-list-colors to see the modus face names
  (setq! modus-themes-common-palette-overrides `((bg-main "gray98")
                                                 (fg-main "gray10")
                                                 (bg-region "gray90")
                                                 (string green-warmer)
                                                 ;; (bg-mode-line-active bg-dim)
                                                 (bg-mode-line-active "gray90")
                                                 ;; (border-mode-line-active bg-active)
                                                 (border-mode-line-active "gray90")
                                                 (border-mode-line-inactive bg-mode-line-inactive)
                                                 (bg-paren-match bg-magenta-intense)
                                                 ;; lighten the magit diff bg colors
                                                 (bg-added bg-green-nuanced)
                                                 (bg-added-faint bg-green-nuanced)
                                                 (bg-added-refine bg-green-nuanced)
                                                 (bg-removed bg-red-nuanced)
                                                 (bg-removed-faint bg-red-nuanced)
                                                 (bg-removed-refine bg-red-nuanced)
                                                 ,@modus-themes-preset-overrides-warmer)))

(load-theme 'modus-operandi :no-confirm)

;; (use-package! modus-themes
;;   :config
;;   (setq! modus-themes-custom-auto-reload t
;;          modus-operandi-palette-overrides '((bg-main "gray98")
;;                                             (fg-main "gray10")
;;                                             (bg-hl-line "azure2")
;;                                             (bg-region "gray90")
;;                                             (fg-region :inherit)
;;                                             (comment "gray50")


;;                                             ;; (clojure-ts-keyword-face red)
;;                                             ;; (comment red)
;;                                             (fl-keyword maroon))
;;          modus-themes-common-palette-overrides `(;; From the section "Make the mode line borderless"
;;                                                  ;; (border-mode-line-active unspecified)
;;                                                  ;; (border-mode-line-inactive unspecified)
;;                                                  (border-mode-line-active bg-mode-line-active)
;;                                                  (border-mode-line-inactive bg-mode-line-inactive)

;;                                                  (bg-added bg-green-nuanced)
;;                                                  (bg-added-faint bg-green-nuanced)
;;                                                  (bg-added-refine bg-green-nuanced)

;;                                                  (bg-removed bg-red-nuanced)
;;                                                  (bg-removed-faint bg-red-nuanced)
;;                                                  (bg-removed-refine bg-red-nuanced)
;;                                                  ;; (fg-diff)

;;                                                  ;;  use header colors of the intense preset
;;                                                  ;; (bg-heading-0 unspecified)
;;                                                  ;; (bg-heading-1 bg-magenta-nuanced)
;;                                                  ;; (bg-heading-2 bg-red-nuanced)
;;                                                  ;; (bg-heading-3 bg-blue-nuanced)
;;                                                  ;; (bg-heading-4 bg-cyan-nuanced)
;;                                                  ;; (bg-heading-5 bg-green-nuanced)
;;                                                  ;; (bg-heading-6 bg-yellow-nuanced)
;;                                                  ;; (bg-heading-7 bg-red-nuanced)
;;                                                  ;; (bg-heading-8 bg-magenta-nuanced)

;;                                                  ;; (overline-heading-0 unspecified)
;;                                                  ;; (overline-heading-1 magenta-cooler)
;;                                                  ;; (overline-heading-2 magenta-warmer)
;;                                                  ;; (overline-heading-3 blue)
;;                                                  ;; (overline-heading-4 cyan)
;;                                                  ;; (overline-heading-5 green)
;;                                                  ;; (overline-heading-6 yellow-cooler)
;;                                                  ;; (overline-heading-7 red-cooler)
;;                                                  ;; (overline-heading-8 magenta)

;;                                                  (bg-paren-match bg-magenta-intense)

;;                                                  ;; Remove the border
;;                                                  ;; (border-mode-line-active unspecified)
;;                                                  ;; (border-mode-line-inactive unspecified)

;;                                                  ;; And expand the preset here.  Note that the ,@ works because
;;                                                  ;; we use the backtick for this list, instead of a straight
;;                                                  ;; quote.
;;                                                  ;; ,@modus-themes-preset-overrides-cooler
;;                                                  ;; ,@modus-themes-preset-overrides-faint
;;                                                  ,@modus-themes-preset-overrides-warmer
;;                                                  ;; ,@modus-themes-preset-overrides-intense
;;                                                  ))

;; :custom-face
;; ;; '(mode-line ((t :background "gray90"
;; ;;                 :box (:line-width 4
;; ;;                       :color "gray90"))))
;; ;; '(mode-line-inactive ((t :background "gray80"
;; ;;                          :box (:line-width 4
;; ;;                                :color "gray80"))))
;; (magit-section-heading ((t :foreground "medium blue")))
;; (magit-diff-file-heading ((t :foreground "gray10")))
;; (cider-test-success-face ((t :foreground "gray90" :background "green")))
;; (diff-hl-insert ((t :foreground "#6cc06c")))

;; (custom-set-faces
;;  '(mode-line-active ((t :background "gray88"
;;                         :box (:line-width 4
;;                               :color "gray88"))))
;;  '(mode-line-inactive ((t :background "gray80"
;;                           :box (:line-width 4
;;                                 :color "gray80"))))
;;  '(magit-section-heading ((t :foreground "medium blue")))
;;  '(magit-diff-file-heading ((t :foreground "gray10")))
;;  '(cider-test-success-face ((t :foreground "gray90" :background "green")))
;;  ;; removes black box around diff insert indicator in fringe but may also make
;;  ;; any characters in the fringe show up
;;  '(diff-hl-insert ((t :foreground "#6cc06c")))
;;  ;; Match clojure-mode
;;  '(clojure-ts-keyword-face ((t :foreground "#721045" :weight medium)))
;;  '(eglot-highlight-symbol-face ((t :background "lavender")))
;;  ;; '(window-divider ((t :background "gray90" :color "gray90")))
;;  ;; '(window-divider-first-pixel ((t :background "red" :color "red")))
;;  ;; '(window-divider-last-pixel ((t :background "red" :color "red")))
;;  ;; '(vertical-border ((t :background "red" :color "red")))
;;  )

;; (load-theme 'modus-operandi :no-confirm))

(after! doom-modeline
  (setq! doom-modeline-lsp nil
         ;; Not really necessary since doom-modeline respects display-time-mode
         doom-modeline-time nil
         doom-modeline-battery nil
         ;; doom-modeline-height 20 ;; This gets overridden by the char size
         doom-modeline-repl nil
         doom-modeline-debug nil
         ;; all-the-icons-scale-factor 1.1
         ))

(after! org
  (setq! org-startup-folded 'fold))

(add-hook 'js2-mode #'rainbow-mode)

(after! devdocs
  (map! :leader
        :map devdocs-mode-map
        :nv "d d" 'devdocs-lookup))

;; clean up whitespace in prog modes
;; (add-hook 'prog-mode-hook #'ws-butler-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (ws-butler-mode)
            (add-hook 'before-save-hook #'delete-trailing-whitespace)))

;; (add-hook 'prog-mode-hook (lambda () (rainbow-delk)))

(after! envrc
  (setq envrc-global-mode 1))

(after! yasnippet
  ;; TODO: Should we set the doom-snippets-dir instead?
  (setq! +snippets-dir (expand-file-name "~/emacs/snippets")))

;; (use-package! evil-collection
;;   :config
;;   (evil-collection-init 'package-list))

(after! sql
  (add-hook! 'sql-mode (lambda () (setq! devdocs-current-docs '("postgresql~16"))))
  (set-popup-rule! "^\*SQL" :ignore t)
  (setq sql-debug-send t
        sql-send-terminator t))

;; (after! eglot
;;   :defer t
;;   :config
;;   ;; eglot need extra time for the clojure-lsp to start
;;   (setq! eglot-connect-timeout 45)
;;   (add-hook
;;    'eglot-managed-mode-hook
;;    (lambda ()
;;      ;; we want eglot to setup callbacks from eldoc, but we don't want eldoc
;;      ;; running after every command. As a workaround, we disable it after we just
;;      ;; enabled it. Now calling `M-x eldoc` will put the help we want in the eldoc
;;      ;; buffer. Alternatively we could tell eglot to stay out of eldoc, and add
;;      ;; the hooks manually, but that seems fragile to updates in eglot.
;;      (eldoc-mode -1)))

;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              '((clojure-mode clojurescript-mode clojurec-mode clojure-ts-mode)
;;   ;;                . ("/usr/bin/clojure-lsp")))

;;   ;;   ;; TODO: figure out why python-mode + eglot uses so much CPU. Maybe it's b/c
;;   ;;   ;; flyckeck has an eglot checker so we're doing it twice.
;;   ;;   (add-to-list 'eglot-server-programs
;;   ;;                `(typescript-tsx-mode . ("typescript-language-server" "--stdio")))
;;   ;;   ;; (add-to-list 'eglot-server-programs
;;   ;;   ;;              `(python-mode . ("pyright" "-v" "venv" "-w")))
;;   ;;   (add-to-list 'eglot-server-programs
;;   ;;                '(graphql-mode . ("graphql-lsp" "server" "--method stream")))

;;   ;;   (map! :leader
;;   ;;         :map eglot-mode-map
;;   ;;         :nv "h e" 'eldoc))
;;   )

(after! lsp-mode
  ;; :custom
  ;; (lsp-ui-sideline-mode nil)
  ;; (lsp-ui-sideline-enable nil)
  ;; (lsp-modeline-code-actions-enable nil)
  ;; (lsp-modeline-diagnostics-enable nil)
  ;; (lsp-modeline-workspace-status-enable nil)
  ;; lsp-ui-mode nil
  ;; lsp-auto-configure nil
  ;; lsp-enable
  ;; lsp-headerline-breadcrumb-enable nil
  ;; lsp-lens-enable nil
  ;; lsp-signature-auto-activate t
  ;; lsp-signature-render-documentation t


  ;; TODO: Temporary disable lsp eldoc for performance reasons, mac only?
  (setq! lsp-eldoc-enable-hover nil)
  ;; lsp-eldoc-enable-hover t
  ;; lsp-eldoc-render-all t

  ;; Disabling lsp-auto-configure is the only way I could get lsp-mode to not
  ;; turn on lsp-ui by default.
  ;; lsp-auto-configure nil

  ;; Fix potential lockups
  ;; https://github.com/hlissner/doom-emacs/issues/4093
  ;; Update 11/17/2022: Disabled  b/c of "too many open files" error
  (setq! lsp-enable-file-watchers nil)

  ;; +format-with-lsp causes problems with undo in web-mode,
  ;; could also do (setq-hook! 'web-mode-hook +format-with-lsp nil)
  ;; +format-with-lsp nil

  ;; lsp-before-save-edits nil

  ;; It seems like the lsp client was still trying to use flake8 even
  ;; though its disabled in the custom settings
  (setq! lsp-pylsp-plugins-flake8-enabled nil)
  (setq! lsp-pylsp-plugins-mccabe-enabled nil)
  (setq! lsp-pylsp-plugins-pycodestyle-enabled nil)
  (setq! lsp-pylsp-plugins-pyflakes-enabled nil)

  ;; Disable lsp-ui mode
  (add-hook! 'prog-mode (lambda () (lsp-ui-mode nil)))

  ;; lsp-log-io t
  ;; :config
  (add-to-list 'lsp-disabled-clients 'semgrep-ls) ;; semgrep-ls kept crashing
  (add-to-list 'lsp-language-id-configuration '(web-mode . "html"))

  ;; (setq! lsp-pylsp-server-command '("pylsp" "--verbose"))

  (lsp-register-custom-settings
   '(("pylsp.plugins.black.enabled" t t)
     ("pylsp.plugins.ruff.enabled" t t)
     ;; Don't load other configration sources
     ("pylsp.configurationSources" [])
     ;; The ruff plugins disables flake8, pycodestyle, mccabe and pyflakes
     ;; already but doing it here again for good measure.
     ("pylsp.plugins.flake8.enabled" nil t)
     ("pylsp.plugins.pycodestyle.enabled" nil t)
     ("pylsp.plugins.mccabe.enabled" nil t)
     ("pylsp.plugins.pyflakes.enabled" nil t)
     ;; pylsp mypy config
     ("pylsp.plugins.pylsp_mypy.enabled" t t)
     ("pylsp.plugins.pylsp_mypy.live_mode" t t)
     ("pylsp.plugins.pylsp_mypy.strict" t t)

     ;;
     ;; // --- pylsp_mypy settings ---
     ;;   // Execute via `dmypy run` rather than `mypy`. This uses the `dmypy` daemon and may dramatically improve the
     ;;   // responsiveness of the `pylsp` server, however this currently does not work in `live_mode`. Enabling this
     ;;   // disables `live_mode`, even for conflicting configs.
     ;;   "pylsp.plugins.pylsp_mypy.dmypy": false,
     ;;   // Provide type checking as you type. This writes to a tempfile every time a check is done. Turning off
     ;;   // `live_mode` means you must save your changes for mypy diagnostics to update correctly.
     ;;   "pylsp.plugins.pylsp_mypy.live_mode": true,
     ;;   // Refers to the `strict` option of `mypy`. This option often is too strict to be useful.
     ;;   "pylsp.plugins.pylsp_mypy.strict": false,
     ))

  ;; Disable the deprecated Microsoft Python language server
  (add-to-list 'lsp-disabled-clients 'mspyls)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.tox\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pytest_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.expo\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.log\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.clj-kondo\\'")

  ;;   ;; (add-hook 'web-mode-hook (lambda ()
  ;;   ;;                            (rainbow-mode)
  ;;   ;;                            (setq! +format-with-lsp nil)))

  ;; Don't include snippets in completions by default.
  ;; (setq! +lsp-company-backends 'company-capf)
  ;; (add-to-list '+company-backend-alist '(prog-mode :derived (:separate company-capf)))
  )

;; Workaround for https://github.com/doomemacs/doomemacs/issues/8541
(let ((lfile (concat doom-local-dir "straight/repos/transient/lisp/transient.el")))
  (if (file-exists-p lfile)
      (load lfile)))

(after! magit
  ;;   (add-hook! 'magit-pre-refresh . diff-hl-magit-pre-refresh)
  ;;   (add-hook! 'magit-post-refresh . diff-hl-magit-post-refresh)
  (map! :map magit-mode-map
        :nv "z" 'magit-stash)
  ;; TODO: Was this changed to magit-section-disable-line-numbers
  ;; (setq! magit-disable-line-numbers nil)
  ;; (setq! magit-git-debug t)
  ;; remove some hooks from the magit status screen so it loads a bit faster
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  )

(after! diff-hl
  ;; Shows uncommitted changes in the fringe
  (global-diff-hl-mode 1))

(after! treemacs
  (setq! treemacs-load-them "Idea"))

;; (use-package! flyover
;;   :defer t
;;   :hook (prog-mode . flyover-mode)
;;   :config
;;   (setq flyover-levels '(error warning info)  ; Show all levels
;;         flyover-use-theme-colors t
;;         flyover-display-mode 'show-only-on-same-line
;;         ;; flyover-show-at-eol nil
;;         ;; flyover-checkers '(flycheck flymake)
;;         ;; flyover-checkers '(flymake)
;;         ;; flyover-virtual-line-type 'straight-arrow
;;         flyover-virtual-line-icon "──► ")
;;   ;; (setq eglot-stay-out-of '(flymake))
;;   )


;; Flymake mode
;; (setq flymake-show-diagnostics-at-end-of-line t
;;       flymake-popon-mode -1)
;; (after! flymake-mode
;;   )
;; (add-hook 'flymake-mode (lambda () (flymake-popon-mode -1)))
;; (add-hook 'flymake-popon-mode (lambda () (flymake-popon-mode -1)))

(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.ftl.?\\'" . web-mode))
  ;; (add-to-list '+format-on-save-enabled-modes 'html-mode t)
  (add-to-list 'web-mode-engines-alist '("freemarker" . "\\.ftl.?\\'")))

(add-hook 'web-mode (lambda ()
                      (rainbow-mode)
                      (setq! +format-with-lsp nil)))

(after! vterm
  (set-popup-rule! "^vterm" :ignore t)
  ;; Doom hides the modeline for vterm by default
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode)
  (map! :map vterm-mode-map
        :i [tab] 'vterm-send-tab
        "C-<escape>" 'vterm-send-escape)

  ;; Switch to normal mode when the process in a vterm buffer exits. The
  ;; lambda is required so that the arguments don't get passed to
  ;; evil-normal-state
  ;; (add-to-list 'vterm-exit-functions (lambda (&rest r) (evil-normal-state)))
  )

;; clojure-mode
(after! clojure-mode
  ;; Add cider-edit-jack-in-command to safe local variables list so we don't get a message
  ;; when this variable is in a projects dir locals.
  (add-to-list 'safe-local-variable-values '(cider-edit-jack-in-command . t))
  (add-to-list 'safe-local-variable-values '(lsp-clojure-custom-server-command . "/usr/bin/clojure-lsp"))
  ;; TODO: Why does this get added to the 'theme-value property of safe-local-varialbes values instead of added to the list
  ;;(add-to-list 'safe-local-variable-values '(lsp-clojure-custom-server-command . "/home/brett/bin/clojure-lsp-emacs"))

  ;; Wrap comments past the 90th column
  (setq! fill-column 90)

  ;; Node: This was from Beacon but I'm leaving it here just in case
  ;; (put-clojure-indent 'defresolver :defn)

  ;; TODO: font lock for defresolver
  (font-lock-add-keywords 'clojure-mode
                          `((,(concat "(\\(?:\.*/\\)?"
                                      (regexp-opt '("defresolver") t)
                                      "\\>")
                             1 font-lock-builtin-face)))

  ;; Don't use rainbow delimeters since we set the parens to a lighter color in the theme
  ;; (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

  (map! :map clojure-mode-map
        :nv ">" 'sp-forward-slurp-sexp
        :nv "<" 'sp-forward-barf-sexp
        :nv ", n s" 'clojure-sort-ns))

(add-hook 'clojure-mode (lambda ()
                          (setq devdocs-current-docs '("clojure~1.11"))))

;; (use-package! clojure-ts-mode
;;   :config
;;   ;; There's an existing hook defined somewhere, not sure where, that calls calls cider
;;   ;; mode but tends to toggle it off it its already enabled
;;   (map! :map clojure-ts-mode-map
;;         :nv ">" 'sp-forward-slurp-sexp
;;         :nv "<" 'sp-forward-barf-sexp
;;         :nv ", n s" 'clojure-sort-ns)
;;   )

(after! cider
  ;; See https://github.com/practicalli/doom-emacs-config/blob/main/%2Bclojure.el
  ;; although some settings already incorporated into doom
  ;; :after '(clojure-mode clojure-ts-mode)
  ;; :after (clojure-mode pendant)
  ;; :custom
  (setq! cider-show-error-buffer t)               ;'only-in-repl
  (setq! cider-use-xref nil) ;; "Use lsp"
  (setq! cider-print-fn 'puget) ;; "Pretty printing with sorted keys / set values"
  (setq! cider-result-overlay-position 'at-point) ;; "Results shown right after expression"
  (setq! cider-repl-buffer-size-limit 100) ;; "Limit lines shown in REPL buffer"
  (setq! cider-repl-history-size 42)
  (setq! cider-enable-nrepl-jvmti-agent t) ;; "Start JVM with -Djdk.attach.allowAttachSelf"
  (setq! cider-download-java-sources t)
  ;;   (set-keymap-parent clojure-ts-mode-map clojure-mode-map)
  ;;   (set-keymap-parent clojure-ts-clojurescript-mode-map clojurescript-mode-map)
  ;;   (set-keymap-parent clojure-ts-clojurec-mode-map clojurec-mode-map)
  )

(after! treesit
  ;; :defer t
  ;; :config
  (add-to-list 'treesit-extra-load-path "/home/brett/.emacs.d/.local/cache/tree-sitter"))

;;
;; Custom packages, see ./packages.el
;;

(use-package! ba
  :load-path doom-user-dir
  :config
  (map!
   :leader
   :nv "o 1" (defun ba/switch-to-vterm-1 () (interactive) (ba/switch-to-vterm "vterm<1>"))
   :nv "o 2" (defun ba/switch-to-vterm-2 () (interactive) (ba/switch-to-vterm "vterm<2>"))
   :nv "o 3" (defun ba/switch-to-vterm-3 () (interactive) (ba/switch-to-vterm "vterm<3>"))
   :nv "o 4" (defun ba/switch-to-vterm-4 () (interactive) (ba/switch-to-vterm "vterm<4>"))))

(use-package! banzai
  :load-path doom-user-dir
  :config
  (add-hook 'after-save-hook 'auto-eval-sql-clj-on-hug-sql-edit)
  (pendant-setup-key-bindings)
  (banzai-at-work-setup-key-bindings))

(use-package! claude-code-ide
  ;; :defer-incrementally (transient)
  :defer t
  :config
  ;; (setq claude-code-ide-terminal-backend 'eat)
  ;; Optionally enable Emacs MCP tools
  ;; TODO: Maybe this needs a hook to run in prog mode;
  (claude-code-ide-emacs-tools-setup)
  (map! :leader
        :g "i c" 'claude-code-ide-menu))

(use-package dir-config
  ;;  :ensure t
  :custom
  (dir-config-file-names '(".dir-config.el"))
  (dir-config-allowed-directories '("~/devel"))
  :config
  (dir-config-mode))

(use-package! evil-matchit
  :defer t
  :config
  (global-evil-matchit-mode 1))

(use-package! sqlformat
  :defer t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  ;; TODO: Probably better to set sqlformat-on-save-mode per project
  ;; :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

;; (use-package! casual-dired
;;   ;; :ensure t
;;   :defer t
;;   :map  dired-mode-map

;;   :config
;;   (map! :map dired-mode-map
;;         :m "?" 'casual-dired-tmenu)

;;   ;; :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu))
;;   )


;; (use-package! smart-mode-line
;;   :config
;;   (sml/setup))

;; (use-package! mini-modeline
;;   :after (smart-mode-line)
;;   :config
;;   (setq mini-modeline-mode t
;;         mini-modeline-display-gui-line nil
;;         mini-modeline-enhance-visual t
;;         ;; Hide minior modes from the mini modeline
;;         rm-whitelist ""
;;         mini-modeline-face-attr '(:background "gray95"))

;; (setq rm-blacklist ""
;;       ;; (format "^ \\(%s\\)$"
;;       ;;         (mapconcat #'identity
;;       ;;                    '("Fly.*" "Projectile.*" "PgLn" "ws" "Minimode" "WK" "better-jumper" "EGkj" "snipe"
;;       ;;                      "GCMH" "wb" "DirCfg" "yas" "envrc[none]" "SP" "Outl" "ElDoc" "Apheleia")
;;       ;;                    "\\|"))
;;       )
;;)
;;
;;
;;
;;

(use-package parenthesis-face
  :defer t
  :after (:any clojure-mode clojure-ts-mode lisp-mode)
  :hook (clojure-mode . parenthesis-face-mode)
  :config
  (custom-set-faces '(parenthesis-face ((t :foreground "gray60")))))


(use-package evil-cleverparens
  :hook ((clojure-mode . evil-cleverparens-mode)
         (lisp-mode . evil-cleverparens-mode)
         (evil-cleverparens-mode . smartparens-strict-mode)
         ;; smartparents strict mode gets confused when deleting blocks of text in
         ;; visual mode, especially with blank lines and even in comment blocks
         (evil-visual-state-entry . (lambda () smartparens-strict-mode 1))
         (evil-visual-state-exit . (lambda () smartparens-strict-mode -1)))
  :config
  (require 'evil-cleverparens-text-objects))

;; (use-package! indent-bars
;;   ;; :defer t
;;   :custom
;;   (indent-bars-treesit-support t))

;; (use-package! inf-janet
;;   :config
;;   (setq! inf-janet-program "/usr/local/bin/janet"))

(use-package! nvm
  :defer t
  :hook (projectile-after-switch-project . (lambda ()
                                             (let ((filename (expand-file-name ".nvmrc" (projectile-project-root))))
                                               (when (file-exists-p filename)
                                                 (nvm-use-for filename))))))
