;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;;;

;; To reenable font smoothing on Mac
;; defaults write org.gnu.Emacs CGFontRenderingFontSmoothingDisabled -bool NO
;;
;; To control anti-aliasing on OSX:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0 (none)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 1 (light)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 2 (medium)
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 3 (strong)
;;

;; Maximize frame on startup
(unless (frame-parameter nil 'fullscreen)
  (toggle-frame-maximized))

(setq doom-font "CommitMono:size=18"
      ;; doom-font "Source Code Pro Medium:size=18"  ;; test live with set-frame-font
      ;; doom-font "MesloLGS NF:size=17"
      ;; doom-font "Fira Code Retina:size=18" ;; ok with line-spacing=0.1
      ;; doom-font "Fira Code Retina:size=17"
      ;;

      doom-theme 'modus-operandi
      ;; doom-theme 'doom-one-light
      ;; doom-theme 'doom-opera-light
      ;; doom-theme 'kaolin-light
      ;; doom-theme 'doom-tomorrow-day
      ;; doom-theme 'doom-homage-white
      ;; doom-theme nil
      ;; solaire-mode nil
      ;; solaire-global-mode +1
      inhibit-startup-screen t
      doom-localleader-key ","

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
      ;;
      enable-local-variables t

      ;; Performance improvements from
      ;; https://readingworldmagazine.com/emacs/2021-08-11-2021-08-11-emacs-snitch-and-how-to-improve-performance/
      ns-use-srgb-colorspace t

      ;; Don't lock files.
      create-lockfiles nil

      ;; Set jit-lock-defer-time to 0 to defer fontification until there is not
      ;; pending input.
      jit-lock-defer-time 0

      ;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
      cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"

      auth-sources '("~/.authinfo")

      mac-command-modifier 'ctrl
      mac-control-modifier 'meta
      mac-option-modifier 'meta

      evil-escape-key-sequence "kj"

      ;; For faster rending in large files
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t
      global-so-long-mode 1

      ;; Always create a new buffer for an async shell comamnd if the default buffer
      ;; has a process attached.
      async-shell-command-buffer 'new-buffer)

(use-package! doom-themes
  :defer t
  :config
  (custom-theme-set-faces! 'doom-one-light
    '(font-lock-function-name-face :foreground "steelblue")
    '(font-lock-variable-name-face :foreground "steelblue4")

    ;; The parenthesis face is provided by https://github.com/tarsius/paren-face
    '(parenthesis :foreground "darkgrey")

    ;; Fix output of some commands (e.g. graphql-codegen errors) where the text
    ;; color matches the background color. I'm not sure why setting the background
    ;; here fixes the font color
    '(vterm-color-black :background "dark-grey")
    '(elfeed-search-title-face :height 200)
    )

  (custom-theme-set-faces! 'doom-opera-light
    '(font-lock-variable-name-face :foreground "steelblue4")
    '(font-lock-function-name-face :foreground "navy")
    '(font-lock-type-face :foreground "DarkOrange4")
    '(clojure-keyword-face :foreground "maroon")
    '(font-lock-comment-face :foreground "#888888")
    '(font-lock-doc-face :foreground "#888888")
    '(vertico-current :background "alice blue")
    '(region :background "snow2")
    '(line-number :background "white" :foreground "dark gray")

    ;; The parenthesis face is provided by https://github.com/tarsius/paren-face
    '(parenthesis :foreground "darkgrey")))

(use-package! modus-themes
  :config
  (setq modus-themes-custom-auto-reload t
        modus-operandi-palette-overrides
        '((bg-main "gray98")
          (fg-main "gray10")
          (bg-hl-line "azure2")
          (bg-region "gray90")
          (fg-region :inherit)
          (comment "gray50"))

        modus-themes-common-palette-overrides
        `(;; From the section "Make the mode line borderless"
          ;; (border-mode-line-active unspecified)
          ;; (border-mode-line-inactive unspecified)
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)

          (bg-added bg-green-nuanced)
          (bg-added-faint bg-green-nuanced)
          (bg-added-refine bg-green-nuanced)

          (bg-removed bg-red-nuanced)
          (bg-removed-faint bg-red-nuanced)
          (bg-removed-refine bg-red-nuanced)
          ;; (fg-diff)

          ;;  use header colors of the intense preset
          ;; (bg-heading-0 unspecified)
          ;; (bg-heading-1 bg-magenta-nuanced)
          ;; (bg-heading-2 bg-red-nuanced)
          ;; (bg-heading-3 bg-blue-nuanced)
          ;; (bg-heading-4 bg-cyan-nuanced)
          ;; (bg-heading-5 bg-green-nuanced)
          ;; (bg-heading-6 bg-yellow-nuanced)
          ;; (bg-heading-7 bg-red-nuanced)
          ;; (bg-heading-8 bg-magenta-nuanced)

          ;; (overline-heading-0 unspecified)
          ;; (overline-heading-1 magenta-cooler)
          ;; (overline-heading-2 magenta-warmer)
          ;; (overline-heading-3 blue)
          ;; (overline-heading-4 cyan)
          ;; (overline-heading-5 green)
          ;; (overline-heading-6 yellow-cooler)
          ;; (overline-heading-7 red-cooler)
          ;; (overline-heading-8 magenta)

          (bg-paren-match bg-magenta-intense)

          ;; And expand the preset here.  Note that the ,@ works because
          ;; we use the backtick for this list, instead of a straight
          ;; quote.
          ;; ,@modus-themes-preset-overrides-cooler
          ;; ,@modus-themes-preset-overrides-faint
          ,@modus-themes-preset-overrides-warmer
          ;; ,@modus-themes-preset-overrides-intense
          ))

  (custom-set-faces
   '(mode-line ((t :background "gray90"
                   :box (:line-width 4
                         :color "gray90"))))
   '(mode-line-inactive ((t :background "gray80"
                            :box (:line-width 4
                                  :color "gray80"))))
   '(magit-section-heading ((t :foreground "medium blue")))
   '(magit-diff-file-heading ((t :foreground "gray10")))
   '(cider-test-success-face ((t :foreground "gray90" :background "green")))
   )

  (load-theme 'modus-operandi :no-confirm))

(use-package! doom-modeline
  :defer t
  :config
  (setq doom-modeline-lsp nil
        doom-modeline-time nil ;; Not necessary since doom-modeline respects display-time-mode
        ;; doom-modeline-height 20 ;; This gets overridden by the char size
        doom-modeline-repl nil
        doom-modeline-lsp nil
        doom-modeline-debug nil
        all-the-icons-scale-factor 1.1
        ))

(use-package! flycheck
  :defer t
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-ruff))


(use-package! typescript-mode
  ;; typescript-mode + tree-sitter seems to work better than the web-mode derived typescript-tsx-mode
  :defer t
  :mode "\\.tsx\\'"
  :config
  ;; (require 'tree-sitter)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx))
  )

(use-package! request
  :defer t)

(use-package! evil-matchit
  :defer t
  :config
  (global-evil-matchit-mode 1))

;; clean up whitespace in prog modes
;; (add-hook 'prog-mode-hook #'ws-butler-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (ws-butler-mode)
            (add-hook 'before-save-hook #'delete-trailing-whitespace)))

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

;; Reload dir-locals on save
;; (add-hook 'emacs-lisp-mode-hook
;;           (defun enable-autoreload-for-dir-locals ()
;;             (when (and (buffer-file-name)
;;                        (equal dir-locals-file
;;                               (file-name-nondirectory (buffer-file-name))))
;;               (add-hook (make-variable-buffer-local 'after-save-hook)
;;                         'my-reload-dir-locals-for-all-buffer-in-this-directory))))


(after! envrc
  (setq envrc-global-mode 1)
  (defun ba/envrc-allow-and-reload-all ()
    (interactive)
    (envrc-allow)
    (envrc-reload-all)))

(after! yasnippet
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              ;; TODO: move this to ~/emacs/snippets
              (yas-load-directory "~/.emacs.snippets.d"))))

(after! evil-collection
  (evil-collection-init 'package-list))

(after! json
  (add-hook 'json-mode-hook
            (lambda ()
              ;; The json-python-json checker has an issue where pyenv-exec errors
              ;; and causes high CPU usage.
              (flycheck-disable-checker 'json-python-json))))



(after! sql
  (set-popup-rule! "^\*SQL" :ignore t)
  (setq sql-debug-send t
        sql-send-terminator t))


(use-package! sqlformat
  :defer t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  ;; TODO: Probably better to set sqlformat-on-save-mode per project
  ;; :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

(use-package! lsp
  :config
  ;; (add-to-list 'lsp-disabled-clients '(web-mode . ()))
  (add-to-list 'lsp-language-id-configuration '(web-mode . "html"))
  ;; Seee https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-ui-sideline-mode nil
        lsp-ui-sideline-enable nil

        ;; Stay out of my modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil

        ;; lsp-ui-mode nil
        ;; lsp-headerline-breadcrumb-enable nil
        ;; lsp-lens-enable nil
        ;; lsp-signature-auto-activate t
        ;; lsp-signature-render-documentation t
        ;; lsp-eldoc-enable-hover t
        ;; lsp-eldoc-render-all t

        ;; Disabling lsp-auto-configure is the only way I could get lsp-mode to not
        ;; turn on lsp-ui by default.
        ;; lsp-auto-configure nil

        ;; Fix potential lockups
        ;; https://github.com/hlissner/doom-emacs/issues/4093
        ;; Update 11/17/2022: Disabled  b/c of "too many open files" error
        lsp-enable-file-watchers nil

        ;; +format-with-lsp causes problems with undo in web-mode,
        ;; could also do (setq-hook! 'web-mode-hook +format-with-lsp nil)
        ;; +format-with-lsp nil

        ;; lsp-before-save-edits nil

        ;; It seems like the lsp client was still trying to use flake8 even
        ;; though its disabled in the custom settings
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pyflakes-enabled nil
        ;; lsp-log-io t
        )

  ;; (setq lsp-pylsp-server-command '("pylsp" "--verbose"))


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
      :nv "r q" 'query-replace)


(map! :map prog-mode-map
      :i [tab] 'company-indent-or-complete-common)

(use-package! compat)

(use-package! with-editor
  :defer t
  :after compat)

(use-package! magit
  :defer t
  :after with-editor
  :hook
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (map! :map magit-mode-map
        :nv "z" 'magit-stash)
  (setq magit-disable-line-numbers nil)
  ;; remove some hooks from the magit status screen so it loads a bit faster
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

(after! diff-hl
  ;; Shows uncommitted changes in the fringe
  (global-diff-hl-mode 1))

;; (map! :map evil-magit-section-map
;;       :nv "z" 'magit-stash)

(use-package! web-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ftl.?\\'" . web-mode))
  ;; (add-to-list '+format-on-save-enabled-modes 'html-mode t)
  :hook (web-mode . (lambda ()
                      (rainbow-mode)
                      (setq +format-with-lsp nil)
                      ;; (when (string= web-mode-engine "freemarker")
                      ;;   (setq comment-start "<#--" )
                      ;;   (setq comment-end "-->" ))
                      ))

  :config
  (add-to-list 'web-mode-engines-alist '("freemarker" . "\\.ftl.?\\'")))

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


(use-package! vterm
  ;; :defer t
  :config
  ;; override doom's default vterm popup rule
  (set-popup-rule! "^vterm" :ignore t)
  ;; Doom hides the modeline for vterm by default
  (remove-hook 'vterm-mode-hook #'hide-mode-line-mode)
  (map! :map vterm-mode-map
        :i [tab] 'vterm-send-tab)
  (map!
   :leader
   :nv "o 1" (defun ba/switch-to-vterm-1 () (interactive) (ba/switch-to-vterm "vterm<1>"))
   :nv "o 2" (defun ba/switch-to-vterm-2 () (interactive) (ba/switch-to-vterm "vterm<2>"))
   :nv "o 3" (defun ba/switch-to-vterm-3 () (interactive) (ba/switch-to-vterm "vterm<3>"))
   :nv "o 4" (defun ba/switch-to-vterm-4 () (interactive) (ba/switch-to-vterm "vterm<4>")))
  ;; Switch to normal mode when the process in a vterm buffer exits. The
  ;; lambda is required so that the arguments don't get passed to
  ;; evil-normal-state
  ;; (add-to-list 'vterm-exit-functions (lambda (&rest r) (evil-normal-state)))
  )


(use-package! paren-face
  :config
  (global-paren-face-mode 1))


(use-package! clojure-mode
  :defer t
  :config
  ;; Add cider-edit-jack-in-command to safe local variables list so we don't get a message
  ;; when this variable is in a projects dir locals.
  (add-to-list 'safe-local-variable-values '(cider-edit-jack-in-command . t))
  ;; TODO: Why does this get added to the 'theme-value property of safe-local-varialbes values instead of added to the list
  (add-to-list 'safe-local-variable-values '(lsp-clojure-custom-server-command . "/home/brett/bin/clojure-lsp-emacs"))
  ;; Wrap comments past the 90th column
  (setq fill-column 90)

  (put-clojure-indent 'defresolver :defn)

  ;; TODO: font lock for defresolver
  (font-lock-add-keywords 'clojure-mode
                          `((,(concat "(\\(?:\.*/\\)?"
                                      (regexp-opt '("defresolver") t)
                                      "\\>")
                             1 font-lock-builtin-face)))

  ;; Don't use rainbow delimeters since we set the parens to a lighter color in the theme
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(map! :map clojure-mode-map
      :nv ", n s" 'clojure-sort-ns)

(use-package! cider
  ;; See https://github.com/practicalli/doom-emacs-config/blob/main/%2Bclojure.el
  ;; although some settings already incorporated into doom
  :after clojure-mode
  :config
  (setq cider-show-error-buffer t               ;'only-in-repl
        cider-use-xref nil                      ; use lsp
        cider-print-fn 'puget                   ; pretty printing with sorted keys / set values
        cider-result-overlay-position 'at-point ; results shown right after expression
        cider-repl-buffer-size-limit 100        ; limit lines shown in REPL buffer
        cider-repl-history-size 42
        ;; cider-enrich-classpath t
        ))

(use-package! evil-cleverparens
  :defer t
  :after clojure-mode
  :hook ((clojure-mode . evil-cleverparens-mode)
         (lisp-mode . evil-cleverparens-mode)
         (evil-cleverparens-mode . smartparens-strict-mode)
         ;; smartparents strict mode gets confused when deleting blocks of text in
         ;; visual mode, especially with blank lines and even in comment blocks
         (evil-visual-state-entry . turn-off-smartparens-strict-mode)
         (evil-visual-state-exit . turn-off-smartparens-strict-mode))
  :config
  (require 'evil-cleverparens-text-objects))

;; (use-package! inf-janet
;;   :config
;;   (setq inf-janet-program "/usr/local/bin/janet"))

(use-package! nvm
  :defer t
  :config
  ;; Load the project .nvmrc when the project is switched
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (let ((filename (f-expand ".nvmrc" (projectile-project-root))))
                (when (f-exists-p filename)
                  (nvm-use-for filename))))))

