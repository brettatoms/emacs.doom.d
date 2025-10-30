;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

;; TODO: If nongnu-elpa isn't working then uncomment this:
;; (defadvice! straight-use-recipes-ignore-nongnu-elpa-a (fn recipe)
;;   :around #'straight-use-recipes
;;   (unless (eq 'nongnu-elpa (car recipe))
;;     (funcall fn recipe)))

(doom! :completion
       (corfu
        +icons
        +orderless
        ;; +dabbrev
        )

       ;;helm             ; the *other* search engine for love and life
       ;;ido              ; the other *other* search engine...
       (vertico
        +icons
        ;; +childframe
        )

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       indent-guides     ; highlighted indent columns

       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       (modeline          ; snazzy, Atom-inspired modeline, plus API
        ;; +light
        )

       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; display visual hints when editing in evil
       (popup            ; tame sudden yet inevitable temporary windows
        ;; +all             ; catch all popups that start with an asterix
        +defaults
        )       ; default popup rules
       smooth-scroll     ; So smooth you won't believe it's not butter
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       (vc-gutter         ; vcs diff in the fringe
        ;; +pretty
        )
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers)    ; visually switch windows
       ;; workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil             ; come to the dark side, we have cookies
        +everywhere)
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format           ; automated prettiness
        +onsave
        ;; +lsp ;; use the lsp formatter by default rather than apheleia
        )
       ;;god               ; run Emacs commands without modifier keys
       ;; lispy             ; vim for lisp, for people who dont like vim
       ;;objed             ; text object editing for the innocent
       multiple-cursors  ; editing in many places at once
       ;; (parinfer          ; turn lisp into python, sort of
       ;;  +rust)
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       (whitespace       ; a butler for your whitespace
        +guess
        +trim)
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired            ; making dired pretty [functional]
        +dirvish
        +icons)          ; colorful icons for dired-mode
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       tramp             ; remote files at your arthritic fingertips
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax              ; tasing you for every semicolon you forget
        +childframe
        +icons           ;; Use unicode icons rather than ASCII prefixes in error tooltips or childframes.
        )
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval              ; run code, run (also, repls)
        +overlay)
       llm
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (lsp
        ;; +eglot
        ;; +booster
        )
       (magit             ; a git porcelain for Emacs
        ;;+forge
        )
       make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;; (terraform          ; infrastructure as code
       ;;  +lsp)
       ;;tmux              ; an API for interacting with tmux
       tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;cc                ; C > C++ == 1
       (clojure            ; java with a lisp
        +lsp
        ;; +tree-sitter
        )
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;; (dart +flutter   ; paint ui and not much else
       ;;       +lsp)
       ;;dhall
       ;; (elixir            ; erlang done right
       ;;  +lsp
       ;;  +tree-sitter)
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       (go +lsp)           ; the hipster dialect
       (graphql +lsp)    ; Give queries a REST
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       json                ; At least it ain't XML
       (java +lsp)         ; the poster child for carpal tunnel syndrome
       (javascript         ; all(hope(abandon(ye(who(enter(here))))))
        +lsp
        +tree-sitter)
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix               ; I hereby declare "nix geht mehr!"
        +lsp
        +tree-sitter)
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        ;; +present         ; Emacs for presentations
        +pretty
        ;; +roam2
        )
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python            ; beautiful is better than ugly
        +pyenv
        +lsp
        ;; NOTE: Need to npm i -g pyright
        ;; +pyright
        +tree-sitter
        )
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       rest                ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        +lsp
        +tree-sitter)
       ;; (scala              ; java, but good
       ;;  +lsp)
       (scheme             ; a fully conniving family of lisps
        +guile)
       (sh                 ; she sells (ba|z|fi)sh shells on the C xor
        +fish)
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web                ; the tubes
        +lsp
        +tree-sitter)
       yaml
       (zig               ; C, but simpler
        +lsp
        +tree-sitter)

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;; irc               ; how neckbeards socialize
       ;; (rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens))
