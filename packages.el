;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! prettier-js)

(package! paren-face :recipe (:host github :repo "tarsius/paren-face"))
(package! evil-cleverparens)
(package! modus-themes :recipe (:host github :repo "protesilaos/modus-themes"))
(package! nvm :recipe (:host github :repo "rejeep/nvm.el"))

;; (package! verb :recipe (:host github :repo "federicotdn/verb"))

(package! request :recipe (:host github :repo "tkf/emacs-request"))

;; TODO: Tryout symex, a "a vim-inspired way of editing Lisp code as trees"
;; (package! symex :recipe (:host github :repo "countvajhula/symex.el"))

(package! devdocs :recipe (:host github :repo "astoff/devdocs.el"))

(package! command-log-mode :recipe (:host github :repo "lewang/command-log-mode"))

;; (package! bookmark-in-project :recipe (:host gitlab :repo "ideasman42/emacs-bookmark-in-project"))

;; (package! ejc-sql :recipe (:host github :repo "kostafey/ejc-sql"))

(package! sqlformat :recipe (:host github :repo "purcell/sqlformat"))

(package! evil-matchit :pin "7a2a7384b6e752a805d6fbb78cf9425e54c2b18b")
(package! binky-node :recipe (:host github :repo "liuyinz/binky-mode"))

;; For Emacs 28.x only.  Use built-in tree-sitter for >= 29.x
;; (package! tree-sitter :pin "d3eab879e9b0ccc9582113d81a93ad06f3c6bfb1")
;; (package! tree-sitter-langs :pin "1076cf2366be8ef1bd6fd7239f92f16cc0890fce")

;; Later versions of transient and with-editor are using defvar-keymap and some other changes
;; that were causing some problems
;; (package! transient :pin "c2bdf7e")
;; (package! with-editor :pin "e5ba1ca")

(package! inf-clojure :recipe (:host github :repo "clojure-emacs/inf-clojure"))


;; -------------------------------------------------------------------
;; Below here are packages that are normally updated with doom emacs.
;; -------------------------------------------------------------------

;; (package! cider :pin "9c605cd4938c0055c2766c55606f19ecbf664e8e")  ;; cider 1.11.1
;; (package! clojure-mode :pin "525fc1b131b1fc537aa82d83d9eb2ea833cface6") ;; clojure-mode 5.18.0
(package! logview) ;; optional dependency for new cider log mode
;; ;; TODO: some clojure snippets are included in doom/snippets and we don't really
;; ;; use snippets much so maybe we don't really need these
(package! clojure-snippets :recipe (:host github :repo "mpenet/clojure-snippets"))

;; Projectile 2.8 until Doom catches up
;; (package! projectile :pin "971cd5c4f25ff1f84ab7e8337ffc7f89f67a1b52")

;; (package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh"))

;; TODO: eat is a terminal emulator that might be worth trying
;; (package! eat)
