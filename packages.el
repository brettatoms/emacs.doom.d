;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! prettier-js)

(package! paren-face :recipe (:host github :repo "tarsius/paren-face"))
(package! evil-cleverparens)
(package! evil-matchit)
(package! modus-themes :recipe (:host github :repo "protesilaos/modus-themes"))
(package! nvm :recipe (:host github :repo "rejeep/nvm.el"))

(package! devdocs :recipe (:host github :repo "astoff/devdocs.el"))

(package! sqlformat :recipe (:host github :repo "purcell/sqlformat"))

;; Didn't give this much of a try since I had ranger installed which took over
;; the dired sessions.
;; (package! casual-dired :recipe (:host github :repo "kickingvegas/casual-dired"))

(package! dir-config)

(package! rainbow-delimiters
  :recipe (:host github
           :repo "Fanael/rainbow-delimiters"))

;; ;; TODO: some clojure snippets are included in doom/snippets and we don't really
;; ;; use snippets much so maybe we don't really need these
(package! clojure-snippets :recipe (:host github :repo "mpenet/clojure-snippets"))

(package! claude-code-ide
  :recipe (:host github
           :repo "manzaltu/claude-code-ide.el"))

;; (package! eca
;;   :recipe (:host github
;;            :repo "editor-code-assistant/eca-emacs"
;;            :files ("*.el")))

(package! eat
  :recipe (:host codeberg
           :repo "akib/emacs-eat"
           ;; :files ("*.el")
           ))

(package! transient :pin "053d56e4de2dd78bf32f7af7ed5f289a91cdb6ac")
(package! magit :pin "b828afbb4b45641998fb6483a08effb1efb214e1")

;; (package! mini-modeline
;;   :recipe (:host github
;;            :repo "kiennq/emacs-mini-modeline"))

;; (package! smart-mode-line
;;   :recipe (:host github
;;            :repo "Malabarba/smart-mode-line"))
