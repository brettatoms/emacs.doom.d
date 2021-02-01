;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! evil-collection)
(package! prettier-js)
(package! graphql-mode)

;; The doom python module doesn't seem to be installing blacken
(package! blacken)
;; Fix issue with newline-and-indent in (web-mode-point-context) for tsx files
(package! web-mode :pin "a3ce21f795e03c7a5489a24b2b3c4fce2d7a2f59")
