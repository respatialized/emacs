;; MELPA setup

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Xah-Fly-Keys setup
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
(xah-fly-keys 1)

;; Enabling the theme
(load-theme 'ample-zen)

;; Setting font and line-height defaults
(setq line-spacing 0.45)
(set-face-attribute 'default t :font "Fira Mono-13")

;; Setting up Org-mode

