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
(setq line-spacing 0.25)
(set-face-attribute 'default t :font "Fira Mono-13")

;; Setting up Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/personal.org"
                             "~/org/work.org" 
                             "~/org/built-environments.org"
			     "~/org/programming-math.org"))

(setq org-src-fontify-natively t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/home/andrew/.conda")
 '(custom-safe-themes
   (quote
    ("15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
 '(package-selected-packages
   (quote
    (cider conda pyenv-mode-auto anaconda-mode pyenv-mode s dash-functional dash flatui-theme zenburn-theme xah-fly-keys ample-zen-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cl)

; ob-ipython
(require 'ob-ipython)
(setq ob-ipython-command "ipython3")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (clojure . t)
   (python . t)))



(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)

; ob-clojure
(setq org-babel-clojure-backend 'cider)
(require 'ob-clojure)
(require 'cider)
