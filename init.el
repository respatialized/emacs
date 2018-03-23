;;; Package --- Summary

  ;;; Commentary:
  ;; Emacs init file responsible for either loading a pre-compiled configuration file
  ;; or tangling and loading a literate org configuration file.

  ;;; Code:

  ;; Don't attempt to find/apply special file handlers to files loaded during startup.
  (let ((file-name-handler-alist nil))
     ;; If config is pre-compiled, then load that
     (if (file-exists-p (expand-file-name "newinit.elc" user-emacs-directory))
         (load-file (expand-file-name "newinit.elc" user-emacs-directory))
       ;; Otherwise use org-babel to tangle and load the configuration
       (require 'org)
       (org-babel-load-file (expand-file-name "newinit.org" user-emacs-directory))))

;; init.el ends here

;;; package --- Summary
;;; Commentary:
; This file is not a part of GNU Emacs.

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'diminish)
  (package-install 'diminish))
(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package xah-fly-keys
  :ensure t
  :pin melpa
  :config ; custom keybindings for common commands I use.
  (xah-fly-keys 1)
  (xah-fly-keys-set-layout "qwerty")
  (define-key xah-fly-leader-key-map
   (kbd "z") 'comment-region)
  (define-key xah-fly-leader-key-map
   (kbd "w") 'org-agenda)
  (define-key xah-fly-leader-key-map
   (kbd "<backtab>") 'yas-expand)
  (define-key xah-fly-leader-key-map
   (kbd "c") 'org-capture)
  (define-key xah-fly-leader-key-map
   (kbd "b") 'sdcv-search-pointer)
  (define-key xah-fly-leader-key-map
   (kbd "5") 'split-window-vertically)
)

(use-package magit
  :ensure t
  :pin melpa)

(use-package ghub
  :ensure t
  :pin melpa)

(use-package magithub
  :ensure t
  :pin melpa
  :config
  (setq magithub-clone-default-directory "~/repos_external")
)

(use-package ein
  :pin melpa
  :ensure t
)

(use-package cider
  :pin melpa
  :ensure t
)

(use-package ensime
  :ensure t
  :pin melpa
  :config
  (setq ensime-sbt-command "/usr/bin/sbt"
    sbt:program-name "/usr/bin/sbt")
  (setq ensime-startup-notification nil)
)

(use-package sbt-mode
  :pin melpa)
(use-package scala-mode
  :pin melpa)

(use-package yaml-mode
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

(use-package graphviz-dot-mode
  :pin melpa
)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (clojure . t)
   (shell .t)
   (ditaa .t)
   (dot . t)
   (python . t)
   (scala . t)))

(setq org-src-tab-acts-natively t)


(use-package ob-clojure
  :pin melpa
  :config
  (setq org-babel-clojure-backend 'cider)
)

(use-package org
  :ensure org-plus-contrib
  :defer t)

(setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/upcoming.org" :maxlevel . 2)
                           ("~/repos_main/orgbrain/decks.org" :maxlevel . 2)))
(setq org-outline-path-complete-in-steps t)
(setq org-refile-use-outline-path 'file)


(setq org-todo-keywords '(
  (sequence "TODO(t)" "WAITING(w)" "EVENT(e)" "PROJECT(p)" "GOAL(g)" "|"
 "COMPLETE(d)" "CANCELLED(c)" "PAST EVENT(o)" "NOTE(n)" "COMPLETE PROJECT(q)")
  (sequence "NOT GROOMED(b)" "OPEN(s)" "IN PROGRESS(a)" "EPIC(r)" "|"
  "REVIEW(y)" "DONE(f)" "WON'T DO(x)")))




; org-agenda
(setq org-agenda-files '("~/org/gtd.org"
                         "~/org/upcoming.org"))

(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-capture-templates
 '(("t" "Todo" entry (file+olp "~/org/inbox.org" "Tasks")
        "* TODO %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n"
        :empty-lines 1)
("n" "Note" entry (file+olp "~/org/inbox.org" "Notes")
        "* NOTE %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n"
        :empty-lines 1)
   ("m" "Memorize" entry (file+olp "~/org/inbox.org" "Facts") 
        "* %? \t :note:\n :PROPERTIES: \n :CREATED: %u \n :ANKI_NOTE_TYPE: Basic \n :END: \n** Front\n\n** Back"
        :empty-lines 1)))

(require 'ox)
(use-package org-download
  :pin melpa
)

(use-package org-brain
  :pin melpa
  :ensure t
  :config
  (setq org-brain-path "~/repos_main/orgbrain")
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
)

(use-package deft
  :pin melpa
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory org-brain-path)
  (setq deft-recursive t)
)

(use-package org-journal
  :pin melpa
  :ensure t
  :config
  (setq org-journal-dir (concat org-brain-path "/journal"))
)

(use-package ivy
  :ensure t
  :pin melpa
  :config
  (ivy-mode 1)
  (counsel-mode 1)
)

(use-package which-key
  :ensure t
  :pin melpa
  :config
  (which-key-mode)
)

(use-package flycheck
  :pin melpa
  :ensure t
  :config (global-flycheck-mode))

(use-package sdcv
  :pin melpa
  :ensure t
)

(use-package nov
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
)

(use-package atomic-chrome
  :pin melpa
  :ensure t
  :config
  (setq atomic-chrome-url-major-mode-alist
    '(("databricks" . scala-mode)
      ("ipynb" . python-mode)))
  (atomic-chrome-start-server)
)

(use-package undo-tree
  :pin melpa
  :ensure t
  :config
  (global-undo-tree-mode)
)

(load-file "~/.emacs.d/wold-theme.el")
(load-theme 'wold t)

(use-package golden-ratio
  :pin melpa
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
)

(add-to-list 'default-frame-alist
  '(font . "League Mono-12.5"))

(set-face-font 'fixed-pitch "League Mono-12.5")
(set-face-font 'variable-pitch "IBM Plex Sans-14:spacing=110")
(set-face-font 'org-column "League Mono-12.5")
(set-face-font 'mode-line "League Mono-10.5")
(set-face-font 'mode-line-inactive "League Mono-10.5")
;(set-face-font 'linum "League Mono-10.5")

  (defun set-buffer-variable-pitch ()
    (interactive)
    (variable-pitch-mode t)
    (setq line-spacing 0.45)
     (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
     (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
     (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
     (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch)
     (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    )

  (add-hook 'org-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'eww-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
  (add-hook 'Info-mode-hook 'set-buffer-variable-pitch)



(defun my/tangle-emacs ()
  "If the current file is in '~/repos_main/emacs', the code blocks are tangled"
  (when (equal buffer-file-name
               (concat (getenv "HOME") "/repos-main/emacs/newinit.org"))
    (org-babel-tangle)
    (message "%s tangled" buffer-file-name)))

(add-hook 'after-save-hook #'my/tangle-emacs)
