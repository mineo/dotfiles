;; init
;; --- Summary
;;; Code:
;;; Commentary:

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq ring-bell-function 'ignore ;; disable bell, ...
      inhibit-startup-screen t ;; ... startup screen and ...
      initial-scratch-message nil ;; the scratch message
      x-select-enable-primary t ;; and c&p to primary
      tags-revert-without-query t
      )
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
;; encoding - Set preferred encoding system as UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

;; Don't use tabs for indentation
(setq-default indent-tabs-mode nil)

;; Automatically reload buffers if the file changes
(global-auto-revert-mode 1)

(column-number-mode 1)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Don't show minor modes in the modeline, except for flycheck
(setq rm-whitelist "FlyC.*")

;; Keep point centered when scrolling
(setq scroll-preserve-screen-position t)

;; Don't use dialog boxes
(setq use-dialog-box nil)

;; I'll take a longer route for killing emacs
(global-unset-key "\C-x\C-c")

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(defconst my-packages
  '(
    use-package
))


;; https://github.com/lunaryorn/.emacs.d/blob/7acd8c6538c4f40a3ee530dfc808eaacaf167630/init.el#L166
(defun ensure-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
        (package-install package)
)))

(ensure-packages)

(recentf-mode 1)

;; https://kylewm.com/article/2014/01/30/1/emacs-toggle-light-and-dark-themes
(defcustom default-light-color-theme 'solarized-light
  "default light theme")

(defcustom default-dark-color-theme 'solarized-dark
  "default dark theme")

(defun toggle-dark-light-theme ()
  (interactive)

  (let ((is-light (find default-light-color-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (progn
      (if is-light
          (load-theme default-dark-color-theme 'no-confirm)
        (load-theme default-light-color-theme 'no-confirm))
      (set-face-underline 'show-paren-match t)
      (set-face-underline 'highlight-symbol-face t)
      ;; Set the default font
      (add-to-list 'default-frame-alist '(font . "Consolas-13"))
      ;; Set the font of the mode line. If this is done before
      ;; after-make-frame-functions, emacsclient -a '' segfaults
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (set-face-attribute 'mode-line frame :font "Consolas-12"))))))

(defun mineo-configure-fonts (frame)
    "Set up additional fonts for Emoji and other things in FRAME."
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") nil 'prepend))

(add-hook 'after-make-frame-functions #'mineo-configure-fonts)


(require 'use-package)

(use-package avy
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "a c" 'avy-goto-char
      "a w" 'avy-goto-word-or-subword-1
      "a l" 'avy-goto-line)))

(use-package avy-zap
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "a z" 'avy-zap-to-char)))

(use-package buffer-move
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "b m h" 'buf-move-left
      "b m l" 'buf-move-right
      "b m j" 'buf-move-down
      "b m k" 'buf-move-up)))

(use-package tex-site
  :ensure auctex
  :config
  (dolist (mode '(reftex-mode
                  TeX-PDF-mode
                  TeX-source-correlate-mode
                  TeX-fold-mode
                  (lambda ()
                    (setq TeX-command-default "LatexMk"))))
    (add-hook 'TeX-mode-hook mode))
  (setq TeX-parse-self t                ; parse on load
        TeX-auto-save t)                ; parse on save
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'latex-mode
      ;; preview
      "m p c" 'preview-clearout-document
      "m p d" 'preview-document
      ;; building
      "m _" 'TeX-master-file-ask
      "m c" 'TeX-command-master
      "m v" 'TeX-view
      ;; reftex
      "m =" 'reftex-toc
      "m r -" 'reftex-toc-recenter
      "m r c" 'reftex-citation
      "m r r" 'reftex-reference
      ;; insertion
      "m i e" 'LaTeX-environment
      "m i f" 'TeX-font
      "m i m" 'TeX-insert-macro
      "m i s" 'LaTeX-section
      )))

(use-package reftex
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package auctex-latexmk
  :ensure
  :config (auctex-latexmk-setup))

(use-package column-enforce-mode
  :ensure
  :config
  (add-hook 'prog-mode-hook 'column-enforce-mode))

(auto-insert-mode 1)
;; company
(use-package company
  :ensure
  :bind ("C-a" . company-complete)
  :config
  (setq company-show-numbers t)
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map "\C-a" 'company-complete))
  :init (global-company-mode 1))

(use-package company-emoji
  :ensure
  :config
  (with-eval-after-load 'company
    (company-emoji-init)))

(use-package company-quickhelp
  :ensure
  :config
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook #'company-quickhelp-mode)))

(use-package copyright
  :defer t
  :config
  (setq copyright-names-regexp (user-full-name))
  :init (add-hook 'find-file-hook #'copyright-update))

(use-package editorconfig
  :ensure)

;; eldoc
(use-package eldoc
  :ensure
  :config (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

;; electric-pair-mode
(electric-pair-mode 1)

(use-package ensime
  :ensure
  :config
  (setq ensime-auto-connect 'always
        ;; For some reason ensime doesn't find this automatically
        ensime-sbt-command "/usr/bin/sbt")
  (add-hook 'scala-mode-hook #'ensime-mode)
  (with-eval-after-load 'flycheck
    (add-hook 'ensime-mode-hook (lambda () (flycheck-mode -1))))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ensime-inspector-mode 'emacs))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'scala-mode
      "e l" 'ensime-show-all-errors-and-warnings
      "e n" 'ensime-forward-note
      "e p" 'ensime-backward-note)))

;; ;; enable evil
(use-package evil
  :commands evil-set-initial-state
  :ensure
  :config
  (evil-set-initial-state 'rst-toc-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  ;; I never use evils M-., so free the keybinding for Emacs' find-tag
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)
  (evil-mode 1))

(use-package evil-args
  :ensure
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(setq-default evil-escape-key-sequence "jk")
(use-package evil-escape
  :disabled t
  :ensure
  :config
  (evil-escape-mode 1))

(use-package evil-exchange
  :ensure
  :config
  (evil-exchange-install))

(use-package evil-leader
  :ensure
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "b b" 'bury-buffer
    "b k" 'kill-buffer
    "b K" 'kill-buffer-and-window
    "s" 'toggle-dark-light-theme
    "w 1" 'delete-other-windows
    "w s" 'evil-window-vsplit
    "w o" 'switch-window
    "w w" 'delete-window
    "w c" 'delete-frame
    "w +" 'text-scale-adjust)
  (global-evil-leader-mode 1))

(use-package evil-matchit
  :ensure
  :config (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :ensure
  :config (define-key evil-normal-state-map (kbd "C-o") 'evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :ensure
  :config
  (add-hook 'org-mode-hook 'evil-org-mode 1))

(use-package evil-surround
  :ensure
  :config (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure
  :config (global-evil-visualstar-mode 1))

(use-package expand-region
  :ensure
  :commands er/expand-region
  :init (define-key evil-normal-state-map (kbd "+") 'er/expand-region))

(use-package flatland-theme
  :ensure)

;; flycheck
(use-package flycheck
  :ensure
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (setq-default flycheck-disabled-checkers '(python-pylint python-pyflakes))
  (setq flycheck-flake8rc "setup.cfg")
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "e l" 'flycheck-list-errors
      "e L" 'helm-flycheck
      "e n" 'flycheck-next-error
      "e p" 'flycheck-previous-error
      ))
  (global-flycheck-mode ))

(use-package flycheck-pos-tip
  :ensure
  :disabled t
  :config (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; go
(use-package go-mode
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'go-mode
      "d" 'godoc-at-point)))

(use-package go-eldoc
  :ensure
  :config (go-eldoc-setup))

(use-package company-go
  :ensure
  :config (add-to-list 'company-backends 'company-go))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; haskell
(use-package haskell-mode
  :ensure
  :config
  (setq haskell-tags-on-save t
        haskell-process-type 'cabal-repl
        haskell-process-show-debug-tips nil
        haskell-auto-import-loaded-modules t
        haskell-process-suggest-remove-import-lines t
        haskell-stylish-on-save t
        haskell-indentation-show-indentations nil)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook (lambda () (column-enforce-mode -1)))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'haskell-interactive-mode 'emacs))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'haskell-mode
      "mcb" 'haskell-process-cabal-build
      "mcc" 'haskell-process-cabal
      "mai" 'haskell-add-import
      "ib" 'haskell-interactive-bring
      "ic" 'haskell-interactive-mode-clear
      "if" 'haskell-process-load-file
      "is" 'haskell-interactive-switch))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'haskell-cabal-mode
      "mcc" 'haskell-process-cabal
      "ib" 'haskell-interactive-bring
      "ic" 'haskell-interactive-mode-clear)))

(defun mineo-ignore-function-call-function (orig-fun &rest args)
  "Ignore any call to ORIG-FUN, whatever the ARGS."
  )

(use-package ghc
  :ensure
  :commands ghc-init ghc-debug
  :config
  (use-package company-ghc
    :ensure
    :config (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
  )
  (advice-add 'ghc-check-syntax :around #'mineo-ignore-function-call-function)
  :init
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package flycheck-haskell
  :ensure
  :config(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package helm
  :ensure
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; Enable fuzzy matching
  (setq helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-semantic-fuzzy-match t)
  (define-key evil-normal-state-map (kbd ";") 'helm-M-x)
  (define-key evil-visual-state-map (kbd ";") 'helm-M-x)
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "SPC" 'helm-M-x
      "b s" 'helm-buffers-list
      "h a" 'helm-apropos
      "h h" 'help
      "h f" 'helm-find-files
      "h m" 'helm-man-woman
      "h o" 'helm-occur
      "h p" 'helm-projectile
      "h r" 'helm-recentf
      "h t" 'helm-semantic-or-imenu
      ))
  ;; Let helm always open at the bottom at 40% window height
  ;; From https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  )

(use-package helm-ag
  :ensure)

(use-package helm-projectile
  :ensure
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "h s s" 'helm-swoop
      "h s m" 'helm-multi-swoop)))

(use-package highlight-symbol
  :ensure
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.4))

(use-package ignoramus
  :ensure
  :config (ignoramus-setup))

(use-package hl-line
  :ensure
  :config (global-hl-line-mode 1))

(use-package jabber
  :ensure
  :config
  (setq jabber-message-alert-same-buffer nil
        ;; DBus doesn't work for some reason
        jabber-libnotify-method 'shell
        ;; One auto-away change after 30 minutes is enough
        jabber-autoaway-timeout 30
        jabber-autoaway-xa-timeout 0
        ;; No avatars, please
        jabber-roster-line-format " %c %-25n\n   %u %-8s  %S")
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "j c" 'jabber-connect-all
      "j d" 'jabber-disconnect
      "j r" 'jabber-switch-to-roster-buffer))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'jabber-roster-mode 'emacs)
    (evil-set-initial-state 'jabber-chat-mode 'insert))

  (add-hook 'jabber-alert-message-hooks 'jabber-message-libnotify)
  ;; I don't need notifications about status changes in the echo area
  (remove-hook 'jabber-alert-presence-hooks 'jabber-presence-echo)

  ;; These faces use a huge height by default
  (set-face-attribute 'jabber-title-large nil :height 1.2)
  (set-face-attribute 'jabber-title-medium nil :height 1.1))

(defconst mineo-private-dir (locate-user-emacs-file "private"))

(use-package mineo-jabber
  :load-path mineo-private-dir
  :config
  (mineo-setup-jabber-accounts))

;; line numbers
(use-package linum
  :ensure
  :disabled t
  :config
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package linum-relative
  :ensure
  :disabled t)

(use-package magit
  :ensure
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-to-list 'evil-buffer-regexps '("\*magit.*\*" . 'emacs))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "v" 'magit-status)))

(use-package markdown-mode
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'markdown-mode
      "m v" 'markdown-preview
      "m -" 'markdown-promote
      "m +" 'markdown-demote
      "m e" 'markdown-export
      "m i l" 'markdown-insert-link
      "m o" 'markdown-follow-thing-at-point
      "m n" 'outline-next-heading
      "m p" 'outline-previous-heading)))

(use-package midnight
  :config (setq midnight-mode t))

(use-package paredit
  :ensure
  :config (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package paradox
  :ensure
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'paradox-menu-mode 'emacs))
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

;; This is necessary for jabber because it makes jabber.el retrieve
;; the passwords from pass
(use-package auth-password-store
  :ensure
  :config
  (auth-pass-enable))

(use-package pip-requirements
  :ensure)

(use-package projectile
  :ensure
  :config
  (dolist (dir '(".tox" ".cabal-sandbox" "dist" "build" ".eggs" "docs/build" ".ensime_cache"))
    (add-to-list 'projectile-globally-ignored-directories dir)
    )
  (add-to-list 'projectile-globally-ignored-modes "tags-table-mode")
  (def-projectile-commander-method ?j "Find tag." (call-interactively 'helm-etags-select))
  (def-projectile-commander-method ?P "Test the project." (call-interactively 'projectile-test-project))
  (def-projectile-commander-method ?p "Switch the project." (call-interactively 'helm-projectile-switch-project))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "p" 'projectile-commander
      ))
  (projectile-global-mode 1)
  )

(use-package projectile-addons
  :load-path "lisp/"
  :config
  ;; setting projectile-enable-idle-timer outside of customize doesn't
  ;; initialize the timer, so initialize it manually
  (mineo-initialize-projectile-idle-timer)
  ;; Set the working directory of python processes to projectiles project root
  (add-hook 'inferior-python-mode-hook 'mineo-python-shell-cd-project-root))

(use-package python
  :ensure
  :config
  (add-hook 'python-mode-hook 'outline-minor-mode)
  (setq python-shell-interpreter "ipython2"
        python-shell-interpreter-args "-i"))

(use-package anaconda-mode
  :ensure
  :config (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :ensure
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package virtualenvwrapper
  :ensure
  :config
  (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'python-mode
      "v" 'venv-workon
      "d" 'anaconda-mode-view-doc
      ))
  (evil-set-initial-state 'anaconda-nav-mode 'emacs))

;; org-mode
(use-package org
  :ensure
  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "o a" 'org-agenda
      "o c" 'org-capture
      "o b" 'org-iswitchb
      "o l" 'org-store-link))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key-for-mode 'org-mode
      "m a" 'org-agenda
      "m A" 'org-archive-subtree
      "m c" 'org-capture
      "m j" 'org-goto
      "m r" 'org-refile
      "m s" 'org-schedule))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (setq org-directory "~/.org"
        org-default-notes-file (concat org-directory "/notes.org")
        ;; Keep track of state changes of tasks and show them in the
        ;; agenda by default
        org-log-done 'time
        org-agenda-start-with-log-mode t
        ;; Open my notes file instead of the scratch buffer in new
        ;; emacs instances
        initial-buffer-choice org-default-notes-file
        org-ellipsis " […]"
        org-agenda-files '("~/.org")
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-capture-templates
        '(("t" "Todo" entry (file+headline nil "Tasks")
           "* TODO %?\n  %i")
          ("l" "Todo with link" entry (file+headline nil "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("b" "Buch" entry (file+headline nil "unsortiert")
           "*** %?")
          ( "v" "Video" entry (file+headline nil "Video")
            "** %?")
          ("m" "Musik" item (file+headline nil "Musik")
           "    - [ ] %?")))
  ;; Use mimeopen or evince for PDF files
  (setq org-file-apps (cl-remove "\\.pdf\\'" org-file-apps :test 'equal :key 'car))
  (if (executable-find "mimeopen")
      (add-to-list 'org-file-apps '("pdf" . "mimeopen %s"))
      (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(use-package org-bullets
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; rst
(setq rst-pdf-program "mimeopen")

(use-package solarized
  :ensure solarized-theme
  :config
  (setq solarized-use-less-bold t
        solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        x-underline-at-descent-line t)

  ;; load theme
  (toggle-dark-light-theme))

(use-package smart-mode-line
  :ensure
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)
  (setq sml/projectile-replacement-format "p:%s/")
  )

(use-package switch-window
  :ensure)

(use-package volatile-highlights
  :ensure
  :config (volatile-highlights-mode 1))

(use-package which-key
  :ensure
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom)
  (which-key-add-key-based-replacements "SPC a" "avy"
                "SPC b" "buffer-move"
                "SPC e" "flycheck"
                "SPC h" "helm"
                "SPC h s" "helm-swoop"
                "SPC j" "jabber"
                "SPC m" "major mode"
                "SPC o" "org"
                "SPC p" "projectile"
                "SPC v" "magit"
                "SPC w" "window"))

(use-package whitespace
  :ensure
  :config
  (setq whitespace-style '(face
                       indentation
                       line
                       trailing
                       space-after-tab
                       space-before-tab)))

(use-package whitespace-cleanup-mode
  :ensure
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (dolist (mode (list #'whitespace-mode #'whitespace-cleanup-mode))
       (add-hook hook mode))))

(use-package yasnippet
  :ensure
  :config (yas-global-mode 1))

(use-package yatemplate
  :ensure
  :config (yatemplate-fill-alist))

(provide 'init)
;;; init.el ends here
