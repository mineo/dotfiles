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

(column-number-mode)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(add-to-list 'default-frame-alist
             '(font . "Consolas-13"))
(set-face-attribute 'mode-line nil :font "Consolas-12")

(setq-default line-spacing 2)

;; Don't show minor modes in the modeline
(setq rm-blacklist ".*")

;; Keep point centered when scrolling
(setq scroll-preserve-screen-position t)

;; Don't use dialog boxes
(setq use-dialog-box nil)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(defconst my-packages
  '(
    use-package
    ;; color themes
    solarized-theme
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

(recentf-mode)

;; https://kylewm.com/article/2014/01/30/1/emacs-toggle-light-and-dark-themes
(defcustom default-light-color-theme 'solarized-light
  "default light theme")

(defcustom default-dark-color-theme 'flatland
  "default dark theme")

(defun toggle-dark-light-theme ()
  (interactive)

  (let ((is-light (find default-light-color-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (progn
      (if is-light
          (progn
            (load-theme default-dark-color-theme 'no-confirm)
            (set-face-foreground 'ace-jump-face-foreground "#fa9a4b")
            (set-face-background 'ace-jump-face-foreground "#000000")
            (fci-mode)
            (fci-mode))
        (progn
          (load-theme default-light-color-theme 'no-confirm)
          (fci-mode)
          (fci-mode)))
      (set-face-background 'show-paren-match "#00fa9a"))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'use-package)

(use-package yatemplate
  :ensure t
  :config
  (yatemplate-fill-alist))

(use-package ace-jump-mode
  :ensure
  :config
  (setq ace-jump-mode-move-keys
        (nconc (loop for i from ?a to ?z collect i)
               (loop for i from ?0 to ?9 collect i)))
  (setq ace-jump-word-mode-use-query-char nil)
  (evil-leader/set-key
    "a c" 'ace-jump-char-mode
    "a w" 'ace-jump-word-mode))

(use-package ace-jump-zap
  :ensure
  :config
  (evil-leader/set-key
    "a z" 'ace-jump-zap-to-char
    ))

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
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t) ; parse on save
)

(use-package auctex-latexmk
  :ensure
  :config (auctex-latexmk-setup))

(auto-insert-mode)
;; company
(use-package company
  :ensure
  :bind ("C-a" . company-complete)
  :config
  (setq company-show-numbers t)
  :init (global-company-mode))

(use-package company-quickhelp
  :ensure
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode))

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
(electric-pair-mode)

;; ;; enable evil
(use-package evil
  :commands evil-set-initial-state
  :ensure
  :config
  (evil-set-initial-state 'rst-toc-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-mode))

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

(use-package evil-exchange
  :ensure
  :config
  (evil-exchange-install))

(use-package evil-leader
  :ensure
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "k" 'kill-buffer
    "s" 'toggle-dark-light-theme
    "w s" 'evil-window-vsplit
    "w o" 'switch-window
    "w w" 'delete-window
    "w c" 'delete-frame
    "w +" 'text-scale-adjust)
  (global-evil-leader-mode))

(use-package evil-nerd-commenter
  :ensure
  :config (define-key evil-normal-state-map (kbd "C-o") 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :ensure
  :config (global-evil-surround-mode))

(use-package evil-visualstar
  :ensure
  :config (global-evil-visualstar-mode))

(use-package expand-region
  :ensure
  :commands er/expand-region
  :init (define-key evil-normal-state-map (kbd "+") 'er/expand-region))

;; fci
(use-package fill-column-indicator
  :ensure
  :config
  (setq-default fill-column 80)
  (defvar-local company-fci-mode-on-p nil)

  ;; https://github.com/company-mode/company-mode/issues/180
  ;; deactivate fci-mode around companys completion popup
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (progn
      (add-hook hook 'fci-mode)
      (add-hook hook 'auto-fill-mode)
      )))

(use-package flatland-theme
  :ensure)

;; flycheck
(use-package flycheck
  :ensure
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (setq-default flycheck-disabled-checkers '(python-pylint python-pyflakes))
  (setq flycheck-flake8rc "setup.cfg")
  (evil-leader/set-key
    "e l" 'flycheck-list-errors
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error
    )
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :ensure
  :config (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; go
(use-package go-mode
  :ensure
  :config
  (evil-leader/set-key-for-mode 'go-mode
    "d" 'godoc-at-point))

(use-package go-eldoc
  :ensure
  :config (go-eldoc-setup))

(use-package company-go
  :ensure
  :config (add-to-list 'company-backends 'company-go))

;; guide-key
(use-package guide-key
  :ensure
  :config
  (setq guide-key/guide-key-sequence '("C-c"
                                       "C-x"
                                       "SPC"
                                       )
  )
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode))

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
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-leader/set-key-for-mode 'haskell-mode
    "mcb" 'haskell-process-cabal-build
    "mcc" 'haskell-process-cabal
    "mai" 'haskell-add-import
    "ib" 'haskell-interactive-bring
    "ic" 'haskell-interactive-mode-clear
    "if" 'haskell-process-load-file
    "is" 'haskell-interactive-switch)
  (evil-leader/set-key-for-mode 'haskell-cabal-mode
    "mcc" 'haskell-process-cabal
    "ib" 'haskell-interactive-bring
    "ic" 'haskell-interactive-mode-clear))

(use-package ghc
  :ensure
  :commands ghc-init ghc-debug
  :config
  (use-package company-ghc
    :ensure
    :config (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
  )
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
        helm-semantic-fuzzy-match t
        helm-split-window-in-side-p t)
  (define-key evil-normal-state-map (kbd ";") 'helm-M-x)
  (define-key evil-visual-state-map (kbd ";") 'helm-M-x)
  (evil-leader/set-key
    "SPC" 'helm-M-x
    "b" 'helm-buffers-list
    "h a" 'helm-apropos
    "h h" 'help
    "h f" 'helm-find-files
    "h m" 'helm-man-woman
    "h o" 'helm-occur
    "h p" 'helm-projectile
    "h r" 'helm-recentf
    "h t" 'helm-semantic-or-imenu
    ))

(use-package helm-ag
  :ensure)

(use-package helm-projectile
  :ensure
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure
  :config
  (evil-leader/set-key
    "h s s" 'helm-swoop
    "h s m" 'helm-multi-swoop))

(use-package highlight-symbol
  :ensure
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.4))

(use-package ibuffer-vc
  :ensure
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

(use-package ignoramus
  :ensure
  :config (ignoramus-setup))

(use-package hl-line
  :ensure
  :config (global-hl-line-mode))

;; line numbers
(use-package linum
  :ensure
  :config
  (add-hook 'prog-mode-hook 'linum-mode))

(use-package linum-relative
  :ensure)

(use-package magit
  :ensure
  :config
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-leader/set-key
    "v" 'magit-status))

(use-package markdown-mode
  :ensure
  :config
  (evil-leader/set-key-for-mode 'markdown-mode
    "m v" 'markdown-preview
    "m -" 'markdown-promote
    "m +" 'markdown-demote
    "m e" 'markdown-export
    "m i l" 'markdown-insert-link
    "m o" 'markdown-follow-thing-at-point
    "m n" 'outline-next-heading
    "m p" 'outline-previous-heading))

(use-package midnight
  :config (setq midnight-mode t))

(use-package paredit
  :ensure
  :config (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package paradox
  :ensure
  :config
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (setq paradox-github-token t))

(use-package pip-requirements
  :ensure)

(use-package projectile
  :ensure
  :config
  (dolist (dir '(".tox" ".cabal-sandbox" "dist" "build" ".eggs" "docs/build"))
    (add-to-list 'projectile-globally-ignored-directories dir)
    )
  (add-to-list 'projectile-globally-ignored-modes "tags-table-mode")
  (def-projectile-commander-method ?j "Find tag." (call-interactively 'helm-etags-select))
  (def-projectile-commander-method ?P "Test the project." (call-interactively 'projectile-test-project))
  (def-projectile-commander-method ?p "Switch the project." (call-interactively 'helm-projectile-switch-project))
  (evil-leader/set-key
    "p" 'projectile-commander
    )
  (projectile-global-mode)
  )

(use-package projectile-addons
  :load-path "lisp/"
  :config
  ;; set projectiles idle timer hook to a function that only regenerates tags if
  ;; the project root is not my home directory
  (setq projectile-idle-timer-hook 'mineo-projectile-regenerate-tags)
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
  (evil-leader/set-key-for-mode 'python-mode
    "v" 'venv-workon
    "d" 'anaconda-mode-view-doc
    )
  (evil-set-initial-state 'anaconda-nav-mode 'emacs))

;; org-mode
(use-package org
  :ensure
  :bind (
    ("C-c l" . org-store-link)
    ("C-c c" . org-capture)
    ("C-c a" . org-agenda)
    ("C-c b" . org-iswitchb))
  :config
  (setq org-directory "~/.org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files '("~/.org")))


(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; rst
(setq rst-pdf-program "mimeopen")

(use-package smart-mode-line
  :ensure
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)
  (setq sml/projectile-replacement-format "p:%s/")
  )

(use-package switch-window
  :ensure
  :bind ("C-x o" . switch-window))

(use-package volatile-highlights
  :ensure
  :config (volatile-highlights-mode))

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
  :config (yas-global-mode))

(use-package yatemplate
  :ensure
  :config (yatemplate-fill-alist))

;; load theme
(load-theme 'solarized-light 'no-confirm)

(provide 'init)
;;; init.el ends here
