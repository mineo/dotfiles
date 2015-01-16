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
             '(font . "Consolas-14"))
(setq-default line-spacing 2)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(defconst my-packages
  '(
    use-package
    ;; color themes
    solarized-theme
    base16-theme
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
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; load theme
(load-theme 'solarized-light 'no-confirm)

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
    ;; (load-theme (if is-light default-dark-color-theme default-light-color-theme))
    (if is-light
        (progn
          (load-theme default-dark-color-theme 'no-confirm)
          (set-face-background 'shm-current-face "#424242")
          )
      (progn
        (load-theme default-light-color-theme 'no-confirm)
        (set-face-background 'shm-current-face "#eed481")
        (set-face-background 'shm-quarantine-face "lemonchiffon")
        )
      )
    )
  )

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'use-package)

(use-package ag
  :ensure
  :config
  (use-package wgrep-ag
    :ensure
    :init
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    (add-hook 'ag-mode-hook 'evil-emacs-state))
  )

(use-package tex-site
  :ensure auctex
  :config
  (use-package auctex-latexmk
    :ensure
    :config (auctex-latexmk-setup)
  )
  (dolist (mode '(reftex-mode
                  TeX-PDF-mode
                  TeX-fold-mode))
          (add-hook 'TeX-mode-hook mode))
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t) ; parse on save
)

;; company
(use-package company
  :ensure
  :bind ("C-a" . company-complete)
  :config
  (setq company-show-numbers t)
  :diminish company-mode
  :init (global-company-mode)
)

(use-package copyright
  :defer t
  :init (add-hook 'find-file-hook #'copyright-update))

;; eldoc
(use-package eldoc
  :ensure
  :diminish eldoc-mode
  :config (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

;; electric-pair-mode
(electric-pair-mode)

;; ;; enable evil
(use-package evil
  :commands evil-set-initial-state
  :ensure
  :diminish undo-tree-mode
  :config
  (evil-set-initial-state 'man-mode 'emacs)
  (use-package ace-jump-mode
    :ensure)
  (use-package evil-leader
    :ensure
    :config
      (evil-leader/set-leader "SPC")
      (evil-leader/set-key "SPC c" 'ace-jump-char-mode
                           "SPC w" 'ace-jump-word-mode
                           "a" 'projectile-ack
                           "b" 'ido-switch-buffer
                           "e" 'flycheck-list-errors
                           "k" 'kill-buffer
                           "l e" 'flycheck-list-errors
                           "m" 'magit-status
                           "o" 'ido-find-file
                           "p" 'projectile-commander
                           "r" 'recentf-ido-find-file
                           "q" 'save-buffers-kill-emacs
                           "s" 'toggle-dark-light-theme
                           "t" 'imenu-anywhere
                           "w" 'evil-window-vsplit)
     (global-evil-leader-mode)
  )
  (use-package evil-nerd-commenter
    :ensure
    :config (define-key evil-normal-state-map (kbd "C-o") 'evilnc-comment-or-uncomment-lines)
  )
  (use-package evil-surround
    :ensure
    :config (global-evil-surround-mode)
  )
  (use-package expand-region
    :ensure
    :commands er/expand-region
    :init (define-key evil-normal-state-map (kbd "+") 'er/expand-region))
  (define-key evil-normal-state-map (kbd ";") 'smex)
  (define-key evil-visual-state-map (kbd ";") 'smex)
  (evil-mode)
)

;; fci
(use-package fill-column-indicator
  :ensure
  :diminish auto-fill-function
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

;; flycheck
(use-package flycheck
  :ensure
  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (setq-default flycheck-disabled-checkers '(python-pylint python-pyflakes))
  (global-flycheck-mode)
  (use-package flycheck-pos-tip
    :ensure
    :config (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  )
)

;; go
(use-package go-mode
  :ensure
  :config
  (use-package go-eldoc
    :ensure
    :config (go-eldoc-setup)
  )
  (use-package company-go
    :ensure
    :config (add-to-list 'company-backends 'company-go)
  )
  (evil-leader/set-key-for-mode 'go-mode
    "d" 'godoc-at-point
  )
)

;; guide-key
(use-package guide-key
  :ensure
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence '("C-c"
                                       "C-x"
                                       )
  )
  (setq guide-key/recursive-key-sequence-flag t)
  ;; (diminish 'guide-key-mode)
  (guide-key-mode)
)

;; haskell
(use-package haskell-mode
  :ensure
  :config
  (use-package ghc
    :ensure
    :commands ghc-init ghc-debug
    :config
    (use-package company-ghc
      :ensure
      :config (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
    )
    :init
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  )

  (use-package flycheck-haskell
    :ensure
    :config(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  )

  ;; structured-haskell-mode
  (use-package shm
    :ensure
    :config
    (set-face-background 'shm-current-face "#585b6e")
    (set-face-background 'shm-quarantine-face "#171717")
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  )

  (setq haskell-tags-on-save t
        haskell-process-type 'cabal-repl
        haskell-process-show-debug-tips nil
        haskell-auto-import-loaded-modules t
        haskell-process-suggest-remove-import-lines t)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-leader/set-key-for-mode 'haskell-mode
    "cb" 'haskell-process-cabal-build
    "cc" 'haskell-process-cabal
    "cs" 'haskell-interactive-switch
  )
)


;; ido
(use-package ido
  :ensure
  :config
  (setq ido-everywhere t)
  (use-package ido-vertical-mode
    :ensure
    :config (ido-vertical-mode)
  )
  (use-package ido-ubiquitous
    :ensure
    :config (ido-ubiquitous-mode)
  )
  (use-package flx-ido
    :ensure
    :config
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)
    (flx-ido-mode))
  (ido-mode)
)

(use-package highlight-symbol
  :ensure
  :diminish highlight-symbol-mode
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  (setq highlight-symbol-on-navigation-p t
        highlight-symbol-idle-delay 0.4
  )
)

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
                filename-and-process)))
  )

(use-package ignoramus
  :ensure
  :config (ignoramus-setup))

(use-package imenu-anywhere
  :ensure
  :commands imenu-anywhere ido-imenu-anywhere
)

(use-package hl-line
  :ensure
  :config (global-hl-line-mode))

;; line numbers
(use-package linum
  :ensure
  :config
  (use-package linum-relative
    :ensure
  )
  (global-linum-mode)
)

(use-package magit
  :ensure
  :diminish magit-auto-revert-mode
  :config (evil-set-initial-state 'magit-mode 'emacs)
)

(use-package midnight
  :config (setq midnight-mode t))

(use-package paradox
  :ensure
  :config
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (setq paradox-github-token t)
)

(use-package pip-requirements
  :ensure
)

(use-package projectile
  :ensure
  :diminish projectile-mode
  :config
  (dolist (dir '(".tox" ".cabal-sandbox" "dist" "build" ".eggs"))
    (add-to-list 'projectile-globally-ignored-directories dir)
  )
  (use-package perspective
    :ensure
    :config
    (persp-mode)
    (use-package persp-projectile
      :ensure
    )
  )
  (setq projectile-enable-idle-timer t)
  (projectile-global-mode)
)

(use-package python
  :ensure
  :config
  (use-package anaconda-mode
    :ensure
    :diminish anaconda-mode
    :config (add-hook 'python-mode-hook 'anaconda-mode)
  )
  (use-package company-anaconda
    :ensure
    :config (add-to-list 'company-backends 'company-anaconda)
  )
  (use-package virtualenvwrapper
    :ensure
    :config
    (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
    (evil-leader/set-key-for-mode 'python-mode
      "v" 'venv-workon
      "d" 'anaconda-mode-view-doc
      )
    (evil-set-initial-state 'anaconda-nav-mode 'emacs)
  )
  (use-package flycheck-pyflakes
    :ensure
    :config (setq-default flycheck-python-flake8-executable "/usr/bin/flake8-python2")
    )
  (add-hook 'python-mode-hook 'outline-minor-mode)
)

;; prog-mode stuff
(add-hook 'prog-mode-hook 'which-function-mode)

;; org-mode
(use-package org
  :ensure
  :bind (
    ("C-c l" . org-store-link)
    ("C-c c" . org-capture)
    ("C-c a" . org-agenda)
    ("C-c b" . org-iswitchb)
  )
)


(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)

(use-package smart-mode-line
  :ensure
  :diminish
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful )
  )


;; smex
(use-package smex
  :ensure
  :commands smex
  :bind(
    ("M-x" . smex)
    ("M-X" . smex-major-mode-commands)
    ;; This is your old M-x.
    ("C-c C-c M-x" . execute-extended-command)
  )
  :config (smex-initialize)
)

(use-package switch-window
  :ensure
  :bind ("C-x o" . switch-window))

(use-package volatile-highlights
  :ensure
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode)
)

(use-package whitespace
  :diminish whitespace-mode
  :ensure
  :config
  (setq whitespace-style '(face
                       indentation
                       line
                       trailing
                       space-after-tab
                       space-before-tab
                       ))
  (use-package whitespace-cleanup-mode
    :diminish whitespace-cleanup-mode
    :ensure
    :config
    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
      (dolist (mode (list #'whitespace-mode #'whitespace-cleanup-mode))
         (add-hook hook mode)))
  )
)

(use-package yasnippet
  :ensure
  :diminish yas-minor-mode
  :config (yas-global-mode)
)
(provide 'init)
;;; init.el ends here
