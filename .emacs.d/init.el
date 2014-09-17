;; init
;; --- Summary
;;; Code:
;;; Commentary:

;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Disabled bell, startup screen, scratch message
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-scratch-message nil)
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

;; Don't use tabs for indentation
(setq indent-tabs-mode nil)

;; Automatically reload buffers if the file changes
(global-auto-revert-mode 1)

(column-number-mode)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(set-frame-font "Consolas-13")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages)))

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
    (load-theme (if is-light default-dark-color-theme default-light-color-theme))))

(require 'use-package)

;; company
(use-package company
  :ensure
  :bind ("C-a" . company-complete)
  :config
  (setq company-show-numbers t)
  :diminish "©"
  :init (global-company-mode)
)

;; eldoc
(use-package eldoc
  :ensure
  :diminish "e"
  :init (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

;; electric-pair-mode
(use-package electric-pair-mode
  :init (electric-pair-mode))

;; ;; enable evil
(use-package evil
  :commands evil-set-initial-state
  :ensure
  :config
  (use-package ack-and-a-half
    :ensure)
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
                           "f" 'projectile-find-file
                           "k" 'kill-buffer
                           "m" 'magit-status
                           "o" 'ido-find-file
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
  :init (evil-mode)
)

;; fci
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(use-package fill-column-indicator
  :ensure
  :config (setq-default fill-column 80)
  :init
  (progn
    (dolist (hook '(prog-mode-hook text-mode-hook))
      (add-hook hook 'fci-mode))))

;; flycheck
(use-package flycheck
  :ensure
  :config (setq-default flycheck-disabled-checkers '(python-pylint python-pyflakes))
  :init
  (progn
    (global-flycheck-mode)
    (use-package flycheck-pos-tip
      :ensure
      :config (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
    )
  )
)

;; go
(use-package go-mode
  :ensure
  :init
  (use-package go-eldoc
    :ensure
    :config (go-eldoc-setup)
  )
  (use-package company-go
    :ensure
    :config (add-to-list 'company-backends 'company-go)
  )
)

;; guide-key
(use-package guide-key
  :ensure
  :diminish
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-c"
					 "C-x 4" ; window commands
					 "C-x 5" ; frame commands
					 )
    )
    (setq guide-key/recursive-key-sequence-flag t)
  )
  :init (guide-key-mode)
)

;; haskell
(use-package haskell-mode
  :ensure
  :config
  (progn

    (use-package ghc
      :ensure
      :commands ghc-init ghc-debug
      :init
      (progn
	(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
	(use-package company-ghc
	  :ensure
	  :config (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
	  )
      )
    )

    (use-package flycheck-haskell
      :ensure
      :config(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
    )

    ;; structured-haskell-mode
    (use-package shm
      :ensure
      :config
      (progn
	(set-face-background 'shm-current-face "#eee8d5")
	(set-face-background 'shm-quarantine-face "lemonchiffon")
      )
      :init
	(add-hook 'haskell-mode-hook 'structured-haskell-mode)
    )

    (evil-set-initial-state 'haskell-interactive-mode 'emacs)
))


;; ido
(use-package ido
  :ensure
  :config
  (setq ido-everywhere t)
  (use-package ido-vertical-mode
    :ensure
    :init (ido-vertical-mode)
  )
  (use-package ido-ubiquitous
    :ensure
    :init (ido-ubiquitous-mode)
  )
  (use-package flx-ido
    :ensure
    :config
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)
    :init (flx-ido-mode 1))
  :init (ido-mode)
)

(use-package ignoramus
  :ensure
  :init (ignoramus-setup))

(use-package imenu-anywhere
  :ensure
  :commands imenu-anywhere ido-imenu-anywhere
)

(use-package hl-line
  :ensure
  :init (global-hl-line-mode))

;; line numbers
(use-package linum
  :ensure
  :config
  (use-package linum-relative
    :ensure
  )
  :init
  (global-linum-mode)
)

(use-package magit
  :ensure
  :config (evil-set-initial-state 'magit-mode 'emacs)
)
;; projectile
(use-package projectile
  :ensure
  :commands projectile-find-file
  :config (setq projectile-globally-ignored-directories (quote (".tox" ".cabal-sandbox" "dist" "build")))
  :init (projectile-global-mode)
)

(use-package python
  :ensure
  :config
  (add-hook 'python-mode-hook 'electric-indent-mode)
  (use-package anaconda-mode
    :ensure
    :config (add-hook 'python-mode-hook 'anaconda-mode)
  )
  (use-package company-anaconda
    :ensure
    :config (add-to-list 'company-backends 'company-anaconda)
  )
  (use-package virtualenvwrapper
    :ensure
    :config
    (setq venv-location "~/dev/virtualenvs")
    (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
    (evil-leader/set-key-for-mode 'python-mode
      "v" 'venv-workon
      "d" 'anaconda-mode-view-doc)
  )
  (use-package flycheck-pyflakes
    :ensure
    :config (setq-default flycheck-python-flake8-executable "/usr/bin/flake8-python2")
  )
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


(use-package whitespace
  :diminish
  :ensure
  :config
  (progn
    (setq whitespace-style '(face
                         indentation
                         line
                         trailing
                         space-after-tab
                         space-before-tab
                         ))
    (use-package whitespace-cleanup-mode
      :diminish
      :ensure
      :init
      (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
        (dolist (mode (list #'whitespace-mode #'whitespace-cleanup-mode))
           (add-hook hook mode)))
    )
  )
)

(use-package yasnippet
  :ensure
  ;; :init (yas-global-mode)
)
(provide 'init)
;;; init.el ends here