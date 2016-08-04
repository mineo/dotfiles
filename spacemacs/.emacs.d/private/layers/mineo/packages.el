;;; packages.el --- mineo Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014, 2016 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq mineo-packages
    '(
      auctex
      auth-password-store
      beacon
      (bibtex :location built-in)
      cask-mode
      column-enforce-mode
      company
      company-emoji
      copyright
      editorconfig
      flycheck-cask
      flycheck-package
      highlight-symbol
      magit
      (midnight :location built-in)
      neotree
      projectile
      (projectile-addons :location local)
      org
      virtualenvwrapper
      whitespace
      whitespace-cleanup-mode
      writeroom-mode
      yatemplate))

;; List of packages to exclude.
(setq mineo-excluded-packages '())

(defun mineo/post-init-auctex ()
  "Post-initialize auctex."
  ;; For zc etc.
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  ;; Enable fold mode
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  ;; Fold automatically
  (add-hook 'LaTeX-mode-hook 'TeX-fold-buffer t)
  ;; Automatically insert braces for super and subscripts
  (setq TeX-electric-sub-and-superscript t)
  )

(defun mineo/init-beacon ()
  "Initialize beacon."
  (use-package beacon
    :config
    (setq beacon-dont-blink-commands
          (delete 'recenter-top-bottom beacon-dont-blink-commands))
    (beacon-mode 1)))

(defun mineo/init-bibtex ()
  "Initialize bibtex"
  (use-package bibtex
    :config
    (add-hook 'bibtex-mode-hook 'goto-address-mode)))

(defun mineo/init-auth-password-store ()
  "Initialize auth-password-store."
  (use-package auth-password-store
    :config
    (auth-pass-enable)))

(defun mineo/init-cask-mode ()
  "Initialize cask-mode."
  (use-package cask-mode))

(defun mineo/init-column-enforce-mode ()
  "Initialize enforce-column-mode."
  (use-package column-enforce-mode
    :config
    (add-hook 'prog-mode-hook 'column-enforce-mode)))

(defun mineo/post-init-company ()
  "Post-initialize company."
  (use-package company
    :config
    (setq company-show-numbers t
          company-tooltip-align-annotations t)))

(defun mineo/post-init-company-emoji ()
  "Post-initialize company-emoji."
  (setq company-emoji-insert-unicode t))

(defun mineo/init-copyright ()
  "Initialize copyright."
  (use-package copyright
    :config
    (add-hook 'find-file-hook #'copyright-update)
    (setq copyright-names-regexp (user-full-name))))

(defun mineo/init-editorconfig ()
  "Initialize editorconfig."
  (use-package editorconfig
    :config
    (editorconfig-mode 1)))

(defun mineo/init-flycheck-cask ()
  "Initialize flycheck-cask."
  (use-package flycheck-cask
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(defun mineo/init-flycheck-package ()
  "Initialize flycheck-package."
  (use-package flycheck-package
    :config
    (flycheck-package-setup)))

(defun mineo/init-highlight-symbol ()
  "Initialize highlight-symbol."
  (use-package highlight-symbol
    :config
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (setq highlight-symbol-on-navigation-p t
          highlight-symbol-idle-delay 0.4)))

(defun mineo/init-midnight ()
  "Initialize midnight."
  (use-package midnight))

(defun mineo/init-projectile-addons ()
  "Initialize projectile-addons."
  (use-package projectile-addons
    :config
    (add-hook 'inferior-python-mode-hook 'mineo-python-shell-cd-project-root)))

(defun mineo/init-virtualenvwrapper ()
  "Initialize virtualenvwrapper."
  (use-package virtualenvwrapper
    :config
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "v" 'venv-workon
      "V" 'venv-deactivate)
    (spaceline-define-segment virtualenvwrapper-active-env
      "Shows the currently activated virtual environment."
      (format "venv:%s" venv-current-name)
      :when (equal major-mode 'python-mode))))

(defun mineo/init-whitespace-cleanup-mode ()
  "Initialize whitespace-cleanup-mode."
  (use-package whitespace-cleanup-mode
    :config
    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
      (dolist (mode (list #'whitespace-mode #'whitespace-cleanup-mode))
        (add-hook hook mode)))))

(defun mineo/init-writeroom-mode ()
  "Initialize writeroom-mode."
  (use-package writeroom-mode
    :config
    (setq writeroom-fullscreen-effect 'maximized)))

(defun mineo/init-yatemplate ()
  "Initialize yatemplate."
    (use-package yatemplate
      :config
      (setq yatemplate-dir (locate-user-emacs-file "private/templates"))
      (yatemplate-fill-alist)
      (auto-insert-mode 1)))

(defun mineo/post-init-magit ()
  "Post-initialize magit."
  (use-package magit
    :config
    (add-to-list 'evil-buffer-regexps
                 '("COMMIT_EDITMSG" . insert))))

(defun mineo/post-init-neotree ()
  "Initialize neotree."
  (use-package neotree
    :config
    (setq neo-theme 'ascii)))

(defun mineo/post-init-org ()
  "Post-initialize org."
  (setq mineo-org-babel-languages '(C python sh shell))
  (with-eval-after-load 'org
    (dolist (language mineo-org-babel-languages)
      (cl-pushnew `(,language . t) org-babel-load-languages)))

  (setq org-directory "~/.org"
        org-default-notes-file (concat org-directory "/notes.org")
        ;; Keep track of state changes of tasks and show them in the
        ;; agenda by default
        org-log-done 'time
        org-agenda-start-with-log-mode t
        ;; Open my notes file instead of the scratch buffer in new
        ;; emacs instances
        initial-buffer-choice org-default-notes-file
        ;; Other stuff
        org-hide-emphasis-markers t
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
           "- [ ] %?"))
        ;; Sort these a bit, the defaults are not very good with the Consolas
        ;; font
        org-bullets-bullet-list '("✿" "✸" "◉" "○")
        org-clock-idle-time 5)
  (add-hook 'org-mode-hook 'spacemacs-fixes//org-babel-do-load-languages))

(defun mineo/post-init-projectile ()
  "Post-initialize projectile."
  (use-package projectile
    :config
    (dolist (dir '(".tox" ".cabal-sandbox" "dist" "build" ".eggs" "htmlcov" "docs/build" ".ensime_cache"))
      (add-to-list 'projectile-globally-ignored-directories dir)
      )
    (add-to-list 'projectile-globally-ignored-modes "tags-table-mode")))

(defun mineo/post-init-whitespace ()
  "Post-initialize whitespace."
  (use-package whitespace
    :config
    (setq spacemacs-show-trailing-whitespace nil)
    (setq whitespace-style '(face
                             tabs
                             trailing
                             space-before-tab
                             newline
                             empty
                             space-after-tab
                             tab-mark
                             newline-mark))
    (setq whitespace-display-mappings
          '((newline-mark ?\n [?< ?\n])
            (tab-mark     ?\t [?\u00BB ?\t] [?\\ ?\t])))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
