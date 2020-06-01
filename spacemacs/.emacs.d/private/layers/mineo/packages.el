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
      (auth-source-pass :location built-in)
      beacon
      (calc :location built-in)
      cask-mode
      compact-docstrings
      company
      company-emoji
      copyright
      flycheck
      flycheck-cask
      git-commit
      github-clone
      go-mode
      highlight-symbol
      magit
      (midnight :location built-in)
      projectile
      (projectile-addons :location local)
      org
      sh-script
      ssh-config-mode
      (tar-mode :location built-in)
      virtualenvwrapper
      whitespace
      whitespace-cleanup-mode
      yaml-mode))

;; List of packages to exclude.
(setq mineo-excluded-packages '())

(defun mineo/init-beacon ()
  "Initialize beacon."
  (use-package beacon
    :config
    (setq beacon-dont-blink-commands
          (delete 'recenter-top-bottom beacon-dont-blink-commands))
    (beacon-mode 1)))

(defun mineo/init-calc ()
  "Initialize calc."
  (use-package calc
    :config
    (setq calc-group-digits t
          calc-algebraic-mode t)))

(defun mineo/init-auth-source-pass ()
  "Initialize auth-source-pass."
  (use-package auth-source-pass
    :config
    (auth-source-pass-enable)))

(defun mineo/init-cask-mode ()
  "Initialize cask-mode."
  (use-package cask-mode))

(defun mineo/init-compact-docstrings ()
  "Initialize compact-docstrings."
  (use-package compact-docstrings
    :config
    (add-hook 'prog-mode-hook #'compact-docstrings-mode)))

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
    (setq copyright-names-regexp (user-full-name))))

(defun mineo/post-init-flycheck ()
  "Initialize flycheck."
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  ;; pylint with its default settings is much too strict.
  (setq-default flycheck-disabled-checkers '(python-pylint)
                ;; I don't install flake8 in all virtualenvs. Use the one
                ;; installed globally. Tests cover project-specific settings and
                ;; plugins.
                flycheck-python-flake8-executable "/usr/bin/python3")
  ;; I'm working with code bases where there's more than 400 warnings for a
  ;; single shell script, so bump this up :(
  (setq flycheck-checker-error-threshold 1000))

(defun mineo/init-flycheck-cask ()
  "Initialize flycheck-cask."
  (use-package flycheck-cask
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(defun mineo/post-init-git-commit ()
  "Post-initialize git-commit."
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

;; Required for mineo-git-clone-tmp
(defun mineo/init-github-clone ()
  "Initialize github-clone."
  (use-package github-clone))

(defun mineo/post-init-go-mode ()
  "Initialize go-mode."
  (spacemacs|use-package-add-hook go-mode
    :post-config (when (featurep 'whitespace)
                   (add-hook 'go-mode-hook #'spacemacs/toggle-whitespace-off))))

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
    (add-hook 'inferior-python-mode-hook 'mineo-python-shell-cd-project-root)
    (advice-add 'projectile-default-test-command :before-until #'mineo-projectile-default-tox-test-command)))

(defun mineo/init-ssh-config-mode ()
  "Initialize ssh-config-mode."
  (use-package ssh-config-mode
    :init
    ;; This is where ssh-config-mode looks for the keywords.txt file
    (setq ssh-config-load-file-dir (configuration-layer//get-package-directory 'ssh-config-mode))))

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

(defun mineo/post-init-magit ()
  "Post-initialize magit."
  (use-package magit
    :config
    (add-to-list 'evil-buffer-regexps
                 '("COMMIT_EDITMSG" . insert))
    (setq magit-revision-show-gravatars nil)

    ;; Some git remotes (for example GitHub) include direct links to pages for
    ;; opening pull requests etc. in their responses to pushes. Make them
    ;; clickable.
    (add-hook 'magit-process-mode-hook 'goto-address-mode)
    ;; Make URLs in commit messages clickable.
    (add-hook 'magit-revision-mode-hook 'goto-address-mode)
    ))

(defun mineo/post-init-org ()
  "Post-initialize org."
  (setq mineo-org-babel-languages '(C python shell))
  (with-eval-after-load 'org
    (dolist (language mineo-org-babel-languages)
      (cl-pushnew `(,language . t) org-babel-load-languages)))

  (defalias
    'spacemacs/helm-jump-in-buffer
    'helm-semantic-or-imenu
    "Always use `helm-semantic-or-imenu' for jumping in the buffer, especially in org-mode.")

  (setq org-directory "~/.org"
        org-default-notes-file (concat org-directory "/notes.org")
        ;; Open my notes file instead of the scratch buffer in new
        ;; emacs instances
        ;; Other stuff
        org-hide-emphasis-markers t
        org-ellipsis " ▾"
        org-agenda-files '("~/.org")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-bullets-bullet-list '("○"))

  (when (file-exists-p org-default-notes-file)
   (setq initial-buffer-choice org-default-notes-file))

  (add-hook 'org-mode-hook 'spacemacs-fixes//org-babel-do-load-languages)
  (add-hook 'org-mode-hook 'spacemacs/toggle-whitespace-off))

(defun mineo/post-init-projectile ()
  "Post-initialize projectile."
  (use-package projectile
    :config
    (dolist (dir '(".tox" ".cabal-sandbox" "dist" "build" ".eggs" "htmlcov" "docs/build" ".ensime_cache"))
      (add-to-list 'projectile-globally-ignored-directories dir)
      )
    (add-to-list 'projectile-globally-ignored-modes "tags-table-mode")
    (when (file-directory-p "~/dev")
       (setq projectile-project-search-path '("~/dev")))))

(defun mineo/post-init-sh-script ()
  "Post-initialize sh-script."
  (use-package sh-script
    :config
    (sh-electric-here-document-mode -1)))


(defun mineo/post-init-tar-mode()
  "Post-initialize tar-mode."
  (use-package tar-mode
    :config
    (evil-set-initial-state 'tar-mode 'emacs)))


(defun mineo/post-init-whitespace ()
  "Post-initialize whitespace."
  (use-package whitespace
    :config
    (setq spacemacs-show-trailing-whitespace nil
          whitespace-style '(face
                             tabs
                             trailing
                             space-before-tab
                             newline
                             empty
                             space-after-tab
                             tab-mark
                             newline-mark)
          whitespace-display-mappings
          '((newline-mark ?\n [?< ?\n])
            (tab-mark     ?\t [?\u00BB ?\t] [?\\ ?\t])))))

(defun mineo/post-init-yaml-mode ()
  "Initialize yaml-mode."
  (use-package yaml-mode
    :config
    (add-hook 'yaml-mode-hook 'goto-address-mode)))
