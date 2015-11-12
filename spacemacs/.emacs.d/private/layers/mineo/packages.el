;;; packages.el --- mineo Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
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
      auth-password-store
      beacon
      (bibtex :location built-in)
      column-enforce-mode
      company
      company-emoji
      copyright
      highlight-symbol
      jabber
      (midnight :location built-in)
      (mineo-private :location local)
      projectile
      (projectile-addons :location local)
      org
      show-paren
      virtualenvwrapper
      whitespace
      whitespace-cleanup-mode
      writeroom-mode
      yatemplate))

;; List of packages to exclude.
(setq mineo-excluded-packages '())

(defun mineo/init-beacon ()
  "Initialize beacon."
  (use-package beacon
    :config
    (beacon-mode)))

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

(defun mineo/init-company-emoji ()
  "Initialize company-emoji."
  (use-package company-emoji))

(defun mineo/init-copyright ()
  "Initialize copyright."
  (use-package copyright
    :config
    (add-hook 'find-file-hook #'copyright-update)
    (setq copyright-names-regexp (user-full-name))))

(defun mineo/init-highlight-symbol ()
  "Initialize highlight-symbol."
  (use-package highlight-symbol
    :config
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
    (setq highlight-symbol-on-navigation-p t
          highlight-symbol-idle-delay 0.4)))

(defun mineo/post-init-jabber ()
  "Initialize jabber."
  (use-package jabber
    :config
    (setq ;; Show message alerts for the current buffer because it might
     ;; be in an unfocused frame.
     jabber-message-alert-same-buffer t
     ;; DBus doesn't work for some reason
     jabber-libnotify-method 'shell
     ;; One auto-away change after 30 minutes is enough
     jabber-autoaway-timeout 30
     jabber-autoaway-xa-timeout 0
     ;; No avatars, please
     jabber-roster-line-format " %c %-25n\n   %u %-8s  %S")

    (setq jabber-chat-time-format "%H%M"
          ;; Align nicknames
          jabber-chat-foreign-prompt-format "%t %10n| "
          jabber-chat-local-prompt-format "%t %10n| ")
    (add-hook 'jabber-alert-message-hooks 'jabber-message-libnotify)
    ;; I don't need notifications about status changes in the echo area
    (remove-hook 'jabber-alert-presence-hooks 'jabber-presence-echo)
    (with-eval-after-load 'evil
      (evil-set-initial-state 'jabber-roster-mode 'emacs)
      (evil-set-initial-state 'jabber-chat-mode 'insert))
    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-hook jabber-chat-mode))
    (with-eval-after-load 'company-emoji
      (push 'company-emoji company-backends-jabber-chat-mode))))

(defun mineo/init-midnight ()
  "Initialize midnight."
  (use-package midnight))

(defun mineo/init-mineo-private ()
  "Initialize mineo-private."
  (use-package mineo-private
    :config
    (advice-add 'spacemacs/cycle-spacemacs-theme :after
                #'mineo-face-overrides))
    (mineo-setup-jabber-accounts)))

(defun mineo/init-projectile-addons ()
  "Initialize projectile-addons."
  (use-package projectile-addons
    :config
    (mineo-initialize-projectile-idle-timer)
    (add-hook 'inferior-python-mode-hook 'mineo-python-shell-cd-project-root)))

(defun mineo/init-show-paren-mode ()
  "Initialize show-paren-mode."
  (use-package show-paren-mode
    :config
    (show-paren-mode 1)))

(defun mineo/init-virtualenvwrapper ()
  "Initialize virtualenvwrapper."
  (use-package virtualenvwrapper
    :config
    (evil-leader/set-key-for-mode 'python-mode
      "m v" 'venv-workon
      "m V" 'venv-deactivate)
    (spacemacs|define-mode-line-segment virtualenvwrapper-active-env
      (format "venv:%s" venv-current-name)
      :when (equal major-mode 'python-mode))
    (add-to-list 'spacemacs-mode-line-left 'virtualenvwrapper-active-env t)))

(defun mineo/init-whitespace ()
  "Initialize whitespace."
  (use-package whitespace
    :config
    (setq spacemacs-show-trailing-whitespace nil)
    (setq whitespace-style '(face
                             indentation
                             line
                             trailing
                             space-after-tab
                             space-before-tab))))

(defun mineo/init-whitespace-cleanup-mode ()
  "Initialize whitespace-cleanup-mode."
  (use-package whitespace-cleanup-mode
    :config
    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
      (dolist (mode (list #'whitespace-mode #'whitespace-cleanup-mode))
        (add-hook hook mode)))))

(defun mineo/init-writeroom-mode ()
  "Initialize writeroom-mode."
  (use-package writeroom-mode))

(defun mineo/init-yatemplate ()
  "Initialize yatemplate."
    (use-package yatemplate
      :config
      (setq yatemplate-dir (locate-user-emacs-file "private/templates"))
      (yatemplate-fill-alist)
      (auto-insert-mode 1)))

(defun mineo/post-init-org ()
  "Post-initialize org."
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
             "- [ ] %?"))))

(defun mineo/post-init-projectile ()
  "Post-initialize projectile."
  (use-package projectile
    :config
    (dolist (dir '(".tox" ".cabal-sandbox" "dist" "build" ".eggs" "htmlcov" "docs/build" ".ensime_cache"))
      (add-to-list 'projectile-globally-ignored-directories dir)
      )
    (add-to-list 'projectile-globally-ignored-modes "tags-table-mode")))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
