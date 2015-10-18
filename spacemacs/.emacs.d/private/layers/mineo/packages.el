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
      enforce-column-mode
      highlight-symbol
      (mineo-private :location local)
      (projectile-addons :location local)
      yatemplate
      ))

;; List of packages to exclude.
(setq mineo-excluded-packages '())

;; For each package, define a function mineo/init-<package-name>
;;
;; (defun mineo/init-my-package ()
;;   "Initialize my package"
;;   )
(defun mineo/init-enforce-column-mode ()
  "Initialize enforce-column-mode."
  (use-package enforce-column-mode
    :config
    (add-hook 'prog-mode-hook 'enforce-column-mode)))

(defun mineo/init-highlight-symbol ()
  "Initialize highlight-symbol."
  (use-package highlight-symbol
    :config
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (setq highlight-symbol-on-navigation-p t
          highlight-symbol-idle-delay 0.4)))

(defun mineo/init-mineo-private ()
  "Initialize mineo-private."
  (use-package mineo-private
    :config
    (set-face-underline 'highlight-symbol-face t)
    (set-face-underline 'show-paren-match t)
    (advice-add 'spacemacs/cycle-spacemacs-theme :after
                #'(lambda () (set-face-underline 'highlight-symbol-face t)))
    (advice-add 'spacemacs/cycle-spacemacs-theme :after
                #'(lambda () (set-face-underline 'show-paren-match t)))
    (mineo-setup-jabber-accounts)))

(defun mineo/init-projectile-addons ()
  "Initialize projectile-addons."
  (use-package projectile-addons
    :config
    (mineo-initialize-projectile-idle-timer)
    (add-hook 'inferior-python-mode-hook 'mineo-python-shell-cd-project-root)))

(defun mineo/init-yatemplate ()
  "Initialize yatemplate."
    (use-package yatemplate
      :config
      (setq yatemplate-dir (locate-user-emacs-file "private/templates"))
      (yatemplate-fill-alist)
      (auto-insert-mode 1)))
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
