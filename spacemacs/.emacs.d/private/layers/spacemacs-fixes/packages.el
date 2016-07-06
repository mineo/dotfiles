;;; packages.el --- spacemacs-fixes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Wieland Hoffmann <wieland@mineo>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst spacemacs-fixes-packages
  '(org))

(defun spacemacs-fixes/post-init-org ()
  "Post-initialize org."
  (use-package org
    :config
    (add-hook 'org-mode-hook 'spacemacs-fixes//org-babel-do-load-languages)))


;;; packages.el ends here
