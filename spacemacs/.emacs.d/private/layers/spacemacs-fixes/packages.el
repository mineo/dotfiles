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

(defun mineo/post-init-org ()
  "Post-initialize org."
  (use-package org
    :config
    (defalias
      'spacemacs/helm-jump-in-buffer
      'helm-semantic-or-imenu
      "Always use `helm-semantic-or-imenu' for jumping in the buffer, especially in org-mode.")))

;;; packages.el ends here
