;;; packages.el --- mineo-irony layer packages file for Spacemacs.
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

(defconst mineo-irony-packages
  '(
    company
    company-irony
    company-irony-c-headers
    irony
    irony-eldoc
    flycheck-irony
    ))

(defun mineo-irony/init-irony ()
  (use-package irony
    :config
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(defun mineo-irony/init-company-irony ()
  (use-package company-irony
    :init
    (push 'company-irony company-backends-c-mode-common)))

(defun mineo-irony/init-irony-eldoc ()
  (use-package irony-eldoc
    :config
    (add-hook 'irony-mode-hook 'irony-eldoc))
  )

(defun mineo-irony/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :init
    (with-eval-after-load 'company-irony
      (push '(company-irony-c-headers company-irony)
            company-backends-c-mode-common)))
  )

(defun mineo-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;; packages.el ends here
