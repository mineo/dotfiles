;;; keybindings.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Wieland Hoffmann

;; Author: Wieland Hoffmann <wieland@mineo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defconst mineo-rtags-overrides
  '(("C-]" 'rtags-find-symbol-at-point)
    ("M-." 'rtags-find-symbol-at-point)))

(defun mineo-rtags-set-evil-keys ()
  (dolist (override mineo-rtags-overrides)
    (evil-local-set-key 'normal (car override) (cdr override))))

(add-hook 'c-mode-common-hook 'mineo-rtags-set-evil-keys)

;;; https://github.com/mheathr/spacemacs/blob/develop/contrib/!lang/c-c%2B%2B/packages.el

(dolist (mode '(c-mode c++-mode))
  (evil-leader/set-key-for-mode mode
    "g ." 'rtags-find-symbol-at-point
    "g ," 'rtags-find-references-at-point
    "g v" 'rtags-find-virtuals-at-point
    "g V" 'rtags-print-enum-value-at-point
    "g /" 'rtags-find-all-references-at-point
    "g Y" 'rtags-cycle-overlays-on-screen
    "g >" 'rtags-find-symbol
    "g <" 'rtags-find-references
    "g [" 'rtags-location-stack-back
    "g ]" 'rtags-location-stack-forward
    "g D" 'rtags-diagnostics
    "g G" 'rtags-guess-function-at-point
    "g p" 'rtags-set-current-project
    "g P" 'rtags-print-dependencies
    "g e" 'rtags-reparse-file
    "g E" 'rtags-preprocess-file
    "g R" 'rtags-rename-symbol
    "g M" 'rtags-symbol-info
    "g S" 'rtags-display-summary
    "g O" 'rtags-goto-offset
    "g ;" 'rtags-find-file
    "g F" 'rtags-fixit
    "g L" 'rtags-copy-and-print-current-location
    "g X" 'rtags-fix-fixit-at-point
    "g B" 'rtags-show-rtags-buffer
    "g I" 'rtags-imenu
    "g T" 'rtags-taglist
    "g h" 'rtags-print-class-hierarchy
    "g a" 'rtags-print-source-arguments))

(provide 'keybindings)
;;; keybindings.el ends here
