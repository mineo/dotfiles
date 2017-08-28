;;; funcs.el --- My functions                        -*- lexical-binding: t; -*-

;; Copyright (C) 2017Wieland Hoffmann

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

;;; Code:
(defun mineo-flycheck-rtags-setup ()
  "Setup Flycheck and RTags integration."
  ;; See RTags Flycheck integration at https://github.com/Andersbakken/rtags
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))

(provide 'funcs)
;;; funcs.el ends here
