;;; funcs.el --- My functions                        -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Wieland Hoffmann

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
(defun mineo-configure-fonts (frame)
    "Set up additional fonts for Emoji and other things in FRAME."
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") nil 'prepend))

(defun mineo-face-overrides ()
  (set-face-underline 'highlight-symbol-face t)
  (set-face-underline 'sp-show-pair-match-face t)

  ;; These faces use a huge height by default
  (set-face-attribute 'jabber-title-large nil :height 1.2)
  (set-face-attribute 'jabber-title-medium nil :height 1.1)
  )


(add-hook 'after-make-frame-functions #'mineo-configure-fonts)
(advice-add 'spacemacs/cycle-spacemacs-theme :after
            #'mineo-face-overrides)

(provide 'funcs)
;;; funcs.el ends here
