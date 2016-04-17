;;; config.el ---                                    -*- lexical-binding: t; -*-

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

;;; Commentary:

;; 

;;; Code:
(defun mineo-configure-fonts (frame)
  "Set up additional fonts for Emoji and other things in FRAME."
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") nil 'prepend))

(add-hook 'after-make-frame-functions #'mineo-configure-fonts)

(spacemacs|defvar-company-backends jabber-chat-mode)

(provide 'config)
;;; config.el ends here
