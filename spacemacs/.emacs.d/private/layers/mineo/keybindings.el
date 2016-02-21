;;; keybindings.el --- My keybindings                -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Wieland Hoffmann

;; Author: Wieland Hoffmann <wieland@slartibartfass>
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

(spacemacs/set-leader-keys
  "o j c" 'jabber-connect-all
  "o j d" 'jabber-disconnect
  "o j r" 'jabber-switch-to-roster-buffer
  "o j w" 'jabber-chat-with
  "o o a" 'org-agenda
  "o o c" 'org-capture
  "o s"   'spacemacs/cycle-spacemacs-theme
  "o w"   'writeroom-mode
  "p j"   'helm-etags-select
  "p t"   'projectile-test-project)

(spacemacs/declare-prefix "o j" "jabber")
(spacemacs/declare-prefix "o o" "org")

(define-key evil-insert-state-map "\C-a" 'company-complete)
(define-key evil-insert-state-map (kbd "TAB") 'hippie-expand)

(provide 'keybindings)
;;; keybindings.el ends here
