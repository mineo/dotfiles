;;; keybindings.el --- My keybindings                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wieland Hoffmann

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

(evil-leader/set-key
  "o j c" 'jabber-connect-all
  "o j d" 'jabber-disconnect-all
  "o j r" 'jabber-switch-to-roster-buffer
  "o j w" 'jabber-chat-with
  "o o a" 'org-agenda
  "o o c" 'org-capture)


(provide 'keybindings)
;;; keybindings.el ends here
