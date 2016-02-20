;;; projectile-addons.el --- Some additional functions for projectile  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016  Wieland Hoffmann

;; Author: Wieland Hoffmann
;; Package-Requires: ((python "0.24.4") (projectile "0.11.0"))
;; Package-Version: 0.1.0

;; This file is not part of GNU Emacs.

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

;; Utility functions for projectile that it doesn't provide by itself.

;;; Code:
(require 'projectile)

(defun mineo-python-shell-cd-project-root ()
  "CD the python shell to the project root."
  (let* ((proot (projectile-project-root))
         (cdmessage (format "%%cd %s" proot)))
    (when proot
        (python-shell-send-string cdmessage (get-buffer-process (current-buffer))))))

(provide 'projectile-addons)
;;; projectile-addons.el ends here
