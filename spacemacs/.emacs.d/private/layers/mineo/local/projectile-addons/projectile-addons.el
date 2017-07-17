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

(defun mineo-python-get-tox-environments (path)
  "Get a list of all tox environments in PATH."
  (split-string (shell-command-to-string "tox -l") nil))

(defun mineo-python-select-tox-environment (path)
  "Interactive select a tox environment in PATH."
  (let* ((tox-environments (mineo-python-get-tox-environments path))
         (all-environments (pushnew "all" tox-environments))
         (selected-environment (completing-read "Tox environment: " all-environments)))
    (if (string= selected-environment "all")
        nil
      selected-environment)))

(defun mineo-projectile-default-tox-test-command (projectile-project-type)
  "Like `projectile-default-test-command', more clever for `python-tox'.

Figures out the tox test command for a project of type
PROJECTILE-PROJECT-TYPE. If PROJECTILE-PROJECT-TYPE is not
`python-tox', returns nil."
  (if (eq projectile-project-type 'python-tox)
      (let* ((tox-environment (mineo-python-select-tox-environment (projectile-project-root))))
        (if tox-environment
            (format "tox -e %s" tox-environment)
          "tox"))
    nil))

(provide 'projectile-addons)
;;; projectile-addons.el ends here
