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
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend))

(defun mineo-face-overrides ()
  (set-face-underline 'highlight-symbol-face t)
  (set-face-underline 'sp-show-pair-match-face t))

(defun mineo-git-clone-tmp (repository)
  "Clone REPOSITORY into a temporary folder."
  (interactive "MUrl or User/Repo:")
  (let ((is-github-repo (or
                         (string-match-p "github.com" repository)
                         ;; This regexp (except for the ^) has been taken from
                         ;; github-clone-repo-name
                         (string-match-p "^\\([[:alnum:]\-_.]+\\)/\\([[:alnum:]\-_.]+\\)$" repository)))
        (likely-target-directory (concat
                                  temporary-file-directory
                                  (cdr (github-clone-repo-name repository)))))
    (if is-github-repo
        (github-clone repository temporary-file-directory)
      ;; magit-clone runs asynchronously, figure out if there's any way to wait
      ;; for it be fore calling projectile-find-file-in-directory
      ;; (magit-clone repository temp-folder))
      (error "Cloning non-github repos is not supported right now :("))
    (projectile-find-file-in-directory likely-target-directory)))

(provide 'funcs)
;;; funcs.el ends here
