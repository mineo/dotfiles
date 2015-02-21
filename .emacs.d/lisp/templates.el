;;; templates.el --- File templates with yasnippet   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wieland Hoffmann

;; Author: Wieland Hoffmann
;; Keywords: files, convenience

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

;;

;;; Code:

(require 'yasnippet)

(provide 'templates)
;;; templates.el ends here


(defgroup mineo-template
  nil
  "Customization group for my template stuff."
  :group 'tools)

(defcustom mineo-template-dir
  (locate-user-emacs-file "templates")
  "The directory containing file templates."
  :group 'mineo-template
  )

(defun mineo-expand-yas-buffer ()
  "Expand the whole buffer with yas-expand-snippet."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun mineo-sorted-files-in-dir ()
  "Return a sorted list of files in the template directory."
  (sort (file-expand-wildcards (concat mineo-template-dir "**/*")) 'string<)
  )

(defun mineo-template-filename-split-regex (filename)
  "Split the regular expression from FILENAME and return it."
  (nth 1 (split-string filename ":")))

(defun mineo-fill-alist ()
  "Fill auto-insert-alist."
  (dolist (filename (reverse (mineo-sorted-files-in-dir)) nil)
    (let ((file-regex (mineo-template-filename-split-regex filename)))
      (push `(,file-regex . [,filename mineo-expand-yas-buffer]) auto-insert-alist)
      )))
(provide 'templates)
;;; templates.el ends here
