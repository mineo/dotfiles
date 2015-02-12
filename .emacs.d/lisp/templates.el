;;; templates --- Summary
;;; Commentary:
;;; Code:

(require 'yasnippet)

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
  "Fill an alist."
(let ((template-alist ()))
  (dolist (filename (mineo-sorted-files-in-dir) template-alist)
    (let ((file-regex (mineo-template-filename-split-regex filename)))
      (add-to-list 'auto-insert-alist `(,file-regex . [,filename mineo-expand-yas-buffer]) t)
      ))))
(provide 'templates)
;;; templates.el ends here
