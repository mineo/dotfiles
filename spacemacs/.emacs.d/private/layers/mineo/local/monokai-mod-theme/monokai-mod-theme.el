;;; monokai-mod-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/monokai-emacs
;; Version: 2.0.1

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
;; A port of the popular Textmate theme Monokai for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.monokai.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal monokai theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/monokai-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The monokai-mod theme requires Emacs 24 or later!"))

(deftheme monokai-mod "The monokai-mod colour theme")

(defgroup monokai-mod nil
  "monokai-mod theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom monokai-mod-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'monokai-mod)

(defcustom monokai-mod-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'monokai-mod)

(defcustom monokai-mod-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'monokai-mod)

(defcustom monokai-mod-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'monokai-mod)

(defcustom monokai-mod-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'monokai-mod)

(defcustom monokai-mod-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'monokai-mod)

(defcustom monokai-mod-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'monokai-mod)

(let* (;; Variable pitch
       (monokai-mod-pitch (if monokai-mod-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (class                    '((class color) (min-colors 257)))
       ;; Primary colors
       (monokai-mod-yellow           "#E6DB74")
       (monokai-mod-orange           "#FD971F")
       (monokai-mod-red              "#F92672")
       (monokai-mod-magenta          "#FD5FF0")
       (monokai-mod-violet           "#AE81FF")
       (monokai-mod-blue             "#66D9EF")
       (monokai-mod-cyan             "#A1EFE4")
       (monokai-mod-green            "#A6E22E")
       (monokai-mod-gray             "#3E3D31")
       ;; Darker and lighter accented colors
       (monokai-mod-yellow-d         "#BEB244")
       (monokai-mod-yellow-l         "#FFF7A8")
       (monokai-mod-orange-d         "#D47402")
       (monokai-mod-orange-l         "#FFAC4A")
       (monokai-mod-red-d            "#F70057")
       (monokai-mod-red-l            "#FA518D")
       (monokai-mod-magenta-d        "#FB35EA")
       (monokai-mod-magenta-l        "#FE8CF4")
       (monokai-mod-violet-d         "#945AFF")
       (monokai-mod-violet-l         "#C9ACFF")
       (monokai-mod-blue-d           "#40CAE4")
       (monokai-mod-blue-l           "#92E7F7")
       (monokai-mod-cyan-d           "#74DBCD")
       (monokai-mod-cyan-l           "#D3FBF6")
       (monokai-mod-green-d          "#86C30D")
       (monokai-mod-green-l          "#BBEF53")
       (monokai-mod-gray-d           "#35331D")
       (monokai-mod-gray-l           "#7B7962")
       ;; Adaptive colors
       (monokai-mod-fg               "#F8F8F2")
       (monokai-mod-bg               "#1B1D1E")
       (monokai-mod-highlight-line   "#293739")
       (monokai-mod-highlight        "#FFB269")
       (monokai-mod-emph             "#F8F8F0")
       (monokai-mod-comments         "#75715E")
       ;; Adaptive higher/lower contrast accented colors
       (monokai-mod-fg-hc            "#141414")
       (monokai-mod-fg-lc            "#171A0B")
       ;; High contrast colors
       (monokai-mod-yellow-hc        "#FFFACE")
       (monokai-mod-yellow-lc        "#9A8F21")
       (monokai-mod-orange-hc        "#FFBE74")
       (monokai-mod-orange-lc        "#A75B00")
       (monokai-mod-red-hc           "#FEB0CC")
       (monokai-mod-red-lc           "#F20055")
       (monokai-mod-magenta-hc       "#FEC6F9")
       (monokai-mod-magenta-lc       "#F309DF")
       (monokai-mod-violet-hc        "#F0E7FF")
       (monokai-mod-violet-lc        "#7830FC")
       (monokai-mod-blue-hc          "#CAF5FD")
       (monokai-mod-blue-lc          "#1DB4D0")
       (monokai-mod-cyan-hc          "#D3FBF6")
       (monokai-mod-cyan-lc          "#4BBEAE")
       (monokai-mod-green-hc         "#CCF47C")
       (monokai-mod-green-lc         "#679A01")
       ;; Distinct fringe
       (monokai-mod-fringe-bg (if monokai-mod-distinct-fringe-background
                              "#232526"
                            monokai-mod-bg))

       ;; Definitions for terminals that do not support 256 colors
       (terminal-class                    '((class color) (min-colors 89)))
       ;; Primary colors
       (terminal-monokai-mod-yellow           "#CDC673")
       (terminal-monokai-mod-orange           "#FF8C00")
       (terminal-monokai-mod-red              "#FF1493")
       (terminal-monokai-mod-magenta          "#D700D7")
       (terminal-monokai-mod-violet           "#AF87FF")
       (terminal-monokai-mod-blue             "#5FD7FF")
       (terminal-monokai-mod-cyan             "#5FFFFF")
       (terminal-monokai-mod-green            "#87D700")
       (terminal-monokai-mod-gray             "#3D3D3D")
       ;; Darker and lighter accented colors
       (terminal-monokai-mod-yellow-d         "#878700")
       (terminal-monokai-mod-yellow-l         "#FFFF87")
       (terminal-monokai-mod-orange-d         "#AF5F00")
       (terminal-monokai-mod-orange-l         "#FFAF5F")
       (terminal-monokai-mod-red-d            "#870000")
       (terminal-monokai-mod-red-l            "#FF5F87")
       (terminal-monokai-mod-magenta-d        "#AF0087")
       (terminal-monokai-mod-magenta-l        "#FF87DF")
       (terminal-monokai-mod-violet-d         "#5F00AF")
       (terminal-monokai-mod-violet-l         "#AF87D7")
       (terminal-monokai-mod-blue-d           "#008787")
       (terminal-monokai-mod-blue-l           "#87D7FF")
       (terminal-monokai-mod-cyan-d           "#5FAFAF")
       (terminal-monokai-mod-cyan-l           "#AFFFFF")
       (terminal-monokai-mod-green-d          "#5F8700")
       (terminal-monokai-mod-green-l          "#AFD700")
       (terminal-monokai-mod-gray-d           "#333333")
       (terminal-monokai-mod-gray-l           "#707070")
       ;; Adaptive colors
       (terminal-monokai-mod-fg               "#F5F5F5")
       (terminal-monokai-mod-bg               "#1B1E1C")
       (terminal-monokai-mod-highlight-line   "#474747")
       (terminal-monokai-mod-highlight        "#F4A460")
       (terminal-monokai-mod-emph             "#FFFAFA")
       (terminal-monokai-mod-comments         "#8B8878")
       ;; Adaptive higher/lower contrast accented colors
       (terminal-monokai-mod-fg-hc            "#171A0B")
       (terminal-monokai-mod-fg-lc            "#141414")
       ;; High contrast colors
       (terminal-monokai-mod-yellow-hc        terminal-monokai-mod-yellow-d)
       (terminal-monokai-mod-yellow-lc        terminal-monokai-mod-yellow-l)
       (terminal-monokai-mod-orange-hc        terminal-monokai-mod-orange-d)
       (terminal-monokai-mod-orange-lc        terminal-monokai-mod-orange-l)
       (terminal-monokai-mod-red-hc           terminal-monokai-mod-red-d)
       (terminal-monokai-mod-red-lc           terminal-monokai-mod-red-l)
       (terminal-monokai-mod-magenta-hc       terminal-monokai-mod-magenta-d)
       (terminal-monokai-mod-magenta-lc       terminal-monokai-mod-magenta-l)
       (terminal-monokai-mod-violet-hc        terminal-monokai-mod-violet-d)
       (terminal-monokai-mod-violet-lc        terminal-monokai-mod-violet-l)
       (terminal-monokai-mod-blue-hc          terminal-monokai-mod-blue-d)
       (terminal-monokai-mod-blue-lc          terminal-monokai-mod-blue-l)
       (terminal-monokai-mod-cyan-hc          terminal-monokai-mod-cyan-d)
       (terminal-monokai-mod-cyan-lc          terminal-monokai-mod-cyan-l)
       (terminal-monokai-mod-green-hc         terminal-monokai-mod-green-d)
       (terminal-monokai-mod-green-lc         terminal-monokai-mod-green-l)
       ;; Distinct fringe
       (terminal-monokai-mod-fringe-bg (if monokai-mod-distinct-fringe-background
                                       terminal-monokai-mod-gray
                                     terminal-monokai-mod-bg)))

  ;; Define faces
  (custom-theme-set-faces
   'monokai-mod

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,class (:foreground ,monokai-mod-red
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(font-lock-comment-face
     ((,class (:foreground ,monokai-mod-comments
                           :background nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(font-lock-constant-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(font-lock-doc-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(font-lock-function-name-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(font-lock-keyword-face
     ((,class (:foreground ,monokai-mod-red
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight normal))))

   `(font-lock-negation-char-face
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(font-lock-preprocessor-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(font-lock-regexp-grouping-construct
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,class (:foreground ,monokai-mod-violet
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :weight normal))))

   `(font-lock-string-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(font-lock-type-face
     ((,class (:foreground ,monokai-mod-blue
                           :italic nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :italic nil))))

   `(font-lock-variable-name-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(font-lock-warning-face
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold
                           :italic t
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight bold
                                    :italic t
                                    :underline t))))

   `(c-annotation-face
     ((,class (:inherit font-lock-constant-face))
      (,terminal-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,class (:foreground ,monokai-mod-fg
                            :background ,monokai-mod-bg))
       (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                     :background ,terminal-monokai-mod-bg))))

   `(highlight
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-highlight))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-highlight))))

   `(lazy-highlight
     ((,class (:inherit highlight
                        :background ,monokai-mod-comments))
      (,terminal-class (:inherit highlight
                                 :background ,terminal-monokai-mod-comments))))

   `(region
     ((,class (:inherit highlight
                        :background ,monokai-mod-highlight))
      (,terminal-class (:inherit highlight
                                 :background ,terminal-monokai-mod-highlight))))

   `(secondary-selection
     ((,class (:inherit region
                        :background ,monokai-mod-blue))
      (,terminal-class (:inherit region
                                 :background ,terminal-monokai-mod-blue))))

   `(shadow
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(match
     ((,class (:background ,monokai-mod-green
                           :foreground ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-green
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold))))

   `(cursor
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-fg
                                    :inverse-video t))))

   `(mouse
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-fg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-fg
                                    :inverse-video t))))

   `(escape-glyph
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(escape-glyph-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(fringe
     ((,class (:foreground ,monokai-mod-fg
                           :background ,monokai-mod-fringe-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :background ,terminal-monokai-mod-fringe-bg))))

   `(link
     ((,class (:foreground ,monokai-mod-blue
                           :underline t
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :underline t
                                    :weight bold))))

   `(link-visited
     ((,class (:foreground ,monokai-mod-violet
                           :underline t
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :underline t
                                    :weight normal))))

   `(success
     ((,class (:foreground ,monokai-mod-green ))
      (,terminal-class (:foreground ,terminal-monokai-mod-green ))))

   `(warning
     ((,class (:foreground ,monokai-mod-yellow ))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow ))))

   `(error
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(eval-sexp-fu-flash
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-green))))

   `(eval-sexp-fu-flash-error
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-red))))

   `(trailing-whitespace
     ((,class (:background ,monokai-mod-red))
      (,terminal-class (:background ,terminal-monokai-mod-red))))

   `(vertical-border
     ((,class (:foreground ,monokai-mod-gray))
      (,terminal-class (:foreground ,terminal-monokai-mod-gray))))

   `(menu
     ((,class (:foreground ,monokai-mod-fg
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :background ,terminal-monokai-mod-bg))))

   `(minibuffer-prompt
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,class (:foreground ,monokai-mod-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold))))

   `(mode-line
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,monokai-mod-emph
                              :background ,monokai-mod-highlight-line
                              :box (:line-width 1
                                                :color ,monokai-mod-gray
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-monokai-mod-fg
                                       :background ,terminal-monokai-mod-bg
                                       :box (:line-width 1
                                                         :color ,terminal-monokai-mod-highlight-line
                                                         :style unspecified)))))

   `(powerline-active1
     ((,class (:background ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-gray-d))))

   `(powerline-active2
     ((,class (:background ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-bg))))


   `(mode-line-inactive
     ((,class (:inverse-video unspecified
                              :underline unspecified
                              :foreground ,monokai-mod-comments
                              :background ,monokai-mod-bg
                              :box (:line-width 1
                                                :color ,monokai-mod-gray
                                                :style unspecified)))
      (,terminal-class (:inverse-video unspecified
                                       :underline unspecified
                                       :foreground ,terminal-monokai-mod-comments
                                       :background ,terminal-monokai-mod-bg
                                       :box (:line-width 1
                                                         :color ,terminal-monokai-mod-gray
                                                         :style unspecified)))))

   `(powerline-inactive1
     ((,class (:background ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-gray-d))))

   `(powerline-inactive2
     ((,class (:background ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-gray))))

   ;; header-line
   `(header-line
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-highlight-line
                           :box (:color ,monokai-mod-gray
                                        :line-width 1
                                        :style unspecified)))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-highlight-line
                                    :box (:color ,terminal-monokai-mod-gray
                                                 :line-width 1
                                                 :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,class (:background ,monokai-mod-yellow
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground ,terminal-monokai-mod-bg))))

   `(cua-rectangle
     ((,class (:inherit region))
      (,terminal-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,class (:inherit secondary-selection))
      (,terminal-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   ;; dired
   `(dired-directory
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(dired-flagged
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(dired-header
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(dired-ignored
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   `(dired-mark
     ((,class (:foreground ,monokai-mod-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold))))

   `(dired-marked
     ((,class (:foreground ,monokai-mod-violet
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :inherit bold))))

   `(dired-perm-write
     ((,class (:foreground ,monokai-mod-fg
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :underline t))))

   `(dired-symlink
     ((,class (:foreground ,monokai-mod-cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :slant italic))))

   `(dired-warning
     ((,class (:foreground ,monokai-mod-orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-blue))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-blue))))

   `(dropdown-list-selection-face
     ((,class (:background ,monokai-mod-green
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-green
                                    :foreground ,terminal-monokai-mod-bg))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,class (:inherit ecb-history-bucket-node-face
                        :foreground ,monokai-mod-yellow))
      (,terminal-class (:inherit ecb-history-bucket-node-face
                                 :foreground ,terminal-monokai-mod-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,monokai-mod-fg))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-monokai-mod-fg))))

   `(ecb-history-dead-buffer-face
     ((,class (:inherit ecb-history-general-face
                        :foreground ,monokai-mod-comments))
      (,terminal-class (:inherit ecb-history-general-face
                                 :foreground ,terminal-monokai-mod-comments))))

   `(ecb-directory-not-accessible-face
     ((,class (:inherit ecb-directories-general-face
                        :foreground ,monokai-mod-comments))
      (,terminal-class (:inherit ecb-directories-general-face
                                 :foreground ,terminal-monokai-mod-comments))))

   `(ecb-bucket-node-face
     ((,class (:inherit ecb-default-general-face
                        :weight normal
                        :foreground ,monokai-mod-blue))
      (,terminal-class (:inherit ecb-default-general-face
                                 :weight normal
                                 :foreground ,terminal-monokai-mod-blue))))

   `(ecb-tag-header-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,class (:inherit ecb-analyse-general-face
                        :foreground ,monokai-mod-green))
      (,terminal-class (:inherit ecb-analyse-general-face
                                 :foreground ,terminal-monokai-mod-green))))

   `(ecb-directories-general-face
     ((,class (:inherit ecb-default-general-face
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,class (:inherit ecb-methods-general-face
                        :foreground ,monokai-mod-cyan))
      (,terminal-class (:inherit ecb-methods-general-face
                                 :foreground ,terminal-monokai-mod-cyan))))

   `(ecb-mode-line-prefix-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(ecb-tree-guide-line-face
     ((,class (:inherit ecb-default-general-face
                        :foreground ,monokai-mod-gray
                        :height 1.0))
      (,terminal-class (:inherit ecb-default-general-face
                                 :foreground ,terminal-monokai-mod-gray
                                 :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,class (:foreground ,monokai-mod-emph))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph))))

   `(ee-category
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(ee-link
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(ee-link-visited
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(ee-marked
     ((,class (:foreground ,monokai-mod-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta
                                    :weight bold))))

   `(ee-omitted
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(ee-shadow
     ((,class (:inherit shadow))
      (,terminal-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(grep-error-face
     ((,class (:foreground ,monokai-mod-red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold
                                    :underline t))))

   `(grep-hit-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(grep-match-face
     ((,class (:foreground ,monokai-mod-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold))))

   ;; isearch
   `(isearch
     ((,class (:inherit region
                        :background ,monokai-mod-green))
      (,terminal-class (:inherit region
                                 :background ,terminal-monokai-mod-green))))

   `(isearch-fail
     ((,class (:inherit isearch
                        :foreground ,monokai-mod-red
                        :background ,monokai-mod-bg
                        :bold t))
      (,terminal-class (:inherit isearch
                                 :foreground ,terminal-monokai-mod-red
                                 :background ,terminal-monokai-mod-bg
                                 :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-bg
                           :inverse-video nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-bg
                                    :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,class (:foreground ,monokai-mod-yellow
                           :background ,monokai-mod-bg
                           :inverse-video nil
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :background ,terminal-monokai-mod-bg
                                    :inverse-video nil
                                    :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,class (:inherit bold
                        :foreground ,monokai-mod-emph))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-monokai-mod-emph))))

   `(font-latex-doctex-documentation-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,class (:inherit italic :foreground ,monokai-mod-emph))
      (,terminal-class (:inherit italic :foreground ,terminal-monokai-mod-emph))))

   `(font-latex-math-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(font-latex-sectioning-0-face
     ((,class (:inherit font-latex-sectioning-1-face
                        :height ,monokai-mod-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-1-face
                                 :height ,monokai-mod-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,class (:inherit font-latex-sectioning-2-face
                        :height ,monokai-mod-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-2-face
                                 :height ,monokai-mod-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,class (:inherit font-latex-sectioning-3-face
                        :height ,monokai-mod-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-3-face
                                 :height ,monokai-mod-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,class (:inherit font-latex-sectioning-4-face
                        :height ,monokai-mod-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-4-face
                                 :height ,monokai-mod-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,class (:inherit font-latex-sectioning-5-face
                        :height ,monokai-mod-height-plus-1))
      (,terminal-class (:inherit font-latex-sectioning-5-face
                                 :height ,monokai-mod-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-yellow
                        :weight bold))
      (,terminal-class (:inherit ,monokai-mod-pitch :
                                 foreground ,terminal-monokai-mod-yellow
                                 :weight bold))))

   `(font-latex-sedate-face
     ((,class (:foreground ,monokai-mod-emph))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph))))

   `(font-latex-slide-title-face
     ((,class (:inherit (,monokai-mod-pitch font-lock-type-face)
                        :weight bold
                        :height ,monokai-mod-height-plus-3))
      (,terminal-class (:inherit (,monokai-mod-pitch font-lock-type-face)
                                 :weight bold
                                 :height ,monokai-mod-height-plus-3))))

   `(font-latex-string-face
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(font-latex-subscript-face
     ((,class (:height ,monokai-mod-height-minus-1))
      (,terminal-class (:height ,monokai-mod-height-minus-1))))

   `(font-latex-superscript-face
     ((,class (:height ,monokai-mod-height-minus-1))
      (,terminal-class (:height ,monokai-mod-height-minus-1))))

   `(font-latex-verbatim-face
     ((,class (:inherit fixed-pitch
                        :foreground ,monokai-mod-fg
                        :slant italic))
      (,terminal-class (:inherit fixed-pitch
                                 :foreground ,terminal-monokai-mod-fg
                                 :slant italic))))

   `(font-latex-warning-face
     ((,class (:inherit bold
                        :foreground ,monokai-mod-orange))
      (,terminal-class (:inherit bold
                                 :foreground ,terminal-monokai-mod-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-blue))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-blue))))

   `(ac-selection-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(ac-candidate-mouse-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(ac-completion-face
     ((,class (:foreground ,monokai-mod-emph
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :underline t))))

   `(ac-gtags-candidate-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-blue))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-blue))))

   `(ac-gtags-selection-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(ac-yasnippet-candidate-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-yellow))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-yellow))))

   `(ac-yasnippet-selection-face
     ((,class (:background ,monokai-mod-yellow
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground ,terminal-monokai-mod-bg))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-blue))))

   `(ahs-edit-mode-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-highlight))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-highlight))))

   `(ahs-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta
                                    :background unspecified))))

   `(ahs-plugin-bod-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-violet ))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-cyan ))))

   `(ahs-plugin-defalt-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-green))))

   `(ahs-warning-face
     ((,class (:foreground ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(android-mode-error-face
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight bold))))

   `(android-mode-info-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(android-mode-verbose-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(android-mode-warning-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,class (:foreground ,monokai-mod-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :weight bold))))

   ;; bm
   `(bm-face
     ((,class (:background ,monokai-mod-yellow-lc
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc
                                    :foreground ,terminal-monokai-mod-bg))))

   `(bm-fringe-face
     ((,class (:background ,monokai-mod-yellow-lc
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc
                                    :foreground ,terminal-monokai-mod-bg))))

   `(bm-fringe-persistent-face
     ((,class (:background ,monokai-mod-green-lc
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc
                                    :foreground ,terminal-monokai-mod-bg))))

   `(bm-persistent-face
     ((,class (:background ,monokai-mod-green-lc
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc
                                    :foreground ,terminal-monokai-mod-bg))))

   ;; calfw
   `(cfw:face-day-title
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(cfw:face-annotation
     ((,class (:inherit cfw:face-day-title
                        :foreground ,monokai-mod-yellow))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-monokai-mod-yellow))))

   `(cfw:face-default-content
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(cfw:face-default-day
     ((,class (:inherit cfw:face-day-title
                        :weight bold))
      (,terminal-class (:inherit cfw:face-day-title
                                 :weight bold))))

   `(cfw:face-disable
     ((,class (:inherit cfw:face-day-title
                        :foreground ,monokai-mod-comments))
      (,terminal-class (:inherit cfw:face-day-title
                                 :foreground ,terminal-monokai-mod-comments))))

   `(cfw:face-grid
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(cfw:face-header
     ((,class (:foreground ,monokai-mod-blue-hc
                           :background ,monokai-mod-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue-hc
                                    :background ,terminal-monokai-mod-blue-lc
                                    :weight bold))))

   `(cfw:face-holiday
     ((,class (:background nil
                           :foreground ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:background nil
                                    :foreground ,terminal-monokai-mod-red
                                    :weight bold))))

   `(cfw:face-periods
     ((,class (:foreground ,monokai-mod-magenta))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta))))

   `(cfw:face-select
     ((,class (:background ,monokai-mod-magenta-lc
                           :foreground ,monokai-mod-magenta-hc))
      (,terminal-class (:background ,terminal-monokai-mod-magenta-lc
                                    :foreground ,terminal-monokai-mod-magenta-hc))))

   `(cfw:face-saturday
     ((,class (:foreground ,monokai-mod-cyan-hc
                           :background ,monokai-mod-cyan-lc))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan-hc
                                    :background ,terminal-monokai-mod-cyan-lc))))

   `(cfw:face-sunday
     ((,class (:foreground ,monokai-mod-red-hc
                           :background ,monokai-mod-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red-hc
                                    :background ,terminal-monokai-mod-red-lc
                                    :weight bold))))

   `(cfw:face-title
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-yellow
                        :weight bold
                        :height ,monokai-mod-height-plus-4))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-yellow
                                 :weight bold
                                 :height ,monokai-mod-height-plus-4))))

   `(cfw:face-today
     ((,class (:weight bold
                       :background ,monokai-mod-highlight-line
                       :foreground nil))
      (,terminal-class (:weight bold
                                :background ,terminal-monokai-mod-highlight-line
                                :foreground nil))))

   `(cfw:face-today-title
     ((,class (:background ,monokai-mod-yellow-lc
                           :foreground ,monokai-mod-yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc
                                    :foreground ,terminal-monokai-mod-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg))))

   `(cfw:face-toolbar-button-off
     ((,class (:background ,monokai-mod-yellow-lc
                           :foreground ,monokai-mod-yellow-hc
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc
                                    :foreground ,terminal-monokai-mod-yellow-hc
                                    :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,class (:background ,monokai-mod-yellow-hc
                           :foreground ,monokai-mod-yellow-lc
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-hc
                                    :foreground ,terminal-monokai-mod-yellow-lc
                                    :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,class (:foreground ,monokai-mod-yellow
                           :background nil
                           :box (:color ,monokai-mod-yellow :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :background nil
                                    :box (:color ,terminal-monokai-mod-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(cider-instrumented-face
     ((,class (:foreground ,monokai-mod-violet
                           :background nil
                           :box (:color ,monokai-mod-violet :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :background nil
                                    :box (:color ,terminal-monokai-mod-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,class (:foreground ,monokai-mod-blue
                           :background nil
                           :box (:color ,monokai-mod-blue :line-width -1 :style nil)))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background nil
                                    :box (:color ,terminal-monokai-mod-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-orange))))

   `(cider-test-failure-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-red))))

   `(cider-test-success-face
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-green))))

   `(cider-traced-face
     ((,class :box (:color ,monokai-mod-blue :line-width -1 :style nil))
      (,terminal-class :box (:color ,terminal-monokai-mod-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,class (:foreground ,monokai-mod-red
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-error-face
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold
                                    :underline t))))

   `(clojure-test-success-face
     ((,class (:foreground ,monokai-mod-green
                           :weight bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold
                                    :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph))))

   `(company-tooltip-selection
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(company-tooltip-mouse
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(company-tooltip-common
     ((,class (:foreground ,monokai-mod-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :underline t))))

   `(company-tooltip-common-selection
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-blue
                                    :underline t))))

   `(company-preview
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph))))

   `(company-preview-common
     ((,class (:foreground ,monokai-mod-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :underline t))))

   `(company-scrollbar-bg
     ((,class (:background ,monokai-mod-gray))
      (,terminal-class (:background ,terminal-monokai-mod-gray))))

   `(company-scrollbar-fg
     ((,class (:background ,monokai-mod-comments))
      (,terminal-class (:background ,terminal-monokai-mod-comments))))

   `(company-tooltip-annotation
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-green))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-green))))

   `(company-template-field
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-blue))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-blue))))

   ;; compilation
   `(compilation-column-face
     ((,class (:foreground ,monokai-mod-cyan
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :underline nil))))

   `(compilation-column-number
     ((,class (:inherit font-lock-doc-face
                        :foreground ,monokai-mod-cyan
                        :underline nil))
      (,terminal-class (:inherit font-lock-doc-face
                                 :foreground ,terminal-monokai-mod-cyan
                                 :underline nil))))

   `(compilation-enter-directory-face
     ((,class (:foreground ,monokai-mod-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :underline nil))))

   `(compilation-error
     ((,class (:inherit error
                        :underline nil))
      (,terminal-class (:inherit error
                                 :underline nil))))

   `(compilation-error-face
     ((,class (:foreground ,monokai-mod-red
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :underline nil))))

   `(compilation-face
     ((,class (:foreground ,monokai-mod-fg
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :underline nil))))

   `(compilation-info
     ((,class (:foreground ,monokai-mod-comments
                           :underline nil
                           :bold nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :underline nil
                                    :bold nil))))

   `(compilation-info-face
     ((,class (:foreground ,monokai-mod-blue
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :underline nil))))

   `(compilation-leave-directory-face
     ((,class (:foreground ,monokai-mod-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :underline nil))))

   `(compilation-line-face
     ((,class (:foreground ,monokai-mod-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :underline nil))))

   `(compilation-line-number
     ((,class (:foreground ,monokai-mod-green
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :underline nil))))

   `(compilation-warning
     ((,class (:inherit warning
                        :underline nil))
      (,terminal-class (:inherit warning
                                 :underline nil))))

   `(compilation-warning-face
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal
                                    :underline nil))))

   `(compilation-mode-line-exit
     ((,class (:inherit compilation-info
                        :foreground ,monokai-mod-green
                        :weight bold))
      (,terminal-class (:inherit compilation-info
                                 :foreground ,terminal-monokai-mod-green
                                 :weight bold))))

   `(compilation-mode-line-fail
     ((,class (:inherit compilation-error
                        :foreground ,monokai-mod-red
                        :weight bold))
      (,terminal-class (:inherit compilation-error
                                 :foreground ,terminal-monokai-mod-red
                                 :weight bold))))

   `(compilation-mode-line-run
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,class (:foreground ,monokai-mod-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold))))

   `(cscope-function-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(cscope-line-number-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(cscope-line-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(cscope-mouse-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-fg))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph
                           :underline ,monokai-mod-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph
                                    :underline ,terminal-monokai-mod-emph
                                    :weight bold))))

   `(ctbl:face-continue-bar
     ((,class (:background ,monokai-mod-gray
                           :foreground ,monokai-mod-yellow))
      (,terminal-class (:background ,terminal-monokai-mod-gray
                                    :foreground ,terminal-monokai-mod-yellow))))

   `(ctbl:face-row-select
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg
                           :underline t))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg
                                    :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(coffee-mode-function-param
     ((,class (:foreground ,monokai-mod-violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,class (:inherit ,monokai-mod-pitch
                        :height ,monokai-mod-height-plus-3
                        :foreground ,monokai-mod-violet
                        :weight bold))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :height ,monokai-mod-height-plus-3
                                 :foreground ,terminal-monokai-mod-violet
                                 :weight bold))))

   `(custom-variable-tag
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-cyan
                        :height ,monokai-mod-height-plus-3))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-cyan
                                 :height ,monokai-mod-height-plus-3))))

   `(custom-comment-tag
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(custom-group-tag
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-blue
                        :height ,monokai-mod-height-plus-3))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-blue
                                 :height ,monokai-mod-height-plus-3))))

   `(custom-group-tag-1
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-red
                        :height ,monokai-mod-height-plus-3))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-red
                                 :height ,monokai-mod-height-plus-3))))

   `(custom-state
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   ;; diff
   `(diff-added
     ((,class (:foreground ,monokai-mod-orange
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-bg))))

   `(diff-changed
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-bg))))

   `(diff-removed
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg))))

   `(diff-context
     ((,class (:background ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-bg))))

   `(diff-header
     ((,class (:background ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-bg))))

   `(diff-file-header
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-fg
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-fg
                                    :weight bold))))

   `(diff-refine-added
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-green))))

   `(diff-refine-change
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-blue))))

   `(diff-refine-removed
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,class (:background ,monokai-mod-blue-lc
                           :foreground ,monokai-mod-blue-hc))
      (,terminal-class (:background ,terminal-monokai-mod-blue-lc
                                    :foreground ,terminal-monokai-mod-blue-hc))))

   `(diff-hl-delete
     ((,class (:background ,monokai-mod-red-lc
                           :foreground ,monokai-mod-red-hc))
      (,terminal-class (:background ,terminal-monokai-mod-red-lc
                                    :foreground ,terminal-monokai-mod-red-hc))))

   `(diff-hl-insert
     ((,class (:background ,monokai-mod-green-lc
                           :foreground ,monokai-mod-green-hc))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc
                                    :foreground ,terminal-monokai-mod-green-hc))))

   `(diff-hl-unknown
     ((,class (:background ,monokai-mod-violet-lc
                           :foreground ,monokai-mod-violet-hc))
      (,terminal-class (:background ,terminal-monokai-mod-violet-lc
                                    :foreground ,terminal-monokai-mod-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,class (:background ,monokai-mod-orange-lc))
      (,terminal-class (:background ,terminal-monokai-mod-orange-lc))))

   `(ediff-fine-diff-B
     ((,class (:background ,monokai-mod-green-lc))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc))))

   `(ediff-fine-diff-C
     ((,class (:background ,monokai-mod-yellow-lc))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc))))

   `(ediff-current-diff-C
     ((,class (:background ,monokai-mod-blue-lc))
      (,terminal-class (:background ,terminal-monokai-mod-blue-lc))))

   `(ediff-even-diff-A
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-fg-lc ))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-fg-lc ))))

   `(ediff-odd-diff-A
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-fg-hc ))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-fg-hc ))))

   `(ediff-even-diff-B
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-fg-hc ))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-fg-hc ))))

   `(ediff-odd-diff-B
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-fg-lc ))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-fg-lc ))))

   `(ediff-even-diff-C
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-fg ))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-fg ))))

   `(ediff-odd-diff-C
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-bg ))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-bg ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,monokai-mod-red)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-red-hc
                           :background ,monokai-mod-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-monokai-mod-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-red-hc
                                    :background ,terminal-monokai-mod-red-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,monokai-mod-yellow)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-yellow-hc
                           :background ,monokai-mod-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style line))) terminal-class)
       (:underline (:style line :color ,terminal-monokai-mod-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow-hc
                                    :background ,terminal-monokai-mod-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,class (:foreground ,monokai-mod-red
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,class (:foreground ,monokai-mod-yellow
                           :background unspecified
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :background unspecified
                                    :weight bold))))

   `(edts-face-error-mode-line
     ((,class (:background ,monokai-mod-red
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,class (:background ,monokai-mod-yellow
                           :foreground unspecified))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(elfeed-search-feed-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(elfeed-search-tag-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(elfeed-search-title-face
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))
   `(ein:cell-input-prompt
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))
   `(ein:cell-output-prompt
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))
   `(ein:notification-tab-normal
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))
   `(ein:notification-tab-selected
     ((,class (:foreground ,monokai-mod-orange :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-red)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-red-hc
                           :background ,monokai-mod-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-red-hc
                                    :background ,terminal-monokai-mod-red-lc
                                    :weight bold
                                    :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-orange)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-orange-hc
                           :background ,monokai-mod-orange-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-orange)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange-hc
                                    :background ,terminal-monokai-mod-orange-lc
                                    :weight bold
                                    :underline t))))

   ;; epc
   `(epc:face-title
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-bg
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :underline nil))))

   ;; erc
   `(erc-action-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-bold-face
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(erc-current-nick-face
     ((,class (:foreground ,monokai-mod-blue :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   `(erc-dangerous-host-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(erc-highlight-face
     ((,class (:inherit erc-default-face
                        :background ,monokai-mod-highlight))
      (,terminal-class (:inherit erc-default-face
                                 :background ,terminal-monokai-mod-highlight))))

   `(erc-direct-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-error-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-input-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(erc-keyword-face
     ((,class (:foreground ,monokai-mod-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   `(erc-nick-default-face
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(erc-my-nick-face
     ((,class (:foreground ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold))))

   `(erc-nick-msg-face
     ((,class (:inherit erc-default-face))
      (,terminal-class (:inherit erc-default-face))))

   `(erc-notice-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(erc-pal-face
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight bold))))

   `(erc-prompt-face
     ((,class (:foreground ,monokai-mod-orange
                           :background ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :background ,terminal-monokai-mod-bg
                                    :weight bold))))

   `(erc-timestamp-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,class (:foreground ,monokai-mod-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :inherit bold))))

   `(eshell-ls-archive
     ((,class (:foreground ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :inherit bold))))

   `(eshell-ls-backup
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,class (:foreground ,monokai-mod-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :inherit bold))))

   `(eshell-ls-executable
     ((,class (:foreground ,monokai-mod-green
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :inherit bold))))

   `(eshell-ls-unreadable
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(eshell-ls-missing
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,class (:inherit font-lock-doc-face))
      (,terminal-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,class (:foreground ,monokai-mod-yellow
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :inherit bold))))

   `(eshell-ls-symlink
     ((,class (:foreground ,monokai-mod-cyan
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-red-l
                           :inherit italic))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-red-l
                                    :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-green-l
                           :inherit italic))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line :foreground ,terminal-monokai-mod-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,class (:inherit region))
      (,terminal-class (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-orange
                           :underline t
                           :slant italic))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-orange
                                    :underline t
                                    :slant italic))))

   `(fic-face
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-orange
                                    :weight normal
                                    :slant italic))))

   `(font-lock-fic-face
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-orange
                           :weight normal
                           :slant italic))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-orange
                                    :weight normal
                                    :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,class (:foreground ,monokai-mod-blue
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight normal
                                    :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,monokai-mod-red-hc
                           :background ,monokai-mod-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-red-hc
                                    :background ,terminal-monokai-mod-red-lc
                                    :weight bold
                                    :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,monokai-mod-green-hc
                           :background ,monokai-mod-green-lc))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-green-hc
                                    :background ,terminal-monokai-mod-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,class (:foreground ,monokai-mod-yellow-hc
                           :background ,monokai-mod-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow-hc
                                    :background ,terminal-monokai-mod-yellow-lc
                                    :weight bold
                                    :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-red)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-red-hc
                           :background ,monokai-mod-red-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-red-hc
                                    :background ,terminal-monokai-mod-red-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-yellow)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-yellow-hc
                           :background ,monokai-mod-yellow-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow-hc
                                    :background ,terminal-monokai-mod-yellow-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-blue)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-blue-hc
                           :background ,monokai-mod-blue-lc
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-blue)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue-hc
                                    :background ,terminal-monokai-mod-blue-lc
                                    :weight bold
                                    :underline t))))

   `(flycheck-fringe-error
     ((,class (:foreground ,monokai-mod-red-hc
                           :background ,monokai-mod-red-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red-hc
                                    :background ,terminal-monokai-mod-red-lc
                                    :weight bold))))

   `(flycheck-fringe-warning
     ((,class (:foreground ,monokai-mod-yellow-hc
                           :background ,monokai-mod-yellow-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow-hc
                                    :background ,terminal-monokai-mod-yellow-lc
                                    :weight bold))))

   `(flycheck-fringe-info
     ((,class (:foreground ,monokai-mod-blue-hc
                           :background ,monokai-mod-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue-hc
                                    :background ,terminal-monokai-mod-blue-lc
                                    :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-yellow)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-yellow
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-yellow)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold
                                    :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,monokai-mod-red)
                   :inherit unspecified))
      (,class (:foreground ,monokai-mod-red
                           :weight bold
                           :underline t))
      (,(append '((supports :underline (:style wave))) terminal-class)
       (:underline (:style wave :color ,terminal-monokai-mod-red)
                   :inherit unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold
                                    :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,class (:background ,monokai-mod-green
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-green
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter:deleted
     ((,class (:background ,monokai-mod-red
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter:modified
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter:unchanged
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,class (:foreground ,monokai-mod-green
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :inherit bold))))

   `(git-gutter-fr:deleted
     ((,class (:foreground ,monokai-mod-red
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :inherit bold))))

   `(git-gutter-fr:modified
     ((,class (:foreground ,monokai-mod-blue
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,class (:background ,monokai-mod-green
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-green
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter+-deleted
     ((,class (:background ,monokai-mod-red
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter+-modified
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter+-unchanged
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-bg
                           :inherit bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-bg
                                    :inherit bold))))

   `(git-gutter-fr+-added
     ((,class (:foreground ,monokai-mod-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold))))

   `(git-gutter-fr+-deleted
     ((,class (:foreground ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold))))

   `(git-gutter-fr+-modified
     ((,class (:foreground ,monokai-mod-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-highlight-line
                           :inherit bold))
      (,terminal-class (:foreground ,monokai-mod-blue
                                    :background ,terminal-monokai-mod-highlight-line
                                    :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(guide-key/key-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(guide-key/prefix-command-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,class (:weight bold
                       :inherit gnus-group-mail-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,class (:inherit gnus-group-news-1-empty))
      (,terminal-class (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,class (:weight bold
                       :inherit gnus-group-mail-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,class (:inherit gnus-group-news-2-empty))
      (,terminal-class (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,class (:weight bold
                       :inherit gnus-group-mail-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,class (:inherit gnus-group-news-3-empty))
      (,terminal-class (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,class (:weight bold
                       :inherit gnus-group-mail-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,class (:inherit gnus-group-news-low-empty))
      (,terminal-class (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,class (:weight bold
                       :inherit gnus-group-news-1-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,class (:weight bold
                       :inherit gnus-group-news-2-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,class (:weight bold
                       :inherit gnus-group-news-3-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,class (:weight bold
                       :inherit gnus-group-news-4-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,class (:weight bold
                       :inherit gnus-group-news-5-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,class (:weight bold
                       :inherit gnus-group-news-6-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,class (:weight bold
                       :inherit gnus-group-news-low-empty))
      (,terminal-class (:weight bold
                                :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-from
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-name
     ((,class (:inherit message-header-name))
      (,terminal-class (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,class (:inherit message-header-other))
      (,terminal-class (:inherit message-header-other))))

   `(gnus-header-subject
     ((,class (:inherit message-header-subject))
      (,terminal-class (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(gnus-summary-high-ancient
     ((,class (:foreground ,monokai-mod-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   `(gnus-summary-high-read
     ((,class (:foreground ,monokai-mod-green
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight bold))))

   `(gnus-summary-high-ticked
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight bold))))

   `(gnus-summary-high-unread
     ((,class (:foreground ,monokai-mod-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :weight bold))))

   `(gnus-summary-low-ancient
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-summary-low-read
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-summary-low-ticked
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(gnus-summary-low-unread
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(gnus-summary-normal-ancient
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-summary-normal-read
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-summary-normal-ticked
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(gnus-summary-normal-unread
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(gnus-summary-selected
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(gnus-cite-1
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-cite-2
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-cite-3
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-cite-4
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-cite-5
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-cite-6
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-cite-7
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(gnus-cite-8
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(gnus-cite-9
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(gnus-cite-10
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(gnus-cite-11
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(gnus-group-news-1-empty
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(gnus-group-news-2-empty
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-group-news-3-empty
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(gnus-group-news-4-empty
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-group-news-5-empty
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(gnus-group-news-6-empty
     ((,class (:foreground ,monokai-mod-blue-lc))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue-lc))))

   `(gnus-group-news-low-empty
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(gnus-signature
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(gnus-x-face
     ((,class (:background ,monokai-mod-fg
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-fg
                                    :foreground ,terminal-monokai-mod-bg))))


   ;; helm
   `(helm-apt-deinstalled
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(helm-apt-installed
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(helm-bookmark-directory
     ((,class (:inherit helm-ff-directory))
      (,terminal-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(helm-bookmark-gnus
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(helm-bookmark-info
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(helm-bookmark-man
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(helm-bookmark-w3m
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(helm-bookmarks-su
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(helm-buffer-file
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(helm-buffer-directory
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(helm-buffer-process
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(helm-buffer-saved-out
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg
                                    :inverse-video t))))

   `(helm-buffer-size
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(helm-candidate-number
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph
                           :bold t))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph
                                    :bold t))))

   `(helm-ff-directory
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(helm-ff-executable
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(helm-ff-file
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-fg))))

   `(helm-ff-invalid-symlink
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-orange
                           :slant italic))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-orange
                                    :slant italic))))

   `(helm-ff-prefix
     ((,class (:background ,monokai-mod-green
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-green
                                    :foreground ,terminal-monokai-mod-bg))))

   `(helm-ff-symlink
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(helm-grep-file
     ((,class (:foreground ,monokai-mod-cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :underline t))))

   `(helm-grep-finish
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(helm-grep-lineno
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(helm-grep-match
     ((,class (:inherit helm-match)))
     ((,terminal-class (:inherit helm-match))))

   `(helm-grep-running
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(helm-header
     ((,class (:inherit header-line))
      (,terminal-class (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(helm-lisp-show-completion
     ((,class (:foreground ,monokai-mod-yellow
                           :background ,monokai-mod-highlight-line
                           :bold t))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :background ,terminal-monokai-mod-highlight-line
                                    :bold t))))

   `(helm-M-x-key
     ((,class (:foreground ,monokai-mod-orange
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :underline t))))

   `(helm-moccur-buffer
     ((,class (:foreground ,monokai-mod-cyan
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :underline t))))

   `(helm-match
     ((,class (:foreground ,monokai-mod-green :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green :inherit bold))))

   `(helm-match-item
     ((,class (:inherit helm-match))
      (,terminal-class (:inherit helm-match))))

   `(helm-selection
     ((,class (:background ,monokai-mod-highlight-line
                           :inherit bold
                           :underline nil))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :inherit bold
                                    :underline nil))))

   `(helm-selection-line
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph
                           :underline nil))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph
                                    :underline nil))))

   `(helm-separator
     ((,class (:foreground ,monokai-mod-gray))
      (,terminal-class (:foreground ,terminal-monokai-mod-gray))))

   `(helm-source-header
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-orange-l
                           :underline t))
      (,terminal-class (:background ,terminal-monokai-mod-violet-l
                                    :foreground ,terminal-monokai-mod-bg
                                    :underline nil))))

   `(helm-swoop-target-line-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(helm-swoop-target-word-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(helm-time-zone-current
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(helm-time-zone-home
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(helm-visible-mark
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-magenta :bold t))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,class :foreground ,monokai-mod-blue)
      (,terminal-class :foreground ,terminal-monokai-mod-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,class :foreground ,monokai-mod-blue-l)
      (,terminal-class :foreground ,terminal-monokai-mod-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,class :foreground ,monokai-mod-blue-l)
      (,terminal-class :foreground ,terminal-monokai-mod-blue-l)))

   `(helm-ls-git-untracked-face
     ((,class :foreground ,monokai-mod-orange)
      (,terminal-class :foreground ,terminal-monokai-mod-orange)))

   `(helm-ls-git-added-copied-face
     ((,class :foreground ,monokai-mod-green)
      (,terminal-class :foreground ,terminal-monokai-mod-green)))

   `(helm-ls-git-added-modified-face
     ((,class :foreground ,monokai-mod-green-l)
      (,terminal-class :foreground ,terminal-monokai-mod-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,class :foreground ,monokai-mod-red)
      (,terminal-class :foreground ,terminal-monokai-mod-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,class :foreground ,monokai-mod-red-l)
      (,terminal-class :foreground ,terminal-monokai-mod-red-l)))

   `(helm-ls-git-conflict-face
     ((,class :foreground ,monokai-mod-yellow)
      (,terminal-class :foreground ,terminal-monokai-mod-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,class (:foreground ,monokai-mod-yellow-lc
                           :background ,monokai-mod-yellow-hc))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow-lc
                                    :background ,terminal-monokai-mod-yellow-hc))))

   `(hi-pink
     ((,class (:foreground ,monokai-mod-magenta-lc
                           :background ,monokai-mod-magenta-hc))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta-lc
                                    :background ,terminal-monokai-mod-magenta-hc))))

   `(hi-green
     ((,class (:foreground ,monokai-mod-green-lc
                           :background ,monokai-mod-green-hc))
      (,terminal-class (:foreground ,terminal-monokai-mod-green-lc
                                    :background ,terminal-monokai-mod-green-hc))))

   `(hi-blue
     ((,class (:foreground ,monokai-mod-blue-lc
                           :background ,monokai-mod-blue-hc))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue-lc
                                    :background ,terminal-monokai-mod-blue-hc))))

   `(hi-black-b
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg
                                    :weight bold))))

   `(hi-blue-b
     ((,class (:foreground ,monokai-mod-blue-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue-lc
                                    :weight bold))))

   `(hi-green-b
     ((,class (:foreground ,monokai-mod-green-lc
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-green-lc
                                    :weight bold))))

   `(hi-red-b
     ((,class (:foreground ,monokai-mod-red
                           :weight bold))))

   `(hi-black-hb
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg
                                    :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(highlight-changes-delete
     ((,class (:foreground ,monokai-mod-red
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,class (:background ,monokai-mod-gray))
      (,terminal-class (:background ,terminal-monokai-mod-gray))))

   `(highlight-indentation-current-column-face
     ((,class (:background ,monokai-mod-gray))
      (,terminal-class (:background ,terminal-monokai-mod-gray))))

   ;; hl-line-mode
   `(hl-line
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(hl-line-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal))))

   `(ido-only-match
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-yellow
                                    :weight normal))))

   `(ido-subdir
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(ido-incomplete-regexp
     ((,class (:foreground ,monokai-mod-red
                           :weight bold ))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold ))))

   `(ido-indicator
     ((,class (:background ,monokai-mod-red
                           :foreground ,monokai-mod-bg
                           :width condensed))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground ,terminal-monokai-mod-bg
                                    :width condensed))))

   `(ido-virtual
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   ;; info
   `(info-header-xref
     ((,class (:foreground ,monokai-mod-green
                           :inherit bold
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :inherit bold
                                    :underline t))))

   `(info-menu
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(info-node
     ((,class (:foreground ,monokai-mod-violet
                           :inherit bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :inherit bold))))

   `(info-quoted-name
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(info-reference-item
     ((,class (:background nil
                           :underline t
                           :inherit bold))
      (,terminal-class (:background nil
                                    :underline t
                                    :inherit bold))))

   `(info-string
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(info-title-1
     ((,class (:height ,monokai-mod-height-plus-4))
      (,terminal-class (:height ,monokai-mod-height-plus-4))))

   `(info-title-2
     ((,class (:height ,monokai-mod-height-plus-3))
      (,terminal-class (:height ,monokai-mod-height-plus-3))))

   `(info-title-3
     ((,class (:height ,monokai-mod-height-plus-2))
      (,terminal-class (:height ,monokai-mod-height-plus-2))))

   `(info-title-4
     ((,class (:height ,monokai-mod-height-plus-1))
      (,terminal-class (:height ,monokai-mod-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,class (:background ,monokai-mod-gray :inherit bold))
      (,terminal-class (:background ,monokai-mod-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,class (:inherit bold))
      (,terminal-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,class (:foreground ,monokai-mod-violet
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,class (:foreground ,monokai-mod-green
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,class (:foreground ,monokai-mod-yellow
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :underline t))))

   `(ivy-remote
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(swiper-line-face
     ((,class (:background ,monokai-mod-highlight-line))))

   `(swiper-match-face-1
     ((,class (:background ,monokai-mod-gray-d))))

   `(swiper-match-face-2
     ((,class (:background ,monokai-mod-green))))

   `(swiper-match-face-3
     ((,class (:background ,monokai-mod-orange))))

   `(swiper-match-face-4
     ((,class (:background ,monokai-mod-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,class (:weight bold
                       :foreground ,monokai-mod-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-red))))

   `(jabber-activity-personal-face
     ((,class (:weight bold
                       :foreground ,monokai-mod-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-blue))))

   `(jabber-chat-error
     ((,class (:weight bold
                       :foreground ,monokai-mod-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-red))))

   `(jabber-chat-prompt-foreign
     ((,class (:weight bold
                       :foreground ,monokai-mod-red))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-red))))

   `(jabber-chat-prompt-local
     ((,class (:weight bold
                       :foreground ,monokai-mod-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-blue))))

   `(jabber-chat-prompt-system
     ((,class (:weight bold
                       :foreground ,monokai-mod-green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-green))))

   `(jabber-chat-text-foreign
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(jabber-chat-text-local
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(jabber-chat-rare-time-face
     ((,class (:underline t
                          :foreground ,monokai-mod-green))
      (,terminal-class (:underline t
                                   :foreground ,terminal-monokai-mod-green))))

   `(jabber-roster-user-away
     ((,class (:slant italic
                      :foreground ,monokai-mod-green))
      (,terminal-class (:slant italic
                               :foreground ,terminal-monokai-mod-green))))

   `(jabber-roster-user-chatty
     ((,class (:weight bold
                       :foreground ,monokai-mod-orange))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-orange))))

   `(jabber-roster-user-dnd
     ((,class (:slant italic
                      :foreground ,monokai-mod-red))
      (,terminal-class (:slant italic
                               :foreground ,terminal-monokai-mod-red))))

   `(jabber-roster-user-error
     ((,class (:weight light
                       :slant italic
                       :foreground ,monokai-mod-red))
      (,terminal-class (:weight light
                                :slant italic
                                :foreground ,terminal-monokai-mod-red))))

   `(jabber-roster-user-offline
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(jabber-roster-user-online
     ((,class (:weight bold
                       :foreground ,monokai-mod-blue))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-blue))))

   `(jabber-roster-user-xa
     ((,class (:slant italic
                      :foreground ,monokai-mod-magenta))
      (,terminal-class (:slant italic
                               :foreground ,terminal-monokai-mod-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(js2-external-variable
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(js2-function-param
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(js2-instance-member
     ((,class (:foreground ,monokai-mod-magenta))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta))))

   `(js2-jsdoc-html-tag-delimiter
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(js2-jsdoc-html-tag-name
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(js2-object-property
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(js2-function-call
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(js2-jsdoc-tag
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(js2-jsdoc-type
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(js2-jsdoc-value
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(js2-magic-paren
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(js2-private-function-call
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(js2-private-member
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(js2-warning
     ((,class (:underline ,monokai-mod-orange))
      (,terminal-class (:underline ,terminal-monokai-mod-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,class (:inherit bold))
      (,terminal-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-fringe-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-fringe-bg))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,class (:inherit dimonokai-mod-red-directory))
      (,terminal-class (:inherit dimonokai-mod-red-directory))))

   `(lusty-file-face
     ((,class nil)
      (,terminal-class nil)))

   `(lusty-match-face
     ((,class (:inherit ido-first-match))
      (,terminal-class (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,class (:foreground ,monokai-mod-cyan
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-bg))))

   `(magit-diff-added-highlight
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-highlight-line))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-highlight-line))))

   `(magit-diff-removed
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg))))

   `(magit-diff-removed-highlight
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-highlight-line))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-highlight-line))))

   `(magit-section-title
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(magit-branch
     ((,class (:foreground ,monokai-mod-orange
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight bold))))

   `(magit-item-highlight
     ((,class (:background ,monokai-mod-highlight-line
                           :weight unspecified))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :weight unspecified))))

   `(magit-log-author
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(magit-log-graph
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(magit-log-head-label-bisect-bad
     ((,class (:background ,monokai-mod-red-hc
                           :foreground ,monokai-mod-red-lc
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-red-hc
                                    :foreground ,terminal-monokai-mod-red-lc
                                    :box 1))))

   `(magit-log-head-label-bisect-good
     ((,class (:background ,monokai-mod-green-hc
                           :foreground ,monokai-mod-green-lc
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-green-hc
                                    :foreground ,terminal-monokai-mod-green-lc
                                    :box 1))))

   `(magit-log-head-label-default
     ((,class (:background ,monokai-mod-highlight-line
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :box 1))))

   `(magit-log-head-label-local
     ((,class (:background ,monokai-mod-blue-lc
                           :foreground ,monokai-mod-blue-hc
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-blue-lc
                                    :foreground ,terminal-monokai-mod-blue-hc
                                    :box 1))))

   `(magit-log-head-label-patches
     ((,class (:background ,monokai-mod-red-lc
                           :foreground ,monokai-mod-red-hc
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-red-lc
                                    :foreground ,terminal-monokai-mod-red-hc
                                    :box 1))))

   `(magit-log-head-label-remote
     ((,class (:background ,monokai-mod-green-lc
                           :foreground ,monokai-mod-green-hc
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc
                                    :foreground ,terminal-monokai-mod-green-hc
                                    :box 1))))

   `(magit-log-head-label-tags
     ((,class (:background ,monokai-mod-yellow-lc
                           :foreground ,monokai-mod-yellow-hc
                           :box 1))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc
                                    :foreground ,terminal-monokai-mod-yellow-hc
                                    :box 1))))

   `(magit-log-sha1
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   ;; man
   `(Man-overstrike
     ((,class (:foreground ,monokai-mod-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   `(Man-reverse
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(Man-underline
     ((,class (:foreground ,monokai-mod-green :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(monky-diff-add
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(monky-diff-del
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(markdown-header-face-1
     ((,class (:inherit markdown-header-face
                        :height ,monokai-mod-height-plus-4))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,monokai-mod-height-plus-4))))

   `(markdown-header-face-2
     ((,class (:inherit markdown-header-face
                        :height ,monokai-mod-height-plus-3))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,monokai-mod-height-plus-3))))

   `(markdown-header-face-3
     ((,class (:inherit markdown-header-face
                        :height ,monokai-mod-height-plus-2))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,monokai-mod-height-plus-2))))

   `(markdown-header-face-4
     ((,class (:inherit markdown-header-face
                        :height ,monokai-mod-height-plus-1))
      (,terminal-class (:inherit markdown-header-face
                                 :height ,monokai-mod-height-plus-1))))

   `(markdown-header-face-5
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,class (:inherit markdown-header-face))
      (,terminal-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(message-header-name
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(message-header-other
     ((,class (:foreground ,monokai-mod-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :weight normal))))

   `(message-header-to
     ((,class (:foreground ,monokai-mod-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :weight normal))))

   `(message-header-cc
     ((,class (:foreground ,monokai-mod-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :weight normal))))

   `(message-header-newsgroups
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(message-header-subject
     ((,class (:foreground ,monokai-mod-cyan
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :weight normal))))

   `(message-header-xheader
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(message-mml
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(message-separator
     ((,class (:foreground ,monokai-mod-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(mew-face-header-from
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(mew-face-header-date
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-header-to
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(mew-face-header-key
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-header-private
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-header-important
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(mew-face-header-marginal
     ((,class (:foreground ,monokai-mod-fg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :weight bold))))

   `(mew-face-header-warning
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(mew-face-header-xmew
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-header-xmew-bad
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(mew-face-body-url
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(mew-face-body-comment
     ((,class (:foreground ,monokai-mod-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :slant italic))))

   `(mew-face-body-cite1
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-body-cite2
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(mew-face-body-cite3
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(mew-face-body-cite4
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(mew-face-body-cite5
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(mew-face-mark-review
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(mew-face-mark-escape
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-mark-delete
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(mew-face-mark-unlink
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(mew-face-mark-refile
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-mark-unread
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(mew-face-eof-message
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(mew-face-eof-part
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(mingus-pausing-face
     ((,class (:foreground ,monokai-mod-magenta))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta))))

   `(mingus-playing-face
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(mingus-playlist-face
     ((,class (:foreground ,monokai-mod-cyan ))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan ))))

   `(mingus-song-file-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(mingus-stopped-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,class (:background ,monokai-mod-violet-d))
      (,terminal-class (:background ,terminal-monokai-mod-violet-d))))

   `(mmm-cleanup-submode-face
     ((,class (:background ,monokai-mod-orange-d))
      (,terminal-class (:background ,terminal-monokai-mod-orange-d))))

   `(mmm-declaration-submode-face
     ((,class (:background ,monokai-mod-cyan-d))
      (,terminal-class (:background ,terminal-monokai-mod-cyan-d))))

   `(mmm-comment-submode-face
     ((,class (:background ,monokai-mod-blue-d))
      (,terminal-class (:background ,terminal-monokai-mod-blue-d))))

   `(mmm-output-submode-face
     ((,class (:background ,monokai-mod-red-d))
      (,terminal-class (:background ,terminal-monokai-mod-red-d))))

   `(mmm-special-submode-face
     ((,class (:background ,monokai-mod-green-d))
      (,terminal-class (:background ,terminal-monokai-mod-green-d))))

   `(mmm-code-submode-face
     ((,class (:background ,monokai-mod-gray))
      (,terminal-class (:background ,terminal-monokai-mod-gray))))

   `(mmm-default-submode-face
     ((,class (:background ,monokai-mod-gray-d))
      (,terminal-class (:background ,terminal-monokai-mod-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(moccur-edit-done-face
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-bg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-bg
                                    :slant italic))))

   `(moccur-edit-face
     ((,class (:background ,monokai-mod-yellow
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground ,terminal-monokai-mod-bg))))

   `(moccur-edit-file-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(moccur-edit-reject-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(moccur-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph
                                    :weight bold))))

   `(search-buffers-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph
                                    :weight bold))))

   `(search-buffers-header-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,class (:foreground ,monokai-mod-green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-2-face
     ((,class (:foreground ,monokai-mod-blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-3-face
     ((,class (:foreground ,monokai-mod-orange
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-4-face
     ((,class (:foreground ,monokai-mod-yellow
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-5-face
     ((,class (:foreground ,monokai-mod-cyan
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-6-face
     ((,class (:foreground ,monokai-mod-green
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :slant italic
                                    :weight normal))))

   `(mu4e-cited-7-face
     ((,class (:foreground ,monokai-mod-blue
                           :slant italic
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :slant italic
                                    :weight normal))))

   `(mu4e-flagged-face
     ((,class (:foreground ,monokai-mod-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta
                                    :weight bold))))

   `(mu4e-view-url-number-face
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal))))

   `(mu4e-warning-face
     ((,class (:foreground ,monokai-mod-red
                           :slant normal
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :slant normal
                                    :weight bold))))

   `(mu4e-header-highlight-face
     ((,class (:inherit unspecified
                        :foreground unspecified
                        :background ,monokai-mod-highlight-line
                        :underline ,monokai-mod-emph
                        :weight normal))
      (,terminal-class (:inherit unspecified
                                 :foreground unspecified
                                 :background ,terminal-monokai-mod-highlight-line
                                 :underline ,terminal-monokai-mod-emph
                                 :weight normal))))


   `(mu4e-draft-face
     ((,class (:inherit font-lock-string-face))
      (,terminal-class (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,class (:inherit font-lock-comment-face))
      (,terminal-class (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,class (:inherit font-lock-builtin-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-builtin-face
                                 :weight normal))))

   `(mu4e-header-face
     ((,class (:inherit default))
      (,terminal-class (:inherit default))))

   `(mu4e-header-marks-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,class (:inherit font-lock-type-face))
      (,terminal-class (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,class (:inherit font-lock-pseudo-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-pseudo-keyword-face
                                 :weight bold))))

   `(mu4e-moved-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-ok-face
     ((,class (:inherit font-lock-comment-face
                        :slant normal
                        :weight bold))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant normal
                                 :weight bold))))

   `(mu4e-replied-face
     ((,class (:inherit font-lock-builtin-face
                        :weight normal))
      (,terminal-class (:inherit font-lock-builtin-face
                                 :weight normal))))

   `(mu4e-system-face
     ((,class (:inherit font-lock-comment-face
                        :slant italic))
      (,terminal-class (:inherit font-lock-comment-face
                                 :slant italic))))

   `(mu4e-title-face
     ((,class (:inherit font-lock-type-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-type-face
                                 :weight bold))))

   `(mu4e-trashed-face
     ((,class (:inherit font-lock-comment-face
                        :strike-through t))
      (,terminal-class (:inherit font-lock-comment-face
                                 :strike-through t))))

   `(mu4e-unread-face
     ((,class (:inherit font-lock-keyword-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-keyword-face
                                 :weight bold))))

   `(mu4e-view-attach-number-face
     ((,class (:inherit font-lock-variable-name-face
                        :weight bold))
      (,terminal-class (:inherit font-lock-variable-name-face
                                 :weight bold))))

   `(mu4e-view-contact-face
     ((,class (:foreground ,monokai-mod-fg
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :weight normal))))

   `(mu4e-view-header-key-face
     ((,class (:inherit message-header-name
                        :weight normal))
      (,terminal-class (:inherit message-header-name
                                 :weight normal))))

   `(mu4e-view-header-value-face
     ((,class (:foreground ,monokai-mod-cyan
                           :weight normal
                           :slant normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :weight normal
                                    :slant normal))))

   `(mu4e-view-link-face
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,class (:foreground ,monokai-mod-blue
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight normal
                                    :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(nav-face-button-num
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(nav-face-dir
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(nav-face-hdir
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(nav-face-file
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(nav-face-hfile
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-bg
                                    :weight bold))))


   `(neo-header-face
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg))))

   `(neo-root-dir-face
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-bg))))

   `(neo-dir-link-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-bg))))

   `(neo-file-link-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(neo-button-face
     ((,class (:underline nil))
      (,terminal-class (:underline nil))))

   `(neo-expand-btn-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(neo-vc-default-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(neo-vc-user-face
     ((,class (:foreground ,monokai-mod-red
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :slant italic))))

   `(neo-vc-up-to-date-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(neo-vc-edited-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(neo-vc-needs-update-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(neo-vc-unlocked-changes-face
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-comments))))

   `(neo-vc-added-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(neo-vc-removed-face
     ((,class (:strike-through t))
      (,terminal-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(neo-vc-missing-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(neo-vc-ignored-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))


   ;; org-mode
   `(org-agenda-structure
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-highlight-line
                           :weight bold
                           :slant normal
                           :inverse-video nil
                           :height ,monokai-mod-height-plus-1
                           :underline nil
                           :box (:line-width 2 :color ,monokai-mod-bg)))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-highlight-line
                                    :weight bold
                                    :slant normal
                                    :inverse-video nil
                                    :height ,monokai-mod-height-plus-1
                                    :underline nil
                                    :box (:line-width 2 :color ,terminal-monokai-mod-bg)))))

   `(org-agenda-calendar-event
     ((,class (:foreground ,monokai-mod-emph))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph))))

   `(org-agenda-calendar-sexp
     ((,class (:foreground ,monokai-mod-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :slant italic))))

   `(org-agenda-date
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video nil
                           :overline nil
                           :slant normal
                           :height 1.0
                           :box (:line-width 2 :color ,monokai-mod-bg)))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video nil
                                    :overline nil
                                    :slant normal
                                    :height 1.0
                                    :box (:line-width 2 :color ,terminal-monokai-mod-bg)))) t)

   `(org-agenda-date-weekend
     ((,class (:inherit org-agenda-date
                        :inverse-video nil
                        :background unspecified
                        :foreground ,monokai-mod-comments
                        :weight unspecified
                        :underline t
                        :overline nil
                        :box unspecified))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video nil
                                 :background unspecified
                                 :foreground ,terminal-monokai-mod-comments
                                 :weight unspecified
                                 :underline t
                                 :overline nil
                                 :box unspecified))) t)

   `(org-agenda-date-today
     ((,class (:inherit org-agenda-date
                        :inverse-video t
                        :weight bold
                        :underline unspecified
                        :overline nil
                        :box unspecified
                        :foreground ,monokai-mod-blue
                        :background ,monokai-mod-bg))
      (,terminal-class (:inherit org-agenda-date
                                 :inverse-video t
                                 :weight bold
                                 :underline unspecified
                                 :overline nil
                                 :box unspecified
                                 :foreground ,terminal-monokai-mod-blue
                                 :background ,terminal-monokai-mod-bg))) t)

   `(org-agenda-done
     ((,class (:foreground ,monokai-mod-comments
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :slant italic))) t)

   `(org-archived
     ((,class (:foreground ,monokai-mod-comments
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :weight normal))))

   `(org-block
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-gray))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-gray))))

   `(org-block-background
     ((,class (:background ,monokai-mod-gray))
      (,terminal-class (:background ,terminal-monokai-mod-gray))))

   `(org-block-begin-line
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-gray-d
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-gray-d
                                    :slant italic))))

   `(org-block-end-line
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-gray-d
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-gray-d
                                    :slant italic))))

   `(org-checkbox
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-fg
                           :box (:line-width 1 :style released-button)))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-fg
                                    :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(org-date
     ((,class (:foreground ,monokai-mod-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :underline t))))

   `(org-done
     ((,class (:weight bold
                       :foreground ,monokai-mod-green))
      (,terminal-class (:weight bold
                                :foreground ,terminal-monokai-mod-green))))

   `(org-ellipsis
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(org-formula
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(org-headline-done
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(org-hide
     ((,class (:foreground ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg))))

   `(org-level-1
     ((,class (:inherit ,monokai-mod-pitch
                        :height ,monokai-mod-height-plus-4
                        :foreground ,monokai-mod-orange))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :height ,monokai-mod-height-plus-4
                                 :foreground ,terminal-monokai-mod-orange))))

   `(org-level-2
     ((,class (:inherit ,monokai-mod-pitch
                        :height ,monokai-mod-height-plus-3
                        :foreground ,monokai-mod-green))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :height ,monokai-mod-height-plus-3
                                 :foreground ,terminal-monokai-mod-green))))

   `(org-level-3
     ((,class (:inherit ,monokai-mod-pitch
                        :height ,monokai-mod-height-plus-2
                        :foreground ,monokai-mod-blue))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :height ,monokai-mod-height-plus-2
                                 :foreground ,terminal-monokai-mod-blue))))

   `(org-level-4
     ((,class (:inherit ,monokai-mod-pitch
                        :height ,monokai-mod-height-plus-1
                        :foreground ,monokai-mod-yellow))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :height ,monokai-mod-height-plus-1
                                 :foreground ,terminal-monokai-mod-yellow))))

   `(org-level-5
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-cyan))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-cyan))))

   `(org-level-6
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-green))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-green))))

   `(org-level-7
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-red))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-red))))

   `(org-level-8
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-blue))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-blue))))

   `(org-link
     ((,class (:foreground ,monokai-mod-blue
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :underline t))))

   `(org-sexp-date
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(org-scheduled
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(org-scheduled-previously
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(org-scheduled-today
     ((,class (:foreground ,monokai-mod-blue
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight normal))))

   `(org-special-keyword
     ((,class (:foreground ,monokai-mod-comments
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :weight bold))))

   `(org-table
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(org-tag
     ((,class (:weight bold))
      (,terminal-class (:weight bold))))

   `(org-time-grid
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(org-todo
     ((,class (:foreground ,monokai-mod-red
                           :weight bold)))
     ((,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :weight bold))))

   `(org-upcoming-deadline
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal
                                    :underline nil))))

   `(org-warning
     ((,class (:foreground ,monokai-mod-orange
                           :weight normal
                           :underline nil))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange
                                    :weight normal
                                    :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,class (:background ,monokai-mod-blue-lc
                           :foreground ,monokai-mod-blue-hc))
      (,terminal-class (:background ,terminal-monokai-mod-blue-lc
                                    :foreground ,terminal-monokai-mod-blue-hc))))

   `(org-habit-clear-future-face
     ((,class (:background ,monokai-mod-blue-lc))
      (,terminal-class (:background ,terminal-monokai-mod-blue-lc))))

   `(org-habit-ready-face
     ((,class (:background ,monokai-mod-green-lc
                           :foreground ,monokai-mod-green))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc
                                    :foreground ,terminal-monokai-mod-green))))

   `(org-habit-ready-future-face
     ((,class (:background ,monokai-mod-green-lc))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc))))

   `(org-habit-alert-face
     ((,class (:background ,monokai-mod-yellow
                           :foreground ,monokai-mod-yellow-lc))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground ,terminal-monokai-mod-yellow-lc))))

   `(org-habit-alert-future-face
     ((,class (:background ,monokai-mod-yellow-lc))
      (,terminal-class (:background ,terminal-monokai-mod-yellow-lc))))

   `(org-habit-overdue-face
     ((,class (:background ,monokai-mod-red
                           :foreground ,monokai-mod-red-lc))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground ,terminal-monokai-mod-red-lc))))

   `(org-habit-overdue-future-face
     ((,class (:background ,monokai-mod-red-lc))
      (,terminal-class (:background ,terminal-monokai-mod-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(org-agenda-restriction-lock
     ((,class (:background ,monokai-mod-yellow))
      (,terminal-class (:background ,terminal-monokai-mod-yellow))))

   `(org-clock-overlay
     ((,class (:background ,monokai-mod-yellow))
      (,terminal-class (:background ,terminal-monokai-mod-yellow))))

   `(org-column
     ((,class (:background ,monokai-mod-highlight-line
                           :strike-through nil
                           :underline nil
                           :slant normal
                           :weight normal
                           :inherit default))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :strike-through nil
                                    :underline nil
                                    :slant normal
                                    :weight normal
                                    :inherit default))))

   `(org-column-title
     ((,class (:background ,monokai-mod-highlight-line
                           :underline t
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :underline t
                                    :weight bold))))

   `(org-date-selected
     ((,class (:foreground ,monokai-mod-red
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :inverse-video t))))

   `(org-document-info
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(org-document-title
     ((,class (:foreground ,monokai-mod-emph
                           :weight bold
                           :height ,monokai-mod-height-plus-4))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :weight bold
                                    :height ,monokai-mod-height-plus-4))))

   `(org-drawer
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(org-footnote
     ((,class (:foreground ,monokai-mod-magenta
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta
                                    :underline t))))

   `(org-latex-and-export-specials
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(org-mode-line-clock-overrun
     ((,class (:inherit mode-line))
      (,terminal-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,class (:inherit org-level-1))
      (,terminal-class (:inherit org-level-1))))

   `(outline-2
     ((,class (:inherit org-level-2))
      (,terminal-class (:inherit org-level-2))))

   `(outline-3
     ((,class (:inherit org-level-3))
      (,terminal-class (:inherit org-level-3))))

   `(outline-4
     ((,class (:inherit org-level-4))
      (,terminal-class (:inherit org-level-4))))

   `(outline-5
     ((,class (:inherit org-level-5))
      (,terminal-class (:inherit org-level-5))))

   `(outline-6
     ((,class (:inherit org-level-6))
      (,terminal-class (:inherit org-level-6))))

   `(outline-7
     ((,class (:inherit org-level-7))
      (,terminal-class (:inherit org-level-7))))

   `(outline-8
     ((,class (:inherit org-level-8))
      (,terminal-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,terminal-class (:foreground ,monokai-mod-comments))))

   ;; perspective
   `(persp-selected-face
     ((,class (:foreground ,monokai-mod-blue
                           :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal))))

   ;; popup
   `(popup-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg))))

   `(popup-isearch-match
     ((,class (:background ,monokai-mod-green))
      (,terminal-class (:background ,terminal-monokai-mod-green))))

   `(popup-menu-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg))))

   `(popup-menu-mouse-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-fg))))

   `(popup-menu-selection-face
     ((,class (:background ,monokai-mod-magenta
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-magenta
                                    :foreground ,terminal-monokai-mod-bg))))

   `(popup-scroll-bar-background-face
     ((,class (:background ,monokai-mod-comments))
      (,terminal-class (:background ,terminal-monokai-mod-comments))))

   `(popup-scroll-bar-foreground-face
     ((,class (:background ,monokai-mod-emph))
      (,terminal-class (:background ,terminal-monokai-mod-emph))))

   `(popup-tip-face
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(rainbow-delimiters-depth-4-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(rainbow-delimiters-depth-7-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(rainbow-delimiters-depth-10-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,monokai-mod-fg
                           :background ,monokai-mod-bg
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :background ,terminal-monokai-mod-bg
                                    :inverse-video t))))

   ;; rhtm-mode
   `(erb-face
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-delim-face
     ((,class (:foreground ,monokai-mod-cyan
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-exec-face
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-exec-delim-face
     ((,class (:foreground ,monokai-mod-cyan
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-out-face
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-out-delim-face
     ((,class (:foreground ,monokai-mod-cyan
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-comment-face
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg))))

   `(erb-comment-delim-face
     ((,class (:foreground ,monokai-mod-cyan
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :background ,terminal-monokai-mod-bg))))

   ;; rst-mode
   `(rst-level-1-face
     ((,class (:background ,monokai-mod-yellow
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground ,terminal-monokai-mod-bg))))

   `(rst-level-2-face
     ((,class (:background ,monokai-mod-cyan
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-cyan
                                    :foreground ,terminal-monokai-mod-bg))))

   `(rst-level-3-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg))))

   `(rst-level-4-face
     ((,class (:background ,monokai-mod-violet
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-violet
                                    :foreground ,terminal-monokai-mod-bg))))

   `(rst-level-5-face
     ((,class (:background ,monokai-mod-magenta
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-magenta
                                    :foreground ,terminal-monokai-mod-bg))))

   `(rst-level-6-face
     ((,class (:background ,monokai-mod-red
                           :foreground ,monokai-mod-bg))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground ,terminal-monokai-mod-bg))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(rpm-spec-doc-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(rpm-spec-ghost-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(rpm-spec-macro-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(rpm-spec-package-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(rpm-spec-section-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(rpm-spec-tag-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(rpm-spec-var-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,class (:foreground ,monokai-mod-violet
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :weight bold))))

   `(sh-escaped-newline
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   `(sh-heredoc
     ((,class (:foreground ,monokai-mod-yellow
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(sp-wrap-overlay-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(sp-show-pair-enclosing
     ((,class (:inherit highlight))
      (,terminal-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   `(show-paren-mismatch
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-mismatch
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   `(paren-face-no-match
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-bg
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-bg
                                    :weight normal
                                    :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-comments))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-comments))))

   `(speedbar-directory-face
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-blue))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-blue))))

   `(speedbar-file-face
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-fg))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-fg))))

   `(speedbar-highlight-face
     ((,class (:inherit ,monokai-mod-pitch
                        :background ,monokai-mod-highlight-line))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :background ,terminal-monokai-mod-highlight-line))))

   `(speedbar-selected-face
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-yellow
                        :underline t))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-yellow
                                 :underline t))))

   `(speedbar-separator-face
     ((,class (:inherit ,monokai-mod-pitch
                        :background ,monokai-mod-blue
                        :foreground ,monokai-mod-bg
                        :overline ,monokai-mod-cyan-lc))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :background ,terminal-monokai-mod-blue
                                 :foreground ,terminal-monokai-mod-bg
                                 :overline ,terminal-monokai-mod-cyan-lc))))

   `(speedbar-tag-face
     ((,class (:inherit ,monokai-mod-pitch
                        :foreground ,monokai-mod-green))
      (,terminal-class (:inherit ,monokai-mod-pitch
                                 :foreground ,terminal-monokai-mod-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,class (:background ,monokai-mod-blue
                           :foreground ,monokai-mod-bg
                           :height ,monokai-mod-height-plus-1
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-blue
                                    :foreground ,terminal-monokai-mod-bg
                                    :height ,monokai-mod-height-plus-1
                                    :weight bold))))

   `(sr-editing-path-face
     ((,class (:background ,monokai-mod-yellow
                           :foreground ,monokai-mod-bg
                           :weight bold
                           :height ,monokai-mod-height-plus-1))
      (,terminal-class (:background ,terminal-monokai-mod-yellow
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold
                                    :height ,monokai-mod-height-plus-1))))

   `(sr-highlight-path-face
     ((,class (:background ,monokai-mod-green
                           :foreground ,monokai-mod-bg
                           :weight bold
                           :height ,monokai-mod-height-plus-1))
      (,terminal-class (:background ,terminal-monokai-mod-green
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold
                                    :height ,monokai-mod-height-plus-1))))

   `(sr-passive-path-face
     ((,class (:background ,monokai-mod-comments
                           :foreground ,monokai-mod-bg
                           :weight bold
                           :height ,monokai-mod-height-plus-1))
      (,terminal-class (:background ,terminal-monokai-mod-comments
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold
                                    :height ,monokai-mod-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,class (:inherit dimonokai-mod-red-marked))
      (,terminal-class (:inherit dimonokai-mod-red-marked))))

   `(sr-marked-file-face
     ((,class (:inherit dimonokai-mod-red-marked))
      (,terminal-class (:inherit dimonokai-mod-red-marked))))

   `(sr-alt-marked-dir-face
     ((,class (:background ,monokai-mod-magenta
                           :foreground ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-magenta
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold))))

   `(sr-alt-marked-file-face
     ((,class (:background ,monokai-mod-magenta
                           :foreground ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-magenta
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,class (:inherit dimonokai-mod-red-directory
                        :weight normal))
      (,terminal-class (:inherit dimonokai-mod-red-directory
                                 :weight normal))))

   `(sr-symlink-directory-face
     ((,class (:inherit dimonokai-mod-red-directory
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dimonokai-mod-red-directory
                                 :slant italic
                                 :weight normal))))

   `(sr-symlink-face
     ((,class (:inherit dimonokai-mod-red-symlink
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dimonokai-mod-red-symlink
                                 :slant italic
                                 :weight normal))))

   `(sr-broken-link-face
     ((,class (:inherit dimonokai-mod-red-warning
                        :slant italic
                        :weight normal))
      (,terminal-class (:inherit dimonokai-mod-red-warning
                                 :slant italic
                                 :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(sr-encrypted-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(sr-log-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(sr-packaged-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(sr-html-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(sr-xml-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,class (:background ,monokai-mod-red
                           :foreground ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:background ,terminal-monokai-mod-red
                                    :foreground ,terminal-monokai-mod-bg
                                    :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-yellow))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-yellow))))

   `(syslog-hour-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-green))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-green))))

   `(syslog-error-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-red
                                    :weight bold))))

   `(syslog-warn-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-orange
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-orange
                                    :weight bold))))

   `(syslog-info-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-blue
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   `(syslog-debug-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-cyan
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-cyan
                                    :weight bold))))

   `(syslog-su-face
     ((,class (:background unspecified
                           :foreground ,monokai-mod-magenta))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-magenta))))

   ;; table
   `(table-cell
     ((,class (:foreground ,monokai-mod-fg
                           :background ,monokai-mod-highlight-line))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :background ,terminal-monokai-mod-highlight-line))))

   ;; term
   `(term-color-black
     ((,class (:foreground ,monokai-mod-bg
                           :background ,monokai-mod-highlight-line))
      (,terminal-class (:foreground ,terminal-monokai-mod-bg
                                    :background ,terminal-monokai-mod-highlight-line))))

   `(term-color-red
     ((,class (:foreground ,monokai-mod-red
                           :background ,monokai-mod-red-d))
      (,terminal-class (:foreground ,terminal-monokai-mod-red
                                    :background ,terminal-monokai-mod-red-d))))

   `(term-color-green
     ((,class (:foreground ,monokai-mod-green
                           :background ,monokai-mod-green-d))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :background ,terminal-monokai-mod-green-d))))

   `(term-color-yellow
     ((,class (:foreground ,monokai-mod-yellow
                           :background ,monokai-mod-yellow-d))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :background ,terminal-monokai-mod-yellow-d))))

   `(term-color-blue
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-blue-d))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-blue-d))))

   `(term-color-magenta
     ((,class (:foreground ,monokai-mod-magenta
                           :background ,monokai-mod-magenta-d))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta
                                    :background ,terminal-monokai-mod-magenta-d))))

   `(term-color-cyan
     ((,class (:foreground ,monokai-mod-cyan
                           :background ,monokai-mod-cyan-d))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :background ,terminal-monokai-mod-cyan-d))))

   `(term-color-white
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-fg))))

   `(term-default-fg-color
     ((,class (:inherit term-color-white))
      (,terminal-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,class (:inherit term-color-black))
      (,terminal-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,class (:background ,monokai-mod-green-l
                           :foreground ,monokai-mod-bg
                           :inherit ,monokai-mod-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,class (:foreground ,monokai-mod-magenta
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta
                                    :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,class (:foreground ,monokai-mod-blue
                           :background ,monokai-mod-highlight-line
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :background ,terminal-monokai-mod-highlight-line
                                    :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,class (:foreground ,monokai-mod-emph))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph))))

   `(tuareg-font-lock-error-face
     ((,class (:foreground ,monokai-mod-yellow
                           :background ,monokai-mod-red
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :background ,terminal-monokai-mod-red
                                    :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-bg))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-bg))))

   `(undo-tree-visualizer-unmodified-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(undo-tree-visualizer-current-face
     ((,class (:foreground ,monokai-mod-blue
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,class (:foreground ,monokai-mod-emph
                           :background ,monokai-mod-bg
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :background ,terminal-monokai-mod-bg
                                    :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,class (:background ,monokai-mod-green-lc
                           :foreground ,monokai-mod-green-hc))
      (,terminal-class (:background ,terminal-monokai-mod-green-lc
                                    :foreground ,terminal-monokai-mod-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,class (:inherit link))
      (,terminal-class (:inherit link))))

   `(w3m-arrived-anchor
     ((,class (:inherit link-visited))
      (,terminal-class (:inherit link-visited))))

   `(w3m-form
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-fg))))

   `(w3m-header-line-location-title
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-yellow))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-yellow))))

   `(w3m-header-line-location-content

     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg))))

   `(w3m-bold
     ((,class (:foreground ,monokai-mod-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :weight bold))))

   `(w3m-image-anchor
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-cyan
                           :inherit link))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-cyan
                                    :inherit link))))

   `(w3m-image
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-cyan))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,class (:foreground ,monokai-mod-emph))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph))))

   `(w3m-lnum-match
     ((,class (:background ,monokai-mod-highlight-line))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line))))

   `(w3m-lnum
     ((,class (:underline nil
                          :bold nil
                          :foreground ,monokai-mod-red))
      (,terminal-class (:underline nil
                                   :bold nil
                                   :foreground ,terminal-monokai-mod-red))))

   `(w3m-session-select
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(w3m-session-selected
     ((,class (:foreground ,monokai-mod-emph
                           :bold t
                           :underline t))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :bold t
                                    :underline t))))

   `(w3m-tab-background
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-fg))))

   `(w3m-tab-selected-background
     ((,class (:background ,monokai-mod-bg
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-bg
                                    :foreground ,terminal-monokai-mod-fg))))

   `(w3m-tab-mouse
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-yellow))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-yellow))))

   `(w3m-tab-selected
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-emph
                           :bold t))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-emph
                                    :bold t))))

   `(w3m-tab-unselected
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-fg))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-fg))))

   `(w3m-tab-selected-retrieving
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-red))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-red))))

   `(w3m-tab-unselected-retrieving
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-orange))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-orange))))

   `(w3m-tab-unselected-unseen
     ((,class (:background ,monokai-mod-highlight-line
                           :foreground ,monokai-mod-violet))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :foreground ,terminal-monokai-mod-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(web-mode-comment-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   `(web-mode-constant-face
     ((,class (:foreground ,monokai-mod-blue
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :weight bold))))

   `(web-mode-current-element-highlight-face
     ((,class (:underline unspecified
                          :weight unspecified
                          :background ,monokai-mod-highlight-line))
      (,terminal-class (:underline unspecified
                                   :weight unspecified
                                   :background ,terminal-monokai-mod-highlight-line))))

   `(web-mode-css-at-rule-face
     ((,class (:foreground ,monokai-mod-violet
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet
                                    :slant italic))))

   `(web-mode-css-pseudo-class-face
     ((,class (:foreground ,monokai-mod-green
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :slant italic))))

   `(web-mode-doctype-face
     ((,class (:foreground ,monokai-mod-comments
                           :slant italic
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :slant italic
                                    :weight bold))))

   `(web-mode-folded-face
     ((,class (:underline t))
      (,terminal-class (:underline t))))

   `(web-mode-function-name-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(web-mode-html-attr-name-face
     ((,class (:foreground ,monokai-mod-blue
                           :slant normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue
                                    :slant normal))))

   `(web-mode-html-attr-value-face
     ((,class (:foreground ,monokai-mod-cyan
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan
                                    :slant italic))))

   `(web-mode-html-tag-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(web-mode-keyword-face
     ((,class (:foreground ,monokai-mod-yellow
                           :weight normal))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :weight normal))))

   `(web-mode-preprocessor-face
     ((,class (:foreground ,monokai-mod-yellow
                           :slant normal
                           :weight unspecified))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow
                                    :slant normal
                                    :weight unspecified))))

   `(web-mode-string-face
     ((,class (:foreground ,monokai-mod-cyan))
      (,terminal-class (:foreground ,terminal-monokai-mod-cyan))))

   `(web-mode-type-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(web-mode-variable-name-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(web-mode-warning-face
     ((,class (:inherit font-lock-warning-face))
      (,terminal-class (:inherit font-lock-warning-face))))

   `(web-mode-block-attr-name-face
     ((,class (:inherit web-mode-html-attr-name-face))
      (,terminal-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-block-attr-value-face
     ((,class (:inherit web-mode-html-attr-value-face))
      (,terminal-class (:inherit web-mode-html-attr-value-face))))

   `(web-mode-block-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,class (:inherit font-lock-preprocessor-face))
      (,terminal-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-face
     ((,class (:background unspecified))
      (,terminal-class (:background unspecified))))

   `(web-mode-block-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,class (:box 1
                    :weight bold))
      (,terminal-class (:box 1
                             :weight bold))))

   `(web-mode-css-color-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-priority-face
     ((,class (:inherit font-lock-builtin-face))
      (,terminal-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,class (:inherit font-lock-variable-name-face))
      (,terminal-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,class (:inherit font-lock-keyword-face))
      (,terminal-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-json-context-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(web-mode-json-key-face
     ((,class (:foreground ,monokai-mod-violet))
      (,terminal-class (:foreground ,terminal-monokai-mod-violet))))

   `(web-mode-json-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(web-mode-part-comment-face
     ((,class (:inherit web-mode-comment-face))
      (,terminal-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,class (:inherit web-mode-block-face))
      (,terminal-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,class (:inherit web-mode-string-face))
      (,terminal-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(web-mode-whitespace-face
     ((,class (:background ,monokai-mod-red))
      (,terminal-class (:background ,terminal-monokai-mod-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,class (:background unspecified
                           :foreground ,monokai-mod-comments
                           :inverse-video unspecified
                           :slant italic))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-comments
                                    :inverse-video unspecified
                                    :slant italic))))

   `(whitespace-hspace
     ((,class (:background unspecified
                           :foreground ,monokai-mod-emph
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-emph
                                    :inverse-video unspecified))))

   `(whitespace-tab
     ((,class (:background unspecified
                           :foreground ,monokai-mod-red
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-red
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-newline
     ((,class(:background unspecified
                          :foreground ,monokai-mod-comments
                          :inverse-video unspecified))
      (,terminal-class(:background unspecified
                                   :foreground ,terminal-monokai-mod-comments
                                   :inverse-video unspecified))))

   `(whitespace-trailing
     ((,class (:background unspecified
                           :foreground ,monokai-mod-orange-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-orange-lc
                                    :inverse-video t))))

   `(whitespace-line
     ((,class (:background unspecified
                           :foreground ,monokai-mod-magenta
                           :inverse-video unspecified))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-magenta
                                    :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,class (:background ,monokai-mod-red-lc
                           :foreground unspecified
                           :inverse-video unspecified))
      (,terminal-class (:background ,terminal-monokai-mod-red-lc
                                    :foreground unspecified
                                    :inverse-video unspecified))))

   `(whitespace-indentation
     ((,class (:background unspecified
                           :foreground ,monokai-mod-yellow
                           :inverse-video unspecified
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-yellow
                                    :inverse-video unspecified
                                    :weight bold))))

   `(whitespace-empty
     ((,class (:background unspecified
                           :foreground ,monokai-mod-red-lc
                           :inverse-video t))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-red-lc
                                    :inverse-video t))))

   `(whitespace-space-after-tab
     ((,class (:background unspecified
                           :foreground ,monokai-mod-orange
                           :inverse-video t
                           :weight bold))
      (,terminal-class (:background unspecified
                                    :foreground ,terminal-monokai-mod-orange
                                    :inverse-video t
                                    :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(wl-highlight-folder-many-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(wl-highlight-folder-path-face
     ((,class (:foreground ,monokai-mod-orange))
      (,terminal-class (:foreground ,terminal-monokai-mod-orange))))

   `(wl-highlight-folder-unread-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(wl-highlight-folder-zero-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(wl-highlight-folder-unknown-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(wl-highlight-message-citation-header
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(wl-highlight-message-cited-text-1
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(wl-highlight-message-cited-text-2
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(wl-highlight-message-cited-text-3
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(wl-highlight-message-cited-text-4
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(wl-highlight-message-header-contents-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(wl-highlight-message-headers-face
     ((,class (:foreground ,monokai-mod-red))
      (,terminal-class (:foreground ,terminal-monokai-mod-red))))

   `(wl-highlight-message-important-header-contents
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(wl-highlight-message-header-contents
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(wl-highlight-message-important-header-contents2
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(wl-highlight-message-signature
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(wl-highlight-summary-answemonokai-mod-red-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(wl-highlight-summary-disposed-face
     ((,class (:foreground ,monokai-mod-fg
                           :slant italic))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg
                                    :slant italic))))

   `(wl-highlight-summary-new-face
     ((,class (:foreground ,monokai-mod-blue))
      (,terminal-class (:foreground ,terminal-monokai-mod-blue))))

   `(wl-highlight-summary-normal-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(wl-highlight-summary-thread-top-face
     ((,class (:foreground ,monokai-mod-yellow))
      (,terminal-class (:foreground ,terminal-monokai-mod-yellow))))

   `(wl-highlight-thread-indent-face
     ((,class (:foreground ,monokai-mod-magenta))
      (,terminal-class (:foreground ,terminal-monokai-mod-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,class (:foreground ,monokai-mod-fg))
      (,terminal-class (:foreground ,terminal-monokai-mod-fg))))

   `(wl-highlight-summary-displaying-face
     ((,class (:underline t
                          :weight bold))
      (,terminal-class (:underline t
                                   :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,class (:inherit error))
      (,terminal-class (:inherit error))))

   `(weechat-highlight-face
     ((,class (:foreground ,monokai-mod-emph
                           :weight bold))
      (,terminal-class (:foreground ,terminal-monokai-mod-emph
                                    :weight bold))))

   `(weechat-nick-self-face
     ((,class (:foreground ,monokai-mod-green
                           :weight unspecified
                           :inverse-video t))
      (,terminal-class (:foreground ,terminal-monokai-mod-green
                                    :weight unspecified
                                    :inverse-video t))))

   `(weechat-prompt-face
     ((,class (:inherit minibuffer-prompt))
      (,terminal-class (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,class (:foreground ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments))))

   ;; which-func-mode
   `(which-func
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   ;; window-number-mode
   `(window-number-face
     ((,class (:foreground ,monokai-mod-green))
      (,terminal-class (:foreground ,terminal-monokai-mod-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-comments))))

   `(yascroll:thumb-fringe
     ((,class (:foreground ,monokai-mod-comments
                           :background ,monokai-mod-comments))
      (,terminal-class (:foreground ,terminal-monokai-mod-comments
                                    :background ,terminal-monokai-mod-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,class (:background ,monokai-mod-highlight-line
                           :box ,monokai-mod-emph))
      (,terminal-class (:background ,terminal-monokai-mod-highlight-line
                                    :box ,terminal-monokai-mod-emph)))))

  (custom-theme-set-variables
   'monokai-mod
   `(ansi-color-names-vector [,monokai-mod-bg ,monokai-mod-red ,monokai-mod-green ,monokai-mod-yellow
                                          ,monokai-mod-blue ,monokai-mod-magenta ,monokai-mod-cyan ,monokai-mod-fg])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,monokai-mod-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,monokai-mod-magenta ,monokai-mod-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,monokai-mod-highlight-line . 0)
       (,monokai-mod-green-lc . 20)
       (,monokai-mod-cyan-lc . 30)
       (,monokai-mod-blue-lc . 50)
       (,monokai-mod-yellow-lc . 60)
       (,monokai-mod-orange-lc . 70)
       (,monokai-mod-magenta-lc . 85)
       (,monokai-mod-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,monokai-mod-bg)
   `(pos-tip-background-color ,monokai-mod-green)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,monokai-mod-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,monokai-mod-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,monokai-mod-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,monokai-mod-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,monokai-mod-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,monokai-mod-bg ,monokai-mod-highlight-line
                  ,monokai-mod-red-d ,monokai-mod-red
                  ,monokai-mod-green-d ,monokai-mod-green
                  ,monokai-mod-yellow-d ,monokai-mod-yellow
                  ,monokai-mod-blue-d ,monokai-mod-blue
                  ,monokai-mod-magenta-d ,monokai-mod-magenta
                  ,monokai-mod-cyan-d ,monokai-mod-cyan
                  ,monokai-mod-fg ,monokai-mod-emph))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monokai-mod)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; monokai-mod-theme.el ends here
