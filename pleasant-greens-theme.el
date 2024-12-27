;;; Package --- Summary -*- lexical-binding: t; rainbow-mode -*-
;;
;; Copyleft (C) 1984–2026  insomniaSalt` <insomniaSalt@gmail.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;; Commentary:
;; My theme.  Don't h8.
;;
;; Opinions / Rules:
;; - theme is only for faces, everything else goes into custom.el
;; - no additional faces in init.el and custom.el
;; - themes are useful for GUI Emacs and maybe TTY but not Terminal.
;; - for terminal emulator configure your terminal colors instead!
;;
;;; Code:
;;

(deftheme pleasant-greens
  "Created 2020-05-13, Milan Santosi.")

(custom-theme-set-faces
  'pleasant-greens

  '(default ((t (:inherit nil :extend nil :stipple nil :background "#001500" :foreground "#FFEE84" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 143 :width normal :foundry "CTDB" :family "Fira Code"))))

  '(cov-none-face ((t (:foreground "royal blue"))))
  '(diff-file-header ((t (:extend t :background "#004000" :weight bold))))
  '(diff-hl-delete ((t (:inherit diff-removed :foreground "pink"))))
  '(diff-indicator-added ((t (:inherit diff-added :foreground "#002F00"))))
  '(diff-indicator-removed ((t (:inherit diff-removed))))
  '(diff-refine-added ((t (:inherit diff-refine-changed :background "#bbffbb" :foreground "black"))))
  '(diff-refine-changed ((t (:background "#aaaa55"))))
  '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#ffcccc" :foreground "black"))))
  '(diff-removed ((t (:inherit diff-changed :extend t :background "firebrick" :foreground "black"))))
  '(dired-directory ((t (:foreground "cornflower blue"))))
  '(edmacro-label ((t (:inherit bold :foreground "LightBlue"))))
  '(epa-string ((t (:foreground "LightBlue3"))))
  '(ivy-remote ((t (:foreground "Lightblue4"))))
  '(magit-reflog-checkout ((t (:foreground "lightblue"))))
  '(makefile-makepp-perl ((t (:background "LightBlue3"))))
  '(message-header-cc ((t (:foreground "dark slate blue"))))
  '(message-header-newsgroups ((t (:foreground "Lightblue4" :slant italic :weight bold))))
  '(message-header-subject ((t (:foreground "cadet blue" :weight bold))))
  '(message-header-to ((t (:foreground "CadetBlue" :weight bold))))
  '(message-header-xheader ((t (:foreground "LightBlue2"))))
  '(org-agenda-structure ((t (:foreground "LightBlue1"))))
  '(shr-text ((t nil)))
  '(ansi-color-blue
	 ((t(
		  :background "#0070bf" :foreground "#0070bf"))))
  '(ansi-color-bright-blue
	 ((t ( :background "#00ddfe" :foreground "#00ddfe"))))
  '(button ((t ( :weight bold))))
  '(c-nonbreakable-space-face
	 ((t (:background "orange" :foreground "dark green"))) t)
  '(clang-include-fixer-highlight
	 ((t (:background "green" :foreground "dark green"))))
  '(company-tooltip
	 ((t (:background "#001000" :foreground "dark green"))))
  '(company-tooltip-annotation
	 ((t (:background "#2F0000" :foreground "firebrick1"))))
  '(company-tooltip-scrollbar-track
	 ((t (:background "Goldenrod4" :foreground "khaki2"))))
  '(company-tooltip-selection
	 ((t (:background "black" :foreground "violet"))))
  '(completions-common-part ((t (:foreground "SteelBlue1"))))
  '(completions-first-difference ((t (:inherit bold))))
  '(cursor ((t (:background "green"))))
  '(custom-button
	 ((t (:background "#47a757" :foreground "black" :box
		   (:line-width 2 :style released-button)))))
  '(custom-changed
	 ((t (:background "dark olive green" :foreground "pale green"))))
  '(custom-comment ((t (:background "dark green"))))
  '(custom-comment-tag ((t (:foreground "dim gray"))))
  '(custom-group-tag
	 ((t
		(:inherit variable-pitch :foreground "SeaGreen1" :weight bold
	      :height 1.2))))
  '(custom-modified ((t (:background "#00b0ff" :foreground "white"))))
  '(custom-state ((t (:foreground "pale green"))))
  '(custom-variable-obsolete ((t (:foreground "turquoise"))))
  '(custom-variable-tag ((t (:foreground "SeaGreen3" :weight bold))))
  '(diff-added
	 ((t
		(:inherit diff-changed :extend t :background "spring green"
	      :foreground "black"))))
  '(diff-changed-unspecified
	 ((t
		(:inherit diff-changed :extend t :background "grey90" :foreground
	      "black"))))
  '(diff-header ((t (:extend t :background "gray17"))))
  '(diff-removed
	 ((t
		(:inherit diff-changed :extend t :background "tomato" :foreground
	      "black"))))
  '(ediff-current-diff-B ((t (:background "#310012"))))
  '(ediff-even-diff-A ((t (:background "#000000" :foreground "cyan"))))
  '(ediff-even-diff-B ((t (:background "#A25700"))))
  '(ediff-even-diff-C ((t (:background "#004000"))))
  '(ediff-fine-diff-B ((t (:background "#002029"))))
  '(ediff-odd-diff-A ((t (:background "#000232"))))
  '(ediff-odd-diff-B ((t (:background "#200000"))))
  '(ediff-odd-diff-C ((t (:background "#070000"))))
  '(error ((t (:foreground "orange red" :weight bold))))
  '(eshell-ls-directory ((t (:foreground "dodger blue" :weight bold))))
  '(eshell-prompt
	 ((t (:foreground "medium spring green" :weight bold))))
  '(fill-column-indicator
	 ((t
		(:stipple nil :foreground "#4703E0" :inverse-video nil :box nil
	      :strike-through nil :overline nil :underline nil :slant
	      normal :weight normal))))

  '(font-lock-builtin-face ((t (:foreground "green"))))
  '(font-lock-comment-face ((t (:foreground "chartreuse3"))))
  '(font-lock-constant-face
	 ((t (:foreground "SeaGreen4" :weight bold :width normal))))
  '(font-lock-function-name-face ((t (:foreground "OliveDrab2"))))
  '(font-lock-keyword-face
	 ((t (:foreground "limegreen" :slant oblique))))
  '(font-lock-string-face ((t (:foreground "powder blue"))))
  '(font-lock-type-face ((t (:foreground "goldenrod"))))
  '(font-lock-variable-name-face
	 ((t (:foreground "gold" :weight extra-bold))))
  '(font-lock-warning-face ((t (:foreground "orange red"))))
  '(fringe ((t (:background "black"))))
  '(hbut-item-face ((t (:background "yellow" :foreground "black"))))
  '(header-line
	 ((t
		(:inherit mode-line :background "grey20" :foreground "grey90"
	      :box nil))))
  '(helm-source-header
	 ((t
		(:extend t :background "#abd7f0" :foreground "black" :weight bold
	      :height 1.3))))
  '(help-key-binding
	 ((t
		(:inherit fixed-pitch :background "#002f00" :foreground "white"
	      :box (:line-width (-1 . -1) :color "#0c0c0c")))))
  '(highlight ((t (:background "#002900" :foreground "#00FF00"))))
  '(highlight-indent-guides-character-face
	 ((t (:foreground "#3000aa"))))
  '(hl-line ((t (:extend t :background "#004200"))))
  '(holiday ((t (:background "pink" :foreground "black"))))
  '(isearch
	 ((t (:background "#00AA00" :foreground "#001211" :weight bold))))
  '(isearch-fail ((t (:background "#2A1029" :foreground "#FB000F"))))
  '(isearch-group-1
	 ((t (:background "#0020a0" :foreground "lightblue1"
		   :weight bold))))
  '(isearch-group-2
	 ((t (:background "#59109F" :foreground "#aFF0F0" :weight bold))))
  '(ivy-minibuffer-match-face-1
	 ((t (:background "#d3d3d3" :foreground "black"))))
  '(ivy-minibuffer-match-face-2
	 ((t (:background "#e99ce8" :foreground "black" :weight bold))))
  '(ivy-minibuffer-match-face-3
	 ((t (:background "#bbbbff" :foreground "black" :weight bold))))
  '(ivy-minibuffer-match-face-4
	 ((t (:background "#ffbbff" :foreground "black" :weight bold))))
  '(lazy-highlight
	 ((t
		(:background "paleturquoise" :distant-foreground "black"
		  :foreground "black"))))
  '(line-number ((t (:inherit (shadow default) :height 0.75))))
  '(link ((t (:foreground "#00AE9A" :underline t))))
  '(log-view-file
	 ((t
		(:extend t :background "grey70"
	      :foreground "black" :weight bold))))
  '(log-view-message
	 ((t (:extend t :background "grey85" :foreground "black"))))
  '(lsp-details-face
	 ((t (:inherit shadow :background "#033300" :height 0.8))))
  '(lsp-ui-doc-background ((t (:background "gray7"))))
  '(lsp-ui-peek-header ((t (:background "grey7"
							 :foreground "white"))))
  '(lsp-ui-peek-list ((t (:background "gray7"))))
  '(lsp-ui-peek-peek ((t (:background "gray7"))))
  '(lsp-ui-peek-selection
	 ((t (:background "grey7" :foreground "white"))))
  '(magit-diff-added
	 ((t (:extend t :background "#070907" :foreground "#22aa22"))))
  '(magit-diff-added-highlight
	 ((t (:extend t :background "#002300" :foreground "#22AA22"))))
  '(magit-diff-context-highlight
	 ((t (:extend t :foreground "white smoke"))))
  '(magit-diff-removed-highlight
	 ((t (:extend t :background "#230000" :foreground "#aa2222"))))
  '(magit-section-highlight
	 ((t (:extend t :background "SeaGreen3" :foreground "black"))))
  '(makefile-space ((t (:background "#132500"))))
  '(markup-gen-face ((t (:foreground "slate blue"))))
  '(match ((t (:background "dark green"))))
  '(minibuffer-prompt ((t (:foreground "limegreen"))))
  '(mode-line
	 ((t
		(:background "chartreuse" :foreground "black" :box
		  (:line-width 1 :style released-button)
		  :weight bold))))
  '(mode-line-inactive
	 ((t
		(:inherit mode-line :background "#002000" :foreground
	      "SpringGreen4" :weight light))))
  '(org-agenda-restriction-lock
	 ((t (:background "#eeeeee" :foreground "black"))))
  '(org-column
	 ((t
		(:background "grey90" :foreground "black" :strike-through nil
		  :underline nil :slant normal :weight normal))))
  '(org-column-title
	 ((t
		(:background "grey90" :foreground "black" :underline t :weight
		  bold))))
  '(org-document-info ((t (:foreground "deep sky blue"))))
  '(org-document-title ((t (:foreground "cyan" :weight bold))))
  '(org-drawer ((t (:foreground "SteelBlue1"))))
  '(org-table ((t (:foreground "lightBlue"))))
  '(popup-isearch-match
	 ((t (:inherit default :background "#005200"
		   :foreground "#00ffaa"))))
  '(pulse-highlight-face
	 ((t (:background "#FFFFAA" :foreground "black"))))
  '(pulse-highlight-start-face
	 ((t (:background "#FFFFAA" :foreground "black"))))
  '(region
	 ((t (:extend t :background "forest green" :foreground "black"))))
  '(sh-quoted-exec ((t (:foreground "magenta"))))
  '(shadow ((t (:foreground "grey32"))))
  '(show-paren-match ((t (:background "gold" :foreground "black"))))
  '(smerge-lower
	 ((t (:extend t :background "#bbffbb" :foreground "black"))))
  '(smerge-upper
	 ((t (:extend t :background "#ffbbbb" :foreground "dark green"))))
  '(speedbar-directory-face ((t (:foreground "deep sky blue"))))
  '(speedbar-separator-face
	 ((t(
		  :background "deep sky blue"
		  :foreground "white"
		  :overline "gray"))))
  '(tooltip
	 ((t
		(:inherit variable-pitch :background "lightgreen" :foreground
	      "black"))))
  '(vterm-color-blue ((t nil)))
  '(w3m-anchor
	 ((t (:foreground "forest green" :underline t :weight bold))))
  '(warning ((t (:foreground "firebrick4" :weight bold))))
  '(whitespace-line ((t (:background "#202510"
						  :foreground "violet"))))
  '(whitespace-space
	 ((t (:background "#153320" :foreground "fuchsia"))))
  '(whitespace-tab
	 ((t (:background "#102520" :foreground "goldenrod"))))
  '(widget-field ((t (:extend t :background "#002d00"))))
  '(widget-single-line-field
	 ((t (:background "gray85" :foreground "black"))))
  )

(provide-theme 'pleasant-greens)
;;; pleasant-greens-theme.el ends here
