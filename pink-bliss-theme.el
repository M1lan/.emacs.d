;;; pink-bliss-theme.el --- A pink color theme for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2005–2015  Alex Schroeder <alex@gnu.org>
;;               2024       Marie K. Ekeberg <mke@themkat.net>
;;               2025       Milan `insomniaSalt` Santosi <insomniaSalt@gmail.com>

;; Author: Marie K. Ekeberg <mke@themkat.net>
;; Maintainer: [Your Name]
;; URL: https://github.com/[your-username]/pink-bliss-theme
;; Version: 2.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces, theme, pink

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Pink Bliss is a bright pink theme for Emacs.
;; Based on the original Pink Bliss theme with additional modern mode support.

;;; Code:

(require 'cl-lib)

(deftheme pink-bliss
  "A theme based on the color pink.
It is very pink.")

(let ((class '((class color) (min-colors 256))))
  (custom-theme-set-faces
   'pink-bliss
   `(default ((,class (:background "misty rose" :foreground "magenta4"))))
   `(cursor ((,class (:background "deep pink"))))
   `(button ((,class (:bold t))))
   `(fringe ((,class (:background "misty rose"))))
   `(menu ((,class (:background "pink" :foreground "violet red"))))
   
   ;; Mode line styles
   `(mode-line ((,class (:background "pink" :foreground "purple"
                                    :box (:line-width 1 :style released-button)))))
   `(mode-line-inactive ((,class (:background "pink" :foreground "orchid"
                                            :box (:line-width 1
                                                            :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground "purple" :weight bold))))
   `(mode-line-emphasis ((,class (:foreground "deep pink" :weight bold))))
   `(mode-line-highlight ((,class (:foreground "violet red" :box (:line-width 1 :style released-button)))))
   
   `(minibuffer-prompt ((,class (:foreground "deep pink" :weight bold))))
   `(tool-bar ((,class (:background "pink"
                                   :box (:line-width 1 :style released-button)))))
   `(tooltip ((,class (:background "lemon chiffon"
                                  :foreground "violet red"))))
   `(region ((,class (:background "seashell"))))
   
   ;; Line highlighting
   `(hl-line ((,class (:background "pink" :extend t))))
   
   ;; isearch
   `(isearch ((,class (:foreground "white" :background "hot pink"))))
   `(isearch-fail ((,class (:foreground "white" :background "red"))))
   `(lazy-highlight ((,class (:foreground "white" :background "deep pink"))))
   
   ;; info-mode
   `(header-line ((,class (:background "hot pink" :foreground "white"))))
   
   ;; calendar
   `(calendar-today-face ((,class (:foreground "lemon chiffon"))))
   `(diary-face ((,class (:bold t :foreground "yellow"))))
   `(holiday-face ((,class (:bold t :foreground "peru"))))
   
   ;; font-lock
   `(font-lock-builtin-face ((,class (:foreground "orchid" :weight bold))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "coral"))))
   `(font-lock-comment-face ((,class (:foreground "salmon"))))
   `(font-lock-constant-face ((,class (:foreground "orchid"))))
   `(font-lock-doc-face ((,class (:foreground "coral"))))
   `(font-lock-function-name-face ((,class (:foreground "deep pink"))))
   `(font-lock-keyword-face ((,class (:foreground "purple" :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground "red"))))
   `(font-lock-preprocessor-face ((,class (:foreground "HotPink2" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "pale violet red"))))
   `(font-lock-type-face ((,class (:foreground "light slate blue" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "hot pink"))))
   `(font-lock-warning-face ((,class (:bold t :foreground "red"))))
   
   ;; cperl
   `(cperl-array-face ((,class (:bold t :foreground "tomato"))))
   `(cperl-hash-face  ((,class (:bold t :foreground "chocolate"))))
   `(cperl-nonoverridable-face  ((,class (:foreground "red"))))
   
   ;; makefiles
   `(makefile-shell-face  ((,class (:background "linen"))))
   
   ;; helm
   `(helm-header ((,class (:background "hot pink" :foreground "seashell"))))
   `(helm-ff-dotted-directory ((,class (:background "seashell" :foreground "hot pink" :weight bold))))
   `(helm-candidate-number ((,class (:background "magenta" :foreground "seashell"))))
   `(helm-source-header ((,class (:background "hot pink" :foreground "seashell"))))
   `(helm-selection ((,class (:background "pink" :foreground "purple" :weight bold))))
   
   ;; ivy (part of swiper)
   `(ivy-confirm-face ((,class (:foreground "magenta"))))
   `(ivy-current-match ((,class (:background "light pink"))))
   `(ivy-match-required-face ((,class (:foreground "deep pink" :weight bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground "deep pink"))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground "hot pink" :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground "medium violet red"))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground "purple" :weight bold))))
   
   ;; vertico, consult, marginalia (modern completion UI)
   `(vertico-current ((,class (:background "pink" :foreground "purple" :weight bold))))
   `(completions-common-part ((,class (:foreground "hot pink"))))
   `(completions-first-difference ((,class (:foreground "deep pink" :weight bold))))
   `(marginalia-documentation ((,class (:foreground "salmon"))))
   `(marginalia-key ((,class (:foreground "orchid"))))
   
   ;; gnus
   `(message-header-name ((,class (:foreground "red"))))
   `(message-header-other ((,class (:foreground "dark orange"))))
   
   ;; ediff
   `(ediff-current-diff-A ((,class (:background "papaya whip"))))
   `(ediff-current-diff-Ancestor ((,class (:background "papaya whip"))))
   `(ediff-current-diff-B ((,class (:background "papaya whip"))))
   `(ediff-current-diff-C ((,class (:background "papaya whip"))))
   `(ediff-even-diff-A ((,class (:background "seashell"))))
   `(ediff-even-diff-Ancestor ((,class (:background "seashell"))))
   `(ediff-even-diff-B ((,class (:background "seashell"))))
   `(ediff-even-diff-C ((,class (:background "seashell"))))
   `(ediff-fine-diff-A ((,class (:background "moccasin"))))
   `(ediff-fine-diff-Ancestor ((,class (:background "moccasin"))))
   `(ediff-fine-diff-B ((,class (:background "moccasin"))))
   `(ediff-fine-diff-C ((,class (:background "moccasin"))))
   `(ediff-odd-diff-A ((,class (:background "seashell"))))
   `(ediff-odd-diff-Ancestor ((,class (:background "seashell"))))
   `(ediff-odd-diff-B ((,class (:background "seashell"))))
   `(ediff-odd-diff-C ((,class (:background "seashell"))))
   
   ;; highlights (mouse hovers, other hovers etc.)
   `(highlight ((,class (:background "pink" :foreground "magenta4"))))
   
   ;; widgets
   `(widget-field ((,class (:background "pink" :foreground "magenta4" :extend t))))
   `(widget-button ((,class (:background "pink" :foreground "magenta4" :box t :weight bold))))
   
   ;; dashboard
   `(dashboard-items-face ((,class (:foreground "magenta4"))))
   `(dashboard-no-items-face ((,class (:foreground "magenta4"))))
   `(dashboard-heading-face ((,class (:foreground "deep pink" :weight bold))))
   `(dashboard-banner-logo-title-face ((,class (:foreground "hot pink" :weight bold))))
   
   ;; magit
   `(magit-section-highlight ((,class (:background "pink"))))
   `(magit-diff-hunk-heading ((,class (:foreground "black" :background "MistyRose2"))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground "black" :background "MistyRose3"))))
   `(magit-diff-context ((,class (:inherit default))))
   `(magit-diff-context-highlight ((,class (:background "MistyRose2"))))
   `(magit-diff-removed ((,class (:background "RosyBrown2"))))
   `(magit-diff-added ((,class (:background "RosyBrown1"))))
   `(magit-diff-removed-highlight ((,class (:background "pink3"))))
   `(magit-diff-added-highlight ((,class (:background "pink1"))))
   `(magit-diff-whitespace-warning ((,class (:background "violet red"))))
   `(magit-section-heading ((,class (:foreground "firebrick"))))
   `(magit-section-highlight ((,class (:background "#fdc"))))
   `(magit-diff-file-heading ((,class (:foreground "firebrick4"))))
   `(magit-diff-file-heading-highlight ((,class (:background "#fdd"))))
   `(magit-hash ((,class (:inherit bold))))
   `(magit-branch-local ((,class (:foreground "PaleVioletRed2" :weight bold))))
   `(magit-branch-remote ((,class (:foreground "PaleVioletRed3" :weight bold))))
   
   ;; company-mode
   `(company-echo ((,class (:background "pink" :foreground "magenta4" :extend t))))
   `(company-tooltip ((,class (:background "pink" :foreground "magenta4" :extend t))))
   `(company-tooltip-annotation ((,class (:background "pink" :foreground "seashell" :extend t))))
   `(company-tooltip-selection ((,class (:background "hot pink" :foreground "magenta4" :weight bold :extend t))))
   `(company-tooltip-common ((,class (:foreground "purple" :weight bold))))
   `(company-tooltip-scrollbar-track ((,class (:background "MistyRose2"))))
   `(company-tooltip-scrollbar-thumb ((,class (:background "hot pink"))))
   
   ;; corfu (modern company alternative)
   `(corfu-default ((,class (:background "pink" :foreground "magenta4"))))
   `(corfu-current ((,class (:background "hot pink" :foreground "white"))))
   `(corfu-bar ((,class (:background "deep pink"))))
   `(corfu-border ((,class (:background "violet red"))))
   
   ;; lsp-mode
   `(lsp-headerline-breadcrumb-path-face ((,class (:foreground "seashell"))))
   `(lsp-headerline-breadcrumb-separator-face ((,class (:foreground "purple" :height 0.8))))
   `(lsp-headerline-breadcrumb-symbols-face ((,class (:foreground "seashell"))))
   `(lsp-ui-doc-background ((,class (:background "seashell"))))
   `(lsp-face-highlight-textual ((,class (:background "pink" :foreground "purple" :weight bold))))
   `(lsp-face-highlight-read ((,class (:background "hot pink" :foreground "white"))))
   `(lsp-face-highlight-write ((,class (:background "deep pink" :foreground "white" :weight bold))))
   
   ;; eglot (built-in LSP client)
   `(eglot-highlight-symbol-face ((,class (:background "pink" :foreground "purple" :weight bold))))
   `(eglot-mode-line ((,class (:foreground "deep pink" :weight bold))))
   
   ;; treesit (new built-in syntax highlighting)
   `(tree-sitter-hl-face:function ((,class (:foreground "deep pink"))))
   `(tree-sitter-hl-face:function.call ((,class (:foreground "deep pink"))))
   `(tree-sitter-hl-face:method ((,class (:foreground "hot pink"))))
   `(tree-sitter-hl-face:method.call ((,class (:foreground "hot pink"))))
   `(tree-sitter-hl-face:property ((,class (:foreground "orchid"))))
   `(tree-sitter-hl-face:constant ((,class (:foreground "orchid"))))
   `(tree-sitter-hl-face:type ((,class (:foreground "light slate blue" :weight bold))))
   `(tree-sitter-hl-face:type.parameter ((,class (:foreground "light slate blue"))))
   `(tree-sitter-hl-face:constructor ((,class (:foreground "purple" :weight bold))))
   `(tree-sitter-hl-face:string ((,class (:foreground "pale violet red"))))
   `(tree-sitter-hl-face:comment ((,class (:foreground "salmon"))))
   `(tree-sitter-hl-face:keyword ((,class (:foreground "purple" :weight bold))))
   `(tree-sitter-hl-face:operator ((,class (:foreground "deep pink" :weight bold))))
   
   ;; org-mode
   `(org-level-1 ((,class (:foreground "violet red" :background "pink" :weight extra-bold :height 1.5))))
   `(org-level-2 ((,class (:foreground "hot pink" :background "pink" :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground "pale violet red" :background "pink" :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground "deep pink" :background "pink" :weight bold :height 1.0))))
   `(org-level-5 ((,class (:foreground "PaleVioletRed3" :weight bold :height 1.0))))
   `(org-level-6 ((,class (:foreground "maroon" :weight bold :height 1.0))))
   `(org-level-7 ((,class (:foreground "light slate blue" :weight bold :height 1.0))))
   `(org-level-8 ((,class (:foreground "coral" :weight bold :height 1.0))))
   `(org-block ((,class (:background "white"))))
   `(org-block-begin-line ((,class (:foreground "purple" :background "pink" :extend t :weight bold))))
   `(org-block-end-line ((,class (:foreground "purple" :background "pink" :extend t :weight bold))))
   `(org-table ((,class (:foreground "hot pink" :background "white" :weight bold))))
   `(org-quote ((,class (:background "white" :extend t))))
   `(org-verse ((,class (:background "white" :extend t))))
   `(org-verbatim ((,class (:background "white"))))
   `(org-link ((,class (:foreground "purple" :underline t :weight bold))))
   `(org-todo ((,class (:foreground "hot pink" :weight bold))))
   `(org-done ((,class (:foreground "green" :weight bold))))
   `(org-agenda-structure ((,class (:foreground "hot pink" :weight bold))))
   `(org-agenda-date ((,class (:foreground "purple"))))
   `(org-agenda-date-today ((,class (:foreground "deep pink" :weight bold))))
   `(org-agenda-done ((,class (:foreground "green"))))
   
   ;; Markdown mode (mimic some org mode styling)
   `(markdown-header-face-1 ((,class (:foreground "violet red" :weight bold :height 1.5))))
   `(markdown-header-face-2 ((,class (:foreground "hot pink" :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground "pale violet red" :weight bold :height 1.1))))
   `(markdown-header-face-4 ((,class (:foreground "deep pink" :weight bold :height 1.0))))
   `(markdown-header-face-5 ((,class (:foreground "light slate blue" :weight bold :height 1.0))))
   `(markdown-header-face-6 ((,class (:foreground "coral" :weight bold :height 1.0))))
   `(markdown-code-face ((,class (:background "white" :extend t))))
   `(markdown-markup-face ((,class (:foreground "purple"))))
   `(markdown-link-face ((,class (:foreground "purple" :weight bold))))
   `(markdown-url-face ((,class (:foreground "hot pink" :underline t))))
   
   ;; Dired
   `(dired-directory ((,class (:foreground "deep pink" :weight bold))))
   `(dired-flagged ((,class (:foreground "white" :background "red"))))
   `(dired-marked ((,class (:foreground "white" :background "deep pink" :weight bold))))
   
   ;; Centaur tabs
   `(centaur-tabs-selected ((,class (:background "hot pink" :foreground "white"))))
   `(centaur-tabs-selected-modified ((,class (:background "violet red" :foreground "white" :weight bold))))
   `(centaur-tabs-modified-marker-selected ((,class (:background "violet red" :foreground "white"))))
   `(centaur-tabs-unselected ((,class (:background "pink" :foreground "white"))))
   `(centaur-tabs-unselected-modified ((,class (:background "pink" :foreground "white" :weight bold))))
   `(centaur-tabs-modified-marker-unselected ((,class (:background "pink" :foreground "white"))))))

;; Define custom variables
(custom-theme-set-variables
 'pink-bliss
 '(org-fontify-quote-and-verse-blocks t)
 '(CUA-mode-read-only-cursor-color "dark grey")
 '(help-highlight-face 'info-xref)
 '(list-matching-lines-buffer-name-face 'bold))

;; Define the foreground colors for chat applications like ERC, Circe, etc.
(defvar pink-bliss-foreground-colors
  (let ((candidates)
	    (green-limit #xa000)
	    (both-limit #xa000))
    (dolist (item color-name-rgb-alist)
      (cl-destructuring-bind (color red green blue) item
	    (when (and (not (color-gray-p color))
		       (< green green-limit)
		       (not (and (> red both-limit)
			         (> green both-limit))))
	      (setq candidates (cons color candidates)))))
    candidates)
  "Colors to use for nicks in chat applications like ERC, Circe, etc.
To check out the list, evaluate:
\(list-colors-display pink-bliss-foreground-colors).")

;; Add chat-related faces
(defun pink-bliss-set-chat-faces ()
  "Set chat-related faces for Pink Bliss."
  (with-eval-after-load 'erc
    (setq erc-nick-default-face '(:weight bold)
          erc-current-nick-face '(:foreground "deep pink" :weight bold)
          erc-notice-face '(:foreground "pink")
          erc-input-face '(:foreground "purple")))
  
  (with-eval-after-load 'circe
    (setq circe-highlight-nick-type 'sender)))

;; Set these chat faces when the theme is loaded
(pink-bliss-set-chat-faces)

;; Register the theme in the custom theme path
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pink-bliss)
;;; pink-bliss-theme.el ends here
