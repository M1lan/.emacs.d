;;; custom.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file is not edited by hand!  Use M-customize instead.
;;
;;
;; Tested on GNU Emacs 31.0.50 (build 1, x86_64-pc-linux-gnu, GTK+
;; Version 3.24.43, cairo version 1.18.0) of 2024-09-06
;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 1.0)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7"
    "#8cc4ff" "#eeeeec"])
 '(beacon-color "#d33682")
 '(blink-matching-paren t)
 '(browse-url-firefox-program "firefox-bin")
 '(browse-url-xterm-program "kitty")
 '(chatgpt-shell-openai-key nil)
 '(column-number-mode t)
 '(company-dabbrev-downcase t)
 '(company-dabbrev-ignore-case t)
 '(company-require-match nil)
 '(company-selection-wrap-around t)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 5)
 '(company-tooltip-limit 15)
 '(copilot-indent-warning-suppress t)
 '(custom-enabled-themes '(pleasant-greens))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   '("1301f2a53066d132295757da03efc602e07b3b57d2339b00fc3684e4d0a5204e"
     default))
;; '(default-frame-alist '((top . 200) (left . 400) (width . 80) (height . 40)))
 '(desktop-save 'ask)
 '(desktop-save-mode t)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-details-hidden-string "")
 '(dired-hide-details-hide-information-lines nil)
 '(dired-listing-switches "-alGh")
 '(dired-recursive-copies 'always)
 '(dired-use-ls-dired t)
 '(display-fill-column-indicator t)
 '(display-line-numbers nil)
 '(dockerfile-enable-auto-indent nil)
 '(exwm-replace t)
 '(fci-rule-color "#eee8d5")
 '(fill-column 69)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flyspell-auto-correct-binding [67108923])
 '(focus-follows-mouse 'auto-raise)
 '(font-use-system-font nil)
 '(frame-background-mode 'light)
 '(fringe-mode '(1 . 1) nil (fringe))
 '(global-company-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-display-line-numbers-mode nil)
 '(global-prettify-symbols-mode t)
 '(helm-mode t)
 '(highlight-indent-guides-method 'character)
 '(horizontal-scroll-bar-mode nil)
 '(hyperbole-mode nil)
 '(ido-mode 'both nil (ido))
 '(initial-buffer-choice t)
 '(initial-scratch-message
   ";; -*- lexical-binding: t -*-\12\12;; This buffer is for text that is not saved, and for Lisp\12;; evaluation.  To create a file, visit it with ‘C-x C-f’ and enter\12;; text in its buffer.\12")
 '(insert-directory-program "ls")
 '(jdee-db-active-breakpoint-face-colors (cons "#222228" "#819cd6"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#222228" "#5b94ab"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#222228" "#515462"))
 '(kubernetes-pods-display-completed t)
 '(line-number-mode t)
 '(ls-lisp-dirs-first t)
 '(lsp-headerline-breadcrumb-enable t)
 '(lsp-headerline-breadcrumb-icons-enable t)
 '(markdown-command "multimarkdown")
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(notmuch-search-line-faces
   '(("unread" :foreground "#aeee00") ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t)))
 '(objed-cursor-color "#e1c1ee")
 '(org-fold-core-style 'overlays)
 '(pdf-view-midnight-colors (cons "#c6c6c6" "#282b33"))
 '(pixel-scroll-precision-mode t)
 '(recentf-auto-cleanup 'never)
 '(recentf-mode t)
 '(red "#ffffff")
 '(rustic-ansi-faces
   ["#282b33" "#e1c1ee" "#5b94ab" "#cfcf9c" "#819cd6" "#a6c1e0"
    "#7289bc" "#c6c6c6"])
 '(safe-local-variable-values
   '((git-commit-major-mode . git-commit-elisp-text-mode)
     (eval and buffer-file-name
	   (not (eq major-mode 'package-recipe-mode))
	   (or (require 'package-recipe-mode nil t)
	       (let ((load-path (cons "../package-build" load-path)))
		 (require 'package-recipe-mode nil t)))
	   (package-recipe-mode))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(server-mode t)
 '(show-paren-mode t)
 '(straight-use-package-by-default t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(truncate-lines t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900")
     (100 . "#2aa198") (120 . "#268bd2") (140 . "#d33682")
     (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16")
     (220 . "#b58900") (240 . "#859900") (260 . "#2aa198")
     (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4")
     (340 . "#dc322f") (360 . "#cb4b16")))
 '(vc-annotate-very-old-color nil)
 '(visible-bell t)
 '(vterm-enable-manipulate-selection-data-by-osc52 t)
 '(warning-suppress-log-types '((native-compiler)))
 '(warning-suppress-types '((comp)))
 '(winner-mode t)
 '(yas-global-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
