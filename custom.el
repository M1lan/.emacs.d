;;; custom.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file is not edited by hand!  Use M-customize instead.
;;
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
   ["#252525" "#ed4a46" "#70b433" "#dbb32d" "#368aeb" "#eb6eb7"
    "#3fc5b7" "#181818"])
 '(beacon-color "#d33682")
 '(blink-matching-paren t)
 '(browse-url-firefox-program "firefox-bin")
 '(browse-url-xterm-program "kitty")
 '(chatgpt-shell-openai-key nil)
 '(column-number-mode t)
 '(company-dabbrev-downcase t)
 '(company-dabbrev-ignore-case t)
 '(company-minimum-prefix-length 2)
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
   '("8bb0936189769d649025f7acbf07a353e76befb87a085631696d8002965acceb"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077"
     "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307"
     "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "b785d0ad12b0e661443aa636b4eaf012d75a252b1e5fae6e2c16ef80dcb32add"
     "1301f2a53066d132295757da03efc602e07b3b57d2339b00fc3684e4d0a5204e"
     default))
 '(desktop-save 'ask)
 '(desktop-save-mode t)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-details-hidden-string "")
 '(dired-hide-details-hide-information-lines nil)
 '(dired-listing-switches "-alGh")
 '(dired-recursive-copies 'always)
 '(dired-use-ls-dired t)
 '(display-fill-column-indicator t)
 '(display-line-numbers t)
 '(display-line-numbers-type 'relative)
 '(dockerfile-enable-auto-indent nil)
 '(erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
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
 '(global-ligature-mode t)
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
 '(recentf-max-menu-items 99)
 '(recentf-max-saved-items 50)
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
 '(speedbar-default-position 'left)
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
 '(which-key-mode t)
 '(winner-mode t)
 '(word-wrap t)
 '(yas-global-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#001500" :foreground "#FFFF84" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 1.0 :width normal :foundry "CTDB" :family "Fira Code"))))
 '(font-lock-comment-face ((t (:foreground "chartreuse3")))))
