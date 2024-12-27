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
	["#252525" "#ed4a46" "#70b433" "#dbb32d" "#368aeb" "#eb6eb7" "#3fc5b7"
	  "#181818"])
 '(auth-source-debug t)
 '(beacon-color "#d33682")
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-cursor-mode nil)
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
 '(copilot-indent-offset-warning-disable t)
 '(copilot-indent-warning-suppress t)
 '(copilot-indentation-alist
	'((latex-mode tex-indent-basic) (nxml-mode nxml-child-indent)
	   (python-mode python-indent py-indent-offset python-indent-offset)
	   (python-ts-mode python-indent py-indent-offset
		 python-indent-offset)
	   (web-mode web-mode-markup-indent-offset web-mode-html-offset)
	   (ada-mode ada-indent) (ada-ts-mode ada-ts-mode-indent-offset)
	   (apache-mode apache-indent-level) (awk-mode c-basic-offset)
	   (bash-ts-mode sh-basic-offset sh-indentation)
	   (bpftrace-mode c-basic-offset) (c++-mode c-basic-offset)
	   (c++-ts-mode c-basic-offset c-ts-mode-indent-offset)
	   (c-mode c-basic-offset)
	   (c-ts-mode c-basic-offset c-ts-mode-indent-offset)
	   (cmake-mode cmake-tab-width)
	   (cmake-ts-mode cmake-tab-width cmake-ts-mode-indent-offset)
	   (coffee-mode coffee-tab-width) (cperl-mode cperl-indent-level)
	   (crystal-mode crystal-indent-level) (csharp-mode c-basic-offset)
	   (csharp-ts-mode c-basic-offset csharp-ts-mode-indent-offset)
	   (css-mode css-indent-offset) (css-ts-mode css-indent-offset)
	   (d-mode c-basic-offset) (elixir-ts-mode elixir-ts-indent-offset)
	   (enh-ruby-mode enh-ruby-indent-level)
	   (erlang-mode erlang-indent-level) (ess-mode ess-indent-offset)
	   (f90-mode f90-associate-indent f90-continuation-indent
		 f90-critical-indent f90-do-indent f90-if-indent
		 f90-program-indent f90-type-indent)
	   (feature-mode feature-indent-offset feature-indent-level)
	   (fsharp-mode fsharp-continuation-offset fsharp-indent-level
		 fsharp-indent-offset)
	   (gdscript-mode gdscript-indent-offset)
	   (go-ts-mode go-ts-mode-indent-offset) (gpr-mode gpr-indent)
	   (gpr-ts-mode gpr-ts-mode-indent-offset)
	   (graphql-mode graphql-indent-level)
	   (groovy-mode groovy-indent-offset)
	   (haskell-mode haskell-indent-spaces haskell-indent-offset
		 haskell-indentation-layout-offset
		 haskell-indentation-left-offset
		 haskell-indentation-starter-offset
		 haskell-indentation-where-post-offset
		 haskell-indentation-where-pre-offset shm-indent-spaces)
	   (haxor-mode haxor-tab-width) (hcl-mode hcl-indent-level)
	   (html-ts-mode html-ts-mode-indent-offset)
	   (idl-mode c-basic-offset) (jade-mode jade-tab-width)
	   (java-mode c-basic-offset)
	   (java-ts-mode c-basic-offset java-ts-mode-indent-offset)
	   (js-mode js-indent-level) (js-ts-mode js-indent-level)
	   (js-jsx-mode js-indent-level sgml-basic-offset)
	   (js2-mode js2-basic-offset)
	   (js2-jsx-mode js2-basic-offset sgml-basic-offset)
	   (js3-mode js3-indent-level) (json-mode js-indent-level)
	   (json-ts-mode json-ts-mode-indent-offset)
	   (jsonian-mode jsonian-default-indentation)
	   (julia-mode julia-indent-offset) (kotlin-mode kotlin-tab-width)
	   (kotlin-ts-mode kotlin-ts-mode-indent-offset)
	   (latex-mode . editorconfig-set-indentation-latex-mode)
	   (lisp-mode . editorconfig-set-indentation-lisp-mode)
	   (livescript-mode livescript-tab-width)
	   (lua-mode lua-indent-level) (lua-ts-mode lua-ts-indent-offset)
	   (magik-mode magik-indent-level)
	   (magik-ts-mode magik-indent-level)
	   (matlab-mode matlab-indent-level) (meson-mode meson-indent-basic)
	   (mips-mode mips-tab-width) (mustache-mode mustache-basic-offset)
	   (nasm-mode nasm-basic-offset) (nginx-mode nginx-indent-level)
	   (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
	   (objc-mode c-basic-offset) (octave-mode octave-block-offset)
	   (perl-mode perl-indent-level) (php-mode c-basic-offset)
	   (php-ts-mode php-ts-mode-indent-offset)
	   (pike-mode c-basic-offset) (protobuf-mode c-basic-offset)
	   (ps-mode ps-mode-tab) (pug-mode pug-tab-width)
	   (puppet-mode puppet-indent-level)
	   (python-mode . editorconfig-set-indentation-python-mode)
	   (python-ts-mode . editorconfig-set-indentation-python-mode)
	   (rjsx-mode js-indent-level sgml-basic-offset)
	   (ruby-mode ruby-indent-level) (ruby-ts-mode ruby-indent-level)
	   (rust-mode rust-indent-offset)
	   (rust-ts-mode rust-indent-offset rust-ts-mode-indent-offset)
	   (rustic-mode rustic-indent-offset) (scala-mode scala-indent:step)
	   (scss-mode css-indent-offset) (sgml-mode sgml-basic-offset)
	   (sh-mode sh-basic-offset sh-indentation)
	   (slim-mode slim-indent-offset) (sml-mode sml-indent-level)
	   (svelte-mode svelte-basic-offset)
	   (swift-mode swift-mode:basic-offset)
	   (terra-mode terra-indent-level)
	   (tcl-mode tcl-indent-level tcl-continued-indent-level)
	   (toml-ts-mode toml-ts-mode-indent-offset)
	   (typescript-mode typescript-indent-level)
	   (typescript-ts-base-mode typescript-ts-mode-indent-offset)
	   (verilog-mode verilog-indent-level
		 verilog-indent-level-behavioral
		 verilog-indent-level-declaration verilog-indent-level-module
		 verilog-cexp-indent verilog-case-indent)
	   (web-mode (web-mode-indent-style lambda (size) 2)
		 web-mode-attr-indent-offset web-mode-attr-value-indent-offset
		 web-mode-code-indent-offset web-mode-css-indent-offset
		 web-mode-markup-indent-offset web-mode-sql-indent-offset
		 web-mode-block-padding web-mode-script-padding
		 web-mode-style-padding)
	   (yaml-mode yaml-indent-offset) (yaml-ts-mode yaml-indent-offset)
	   (zig-mode zig-indent-offset)))
 '(copilot-max-char 1000000)
 '(create-lockfiles t)
 '(custom-enabled-themes '(pleasant-greens))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
	'("42c671177193fa0b01db775c79a94fc5d5bfba70e2377a4a8bef024bb1bd0ba5"
	   "a17b44e2b78faeab4435e5d515619f9501ddd573c95a23875f8dfcbca6059f8c"
	   "5b6dbc107876575f4f54f91b9ec448fbd7906af083d3a13e45843fd9aa8e9ef2"
	   "2164fa756e7bf6e200d86cf5eaaadf8f63041e354139a106ce677180acab42a1"
	   "8bb0936189769d649025f7acbf07a353e76befb87a085631696d8002965acceb"
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
 '(desktop-path '("~" "~/.emacs.d/"))
 '(desktop-restore-frames nil)
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
 '(ediff-split-window-function 'split-window-horizontally t)
 '(editorconfig-indentation-alist
	'((ada-mode ada-indent) (ada-ts-mode ada-ts-mode-indent-offset)
	   (apache-mode apache-indent-level) (awk-mode c-basic-offset)
	   (bash-ts-mode sh-basic-offset sh-indentation)
	   (bpftrace-mode c-basic-offset) (c++-mode c-basic-offset)
	   (c++-ts-mode c-basic-offset c-ts-mode-indent-offset)
	   (c-mode c-basic-offset)
	   (c-ts-mode c-basic-offset c-ts-mode-indent-offset)
	   (cmake-mode cmake-tab-width)
	   (cmake-ts-mode cmake-tab-width cmake-ts-mode-indent-offset)
	   (coffee-mode coffee-tab-width) (cperl-mode cperl-indent-level)
	   (crystal-mode crystal-indent-level) (csharp-mode c-basic-offset)
	   (csharp-ts-mode c-basic-offset csharp-ts-mode-indent-offset)
	   (css-mode css-indent-offset) (css-ts-mode css-indent-offset)
	   (d-mode c-basic-offset) (elixir-ts-mode elixir-ts-indent-offset)
	   (emacs-lisp-mode lisp-indent-offset)
	   (enh-ruby-mode enh-ruby-indent-level)
	   (erlang-mode erlang-indent-level) (ess-mode ess-indent-offset)
	   (f90-mode f90-associate-indent f90-continuation-indent
		 f90-critical-indent f90-do-indent f90-if-indent
		 f90-program-indent f90-type-indent)
	   (feature-mode feature-indent-offset feature-indent-level)
	   (fsharp-mode fsharp-continuation-offset fsharp-indent-level
		 fsharp-indent-offset)
	   (gdscript-mode gdscript-indent-offset)
	   (go-ts-mode go-ts-mode-indent-offset) (gpr-mode gpr-indent)
	   (gpr-ts-mode gpr-ts-mode-indent-offset)
	   (graphql-mode graphql-indent-level)
	   (groovy-mode groovy-indent-offset)
	   (haskell-mode haskell-indent-spaces haskell-indent-offset
		 haskell-indentation-layout-offset
		 haskell-indentation-left-offset
		 haskell-indentation-starter-offset
		 haskell-indentation-where-post-offset
		 haskell-indentation-where-pre-offset shm-indent-spaces)
	   (haxor-mode haxor-tab-width) (hcl-mode hcl-indent-level)
	   (html-ts-mode html-ts-mode-indent-offset)
	   (idl-mode c-basic-offset) (jade-mode jade-tab-width)
	   (java-mode c-basic-offset)
	   (java-ts-mode c-basic-offset java-ts-mode-indent-offset)
	   (js-mode js-indent-level) (js-ts-mode js-indent-level)
	   (js-jsx-mode js-indent-level sgml-basic-offset)
	   (js2-mode js2-basic-offset)
	   (js2-jsx-mode js2-basic-offset sgml-basic-offset)
	   (js3-mode js3-indent-level) (json-mode js-indent-level)
	   (json-ts-mode json-ts-mode-indent-offset)
	   (jsonian-mode jsonian-default-indentation)
	   (julia-mode julia-indent-offset) (kotlin-mode kotlin-tab-width)
	   (kotlin-ts-mode kotlin-ts-mode-indent-offset)
	   (latex-mode . editorconfig-set-indentation-latex-mode)
	   (lisp-mode . editorconfig-set-indentation-lisp-mode)
	   (livescript-mode livescript-tab-width)
	   (lua-mode lua-indent-level) (lua-ts-mode lua-ts-indent-offset)
	   (magik-mode magik-indent-level)
	   (magik-ts-mode magik-indent-level)
	   (matlab-mode matlab-indent-level) (meson-mode meson-indent-basic)
	   (mips-mode mips-tab-width) (mustache-mode mustache-basic-offset)
	   (nasm-mode nasm-basic-offset) (nginx-mode nginx-indent-level)
	   (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
	   (objc-mode c-basic-offset) (octave-mode octave-block-offset)
	   (perl-mode perl-indent-level) (php-mode c-basic-offset)
	   (php-ts-mode php-ts-mode-indent-offset)
	   (pike-mode c-basic-offset) (protobuf-mode c-basic-offset)
	   (ps-mode ps-mode-tab) (pug-mode pug-tab-width)
	   (puppet-mode puppet-indent-level)
	   (python-mode . editorconfig-set-indentation-python-mode)
	   (python-ts-mode . editorconfig-set-indentation-python-mode)
	   (rjsx-mode js-indent-level sgml-basic-offset)
	   (ruby-mode ruby-indent-level) (ruby-ts-mode ruby-indent-level)
	   (rust-mode rust-indent-offset)
	   (rust-ts-mode rust-indent-offset rust-ts-mode-indent-offset)
	   (rustic-mode rustic-indent-offset) (scala-mode scala-indent:step)
	   (scss-mode css-indent-offset) (sgml-mode sgml-basic-offset)
	   (sh-mode sh-basic-offset sh-indentation)
	   (slim-mode slim-indent-offset) (sml-mode sml-indent-level)
	   (svelte-mode svelte-basic-offset)
	   (swift-mode swift-mode:basic-offset)
	   (terra-mode terra-indent-level)
	   (tcl-mode tcl-indent-level tcl-continued-indent-level)
	   (toml-ts-mode toml-ts-mode-indent-offset)
	   (typescript-mode typescript-indent-level)
	   (typescript-ts-base-mode typescript-ts-mode-indent-offset)
	   (verilog-mode verilog-indent-level
		 verilog-indent-level-behavioral
		 verilog-indent-level-declaration verilog-indent-level-module
		 verilog-cexp-indent verilog-case-indent)
	   (web-mode (web-mode-indent-style lambda (size) 2)
		 web-mode-attr-indent-offset web-mode-attr-value-indent-offset
		 web-mode-code-indent-offset web-mode-css-indent-offset
		 web-mode-markup-indent-offset web-mode-sql-indent-offset
		 web-mode-block-padding web-mode-script-padding
		 web-mode-style-padding)
	   (yaml-mode yaml-indent-offset) (yaml-ts-mode yaml-indent-offset)
	   (zig-mode zig-indent-offset)))
 '(erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))
 '(everlasting-scratch-mode t)
 '(exwm-replace t)
 '(fast-but-imprecise-scrolling t)
 '(fci-rule-color "#eee8d5")
 '(ffap-machine-p-known 'reject)
 '(fill-column 72)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-go-build-executable nil)
 '(flycheck-go-gofmt-executable "goimports")
 '(flycheck-go-version nil)
 '(flyspell-auto-correct-binding [67108923])
 '(focus-follows-mouse 'auto-raise)
 '(font-use-system-font nil)
 '(frame-background-mode 'light)
 '(frame-inhibit-implied-resize t)
 '(fringe-mode '(4 . 4) nil (fringe))
 '(global-company-mode t)
 '(global-display-fill-column-indicator-mode t)
 '(global-display-line-numbers-mode nil)
 '(global-ligature-mode t)
 '(global-prettify-symbols-mode t)
 '(global-text-scale-adjust-resizes-frames nil)
 '(global-visual-line-mode nil)
 '(go-command "/home/milan/.goenv/versions/1.23.2/bin/go")
 '(gofmt-command "goimports")
 '(helm-mode t)
 '(highlight-indent-guides-auto-enabled t)
 '(highlight-indent-guides-method 'character)
 '(history-length 700)
 '(horizontal-scroll-bar-mode nil)
 '(hyperbole-mode nil)
 '(ido-mode 'both nil (ido))
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message
	";; -*- lexical-binding: t -*-\12\12;; This buffer is for text that is not saved, and for Lisp\12;; evaluation.  To create a file, visit it with ‘C-x C-f’ and enter\12;; text in its buffer.\12")
 '(insert-directory-program "ls")
 '(jdee-db-active-breakpoint-face-colors (cons "#222228" "#819cd6"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#222228" "#5b94ab"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#222228" "#515462"))
 '(kubernetes-pods-display-completed t)
 '(line-number-mode t)
 '(lisp-indent-offset 2)
 '(load-prefer-newer t)
 '(ls-lisp-dirs-first t)
 '(lsp-headerline-breadcrumb-enable t)
 '(lsp-headerline-breadcrumb-icons-enable t)
 '(markdown-command "multimarkdown")
 '(menu-bar-mode nil)
 '(mouse-autoselect-window t)
 '(native-comp-always-compile t)
 '(native-comp-async-report-warnings-errors 'silent)
 '(notmuch-search-line-faces
	'(("unread" :foreground "#aeee00") ("flagged" :foreground "#0a9dff")
	   ("deleted" :foreground "#ff2c4b" :bold t)))
 '(objed-cursor-color "#e1c1ee")
 '(org-fold-core-style 'overlays)
 '(pdf-view-midnight-colors (cons "#c6c6c6" "#282b33"))
 '(pixel-scroll-precision-mode t)
 '(projectile-auto-discover t)
 '(projectile-project-search-path '("~/projects/"))
 '(recentf-auto-cleanup 'never)
 '(recentf-max-menu-items 99)
 '(recentf-max-saved-items 50)
 '(recentf-mode t)
 '(red "#ffffff")
 '(rustic-ansi-faces
	["#282b33" "#e1c1ee" "#5b94ab" "#cfcf9c" "#819cd6" "#a6c1e0" "#7289bc"
	  "#c6c6c6"])
 '(safe-local-variable-values
	'((git-commit-major-mode . git-commit-elisp-text-mode)
	   (eval and buffer-file-name
		 (not (eq major-mode 'package-recipe-mode))
		 (or (require 'package-recipe-mode nil t)
		   (let ((load-path (cons "../package-build" load-path)))
			 (require 'package-recipe-mode nil t)))
		 (package-recipe-mode))))
 '(save-place-limit 800)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(server-mode t)
 '(shell-command-prompt-show-cwd t)
 '(shell-dynamic-complete-functions
	'(comint-c-a-p-replace-by-expanded-history
	   shell-environment-variable-completion shell-command-completion
	   shell-c-a-p-replace-by-expanded-directory
	   pcomplete-completions-at-point shell-filename-completion
	   comint-filename-completion))
 '(show-paren-mode t)
 '(speedbar-default-position 'left)
 '(split-width-threshold 90)
 '(straight-use-package-by-default t)
 '(tab-width 4)
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
 '(ediff-current-diff-A ((t (:extend t :background "#00aadd"))))
 '(eglot-inlay-hint-face ((t (:inherit shadow :height 0.9))))
 '(font-lock-comment-face ((t (:foreground "forest green"))))
 '(font-lock-keyword-face ((t (:foreground "chartreuse" :slant oblique))))
 '(font-lock-type-face ((t (:foreground "gold"))))
 '(fringe ((t (:background "#001500"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "green4")))))
