;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; No comments.  Let's dive straight in.
;;

;;; Code:

;; no gc during startup
;; default 800000
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; Load custom file and keybinds separately
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(load-file "~/.emacs.d/keybinds.el")

;; Disable scroll bars
(defun disable-scroll-bars (frame)
  "Disable scrollbars in the given FRAME."
  (modify-frame-parameters frame '((vertical-scroll-bars . nil)
                                   (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'disable-scroll-bars)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; Straight.el Bootstrap for managing packages
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory))
      (bootstrap-version 7))
  (unless
      (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 (concat "https://raw.githubusercontent.com/radian-software/"
		 "straight.el/develop/install.el")
	 'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package with Straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Sane backup settings
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-backups/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Golden-ratio scrolling
(use-package golden-ratio-scroll-screen
  :bind (("M-<up>" . golden-ratio-scroll-screen-down)
         ("M-<down>" . golden-ratio-scroll-screen-up)))

;; Dired tweaks
;; hide details. show with (
(add-hook 'dired-mode-hook
	  (lambda ()
            (dired-hide-details-mode)
            ;; (dired-sort-toggle-or-edit)
	    ))

;; ...
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed random number generator
(random t)

(use-package svg)
;; svg weirdness
(add-to-list 'image-types 'svg)
;; overriding image.el function image-type-available-p
(defun image-type-available-p (type)
  "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (if (eq 'svg type)
      nil
    (and (fboundp 'init-image-library)
         (init-image-library type))))

;; Hippie expand configuration
(use-package hippie-exp
  :config
  (dolist (func '(try-expand-line try-expand-list
				  try-complete-file-name-partially
				  try-complete-file-name))
    (delete func hippie-expand-try-functions-list)))


;; transparency (wayland / pgkt)
(defun transparency (value)
  "Sets the transparency VALUE of the frame window.
VALUE from 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

;; mark-whole-buffer && indent-region
(defun indent-buffer ()
  "Indent buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; ANSI Colors in Compilation Buffers
(use-package ansi-color
  :hook (compilation-filter . ansi-color-apply-on-region))

;; Projectile for project management
(use-package projectile
 :config
 :bind-keymap ("C-c p" . projectile-command-map))

;;(add-hook 'project-find-functions #'project-projectile)


(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; bash completion for shells
(quelpa '(bash-completion :repo "szermatt/emacs-bash-completion" :fetcher github))

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)


;; Company mode (autocompletion)
(use-package company
  :init (global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1))

(use-package company-quickhelp)

(use-package wgrep)

(use-package web-beautify)

;; Deadgrep for fast search
(use-package deadgrep
  :commands deadgrep
  :config
  (defun deadgrep--include-args (rg-args)
    (push "--hidden" rg-args)
    (push "--glob=!.git/" rg-args))
  (advice-add 'deadgrep--arguments :filter-return
	      #'deadgrep--include-args))



(use-package go-mode)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(use-package project)
(use-package jsonrpc)
(use-package eglot)
(use-package templ-ts-mode)

;; ;; LSP Mode for language server protocol
;; (use-package lsp-mode
;;   :commands lsp
;;   :bind ("M-RET" . lsp-execute-code-action)
;;   :hook ((go-mode . lsp-deferred)
;; 	 ;;         (go-mode . lsp-go-install-save-hooks)
;;          (go-mode . yas-minor-mode)))

;; ;; Go-specific hooks
;; (defun lsp-go-install-save-hooks ()
;;   "Hooks for Go mode."
;;   (add-hook 'before-save-hook #'lsp-format-buffer nil t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports nil t))


;; (use-package lsp-ui
;;   :after lsp-mode
;;   :custom (lsp-ui-sideline-show-code-actions t))

;; ;; DAP Mode (Debug Adapter Protocol)
;; (use-package dap-mode
;;   :after lsp-mode
;;   :config (dap-auto-configure-mode))

;; Snippets
(use-package yasnippet
  :hook ((go-mode . yas-minor-mode)
         (k8s-mode . yas-minor-mode)))
(use-package yasnippet-snippets)

;; Vterm for better terminal integration (but I do use kitty)
(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (yas-minor-mode -1)
                        (flycheck-mode -1))))

;; Kitty terminal integration
(defun start-kitty-and-listen-on-unix-socket ()
  "Start Kitty Terminal listening on unix socket."
  (interactive)
  (start-process-shell-command
   "run-kitty" "*kitty*"
   (concat "pgrep -xf -- \"kitty -1 -o allow_remote_control=yes "
	   "--listen-on unix:/${XDG_RUNTIME_DIR}/kitty.sock\" "
	   "&> /dev/null || kitty -1 -o allow_remote_control=yes "
	   "--listen-on unix:/${XDG_RUNTIME_DIR}/kitty.sock"
	   )))


(defun send-current-line-to-kitty ()
  "Send the current line to Kitty terminal."
  (interactive)
  (let ((current-line (string-trim (thing-at-point 'line t))))
    (start-process-shell-command
     "send-to-kitty" nil
     (concat "echo " (shell-quote-argument current-line)
	     "| kitten @ --to unix:/run/user/1000/kitty.sock "
	     "send-text --stdin"))
    (forward-line)))

(require 'sh-script) ;; need sh-mode-map below
(define-key sh-mode-map (kbd "C-c C-c") 'send-current-line-to-kitty)

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (company-mode -1)
	    (display-fill-column-indicator-mode -1)))

(add-hook 'comint-mode-hook
	  (lambda ()
	    (company-mode -1)
	    (display-fill-column-indicator-mode -1)))

;; Eshell prompt
(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize "┌─[" 'face `(:foreground "green"))
	 (propertize
	  (user-login-name) 'face `(:foreground "forestgreen3"))
	 (propertize "@" 'face `(:foreground "green"))
	 (propertize
	  (system-name) 'face `(:foreground "forestgreen3"))
	 (propertize "]──[" 'face `(:foreground "green"))
	 (propertize
	  (format-time-string "%H:%M" (current-time))
	  'face `(:foreground "yellow"))
	 (propertize "]──[" 'face `(:foreground "green"))
	 (propertize
	  (concat (eshell/pwd)) 'face `(:foreground "lightblue"))
	 (propertize "]\n" 'face `(:foreground "green"))
	 (propertize "└─>" 'face `(:foreground "green"))
	 (propertize
	  (if (= (user-uid) 0) " # " " $ ")
	  'face `(:foreground "green")))))


;; etags / gtags
(use-package gtags-mode)
(use-package counsel-etags
  ;; :ensure t
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
			'counsel-etags-virtual-update-tags
			'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(use-package editorconfig
  :ensure t)

;; google gemini
;;(use-package google-gemini
;;  :ensure t)

;; github copilot
(use-package copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "M-RET")
			'copilot-accept-completion)

(define-key copilot-completion-map (kbd "M-<right>")
  'copilot-next-completion)

(define-key copilot-completion-map (kbd "M-<left>")
			'copilot-previous-completion)

;; Additional modes
(use-package web-mode)

;;;; sometimes I actually need this to think.. :-D
;;(use-package fireplace)

(use-package claudia
  :commands (claudia-mode))

(use-package uuid)

(defun create-scratch-buffer (&optional nomode)
  "Create a new scratch buffer and switch to it. If the region is active, then
 paste the contents of the region in the new buffer. The new buffer inherits
 the mode of the original buffer unless nomode is set.
 Return the buffer."
  (interactive "P")
  (let (bufname (mjmode  major-mode) (paste (and (region-active-p) (prog1 (buffer-substring (mark t) (point)) (deactivate-mark)))))
    (if (and (not nomode) (boundp 'ess-dialect) ess-dialect)
      (setq mjmode (intern-soft (concat ess-dialect "-mode"))))
    (setq bufname (generate-new-buffer-name "*scratch*"))
    (switch-to-buffer (get-buffer-create bufname))
    (if paste (insert paste))
    (if (and (not nomode) mjmode) (ignore-errors (funcall mjmode)))
    (get-buffer bufname)
    ))

(use-package polymode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

(use-package poly-markdown)
(use-package poly-org)

;;(use-package chatgpt-shell)
(use-package cov)
(use-package docker)
(use-package dockerfile-mode)

(use-package everlasting-scratch)
(use-package flycheck)
(use-package flyspell)
(use-package go-guru)
(use-package go-mode)
(use-package go-ts-mode)

(use-package just-mode)
(use-package magit)
(use-package protobuf-mode)
(use-package haskell-mode)
(use-package terraform-mode
  :custom (terraform-format-on-save t))

;;(use-package quelpa)
;;(use-package quelpa-use-package)

;;(unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))

(use-package rainbow-mode)
(use-package rust-mode)
(use-package rustic)
(use-package sudo-edit)
(use-package vlf)
(use-package xref)

(use-package yaml-mode)

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

;; tree-sitter
(use-package tree-sitter
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript
	 "https://github.com/tree-sitter/tree-sitter-javascript"
	 "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript"
	     "master" "tsx/src")
	(typescript
	 "https://github.com/tree-sitter/tree-sitter-typescript"
	 "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(templ "https://github.com/vrischmann/tree-sitter-templ")))

;; (use-package typescript-mode
;;   :after tree-sitter
;;   :config
;;   ;; we choose this instead of tsx-mode so that eglot can
;;   ;; automatically figure out language for server see
;;   ;; https://github.com/joaotavora/eglot/issues/624 and
;;   ;; https://github.com/joaotavora/eglot#handling-quirky-servers
;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "TypeScript TSX")

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist
;; 	       '("\\.tsx?\\'" . typescriptreact-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter
;;   ;; typescript parser use our derived mode to map both .tsx AND .ts
;;   ;; -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;; 	       '(typescriptreact-mode . tsx)))


(use-package tsi.el
  :after tree-sitter
  ;;;  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will
  ;; cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  ;;(add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1))))


(use-package highlight-indent-guides)

;; highlight diff
(use-package diff-hl
  :config
  :hook ((text-mode prog-mode vc-dir-mode) . turn-on-diff-hl-mode))

;; Some hooks :)
(add-hook 'prog-mode-hook (lambda ()
;;			    (display-line-numbers-mode t)
			    (display-fill-column-indicator-mode t)
;;                            (setf truncate-lines t)
                            (setq-local subword-mode t)
                            (yas-minor-mode t)
                            (flycheck-mode t)))

(add-hook 'go-mode-hook (lambda ()
			  (go-ts-mode)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;;Ligatures! yay!
(use-package ligature)

;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes
(ligature-set-ligatures
 'prog-mode
 '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-"
   "::" ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
   "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
   "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
   "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
   "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
   "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
   "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
   "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
   "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

;; an ancient crude hack to use emacs as pager in M-x shell buffers.
;; depends on ~/bin/emacs-pipe.rb
;;(quelpa '(emacs-pager :repo "mbriggs/emacs-pager" :fetcher github))
;;(add-to-list 'auto-mode-alist '("\\.emacs-pager$" . emacs-pager-mode))

(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
