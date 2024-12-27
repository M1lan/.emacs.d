;;; Keybinds.El --- Emacs keybindings configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;  note: If your window manager steals keys like M-Space
;;  (cycle-spacing), you're missing out.
;;
;;
;; Custom keybindings to enhance productivity and usability.

;;; Code:

;; restore frame
(global-set-key (kbd "s-<MonBrightnessUp>") 'reframe)

;; Use `ibuffer` instead of the default buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind `backward-kill-word` to `C-M-h` for convenience.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align code using regular expressions.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Use `hippie-expand` for versatile text expansion.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Toggle the menu bar (useful for discovering new modes).
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size adjustments.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Use regular expression searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file using `imenu`.
(global-set-key (kbd "C-x C-i") 'imenu)

;; I use pgup and pgdn for scrolling so these can be rebound:
(global-set-key (kbd "C-v") 'pop-to-mark-command)
(global-set-key (kbd "M-v") 'indent-buffer)

;; use
;; File finding enhancements.
(global-set-key (kbd "C-x M-f") 'find-file-other-window)
;; Ensure `find-file-in-project` is available or install the corresponding package.
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Window switching with `windmove`.
(windmove-default-keybindings) ;; Shift + Arrow keys to move betvveween windows.

;; Navigate windows in reverse.
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
;; Skip a window.
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))

;; Start or switch to `eshell`.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new `eshell` even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular `shell`.
(global-set-key (kbd "C-x M-m") 'shell)

;; Allow `M-x` without Meta (useful for certain keyboards or terminals).
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Search more than just commands with `apropos`.(global-set-key (kbd "C-h a") 'apropos)

;; You know, like Readline ;)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Evaluate and replace expressions anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; ripgrep is best grep is deadgrep.
(global-set-key (kbd "M-<XF86Search>") 'deadgrep)

;; For debugging Emacs modes.
;; Ensure `message-point` is defined or replace with `message` or appropriate function.
(global-set-key (kbd "C-c p") 'message-point)

;; Quick access to Magit status.
;; Ensure `magit` is installed.
(global-set-key (kbd "C-x g") 'magit-status)

;; Join the current line with the previous one.
(global-set-key (kbd "C-c q") 'join-line)

;; Fix up whitespace intelligently.
(global-set-key (kbd "S-<backspace>") 'fixup-whitespace)

(provide 'keybinds)
;;; keybinds.el ends here
