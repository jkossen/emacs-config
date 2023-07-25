;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; enable use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; No tabs for indent
(setq indent-tabs-mode nil)
(setq tab-width 4)

;; backups
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.saves/")))

;; delete excess backup versions silently
(setq delete-old-versions t)
(setq kept-new-versions 16)
(setq kept-old-versions 4)

;; show column number in modeline
(setq column-number-mode t)

;; Enable line numbering by default
(global-display-line-numbers-mode t)

;; theme
(use-package dracula-theme
  :init
  (load-theme 'dracula t))
;; (use-package material-theme
;;   :init
;;   (load-theme 'material-light t))

;;; Completion framework
(use-package vertico
  :init
  ;; Enable completion by narrowing
  (vertico-mode t))

;;; LSP Support

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (vue-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package eglot
  :init
  ;; Enable LSP support by default in programming buffers
  (add-hook 'prog-mode-hook #'eglot-ensure))

(use-package company
  :init
  (global-company-mode))

;;; Pop-up completion, alternative to company?
;;(use-package corfu
  ;; Enable autocompletion by default in programming buffers
;;  :hook prog-mode
;;  )


;; Prettier formatting
(use-package prettier)

;;; Git client
(use-package magit
  :init
  ;; Bind the `magit-status' command to a convenient key.
  (global-set-key (kbd "C-c g") #'magit-status))

;; Git auto-commit mode
(use-package git-auto-commit-mode)

;;; Go Support
(use-package go-mode)

;;; PHP Support
(use-package php-mode)

;;; VUE Support
(use-package vue-mode)

;;; Typescript Support
(use-package typescript-mode)

;;; Markdown support
(use-package markdown-mode)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Custom keybindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-w") 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-c\C-r" 'reload-dotemacs)
(global-set-key (kbd "C-c C-d") 'duplicate-line)

(global-set-key "\C-c\ h" (lambda() (interactive)(find-file "~/SynologyDrive/Notities/01-Todo.md")))
(global-set-key "\C-c\ j" (lambda() (interactive)(find-file "~/SynologyDrive/Notities/00-Journal.md")))
(global-set-key "\C-c\ w" (lambda() (interactive)(find-file "~/work/org/journal.org")))


;; Setup fonts
(set-face-attribute 'bold nil :weight 'semi-bold)
(if (eql system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :font "Menlo" :height 160 :weight 'regular)
      (set-face-attribute 'fixed-pitch nil :font "Menlo" :height 160 :weight 'regular)
      (set-face-attribute 'variable-pitch nil :font "Arial" :height 160 :weight 'regular))
  (progn
    (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140 :weight 'regular)
    (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 140)
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)))

;; OrgMode customizations
(load-file "~/.config/emacs/org.el")

;;; Custom functions

;; Duplicate lines easily
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

;; Reload config
(defun reload-dotemacs ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.config/emacs/init.el") )


