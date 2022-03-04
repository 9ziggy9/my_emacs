(load "./custom.el")

;; DISABLE STUPID SCROLLBARS AND SUCH
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1) ;; Disable the toolbar
(set-fringe-mode 10) ;; Give some breathing room
(menu-bar-mode -1) ;; Disable the menu bar

;; CENTRALIZE UGLY BACKUPS
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; SETTING FONT
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 110)

;; THEMING
(load-theme 'misterioso t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; AUTOFOCUS/MAXIMIZE HELP WINDOWS
(add-to-list 'display-buffer-alist
	     '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist
	     '("*Help*" display-buffer-same-window))

;; PRETTIFY SYMBOLS
(global-prettify-symbols-mode +1)
(setq prettify-symbols-alist '(("lambda" . ?λ)
				("->" . ?→)
				("=>" . ?⇒)
				("/=" . ?≠)
				("<=" . ?≤)
				(">=" . ?≥)))

;; AUTOPAIRS
(electric-pair-mode t)

;; LINE NUMBERS
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
;; Hook for certain modes to disable line numbers
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; PACKAGE MANAGEMENT
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			("org" . "https://orgmode.org/elpa/")
			("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Grab use-package if not on platform
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; INSTALL PACKAGES
;; for command logging, use clm/toggle-command-...
;; to open buffer which displays what commands are
;; actually associated with the keybindings you are using!
(use-package command-log-mode)

;; SEARCH: find in current file! Bound to C-s
(use-package swiper)

;; particularly useful for switching themes: M-x counsel themes
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; completion framework in find files, etc, can also try
;; helm, ivy is more minimalistic.
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; rich mode for ivy (description of commands)
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; rainbow parens/brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; SELF-DISCOVERABILITY FEATURE, shows commands that follow
;; currently invoked binding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (define-key which-key-mode-map (kbd "C-x n") 'which-key-C-h-dispatch))

;; UNDO TREE
(use-package undo-tree)
(global-undo-tree-mode)

;; EVIL MODE
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-easymotion)

;; HELPFUL DOCUMENTATION
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-callable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; TOD0 TOOL
(use-package fixmee)
(use-package button-lock)
(global-fixmee-mode 1)

;; TODOOOO: MULTI-CURSOR
(use-package evil-mc)
(evil-mc-mode 1)
(use-package evil-multiedit)
(evil-multiedit-mode 1)

;; KEYBINDINGS
;; general SPC buffer
(use-package general)
(general-create-definer my-leader-def
  :keymaps '(normal visual)
  :prefix "SPC")
(my-leader-def
    "p" '(custom/scratch-toggle :which-key "scratchpad")
    "T" '(fixmee-view-listing :which-key "todo list")
    "t" '(term :which-key "terminal")
    "s" '(swiper :which-key "search file")
    "c" 'compile
    "ee" '(eval-buffer :which-key "evaluate buffer")
    "el" '(eval-last-sexp :which-key "evaluate to point")
    ":" '(counsel-M-x :which-key "M-x")
    "n" '(custom/org-browse :which-key "notes")
    "f"  '(counsel-find-file :which-key "find file")
    "bb" '(counsel-switch-buffer :which-key "buffer list")
    "bl" '(mode-line-other-buffer :which-key "previous buffer")
    "bk" '(kill-buffer-and-window :which-key "kill/close buffer")
    "hl" '(view-lossage :which-key "command history")
    "hc" '(describe-command :which-key "describe command")
    "hk" '(describe-key :which-key "describe key")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "SPC" '(closure (t) (&rest _)
		    (interactive)
		    (let ((current-prefix-arg t))
			(evil-avy-goto-char-timer))))

;; window movement
(define-key evil-normal-state-map (kbd "<down>") 'windmove-down)
(define-key evil-normal-state-map (kbd "<up>") 'windmove-up)
(define-key evil-normal-state-map (kbd "<left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "<right>") 'windmove-right)

;; multi cursor
(define-key evil-normal-state-map (kbd "C-j") 'evil-mc-make-cursor-move-next-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-mc-undo-last-added-cursor)
(define-key evil-normal-state-map (kbd "C-l") 'evil-multiedit-match-and-next)
(define-key evil-normal-state-map (kbd "C-h") 'evil-multiedit-abort)
(define-key evil-normal-state-map (kbd "<escape>") '(lambda () (interactive)
						     (evil-mc-undo-all-cursors)
						     (evil-multiedit-abort)))
;; commenting in visual select
(define-key evil-visual-state-map (kbd "g") 'comment-or-uncomment-region)

;; HYDRA BINDINGS
(use-package hydra)
(defhydra hydra-zoom (global-map "C-+")
  "zoom"
  ("o" text-scale-decrease "out")
  ("i" text-scale-increase "in"))

;; LANGUAGE SERVERS
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; C
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda() (require 'ccls) (lsp))))

;;;;;;;; DO NOT TOUCH ;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(ccls lsp-mode evil-surround wrap-region evil-multiedit hydra evil-mc fixmee autopair multiple-cursors evil-easymotion helpful evil-collection evil general blackboard-theme kooten-theme all-the-icons ivy-rich which-key rainbow-delimiters green-is-the-new-black-theme green-phosphor-theme counsel swiper ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
