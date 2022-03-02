;; DISABLE STUPID SCROLLBARS AND SUCH
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1) ;; Disable the toolbar
(tooltip-mode -1) ;; Disable tooltips
(set-fringe-mode 10) ;; Give some breathing room
(menu-bar-mode -1) ;; Disable the menu bar

;; SETTING FONT
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 110)

;; THEMING
(load-theme 'green-is-the-new-black t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; LINE NUMBERS
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative) ;; may need :config block
;; Hook for certain modes to disable line numbers
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; HERE MAY BE DRAGONS: KEYBINDINGS
(use-package general)
(general-define-key
 "C-b" 'counsel-switch-buffer) ;; CHANGE BUFFER PROMPT OVERWRITES DEFAULTS

;; EVIL MODE
(use-package evil
  :init
  (setq evil-want-integratoin t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(evil general blackboard-theme kooten-theme all-the-icons ivy-rich which-key rainbow-delimiters green-is-the-new-black-theme green-phosphor-theme counsel swiper ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
