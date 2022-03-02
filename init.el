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

;; INSTALL PACKAGES

;; for command logging, use clm/toggle-command-...
;; to open buffer which displays what commands are
;; actually associated with the keybindings you are using!
(use-package command-log-mode)

;; SEARCH: find in current file! Bound to C-s
(use-package swiper)

;; particularly useful for switching themes: M-x counsel themes
(use-package counsel)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(green-is-the-new-black-theme green-phosphor-theme counsel swiper ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
