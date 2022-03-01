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
(load-theme 'tango-dark)

;; PACKAGE MANAGEMENT
(require 'package)

(setq package-archive '(("melpa" . "https://melpa.org/packages/")
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
