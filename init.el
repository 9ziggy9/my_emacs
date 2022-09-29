;; FULL ON TARD NOTES:
;; M-: to open eval buffer, I do not understand why I can't remember this
;; SELF-DISCOVERY
;; TO SEE LOG OF COMMANDS RAN: SPC-h-l

;; DISABLE STUPID SCROLLBARS AND SUCH
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tool-bar-mode -1) ;; Disable the toolbar
(set-fringe-mode 10) ;; Give some breathing room
(menu-bar-mode -1) ;; Disable the menu bar

;; stop the backup files damnit
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; will this work?
(auto-image-file-mode 1)

;; opacity
(set-frame-parameter (selected-frame) 'alpha '(97 . 96))
(add-to-list 'default-frame-alist '(alpha . (97 . 96)))

;; CENTRALIZE UGLY BACKUPS
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
(setq undo-tree-history-directory-alist
      `(("." . "~/.emacs.d/.undo-tree-history")))

;; set to system clipboard
(setq x-select-enable-clipboard t)

;; SETTING FONT
;; (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono-16:normal")
;; (set-face-attribute 'mode-line nil :font "Iosevka Nerd Font Mono-16:normal")
(defun my-frame-init ()
  (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono-16:normal")
  (set-face-attribute 'mode-line nil :font "Iosevka Nerd Font Mono-16:normal"))

(defun set-my-fira ()
  (interactive)
  (set-frame-attribute 'default nil '(:font "Fira Code Nerd Font Mono-20:normal")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(my-frame-init)))
  (my-frame-init))

;; AFTER FRAME HOOK
;; (add-hook 'after-make-frame-functions)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-\-") 'text-scale-decrease)
(global-set-key (kbd "C-<return>") 'make-frame)
(global-set-key (kbd "<print>") (lambda () (interactive) (zig/toggler "*scratch*")))
(global-set-key (kbd "C-<tab>") (lambda () (interactive) (zig/toggler "*eshell*")))
(global-set-key (kbd "H-<tab>") (lambda () (interactive) (zig/toggler "*eshell*")))

;; AUTOFOCUS/MAXIMIZE HELP WINDOWS
(add-to-list 'display-buffer-alist
	     '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist
	     '("*Help*" display-buffer-same-window))


;; PRETTIFY SYMBOLS
;; (global-prettify-symbols-mode +1)
;; (setq prettify-symbols-alist '(("lambda" . ?λ)))

;; AUTOPAIRS
(electric-pair-mode t)

;; LINE NUMBERS
(column-number-mode)
;; (setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
;; Hook for certain modes to disable line numbers
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Helpful mode hook for docs
;; (dolist (mode '(woman-mode-hook
;; 		help-mode-hook))
;;   (add-hook mode (lambda () (helpful-mode))))

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

;; SEARCH: find in current file! Bound to C-s
(use-package swiper)

;; AUCTeX
;; TODO: add auto safe, to get some auto refreshage
;; C-c C-e -- menu to add document elements
;; C-c C-c -- compile/view
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  :hook (LaTeX-mode .
    (lambda ()
      (push (list 'output-pdf "Evince")
	    TeX-view-program-selection))))

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

;; RICH IVY MODE
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; THEMING
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
  (load-theme 'doom-material t))

;; RAINBOW PARENS
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; PAREDIT FOR RACKET
(use-package paredit
             :ensure t
             :config
             (add-hook 'racket-mode-hook #'enable-paredit-mode))

;; Get expand selection in delimiters fast
(use-package expand-region)

;; Helpful for examining bindings on the fly
(use-package command-log-mode)

;; SELF-DISCOVERABILITY FEATURE, shows commands that follow
;; currently invoked binding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (define-key which-key-mode-map (kbd "C-x n") 'which-key-C-h-dispatch))

;; UNDO TREE
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; EVIL MODE
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-shift-width 2)
  (setq evil-jump-cross-buffers t)
  ;; comeback and set avy-window-all
  :config
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1))


;;yepperinthepepper

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (setq avy-all-windows 'all-frames)
  (setq avy-all-windows-alt t)
  (setq avy-background t)
  (evil-collection-init))

;; (use-package evil-easymotion
;;   :after evil
;;   :config
;;   (setq avy-))

(use-package evil-snipe
  :init (evil-snipe-mode +1))

;; HELPFUL DOCUMENTATION
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; FZF -- binding this to SPC f z in the future
;; more research needed to see how I can make workflow better match
;; my usage in console.
(use-package fzf)

(use-package evil-mc)
(global-evil-mc-mode 1)
(use-package evil-multiedit)
(evil-multiedit-mode 1)

;; ORG MODE
(defun zig/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))
(use-package org
  :hook (org-mode . zig/org-mode-setup)
  :config
  (setq org-ellipses " ▾"
	org-hide-emphasis-markers t))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org")
  :config
  (org-roam-setup))

;; YAS SNIPPETS
(use-package yasnippet
    :config
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
    (yas-global-mode 1))
;; Some premare snippets
(use-package yasnippet-snippets)

;; LANGUAGE SERVERS
;; NOTE TO SELF: remember, if project is accidentally blacklisted
;; simply run M-x lsp-workspace-blacklist-remove
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; C -- THIS IS MORE THAN ENOUGH FOR C!!!!
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda() (require 'ccls) (lsp))))

(add-hook 'lsp-mode-hook (lambda ()
			 (remove-hook 'before-save-hook #'lsp-format-buffer)))

;; JAVASCRIPT
;; better indent level
(setq-default js-indent-level 2)
;; Improved javascript mode; better for ES6
(use-package js2-mode
  :mode "\\.js\\'"
  :mode "\\.mjs\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))
;; for react/JSX stuff
(use-package rjsx-mode)

;; TYPESCRIPT -- eh, why not?
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; WEB MODE -- html/css/js
(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "focus/.*\\.html\\'")
          ("ctemplate" . "realtimecrm/.*\\.html\\'"))))
(use-package emmet-mode
  :after(web-mode css-mode scss-mode)
  :commands (emmet-mode emmet-expand-line yas/insert-snippet yas-insert-snippet company-complete)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (setq emmet-indentation 2)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  :bind
  ("C-j" . emmet-expand-line)
  ((:map emmet-mode-keymap
	  ("C-c [" . emmet-prev-edit-point)
	  ("C-c ]" . emmet-next-edit-point))))


;;beautify stuff, make sure beautify is installed with npm install -g js-beautify
;;probably won't use this, but it's here since bindings aren't taken
(use-package web-beautify
  :bind (:map web-mode-map
         ("C-c b" . web-beautify-html)
         :map js2-mode-map
         ("C-c b" . web-beautify-js)))

;; DEBUGGING
(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; HASKELL mode -- THERE BE DRAGONS HERE FOR ARCH USERS
(use-package haskell-mode)
;; stolen to make o work in haskell mode...
 (with-eval-after-load "haskell-mode"
    ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
    ;; indentation is done correctly. See
    ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))

    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))

    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)
  )

;; PYTHON
;; (use-package python-mode
;;   :ensure t
;;   :hook (python-mode . lsp-deferred)
;;   :custom
;;   ;; NOTE: Set these if Python 3 is called "python3" on your system!
;;   ;; (python-shell-interpreter "python3")
;;   ;; (dap-python-executable "python3")
;;   (dap-python-debugger 'debugpy)
;;   :config
;;   (require 'dap-python))

;; (use-package pyvenv
;;   :after python-mode
;;   :config
;;   (pyvenv-mode 1))

;; RACKET
(use-package racket-mode
             :ensure t)

;; GO
(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))
(provide 'gopls-config)

(add-hook 'go-mode-hook
    (lambda ()
      (setq-default)
      (setq tab-width 2)
      (setq standard-indent 2)
      (setq indent-tabs-mode nil)))

;; RUST
(use-package rust-mode
  :hook (rust-mode . lsp)
  :bind
  ("C-c g" . rust-run)
  ("C-c t" . rust-test)
  ("C-c b" . cargo-process-build)
  :init
  (which-function-mode 1)
  (setq compilation-error-regexp-alist-alist
      (cons '(cargo "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([0-9]+\\):\\([0-9]+\\) \\(?:[Ee]rror\\|\\([Ww]arning\\)\\):" 1 (2 . 4) (3 . 5) (6))
        compilation-error-regexp-alist-alist))
  :config
  (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :diminish racer-mode
  :hook (rust-mode . racer-mode)
  :bind
  ("M-j" . racer-find-definition)
  ;; (:map racer-mode-map ("M-." . #'xref-find-definitions))
  (:map racer-mode-map ("M-." . nil))
  )

;; AUTOCOMPLETION SYSTEM
(use-package company
  :ensure t
  :defer t
  :init 
  (global-company-mode)
  (setq company-idle-delay 0.25)
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (setq projectile-project-search-path '("~/source"))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile)

;; custom functions
;; evil write and close BUFFER
(evil-define-command zig/evil-write-and-kill-buffer (path)
  "Save and kill buffer."
  :repeat nil
  :move-point nil
  (interactive "<f>")
  (if (zerop (length path))
      (save-buffer)
    (write-file path))
  (kill-buffer (current-buffer)))
(evil-ex-define-cmd "wq" 'zig/evil-write-and-kill-buffer)

;; generic buffer toggler
(defun zig/toggler (string)
  (interactive)
  (if (string= string (buffer-name))
      (previous-buffer)
    "t" '(eshell :which-key '((lambda () (interactive) (zig/toggler "*eshell*"))))
    (switch-to-buffer string)))

;; KEYBINDINGS
;; general SPC buffer
(use-package general)
(general-create-definer my-leader-def
  :keymaps '(normal visual)
  :prefix "SPC")
(my-leader-def
    "RET" '(recenter-top-bottom :which-key "recenter on current line")
    "p" '(counsel-projectile-find-file :which-key "find projectile project")
    "g" '(counsel-projectile-ag :which-key "grep projectile project")
    "S" '(swiper-all :which-key "search all buffers")
    "s" '(swiper :which-key "search file")
    "c" 'compile
    "C" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) 
	  :which-key "config file")
    "ee" '(eval-buffer :which-key "evaluate buffer")
    "el" '(eval-last-sexp :which-key "evaluate to point")
    ":" '(counsel-M-x :which-key "M-x")
    "z" '(fzf :which-key "fuzzy find files")
    "f"  '(counsel-find-file :which-key "find file")
    "bb" '(counsel-switch-buffer :which-key "buffer list")
    "bl" '(mode-line-other-buffer :which-key "previous buffer")
    "bk" '(kill-buffer-and-window :which-key "kill/close buffer")
    "hl" '(view-lossage :which-key "command history")
    "hb" '(describe-bindings :which-key "describe all bindings")
    "hc" '(describe-command :which-key "describe command")
    "hk" '(describe-key :which-key "describe key")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "nf" '(org-roam-node-find :which-key "find/create a note (roam node)")
    "ni" '(org-roam-node-insert :which-key "insert reference to roam node")
    "SPC" '((lambda () 
	      (interactive)
	      (let ((current-prefix-arg nil))
		(evil-avy-goto-char-timer)))
			      :which-key "timer jump"))

;; window movement
(define-key evil-normal-state-map (kbd "<down>") 'windmove-down)
(define-key evil-normal-state-map (kbd "<up>") 'windmove-up)
(define-key evil-normal-state-map (kbd "<left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "<right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "C") 'comment-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-page-down)
(define-key evil-normal-state-map (kbd "D") 'er/expand-region)
(define-key evil-normal-state-map (kbd "S") 'er/contract-region)
(define-key evil-normal-state-map (kbd "-") 'evil-join)
(define-key evil-normal-state-map (kbd "M") 'woman)
(define-key evil-normal-state-map (kbd "E") 'flymake-show-buffer-diagnostics)
(define-key evil-normal-state-map (kbd "f") 'evil-snipe-f)
(define-key evil-normal-state-map (kbd "F") 'evil-snipe-F)
(define-key evil-normal-state-map (kbd "K") 'evil-previous-line-first-non-blank)
(define-key evil-normal-state-map (kbd "J") 'evil-next-line-first-non-blank)
(define-key evil-normal-state-map (kbd "<backspace>") 'pop-global-mark)
(define-key evil-normal-state-map (kbd "<tab>") 'mode-line-other-buffer)
(define-key evil-normal-state-map (kbd "M-<tab>") 'previous-buffer)

;; multi cursor
(define-key evil-normal-state-map (kbd "C-j") 'evil-mc-make-cursor-move-next-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-mc-undo-last-added-cursor)
(define-key evil-normal-state-map (kbd "C-l") 'evil-multiedit-match-and-next)
(define-key evil-normal-state-map (kbd "C-h") 'evil-multiedit-abort)
(define-key evil-normal-state-map (kbd "<escape>") #'(lambda () (interactive)
						     (evil-mc-undo-all-cursors)
						     (evil-multiedit-abort)))
;; commenting in visual select
(define-key evil-visual-state-map (kbd "C") 'comment-or-uncomment-region)

;; evaluate in shell
(define-key evil-visual-state-map (kbd "|") 'shell-command-on-region)

;; HYDRA BINDINGS
;; (use-package hydra)
;; (defhydra hydra-zoom (global-map "C-+")
;;   "zoom"
;;   ("o" text-scale-decrease "out")
;;   ("i" text-scale-increase "in"))

;;;;;;;; DO NOT TOUCH ;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes
   '("d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "84b14a0a41bb2728568d40c545280dbe7d6891221e7fbe7c2b1c54a3f5959289" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(preview-latex auctex racer flycheck-rust cargo rust-mode emmet-mode paredit racket-mode vterm origami-mode yafolding company-math counsel-projectile projectile evil-snipe org-roam org-bullets expand-region go-mode company typescript-mode dap-mode hindent haskell-mode web-beautify web-mode rjsx-mode fzf js2-mode yasnippet-snippets yasnippet pyvenv python-mode manpages manpges ccls lsp-mode evil-surround wrap-region evil-multiedit hydra evil-mc fixmee autopair multiple-cursors evil-easymotion helpful evil-collection evil general blackboard-theme kooten-theme all-the-icons ivy-rich which-key rainbow-delimiters green-is-the-new-black-theme green-phosphor-theme counsel swiper ivy command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
