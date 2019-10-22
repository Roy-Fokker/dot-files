;;; init.el --- Init file for emacs -*- lexical-binding: t -*-
;;; Commentary:
;; This file bootstraps the configuration of Emacs

;;; Code:

;; ------------------------------------------------------------------
;; Change GC threshold for duration of init, technique from DOOM Emacs FAQ
(setq gc-cons-threshold 2147483648               ; set gc threshold to 2GiB
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist) ; file handler check is not needed during start up
(setq file-name-handler-alist nil)

(defun startup/reset-gc-and-file-handler ()
  "Reset the gc and file handler values."
  (setq gc-cons-threshold 536870912  ; set gc threshold to 0.5GiB
        gc-cons-percentage 0.1
        file-name-handler-alist startup/file-name-handler-alist))

;; ------------------------------------------------------------------
;; Reset changed values to defaults
;; values taken from DOOM-Emacs FAQ
(add-hook 'after-init-hook 'startup/reset-gc-and-file-handler)

;; ------------------------------------------------------------------
;; Set defaults for emacs variables
(setq-default inhibit-startup-screen t                        ; Disable Emacs Welcome Screen
			  backup-directory-alist `(("." . "backups"))     ; backup files in this directory
			  custom-file "~/.emacs.d/.emacs-custom.el"       ; save all machine specific settings here
			  auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ; transform backup file names
			  package-enable-at-startup nil                   ; do not load packages before start up
			  delete-by-moving-to-trash t                     ; delete moves to recycle bin
			  column-number-mode t                            ; display column number
			  show-paren-delay 0                              ; show matching immediately
			  scroll-conservatively  most-positive-fixnum     ; scroll sensibly, don't jump around
			  mouse-wheel-scroll-amount '(1 ((shift) . 1))    ; one line at a time
			  mouse-wheel-follow-mouse t                      ; scroll window under mouse
			  find-file-visit-truename t                      ; find true path of a file
			  tab-width 4
			  )

;; Below is required to run emacs if installed via Msys2
;; (setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3" ; fix for bug
;; 			  )

;; ------------------------------------------------------------------
;; enable some convinence behaviours
(global-display-line-numbers-mode)  ; Display line-numbers in all buffers
(global-hl-line-mode)               ; Highlight current line
(menu-bar-mode -1)                  ; Hide menu bar
(tool-bar-mode -1)                  ; Hide tool bar
(scroll-bar-mode -1)                ; Hide scroll bar
(show-paren-mode t)                 ; Parenthesis highlighting
(delete-selection-mode t)           ; Enable delete selection mode
(cua-mode t)                        ; Enable CUA mode

;; set some keybindings
(global-set-key (kbd "<C-tab>") 'switch-to-next-buffer)
(global-set-key (kbd "<C-S-tab>") 'switch-to-prev-buffer)

;; ------------------------------------------------------------------
;; Set utf-8 as default text system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix
              default-buffer-file-coding-system 'utf-8-unix)
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))

;; ------------------------------------------------------------------
;; set font
(set-frame-font "Source Code Pro 10")
(setq inhibit-compacting-font-caches t)

;; ------------------------------------------------------------------
;; Load custom file if it exist
(eval-when-compile
  (defun emacs/load-custom-file ()
    (when (file-exists-p custom-file)
      (load custom-file)))
  
  (if window-system
      (add-hook 'after-init-hook 'emacs/load-custom-file)))

;; ------------------------------------------------------------------
;; All the logic to save and load frame size and location
(eval-when-compile
  ;; Save Frame size and location
  (defun emacs/save-framegeometry ()
    "Get the current frame's geometry and saves to ~/.emacs.d/framegeometry."
    (let ((frame-left      (frame-parameter (selected-frame) 'left))
          (frame-top       (frame-parameter (selected-frame) 'top))
          (frame-width     (frame-parameter (selected-frame) 'width))
          (frame-height    (frame-parameter (selected-frame) 'height))
          (frame-size-file (expand-file-name "~/.emacs.d/framegeometry.el")))

      (when (not (number-or-marker-p frame-left))
	(setq frame-left 0))
      (when (not (number-or-marker-p frame-top))
	(setq frame-top 0))
      (when (not (number-or-marker-p frame-width))
	(setq frame-width 800))
      (when (not (number-or-marker-p frame-height))
	(setq frame-height 600))

      (with-temp-buffer
	(insert
	 ";; This is the previous emacs frame's geometry.\n"
	 ";; Last generated " (current-time-string) ".\n"
	 "(setq initial-frame-alist\n"
	 "      '(" (format "(top . %d)\n" (max frame-top 0))
	 (format "        (left . %d)\n" (max frame-left 0))
	 (format "        (width . %d)\n" (max frame-width 0))
	 (format "        (height . %d)))\n" (max frame-height 0)))
	(when (file-writable-p frame-size-file)
          (write-file frame-size-file)))))

  ;; Load Frame Geometry
  (defun emacs/load-framegeometry ()
    "Loads ~/.emacs.d/framegeometry.el which should load the previous frame's geometry."
    (let ((frame-save-file (expand-file-name "~/.emacs.d/framegeometry.el")))
      (when (file-readable-p frame-save-file)
	(load-file frame-save-file))))

  ;; Hook into emacs kill and init
  (if window-system
      (progn
        (add-hook 'kill-emacs-hook 'emacs/save-framegeometry)
        (add-hook 'after-init-hook 'emacs/load-framegeometry)
	)))

;; ------------------------------------------------------------------
;; Configure Package Archives
(require 'package) ; package management

;; Don't load any packages by default
(setq package-enable-at-startup nil)

;; Where to look for packages
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa"  . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize) ;; initialize package.el

;; install use-package if it's not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; compile it
(eval-when-compile
  (require 'use-package))

;; - Dracula Theme --------------------------------------------------
(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

;; - All the icons --------------------------------------------------
(use-package all-the-icons
  :ensure t)

;; - Doom Modeline --------------------------------------------------
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t)
  :hook (after-init . doom-modeline-mode))

;; - Smex Ivy Counsel Swiper Hydra ----------------------------------

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-initial-input-alist nil
		ivy-re-builders-alist '((t . ivy--regex-fuzzy))
		ivy-height 20)
  :hook (after-init . ivy-mode))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(use-package ivy-rich
  :ensure t
  :hook (after-init . ivy-rich-mode))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
		 ("C-x C-f" . counsel-find-file)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package ivy-hydra
  :ensure t)

;; - Which Key ------------------------------------------------------
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

;; - Rainbow Delimiters ---------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode             . rainbow-delimiters-mode)
		 (lisp-interaction-mode . rainbow-delimiters-mode)
		 (slime-repl-mode       . rainbow-delimiters-mode)
		 (geiser-repl-mode      . rainbow-delimiters-mode)))

;; - WiNum ----------------------------------------------------------
(use-package winum
  :ensure t
  :bind (("C-`" . winum-select-window-by-number)
		 ("M-0" . winum-select-window-0-or-10)
		 ("M-1" . winum-select-window-1)
		 ("M-2" . winum-select-window-2)
		 ("M-3" . winum-select-window-3)
		 ("M-4" . winum-select-window-4)
		 ("M-5" . winum-select-window-5)
		 ("M-6" . winum-select-window-6)
		 ("M-7" . winum-select-window-7)
		 ("M-8" . winum-select-window-8))
  :hook (after-init . winum-mode))

;; - Company Mode ---------------------------------------------------
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
		company-minimum-prefix-length 2
		company-selection-wrap-around t)
  (company-tng-configure-default)
  :hook (after-init . global-company-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;; - Flycheck -------------------------------------------------------
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-popup-tip
  :ensure t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; - YA Snippets ----------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (use-package common-lisp-snippets
    :ensure t)
  (yas-reload-all)
  :hook (after-init . yas-global-mode))

;; - ParEdit --------------------------------------------------------
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode       . paredit-mode)
		 (lisp-mode             . paredit-mode)
		 (lisp-interaction-mode . paredit-mode)
		 (scheme-mode           . paredit-mode)
		 (slime-repl-mode       . paredit-mode)
		 (geiser-repl-mode      . paredit-mode)
		 (prog-mode             . paredit-mode)))

;; - El Doc ---------------------------------------------------------
(use-package eldoc
  :ensure t
  :hook ((emacs-lisp-mode       . turn-on-eldoc-mode)
		 (lisp-mode             . turn-on-eldoc-mode)
		 (lisp-interaction-mode . turn-on-eldoc-mode)
		 (scheme-mode           . turn-on-eldoc-mode)
		 (slime-repl-mode       . turn-on-eldoc-mode)
		 (geiser-repl-mode      . turn-on-eldoc-mode)
		 (prog-mode             . turn-on-eldoc-mode)))

(use-package eldoc-overlay
  :ensure t
  :hook (after-init . global-eldoc-overlay-mode))

;; - Org Mode -------------------------------------------------------
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (setq org-support-shift-select t
		org-use-sub-superscripts '{}
		org-export-with-sub-superscripts '{}))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package htmlize
  :ensure t)

;; - Magit ----------------------------------------------------------
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
;; :config
;; (setq magit-git-global-arguments
;; 	  (nconc magit-git-global-arguments
;; 			 '("-c" "color.ui=false"
;; 			   "-c" "color.diff=false")))

;; - Common Lisp ----------------------------------------------------
;; ---- Implementations ---------------------------------------------
(defvar my/lisp-implementations nil "List of default Lisp implementations.")
(defvar my/default-lisp nil "Default Lisp implementation to use.")
(setq my/lisp-implementations                     ; Which Common Lisp are installed
	  '((sbcl ("sbcl"))
		(ccl ("ccl"))
		(roswell ("ros" "run")))
	  my/default-lisp (if (executable-find "ros") ; Find one to use as default
						  'roswell
						'sbcl))

;; ---- Slime -------------------------------------------------------
(use-package slime
  :ensure slime-company
  :config
  (setq slime-lisp-implementations my/lisp-implementations
		slime-default-lisp my/default-lisp)
  (slime-setup '(slime-fancy slime-company slime-quicklisp slime-asdf)))

;; - Racket ---------------------------------------------------------
;; ---- Implementations ---------------------------------------------
(defvar my/default-scheme nil "Default Scheme implementation to use.")
(setq my/default-scheme '(racket))

;; ---- Geiser ------------------------------------------------------
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations my/default-scheme))

;; - CMake ----------------------------------------------------------
(use-package cmake-mode
  :ensure cmake-font-lock
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
						(add-to-list 'company-backends 'company-cmake))))

;; - Markdown -------------------------------------------------------
(use-package markdown-mode
  :ensure markdown-mode+
  :mode (".markdown" ".md"))

;; - Language Server Protocol Mode ----------------------------------
;; (use-package lsp-mode
;;   :ensure t
;;  )

;; ------------------------------------------------------------------
(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
