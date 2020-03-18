;;; init.el --- Init file for emacs -*- lexical-binding: t -*-
;;; Commentary:
;; This file bootstraps the configuration of Emacs

;;; Code:

;; ------------------------------------------------------------------
;; Change GC threshold for duration of init, technique from DOOM Emacs FAQ
(setq gc-cons-threshold (* 2 1000 1000)     ; set gc threshold to 2GiB
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist) ; file handler check is not needed during start up
(setq file-name-handler-alist nil)

(defun startup/reset-gc-and-file-handler ()
  "Reset the gc and file handler values."
  (setq gc-cons-threshold (* 1 1000 1000)  ; set gc threshold to 0.5GiB
        gc-cons-percentage 0.1
        file-name-handler-alist startup/file-name-handler-alist)
  (garbage-collect)
  t)

;; ------------------------------------------------------------------
;; Reset changed values to defaults
;; values taken from DOOM-Emacs FAQ
(add-hook 'after-init-hook 'startup/reset-gc-and-file-handler)

;; Tell emacs to garbage collect on focus lost
(add-hook 'focus-out-hook #'garbage-collect)

;; ------------------------------------------------------------------
;; Set defaults for emacs variables
(setq-default inhibit-startup-screen t                        ; Disable Emacs Welcome Screen
			  backup-directory-alist `(("." . "backups"))     ; backup files in this directory
			  custom-file (expand-file-name ".emacs-custom.el"; save all machine specific settings here
											user-emacs-directory)
			  auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ; transform backup file names
			  package-enable-at-startup nil                   ; do not load packages before start up
			  delete-by-moving-to-trash t                     ; delete moves to recycle bin
			  column-number-mode t                            ; display column number
			  show-paren-delay 0                              ; show matching immediately
			  scroll-conservatively  most-positive-fixnum     ; scroll sensibly, don't jump around
			  mouse-wheel-scroll-amount '(1 ((shift) . 1))    ; one line at a time
			  mouse-wheel-follow-mouse t                      ; scroll window under mouse
			  find-file-visit-truename t                      ; find true path of a file
			  tab-width 4                                     ; force tab to be 4 spaces
			  w32-get-true-file-attributes nil                ; wonder if this helps with freezing
			  help-window-select t                            ; Focus on new help windows when opened
			  use-package-always-ensure t                     ; Tell use-package to always download missing packages
			  )

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
(fset 'yes-or-no-p 'y-or-n-p)       ; Change yes/no prompt to y/n

;; set some keybindings
(global-set-key (kbd "<C-tab>") 'switch-to-next-buffer)
(global-set-key (kbd "<C-S-tab>") 'switch-to-prev-buffer)

;; ------------------------------------------------------------------
;; Set utf-8 as default text system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default buffer-file-coding-system 'utf-8
              default-buffer-file-coding-system 'utf-8)
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))

;; ------------------------------------------------------------------
;; set font
(setq inhibit-compacting-font-caches t)
(set-face-attribute 'default nil :font "Cascadia Code")
(set-fontset-font t 'latin "Noto Sans")
(set-face-attribute 'mode-line nil :family "Segoe UI Symbol" :height 100)
(set-face-attribute 'mode-line-inactive nil :family "Segoe UI Symbol" :height 100)

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

;; -----------------------------------------------------------------
;; Helper functions to reload init.el when modified
(eval-when-compile
  ;; Open init file for emacs
  (defun emacs/open-init-file ()
	(interactive)
	(find-file user-init-file))

  ;; Reload init file
  (defun emacs/reload-init-file ()
	(interactive)
	(load user-init-file))

  ;; Set some global key bindings to invoke above two functions
  (global-set-key [(control f2)] 'emacs/open-init-file)
  (global-set-key [(control f5)] 'emacs/reload-init-file))

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




(use-package ivy
  :init
  (setq-default ivy-initial-input-alist nil)
  (setq ivy-use-virtual-buffers t
  :hook (after-init . ivy-mode))


(use-package ivy-rich
  :hook (after-init . ivy-rich-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)

(use-package swiper
  :bind (("C-s" . swiper)))

;; - WiNum ----------------------------------------------------------
(use-package winum
  :bind (("C-`" . winum-select-window-by-number)
  :hook (after-init . winum-mode))

  :config
(use-package company
  :config

(use-package company-quickhelp

;; - Flycheck -------------------------------------------------------
(use-package flycheck

;; - ParEdit --------------------------------------------------------
(use-package paredit
  :delight "(p) "


;; - All the icons --------------------------------------------------

;; - Doom Modeline --------------------------------------------------

;; - Treemacs -------------------------------------------------------

;; - Flyspell -------------------------------------------------------

;; - El Doc ---------------------------------------------------------

;; - Org Mode -------------------------------------------------------

;; - Common Lisp ----------------------------------------------------
;; ---- Implementations ---------------------------------------------
(defvar my/default-lisp nil "Default Lisp implementation to use.")
(setq my/default-lisp 'sbcl)

;; - Racket ---------------------------------------------------------
;; ---- Implementations ---------------------------------------------
(defvar my/default-scheme nil "Default Scheme implementation to use.")
(setq my/default-scheme '(racket))

;; - CMake ----------------------------------------------------------

;; - Markdown -------------------------------------------------------

;; - Powershell -----------------------------------------------------

;; - Language Server Protocol Mode ----------------------------------

;; - Python ---------------------------------------------------------

;; - IRC ------------------------------------------------------------


;; ------------------------------------------------------------------
(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
