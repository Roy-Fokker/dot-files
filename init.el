;;; init.el --- Init file for emacs      -*- lexical-binding: t; -*-
;;; Commentary:
;; This file bootstraps the configuration of Emacs

;;; Code:

;; - Garbage Collection setting -------------------------------------
;; Techniques borrowed from DOOM Emacs FAQ
(defvar *my/init-file-name-handler-alist* file-name-handler-alist
  "Save file name handler till startup is done.")

(defun my/defer-garbage-collection ()
  "Function to defer garbage collection."
  (setq gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 0.6))

(defun my/restore-garbage-collection ()
  "Function to restore garbage collection."
  (run-at-time 1 nil
               (lambda ()
		 (setq gc-cons-threshold (* 128 1024 1024)
		       gc-cons-percentage 0.1))))

(defun my/restore-file-name-handler ()
  "Restore file-name-handler list."
  (setq file-name-handler-alist *my/init-file-name-handler-alist*))

(defun my/restore-gc-and-file-handler ()
  "Call restore functions for both gc and file-handler."
  (my/restore-garbage-collection)
  (my/restore-file-name-handler)
  (garbage-collect))

(setq file-name-handler-alist nil)
(my/defer-garbage-collection)

(add-hook 'after-init-hook #'my/restore-gc-and-file-handler())
(add-hook 'minibuffer-setup-hook #'my/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'my/restore-garbage-collection)
(add-hook 'focus-out-hook #'garbage-collect)

;; - Basic Behaviour ------------------------------------------------
;; Disable GUI elements
(tool-bar-mode -1)                              ; Disable tool bar
(menu-bar-mode -1)                              ; Disable menu bar
(scroll-bar-mode -1)                            ; Hide scroll bar

;; Enable Common User Actions, like sane editor
(cua-mode t)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)

;; Window Movement
(windmove-default-keybindings)

;; Editor line behaviour
(global-display-line-numbers-mode)              ; Display line-numbers in all buffers
(global-hl-line-mode)                           ; Highlight current line
(show-paren-mode t)                             ; Parenthesis highlighting
(delete-selection-mode t)                       ; Make delete work as expected
(global-prettify-symbols-mode t)                ; prettify symbols (like lambda)

(defalias 'yes-or-no-p 'y-or-n-p)               ; Change yes/no prompt to y/n

;; Use utf-8 everywhere.
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

;; Set default font
(set-frame-font "Fira Code" nil t)
(set-face-attribute 'default nil)

;; Set editor values to preferences
(setq inhibit-startup-screen t                      ; Disable startup screen.
      visible-bell 1                                ; Disable audible beeps.
      tab-width 4                                   ; Set tab width to 4 spaces.
      backup-inhibited t                            ; Don't use file backups.
      cursor-in-non-selected-windows 'hollow        ; Don't show cursors in inactive window.
      make-pointer-invisible t                      ; Hide mouse when typing.
      fast-but-imprecise-scrolling nil              ; Not sure what this does??!
      jit-lock-defer-time 0                         ; don't wait for jit.
      select-enable-clipboard t                     ; integrate with system clipboard
      x-select-request-type '(UTF8_STRING           ; Treat clipboard input as utf8
			      COMPOUND_TEXT         ;   then other in list.
			      TEXT
			      STRING)
      mouse-yank-at-point t                         ; Paste at text-cursor, not mouse-cursor.
      scroll-preserve-screen-position t             ; Preserve line/column position.
      delete-old-versions -1                        ; Delete execess backup files
      backup-directory-alist `(("." .               ; where to put backup files
				(expand-file-name "backups"
						  user-emacs-directory)))
      vc-follow-symlinks t                          ; don't ask for confirmation when opening symlink file
      find-file-visit-truename t                    ; find true path of the file.
      inhibit-compacting-font-caches t              ; to speed up text rendering.
      )

(setq-default frame-title-format "%b %& emacs"                 ; Window Title => {Buffer Name} {Modified Status}
              delete-by-moving-to-trash t                      ; delete moves to recycle bin
              column-number-mode t                             ; display column number
              show-paren-delay 0                               ; show matching immediately
              scroll-conservatively  most-positive-fixnum      ; scroll sensibly, don't jump around
              mouse-wheel-scroll-amount '(1 ((shift) . 1))     ; one line at a time
              mouse-wheel-follow-mouse t                       ; scroll window under mouse
              find-file-visit-truename t                       ; find true path of a file
              custom-file (expand-file-name ".emacs-custom.el" ; save machine specific settings here
					    user-emacs-directory)
              indicate-empty-lines t                           ; Show empty lines
              truncate-lines t                                 ; disable word wrap
              default-tab-width 4                              ; Default tab width is also 4 spaces.
              help-window-select t                             ; focus on help when shown.
              savehist-save-minibuffer-history t               ; save minibuffer history.
              )

(load-theme 'tango-dark t)

;; Change window splitting behaviour.

(defun vsplit-other-window ()
  "Splits the window vertically and switch to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun hsplit-other-window ()
  "Splits the window horizontally and switch to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(global-set-key (kbd "C-x 2") 'hsplit-other-window) ; Change the bindings for vertical
(global-set-key (kbd "C-x 3") 'vsplit-other-window) ; and horizontal splits.

;; ------------------------------------------------------------------
;; This function will get full path of the special folder regardless
;; where it is on a Windows machine. It makes a call out to cmd/PS.

(defun get-windows-special-folder-path (FOLDER_NAME)
  "FOLDER_NAME is special folder name of interest."
  (car
   (process-lines "powershell"
                  "-NoProfile"
                  "-Command"
                  (concat "[Environment]::GetFolderPath(\""
			  FOLDER_NAME
			  "\")"))))

;; ------------------------------------------------------------------
;; Save and Load Frame Size and Location.
;; This faster than using desktop-save-mode. As it doesn't reload
;; all the file/buffers and modes.

(defun my/save-frame-geometry ()
  "Save Emacs frame geometry into a file to be loaded later."
  (let ((frame-left      (car (frame-position)))
        (frame-top       (cdr (frame-position)))
        (frame-width     (frame-width))
        (frame-height    (frame-height))
        (frame-info-file (expand-file-name "frame-geometry.el"
                                           user-emacs-directory)))

    (unless (number-or-marker-p frame-left)
      (setq frame-left 0))
    (unless (number-or-marker-p frame-top)
      (setq frame-top 0))
    (unless (number-or-marker-p frame-width)
      (setq frame-width 200))
    (unless (number-or-marker-p frame-height)
      (setq frame-height 65))

    (with-temp-buffer
      (insert
       ";; This is previous session's emacs frame geometry.\n"
       ";; Last generated: " (current-time-string) ".\n"
       (format "%S"
               `(setq initial-frame-alist
                      '((top     . ,frame-top)
                        (left    . ,frame-left)
                        (width   . ,frame-width)
                        (height  . ,frame-height)))))
      (when (file-writable-p frame-info-file)
        (write-file frame-info-file)
        (byte-compile-file frame-info-file)))
    ))

(defun my/load-frame-geometry ()
  "Load Emacs frame geometry into current session."
  (let ((frame-info-file (expand-file-name "frame-geometry.el"
                                           user-emacs-directory))
	(frame-info-elc (expand-file-name "frame-geometry.elc"
                                          user-emacs-directory)))
    (cond
     ((file-readable-p frame-info-elc)
      (load-file frame-info-elc))
     ((file-readable-p frame-info-file)
      (load-file frame-info-file)))
    ))

(if window-system
    (progn
      (add-hook 'kill-emacs-hook #'my/save-frame-geometry)
      (add-hook 'after-init-hook #'my/load-frame-geometry)))

;; ------------------------------------------------------------------
;; Load Custom file if it exists.
;; Loaded after emacs finishes initializing.
(defun my/load-custom-file ()
  (when (file-exists-p custom-file)
    (load custom-file)))

(if window-system
    (add-hook 'after-init-hook #'my/load-custom-file))

;; ------------------------------------------------------------------
;; Configure Package Archives
(require 'package) ; package management

;; Don't load any packages by default
(setq package-enable-at-startup nil)

;; Where to look for packages
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa"  . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; only initialize package.el if emacs less than 27
(when (< emacs-major-version 27)
  (package-initialize))

;; install use-package if it's not present
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

;; compile it
(require 'use-package)
(setq use-package-always-ensure t ; always download on first run
      use-package-always-defer  t ; always defer loading packages
      )

;; ------------------------------------------------------------------
;; Packages Used by Emacs

;; - Doom Theme -----------------------------------------------------
(use-package doom-themes
  :init
  (load-theme 'doom-one t))

;; - doom modeline --------------------------------------------------
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

;; - all the icons --------------------------------------------------
(use-package all-the-icons)

;; - ivy swiper and counsel -----------------------------------------
(use-package ivy
  :init
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode t))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  :bind
  (("C-x C-b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  :hook
  (after-init . ivy-mode))

(use-package swiper
  :after ivy
  :bind
  ("C-f" . swiper))

(use-package counsel
  :after ivy
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file))
  :hook
  (after-init . counsel-mode))

;; - Which Key ------------------------------------------------------
(use-package which-key
  :hook
  (after-init . which-key-mode))

;; - Magit ----------------------------------------------------------
(use-package magit
  :bind ("C-x g" . magit-status))

;; - Rainbow Brackets -----------------------------------------------
(use-package rainbow-delimiters
  :hook
  ((prog-mode
    text-mode
    lisp-interaction-mode-hook)
   . rainbow-delimiters-mode))

;; - Paredit --------------------------------------------------------
(use-package paredit
  :hook
  ((lisp-mode
    emacs-lisp-mode
    lisp-interaction-mode)
   . paredit-mode))

;; - Company --------------------------------------------------------
(use-package company
  :config
  (setq company-idle-delay 0
	company-tooltip-limit 20
	company-minimum-prefix-length 2
	company-selection-wrap-around t)
  :hook
  (after-init . global-company-mode))

;; - YA Snippets ----------------------------------------------------
(use-package yasnippet
  :commands yas-minor-mode
  :hook
  (prog-mode . yas-global-mode))

(use-package yasnippet-snippets)
(use-package common-lisp-snippets)

;; - Common Lisp ----------------------------------------------------
(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  (sly-contribs '(sly-fancy)))

(use-package sly-quicklisp
  :requires sly)

(use-package sly-asdf
  :requires sly)

;; ------------------------------------------------------------------
(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
