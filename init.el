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
      backup-directory-alist `(("." . "backups"))   ; where to put backup files
      vc-follow-symlinks t                          ; don't ask for confirmation when opening symlink file
      find-file-visit-truename t                    ; find true path of the file.
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
	      )

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

(global-set-key (kbd "C-x 2") 'hsplit-other-window)
(global-set-key (kbd "C-x 3") 'vsplit-other-window)

;; ------------------------------------------------------------------
;; Load Custom file if it exists.
;; Loaded after emacs finishes initializing.
(eval-when-compile
  (defun my/load-custom-file ()
    (when (file-exists-p custom-file)
      (load custom-file)))

  (if window-system
      (add-hook 'after-init-hook #'my/load-custom-file)))

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
  (require 'use-package)

  (setq use-package-always-ensure t ; always download on first run
	use-package-always-defer  t ; always defer loading packages
	))

;; - Delight or Diminish --------------------------------------------
(use-package diminish)
(use-package delight)

;; - Theme ----------------------------------------------------------
(use-package inkpot-theme
  :init
  (load-theme 'inkpot t))

;; - Which Key ------------------------------------------------------
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

;; - All the icons --------------------------------------------------
(use-package all-the-icons)

(use-package all-the-icons-ivy)

;; - Ivy ------------------------------------------------------------
(use-package ivy
  :diminish
  :init
  (setq-default ivy-initial-input-alist nil)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-height 20
	ivy-display-style 'fancy)
  (all-the-icons-ivy-setup)
  :hook (after-init . ivy-mode))

(use-package ivy-rich
  :hook (after-init . ivy-rich-mode))

(use-package ivy-hydra)

;; - Counsel --------------------------------------------------------
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

;; - Swiper ---------------------------------------------------------
(use-package swiper
  :bind (("C-s" . swiper)))

;; - WiNum ----------------------------------------------------------
(use-package winum
  :bind (("C-`" . winum-select-window-by-number)
	 ("M-1" . winum-select-window-1)
	 ("M-2" . winum-select-window-2)
	 ("M-3" . winum-select-window-3)
	 ("M-4" . winum-select-window-4)
	 ("M-5" . winum-select-window-5)
	 ("M-6" . winum-select-window-6)
	 ("M-7" . winum-select-window-7)
	 ("M-8" . winum-select-window-8))
  :hook (after-init . winum-mode))

;; - Rainbow Delimiters ---------------------------------------------
(use-package rainbow-delimiters
  :hook ((prog-mode
	  text-mode
	  lisp-interaction-mode
	  slime-repl-mode
	  cider-repl-mode
	  racket-repl-mode)
	 . rainbow-delimiters-mode))

;; - Treemacs -------------------------------------------------------
(use-package treemacs
  :config
  (setq treemacs-python-executable "python.exe")
  :bind (("M-0"       . treemacs-select-window)
	 ("C-x t 1"   . treemacs-delete-other-windows)
	 ("C-x t t"   . treemacs)
	 ("C-x t B"   . treemacs-bookmark)
	 ("C-x t f"   . treemacs-find-file)
	 ("C-x t M-f" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook (treemacs-mode . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;; - YA Snippets ----------------------------------------------------
(use-package yasnippet-snippets)

(use-package common-lisp-snippets)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :functions yas-reload-all
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (yas-reload-all))

(use-package ivy-yasnippet
  :bind ("C-c S" . ivy-yasnippet))

;; - Company --------------------------------------------------------
(use-package company
  :delight "(c) "
  :commands (company-complete-common company-dabbrev)
  :hook (after-init . global-company-mode)
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
         (not company-mode/enable-yas)
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :config
  (setq-default company-dabbrev-downcase nil
		company-dabbrev-ignore-case nil)
  (setq company-tooltip-limit 20
	company-idle-delay 0
	company-minimum-prefix-length 2
	company-selection-wrap-around t)
  (setq company-backends
	(mapcar #'company-mode/backend-with-yas company-backends))
  (company-tng-configure-default))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; - Flycheck -------------------------------------------------------
(use-package flycheck
  :delight "(f) "
  :hook (prog-mode . flycheck-mode))

;; - ParEdit --------------------------------------------------------
(use-package paredit
  :delight "(p) "
  :hook ((prog-mode
	  lisp-interaction-mode
	  slime-repl-mode
	  cider-repl-mode
	  racket-repl-mode)
	 . paredit-mode))

;; - Magit ----------------------------------------------------------
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("<f4>"  . magit-status)))

;; - El Doc ---------------------------------------------------------
(use-package eldoc
  :diminish)

;; - Org Mode -------------------------------------------------------
(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :mode (("\\.org$" . org-mode))
  :config
  (setq-default org-support-shift-select t
		org-use-sub-superscripts '{}
		org-export-with-sub-superscripts '{}))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package htmlize)

;; - Doom Modeline --------------------------------------------------

;; - Flyspell -------------------------------------------------------

;; - Common Lisp ----------------------------------------------------
(use-package slime-company)

(use-package slime
  :config
  (setq slime-lisp-implementations '((sbcl ("sbcl")))
	slime-default-lisp 'sbcl)
  (slime-setup '(slime-fancy
		 slime-company
		 slime-quicklisp
		 slime-asdf
		 )))
;; - Racket ---------------------------------------------------------
(use-package racket-mode
  :mode ("\\.rkt[dl]?\\'" . racket-mode))

;; - Clojure --------------------------------------------------------
(use-package clojure-mode-extra-font-locking)

(use-package clojure-mode)

(use-package cider)

;; - CMake ----------------------------------------------------------
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'"
	 "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;; - Markdown -------------------------------------------------------
(use-package markdown-mode
  :delight "ϻ "
  :mode ("\\.markdown\\'"
	 "\\.md\\'"))

;; - Powershell -----------------------------------------------------

;; - Language Server Protocol Mode ----------------------------------

;; - Python ---------------------------------------------------------

;; - IRC ------------------------------------------------------------

;; ------------------------------------------------------------------
(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
