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

;; ------------------------------------------------------------------
;; Configure Package Archives
(require 'package) ; package management

;; Don't load any packages by default
(setq package-enable-at-startup nil)

;; Disable key checks due to elpa keys not being up to date
(setq package-check-signature nil)

;; Where to look for packages
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa"  . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; only initialize package.el if emacs less than 27
(when (< emacs-major-version 27)
  (package-initialize))

;; install use-package if it's not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; compile it
(eval-and-compile
  (require 'use-package))

(setq use-package-always-ensure t   ; always download on first run
      use-package-always-defer  nil ; don't defer loading packages causes issues with :custom
      )

;; = Emacs configuration ============================================
;; - set editor properties ------------------------------------------
(use-package emacs
  :ensure nil
  :custom
  ((inhibit-startup-screen t)                            ; Disable startup screen.
   (visible-bell t)                                      ; Disable audible beeps.
   (tab-width 4)                                         ; Set tab width to 4 spaces.
   (backup-inhibited t)                                  ; Don't use file backups.
   (cursor-in-non-selected-windows 'hollow)              ; Don't show cursors in inactive window.
   (make-pointer-invisible t)                            ; Hide mouse when typing.
   (fast-but-imprecise-scrolling nil)                    ; Not sure what this does??!
   (jit-lock-defer-time 0)                               ; don't wait for jit.
   (select-enable-clipboard t)                           ; integrate with system clipboard
   (x-select-request-type '(UTF8_STRING                  ; Treat clipboard input as utf8
                            COMPOUND_TEXT                ;   then other in list.
                            TEXT
                            STRING))
   (mouse-yank-at-point t)                               ; Paste at text-cursor, not mouse-cursor.
   (scroll-preserve-screen-position t)                   ; Preserve line/column position.
   (delete-old-versions -1)                              ; Delete execess backup files
   (backup-directory-alist `(("." .                      ; where to put backup files
                              (expand-file-name "backups"
                                                user-emacs-directory))))
   (vc-follow-symlinks t)                                ; don't ask for confirmation when opening symlink file
   (find-file-visit-truename t)                          ; find true path of the file.
   (inhibit-compacting-font-caches t)                    ; to speed up text rendering.
   (w32-get-true-file-attributes nil)                    ; disable Net Logon checks
   (frame-resize-pixelwise t)                            ; ensure text size is not rounded
   (create-lockfiles nil)                                ; don't create lockfiles
   (frame-title-format "%b %& emacs")                    ; Window Title => {Buffer Name} {Modified Status}
   (delete-by-moving-to-trash t)                         ; delete moves to recycle bin
   (column-number-mode t)                                ; display column number
   (show-paren-delay 0)                                  ; show matching immediately
   (scroll-conservatively  most-positive-fixnum)         ; scroll sensibly, don't jump around
   (mouse-wheel-scroll-amount '(1 ((shift) . 1)))        ; one line at a time
   (mouse-wheel-follow-mouse t)                          ; scroll window under mouse
   (find-file-visit-truename t)                          ; find true path of a file
   (custom-file (expand-file-name ".emacs-custom.el"     ; save machine specific settings here
                                  user-emacs-directory))
   (indicate-empty-lines t)                              ; Show empty lines
   (truncate-lines t)                                    ; disable word wrap
   (default-tab-width 4)                                 ; Default tab width is also 4 spaces.
   (help-window-select t)                                ; focus on help when shown.
   (savehist-save-minibuffer-history t)                  ; save minibuffer history.
   ))

;; - Set a default dark theme, overriden later ----------------------
(use-package emacs
  :ensure nil
  :init
  (load-theme 'tango-dark t))

;; - set editor behaviour -------------------------------------------
(use-package emacs
  :ensure nil
  :init
  (tool-bar-mode -1)                              ; Disable tool bar
  (tooltip-mode -1)                               ; Disable tooltips
  (menu-bar-mode -1)                              ; Disable menu bar
  (scroll-bar-mode -1)                            ; Hide scroll bar
  (defalias 'yes-or-no-p 'y-or-n-p)               ; Change yes/no prompt to y/n

  :config
  (global-display-line-numbers-mode)              ; Display line-numbers in all buffers
  (global-hl-line-mode)                           ; Highlight current line
  (show-paren-mode)                               ; Parenthesis highlighting
  (delete-selection-mode)                         ; Make delete work as expected
  (global-prettify-symbols-mode)                  ; prettify symbols (like lambda)
  (windmove-default-keybindings)                  ; Window Movement
;;  (recentf-mode)                                  ; Recent Files
  )

;; - set font and utf preferences ---------------------------------------
(use-package emacs
  :ensure nil
  :config
  ;; Use utf-8 everywhere.
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default default-buffer-file-coding-system 'utf-8)

  ;; Set default font
  (set-frame-font "Cascadia Code"  nil t)
  (set-face-attribute 'default nil)
  )

;; - change window splitting ----------------------------------------
(use-package emacs
  :ensure nil
  :preface
  (defun my/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun my/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :bind
  (("C-x 2" . 'my/split-and-follow-horizontally)
   ("C-x 3" . 'my/split-and-follow-vertically)))

;; - CUA Mode -------------------------------------------------------
(use-package emacs
  :ensure nil
  :custom
  (delete-selection-mode t)
  (normal-erase-is-backspace t)
  :hook
  (window-setup . cua-mode)
  :bind
  (("C-s"     . 'save-buffer)
   ("C-f"     . 'isearch-forward)
   ("C-S-f"   . 'isearch-backward)
   ("C-<tab>" . 'other-window)
   ("C-w"     . 'kill-this-buffer)
   ("C-S-w"   . 'delete-window)
   ("C-z"     . 'undo)
   ("<escape>". 'keyboard-escape-quit)))

;; - Fix delete behaviour -------------------------------------------
;; Below function didn't work when applied to <delete> suspect
;; that <delete> means something else to emacs
;; (use-package emacs
;;   :ensure nil
;;   :preface
;;   (defun my/delete-behaviour (n)
;; 	"Make delete button behave as expected in a sensible system"
;; 	(interactive "p")
;; 	(if (use-region-p)
;; 		(delete-region (region-beginning) (region-end))
;; 	  (delete-char n)))
;;   :bind
;;   (("<deletechar>" . 'my/delete-behaviour)))

;; - desktop save mode setup ----------------------------------------
(use-package emacs
  :ensure nil
  :preface
  (setq desktop-dirname             "~/.emacs.d/"            ; Path to save folder
        desktop-base-file-name      "emacs.desktop"          ; file name to save in
        desktop-base-lock-name      "lock"                   ; temp file
        desktop-path                (list desktop-dirname)   ; ???
        desktop-save                t                        ; save without asking
        desktop-files-not-to-save   "^$"                     ; reload tramp paths
        desktop-load-locked-desktop nil                      ; don't load locked file
        desktop-auto-save-timeout   30                       ; frequency of checks for changes to desktop
  )
  (desktop-save-mode t))

;; = Third-Party Packages' Configuration ============================
;; - all-the-icons --------------------------------------------------
(use-package all-the-icons)

(use-package unicode-fonts
  :hook
  (window-setup . unicode-fonts-setup))

;; - Doom Themes ----------------------------------------------------
(use-package doom-themes
  :init
  (load-theme 'doom-one t))

;; - doom modeline --------------------------------------------------
(use-package doom-modeline
  :custom
  (doom-modeline--modal-icon t)
  (doom-modeline-minor-modes t)
  :hook
  (window-setup . doom-modeline-mode))

;; - ivy, ivy-rich, swiper, counsel and smex -------------------------
(use-package ivy
  :diminish
  :init
  (minibuffer-depth-indicate-mode t)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  :bind
  (("C-x b"   . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  :hook
  (after-init . ivy-mode))

(use-package ivy-rich
  :after ivy
  :hook
  (after-init . ivy-rich-mode))

(use-package swiper
  :after ivy
  :bind
  ("C-f" . swiper))

(use-package counsel
  :after ivy
  :bind
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r"     . 'counsel-minibuffer-history))
  :hook
  (after-init . counsel-mode))

(use-package smex
  :custom
  (smex-completion-method 'ivy)
  :hook
  (after-init . smex-initialize))

;; - which key ------------------------------------------------------
(use-package which-key
  :diminish
  :hook
  (after-init . which-key-mode))

;; - rainbow delimiters ---------------------------------------------
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; - Paredit --------------------------------------------------------
(use-package paredit
  :hook
  ((lisp-mode
    emacs-lisp-mode
    lisp-interaction-mode
	scheme-mode
	racket-mode
	racket-repl-mode
	eval-expression-minibuffer-setup)
   . paredit-mode))

;; - Company --------------------------------------------------------
(use-package company
  :custom
  ((company-idle-delay 0)
   (company-tooltip-limit 20)
   (company-minimum-prefix-length 2)
   (company-selection-wrap-around t))
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection))
  :hook
  (after-init . global-company-mode))

(use-package company-box
  :after company
  :hook
  (company-mode . company-box-mode))

;; - Treemacs -------------------------------------------------------
(use-package treemacs
  :custom
  ((treemacs-python-executable "python.exe"))
  :init
  (which-key-add-key-based-replacements "C-c t" "treemacs")
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-c t 1"   . treemacs-delete-other-windows)
   ("C-c t t"   . treemacs)
   ("C-c t b"   . treemacs-bookmark)
   ("C-c t f"   . treemacs-find-file)
   ("C-c t M-f" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook
  (treemacs-mode . treemacs-icons-dired-mode))

;; - Magit ----------------------------------------------------------
(use-package magit
  ;:custom
  ;(magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . magit-status)))

;; - YA Snippets ----------------------------------------------------
(use-package yasnippet
  :after company
  :commands yas-minor-mode
  :init
  (which-key-add-key-based-replacements "C-c &" "YAsnippet")
  :config
  (yas-reload-all)
  (push '(company-capf company-yasnippet) company-backends)
  :hook
  (prog-mode . yas-global-mode))

(use-package yasnippet-snippets)

;; - YA Scroll ------------------------------------------------------
(use-package yascroll
  :custom
  ((yascroll:delay-to-hide nil))
  :hook
  (after-init . global-yascroll-bar-mode))

;; - Git Config files -----------------------------------------------
(use-package git-modes)

;; - Common Lisp using Slime ----------------------------------------
(use-package slime-company)
(use-package common-lisp-snippets)

(use-package slime
  :custom
  (inferior-lisp-program "sbcl")
  (slime-contribs '(slime-fancy
					slime-company
					slime-quicklisp
					slime-asdf
					slime-hyperdoc
					slime-repl
					slime-autodoc
					slime-macrostep
					slime-references
					slime-mdot-fu
					slime-xref-browser
					slime-presentations
					slime-cl-indent
					slime-fancy-inspector
					slime-fontifying-fu
					slime-trace-dialog))
  :hook
  (lisp-mode          . slime-mode)
  (inferior-lisp-mode . inferior-slime-mode))

;; - Racket ---------------------------------------------------------
(use-package geiser
  :commands (geiser-mode
			 run-geiser)
  :bind
  (("C-c \\"   . geiser-insert-lambda)
   ("C-c C-\\" . geiser-insert-lambda)
   ("C-c s"    . geiser-insert-sigma)
   ("C-c C-s"  . geiser-insert-sigma)))

(use-package geiser-racket
  :after geiser
  :commands (run-racket)
  :custom
  ((geiser-default-implementation 'racket)
   (geiser-active-implementation  '(racket))))

;; - Language Server Protocol ---------------------------------------
;; (use-package lsp-mode
;;   :commands lsp
;;   :custom
;;   ((lsp-keymap-prefix "C-c l"))
;;   :hook
;;   (c++-mode . lsp)
;;   (c-mode . lsp)
;;   (lsp-mode . lsp-enable-which-key-integration))

;; (use-package lsp-ui
;;   :custom
;;   ((lsp-ui-doc-enable nil)
;;    (lsp-ui-doc-delay 0.5))
;;   :commands lsp-ui-mode)

;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)

;; ------------------------------------------------------------------
(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
