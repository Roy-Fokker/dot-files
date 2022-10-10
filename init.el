;;; init.el --- Init file for emacs      -*- lexical-binding: t; -*-
;;; Commentary:
;; This file bootstraps the configuration of Emacs

;;; Code:

;; = Emacs configuration ============================================
;; - set editor properties ------------------------------------------
(use-package emacs
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
   (native-comp-deferred-compilation t)                  ; defer compilation of packages
   (native-comp-async-query-on-exit t)                   ; kill any async jobs on exit
   (native-comp-async-jobs-number 0)                     ; only run 4 async jobs at a time
   (native-comp-async-report-warnings-errors nil)        ; don't care for warning messages
   ))

;; - Set a default dark theme, overriden later ----------------------
(use-package emacs
  :init
  (load-theme 'tango-dark t))

;; - set editor behaviour -------------------------------------------
(use-package emacs
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
  (set-face-attribute 'show-paren-match
					   nil
					   :weight 'ultra-bold)
  (delete-selection-mode)                         ; Make delete work as expected
  (global-prettify-symbols-mode)                  ; prettify symbols (like lambda)
  (windmove-default-keybindings)                  ; Window Movement
  )

;; - set font and utf preferences ---------------------------------------
(use-package emacs
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

;; - desktop save mode setup ----------------------------------------
(use-package emacs
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

;; - recentf --------------------------------------------------------
(use-package emacs
  :custom
  ((recentf-max-menu-items 25)
   (recentf-max-saved-items 25))
  :bind
  (("C-x C-r" . 'recentf-open-files))
  :hook
  (after-init . recentf-mode))

;; = Third-Party Packages' Configuration ============================
;; - dracula Theme --------------------------------------------------
(use-package doom-themes
  :init
  (load-theme 'doom-dracula t))

;; - doom modeline --------------------------------------------------
(use-package doom-modeline
  :custom
  ((doom-modeline-icon t)
   (doom-modeline-bar-width 3))
  :init
  (doom-modeline-mode))

;; - rainbow delimiters ---------------------------------------------
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; - all-the-icons --------------------------------------------------
(use-package all-the-icons
  :commands all-the-icons-install-fonts
  :config (unless (find-font (font-spec :name "all-the-icons"))
			(all-the-icons-install-fonts t)))

;; - smartparens ----------------------------------------------------
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

;; - which key ------------------------------------------------------
(use-package which-key
  :hook
  (after-init . which-key-mode))

;; - treemacs -------------------------------------------------------
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
  :after treemacs
  :hook
  (treemacs-mode . treemacs-icons-dired-mode))

;; - company --------------------------------------------------------
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

;; - yasnippets -----------------------------------------------------
(use-package yasnippet
  :after company
  :commands yas-minor-mode
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-global-mode))

(use-package yasnippet-snippets)

;; - magit ----------------------------------------------------------
(use-package magit
  :bind
  (("C-x g" . magit-status)))

;; - ivy ------------------------------------------------------------
(use-package ivy
  :diminish
  :custom
  ((ivy-initial-input-alist nil)
   (ivy-use-virtual-buffers t)
   (ivy-count-format "(%d/%d) ")
   (ivy-height 20)
   (ivy-display-style 'fancy))
  :hook (after-init . ivy-mode))

(use-package ivy-rich
  :hook (after-init . ivy-rich-mode))

(use-package all-the-icons-ivy
  :init
  (all-the-icons-ivy-setup))

;; - counsel --------------------------------------------------------
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; - swiper ---------------------------------------------------------
(use-package swiper
  :bind (("C-f" . swiper)))

;; = Languages ======================================================
;; - C/C++ ----------------------------------------------------------
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :hook
  ((c-mode
	c++-mode) . eglot-ensure))

;; - Common Lisp ----------------------------------------------------
;; ------------------------------------------------------- Snippets -
(use-package common-lisp-snippets)

;; ---------------------------------------------------------- Slime -
(use-package slime
  :custom
  (slime-contribs '(slime-fancy
					slime-company
					slime-quicklisp
					slime-asdf
					slime-hyperdoc
					slime-xref-browser
					slime-cl-indent))
  :config
  (setq inferior-lisp-program "sbcl")
  :hook
  ((lisp-mode          . slime-mode)
   (inferior-lisp-mode . inferior-slime-mode)))

(use-package slime-company
  :after (slime company)
  :custom
  ((slime-company-completion       'simple)
   (slime-company-after-completion 'slime-company-just-one-space)))

;; ------------------------------------------------------------ Sly -
;; (use-package sly
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   :hook
;;   ((lisp-mode . sly-editing-mode)))

;; (use-package sly-quicklisp
;;   :after sly)

;; (use-package sly-asdf
;;   :after sly)

(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
