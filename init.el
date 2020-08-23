;;; init.el --- Init file for emacs      -*- lexical-binding: t; -*-
;;; Commentary:
;; This file bootstraps the configuration of Emacs

;;; Code:

;; - Garbage Collection setting -------------------------------------
;; Techniques borrowed from DOOM Emacs FAQ
(eval-when-compile
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
  (add-hook 'focus-out-hook #'garbage-collect))

;; - Basic Behaviour ------------------------------------------------
;; Disable GUI elements
(tool-bar-mode -1)                              ; Disable tool bar
(menu-bar-mode -1)                              ; Disable menu bar
(scroll-bar-mode -1)                            ; Hide scroll bar

;; Enable Common User Actions, like sane editor
(cua-mode t)
(global-set-key (kbd "C-s") 'save-buffer)

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

;; Change window splitting behaviour.
(eval-when-compile
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
  (global-set-key (kbd "C-x 3") 'vsplit-other-window)) ; and horizontal splits.

;; ------------------------------------------------------------------
;; This function will get full path of the special folder regardless
;; where it is on a Windows machine. It makes a call out to cmd/PS.

(defun get-windows-special-folder-path (FOLDER_NAME)
  "FOLDER_NAME is special folder name of interest."
  (first
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

(eval-when-compile
  (defun my/save-frame-geometry ()
    "Save Emacs frame geometry into a file to be loaded later."
    (let ((frame-left      (first (frame-position)))
          (frame-top       (rest (frame-position)))
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
        (add-hook 'after-init-hook #'my/load-frame-geometry))))

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

;; - No Littering ---------------------------------------------------
(use-package no-littering
  :config
  (setq no-littering-etc-directory (expand-file-name "etc/"
                                                     user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/"
                                                     user-emacs-directory)))

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

;; - Undo Tree ------------------------------------------------------
(use-package undo-tree
  :diminish (undo-tree-mode global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region t)
  (defalias 'redo 'undo-tree-redo)
  (defalias 'undo 'undo-tree-undo)
  :hook (after-init . global-undo-tree-mode)
  :bind (("C-z" . undo)
         ("C-y" . redo)))

;; - Popup Kill Ring ------------------------------------------------
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

;; - All the icons --------------------------------------------------
(use-package all-the-icons)

(use-package all-the-icons-ivy)

;; - Unicode Fonts --------------------------------------------------
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;; - Doom Modeline --------------------------------------------------
(use-package doom-modeline
    :config
    (setq doom-modeline-icon t
          doom-modeline-major-mode-icon t
          doom-modeline-major-mode-color-icon t
          doom-modeline-minor-modes t
          doom-modeline-unicode-fallback t
          doom-modeline-bar-width 3)
    :custom-face
    (doom-modeline-bar ((t (:background "#bd93f9"))))
    (doom-modeline-bar-inactive ((t (:background "#6272a4"))))
    :hook 
    (after-init . doom-modeline-mode))

;; - Rainbow Brackets -----------------------------------------------
(use-package rainbow-delimiters
    :hook ((prog-mode
            text-mode
            lisp-interaction-mode
            slime-repl-mode
            cider-repl-mode
            racket-repl-mode)
           . rainbow-delimiters-mode)
    :custom-face
    (rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
    (rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
    (rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
    (rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
    (rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
    (rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
    (rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
    (rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

;; - Magit ----------------------------------------------------------
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; ------------------------------------------------------------------
(setq initial-scratch-message (concat ";; Startup time: " (emacs-init-time)))

(provide 'init)
;;; init.el ends here
