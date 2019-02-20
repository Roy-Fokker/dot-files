(eval-and-compile
  ;; Change GC threshold for duration of init, technique from DOOM Emacs FAQ
  ;; Reset at the end of this file
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; file handler check is not needed during start up
;; reset at the end of this file
(defvar temp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


;; Some defaults for emacs
(setq-default inhibit-startup-message t) ; Disable emacs welcome screen

(setq backup-directory-alist `(("." . "backups")) ; backup files in this directory
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ; transform backup file names
      package-enable-at-startup nil ; do not load packages before start up
      delete-by-moving-to-trash t  ; delete moves to recycle bin
      version-control nil          ; disable emacs version control
      column-number-mode t         ; display column number
      show-paren-delay 0           ; show matching immediately
      indent-tabs-mode nil         ; don't use hard tabs
      )

(set-language-environment "UTF-8")  ; Use UTF-8
(set-default-coding-systems 'utf-8) ; Use UTF-8
(prefer-coding-system 'utf-8)       ; Use UTF-8
(global-display-line-numbers-mode)  ; Display line-numbers in all buffers
(global-hl-line-mode)               ; Highlight current line
(menu-bar-mode -1)                  ; Hide menu bar
(tool-bar-mode -1)                  ; Hide tool bar
(scroll-bar-mode -1)                ; Hide scroll bar
(show-paren-mode t)                 ; Parenthesis highlighting
(delete-selection-mode t)           ; Enable delete selection mode


(eval-when-compile
  ;; package management
  (require 'package)
  ;; Where to look for packages
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("elpa"  . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; initialize package
  (package-initialize)

  ;; enable 'use-package'
  (unless (package-installed-p 'use-package) ; check if it's already installed
          (package-refresh-contents)      ; update package archive
          (package-install 'use-package)) ; install the most recent version of use-package

  (require 'use-package)
  (setq use-package-always-ensure t))


;; ELDoc for Emacs Lisp, Common Lisp and Scheme
(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :hook
  ((emacs-lisp-mode . turn-on-eldoc-mode)
   (common-lisp-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (scheme-mode . turn-on-eldoc-mode)))


;; Magit
(use-package magit
  :defer t
  :commands magit-get-top-dir
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-git-global-arguments
  (nconc magit-git-global-arguments
         '("-c" "color.ui=false"
           "-c" "color.diff=false"))))

;; Paredit
(use-package paredit
  :hook
  ((lisp-mode . paredit-mode)
   (scheme-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (lisp-interaction-mode . paredit-mode)))


;; Company mode
(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-lenght 3)
  (global-company-mode t))


;; Which Key
(use-package which-key
  :config
  (which-key-mode))


;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


;; Sly
(setq sly-lisp-implementations
      '((sbcl    ("sbcl"))
        (ccl     ("ccl"))
        (roswell ("ros" "run")))
      sly-default-lisp (if (executable-find "sbcl")
                           'sbcl
                          'roswell))
(use-package sly
  :defer t
  :requires (sly-quicklisp sly-autoload)
  :commands sly  
  :hook
  ((lisp-mode . sly-mode)
   (lisp-interaction-mode . sly-mode)))


(eval-when-compile
  ;; Theme
  (use-package moe-theme
    :config
    (moe-dark))

  ;; All the icons
  (use-package all-the-icons)

  ;; Telephone-line
  (use-package telephone-line
    :config
    (setq telephone-line-lhs
    '((nil    . (telephone-line-window-number-segment))
      (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
      (nil    . (telephone-line-misc-info-segment
                       telephone-line-airline-position-segment))
      (accent . (telephone-line-buffer-modified-segment))
      (nil    . (telephone-line-buffer-name-segment))))
    (setq telephone-line-rhs
    '((accent . (telephone-line-minor-mode-segment))
      (nil    . (telephone-line-major-mode-segment))))
    (telephone-line-mode t))
 )

;; Below added by use-package?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (sly-autoloads use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Reset changed values to defaults
;; values taken from DOOM-Emacs FAQ
(eval-and-compile
  (add-hook 'emacs-startup-hook
            '(lambda ()
               (setq gc-cons-threshold 16777216
                     gc-cons-percentage 0.1
                     file-name-handler-alist temp--file-name-handler-alist))))

(setq initial-scratch-message (concat "Startup time: " (emacs-init-time)))
(provide 'init)
