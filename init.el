(eval-and-compile
  ;; Change GC threshold for duration of init, technique from DOOM Emacs FAQ
  ;; Reset at the end of this file
  (setq gc-cons-threshold 2147483648 ; set gc threshold to 2GiB
        gc-cons-percentage 0.6))

;; ------------------------------------------------------------------

;; file handler check is not needed during start up
;; reset at the end of this file
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/reset-gc-and-file-handler ()
  (setq gc-cons-threshold 536870912  ; set gc threshold to 0.5GiB
        gc-cons-percentage 0.1
        file-name-handler-alist startup/file-name-handler-alist))

;; ------------------------------------------------------------------

(setq-default inhibit-startup-screen t) ; Disable Emacs Welcome Screen

(setq backup-directory-alist         `(("." . "backups"))    ; backup files in this directory
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ; transform backup file names
      package-enable-at-startup nil                  ; do not load packages before start up
      delete-by-moving-to-trash t                    ; delete moves to recycle bin
;;      version-control nil                            ; disable emacs version control
      column-number-mode t                           ; display column number
      show-paren-delay 0                             ; show matching immediately
      scroll-conservatively  most-positive-fixnum    ; scroll sensibly, don't jump around
      mouse-wheel-scroll-amount '(1 ((shift) . 1))   ; one line at a time
      mouse-wheel-follow-mouse t                     ; scroll window under mouse
      find-file-visit-truename t                     ; find true path of a file
      )

;; ------------------------------------------------------------------

(global-display-line-numbers-mode)  ; Display line-numbers in all buffers
(global-hl-line-mode)               ; Highlight current line
(menu-bar-mode -1)                  ; Hide menu bar
(tool-bar-mode -1)                  ; Hide tool bar
(scroll-bar-mode -1)                ; Hide scroll bar
(show-paren-mode t)                 ; Parenthesis highlighting
(delete-selection-mode t)           ; Enable delete selection mode
(cua-mode)                          ; Enable CUA mode

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
(set-default-font "Source Code Pro 10")
(setq inhibit-compacting-font-caches t)

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
        (add-hook 'after-init-hook 'emacs/load-framegeometry))))

;; ------------------------------------------------------------------
(eval-when-compile
  ;; package management
  (require 'package)

  ;; Where to look for packages
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("elpa"  . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; initialize package
  (package-initialize))

(eval-when-compile
  ;; - All-the-icons --------------------------------------------------
  (require 'all-the-icons))

(eval-when-compile
  ;; - Doom-ModeLine --------------------------------------------------
  (require 'doom-modeline)
  (doom-modeline 1)
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t)
  (add-hook 'after-init-hook 'doom-modeline-mode))

(eval-when-compile
  ;; - Ivy Counsel Swiper ---------------------------------------------
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-initial-input-alist nil
	ivy-re-builders-alist '((t . ivy--regex-fuzzy))
	ivy-height 20)

  (require 'ivy)
  (ivy-mode)

  (require 'ivy-rich)
  (ivy-rich-mode)

  (require 'counsel)

  (require 'all-the-icons-ivy)
  (all-the-icons-ivy-setup)

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(eval-when-compile
  ;; - Rainbow-Delimiters ---------------------------------------------
  (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook             'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'slime-repl-mode-hook       'rainbow-delimiters-mode)
  ;; (add-hook 'geiser-repl-mode-hook      'rainbow-delimiters-mode)
  )

(eval-when-compile
  ;; - WINUM ----------------------------------------------------------
  (setq winum-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-`") 'winum-select-window-by-number)
	  (define-key map (kbd "C-²") 'winum-select-window-by-number)
	  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
	  (define-key map (kbd "M-1") 'winum-select-window-1)
	  (define-key map (kbd "M-2") 'winum-select-window-2)
	  (define-key map (kbd "M-3") 'winum-select-window-3)
	  (define-key map (kbd "M-4") 'winum-select-window-4)
	  (define-key map (kbd "M-5") 'winum-select-window-5)
	  (define-key map (kbd "M-6") 'winum-select-window-6)
	  (define-key map (kbd "M-7") 'winum-select-window-7)
	  (define-key map (kbd "M-8") 'winum-select-window-8)
	  map))
  (require 'winum)
  (winum-mode))

(eval-when-compile
  ;; - Which-Key ------------------------------------------------------
  (require 'which-key)
  (which-key-mode))

(eval-when-compile
  ;; - Company-Mode ---------------------------------------------------
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-selection-wrap-around t
	)  
  (company-tng-configure-default)

  (require 'company-quickhelp)
  (company-quickhelp-mode)
  )

(eval-when-compile
  ;; - YaSnippers -----------------------------------------------------
  (require 'yasnippet)
  (yas-global-mode 1)
  )

(eval-when-compile
  ;; - Paredit --------------------------------------------------------
  (require 'paredit)
  (add-hook 'emacs-lisp-mode-hook       'paredit-mode)
  (add-hook 'lisp-mode-hook             'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook           'paredit-mode)
  (add-hook 'slime-repl-mode-hook       'paredit-mode)
  ;; (add-hook 'geiser-repl-mode-hook      'paredit-mode)
  )

(eval-when-compile
  ;; - eldoc ----------------------------------------------------------
  (require 'eldoc)
  (add-hook 'scheme-mode-hook           'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
  (add-hook 'lisp-mode-hook             'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'slime-repl-mode-hook       'turn-on-eldoc-mode)
  ;; (add-hook 'geiser-repl-mode-hook      'turn-on-eldoc-mode)
  )


(eval-when-compile
  ;; - Magit ----------------------------------------------------------
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-git-global-arguments
	(nconc magit-git-global-arguments
	       '("-c" "color.ui=false"
		 "-c" "color.diff=false"))))

;; ------------------------------------------------------------------
;; Common Lisp Specific
;; ------------------------------------------------------------------
(eval-when-compile
  ;; - Lisp Implementations ----------------------------------------
  (setq my/lisp-implementations
	'((sbcl ("sbcl"))
	  (ccl ("ccl"))
	  (roswell ("ros" "run")))
	my/default-lisp (if (executable-find "ros")
			    'roswell
			  'sbcl))

  ;; - SLIME --------------------------------------------------------
  (setq slime-lisp-implementations my/lisp-implementations
	slime-default-lisp my/default-lisp)
  
  (require 'slime)
  (slime-setup '(slime-fancy slime-company slime-quicklisp slime-asdf))


  ;; - Sly ---------------------------------------------------------
  ;; (if nil   ; Not currently used... Need better way to switch between
  ;; 	    ; sly and slime
  ;;     (progn
  ;; 	(setq sly-lisp-implementations my/lisp-implementations
  ;; 	      sly-default-lisp my/default-lisp)

  ;; 	(require 'sly)
  ;; 	(require 'sly-autoload)
  ;; 	(require 'sly-quicklisp)

  ;; 	(add-hook 'lisp-mode-hook        'sly-editing-mode)
  ;; 	(add-hook 'lisp-interaction-mode 'sly-mode)
  )

;; ------------------------------------------------------------------
;; ------------------------------------------------------------------
;; Racket Specific
;; ------------------------------------------------------------------
(eval-when-compile
  ;; - Racket-Mode --------------------------------------------------
  (setq my/default-scheme '(racket))
  (setq geiser-active-implementations my/default-scheme)
  
  (require 'geiser)
  (defun geiser-save ()
    (interactive)
    (geiser-repl--write-input-ring))
  )

;; ------------------------------------------------------------------
;; ------------------------------------------------------------------


;; Reset changed values to defaults
;; values taken from DOOM-Emacs FAQ
(eval-and-compile
  (add-hook 'emacs-startup-hook 'startup/reset-gc-and-file-handler))

;; ------------------------------------------------------------------
(setq initial-scratch-message (concat "Startup time: " (emacs-init-time)))
(provide 'init)

;; ------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(package-selected-packages
   (quote
    ())))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
