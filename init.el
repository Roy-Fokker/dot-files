;;; Commentary:
;; This file bootstraps the configuration of Emacs

;;; Code:





(add-hook 'focus-out-hook #'garbage-collect)



(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; ------------------------------------------------------------------
(eval-when-compile
    (when (file-exists-p custom-file)
      (load custom-file)))

  (if window-system

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
