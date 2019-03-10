(eval-and-compile
  ;; Change GC threshold for duration of init, technique from DOOM Emacs FAQ
  ;; Reset at the end of this file
  (setq gc-cons-threshold 2147483648 ; set gc threshold to 2GiB
        gc-cons-percentage 0.6))

;; file handler check is not needed during start up
;; reset at the end of this file
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/reset-gc-and-file-handler ()
  (setq gc-cons-threshold 536870912  ; set gc threshold to 0.5GiB
        gc-cons-percentage 0.1
        file-name-handler-alist startup/file-name-handler-alist)
  )


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

(eval-when-compile
  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "emacs-config.org" user-emacs-directory)))


;; Below added by use-package?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (doom-modeline use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Reset changed values to defaults
;; values taken from DOOM-Emacs FAQ
(eval-and-compile
  (add-hook 'emacs-startup-hook 'startup/reset-gc-and-file-handler))

(setq initial-scratch-message (concat "Startup time: " (emacs-init-time)))
(provide 'init)
