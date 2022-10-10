;;; early-init.el --- Early Init file for emacs      -*- lexical-binding: t; -*-
;;; Commentary:
;; This file sets up garbage collection and package management configuration of Emacs

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
;; - Straight.el ----------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el"
						 user-emacs-directory))
	  (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
									'silent
									'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Don't load any packages by default
(setq package-enable-at-startup nil)

;; Disable key checks due to elpa keys not being up to date
(setq package-check-signature nil)

;; Where to look for packages
(setq package-archives
	  '(("org"   . "https://orgmode.org/elpa/")
		("elpa"  . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")))

;; install use-package if it's not present
(straight-use-package 'use-package)

(use-package straight
  :custom
  ((straight-use-package-by-default   t)    ; always use straight.el with use-package
   (use-package-verbose               t)    ; make output verbose
   ))

