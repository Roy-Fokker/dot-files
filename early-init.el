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

;;Defer compilation to after init
(defvar comp-deferred-compilation)
(setq comp-deferred-compilation t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ensure garbage collection messages are on?
(setq garbage-collection-messages t)

;; Increase max bytes read from a sub-process in a single op
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; - Print message to *messages* buffer with total startup time -------------
(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Emacs ready in %s with %d garbage collections."
					 (format "%.2f seconds"
							 (float-time
							  (time-subtract after-init-time before-init-time)))
					 gcs-done)))

;; - Package Management -----------------------------------------------------
;; Don't load any packaged by default
(setq package-enable-at-startup nil)

;; Where to look for packages
(setq package-archives
	  '(("org"   . "https://orgmode.org/elpa/")
		("elpa"  . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")))

;; Load Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
						 user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
		 'silent
		 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package if it's not present
(straight-use-package 'use-package)

(use-package straight
  :custom
  ((straight-use-package-by-default   t)    ; always use straight.el with use-package
   (use-package-verbose               t)    ; make output verbose
   ))

;; - End early-init.el ------------------------------------------------------
(provide 'early-init)
