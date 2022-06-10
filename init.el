;; -*- lexical-binding: t -*-

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun measure-startup-time ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

; enable only when I want to check
(add-hook 'emacs-startup-hook #'measure-startup-time)

;; straight.el package manager
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; enable use-package
(straight-use-package 'use-package)
(setq use-package-verbose nil
      use-package-compute-statistics t
      straight-use-package-by-default t)

(if (daemonp)
    (setq use-package-always-demand t))

;; load my own configs with keybinds to open the file
(defvar config-directory (concat user-emacs-directory "config/"))
(defun load-config (filename &optional kbd-key)
  (let ((file (expand-file-name (concat filename ".el")
                                config-directory)))
    (load-file file)
    (and kbd-key
         (global-set-key (kbd kbd-key) (lambda () (interactive)
                                         (find-file file))))))

(load-config "locals")
(load-config "theming")
(load-config "base-config")
(load-config "main-packages")
(load-config "programming")
(load-config "org-mode")
(load-config "org-roam")

(global-set-key (kbd "C-c C-g") (lambda () (interactive)
                                  (ido-find-file-in-dir config-directory)))

(global-set-key (kbd "C-c i") (lambda () (interactive)
                                (ido-find-file-in-dir org-directory)))

;; make emacs customizations tool write its thing somewhere else
(setq custom-file "~/.emacs.d/config/customize.el")
;; but I wont use this anyway, can use use-package :custom instead
; (load custom-file)

;; Make gc pauses faster by decreasing the threshold again.
(setq gc-cons-threshold (* 2 1000 1000))
