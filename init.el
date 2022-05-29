;; -*- lexical-binding: t -*-

;; hide stuff early, so it doesnt flash
;; menu-bar is already disabled in early-init.el
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

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
(setq use-package-verbose t
      use-package-compute-statistics t
      straight-use-package-by-default t)

;; load my own configs with keybinds to open the file
(defvar config-directory (concat user-emacs-directory "config/"))
(defun load-config (filename &optional kbd-key)
  (let ((file (expand-file-name (concat filename ".el")
                                config-directory)))
    (load-file file)
    (and kbd-key
         (global-set-key (kbd kbd-key) (lambda () (interactive)
                                         (find-file file))))))

(load-config "theming")

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "GNU Emacs"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-set-footer nil
        dashboard-set-init-info t
        dashboard-set-heading-icons t
        dashboard-set-file-icons nil
        dashboard-set-navigator nil
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (agenda . 5))))


(load-config "base-config" "C-c b")
(load-config "main-packages" "C-c m")
(load-config "programming")

(global-set-key (kbd "C-c C-g") (lambda () (interactive)
                                  (ido-find-file-in-dir config-directory)))

;; make emacs customizations tool write its thing somewhere else
;; even though I probably will use use-package :custom instead
(setq custom-file "~/.emacs.d/config/customize.el")
(load custom-file)
