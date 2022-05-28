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

;; make emacs customizations tool write its thing somewhere else
(setq custom-file "~/.emacs.d/config/customize.el")
(load custom-file)

;; load my own configs
(defun load-user-file (filename)
  (load-file (expand-file-name (concat filename ".el")
			       (concat user-emacs-directory "config/"))))

(load-user-file "base-config")
(load-user-file "main-packages")
