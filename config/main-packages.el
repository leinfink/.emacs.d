(use-package helpful
  :bind (("C-h f" . helpful-callable) ; both functions and macros
         ("C-h F" . helpful-function) ; excludes macros
         ("C-h C" . helpful-command)  ; interactive only
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-c C-d" . helpful-at-point)))

; (use-package workgroups2) ; tested yet

(use-package magit
  :defer t
  :config
  (defun ~/magit-process-environment (env)
    "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
In my case, .dotfiles as GIT_DIR and $HOME as GIT_WORK_TREE, when in $HOME.
Taken from: https://github.com/magit/magit/issues/460 (@cpitclaudel)."
    (let ((default (file-name-as-directory (expand-file-name default-directory)))
          (home (expand-file-name "~/")))
      (when (string= default home)
        (let ((gitdir (expand-file-name "~/.dotfiles/")))
          (push (format "GIT_WORK_TREE=%s" home) env)
          (push (format "GIT_DIR=%s" gitdir) env))))
    env)
  (advice-add 'magit-process-environment
              :filter-return #'~/magit-process-environment))

(use-package forge
  :after (magit)
  :custom (forge-owned-accounts '(("leinfink". nil))))

(use-package treemacs
  :bind (("C-c C-t" . treemacs)
         ("C-x j" . treemacs-select-window))
  :init (setq treemacs-width 25)
  :config
  (doom-themes-treemacs-config)
  (treemacs-hide-gitignored-files-mode t))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :after (treemacs)
  :config
  (setq treemacs-icons-dired--ranger-adjust t) ; fixes first lines in dired with -ls
  (treemacs-icons-dired-mode 1))

(use-package pdf-tools
  :commands (pdf-tools))
