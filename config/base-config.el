;;; Fundamentals

;; encodings
(set-language-environment "UTF-8")

;; put auto-saves in a folder
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; put backups in a folder
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t)

;;; Basic Tweaks

(defalias 'yes-or-no-p #'y-or-n-p)

;; disable warnings
(put 'downcase-region 'disabled nil)
