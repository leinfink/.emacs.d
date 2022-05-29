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

;; save places in files and minibuffer history
(save-place-mode 1)
(savehist-mode 1)

;; disable warnings etc.
(setq use-short-answers t) ; emacs 28
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))
(setq disabled-command-function nil)
(setq server-client-instructions nil)

; I never work in arabic or other r-to-l scripts, so can do this
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)

(use-package esup
  :commands esup)

;;; Basic Tweaks

(column-number-mode 1)
(global-hl-line-mode 1)

(winner-mode 1)

(add-hook 'after-make-frame-functions
          (lambda (_)                                        
            (scroll-bar-mode -1) ; somehow this is necessary, and only for scroll-bar..
            (message ""))) ; clear echo area

;; minibuffer completion
;; very simple this time: no helm, no ivy, not even ido
(icomplete-mode 1)
(icomplete-vertical-mode 1)
(fido-mode 1)

(use-package company
  :defer 3
  :config (global-company-mode 1))

(use-package undo-tree
  :defer 3
  :config
  (setq undo-tree-history-directory-alist
        `(("." . ,(concat user-emacs-directory "undo-tree"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :defer 2
  :config (which-key-mode 1))

;; smooth scrolling (emacs 29)
(pixel-scroll-precision-mode 1)

;; this is apparently nice when using a mouse, didn't test yet
; (setq pixel-scroll-precision-large-scroll-height 40.0)
; (setq pixel-scroll-precision-interpolation-factor 30)

;; a lot of the the following was taken from better-defaults.el
;; Phil Hagelberg (GPLv3)
;; https://github.com/technomancy/better-defaults

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq-default indent-tabs-mode nil)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      sentence-end-double-space nil
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; by Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)
