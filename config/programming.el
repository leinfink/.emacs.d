(use-package smart-hungry-delete
  :bind
  (:map prog-mode-map
        ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	([remap delete-backward-char] . smart-hungry-delete-backward-char)
	([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package xr
  :defer 4)

; not tried yet
; (use-package smartparens)

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((prog-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
