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

(use-package geiser-guile
  :defer t
  :custom
  (geiser-guile-binary "/usr/bin/guile3")
  :commands (geiser run-geiser))

(use-package lua-mode
  :commands (lua-mode))

(use-package gdscript-mode
  :commands (gdscript-godot-open-project-in-editor gdscript-mode))

(use-package smartparens
  :commands (smartparens-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode
                 "\\\\{" "}"
                 :when '(sp-in-docstring-p))
  (sp-with-modes sp-lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil))

  (sp-with-modes (-difference sp-lisp-modes sp-clojure-modes)
    ;; also only use the pseudo-quote inside strings where it serve as
    ;; hyperlink.
    (sp-local-pair "`" "'"
                   :when '(sp-in-string-p
                           sp-in-comment-p)
                   :unless '(sp-lisp-invalid-hyperlink-p)
                   :skip-match (lambda (ms _mb _me)
                                 (cond
                                  ((equal ms "'")
                                   (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                                       (not (sp-point-in-string-or-comment))))
                                  (t (not (sp-point-in-string-or-comment)))))))
  (defun sp-lisp-invalid-hyperlink-p (_id action _context)
  "Test if there is an invalid hyperlink in a Lisp docstring.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    ;; Ignore errors due to us being at the start or end of the
    ;; buffer.
    (ignore-errors
      (or
       ;; foo'|bar
       (and (looking-at "\\sw\\|\\s_")
            ;; do not consider punctuation
            (not (looking-at "[?.,;!]"))
            (save-excursion
              (backward-char 2)
              (looking-at "\\sw\\|\\s_")))
       ;; foo|'bar
       (and (save-excursion
              (backward-char 1)
              (looking-at "\\sw\\|\\s_"))
            (save-excursion
              (forward-char 1)
              (looking-at "\\sw\\|\\s_")
              ;; do not consider punctuation
              (not (looking-at "[?.,;!]")))))))))

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((prog-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
