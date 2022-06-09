(defun make-header-border ()
  (set-default 'header-line-format "")
  (set-face-attribute 'header-line nil :inherit 'default)
  (set-face-attribute 'header-line nil :height 50))

(add-hook 'window-setup-hook #'make-header-border)
(setq default-frame-alist '((font . "Fira Code-10")))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el"
                   :branch "master")
  :hook ((prog-mode text-mode) . ligature-mode)
  :config
  (ligature-set-ligatures 'text-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (ligature-mode))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors") ; alt: "doom-atom"
  ; doom-themes-org-config moved to use-package org-mode
  ; doom-themes-treemacs-config moved to use-package treemacs
  (doom-themes-visual-bell-config)
  (load-theme 'doom-one t)
  (custom-set-faces
   `(magit-header-line ((t (:background ,(doom-color 'bg)
                                        :box nil
                                        :height 100))))))

(use-package doom-modeline
  :demand t
  :hook (text-mode . (lambda () (setq doom-modeline-enable-word-count t)))
  :config
  (setq doom-modeline-hud t ; shows current position in buffer
        doom-modeline-buffer-modification-icon nil
        doom-modeline-icon t)
  (doom-modeline-mode 1))

(use-package solaire-mode
  :after (treemacs)
  :config (solaire-global-mode 1))

(use-package all-the-icons
  :if (or (display-graphic-p) (daemonp))) ; use M-x all-the-icons-install-fonts to install required fonts
