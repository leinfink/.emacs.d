(defun make-header-border ()
  (set-default 'header-line-format "")
  (set-face-attribute 'header-line nil :background (doom-color 'bg))
  (set-face-attribute 'header-line nil :height 40))

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

(defvar night t)

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors") ; alt: "doom-atom"
  ; doom-themes-org-config moved to use-package org-mode
  ; doom-themes-treemacs-config moved to use-package treemacs
  (doom-themes-visual-bell-config)
  (load-theme (if night 'doom-one 'doom-one-light) t)
  (custom-set-faces
   `(magit-header-line ((t (:background ,(doom-color 'bg) :box nil :height 100))))))

(defun toggle-night ()
  (interactive)
  (setq night (not night))
  (if night (load-theme 'doom-one t)
    (load-theme 'doom-one-light t))
  (setq doom-modeline-hud night)
  (set-face-attribute 'header-line nil :background (doom-color 'bg))
  (set-face-attribute 'header-line nil :height 40)
  (set-face-attribute 'org-roam-header-line nil :height 100))


(use-package doom-modeline
  :demand t
  :hook (text-mode . (lambda () (setq doom-modeline-enable-word-count t)))
  :config
  (setq doom-modeline-hud night ; doesnt work nicely with doom-one-light
        doom-modeline-buffer-modification-icon nil
        doom-modeline-icon t
        doom-modeline-unicode-fallback nil)
  (doom-modeline-mode 1))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  (text-mode . visual-line-mode))

(use-package all-the-icons
  :if (or (display-graphic-p) (daemonp))) ; use M-x all-the-icons-install-fonts to install required fonts
