(setq mode-line-format nil) ; just so it doesnt show before doom-modeline is loaded

(setq header-line-format "")
(set-face-attribute 'header-line nil :inherit 'default)
(set-face-attribute 'header-line nil :height 50)

(set-frame-font "Fira Code-10" nil t)

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el"
                   :branch "master")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'text-mode '("ff" "fi" "ffi")) ; sadly not there in fira code
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))


(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :config (doom-modeline-mode 1))

(use-package solaire-mode
  :config
  (defun real-buffer-p ()
    (or (solaire-mode-real-buffer-p)
        (equal (buffer-name) "*dashboard*")))
  (setq solaire-mode-real-buffer-fn #'real-buffer-p)
  (solaire-global-mode 1))

(use-package all-the-icons
  :if (display-graphic-p)) ; use M-x all-the-icons-install-fonts to install required fonts




