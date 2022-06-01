(use-package org
  :straight (:type built-in)
  :commands org-mode
  :custom-face (variable-pitch ((t (:font "Fira Sans-10"))))
  :config
  (doom-themes-org-config)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode))

(use-package org-pomodoro
  :after (org)
  :commands org-pomodoro)
