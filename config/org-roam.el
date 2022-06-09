(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.25)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (defun leinfink/select-persistent-org-roam-buffer ()
    (select-window (get-buffer-window org-roam-buffer)))

  (global-set-key (kbd "C-c n p") #'leinfink/select-persistent-org-roam-buffer)

  (get-buffer-window-list)
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

  (set-face-attribute 'org-roam-header-line nil :height 100)
  (add-hook 'org-roam-mode-hook #'variable-pitch-mode)
  (add-hook 'org-roam-mode-hook #'visual-line-mode)

  (setq org-roam-completion-everywhere t))
