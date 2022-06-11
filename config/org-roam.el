(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :custom-face
  (org-roam-header-line ((t (:inherit font-lock-string-face))))
  :config
  (org-roam-db-autosync-mode)

  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.4)
               (window-height . fit-window-to-buffer)))

  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*org-roam\\*"
  ;;                (display-buffer-in-side-window)
  ;;                (side . right)
  ;;                (slot . 0)
  ;;                (window-width . 0.33)
  ;;                (window-parameters . ((no-other-window . t)
  ;;                                      (no-delete-other-windows . t)))))

  (defun leinfink/select-persistent-org-roam-buffer ()
    (interactive)
    (select-window (get-buffer-window org-roam-buffer)))

  (global-set-key (kbd "C-c n p")
                  #'leinfink/select-persistent-org-roam-buffer)

  (get-buffer-window-list)
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section
            ))

  (set-face-attribute 'org-roam-header-line nil :height 100)
  (add-hook 'org-roam-buffer-postrender-functions #'mixed-pitch-mode)
  (add-hook 'org-roam-buffer-postrender-functions #'visual-line-mode)


  (setq org-roam-completion-everywhere t))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq bibtex-completion-bibliography org-cite-global-bibliography))

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main"
         :files ("*.el" "out"))
    :after org-roam
    :commands (org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
