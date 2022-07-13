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

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)))


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

  (defun lf/select-persistent-org-roam-buffer ()
    "Leinfink's select the persistent org-roam-buffer."
    (interactive)
    (select-window (get-buffer-window org-roam-buffer)))

  (global-set-key (kbd "C-c n p")
                  #'leinfink/select-persistent-org-roam-buffer)

  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section
              ))

  (set-face-attribute 'org-roam-header-line nil :height 100)
  (add-hook 'org-roam-buffer-postrender-functions #'mixed-pitch-mode)
  ; (add-hook 'org-roam-buffer-postrender-functions #'visual-line-mode)

  ;; start with collapsed sections
  (add-hook 'org-roam-buffer-postrender-functions
            #'magit-section-show-level-2)

  (setq org-roam-completion-everywhere t)

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "%d" count)))

  (setq org-roam-node-display-template
        (concat "${title:50} "
                (propertize "${tags:10}  " 'face 'org-tag)
                (propertize "${backlinkscount} " 'face 'org-tag)))

  (defun bms/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview.
https://org-roam.discourse.group/t/using-consult-ripgrep-with-org-roam-for-searching-notes/1226"
  (interactive)
  (let ((consult-ripgrep-args "rg --null --type org --line-buffered\
   --color=never --max-columns=500 --path-separator /\
   --ignore-case --no-heading --line-number ."))
    (consult-ripgrep org-roam-directory)))
  (global-set-key (kbd "C-c n r") 'bms/org-roam-rg-search))


(use-package org-roam-bibtex
  :disabled t
  :after org-roam
  :custom
  (orb-roam-ref-format 'org-cite)
  (bibtex-completion-bibliography org-cite-global-bibliography)
  :config
  (add-to-list 'orb-preformat-keywords "year"))

(use-package citar
  :straight (:host github :repo "emacs-citar/citar" :branch "main")
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))

  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  ;; (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths (list (concat org-roam-directory "references/")))
  (citar-symbols
   `((file,(all-the-icons-faicon "file-o"
                                 :face 'all-the-icons-green
                                 :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes"
                                    :face 'all-the-icons-blue
                                    :v-adjust -0.3) . " ")
     (link ,(all-the-icons-octicon "link"
                                   :face 'all-the-icons-orange
                                   :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  ;; (citar-open-note-functions '(orb-citar-edit-note))
  :config
  (defun custom-citar-org-roam-format (key entry filepath)
    "Format a note FILEPATH from KEY and ENTRY."
    (let* ((template (citar--get-template 'note))
           (note-meta
            (when template
              (citar--format-entry-no-widths
               entry
               template)))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
        ;; This just overrides other template insertion.
        (erase-buffer)
        (citar-org-roam-make-preamble key)
        (insert "#+title: ")
        (when template (insert note-meta))
        (insert "\n")
        )))

  (setq citar-create-note-function #'custom-citar-org-roam-format)

  ;; bugfix
  (defun citar--open-notes (key entry)
    "Open note(s) associated with KEY and ENTRY."
    (or (seq-some
         (lambda (opener)
           (funcall opener key entry)) citar-open-note-functions)
        (citar-file--open-note key entry)))

  (add-to-list 'citar-templates '(note . "${author} (${year}). ${title}")))

(use-package oc-csl-activate
  :after (org)
  :straight (:host github :repo "andras-simonyi/org-cite-csl-activate")
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (cursor-sensor-mode 1)
                             (org-cite-csl-activate-render-all)))
  (setq org-cite-activate-processor 'csl-activate))

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

(use-package delve
  :straight (:repo "publicimageltd/delve"
             :host github
             :type git)
  :after org-roam
  :bind
  ;; the main entry point, offering a list of all stored collections
  ;; and of all open Delve buffers:
  (("<f12>" . delve))
  :config
  ;; set meaningful tag names for the dashboard query
  (setq delve-dashboard-tags '("Tag1" "Tag2"))
  ;; optionally turn on compact view as default
  (add-hook #'delve-mode-hook #'delve-compact-view-mode)
 ;; turn on delve-minor-mode when Org Roam file is opened:
  (delve-global-minor-mode))
