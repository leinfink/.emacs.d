(use-package org
  :straight (:type built-in)
  :custom-face
  (variable-pitch ((t (:font "Fira Sans-10"))))
  (fixed-pitch ((t (:font "Fira Code-10"))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-tag ((t (:inherit fixed-pitch :height 0.9))))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-n" . org-do-demote)
         ("M-p" . org-do-promote)
         ("M-N" . org-demote-subtree)
         ("M-P" . org-promote-subtree)
         ("C-M-<return>" . org-insert-heading))

  :config
  (doom-themes-org-config)
  (add-hook 'org-mode-hook (lambda ()
                             (imenu-add-to-menubar "Imenu")
                             (lf/setup-org-theming)))

  :custom
  (org-catch-invisible-edits t)
  (org-agenda-files (list org-directory))
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (org-clock-idle-time 15)
  (org-contacts-files (list (concat org-directory "contacts.org")))
  (org-todo-keywords '((sequence "TODO(t)" "STARTED" "CURRENT" "WAITING(@/!)"
                                 "|" "DONE(d!)" "CANCELED(c@)")))
  (org-tag-alist '(("@home" . ?h) ("@academic" . ?a) ("@job" . ?w)))
  (org-capture-templates
   '(("t" "Todo"
      entry (file+headline "inbox.org" "Tasks")
      "* TODO %?\n")
     ("c" "Contacts" entry (file "contacts.org")
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:NICKNAME:
:NOTE:
:ADDRESS:
:BIRTHDAY: %^{yyyy-mm-dd}
:END:"))))

;;; org modules

(use-package org-contacts
  :straight (:host nil :type git :repo "https://repo.or.cz/org-contacts.git")
  :after (org))

(use-package org-protocol
  :straight (:type built-in)
  :after (org))

(use-package org-pomodoro
  :after (org)
  :commands org-pomodoro)

(use-package org-noter
  :after (org pdf-tools)
  :commands (org-noter))

;;; visual stuff

(defun lf/setup-org-theming ()
  (setq org-hide-emphasis-markers t
        fill-column 72
        ; org-hidden-keywords '(title)
        line-spacing 0.1
        org-pretty-entities t)

  (setq org-hide-block-startup t
      org-startup-folded "fold")

  (set-face-attribute 'org-level-8 nil :weight 'normal :inherit 'default)

  (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)

  (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.1) ;\large
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.2) ;\Large
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.4) ;\LARGE

  (setq org-cycle-level-faces nil)
  (setq org-n-level-faces 4)

  (set-face-attribute 'org-document-title nil
                      :height 1.6
                      :weight 'semi-bold
                      :foreground 'unspecified
                      :inherit 'org-level-8))

(use-package org-superstar
  :after (org)
  :hook (org-mode . org-superstar-mode)
  :custom (org-superstar-leading-bullet ?\s))

(use-package olivetti
  :after (org)
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 72))

(use-package visual-fill-column)

;; this package was deprecated, but actually...
;; ... its weird hack (inserting real newlines that get ignored)
;; is quicker and more useful in some scenarios than visual-line-mode
(use-package longlines
  :straight (:type built-in)
  :commands (longlines-mode))

(defun lf/toggle-fill-type ()
  "Switch between longlines-mode and visual-line-mode.
Useful when visual-line-mode is slow or does not give enough details
in undo-tree etc."
  (interactive)
  (if (bound-and-true-p longlines-mode)
      (progn (longlines-mode -1)
             (visual-line-mode 1)
             (message "Activated visual-line-mode."))
    (visual-line-mode -1)
    (longlines-mode 1)
    (message "Activated longlines-mode.")))
