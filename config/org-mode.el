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
         ("C-c c" . org-capture))

  :config
  (doom-themes-org-config)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu")))
  (add-hook 'org-mode-hook
            (lambda () (setq fill-column 90)))

  :custom
  (org-agenda-files (list org-directory))
  (org-catch-invisible-edits t)
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
:END:")))
  (org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (org-clock-idle-time 15)
  (org-contacts-files (list (concat org-directory "contacts.org"))))

;;; org modules
(use-package org-contacts)

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

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

;; this package was deprecated, but actually...
;; ... its weird hack (inserting real newlines that get ignored)
;; is quicker and more useful in some scenarios than visual-line-mode
(use-package longlines
  :straight (:type built-in)
  :commands (longlines-mode))

(defun toggle-fill-type ()
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
