; dont use package.el (since we use straight.el instead)
(setq package-enable-at-startup nil)

(defun hide-stuff ()
  "Hide stuff early, so it doesnt flash."
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)
    (setq mode-line-format nil) ; dont show before doom-modeline is loaded
    (setq inhibit-startup-screen t)
    (setq initial-scratch-message ""))

(add-hook 'before-init-hook #'hide-stuff)
