; dont ues package.el (since we use straight.el instead)
(setq package-enable-at-startup nil)

; hide menu-bar immediately
(add-hook 'before-init-hook (lambda () (menu-bar-mode -1)))
