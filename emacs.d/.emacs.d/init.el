;; Initialize package.el and add Melpa repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure 'use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Install and enable Evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; Install and enable vterm
(use-package vterm
  :ensure t)

;; Set indentation for C mode
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)))

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Enable global line numbers
(global-display-line-numbers-mode)

;; Epitech configuration
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "site-start.d/epitech-init.el")

;; Install required packages (use-package and general)
(use-package general
  :ensure t)

;; Key bindings
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "." 'find-file
 "t" 'neotree-toggle
 "f" 'vterm
 "SPC" 'other-window)

;; Display a vertical line at column 80
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Electric Pair mode
(electric-pair-mode t)

;; Configure vline
(use-package vline
  :ensure t
  :config
  (setq vline-global-mode t
        vline-idle-time 0.5))

;; Set default tab width and shift width for C-like modes
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; ... (previous configurations)

;; Flycheck configuration
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-highlighting-mode 'lines
                flycheck-checker-error-threshold nil)
  (add-hook 'prog-mode-hook
            (lambda ()
              (flycheck-mode)
              (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))
  (setq-default mode-line-format
                (list
                 '(:eval (flycheck-format-statusline '(:eval (syntastic-statusline-flag)))))))

;; ... (remaining configurations)

;; Configure Neotree options
(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'icons
        neo-smart-open t
        projectile-switch-project-action 'neotree-projectile-action))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(neotree yasnippet general)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

