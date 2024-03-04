;; Add this to your Emacs configuration file if you haven't already
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package vterm
    :ensure t)

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)))

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; Set the desired width of a tab (in spaces)
(setq-default c-basic-offset 4) ; Set the default indentation for C-like modes

(global-display-line-numbers-mode)
;;
;; Epitech configuration
;;
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "site-start.d/epitech-init.el")

;; Install required packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'general)
  (package-install 'general))

;; Make ESC key exit out of menus
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Display a vertical line at column 80
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(electric-pair-mode t)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 ;; Define your key bindings here
 ;; For example:
 "." 'find-file
 "t" 'neotree-toggle
 "f" 'vterm
 "SPC" 'other-window
 )

(use-package vline
  :ensure t
  :config
  (setq vline-global-mode t
        vline-idle-time 0.5))

(add-to-list 'load-path "~/.emacs.d/plugs/evil")
(add-to-list 'load-path "~/.emacs.d/plugs/flycheck")
(require 'evil)
(evil-mode 1)

(setq-default tab-width 4) ;; Set the default tab width
(setq-default c-basic-offset 4) ;; Set the default shift width for C-like modes

;; Syntastic configuration
(require 'flycheck)
(setq-default flycheck-highlighting-mode 'lines)

(setq flycheck-checker-error-threshold nil)

;; Configure the statusline for Syntastic
(setq-default mode-line-format
              (list
               '(:eval (flycheck-format-statusline '(:eval (syntastic-statusline-flag))))))

;; Configure Neotree options
(setq neo-theme 'icons) ;; Adjust the theme based on your preference
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action) ;; If you're using Projectile


;; Syntastic-like configuration for Flycheck
(add-hook 'prog-mode-hook
          (lambda ()
            (flycheck-mode)
            (setq-local flycheck-check-syntax-automatically '(save mode-enabled))))
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
