;; Set up package.el and MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)))

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; Set the desired width of a tab (in spaces)
(setq-default c-basic-offset 4) ; Set the default indentation for C-like modes

;; Display a vertical line at column 80
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Install required packages
(defvar my-packages '(use-package evil fzf general neotree vterm all-the-icons doom-themes))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Set up use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Enable line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Enable word wrap
(global-visual-line-mode t)

;; Set up auto-pairs
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

;; Set up doom-themes
(use-package doom-themes
  :ensure t
  :config)
  

;; Set up neotree
(use-package neotree
  :ensure t
  :bind ("<leader>T" . neotree-toggle))

;; Set up fzf
(use-package fzf
  :ensure t
  :bind ("<leader>." . fzf))

;; Set up evil mode
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

;; Set up syntastic (you can use flycheck as an alternative)
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; Set up smooth scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

;; Set up doom-modeline for a minimalistic status line
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; Set up custom key bindings
(evil-define-key 'normal global-map (kbd "<leader>.") 'fzf)
(evil-define-key 'normal global-map (kbd "<leader>T") 'neotree-toggle)


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
;; Set background based on time
(if (< (string-to-number (format-time-string "%H")) 12)
    (load-theme 'doom-one-light t)
  (load-theme 'doom-one t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline smooth-scrolling flycheck smartparens doom-themes all-the-icons neotree fzf evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
