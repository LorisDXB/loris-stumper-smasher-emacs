(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq c-basic-offset 4)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) 
(setq-default c-basic-offset 4) 

(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Install required packages  REMOVE evil HERE IN THE PACKAGES TO REMOVE VIM KEYBINDINGS
;; NOTE: THIS WILL DESTROY ALL THE SHORTCUTS I MADE SO YOU'LL PROBS HAVE TO REDO THEM
(defvar my-packages '(use-package fzf evil general neotree vterm all-the-icons doom-themes))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

(require 'use-package)
(setq use-package-always-ensure t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(global-visual-line-mode t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

(use-package doom-themes
  :ensure t
  :config)
  
(use-package neotree
  :ensure t
  :bind ("<leader>T" . neotree-toggle))

(use-package fzf
  :ensure t
  :bind ("<leader>." . fzf))

;; IF YOU UNTOGGLED evil IN THE PACKAGES, REMOVE ALL THIS SECTION
(use-package evil
  :ensure t
  :init
  (evil-mode 1))
;;UNTIL HERE

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;;THESE ARE THE CURRENT KEYBINDS
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "." 'fzf ;;fzf doesnt work mb
 "t" 'neotree-toggle
 "f" 'vterm
 "SPC" 'other-window
 )

(custom-set-variables
 '(package-selected-packages
   '(doom-modeline smooth-scrolling flycheck smartparens doom-themes all-the-icons neotree fzf evil)))
(custom-set-faces
 )
