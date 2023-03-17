;;; init.el --- Emacs init file

;; Created by daigo0927
;;; Commentary:

;;; Code:

;;; don't show starting message
(setq inhibit-startup-message 1)

;; user C-h as backspace
(keyboard-translate ?\C-h ?\C-?)

;; Ignore 'Package cl is duplicated' message
(setq byte-compile-warnings '(not cl-functions obsolete))

;; specify base directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq gc-cons-threshold 12800000)

;; truncate long lines
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; package management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ))
(package-initialize)
(fset 'package-desc-vers 'package--ac-desc-version)


(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package diminish
  :ensure t
  :diminish (eldoc-mode)
  )

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

;; https://github.com/Alexander-Miller/treemacs#installation
;; (use-package treemacs
;;   :ensure t
;;   :config
;;   (use-package treemacs-icons-dired
;;     :hook (dired-mode . treemacs-icons-dired-enable-once)
;;     :ensure t
;;     )
;;   )

;; python major mode
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; Usage: https://iriya-ufo.net/blog/2022/02/22/magit/
(use-package magit :ensure t)

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode)

  :hook
  (unless (window-system)
    (diff-hl-mode . diff-hl-margin-local-mode)
    )
  )

;; go settings: https://emacs-jp.github.io/programming/golang
(with-eval-after-load 'go-mode
  ;; auto-complete
  (require 'go-autocomplete)

  ;; company-mode
  ;; (add-to-list 'company-backends 'company-go)

  ;; eldoc
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  ;; key bindings
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-,") 'pop-tag-mark))

(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; intelligent edit
(require 'iedit)
(global-set-key (kbd "C-x ;") 'iedit-mode)

(semantic-mode 1)
(add-hook 'python-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'python-imenu-create-index)))

;; color theme
;; (use-package rebecca-theme
;;   :ensure t
;;   :config (load-theme 'rebecca t)
;;   )

;; disable doom theme (avoid UI conflicts when emacs is used in -nw mode)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Color list: http://xay-lab.nautilus.xyz/2010/09/emacs.html
  ;; Other: https://qiita.com/hyakt/items/0473112466da7f6d3bdc, 
  (custom-set-faces
   `(mode-line ((t (:background , "SlateBlue1"))))
   `(mode-line-inactive ((t (:background , "SlateBlue4"))))
   `(font-lock-comment-face ((t (:foreground ,(doom-color 'base7)))))
   `(flycheck-error ((t (:foreground ,(doom-color 'red)))))
   `(lsp-flycheck-info-unnecessary-face ((t (:foreground ,(doom-color 'green)))))
   `(font-lock-string-face ((t (:foreground ,(doom-color 'cyan)))))
   )

  :config
  (use-package doom-modeline
    ;; https://github.com/seagle0128/doom-modeline#use-package
    :ensure t
    :hook (after-init . doom-modeline-mode)
    )
  )


(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . lsp)
  )

;; markdown preview
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
;; use pandoc for markdown-preview
;; (setq markdown-command "/usr/local/bin/pandoc")
;; change looks
(setq markdown-preview-stylesheets (list "github.css"))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish
  :init (smartparens-global-mode)
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish
  :init (global-undo-tree-mode)
  )
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; change quit key to C-q
(global-set-key (kbd "C-q") 'keyboard-quit)

;; highlight current line
(global-hl-line-mode t)

;; highlight parenthesis
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(set-face-attribute 'show-paren-match nil
                     :background "turquoise"
                     :underline "turquoise")

(use-package rainbow-delimiters
  :ensure t
  :disabled
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; highlight indent
(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode

  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)

  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-suppress-auto-error t)
  )

(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; show row numbers

;; highlight target region
(transient-mark-mode t)
(use-package volatile-highlights
  :ensure t
  :diminish
  :config (volatile-highlights-mode t)
  )
(put 'set-goal-column 'disabled nil)

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; ivy&counsel: completion interface
(use-package ivy
  :defer 0.1
  :ensure t
  :diminish

  :config
  (ivy-mode)

  (use-package counsel
    :ensure t
    :diminish

    :config
    (counsel-mode)
    )
  )

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ctp$"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml?$"  . web-mode))

(setq web-mode-content-type-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; indent
(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; indent
  (setq web-mode-html-offset   2)
  (setq web-mode-style-padding 2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)

  ;; auto tag closing
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-tag-highlight t)
)
(add-hook 'web-mode-hook 'web-mode-hook)
(put 'dired-find-alternate-file 'disabled nil)

;; rust setting (from https://emacs-jp.github.io/env/rust)
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;;; rust
(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :ensure t
  :hook ((rust-mode . cargo-minor-mode)
	 (rust-mode . lsp))
  )

;;; LSP: language server protocol settings ;;;
(use-package lsp-mode
  :ensure t
  ;; :hook (prog-mode . lsp)
  :hook
  (python-mode . lsp)
  (rust-mode . lsp)

  :config
  (use-package lsp-ui :ensure t)
  (use-package helm-lsp :ensure t)
  (use-package lsp-treemacs :ensure t)
  (use-package lsp-origami :ensure t)
  (use-package lsp-pyright :ensure t)
  (use-package lsp-origami :ensure t)
  )

(use-package company
  :ensure t
  :diminish

  :config
  (global-company-mode)

  :custom
  (company-idle-delay            0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers          t)
  (company-tng-auto-configure  nil)
  )

(use-package yasnippet
  :ensure t
  :diminish
  :config (yas-global-mode)
  )

;; flycheck - syntax checker
(use-package flycheck
  :ensure t
  :init (setq flycheck-global-modes
	      '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
	      flycheck-indication-mode (if (display-graphic-p)
                                           'left-fringe
                                         'left-margin)
	      )
  :hook (after-init . global-flycheck-mode)
  )

(use-package dap-mode
  :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package docker-compose-mode :ensure t)

(use-package yaml-mode :ensure t)

;; Terraform
(use-package company-terraform
  :ensure t
  :config (company-terraform-init)
  )
(use-package terraform-mode
  :ensure t
  :custom (terraform-format-on-save-mode t)
  :hook (terraform-mode . company-mode)
  )
(use-package terraform-doc :ensure t)

(use-package prescient
  :ensure t
  :diminish

  :if (package-installed-p 'company)
  :config (use-package company-prescient :ensure t)

  :if (package-installed-p 'ivy)
  :config (use-package ivy-prescient :ensure t)
  )

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
