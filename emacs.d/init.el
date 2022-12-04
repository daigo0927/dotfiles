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

(use-package diminish :ensure t)

;; python major mode
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; flycheck - syntax checker
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode)

  :hook
  (unless (window-system)
    (diff-hl-mode . diff-hl-margin-local-mode)
    )
  )

;; company mode
(use-package company
  :ensure t
  :diminish company-mode

  :custom
  (company-idle-delay            0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers          t)
  (company-tng-auto-configure  nil)

  :config
  (global-company-mode)
  (company-tng-mode)

  :bind
  (:map company-active-map
	("M-n" . nil)
	("M-p" . nil)
	("C-n" . 'company-select-next)
	("C-p" . 'company-select-previous)
	("C-h" . nil)
	("<tab>" . company-complete-common-or-cycle))
  (:map company-search-map
	("C-n" . 'company-select-next)
	("C-p" . 'company-select-previous)
	("C-h" . 'company-search-delete-char)
	("<space>" . nil)
	("RET" . 'company-complete-selection)
	("<return>" . 'company-complete-selection))

  ;; :config
  ;; Show pretty icons <- disable for suppress company-box unexistent bug
  ;; (use-package company-box
    ;; :diminish
    ;; : hook (company-mode . company-box-mode)
    ;; :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    ;; :config
    ;; (setq company-box-backends-colors nil)
    ;; (setq company-box-show-single-candidate t)
    ;; (setq company-box-max-candidates 50)
    ;; )
  )

;; company-tabnine
(use-package company-tabnine
  :ensure t
  :config (add-to-list 'company-backends #'company-tabnine))

;; py-yapf - auto format
;; (require 'py-yapf)
;; (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

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
(require 'rebecca-theme)
(load-theme 'rebecca t)

;; markdown preview
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
;; use pandoc for markdown-preview
(setq markdown-command "/usr/local/bin/pandoc")
;; change looks
(setq markdown-preview-stylesheets (list "github.css"))

;; smartparens
(require 'smartparens)
(smartparens-global-mode t)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
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
(require 'volatile-highlights)
(volatile-highlights-mode t)

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
(require 'ivy)
(ivy-mode 1) ;; set default input completion to ivy
(counsel-mode 1) ;; Remap basic command like (M-x, C-x, C-f, ...) to ivy

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

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;; LSP: language server protocol settings ;;;
;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init (yas-global-mode)
  :hook (rust-mode . lsp)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom ((lsp-ui-doc-enable              nil)
	   (lsp-ui-doc-header              t)
	   (lsp-ui-flycheck-live-reporting t)
	   (lsp-ui-sideline-enable         nil)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred)

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

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
