(setq inhibit-startup-message 1)

;; specify base directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; python major mode
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\\.py\\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; flycheck - error check
(defun my/turn-on-flycheck-mode ()
  (flycheck-mode 1))
(add-hook 'python-mode-hook 'my/turn-on-flycheck-mode)

;; jedi - completion for python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; auto complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; py-yapf - auto format
(require 'py-yapf)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;; helm - awesome tool
;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key (kbd "C-x C-r") 'helm-recentf) 
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring) 
;; (global-set-key (kbd "M-r") 'helm-occur)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (define-key global-map (kbd "C-c i")   'helm-imenu)

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z')')')

;; (require 'helm-swoop)
;; (global-set-key (kbd "M-o") 'helm-swoop)
;; (global-set-key (kbd "C-M-o") 'helm-multi-swoop)

(require 'iedit)
(global-set-key (kbd "C-x ;") 'iedit-mode)

(semantic-mode 1)
(add-hook 'python-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'python-imenu-create-index)))

;; jedi
(setq load-path (cons "~/emacs.d/elpa" load-path))
(require 'epc)
(require 'auto-complete-config)
(require 'python)
(setenv "PYTHONPATH" "~/.pyenv/versions/3.6.5/lib/python3.6/site-packages")
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; neotree
(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

;; color theme
(require 'rebecca-theme)
(load-theme 'rebecca t)

;; markdown preview
(require 'markdown-preview-mode)
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
(show-paren-mode t)
;; (setq show-paren-style 'parenthesis)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "lightgreen")
(set-face-foreground 'show-paren-match-face "navy")

;; highlight target region
(transient-mark-mode t)
(require 'volatile-highlights)
(volatile-highlights-mode t)

(put 'set-goal-column 'disabled nil)
