;; don't show staring message
(setq inhibit-startup-message 1)

;; user C-h as backspace
(keyboard-translate ?\C-h ?\C-?)

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

;; jedi
(setq load-path (cons "~/emacs.d/elpa" load-path))
(require 'epc)
(require 'auto-complete-config)
(require 'python)
(setenv "PYTHONPATH" "~/.pyenv/versions/3.6.5/lib/python3.6/site-packages")
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; company config
;; (require 'company)
;; (global-company-mode) ; activate all buffer
;; (setq company-idle-delay 0) ;; Trigger completion immediately.
;; (setq company-minimum-prefix-length 2)
;; (setq company-selection-wrap-around t)
;; ;; Number the candidates (use M-1, M-2 etc to select completions).
;; (setq company-show-numbers t)
;; ;; Use the tab-and-go frontend.
;; ;; Allows TAB to select and complete at the same time.
;; (company-tng-configure-default)
;; (setq company-frontends
;;       '(company-tng-frontend
;;         company-pseudo-tooltip-frontend
;;         company-echo-metadata-frontend))

;; (define-key company-active-map (kbd "M-n") nil)
;; (define-key company-active-map (kbd "M-p") nil)
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "C-h") nil)

;; ;; company tabnine config
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)

;; ;; Company python mode
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my/python-mode-hook)


(require 'iedit)
(global-set-key (kbd "C-x ;") 'iedit-mode)

(semantic-mode 1)
(add-hook 'python-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'python-imenu-create-index)))


;; neotree
(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

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
;; (setq show-paren-style 'parenthesis)
;; (setq show-paren-style 'expression)
(setq show-paren-style 'mixed)
;; (set-face-background 'show-paren-match-face "lightgreen") ;; legacy setting
;; (set-face-foreground 'show-paren-match-face "navy")
(set-face-attribute 'show-paren-match nil
                     :background "turquoise"
                     :underline "turquoise")
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; show row numbers

;; highlight target region
(transient-mark-mode t)
(require 'volatile-highlights)
(volatile-highlights-mode t)

(put 'set-goal-column 'disabled nil)
