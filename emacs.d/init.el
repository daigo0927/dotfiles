;; Emacs setting

;; don't show starting message
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

;; auto complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; py-yapf - auto format
(require 'py-yapf)
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;; jedi - completion for python
(setq load-path (cons "~/emacs.d/elpa" load-path))
(require 'epc)
(require 'auto-complete-config)
(require 'python)
(setenv "PYTHONPATH" "~/.pyenv/versions/3.6.5/lib/python3.6/site-packages")
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

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
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; show row numbers

;; highlight target region
(transient-mark-mode t)
(require 'volatile-highlights)
(volatile-highlights-mode t)

(put 'set-goal-column 'disabled nil)

;; ivy&counsel: completion interface
(ivy-mode 1) ;; set default input completion to ivy
(counsel-mode 1) ;; Remap basic command like (M-x, C-x, C-f, ...) to ivy

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
