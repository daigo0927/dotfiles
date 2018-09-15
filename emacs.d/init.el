(prefer-coding-system 'utf-8)
;;カラム番号も表示
(column-number-mode t)

(add-to-list 'load-path "~/.emacs.d/elisp")

;; ;; シェルのPATHを引き継ぐ
;; (autoload 'exec-path-from-shell "exec-path-from-she" nil t)
;; (exec-path-from-shell-initialize)

;;タイトルバーにフルパスを表示
(setq frame-title-fomat "%f")

;;TABの表示幅を4に設定
(setq-default tab-width 4)

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; emacs-thema
;; use Atom thema
(setq custom-theme-directory "~/.emacs.d/elpa/atom-one-dark-theme-20170117.1905")
(load-theme 'atom-one-dark t)


;; GUIではAtom風テーマ
;; (if window-system (progn
;; 	;; Atom風テーマ
;; 	(setq custom-theme-directory "~/.emacs.d/elpa/atom-one-dark-theme-20170117.1905")
;; 	(load-theme 'atom-one-dark t)
					
;; 	))

;; (if (not window-system) (progn
;; 						  ;; プリインストールテーマ
;; 						  (load-theme 'wombat t)
;; 						  ))

;;背景色文字色
;; (set-face-background 'default "black")
;;(set-face-foreground 'default "lightgreen")
;;選択時の背景色文字色
(set-face-background 'region "white")
(set-face-foreground 'region "black")
;;カッコを対応させる
(show-paren-mode t)

(which-function-mode 1)



;set clearness
(set-frame-parameter nil 'alpha 75)

;; AutoInstall
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))


;;auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
			   "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

(defun electric pair ()
	   "Insert character pair without sorounding space"
	   (interactive)
	   (let(pares-require-spaces)
		 (insert-pair)))


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; emacs-python auto-complete
(require 'epc)
(require 'python)
(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(require 'set-pyenv-version-path)
;;(add-hook 'find-file-hook 'set-pyenv-version-path)
;;(add-to-list 'exec-path "~/.pyenv/shims")

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-to-list 'load-path "~/.emacs.d/yasnippet/")
(require 'yasnippet)
(yas/global-mode 1)

;; emacs-java auto-complete
(add-to-list 'load-path "~/.emacs.d/ajc-java-complete/")
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; auto spell check
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;;
(global-set-key (kbd "C-M-$") 'ispell-complete-word)

;; emacs-js auto-complete
(add-hook 'js2-mode-hook
		  '(lambda ()
			 (when (locate-library "tern")
			   (setq tern-command '("tern" "--no-port-file")) ;; .term-port を作らない
			   (tern-mode t)
			   (eval-after-load 'tern
				 '(progn
					(require 'tern-auto-complete)
					(tern-ac-setup)))
			   )
			 ))


(autoload 'js2-mode "js2-mode" nil t)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
;; js2-jsx-modeでflycheckを有効にする
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
(add-hook 'js2-jsx-mode-hook 'flycheck-mode)

;; js2-jsx-modeでauto-complete-modeを有効にする
(add-hook 'emacs-lisp-mode-hook '(lambda ()
								   (require 'auto-complete)
								   (auto-complete-mode t)
								   ))
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'js2-jsx-mode)

;; neotree
(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neotree (if (display-graphic-p) 'icons 'arrow))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(markdown-preview-eww markdown-preview-mode pyenv-mode exec-path-from-shell auto-virtualenvwrapper virtualenvwrapper python-mode yasnippet-snippets neotree jedi flycheck ess atom-one-dark-theme ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq markdown-command "pandoc")
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
(setq markdown-preview-stylesheets (list "github.css"))
