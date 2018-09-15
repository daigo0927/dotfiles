(add-to-list 'load-path "~/.emacs.d/elisp")
(prefer-coding-system 'utf-8)
;;カラム番号も表示
(column-number-mode t)

;;タイトルバーにフルパスを表示
(setq frame-title-fomat "%f")

;;TABの表示幅を4に設定
(setq-default tab-width 4)

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; 既存のテーマをロード
(load-theme 'wombat t)

;;背景色文字色
;;(set-face-background 'default "black")
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
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)



