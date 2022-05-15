;;; init.el --- Emacs init file

;; Created by daigo0927
;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(docker-compose-mode dockerfile-mode rust-mode cargo cargo-mode pandoc go-autocomplete go-eldoc go-mode yaml-mode web-mode yasnippet company-quickhelp company-lsp helm-lsp lsp-python lsp-sh lsp-ui lsp-mode multi-web-mode counsel company-tabnine company company-anaconda volatile-highlights markdown-preview-mode neotree python rebecca-theme smartparens undo-tree markdown-mode iedit helm-swoop python-mode helm flycheck)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "color-46"))))
 '(font-lock-doc-face ((t (:foreground "brightcyan"))))
 '(font-lock-type-face ((t (:foreground "color-183"))))
 '(font-lock-variable-name-face ((t (:foreground "color-189"))))
 '(show-paren-match ((t (:background "color-51" :underline "turquoise"))))
 '(web-mode-current-element-highlight-face ((t (:background "color-41" :foreground "green")))))

;;; custom.el ends here
