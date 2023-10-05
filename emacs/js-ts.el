;; (defun setup-tide-mode ()
;;   "."
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode 1)
;;   (eldoc-mode 1)
;;   (tide-hl-identifier-mode 1)
;;   (column-enforce-mode 1)
;;   (white-space-mode nil)
;;   (setq white-space-line-column 160)
;;   (flycheck-add-mode 'typescript-tslint 'webmode)
;;   (lsp)
;;   (setq column-enforce-column 160)
;;   (setq whitespace-line-column 160)
;;   (whitespace-mode 1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   ;; (message "lsp mode set"))

;; (use-package tide
;;   :after web-mode lsp-mode
;;   :hook (typescript-mode . #'setup-tide-mode))

(use-package dap-mode
  :ensure t)

;;for react-native
(use-package groovy-mode
  :mode (("\\.gradle\\'" . groovy-mode)))

(use-package web-mode
  :mode (("\\.js\\'"   . web-mode)
         ("\\.jsx\\'"  . web-mode)
         ("\\.ts\\'"   . web-mode)
         ("\\.tsx\\'"  . web-mode)
         ("\\.html\\'" . web-mode))
  :hook (web-mode . (lambda ()
                      (interactive)
                      (setq column-enforce-column 160)
                      (setq whitespace-line-column 160)
                      (flycheck-mode 1)
                      (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                                (string-equal "ts" (file-name-extension buffer-file-name)))
                        (lsp-mode)
                        (local-set-key (kbd "C-c C-c" #'compile))
                        (flycheck-add-mode 'eslint 'webmode))))
  :commands web-mode)
