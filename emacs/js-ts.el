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
                      ;; (setq electric-pair-pairs (append electric-pair-pairs '((?' . ?'))))
                      (setq column-enforce-column 160)
                      (setq whitespace-line-column 160)
                      (setq comment-start "\/\/"
                            comment-end "")
                      (flycheck-mode 1)
                      (eldoc-mode 1)
                      (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                                (string-equal "ts" (file-name-extension buffer-file-name)))
                        (lsp-mode)
                        (local-set-key (kbd "C-c C-c" #'compile))
                        (flycheck-add-mode 'eslint 'webmode))))
  :commands web-mode)
