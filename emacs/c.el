(add-hook 'c-mode-hook
          (lambda ()
            (interactive)
            (flycheck-mode 1)
            (local-set-key (kbd "C-c C-c") 'compile)
            (local-set-key (kbd "C-c h") 'ff-find-other-file)
            (eldoc-mode 1)
            (lsp)
            (setq c-default-style "linux"
                  c-basic-offset 4)))

(use-package nasm-mode
  :hook (asm-mode . nasm-mode)
  :bind ("C-c C-c" . compile))
