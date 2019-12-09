;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" default)))
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   (quote
    (slime-company magit markdown-mode company tide slime lsp-java zenburn-theme treemacs-icons-dired treemacs)))
 '(safe-local-variable-values
   (quote
    ((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval when
	   (require
	    (quote rainbow-mode)
	    nil t)
	   (rainbow-mode 1)))))
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(window-divider-default-places (quote bottom-only)))

(global-display-line-numbers-mode 1)

(global-prettify-symbols-mode 1)

(electric-pair-mode 1)

(company-mode 1)

(require 'diff-hl)
(global-diff-hl-mode)

(set-face-attribute 'default nil :height 100)

(add-hook 'java-mode-hook #'lsp)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

; for lisp
(load
 (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(load-theme 'zenburn t)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'treemacs)
(treemacs-resize-icons 14)
