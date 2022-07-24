;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (firebelly)))
 '(custom-safe-themes
   (quote
    ("95a3219337da075d8d03938f8d5394718c2a6b7409a53b818d032b37dc0b56e2" "90fc97b270630aa2d452a597ce1d5ba5508aa8cc9e9b5f77c58ed0a3b136910a" "cf6f7f3e8c24cf271008d2e16f3e449cc6c8a80e641706a83fe708bdafca2bcc" "542ab4e358f00ea866a7697f3e6f5661a16cf5d9087962c9334c75afab9ce6e0" "fb02341256e724afa2a7eb75e650ea5e99446afaf0876bc88ce368ef7d04f263" "0046cebeffcf09cd9700b123baaca04ae10154df824e3a63f9b7d02b3fc6e4a1" "fb9cde30661f8f5f13ef63bf0013b8b4c3128947f611990b4f6975c3f77fa3e5" "ec5f761d75345d1cf96d744c50cf7c928959f075acf3f2631742d5c9fe2153ad" "6383f86cac149fb10fc5a2bac6e0f7985d9af413c4be356cab4bfea3c08f3b42" "12dd37432bb454355047c967db886769a6c60e638839405dad603176e2da366b" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" default)))
 '(fci-rule-color "#383838")
 '(global-display-line-numbers-mode t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (slime-company magit markdown-mode company tide slime lsp-java zenburn-theme treemacs-icons-dired treemacs)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
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
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(window-divider-default-places (quote bottom-only)))

;; Global settings:

(global-display-line-numbers-mode 1)
(global-prettify-symbols-mode 1)
(electric-pair-mode 1)
(auto-complete-mode 1)
(flycheck-mode 1)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

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
  "."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
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
  '(add-to-list 'ac-modes 'slime-repl-mode))

(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "C-x C-g p") 'magit-pull-from-upstream)
(global-set-key (kbd "C-x t") 'treemacs)

(require 'treemacs)
(treemacs-resize-icons 14)

(require 'gruber-darker-theme)
(load-theme 'gruber-darker-theme)

(load
 (expand-file-name "~/js-import.el"))

;;; .emacs ends here
