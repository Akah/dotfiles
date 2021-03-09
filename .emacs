(require 'package)

(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun handle-whitespace ()
  (interactive)
  ;(whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(package-refresh-contents)
(require 'use-package)
(setq use-package-always-ensure t)

(use-package magit)
(use-package column-enforce-mode)
(use-package diff-hl)
(use-package drag-stuff)
(use-package projectile)
(use-package treemacs)

(use-package tide)
(use-package flycheck)
(use-package eldoc)
;;(use-package auto-complete :ensure t)
(use-package web-mode)

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(global-diff-hl-mode 0)
(show-paren-mode 1)
(electric-pair-mode 1)
(projectile-mode +1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(handle-whitespace)
(treemacs-resize-icons 14)
(set-face-attribute 'default nil :height 90)
(setq column-enforce-comments nil
      column-number-mode 1
      indent-tabs-mode nil
      c-default-style "linux"
      c-basic-offset 4
      display-line-numbers 'relative)

(global-set-key (kbd "C-x C-g")   #'magit-status)
(global-set-key (kbd "C-x t")    #'treemacs)
(global-set-key (kbd "M-<up>")   #'drag-stuff-up)
(global-set-key (kbd "M-<down>") #'drag-stuff-down)
(global-set-key (kbd "C-c C-c")  #'compile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(add-hook 'c-mode-hook
	  (lambda ()
	    (interactive)
	    (flycheck-mode 1)
	    ;;(auto-complete-mode 1)
	    (local-set-key (kbd "C-c C-c") 'compile)
	    (column-enforce-mode 1)
	    (handle-whitespace)
	    (setq column-enforce-column 80)))

(defun setup-tide-mode ()
  "."
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  ;;(auto-complete-mode 1)
  (column-enforce-mode 1)
  (handle-whitespace)
  (setq column-enforce-column 160
	flycheck-check-syntax-automatically '(save mode-enabled)))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (interactive)
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'webmode)

(load
 (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;; .emacs ends here

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (cider clojure-mode ac-slime slime pdf-tools company-irony irony company auto-complete web-mode tide magit treemacs projectile diff-hl use-package drag-stuff doom-themes column-enforce-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'doom-one)
