(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 ;;'(custom-enabled-themes '(doom-badger))
 '(custom-safe-themes
   '("6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175"
     "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea"
     "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c"
     "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5"
     "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf"
     "40b961730f8d3c63537d6c3e6601f15c6f6381b9239594c7bf80b7c6a94d3c24"
     "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae"
     "db3e80842b48f9decb532a1d74e7575716821ee631f30267e4991f4ba2ddf56e"
     "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607"
     "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703"
     "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c"
     "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5"
     "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63"
     default))
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#5B6268")
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   '(projectile-ripgrep
     ripgrep
     all-the-icons-dired
     git-gutter
     centaur-tabs paredit
     multiple-cursors
     company
     treemacs-icons-dired
     treemacs-all-the-icons
     diminish
     sudo-edit
     web-mode exwm
     all-the-icons
     dashboard
     tide
     lsp-mode
     which-key
     doom-themes
     ivy
     use-package
     treemacs
     projectile
     magit
     drag-stuff
     diff-hl
     column-enforce-mode
     ac-slime))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(require-final-newline nil)
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
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil
                         :extend nil
                         :stipple nil
                         :background "#171717"
                         :foreground "#F6F3E8"
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         :height 120
                         :width normal
                         :foundry "PfEd"
                         :family "Terminus")))))

(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun setup-tide-mode ()
  "."
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (column-enforce-mode 1)
  (white-space-mode nil)
  (setq white-space-line-column 160)
  (flycheck-add-mode 'typescript-tslint 'webmode)
  (lsp)
  (setq column-enforce-column 160)
  (setq whitespace-line-column 160)
  (whitespace-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (message "lsp mode set"))

(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
      (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))

(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(package-initialize)
(require 'package)

;; setup for exwm:
;; https://github.com/ch11ng/exwm/wiki
(setq use-package-always-ensure t
      exwm-enabled (seq-contains command-line-args "--use-exwm"))

(defun aaa-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-title)))

(add-hook 'exwm-update-class-hook 'aaa-rename-buffer)
(add-hook 'exwm-update-title-hook 'aaa-rename-buffer)

(use-package exwm
  :if exwm-enabled
  :config
  (display-time-mode 1)
  (setq exwm-workspace-number 5)
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
  (require 'exwm-randr)
  (exwm-randr-enable)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  (exwm-enable))

(use-package flycheck)

(use-package company)

(use-package web-mode)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package ivy
  :ensure t
  :config (ivy-mode 1))

(use-package git-gutter
  :hook (prog-mode-hook . git-gutter-mode))

(use-package drag-stuff
  :bind (("M-<up>"   . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package all-the-icons
  :config (unless (package-installed-p 'all-the-icons)
            (all-the-icons-install-fonts)))

(use-package ripgrep)
(use-package projectile-ripgrep)
(use-package projectile
  :after ripgrep projectile-ripgrep
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t))

(use-package treemacs
  :bind ("C-x t" . treemacs)
  :config (treemacs-resize-icons 15))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package ac-slime)

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-badger))

(use-package which-key
  :config (which-key-mode))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode . lsp-mode))
  :commands lsp)

(use-package tide
  :after web-mode
  :hook (typescript-mode . #'setup-tide-mode))

(use-package dashboard
  :after (all-the-icons)
  :config (progn
            (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
            (setq dashboard-center-content t)
            (setq dashboard-items '((recents  . 10)
                                    (projects . 5)))
            (setq dashboard-set-heading-icons t)
            (setq dashboard-set-file-icons t)
            (dashboard-setup-startup-hook)))

(use-package sudo-edit
  :bind ("C-c C-r" . sudo-edit))

(use-package multiple-cursors
  :bind (("C-c c" . mc/edit-lines)
         ("M-<down-mouse-1>" . nil)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config (define-key mc/keymap (kbd "<return>") nil))

(use-package diminish
  :config (progn
            (diminish 'projectile-mode)
            (diminish 'lsp-mode)
            (diminish 'ivy-mode)
            (diminish 'eldoc-mode)
            (diminish 'whitespace-mode)
            (diminish 'which-key-mode)
            (diminish 'flycheck-mode)
            (diminish 'abbrev-mode)
            (diminish 'company-mode)))

(defun lisp-setup ()
  (interactive)
  (paredit-mode t)
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (show-paren-mode nil))

(use-package paredit
  :init (autoload 'enable-paredit-mode "paredit" t)
  :hook ((common-lisp-mode-hook . paredit-mode)
         (emacs-lisp-mode-hook  . paredit-mode)
         (scheme-mode-hook      . paredit-mode)
         (clojure-mode-hook     . paredit-mode)))

(use-package centaur-tabs
  :demand
  :config (centaur-tabs-mode t)
  :bind (("C-x <left>" . centaur-tabs-backward)
         ("C-x <right>" . centaur-tabs-forward)))

;; HOOKS

(add-hook 'common-lisp-mode-hook
          (lambda ()
            (interactive)
            (local-set-key (kbd "RET") 'electrify-return-if-match)))

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (whitespace-mode)
            (diff-hl-mode)
            (display-line-numbers-mode)
            (company-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'lsp)

(add-hook 'web-mode-hook
          (lambda ()
            (interactive)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (lsp)
              (setup-tide-mode))))

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

;; DEFAULTS
;; Global keybindings
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(show-paren-mode 1)
(electric-pair-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq read-file-name-completion-ignore-case t)
(setq whitespace-line-column 160)
(setq split-height-threshold nil split-width-threshold nil)
(setq require-final-newline nil)
(setq-default line-spacing 2)
(setq-default indent-tabs-mode nil)
(setq-default whitespace-display-mappings
              '((space-mark 32 [183] [46])
                (space-mark 160 [164] [95])
                (tab-mark 9 [187 9] [92 9])))

;; COMMANDS

(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(defun laptop ()
  (interactive)
  (set-face-attribute 'default nil :height 100))

(defun dark-mode ()
  (interactive)
  (load-theme 'doom-badger))

(defun light-mode ()
  (interactive)
  (load-theme 'doom-gruvbox-light))

(defun monitor ()
  (interactive)
  (set-face-attribute 'default nil :height 100))

(load
 (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "ecl")

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(setq with-arrows nil)
(defun toggle-arrows ()
  (interactive)
  (setq with-arrows (not with-arrows))
  (unless with-arrows
    (global-unset-key (kbd "<left>"))
    (global-unset-key (kbd "<right>"))
    (global-unset-key (kbd "<up>"))
    (global-unset-key (kbd "<down>"))
    (global-unset-key (kbd "<C-left>"))
    (global-unset-key (kbd "<C-right>"))
    (global-unset-key (kbd "<C-up>"))
    (global-unset-key (kbd "<C-down>"))
    (global-unset-key (kbd "<M-left>"))
    (global-unset-key (kbd "<M-right>"))
    (global-unset-key (kbd "<M-up>"))
    (global-unset-key (kbd "<M-down>"))))

