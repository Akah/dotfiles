(customize-set-variable
 'package-archives '(("org" . "https://orgmode.org/elpa/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defun default-prog-mode-setup ()
  (display-line-numbers-mode)
  (column-number-mode)
  (add-hook 'focus-out-hook 'garbage-collect))

(defun up-scroll ()
  "move cursor up one line and scroll the screen with it"
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (scroll-down-line))
  (previous-line))

(defun down-scroll ()
  "move cursor down one line and scroll the screen with it"
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (scroll-up-line))
  (next-line))

(defun swap-vertical-buffers ()
  (interactive)
  (let* ((this-win (selected-window))
         (next-win (next-window))
         (this-buf (window-buffer this-win))
         (next-buf (window-buffer next-win)))
    (set-window-buffer next-win this-buf)
    (set-window-buffer this-win next-buf)))

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun laptop ()
  (interactive)
  (if (eq system-type 'darwin)
      (set-face-attribute 'default nil :height 110)
    (set-face-attribute 'default nil :height 100)))

(defun monitor ()
  (interactive)
  (if (eq system-type 'darwin)
      (set-face-attribute 'default nil :height 105)
    (set-face-attribute 'default nil :height 100)))

(use-package emacs
  :hook (prog-mode . default-prog-mode-setup)
  :bind (("C-x O" . prev-window)
         ("M-[" . backward-paragraph)
         ("M-]" . forward-paragraph)
         ("s-<up>" . up-scroll)
         ("s-<down>" . down-scroll)
         ("C-x r" . revert-buffer))
  :config
  (monitor)
  (delete-selection-mode t)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (electric-pair-mode)
  (setq gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024) ;; 1mb
        require-final-newline t
        ring-bell-function nil
        auto-save-default nil
        make-backup-files nil
        create-lockfiles nil
        use-package-always-ensure t
        use-package-always-defer nil
        warning-minimum-level :emergency)
  (setq-default indent-tabs-mode nil
                line-spacing 2)
  (when (eq system-type 'darwin)
    (menu-bar-mode 1)
    (server-start)))

(use-package diminish)

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1))

(use-package ripgrep)

(use-package projectile-ripgrep)

(use-package projectile
  :after ripgrep projectile-ripgrep
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "^\\.node_modules$")
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t))

(use-package all-the-icons)
(use-package treemacs-all-the-icons)
(use-package treemacs
  :bind ("C-x t" . treemacs)
  :config
  (treemacs-resize-icons 14)
  (treemacs-load-theme "all-the-icons"))

(use-package dashboard
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-week-agenda t)
  (setq dashboard-center-content t)
  (setq org-agenda-files '())
  (setq dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-time)
  (setq dashboard-items '((recents  . 15)
                          (projects . 10)
                          (agenda .  10)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

((lambda ()
   (use-package doom-themes)
   (use-package vscode-dark-plus-theme)
   (defun disable-all-themes ()
     "disable all active themes."
     (dolist (i custom-enabled-themes)
       (disable-theme i))
     ;;(set-face-attribute 'whitespace-space nil :background nil :foreground "gray90")
     )
   (defun light-mode ()
     (interactive)
     (disable-all-themes)
     (if (eq system-type 'darwin)
         (load-theme 'doom-gruvbox-light t)))
   (defun dark-mode ()
     (interactive)
     (disable-all-themes)
     (if (eq system-type 'darwin)
         (load-theme 'vscode-dark-plus t)
       (load-theme 'doom-dracula t)))
   (dark-mode)))

(use-package centaur-tabs
  :bind (("C-x c" . centaur-tabs-mode))
  :config
  (setq centaur-tabs-set-icons nil)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-modified-marker "*"))

;; (use-package whitespace
;;   :diminish whitespace-mode
;;   :config
;;   (set-face-attribute 'whitespace-space nil :background nil :foreground nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; general software ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :preface
  (defun my/vterm ()
    (interactive)
    (if (one-window-p)
        (split-window-horizontally))
    (vterm)
    (swap-vertical-buffers)
    (other-window 1))
  :bind (("M-ESC" . my/vterm)))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package why-this
  :bind ("C-x y" . why-this)
  :config
  (setq why-this-idle-delay 0.5)
  (set-face-foreground why-this-face
                       (face-attribute font-lock-comment-face :foreground)))

(use-package git-gutter
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after (git-gutter)
  :config
  (define-fringe-bitmap
    'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap
    'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap
    'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; prog  stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; seems to fuck up syntax highlighting in web-mode
;; (use-package rainbow-mode
;;   :diminish rainbow-mode
;;   :hook (prog-mode . rainbow-mode)

(use-package company
  :diminish company-mode
  :config
  (global-company-mode t)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1))

(use-package lsp-mode
  :diminish (lsp-mode)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((web-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred lsp
  :config
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (add-to-list 'lsp-file-watch-ignored-directories "\\.node_modules\\"))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode)

(use-package eldoc
  :diminish eldoc-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sudo-edit
  :bind ("C-c C-r" . sudo-edit))

(use-package drag-stuff
  :bind (("M-<up>"   . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package multiple-cursors
  :bind (("C-c c" . mc/edit-lines)
         ("M-<down-mouse-1>" . nil)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config (define-key mc/keymap (kbd "<return>") nil))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; work specific stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package kotlin-mode)

(use-package json-mode)

(use-package groovy-mode
  :mode (("\\.gradle\\'" . groovy-mode)))

(use-package objc-mode
  :mode (("\\.mm" . objc-mode)))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" .  web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :commands web-mode)
