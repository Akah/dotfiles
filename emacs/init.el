(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defun emacs ()
  (interactive)
  (find-file "~/code/dotfiles/emacs/init.el"))

(defun remacs ()
  (interactive)
  (call-process "remacs" nil 0 nil))

(defun swap-vertical-buffers ()
  (interactive)
  (let* ((this-win (selected-window))
         (next-win (next-window))
         (this-buf (window-buffer this-win))
         (next-buf (window-buffer next-win)))
    (set-window-buffer next-win this-buf)
    (set-window-buffer this-win next-buf)))

(custom-set-variables
 '(gnu-algorithm-priority "normal:-vers-tls1.3"))

(setq package-check-signature nil)

(require 'package)

(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(defun __rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-title)))

(add-hook 'exwm-update-class-hook '__rename-buffer)
(add-hook 'exwm-update-title-hook '__rename-buffer)

(setq exwm-enabled (seq-contains command-line-args "--use-exwm"))

(use-package exwm
  :if exwm-enabled
  :config
    ;; set keyboard layouts and toggle shortcut
  (async-shell-command "setxkbmap -layout gb,de -option 'grp:alt_shift_toggle'")
  (display-time-mode 1)
  (setq exwm-workspace-number 5)
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

(use-package doom-themes
  :ensure t
  :config (progn
            (defun dark-mode ()
              (interactive)
              (load-theme 'doom-badger))
            (defun light-mode ()
              (interactive)
              ;; alternatively use defaul theme: (disable-theme 'doom-badger))
              (load-theme 'doom-gruvbox-light))
            (load-theme 'doom-badger)))

(use-package emacs
  :hook (prog-mode . default-prog-mode-setup)
  :bind (
         ("C-x O" . prev-window)
         ("M-[" . backward-paragraph)
         ("M-]" . forward-paragraph))
  :config
  (defun default-prog-mode-setup ()
    (whitespace-mode)
    (display-line-numbers-mode)
    (column-number-mode))
  (defun prev-window ()
    (interactive)
    (other-window -1))
  (defun laptop ()
    (interactive)
    (set-face-attribute 'default nil :height 100))
  (defun monitor ()
    (interactive)
    (set-face-attribute 'default nil :height 90))
  (defalias 'open 'find-file)
  (defalias 'openo 'find-file-other-window)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (global-auto-revert-mode 1)
  (set-face-attribute 'default nil :height 90)
  (set-fontset-font "fontset-default" 'cyrillic "DejaVu Sans Mono")
  (setq use-package-always-ensure t)
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (setq read-file-name-completion-ignore-case t)
  (setq whitespace-line-column 160)
  (setq pop-up-frames nil)
  (setq mouse-wheel-follow-mouse t)
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  ;;
  (setq-default line-spacing 2)
  (setq-default indent-tabs-mode nil)
  (setq-default whitespace-display-mappings
                '((space-mark 32 [183] [46])
                  (space-mark 160 [164] [95])
                  (tab-mark 9 [187 9] [92 9])))
  ;; enable emojis: requires font-noto-color-emoji package
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)))

(use-package yasnippet
  :config (yas-global-mode))

(use-package flycheck)

(use-package company
  :hook (prog-mode . company-mode))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package nyan-mode
  :config (nyan-mode))

(use-package treemacs-all-the-icons)

(use-package dashboard
  :config (progn
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
            (dashboard-setup-startup-hook)))

(use-package centaur-tabs
  :config (progn
            (centaur-tabs-mode t)
            (setq centaur-tabs-set-icons nil)
            (setq centaur-tabs-style "bar")
            (setq centaur-tabs-modified-marker "*")))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ivy
  :after orderless
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package drag-stuff
  :bind (("M-<up>"   . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package ripgrep)
(use-package projectile-ripgrep)
(use-package projectile
  :after ripgrep projectile-ripgrep
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t))

(use-package treemacs
  :bind ("C-x t" . treemacs)
  :config
  (treemacs-resize-icons 15)
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package which-key
  :config (which-key-mode))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode . lsp-mode))
  :commands lsp)

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

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
            (diminish 'company-mode)
            (diminish 'yas-global-mode)
            (diminish 'yas-minor-mode)))

(use-package anki-editor
  :config (progn
            (setq anki-editor-create-decks t)
            (setq anki-editor-org-tags-as-anki-tags t)))

;; LANGUAGE ADDITIONS
(add-to-list 'load-path "~/code/dotfiles/emacs/")
(load "c.el")
(load "lisp.el")
(load "js-ts.el")

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
;; set jkl; to be navigation keys; equivelant to hjkl but already on the home keys
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-l") 'next-line)
(global-set-key (kbd "M-;") 'forward-char) ; overwrites 'comment-dwim'

;; unset default arrow keys (temporarily until I'm used to `jkl;`')
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))
;; (global-unset-key (kbd "<C-left>"))
;; (global-unset-key (kbd "<C-right>"))
;; (global-unset-key (kbd "<C-up>"))
;; (global-unset-key (kbd "<C-down>"))
;; (global-unset-key (kbd "<M-left>"))
;; (global-unset-key (kbd "<M-right>"))
;; (global-unset-key (kbd "<M-up>"))
;; (global-unset-key (kbd "<M-down>"))
