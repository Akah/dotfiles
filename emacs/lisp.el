(use-package ac-slime)

(use-package paredit
  :init (autoload 'enable-paredit-mode "paredit" t)
  :hook ((common-lisp-mode-hook . paredit-mode)
         (emacs-lisp-mode-hook  . paredit-mode)
         (scheme-mode-hook      . paredit-mode)
         (clojure-mode-hook     . paredit-mode)))

(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\" return.")

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

(defun lisp-setup ()
  (interactive)
  (paredit-mode t)
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (show-paren-mode nil))

(add-hook 'common-lisp-mode-hook
          (lambda ()
            (interactive)
            (local-set-key (kbd "RET") 'electrify-return-if-match)))

(load
 (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
