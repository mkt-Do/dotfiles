;; assign C-h to backspace
(keyboard-translate ?\C-h ?\C-?)

;; don't backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; hidden menu-bar
(menu-bar-mode 0)

;; hidden startup message
(setq inhibit-startup-message 1)

;; delete initial scratch message
(setq initial-scratch-message "")

;; indent (space)
(setq-default indent-tabs-mode nil)

;; highlight paren
(show-paren-mode 1)

;; complement paren
(electric-pair-mode 1)

;; view line number (color like solalized dark)
(progn
  (global-display-line-numbers-mode)
  (set-face-attribute 'line-number nil
                      :foreground "#004354"
                      :background "#000000")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "Gold"))

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives  '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; straight.el setting by myself
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; fallback use-package to straight.el
(setq straight-use-package-by-default t)

;; hiwin
(use-package hiwin)
(hiwin-activate)

;; typescript
(use-package typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; tide (typescript補完)
(use-package tide)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save enabled))
            (eldoc-mode t)
            (company-mode-on)))

;; helm setting
(use-package helm)
(require 'helm-config)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; auto-complete
(use-package auto-complete)
(require 'auto-complete-config)
(ac-config-default)
;; complete by TAB key
(ac-set-trigger-key "TAB")
;; run automatically
(global-auto-complete-mode t)

;; undo-tree
;; run automatically
(use-package undo-tree)
(global-undo-tree-mode t)
;; assign M-/ to redo
(global-set-key (kbd "M-/") "undo-tree-redo")

;; ENSIME
(use-package ensime
  :ensure t
  :pin melpa-stable)
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))
;; use auto-complete
(setq ensime-completion-style 'auto-complete)
(defun scala/enable-eldoc ()
  (setq-local eldoc-documentation-function
              #'(lambda ()
                  (when (ensime-connected-p)
                    (let ((err (ensime-print-errors-at-point))) err))))
  (eldoc-mode +1))
(defun scala/completing-dot-company ()
  (cond (company-backend
         (company-complete-selection)
         (scala/completing-dot))
        (t
         (insert ".")
         (company-complete))))
(defun scala/completing-dot-ac ()
  (insert ".")
  (ac-trigger-key-command t))
;; check type
(bind-key "C-t"`ensime-type-at-point scala-mode-map)
;; interactive command
(defun scala/completing-dot ()
  (interactive "*")
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 's))
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (cond ((not (and (ensime-connected-p) ensime-completion-style))
         (insert "."))
        ((eq ensime-completion-style 'company)
         (scala/completing-dot-ac))))
;; initialization
(setq ensime-startup-snapshot-notification nil)
(add-hook 'ensime-mode-hook #'scala/enable-eldoc)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'flycheck-mode)
