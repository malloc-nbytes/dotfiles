;; Get Melpa Repo.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'dired-x)
(global-set-key (kbd "C-x C-j")         'dired-jump)

(display-time-mode 1)
(setq whitespace-style (quote (face spaces tabs space-mark tab-mark)))
(global-whitespace-mode 1)

(setq-default indent-tab-mode nil)

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'untabify-except-makefiles)

;; (setq split-height-threshold           9999)
;; (setq split-width-threshold            nil)

;; Vanilla settings.
(show-paren-mode 1)
(setq make-backup-files                nil)
(setq display-line-numbers-type       'relative)
(setq inhibit-startup-screen           t)
(setq-default show-trailing-whitespace t)
(scroll-bar-mode      -1)
(ido-mode              1)
(ido-everywhere        1)
(ido-ubiquitous-mode   1)
(menu-bar-mode         0)
(tool-bar-mode         0)
(delete-selection-mode 1)
(global-display-line-numbers-mode)
(set-face-background 'trailing-whitespace "yellow")
(set-frame-font "Fira Code-15")
(windmove-default-keybindings)

;; Other variables.
;;; Good Scroll.
(good-scroll-mode               1)
;;; Dired.
(setq-default dired-dwim-target t)
(setq dired-listing-switches    "-alh")
;;; Auto Complete.
(setq global-auto-complete-mode t)
;;; Expand Region.
(setq alphabet-start            "abc def")
;;; Multiple Cursors.
(setq mc/always-run-for-all     t)
;;; Smart Parens.
(smartparens-global-mode        t)
;;; Golden Ratio
(golden-ratio-mode)

;; Keybind remaps.
;;; Emacs.
(global-set-key (kbd "C-q")         'undo)
(global-set-key (kbd "C-;")         'previous-buffer)
(global-set-key (kbd "C-'")         'next-buffer)
(global-set-key (kbd "C-x m")       'delete-other-windows)
(global-set-key (kbd "C-x '")       'shell)

;;; Crux.
(global-set-key (kbd "C-c e")       'crux-eval-and-replace)
(global-set-key (kbd "C-c d")       'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c k")       'crux-kill-other-buffers)
(global-set-key (kbd "M-j")         'crux-top-join-line)
(global-set-key (kbd "C-h")         'crux-kill-whole-line)
(global-set-key (kbd "C-x C-u")     'crux-upcase-region)
(global-set-key (kbd "C-x C-l")     'crux-downcase-region)
(global-set-key [(shift return)]    #'crux-smart-open-line)
;;; Helm.
(global-set-key (kbd "M-s M-s")     'helm-swoop)
;;; Multiple Cursors.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-tnis)
(global-set-key (kbd "C-?")         'mc/skip-to-next-like-this)
;;; Move Text.
(global-set-key (kbd "M-p")         'move-text-up)
(global-set-key (kbd "M-n")         'move-text-down)
;;; Split windows.
(global-set-key (kbd "C-x -")       'split-window-vertically)
(global-set-key (kbd "C-x /")       'split-window-horizontally)
;;; Smex.
(global-key-binding (kbd "M-x")     #'smex)
(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "M-X")         'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-l")     'execute-extended-command)
;;; Imenu.
(global-set-key (kbd "C-c f")       'imenu-list)
;;; Auto Complete.
(global-auto-complete-mode           1)
;;; Expand Region.
(global-set-key (kbd "C-.") 'er/expand-region)

;; Custom Functions.
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))

(defun compile-and-switch-to-compilation-buffer ()
  "Compile and switch to the compilation buffer, making it full-screen."
  (interactive)
  (setq compilation-read-command t)
  (let ((compilation-buffer-name "*compilation*")
        (buffer-window (selected-window)))
    (call-interactively 'compile)
    (delete-other-windows buffer-window)
    (switch-to-buffer compilation-buffer-name)))
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c M-c") 'compile-and-switch-to-compilation-buffer)

(sp-local-pair 'prog-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


;; Other.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(easy-repeat-command-list
   '(other-window next-buffer scroll-other-window recenter-top-bottom kill-buffer backward-page forward-page previous-error next-error scroll-up-command scroll-down-command beginning-of-defun end-of-defun org-previous-visible-heading org-next-visible-heading org-forward-heading-same-level org-backward-heading-same-level outline-up-heading outline-next-visible-heading outline-previous-visible-heading outline-forward-same-level outline-backward-same-level git-gutter:previous-hunk git-gutter:next-hunk paredit-forward paredit-backward paredit-backward-up enlarge-window-horizontally shrink-window-horizontally enlarge-window))
 '(ido-max-prospects 25)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(ido-completing-read+ golden-ratio smartparens expand-region auto-complete good-scroll rust-mode imenu-list magit vterm crux gruber-darker-theme helm-swoop move-text multiple-cursors smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
