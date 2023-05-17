;; Get Melpa Repo.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(require 'dired-x)
(require 'dashboard)

;; Misc. Config. ;;
(setenv "PATH" (concat (getenv "PATH") ":/home/zdh/.cargo/bin"))
(add-hook 'dired-mode-hook (lambda () (whitespace-mode -1)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(display-time-mode 1)
(setq whitespace-style (quote (face spaces tabs space-mark tab-mark)))
(global-whitespace-mode 1)
(setq-default indent-tab-mode nil)
(show-paren-mode 1)
(setq make-backup-files nil)
(setq display-line-numbers-type 'relative)
(setq inhibit-startup-screen t)
(setq-default show-trailing-whitespace t)
(scroll-bar-mode -1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(global-display-line-numbers-mode)
(set-face-background 'trailing-whitespace "yellow")
(set-frame-font "Fira Code-15")
(windmove-default-keybindings)


;; Package Config. ;;
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "www.gnu.org / Emacs version 27.1")
(setq dashboard-startup-banner "~/Pictures/emacs_fancy_logos/xemacs_color.svg")
(setq dashboard-footer-messages '("I showed you my code, pls respond (ﾉಥДಥ)ﾉ ︵┻━┻･/"))
(setq dashboard-items nil)
(ido-mode 1)
(ido-everywhere 1)
(good-scroll-mode 1)
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq global-auto-complete-mode t)
(setq mc/always-run-for-all t)
(smartparens-global-mode t)
(golden-ratio-mode)
(global-auto-complete-mode 1)


;; Keybindings. ;;
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-.") 'mark-word)
(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-x m") 'delete-other-windows)
(global-set-key (kbd "C-x '") 'shell)

(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "M-j") 'crux-top-join-line)
(global-set-key (kbd "C-h") 'crux-kill-whole-line)
(global-set-key (kbd "C-x C-u") 'crux-upcase-region)
(global-set-key (kbd "C-x C-l") 'crux-downcase-region)
(global-set-key [(shift return)] #'crux-smart-open-line)

(global-set-key (kbd "M-s M-s") 'helm-swoop)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-tnis)
(global-set-key (kbd "C-?") 'mc/skip-to-next-like-this)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x /") 'split-window-horizontally)

(global-key-binding (kbd "M-x") #'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c f") 'imenu-list)
(global-set-key (kbd "C-=") 'er/expand-region)


;; Functions. ;;
(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'untabify-except-makefiles)

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
