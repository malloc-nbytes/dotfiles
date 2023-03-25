(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq make-backup-files nil)

(require 'pdf-tools)

(require 'auto-complete)

(require 'crux)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "M-j") 'crux-top-join-line)
(global-set-key (kbd "C-h") 'crux-kill-whole-line)
(global-set-key (kbd "C-x C-u") 'crux-upcase-region)
(global-set-key [(shift return)] #'crux-smart-open-line)

(require 'helm-swoop)
(global-set-key (kbd "M-s M-s") 'helm-swoop)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-tnis)
(global-set-key (kbd "C-q") 'undo)

(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; imenu-list (see functions)
(global-set-key (kbd "C-c .") 'imenu-list)

;; disable scroll bar
(scroll-bar-mode -1)

(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "C-c c") 'compile)

;; Split windows.
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x /") 'split-window-horizontally)

(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)

(setq global-auto-complete-mode t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(delete-selection-mode 1)

(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "yellow")

(global-key-binding (kbd "M-x") #'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c") 'execute-extended-command)

(setq split-height-threshold 9999)
(setq split-width-threshold nil)

(global-set-key (kbd "C-x m") 'delete-other-windows)

(global-set-key (kbd "C-x '") 'vterm)


(ido-mode 1)
;; (set-frame-font "Ubuntu Mono")
(set-frame-font "Fira Code")

(global-set-key (kbd "C-c f") 'imenu-list)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))
