(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Vanilla settings.
(setq make-backup-files                nil)
(setq display-line-numbers-type       'relative)
(setq inhibit-startup-screen           t)
(setq split-height-threshold           9999)
(setq split-width-threshold            nil)
(setq-default show-trailing-whitespace t)
(scroll-bar-mode      -1)
(ido-mode              1)
(menu-bar-mode         0)
(tool-bar-mode         0)
(delete-selection-mode 1)
(global-display-line-numbers-mode)
(set-face-background 'trailing-whitespace "yellow")
(set-frame-font "Fira Code-15")

;; Other variables.
;;; Good Scroll.
(good-scroll-mode               1)
;;; Dired.
(setq-default dired-dwim-target t)
(setq dired-listing-switches    "-alh")
;;; Auto Complete.
(setq global-auto-complete-mode t)

;; Keybind remaps.
;;; Emacs.
(global-set-key (kbd "C-q")         'undo)
(global-set-key (kbd "C-c c")       'compile)
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
(global-set-key (kbd "C-c C-c")     'execute-extended-command)
;;; Imenu.
(global-set-key (kbd "C-c f")       'imenu-list)

;; Custom Functions.
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))

;; Other.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(good-scroll rust-mode imenu-list magit vterm crux gruber-darker-theme helm-swoop move-text multiple-cursors smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
