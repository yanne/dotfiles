(require 'package)

(setq package-list '(cider
                     clojure-mode
                     markdown-mode
                     coffee-mode
                     less-css-mode
                     web-mode
                     evil
                     evil-leader
                     evil-surround
                     dash-at-point
                     ag
                     exec-path-from-shell
                     smartparens
                     smart-tab
                     flx-ido
                     smex
                     direx
                     projectile
                     rainbow-delimiters
                     magit
                     git-gutter
                     solarized-theme
                     zenburn-theme))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Write backup files to own directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

; General UI stuff
(global-linum-mode t)
(global-hl-line-mode t)
(setq inhibit-startup-message t)
(setq x-underline-at-descent-line t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq-default show-trailing-whitespace t)
(setq-default visible-bell 'top-bottom)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq column-number-mode t)

; Add characters considered part of word
(dolist (c (string-to-list ":_-?!#*"))
  (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

; Always load externally changed files
(global-auto-revert-mode 1)

; Save modified buffers on focus lost
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

; Well, why not
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(load-theme 'zenburn t)
(set-default-font "Monaco-12")

; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'server)
(unless (server-running-p)
  (server-start))
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

;; some custom filetype mappings
(add-to-list 'auto-mode-alist '("\\.jst\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))


(setq js-indent-level 2)
(setq css-indent-offset 2)
;; (add-hook 'js-mode-hook 'js2-minor-mode)

(require 'web-mode)

(require 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(coffee-mode . "Web"))

(require 'smart-tab)
(global-smart-tab-mode 1)

; When executing shell commands from emacs, set PATH correctly.
(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments
  (delete "-i" exec-path-from-shell-arguments))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; Smarter M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching 0)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'recentf)
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'eval-buffer
  "d" 'dash-at-point
  "," 'projectile-find-file
  "c" 'comment-or-uncomment-region
  "g" 'magit-status
  "w" 'save-buffer
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "n" 'other-window
  "p"  (lambda () (interactive) (other-window -1))
  )

(setq evil-want-C-u-scroll         t
      evil-want-C-w-in-emacs-state t)

(define-key evil-insert-state-map [left] 'undefined)
(define-key evil-insert-state-map [right] 'undefined)
(define-key evil-insert-state-map [up] 'undefined)
(define-key evil-insert-state-map [down] 'undefined)

(define-key evil-motion-state-map [left] 'undefined)
(define-key evil-motion-state-map [right] 'undefined)
(define-key evil-motion-state-map [up] 'undefined)
(define-key evil-motion-state-map [down] 'undefined)

(require 'evil)
(evil-mode t)
(setq-default evil-shift-width 2)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'rainbow-delimiters nil)
(global-rainbow-delimiters-mode t)

(require 'icomplete)

(smartparens-global-mode t)
; https://github.com/Fuco1/smartparens/wiki/Example-configuration

(require 'git-gutter)
(git-gutter:linum-setup)
(global-git-gutter-mode t)
(add-to-list 'git-gutter:update-hooks 'after-save-hook)
