(require 'package)

(setq package-list '(markdown-mode
                     auto-complete
                     coffee-mode
                     haskell-mode
                     less-css-mode
                     web-mode
                     rust-mode
                     json-mode
                     yaml-mode
                     evil
                     evil-leader
                     evil-surround
                     dash-at-point
                     ag
                     smartparens
                     smart-tab
                     helm
                     helm-projectile
                     projectile
                     magit
                     git-gutter
                     solarized-theme
                     monokai-theme
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
(setq auto-save-file-name-transforms
       `((".*" , (expand-file-name (concat dotfiles-dir "bak")) t)))

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

(set-default-font "Monaco-13")
(define-key global-map (kbd "RET") 'newline-and-indent)

; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'server)
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(unless (server-running-p)
  (server-start))

(require 'haskell-mode)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.purs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'python-mode-hook
          (lambda ()
            (setq electric-indent-mode nil)
            (dolist (c (string-to-list "_")
                       (modify-syntax-entry c "w" python-mode-syntax-table)))))

(require 'web-mode)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.jst\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


(require 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(coffee-mode . "Web"))

;; (require 'smart-tab)
;; (global-smart-tab-mode 1)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-modes 'coffee-mode)

; When executing shell commands from emacs, set PATH correctly.
(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments
  (delete "-i" exec-path-from-shell-arguments))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'helm-config)
 (global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)


(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  "." 'eval-buffer
  "d" 'dash-at-point
  "," 'helm-projectile-find-file
  "c" 'comment-or-uncomment-region
  "g" 'magit-status
  "w" 'split-window-horizontally
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "n" 'other-window
  "p"  (lambda () (interactive) (other-window -1))
)

(require 'evil)
(evil-mode t)
(setq-default evil-shift-width 2)

(require 'evil-surround)
(global-evil-surround-mode 1)

(smartparens-global-mode t)

(require 'git-gutter)
(git-gutter:linum-setup)
(global-git-gutter-mode t)
(add-to-list 'git-gutter:update-hooks 'after-save-hook)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "cbef37d6304f12fb789f5d80c2b75ea01465e41073c30341dc84c6c0d1eb611d" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(package-selected-packages
   (quote
    (yaml-mode pylint helm-ag ac-coffee auto-complete zenburn-theme web-mode solarized-theme smex smartparens smart-tab sass-mode rust-mode rainbow-mode rainbow-delimiters noctilux-theme monokai-theme markdown-mode magit less-css-mode json-mode helm-projectile haskell-mode golden-ratio git-gutter gist flx-ido f exec-path-from-shell evil-surround evil-leader direx dash-at-point coffee-mode cider ag)))
 '(recentf-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'monokai)
