;; =========================================================================
;; Setup use-package
;; =========================================================================

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;;Don't attempt to load archive if it already exists
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; =========================================================================
;; Look and feel
;; =========================================================================
(setq inhibit-startup-message t)

;; Minimal IU
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Add a boarder around the window
(menu-bar-mode -1)     ; Disable the menu bar

;; Display line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(setq make-backup-files nil) ; Turn off ~Filename

(setq visible-bell t)  ; Use visible bells instead of audio ones

;; Note: Must Come before doom-modeline
;;
;; Note: First time on new machine, must run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; use the escape key to quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Doom theme
(use-package doom-themes
  :config (load-theme 'doom-zenburn t))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better minibuffer menus
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Switch buffers by hitting C-M-j then using the arrow keys
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Shows what the possible key combinations are
;; Activates and `which-key-idle-delay` seconds
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package counsel
  :bind (("M-x" . counsel-M-x) ;; alt-o when using M-x to see extra options
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Replace default help programs
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; write customize interface settings out to custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; I don't know how to get general to work
;; (use-package general)

;; ========================================================================
;; Hydra
;; ========================================================================
(use-package hydra)
;; hydra command for navigating windows
(defhydra hydra-navigate-windows (global-map "C-f")
  "navigate"
  ("<left>" windmove-left "left")
  ("<right>" windmove-right "right")
  ("<up>" windmove-up "up")
  ("<down>" windmove-down "down"))
;; hydra command for scrolling buffers
(defhydra hydra-switch-buffers (global-map "C-x")
  "buffer"
  ("<left>" previous-buffer "prev")
  ("<right>" next-buffer "next"))

;; ========================================================================
;; Project Management
;; ========================================================================
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/")
    (setq projectile-project-search-path '("~/")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Better  Ivy / Counsel integration
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

;; ========================================================================
;; Programming Languages
;; ========================================================================

;; config c++-mode
(setq auto-mode-alist (append '(("\\.cc" . c++-mode)
                                ("\\.inl" . c++-mode)
                                ("\\.hh$" . c++-mode)
                               ) auto-mode-alist))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c++-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))

;; Bind clang-format to Control-Meta-tab
(load "/usr/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-buffer)

;; Python
(setq auto-mode-alist (append '(("\\.asl" . python-mode)
                                ("BUILD" . python-mode)
                                ("\\.wafl" . python-mode)
                                ) auto-mode-alist))
(add-hook 'python-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Docker
(use-package dockerfile-mode :mode "Dockerfile\\'")
