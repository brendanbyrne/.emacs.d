(require 'package)
(package-initialize)

;; =============================================================================
;; everything below in an org file?
;; =============================================================================

;; where to look for package updates
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; Setup use-package if I don't already have it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

;; stop creating ~files
(setq make-backup-files nil)

;; =============================================================================
;; Look and feel
;; =============================================================================

;; default values
(setq-default
 ad-redefinition-action 'accept                 ; Silence warnings for redefinition
 column-number-mode 1                           ; Show the column number
 cursor-in-non-selected-windows t               ; Hide the cursor in inactive windows
 display-time-default-load-average nil          ; Don't display load average
 fill-column 80                                 ; Set width for automatic line breaks
 help-window-select t                           ; Focus new help windows when opened
 inhibit-startup-screen t                       ; Disable start-up screen
 initial-scratch-message ""                     ; Empty the initial *scratch* buffer
 load-prefer-newer t                            ; Prefers the newest version of a file
 make-backup-files nil                          ; Don't create ~FileName
 mouse-wheel-scroll-amount '(1 ((shift) . 1))   ; Scroll one line at a time
 mouse-wheel-progressive-speed nil              ; Disable mouse scroll wheel acceleration
 scroll-conservatively most-positive-fixnum     ; Always scroll by one line
 scroll-set 1                                   ; Keyboard scroll one line at a time
 select-enable-clipboard t                      ; Merge system's and Emacs' clipboard
 tab-width 2                                    ; Set width for tabs
 use-package-always-ensure t                    ; Avoid the :ensure keyword for each package
 user-full-name "Brendan Byrne"                 ; Set the full name of the current user
 user-mail-address "brendan.c.byrne@gmail.com"  ; Set the email address of the current user
 vc-follow-symlinks t                           ; Always follow the symlinks
 view-read-only t)                              ; Always open read-only buffers in view-mode
(cd "~/")                                       ; Move to the user directory
(display-time-mode 1)                           ; Enable time in the mode-line
(global-hl-line-mode)                           ; Hightlight current line
(set-default-coding-systems 'utf-8)             ; Default to utf-8 encoding
(show-paren-mode 1)                             ; Show the parent

;; Zenburn color theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Turn off mouse centric UI elements
(when window-system
	(menu-bar-mode -1)    ; Disable the menu bar
  (scroll-bar-mode -1)  ; Disable the scroll bar
  (tool-bar-mode -1)    ; Disable the tool bar
  (tooltip-mode -1))    ; Disable the tooltips

;; =============================================================================
;; C++
;; =============================================================================

;; config c++-mode
(setq auto-mode-alist (append '(("\\.cc" . c++-mode)
                                ("\\.inl" . c++-mode)
                                ("\\.hh$" . c++-mode)
                               ) auto-mode-alist))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c++-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(use-package flycheck)
(use-package company)

(use-package lsp-mode
	:hook (prog-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)
(use-package company-lsp)
(use-package ccls
  :after projectile
  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))

;; Bind clang-format to Control-Meta-tab
(load "/usr/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)

;; =============================================================================
;; CMake
;; =============================================================================

(use-package cmake-mode
	:config
	(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
	(add-hook 'cmake-mode-hook (lambda () (setq indent-tabs-mode nil)))
)

;; =============================================================================
;; Docker
;; =============================================================================

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; =============================================================================
;; Autogenerated code
;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
