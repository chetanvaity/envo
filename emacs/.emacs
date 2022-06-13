;; .emacs

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 125))

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;;; Don't show the welcome message
(setq inhibit-startup-message t)

;; For Mac OSX
(set-keyboard-coding-system nil)

;; Save and load desktop (previously open files)
;(desktop-save-mode 1)

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; Don't show the menu bar at the top - one more line of the buffer is visible - Yay!
(menu-bar-mode 1)

;; Chetan:
(add-to-list 'load-path "~/.emacs.d/elisp")
(setq auto-mode-alist (cons '("\\.inc$" . php-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xsd$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))
;; JSON mode for .tpl Terraform template files (useful for S3 policies)
(setq auto-mode-alist (cons '("\\.tpl$" . json-mode) auto-mode-alist))

;; Markdown mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/markdown-mode"))
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files")
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Puppet files
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; YAML files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Go Lang mode
(require 'go-mode-autoloads)

(set-frame-font
    "-outline-Courier New-bold-normal-normal-mono-20-*-*-*-c-*-iso8859-1")
;;(set-frame-font "-b&h-lucida-bold-r-normal-sans-14-100-100-100-p-89-iso10646-1")
;;(set-frame-font "-b&h-lucida-medium-r-normal-sans-14-100-100-100-p-80-iso10646-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso8859-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--20-140-100-100-c-100-iso8859-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1")
;;(set-frame-font "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-9")
;;(set-frame-font "-misc-fixed-bold-r-normal--13-100-100-100-c-70-iso8859-1")
;(set-background-color "black")
;(set-foreground-color "light yellow")

;;Redefine the complete-symbol key (Originally C-<TAB>, but it's used by the Window Manager to switch between windows)
(global-set-key '[(f2)] 'save-buffer)
(global-set-key '[(f5)] 'goto-line)
(global-set-key '[(f1)] 'tags-search)
(global-set-key '[(f9)] 'hide-body)
(global-set-key '[(f10)] 'show-all)
(global-set-key '[(f12)] 'save-buffers-kill-emacs)
;; Comment/Uncomment
(global-set-key '[(f6)] 'comment-region)
(global-set-key '[(f7)] 'uncomment-region)
;; [Home] & [End] key should take you to beginning and end of lines..
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;;(global-set-key "\C-p" 'complete-symbol)

;;Hide those temp files
(defun make-backup-file-name (filename)
  (expand-file-name
    (concat "." (file-name-nondirectory filename) "~")
    (file-name-directory filename)))

;; This plist-to-alist definition is needed for color themes to work in EMacs 24
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))

;; Color theme selection
;; Note that transperecncy does not work if color-theme is used
(require 'zenburn)
(require 'color-theme)
;(color-theme-goldenrod) ; Dark bg - golden fonts
;(color-theme-dark-laptop) ; Excellent
;(color-theme-hober) ; Nice
;(color-theme-zenburn) ; Classy, gray bg and subdued colours
;(color-theme-fischmeister)
;(color-theme-billw) ; Black bg, nice
;(color-theme-subtle-hacker) ; Dark green bg
;(color-theme-snow) ; White
;(color-theme-blippblopp) ; White
;(color-theme-whateveryouwant) ; White
;(color-theme-emacs-nw)
;(color-theme-matrix) ; Green fonts
;(color-theme-emacs-nw) ; White
;(color-theme-aliceblue) ; Bluish white
;(color-theme-blue-mood) ; Blue bg
;(color-theme-tty-dark) ; Good black bg, but bright font colors
;(color-theme-digital-ofs1) ; Light coffee bg
;(color-theme-robin-hood) ; Subtly sober green bg - looks good
(color-theme-midnight) ; Black bg, darkish fonts - not bad
;(color-theme-classic) ; Subtle pastel dark bluish bg - looks good
;(color-theme-taylor) ; Black bg - good for coding - Nice and subtle
;(color-theme-jsc-dark) ; Black bg - red-brown fonts - not bad
;(color-theme-pierson) ; Light brownish white bg
;(color-theme-rotor) ; Whitish bg
;(color-theme-pok-wob) ; Black bg - blue n yellow fonts
;(color-theme-sitaramv-solaris) ; Blue bg
;(color-theme-subtle-blue); Very blue BG - not good
;(color-theme-shaman) ; Green bg
;(color-theme-scintilla) ; Light gray bg, not bad
;(color-theme-resolve); Bright blue, worth trying
;(color-theme-raspopovic); Similar to resolve
;(color-theme-oswald) ; Nice black bg, looks bright, Good
;(color-theme-lethe) ; Black bg, bright, but comments different
;(color-theme-euphoria)
;(color-theme-dirac) ; Nice - but # comments in ruby problematic
;(color-theme-)

;; show the column number in the status line
(setq column-number-mode t)

;; Show entire path in the frame title
;(setq frame-title-format '((buffer-file-name "Emacs - %f") "%b"))
;(setq frame-title-format "%f")
(setq frame-title-format "Emacs")

;; Column marker
(require 'column-marker)
(add-hook 'java-mode-hook (lambda () (interactive) (column-marker-1 120)))
;(global-set-key [?\C-c ?m] 'column-marker-1)
;(add-hook 'ruby-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Whitespace sanitizer and Indentation stuff (Enabled on in Java)
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/ethan-wspace.el"))
;(require 'ethan-wspace)
;(add-hook 'java-mode-hook (lambda () (global-ethan-wspace-mode 1)))

;; God help me!!!
(setq-default indent-tabs-mode nil)
;(setq tab-width 4)
;(setq-default indent-tabs-mode t)

;;(require 'haml-mode)

;; auto-complete
;(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
;(ac-config-default)

;; yasnippet
;(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet-0.6.1c")
;(require 'yasnippet) ;; not yasnippet-bundle
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/elisp/yasnippet-0.6.1c/snippets")

;; ajc-java-complete
;(add-to-list 'load-path "~/.emacs.d/elisp/ajc-java-complete/")
;(require 'ajc-java-complete-config)
;(add-hook 'java-mode-hook 'ajc-java-complete-mode)
;(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; icicles
;(add-to-list 'load-path "~/.emacs.d/elisp/icicles/")
;(require 'icicles)
;(icy-mode 1)
;(global-set-key "\M-."     'icicle-find-first-tag)

;; global (a better TAGS package)
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'java-mode-hook '(lambda () (gtags-mode 1)) )
(global-set-key (kbd "M-.") 'gtags-find-tag)
(global-set-key (kbd "M-,") 'gtags-find-rtag)
(setq gtags-mode-hook
      '(lambda ()
        (setq gtags-path-style 'relative)))


;; YAML
;(require 'yaml-mode)
;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;(add-hook 'yaml-mode-hook
;          '(lambda ()
;             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;rsense
;(setq rsense-home "/usr/local/lib/rsense-0.3")
;(add-to-list 'load-path (concat rsense-home "/etc"))
;(require 'rsense)
;; rsense with autocomplete - Complete by C-c .
;(add-hook 'ruby-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))

;; Transparency (does not work with color-theme)
;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;(add-to-list 'default-frame-alist '(alpha 85 50))

;(eval-when-compile (require 'cl))
; (defun toggle-transparency ()
;   (interactive)
;   (if (/=
;        (cadr (frame-parameter nil 'alpha))
;        100)
;       (set-frame-parameter nil 'alpha '(100 100))
;     (set-frame-parameter nil 'alpha '(85 50))))
;(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Follow links version controlled files without asking
(setq vc-follow-symlinks t)

;; Java related
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             "Treat Java 1.5 @-style annotations as comments."
;;             (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
;;             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; buffer-move (useful in with C-x 3)
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; buffer-menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;;; Org mode
(setq org-agenda-files '("~/source/chetanvaity/org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-startup-truncated nil)
(setq org-todo-keywords
      '((sequence "TODO" "PROG" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("PROG" . "orange")))
(setq org-indent-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:foreground "gray27")))))
(setq org-hide-leading-stars t)
;;; End Org mode

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(json-navigator use-package json-mode dockerfile-mode exec-path-from-shell magit pcomplete-extension helm terraform-mode hcl-mode)))

(add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))

;;; Helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq helm-ff-skip-boring-files t)
(setq helm-ff-auto-update-initial-value t)

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h b") 'helm-resume)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;;; End Helm

;;; Shell stuff
(setq-default explicit-shell-file-name "/usr/local/bin/bash")
(require 'pcomplete-extension)
;;; End Shell stuff

;;; EShell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;;; End EShell

;;; iSpell
(setq-default ispell-program-name "/usr/local/bin/ispell")
;;; End iSpell

;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
;;; End Magit

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 125))

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;;; Don't show the welcome message
(setq inhibit-startup-message t)

;; For Mac OSX
(set-keyboard-coding-system nil)

;; Save and load desktop (previously open files)
;(desktop-save-mode 1)

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; Don't show the menu bar at the top - one more line of the buffer is visible - Yay!
(menu-bar-mode 1)

;; Chetan:
(add-to-list 'load-path "~/.emacs.d/elisp")
(setq auto-mode-alist (cons '("\\.inc$" . php-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xsd$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))
;; JSON mode for .tpl Terraform template files (useful for S3 policies)
(setq auto-mode-alist (cons '("\\.tpl$" . json-mode) auto-mode-alist))

;; Markdown mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/markdown-mode"))
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files")
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Puppet files
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; YAML files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Go Lang mode
(require 'go-mode-autoloads)

(set-frame-font
    "-outline-Courier New-bold-normal-normal-mono-20-*-*-*-c-*-iso8859-1")
;;(set-frame-font "-b&h-lucida-bold-r-normal-sans-14-100-100-100-p-89-iso10646-1")
;;(set-frame-font "-b&h-lucida-medium-r-normal-sans-14-100-100-100-p-80-iso10646-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso8859-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--20-140-100-100-c-100-iso8859-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1")
;;(set-frame-font "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-1")
;;(set-frame-font "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-9")
;;(set-frame-font "-misc-fixed-bold-r-normal--13-100-100-100-c-70-iso8859-1")
;(set-background-color "black")
;(set-foreground-color "light yellow")

;;Redefine the complete-symbol key (Originally C-<TAB>, but it's used by the Window Manager to switch between windows)
(global-set-key '[(f2)] 'save-buffer)
(global-set-key '[(f5)] 'goto-line)
(global-set-key '[(f1)] 'tags-search)
(global-set-key '[(f9)] 'hide-body)
(global-set-key '[(f10)] 'show-all)
(global-set-key '[(f12)] 'save-buffers-kill-emacs)
;; Comment/Uncomment
(global-set-key '[(f6)] 'comment-region)
(global-set-key '[(f7)] 'uncomment-region)
;; [Home] & [End] key should take you to beginning and end of lines..
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;;(global-set-key "\C-p" 'complete-symbol)

;;Hide those temp files
(defun make-backup-file-name (filename)
  (expand-file-name
    (concat "." (file-name-nondirectory filename) "~")
    (file-name-directory filename)))

;; This plist-to-alist definition is needed for color themes to work in EMacs 24
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))
;; show the column number in the status line
(setq column-number-mode t)

;; Show entire path in the frame title
;(setq frame-title-format '((buffer-file-name "Emacs - %f") "%b"))
;(setq frame-title-format "%f")
(setq frame-title-format "Emacs")

;; Column marker
(require 'column-marker)
(add-hook 'java-mode-hook (lambda () (interactive) (column-marker-1 120)))
;(global-set-key [?\C-c ?m] 'column-marker-1)
;(add-hook 'ruby-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Whitespace sanitizer and Indentation stuff (Enabled on in Java)
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/ethan-wspace.el"))
;(require 'ethan-wspace)
;(add-hook 'java-mode-hook (lambda () (global-ethan-wspace-mode 1)))

;; God help me!!!
(setq-default indent-tabs-mode nil)
;(setq tab-width 4)
;(setq-default indent-tabs-mode t)

;;(require 'haml-mode)

;; auto-complete
;(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
;(ac-config-default)

;; yasnippet
;(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet-0.6.1c")
;(require 'yasnippet) ;; not yasnippet-bundle
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/elisp/yasnippet-0.6.1c/snippets")

;; ajc-java-complete
;(add-to-list 'load-path "~/.emacs.d/elisp/ajc-java-complete/")
;(require 'ajc-java-complete-config)
;(add-hook 'java-mode-hook 'ajc-java-complete-mode)
;(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; icicles
;(add-to-list 'load-path "~/.emacs.d/elisp/icicles/")
;(require 'icicles)
;(icy-mode 1)
;(global-set-key "\M-."     'icicle-find-first-tag)

;; global (a better TAGS package)
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'java-mode-hook '(lambda () (gtags-mode 1)) )
(global-set-key (kbd "M-.") 'gtags-find-tag)
(global-set-key (kbd "M-,") 'gtags-find-rtag)
(setq gtags-mode-hook
      '(lambda ()
        (setq gtags-path-style 'relative)))


;; YAML
;(require 'yaml-mode)
;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;(add-hook 'yaml-mode-hook
;          '(lambda ()
;             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;rsense
;(setq rsense-home "/usr/local/lib/rsense-0.3")
;(add-to-list 'load-path (concat rsense-home "/etc"))
;(require 'rsense)
;; rsense with autocomplete - Complete by C-c .
;(add-hook 'ruby-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))

;; Transparency (does not work with color-theme)
;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;(add-to-list 'default-frame-alist '(alpha 85 50))

;(eval-when-compile (require 'cl))
; (defun toggle-transparency ()
;   (interactive)
;   (if (/=
;        (cadr (frame-parameter nil 'alpha))
;        100)
;       (set-frame-parameter nil 'alpha '(100 100))
;     (set-frame-parameter nil 'alpha '(85 50))))
;(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Follow links version controlled files without asking
(setq vc-follow-symlinks t)

;; Java related
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; buffer-move (useful in with C-x 3)
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; buffer-menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;;; Org mode
(setq org-agenda-files '("~/source/chetanvaity/org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-startup-truncated nil)
(setq org-todo-keywords
      '((sequence "TODO" "PROG" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("PROG" . "orange")))
(setq org-indent-mode t)

(setq org-hide-leading-stars t)
;;; End Org mode

(put 'narrow-to-region 'disabled nil)


(add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))

;;; Helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq helm-ff-skip-boring-files t)
(setq helm-ff-auto-update-initial-value t)

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h b") 'helm-resume)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;;; End Helm

;;; Shell stuff
(setq-default explicit-shell-file-name "/usr/local/bin/bash")
(require 'pcomplete-extension)
;;; End Shell stuff

;;; EShell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;;; End EShell

;;; iSpell
(setq-default ispell-program-name "/usr/local/bin/ispell")
;;; End iSpell

;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
;;; End Magit

;;; Dired
(require 'dired-x)
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)
        (dired-omit-mode)))
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

; Which files should not be seen in dired mode
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              (seq bol ".")
              ;(seq bol (one-or-more alnum) "~" eol)
              ;(seq bol ".." eol)
              )))
; How to sort the dired buffer
;
;;; End Dired

;;; Window look
(split-window-right)
(shrink-window-horizontally 20)
(find-file "~/source/")
;;;

;;; Poly-mode - Python with SQL
(use-package polymode
  :ensure t
  :mode ("\\.py\\'" . poly-python-sql-mode)
  :config
  (setq polymode-prefix-key (kbd "C-c n"))
  (define-hostmode poly-python-hostmode :mode 'python-mode)

  (define-innermode poly-sql-expr-python-innermode
    :mode 'sql-mode
    :head-matcher (rx "r" (= 3 (char "\"'")) (* (any space)))
    :tail-matcher (rx (= 3 (char "\"'")))
    :head-mode 'host
    :tail-mode 'host)

  (defun poly-python-sql-eval-chunk (beg end msg)
    "Calls out to `sql-send-region` with the polymode chunk region"
    (sql-send-region beg end))

  (define-polymode poly-python-sql-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-sql-expr-python-innermode)
    (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
    (define-key poly-python-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk)))
;;;

;;; Poly-mode - YAML with Jinja
(use-package polymode
  :ensure t
  :mode ("\\.yml\\'" . poly-yaml-jinja-mode)
  :config
  (setq polymode-prefix-key (kbd "C-c n"))
  (define-hostmode poly-yaml-hostmode :mode 'yaml-mode)

  (define-innermode poly-jinja-innermode
    :mode 'jinja-mode
    :head-matcher (rx (= 2 (char "{")) (* (any space)))
    :tail-matcher (rx (= 2 (char "}")))
    :head-mode 'body
    :tail-mode 'body)

  (defun poly-yaml-jinja-eval-chunk (beg end msg)
    "Do nothing"
    nil)

  (define-polymode poly-yaml-jinja-mode
    :hostmode 'poly-yaml-hostmode
    :innermodes '(poly-jinja-innermode)
    (setq polymode-eval-region-function #'poly-yaml-jinja-eval-chunk)
    (define-key poly-yaml-jinja-mode-map (kbd "C-c C-c") 'polymode-eval-chunk)))
;;;
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
