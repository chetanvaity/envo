;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;;; Don't show the welcome message
(setq inhibit-startup-message t)

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
(menu-bar-mode 0)

;; Chetan:
(add-to-list 'load-path "~/.emacs.d/elisp")
(setq auto-mode-alist (cons '("\\.inc$" . php-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xsd$" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))

;; Markdown mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/markdown-mode"))
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files")
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Puppet files
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

 
;;(set-frame-font "-b&h-lucida-bold-r-normal-sans-14-100-100-100-p-89-iso10646-1")
;;(set-frame-font "-b&h-lucida-medium-r-normal-sans-14-100-100-100-p-80-iso10646-1")
(set-frame-font "-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso8859-1")
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
(require 'color-theme)
;(color-theme-goldenrod) ; Dark bg - golden fonts
(color-theme-dark-laptop) ; Excellent
;(color-theme-hober) ; Nice
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
;(color-theme-midnight) ; Black bg, darkish fonts - not bad
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
;(setq-default indent-tabs-mode nil)
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

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
