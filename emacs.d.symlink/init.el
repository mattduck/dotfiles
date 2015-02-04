;;;; Download and install packages
;; =============================================================================

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Packages to install on launch
(defvar my-required-packages
  '(ace-jump-mode
    color-theme
    evil
    fill-column-indicator
    outline-magic
    org
    undo-tree))

(package-initialize)

(defun my-refresh-packages ()
  (interactive)
  
  (when (not package-archive-contents)
    (package-refresh-contents))

  (package-initialize)

  (dolist (p my-required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;; Add "non-elpa" dir to load-path
;; =============================================================================

(let ((base (concat (file-name-directory load-file-name) "non-elpa")))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base)) ; Include top-level sub-directories
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;;;; Add to custom-theme-load-path
;; =============================================================================
(add-to-list 'custom-theme-load-path "non-elpa/color-theme-solarized")

;;;; Load external files
;; =============================================================================

(load-file (concat (getenv "DOTFILES") "/splitscreen/splitscreen.el"))

;;;; Backups
;; =============================================================================

;; Backup everything to the same directory, rather than dropping
;; files all over the place
(if (getenv "DOTFILES")
    (setq backup-directory-alist 
          `(("." . ,(concat (getenv "DOTFILES") "/emacs.d.symlink/.backups")))))

;;;; OS X
;; =============================================================================

;; Map modifier keys so that all are accessible, but the left option key
;; is kept free so can use it for character modifications, eg. alt+3 = #. 
;;
;; CTRL = ctrl
;; LEFT ALT = none
;; COMMAND = meta
;; RIGHT ALT = super
(if (eq system-type 'darwin)
    (setq ns-option-modifier nil
          ns-command-modifier 'meta
          ns-right-option-modifier 'super))

;;;; Custom
;; =============================================================================

(setq custom-file (concat (file-name-directory load-file-name) "custom.el"))
(load custom-file 'noerror)

(savehist-mode 1)

;;;; Text wrap
;; =============================================================================

;; Automatic text wrapping
(defvaralias 'auto-fill-mode 'auto-fill-function) ; Otherwise something breaks looking for auto-fill-mode all the time
(define-globalized-minor-mode global-auto-fill-mode auto-fill-mode turn-on-auto-fill)
(global-auto-fill-mode 1)
(setq-default fill-column 80)

;;;; Indentation
;; =============================================================================

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(electric-indent-mode 1) ; Auto indent code

;; Python indentation is handled differently - taken from
;; www.emacswiki.org/emacs/AutoIndentation
(defun electric-indent-ignore-python (char)
  (if (equal major-mode 'python-mode) `no-indent' nil))
(defun set-newline-and-indent ()
 (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'electric-indent-functions 'electric-indent-ignore-python)
(add-hook 'python-mode-hook 'set-newline-and-indent)

;; Stop indenting in org-mode, as it adds indents all over the place and
;; is generally annoying.
(defun fix-electric-indent-in-org-mode ()
  (setq-local electric-indent-functions (list (lambda (arg) 'no-indent))))
(add-hook 'org-mode-hook 'fix-electric-indent-in-org-mode)

;;;; Display
;; =============================================================================

;; Colour column
(require 'fill-column-indicator)
(setq fci-rule-width 5) ; 5 seems to be max width
(add-hook 'solarized-theme-hook '(lambda () (setq fci-rule-color solarized-base02)))
;; Performance is too slow w/big files to enable this by default
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)

;; Line and Column number in mode-line. These seem to be global.
(line-number-mode 1)
(column-number-mode 1)

;; Highlight cursor line
(global-hl-line-mode 1)

;; Performance is too slow w/big files to enable this by default
;; (require 'linum) ; numbers in margin
;; (global-linum-mode nil)

(set-frame-font "Monaco-15:antialias=subpixel")

;; Necessary on v24.4 to display accurate Solarized colors, due to Emacs bug #8402.
;; v24.3 didn't set ns-use-sgrb-colorspace.
(setq ns-use-srgb-colorspace nil)
(setq solarized-broken-srgb t)

(setq visible-bell nil)

;;;; Parentheses
;; =============================================================================

;; Basic paren matching is built in. We want to enable the "matches..." messages
;; in the mini-buffer, but use show-paren-mode for more control over the
;; on-screen parens.
(require 'paren) ;; Load now to avoid invalid-face error
(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen nil)
(defun my-show-paren-solarized-faces ()
  (set-face-foreground 'show-paren-match solarized-blue)
  (set-face-background 'show-paren-mismatch solarized-red)
  (set-face-foreground 'show-paren-mismatch solarized-base03))
(add-hook 'solarized-theme-hook 'my-show-paren-solarized-faces)

;;;; Prog mode
;; =============================================================================

(defun my-prog-hook ()
  (show-paren-mode 1)
  (outline-minor-mode 1))
(add-hook 'prog-mode-hook 'my-prog-hook)

;;;; Evil
;; =============================================================================

(require 'evil)
(evil-mode 1)

;; I don't need to learn the default emacs undo system yet, this seems simpler.
(require 'undo-tree)

;; Use <esc> to cancel everything.
;; Taken from https://github.com/davvil/.emacs.d/blob/master/init.el.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Remap ";" to ":" and "," to ";", like vimrc
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map "," 'evil-repeat-find-char)

;; As recommended by emacswiki, take RET and SPC out of evil-motion-state-map
;; so it doesn't override bindings in eg. the profiler.
(defun my-move-key (keymap-from keymap-to key)
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; Closest thing to easymotion. I am using SPC for this atm, should maybe
;; swap to something else
(require 'ace-jump-mode)
(setq ace-jump-mode-scope 'window) ;; it's quicker this way
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-line-mode)


;;;; Outline
;; =============================================================================

;; Outline-magic
(defun my-outline-magic-mode-hook () (require 'outline-magic))
(defun my-outline-magic-minor-mode-hook ()
  (require 'outline-magic)
  ;; Provide cycle and positioning keys similar to org-mode
  (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)
  (define-key outline-minor-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
  (define-key outline-minor-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
  (define-key outline-minor-mode-map (kbd "<M-left>") 'outline-promote)
  (define-key outline-minor-mode-map (kbd "<M-right>") 'outline-demote))

(add-hook 'outline-mode-hook 'my-outline-magic-mode-hook)
(add-hook 'outline-minor-mode-hook 'my-outline-magic-minor-mode-hook)

;;;; Org
;; =============================================================================

(setq org-log-done 'time) ; Add timestamp when set task as closed
(setq org-agenda-restore-windows-after-quit t) ; Don't understand why this isn't default
(setq org-src-fontify-natively nil) ; Inline code has syntax highlighting
(setq org-level-color-stars-only nil) ; Don't colour the whole headline
(setq org-catch-invisible-edits "show-and-error") ; Prevent accidentally editing invisible lines
(setq org-M-RET-may-split-line nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cp" 'org-perspectives)

(defun my-org-solarized-faces () 
  (ignore-errors ; The font symbols don't exist until org-mode loaded
    (set-face-attribute 'org-level-1 nil :foreground solarized-blue)
    (set-face-attribute 'org-level-2 nil :foreground solarized-yellow)
    (set-face-attribute 'org-level-3 nil :foreground solarized-violet)
    (set-face-attribute 'org-level-4 nil :foreground solarized-cyan)
    (set-face-attribute 'org-level-5 nil :foreground solarized-blue)
    (set-face-attribute 'org-level-6 nil :foreground solarized-yellow)
    (set-face-attribute 'org-level-7 nil :foreground solarized-violet)
    (set-face-attribute 'org-level-8 nil :foreground solarized-cyan)

    (set-face-attribute 'org-date nil :foreground solarized-blue)
    (set-face-attribute 'org-sexp-date nil :foreground solarized-cyan)
    (set-face-attribute 'org-scheduled nil :foreground solarized-blue)
    (set-face-attribute 'org-scheduled-today nil :foreground solarized-orange)
    (set-face-attribute 'org-scheduled-previously nil :foreground solarized-blue)

    (set-face-attribute 'org-checkbox nil :foreground solarized-yellow)
    (set-face-attribute 'org-tag nil :foreground solarized-yellow :background solarized-base02)
    (set-face-attribute 'org-special-keyword nil :foreground solarized-green 
                        :background solarized-base02)

    (set-face-attribute 'org-code nil :foreground solarized-green)
    (set-face-attribute 'org-verbatim nil :foreground solarized-cyan)
    (set-face-attribute 'org-list-dt nil :foreground solarized-green)

    (set-face-attribute 'org-table nil :foreground solarized-base1)

    (set-face-attribute 'italic nil :foreground solarized-base1)
    (set-face-attribute 'bold nil :foreground solarized-base2)

    (set-face-attribute 'org-agenda-structure nil :foreground solarized-violet)
    (set-face-attribute 'org-agenda-date nil :foreground solarized-blue)
    (set-face-attribute 'org-agenda-date-today nil 
                        :foreground solarized-blue :weight 'bold :slant 'italic)
    (set-face-attribute 'org-warning nil :foreground solarized-orange)
    (set-face-attribute 'org-scheduled-previously nil :foreground solarized-cyan)
    (set-face-attribute 'org-scheduled nil :foreground solarized-cyan)
    (set-face-attribute 'org-upcoming-deadline nil 
                        :background nil :foreground solarized-red :weight 'bold :slant 'italic)

    (set-face-attribute 'warning nil :foreground solarized-red))

(defun my-org-hook ()
  ;; Change tab widths to fit headline indents
  (setq tab-width 2)  
  (setq evil-shift-width 2)

  ;; Disable in-buffer line numbers and the colur column, as both decrease
  ;; org-mode / outline performance significantly on large files.
  (linum-mode 0)
  (fci-mode 0)

  ;; Seems have to set these manually to make them buffer-local, unlike the
  ;; above mode functions. TODO - is this a bad thing?
  (setq-local line-number-mode 1)
  (setq-local column-number-mode nil)

  (my-org-solarized-faces))

(add-hook 'org-mode-hook 'my-org-hook)
(add-hook 'solarized-theme-hook 'my-org-solarized-faces)

;; Default to using my CSS theme for html exports
(setq org-html-head-extra
 "<link id='generic-css-dark' rel='stylesheet' type='text/css'
   href='https://mattduck.github.io/generic-css/css/generic-dark.css'> 
  <link id='generic-css-light' rel='stylesheet' type='text/css'
   href='https://mattduck.github.io/generic-css/css/generic-light.css'> 
  <script type='text/javascript'
   src='https://mattduck.github.io/generic-css/js/generic-css.js'></script>")
(setq org-export-headline-levels 6)
(setq org-export-with-section-numbers 4)


;;;; Solarized
;; =============================================================================
;; Load this last so any Solarized hooks run

(defun my-general-solarized-hook ()
  (setq evil-default-cursor t)
  (set-face-background 'cursor solarized-base1)

  ;; I think this is the easiest way to fontify all my buffers
  ;; after the changes. I always have font-lock-mode on anyway.
  (global-font-lock-mode 0)
  (global-font-lock-mode 1))
(add-hook 'solarized-theme-hook 'my-general-solarized-hook t)


;; Solarized Emacs custom theme setup
(require 'solarized-definitions)
(solarized-load-theme 'dark)
;;(solarized-load-theme 'light)

;; Solarized color-theme setup
;;(require 'color-theme)
;;(require 'color-theme-solarized)
;;(color-theme-initialize)
;;(color-theme-solarized-dark)
;;(color-theme-solarized-light)
