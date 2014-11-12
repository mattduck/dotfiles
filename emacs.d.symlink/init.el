;;;; Download and install packages
;; =============================================================================

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(package-initialize)

;; Packages to install on launch
(defvar my-required-packages
  '(ace-jump-mode
    color-theme
    evil
    fill-column-indicator
    outline-magic
    org
    undo-tree))

(dolist (p my-required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;;;; Load external files
;; =============================================================================

(load-file (concat (getenv "DOTFILES") "/splitscreen/splitscreen.el"))

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
(add-hook 'solarized-theme-hook '(lambda () (setq fci-rule-color sol-base02)))
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

(set-default-font "Inconsolata-15:antialias=subpixel")
(setq visible-bell nil)

;;;; Parentheses
;; =============================================================================

;; Basic paren matching is built in. We want to enable the "matches..." messages
;; in the mini-buffer, but use show-paren-mode for more control over the
;; on-screen parens.
(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen nil)
(defun my-show-paren-solarized-faces ()
  (set-face-foreground 'show-paren-match sol-blue)
  (set-face-background 'show-paren-mismatch sol-red)
  (set-face-foreground 'show-paren-mismatch sol-base03))
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
(setq org-src-fontify-natively t) ; Inline code has syntax highlighting
(setq org-level-color-stars-only t) ; Don't colour the whole headline
(setq org-catch-invisible-edits "show-and-error") ; Prevent accidentally editing invisible lines
(setq org-M-RET-may-split-line nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cp" 'org-perspectives)

(defun my-org-solarized-faces () 
  (ignore-errors ; The font symbols don't exist until org-mode loaded
    (set-face-attribute 'org-level-1 nil :foreground sol-blue)
    (set-face-attribute 'org-level-2 nil :foreground sol-yellow)
    (set-face-attribute 'org-level-3 nil :foreground sol-violet)
    (set-face-attribute 'org-level-4 nil :foreground sol-cyan)
    (set-face-attribute 'org-level-5 nil :foreground sol-blue)
    (set-face-attribute 'org-level-6 nil :foreground sol-yellow)
    (set-face-attribute 'org-level-7 nil :foreground sol-violet)
    (set-face-attribute 'org-level-8 nil :foreground sol-cyan)

    (set-face-attribute 'org-date nil :foreground sol-blue)
    (set-face-attribute 'org-upcoming-deadline nil :foreground sol-base1 :background sol-red)
    (set-face-attribute 'org-scheduled nil :foreground sol-blue)
    (set-face-attribute 'org-scheduled-today nil :foreground sol-orange)
    (set-face-attribute 'org-scheduled-previously nil :foreground sol-blue)

    (set-face-attribute 'org-checkbox nil :foreground sol-yellow)
    (set-face-attribute 'org-tag nil :foreground sol-yellow)

    (set-face-attribute 'org-code nil :foreground sol-green)
    (set-face-attribute 'org-verbatim nil :foreground sol-cyan)
    (set-face-attribute 'org-list-dt nil :foreground sol-green)

    (set-face-attribute 'bold nil :foreground sol-orange)))

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

;;;; Solarized
;; =============================================================================
;; Load this last so any Solarized hooks run

(defun my-general-solarized-hook ()
  (setq evil-default-cursor t)
  (set-face-background 'cursor sol-base1))
(add-hook 'solarized-theme-hook 'my-general-solarized-hook)

(require 'color-theme)
(require 'color-theme-solarized)
(eval-after-load "color-theme"
    '(progn
    (color-theme-initialize)
    (color-theme-solarized-dark)))
