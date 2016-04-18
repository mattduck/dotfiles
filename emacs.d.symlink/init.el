;;;; Fix environment if called as GUI app
;; =============================================================================
(defun md/get-dotfiles-path ()
  ;; TODO - use exec-path-from-shell to get this? I only have the concat thing
  ;; here so it works on the OS X app.
  (or
    (getenv "DOTFILES")
    (concat (expand-file-name "~") "/dotfiles")))

;;;; Setup path
;; =============================================================================
;; If exec-path-from-shell is installed, use it to ensure that Emacs' exec-path
;; inherits all of the values from $PATH in the shell.
(when (and
       (memq window-system '(mac ns)) 
       (fboundp 'exec-path-from-shell-initialize))
  (exec-path-from-shell-initialize))

;;;; Reload
;; =============================================================================
(defun md/dotfiles-reload ()
    (interactive)
    (load-file (concat (md/get-dotfiles-path) "/emacs.d.symlink/init.el")))

;;;; Download and install packages
;; =============================================================================

(setq load-prefer-newer t)  ; new in v24.4

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Packages to install on launch
(defvar md/required-packages
  '(ace-jump-mode
    color-theme  ; TODO - can probably remove this sometime
    evil
    evil-surround  ; Port of the vim surround plugin
    evil-leader  ; Not sure why this isn't built into evil, but it's handy
    exec-path-from-shell  ; Copies Emacs' exec-path from the shell on the OS X app
    fill-column-indicator
    fic-mode  ; Highlight keywords in comments
    flycheck ; Syntax checker
    outline-magic
    org
    smart-mode-line
    helm
    elscreen
    key-chord
    undo-tree))

(package-initialize)

(defun md/refresh-packages ()
  (interactive)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (package-initialize)

  (dolist (p md/required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;; Add "non-elpa" dir to load-path
;; =============================================================================

(let ((base (concat (md/get-dotfiles-path) "/emacs.d.symlink/non-elpa")))
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

(load-file (concat (md/get-dotfiles-path) "/splitscreen/splitscreen.el"))

;;;; Backups
;; =============================================================================

;; Backup everything to the same directory, rather than dropping
;; files all over the place
(setq backup-directory-alist
      `(("." . ,(concat (md/get-dotfiles-path) "/emacs.d.symlink/.backups"))))

;;;; General
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Helm
(require 'helm)
(require 'helm-config)

(helm-mode 1)
(helm-autoresize-mode 0)

;; Replace some standard commands with helm versions
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; Swap default bindings - use tab for completion, C-z for actions
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

;;;; SML
;; TODO replace w/powerline?
(require 'smart-mode-line)

(setq sml/theme 'nil)
(setq sml/shorten-directory t
    sml/shorten-modes t
    sml/extra-filler 0)
(sml/setup)


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

(setq custom-file (concat (md/get-dotfiles-path) "/emacs.d.symlink/custom.el"))
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

(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(electric-indent-mode 1) ; Auto indent code

;; Python indentation is handled differently - taken from
;; www.emacswiki.org/emacs/AutoIndentation
(defun md/electric-indent-ignore-python (char)
  (if (equal major-mode 'python-mode) `no-indent' nil))
(defun md/set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'electric-indent-functions 'md/electric-indent-ignore-python)
(add-hook 'python-mode-hook 'md/set-newline-and-indent)

;; Don't indent in org-mode, as it adds indents all over the place and
;; is generally annoying.
(defun md/fix-electric-indent-in-org-mode ()
  (setq-local electric-indent-functions (list (lambda (arg) 'no-indent))))
(add-hook 'org-mode-hook 'md/fix-electric-indent-in-org-mode)

;;;; Display
;; =============================================================================

;; Colour column
(require 'fill-column-indicator)
(setq fci-rule-width 5) ; 5 seems to be max width

;; Note - fci-rule-color is defined in my solarized theme fork.

;; Performance is too slow w/big files to enable these by default
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)
;; (require 'linum) ; numbers in margin
;; (global-linum-mode nil)

;; Line and Column number in mode-line. These seem to be global.
(line-number-mode 1)
(column-number-mode 1)

;; Highlight cursor line
(global-hl-line-mode 1)

(if (string= system-name "mattmbp.local")
    (set-frame-font "Monaco-12:antialias=subpixel")
    (set-frame-font "Monaco-13:antialias=subpixel"))

;; Necessary on v24.4 to display accurate Solarized colors, due to Emacs bug #8402.
;; v24.3 didn't set ns-use-sgrb-colorspace.
(setq ns-use-srgb-colorspace nil)
(setq solarized-broken-srgb t)

;; Vim-like scrolling with margins
(setq
 scroll-margin 1
 scroll-conservatively 9999
 scroll-step 1)

(blink-cursor-mode 0)  ; Don't blink the cursor

;; Create fringe at left side of split
(require 'fringe)
(defun md/fringe-mode ()
  (interactive)
  (fringe-mode '(8 . 0)))
                
(add-hook 'prog-mode-hook 'md/fringe-mode)  ; I basically just want this for flycheck

;; Remove scrollbars to get extra screen space
(require 'scroll-bar)
(scroll-bar-mode -1)

;; ...and toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; I find it confusing that elscreen tabs get displayed at the top of buffers,
;; so hide them.
(defun md/elscreen-hide-tabs ()
  (interactive)
  ;; This is how elscreen hides tabs in (elscreen-toggle-display-tab)
  (setq elscreen-display-tab nil)
  (elscreen-notify-screen-modification 'force))

(md/elscreen-hide-tabs)


;;;; Parentheses
;; =============================================================================

;; Basic paren matching is built in. We want to enable the "matches..." messages
;; in the mini-buffer, but use show-paren-mode for more control over the
;; on-screen parens.
(require 'paren) ;; Load now to avoid invalid-face error
(setq blink-matching-paren nil)  ; show-paren is sufficient
(setq blink-matching-paren-on-screen nil)
(setq show-paren-style 'parenthesis)



;;;; Highlight note keywords
;; =============================================================================

;; NOTE: fic-mode doesn't seem to fontify the buffer itself, so words often
;; don't appear highlighted unless either something else fontifies the buffer,
;; or we do it manually. Would like to improve this.
;;
;; FIX: fic-mode doesn't seem to identify words on the same line as my cursor
;; when I change theme and then fontify the buffer. All other lines seem fine.
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(setq fic-highlighted-words
      '("TODO" "FIX" "FIXME" "BUG" "WARN" "WARNING" "HACK" "NOTE" "ERROR" "MATT"))

;; By default this includes font-lock-string-face, but I don't want strings to
;; have these words formatted.
(setq fic-activated-faces '(font-lock-doc-face font-lock-comment-face))


;;;; Prog mode
;; =============================================================================

(defun md/prog-hook ()
  (show-paren-mode 1)
  (outline-minor-mode 1))
(add-hook 'prog-mode-hook 'md/prog-hook)

;;;; Evil
;; =============================================================================

(require 'evil-leader)
(require 'evil)
(require 'evil-surround)
(global-evil-leader-mode)  ; Required before evil-mode
(evil-mode 1)
(global-evil-surround-mode 1)


;; I don't need to learn the default emacs undo system yet, this seems simpler.
(require 'undo-tree)

;; Use <esc> to cancel everything.
;; Taken from https://github.com/davvil/.emacs.d/blob/master/init.el
(defun md/minibuffer-keyboard-quit ()
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
(define-key minibuffer-local-map [escape] 'md/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'md/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'md/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'md/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'md/minibuffer-keyboard-quit)

;; Remap ";" to ":" and "," to ";", like vimrc
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map "," 'evil-repeat-find-char)

;; As recommended by emacswiki, take RET and SPC out of evil-motion-state-map
;; so it doesn't override bindings in eg. the profiler.
(defun md/move-key (keymap-from keymap-to key)
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(md/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(md/move-key evil-motion-state-map evil-normal-state-map " ")

;; Closest thing to easymotion.
(require 'ace-jump-mode)
(setq ace-jump-mode-move-keys '(?f ?j ?d ?k ?s ?l ?a ?\; ?g ?h ?r ?u ?e ?i ?w ?o ?t ?y ?b ?v ?n ?c ?m ?x))
(setq ace-jump-mode-scope 'window) ;; If scope is wider than window performance drops a lot
(setq ace-jump-word-mode-use-query-char nil)
(define-key evil-normal-state-map (kbd "f") nil)  ; I used to use "\\", but
                                        ; trying this
(define-key evil-normal-state-map (kbd "f j") 'evil-ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "f k") 'evil-ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "f w") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "f b") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "f f") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "f F") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "f t") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "f T") 'evil-ace-jump-char-mode)

;; Same as vimrc
(define-key evil-normal-state-map "H" 'move-beginning-of-line)
(define-key evil-normal-state-map "L" 'move-end-of-line)
(define-key evil-visual-state-map "H" 'move-beginning-of-line)
(define-key evil-normal-state-map "L" 'move-end-of-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; This seems the easiest way to get vim-style arbitrary prefixes
;; TODO - why did I need this again?
(require 'key-chord)
(setq key-chord-two-keys-delay 0.4)
(defun md/normal-state-and-save ()
  (interactive)
  (evil-normal-state)
  (save-buffer))

(key-chord-define evil-insert-state-map "jj" 'md/normal-state-and-save)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)


;; evil-leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "h" help-map  ; In vim I use <leader>h to list bindings. In emacs you can list
                ; bindings for a prefix by pressing C-h after the prefix, eg. <leader>C-h

  "w" 'save-buffer
  "W" (lambda () (interactive)
        (delete-trailing-whitespace) (save-buffer))

  "bg" 'solarized-toggle-theme-mode ; TODO - font-lock-fontify-buffer in all
                                    ; open buffers? As currently eg. org-mode
                                    ; buffers need to be fontified.

  "cc" 'comment-or-uncomment-region ; TODO - do I want to behave like vim?
                                    ; ie. comment the line or the selection
  "chl" 'solarized-toggle-comment-visibility

  "f" 'helm-find-files
  "C-p" 'helm-buffers-list  ; Equivalent to C-x C-b
  "p" 'helm-mini  ; Equivalent to C-x b
  "x" 'helm-M-x

  "ef" 'eval-defun
  "ee" 'eval-last-sexp  ; Bound to e because I'm used to C-x e
  "eb" 'eval-buffer
  "er" 'eval-region
  "ex" (lambda () (interactive)  ; Fontify buffer - it's sort-of an eval
         (font-lock-fontify-buffer)
         (message "Fontified buffer"))  ; So I know it's run

  "lw" 'toggle-truncate-lines

  ;; Same as vim - insert and save
  "o" (lambda () (interactive)
        (save-excursion
          (end-of-line)
          (open-line 1)
          (save-buffer)))
  "O" (lambda () (interactive)
        (save-excursion
          (evil-previous-visual-line)
          (end-of-line)
          (open-line 1)
          (save-buffer)))

  ;; I've bound this, but NOTE I should just be using g; which works in both vim and evil
  ";" 'goto-last-change

  ;; S prefix, ie. "syntax"
  "sl" 'flycheck-list-errors
  "sn" 'flycheck-next-error
  "sj" 'flycheck-next-error
  "sp" 'flycheck-previous-error
  "sk" 'flycheck-previous-error
  (kbd "s <RET>") 'flycheck-mode
  )

(define-key help-map (kbd "x") 'describe-face)
(define-key help-map (kbd "X") 'helm-colors)


;;;; Outline
;; =============================================================================

;; Outline-magic
(defun md/outline-magic-mode-hook () (require 'outline-magic))
(defun md/outline-magic-minor-mode-hook ()
  (require 'outline-magic)
  ;; Provide cycle and positioning keys similar to org-mode
  (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)
  (define-key outline-minor-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
  (define-key outline-minor-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
  (define-key outline-minor-mode-map (kbd "<M-left>") 'outline-promote)
  (define-key outline-minor-mode-map (kbd "<M-right>") 'outline-demote))

(add-hook 'outline-mode-hook 'md/outline-magic-mode-hook)
(add-hook 'outline-minor-mode-hook 'md/outline-magic-minor-mode-hook)


;;;; Org
;; =============================================================================

(require 'org)  ; Require this now so any customisable faces are loaded

(setq
 org-log-done 'time ; Add timestamp when set task as closed.
 org-agenda-restore-windows-after-quit t ; Agen
 org-src-fontify-natively t ; Inline code has syntax highlighting.
 org-level-color-stars-only nil ; Colour the whole headline.
 org-catch-invisible-edits 'show-and-error ; Prevent accidentally editing invisible lines.
 org-adapt-indentation t           ; Ideally this would be true for
                                        ; clock/properties, but nil for plain lists. Not
                                        ; sure can do this though.
 org-clock-out-remove-zero-time-clocks t
 org-M-RET-may-split-line nil)  ; If press M-RET I want a new item, not to
                                        ; split the line.

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cp" 'org-query)

(defun md/org-timestamp-time-inactive-no-confirm ()
    (interactive)
  (org-insert-time-stamp (current-time) t t))

(defun md/org-timestamp-date-inactive-no-confirm ()
    (interactive)
  (org-insert-time-stamp (current-time) nil t))

(defun md/org-hook ()
  ;; Change tab widths to fit headline indents
  (setq tab-width 2)
  (setq evil-shift-width 2)

  ;; Disable in-buffer line numbers and the colour column, as both decrease
  ;; org-mode / outline performance significantly on large files.
  (linum-mode 0)
  (fci-mode 0)

  ;; Also disable the row and column numbers in the modeline. Seems you have to set
  ;; these manually to make them buffer-local, unlike the above functions - TODO
  ;; is this a bad thing?
  (setq-local line-number-mode nil)
  (setq-local column-number-mode nil)

  ;; Also don't highlight the current line. For some reason this rquires making
  ;; global-hl-line-mode buffer-local.
  (make-variable-buffer-local 'global-hl-line-mode)
  (setq-local global-hl-line-mode nil)

  (define-key org-mode-map (kbd "C-c d") 'md/org-timestamp-date-inactive-no-confirm)
  (define-key org-mode-map (kbd "C-c t") 'md/org-timestamp-time-inactive-no-confirm))


(add-hook 'org-mode-hook 'md/org-hook)

;; Default to using my CSS theme for html exports
(setq org-html-head-extra
 "<link id='generic-css-dark' rel='stylesheet' type='text/css'
   href='https://mattduck.github.io/generic-css/css/generic-dark.css'>
  <link id='generic-css-light' rel='stylesheet' type='text/css'
   href='https://mattduck.github.io/generic-css/css/generic-light.css'>
  <script type='text/javascript'
   src='https://mattduck.github.io/generic-css/js/generic-css.js'></script>")
(setq org-export-headline-levels 6
      org-export-with-section-numbers 4)



;;;; Org-evil
;; =============================================================================
;; Adapted from https://github.com/edwtjo/evil-org-mode. The license below
;; applies to all code up until "evil-org-mode ends here".
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "gk" 'outline-previous-visible-heading
  "gj" 'outline-next-visible-heading
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "-" 'org-cycle-list-bullet
  (kbd "RET") 'org-cycle
  (kbd "TAB") 'org-cycle)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
          ))
      '(normal insert))


;; I don't like the default org-agenda bindings - there are far more
;; bindings/features than I should have to think about, and I usually try to
;; navigate using evil bindings (only to bring up calendars etc).
;;
;; Instead, open org-agenda in evil-normal-mode, with a couple of the useful
;; bindings copied directly from emacs-mode.
(define-minor-mode evil-org-agenda-mode
  "Buffer local minor mode for evil-org-agenda"
  :init-value nil
  :lighter " EvilOrgAgenda"
  :keymap (make-sparse-keymap) ; defines evil-org-agenda-mode-map
  :group 'evil-org-agenda

  (evil-set-initial-state 'evil-org-agenda-mode 'normal))

(add-hook 'org-agenda-mode-hook 'evil-org-agenda-mode)

(evil-define-key 'normal evil-org-agenda-mode-map
  ;; j / k
  (kbd "j") 'org-agenda-next-line
  (kbd "n") 'org-agenda-next-line
  (kbd "C-n") 'org-agenda-next-line
  (kbd "k") 'org-agenda-previous-line
  (kbd "p") 'org-agenda-previous-line
  (kbd "C-p") 'org-agenda-previous-line

  (kbd "q") 'org-agenda-quit
  (kbd "r") 'org-agenda-redo  ; Recalculate the agenda
  (kbd "v") 'org-agenda-view-mode-dispatch  ; Alter the view
  (kbd "|") 'org-agenda-filter-remove-all  ; Remove existing filters
  (kbd "=") 'org-agenda-filter-by-regexp  ; Search
  (kbd "/") 'org-agenda-filter-by-tag  ; Tag filter
  (kbd "^") 'org-agenda-filter-by-top-headline  ; Show other items with same
                                        ; headline as current
  (kbd "A") 'org-agenda-append-agenda)  ; Add another agenda

;; evil-org-mode ends here

;;;; Flycheck
(setq flycheck-flake8rc ".config/flake8")
(setq flycheck-display-errors-delay 0.1)  ; defaults to 0.9, which is too slow
(setq flycheck-highlighting-mode 'symbols)

;;;; Solarized
;; =============================================================================

(require 'color-theme-solarized)
(load-theme 'solarized t)  ; Defaults to light
(solarized-enable-theme 'dark)
