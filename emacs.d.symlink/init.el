;;;; Reload
(defun md/dotfiles-reload ()
    (interactive)
    (load-file (concat (getenv "DOTFILES") "/emacs.d.symlink/init.el")))

;;;; Download and install packages
;; =============================================================================

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Packages to install on launch
(defvar md/required-packages
  '(ace-jump-mode
    color-theme
    evil
    evil-surround
    fill-column-indicator
    outline-magic
    org
    smart-mode-line
    helm
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

(let ((base (concat (getenv "DOTFILES") "/emacs.d.symlink/non-elpa")))
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

;;;; General
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; Helm
(require 'helm)
(require 'helm-config)

(defun md/helm-solarized-hook ()
  (ignore-errors ; Faces don't always exist

    ;; Copy these from emacs-color-theme-solarized until I update my fork
    ;;(set-face-attribute 'helm-apt-deinstalled nil :foreground solarized-base01)
    ;;(set-face-attribute 'helm-apt-installed nil :foreground solarized-green)
    (set-face-attribute 'helm-bookmark-addressbook nil :foreground solarized-blue)
    (set-face-attribute 'helm-bookmark-directory nil :inherit 'helm-ff-directory)
    (set-face-attribute 'helm-bookmark-file nil :inherit 'helm-ff-file)
    (set-face-attribute 'helm-bookmark-gnus nil :foreground solarized-cyan)
    (set-face-attribute 'helm-bookmark-info nil :foreground solarized-green)
    (set-face-attribute 'helm-bookmark-man nil :foreground solarized-violet)
    (set-face-attribute 'helm-bookmark-w3m nil :foreground solarized-yellow)
    ;;(set-face-attribute 'helm-bookmarks-su nil :foreground solarized-orange)
    (set-face-attribute 'helm-buffer-not-saved nil :foreground solarized-orange)
    (set-face-attribute 'helm-buffer-process nil :foreground solarized-magenta)
    (set-face-attribute 'helm-buffer-saved-out nil 
                        :inverse-video t 
                        :foreground solarized-red 
                        :background solarized-base03)
    (set-face-attribute 'helm-buffer-size nil :foreground solarized-base01)
    (set-face-attribute 'helm-candidate-number nil
                        :weight 'bold
                        :foreground solarized-base1
                        :background solarized-base02)
    ;;(set-face-attribute 'helm-emms-playlist nil :foreground solarized-base01)
    ;;(set-face-attribute 'helm-etags+-highlight-face nil :inherit 'highlight)
    (set-face-attribute 'helm-ff-directory nil
                        :background solarized-base03
                        :foreground solarized-blue)
    (set-face-attribute 'helm-ff-executable nil 
                        :weight 'bold
                        :foreground solarized-green)
    (set-face-attribute 'helm-ff-file nil :inherit 'default)
    (set-face-attribute 'helm-ff-invalid-symlink nil 
                        :background solarized-base02
                        :foreground solarized-red)
    (set-face-attribute 'helm-ff-prefix nil 
                        :inverse-video t
                        :foreground solarized-yellow)
    (set-face-attribute 'helm-ff-symlink nil 
                        :weight 'bold
                        :foreground solarized-cyan)
    ;;(set-face-attribute 'helm-gentoo-match nil :inherit 'helm-match)
    (set-face-attribute 'helm-grep-cmd-line nil :inherit 'diff-added)
    (set-face-attribute 'helm-grep-file nil 
                        :underline t
                        :foreground solarized-cyan)
    (set-face-attribute 'helm-grep-finish nil :foreground solarized-green)
    (set-face-attribute 'helm-grep-lineno nil :foreground solarized-orange)
    (set-face-attribute 'helm-grep-match nil :inherit 'helm-match)
    (set-face-attribute 'helm-grep-running nil :foreground solarized-red)
    (set-face-attribute 'helm-helper nil :inherit 'helm-header)
    (set-face-attribute 'helm-history-deleted nil :inherit 'helm-ff-invalid-symlink)
    (set-face-attribute 'helm-history-remote nil :foreground solarized-red)
    (set-face-attribute 'helm-lisp-completion-info nil :foreground solarized-base0)
    (set-face-attribute 'helm-lisp-show-completion nil 
                        :weight 'bold
                        :foreground solarized-yellow
                        :background solarized-base02)
    ;;(set-face-attribute 'helm-ls-git-added-copied-face nil :foreground solarized-green)
    ;;(set-face-attribute 'helm-ls-git-conflict-face nil 
    ;;                    :weight 'bold
    ;;                    :foreground solarized-red)
    ;;(set-face-attribute 'helm-ls-git-deleted-and-staged-face nil 
    ;;                    :slant 'italic
    ;;                    :foreground solarized-base01)
    ;;(set-face-attribute 'helm-ls-git-deleted-not-staged-face nil 
    ;;                    :weight 'bold
    ;;                    :foreground solarized-green)
    ;;(set-face-attribute 'helm-ls-git-modified-and-staged-face nil 
    ;;                    :slant 'italic
    ;;                    :foreground solarized-base01)
    ;;(set-face-attribute 'helm-ls-git-modified-not-staged-face nil 
    ;;                    :slant 'italic
    ;;                    :foreground solarized-base01)
    ;;(set-face-attribute 'helm-ls-git-renamed-modified-face nil :foreground solarized-green)
    ;;(set-face-attribute 'helm-ls-git-untracked-face nil :foreground solarized-red)
    (set-face-attribute 'helm-M-x-key nil :foreground solarized-orange)
    (set-face-attribute 'helm-match nil :inherit 'match)
    (set-face-attribute 'helm-moccur-buffer nil 
                        :underline t
                        :foreground solarized-cyan)
    (set-face-attribute 'helm-selection-line nil :inherit 'secondary-selection)
    (set-face-attribute 'helm-separator nil :foreground solarized-red)
    (set-face-attribute 'helm-source-header nil :inherit 'helm-header)
    (set-face-attribute 'helm-time-zone-current nil :foreground solarized-green)
    (set-face-attribute 'helm-time-zone-home nil :foreground solarized-red)
    (set-face-attribute 'helm-visible-mark nil 
                        :weight 'bold
                        :background solarized-base03
                        :foreground solarized-magenta)
    ;;(set-face-attribute 'helm-w3m-bookmarks nil :inherit 'helm-bookmark-w3m)

    (set-face-attribute 'helm-selection nil
                        :inverse-video t
                        :background solarized-base03
                        :foreground solarized-base01)
    ))

(add-hook 'solarized-theme-hook 'md/helm-solarized-hook)
(helm-mode 1)
(helm-autoresize-mode 0)

;; Replace some standard commands with helm versions
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap list-buffers] 'helm-buffers-list) ; This is probably C-x b

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))


;;;; SML
(require 'smart-mode-line)

(defun md/sml-solarized-hook ()
  ;; This isn't great atm. The active theme is OK, but inactive is not
  ;; particularly readable - not sure if sml allows different faces for
  ;; active/inactive frames, or if it can disable sml faces in inactive frames
  ;; (in which case they would all use the normal mode-line faces).
  ;;
  ;; Ideally, color-theme-solarized would take care of this by default.
  (sml/apply-theme 'respectful)
  (sml/setup)
  (setq sml/shorten-directory t
        sml/shorten-modes t
        sml/extra-filler 0
        sml/override-theme nil)

  (set-face-attribute 'mode-line nil
                      :background solarized-base02
                      :foreground solarized-base3)
  (set-face-attribute 'mode-line-inactive nil
                      :background solarized-base01
                      :foreground solarized-base02)

  (set-face-attribute 'sml/filename nil :foreground nil) ; When nil, uses 'mode-line
  (set-face-attribute 'sml/folder nil :foreground solarized-base0)
  (set-face-attribute 'sml/prefix nil :foreground solarized-base0)

  ;; Git branch - seems it doesn't use sml/git
  (set-face-attribute 'sml/vc nil :foreground solarized-blue)
  (set-face-attribute 'sml/vc-edited nil :foreground solarized-orange)

  (set-face-attribute 'sml/modes nil :foreground solarized-yellow)
  (set-face-attribute 'sml/minor-modes nil :foreground solarized-base0)

  (set-face-attribute 'sml/line-number nil :foreground solarized-base0)
  (set-face-attribute 'sml/col-number nil :foreground solarized-base0)
  (set-face-attribute 'sml/position-percentage nil :foreground solarized-base0))

(add-hook 'solarized-theme-hook 'md/sml-solarized-hook)

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

(setq custom-file (concat (getenv "DOTFILES") "/emacs.d.symlink/custom.el"))
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
(add-hook 'solarized-theme-hook '(lambda () (setq fci-rule-color solarized-base02)))

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

(set-frame-font "Monaco-13:antialias=subpixel")

;; Necessary on v24.4 to display accurate Solarized colors, due to Emacs bug #8402.
;; v24.3 didn't set ns-use-sgrb-colorspace.
(setq ns-use-srgb-colorspace nil)
(setq solarized-broken-srgb t)

;; Vim-like scrolling with margins
(setq
 scroll-margin 1
 scroll-conservatively 9999
 scroll-step 1)

;; Make fringe thinner - default is 8 pixels
(require 'fringe)
(fringe-mode 4)

;; Remove scrollbars to get extra screen space
(require 'scroll-bar)
(scroll-bar-mode -1)

;; ...and toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq visible-bell nil)

;;;; Parentheses
;; =============================================================================

;; Basic paren matching is built in. We want to enable the "matches..." messages
;; in the mini-buffer, but use show-paren-mode for more control over the
;; on-screen parens.
(require 'paren) ;; Load now to avoid invalid-face error
(setq blink-matching-paren t)
(setq blink-matching-paren-on-screen nil)
(defun md/show-paren-solarized-faces ()
  (set-face-foreground 'show-paren-match solarized-orange)
  (set-face-background 'show-paren-match solarized-base02)
  (set-face-background 'show-paren-mismatch solarized-red)
  (set-face-foreground 'show-paren-mismatch solarized-base03))
(add-hook 'solarized-theme-hook 'md/show-paren-solarized-faces)

;;;; Prog mode
;; =============================================================================

(defun md/prog-hook ()
  (show-paren-mode 1)
  (outline-minor-mode 1))
(add-hook 'prog-mode-hook 'md/prog-hook)

;;;; Evil
;; =============================================================================

(require 'evil)
(require 'evil-surround)
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
(setq ace-jump-mode-scope 'window) ;; it's quicker this way
(setq ace-jump-word-mode-use-query-char nil)
(define-key evil-normal-state-map (kbd "SPC") nil)
(define-key evil-normal-state-map (kbd "SPC j") 'evil-ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "SPC k") 'evil-ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "SPC w") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "SPC b") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "SPC f") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC F") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC t") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC T") 'evil-ace-jump-char-mode)

;; Same as vimrc
(define-key evil-normal-state-map "H" 'move-beginning-of-line)
(define-key evil-normal-state-map "L" 'move-end-of-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; This seems the easiest way to get vim-style arbitrary prefixs
(require 'key-chord)
(setq key-chord-two-keys-delay 0.4)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)


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

(setq org-log-done 'time ; Add timestamp when set task as closed.
      org-agenda-restore-windows-after-quit t ; Don't understand why this isn't default.
      org-src-fontify-natively nil ; Inline code has syntax highlighting.
      org-level-color-stars-only nil ; Colour the whole headline.
      org-catch-invisible-edits 'show-and-error ; Prevent accidentally editing invisible lines.
      org-adapt-indentation t           ; Ideally this would be true for
                                        ; clock/properties, but nil for plain lists. Not
                                        ; sure can do this though.
      org-clock-out-remove-zero-time-clocks t
      org-M-RET-may-split-line nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cp" 'org-query)

(defun md/org-solarized-faces ()
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

    (set-face-attribute 'org-clock-overlay nil :background solarized-base2)

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

    (set-face-attribute 'warning nil :foreground solarized-red)))

(defun md/org-hook ()
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

  (md/org-solarized-faces))

(add-hook 'org-mode-hook 'md/org-hook)
(add-hook 'solarized-theme-hook 'md/org-solarized-faces)

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
;; evil-org-mode ends here
;;;; Solarized
;; =============================================================================
;; Load this last so any Solarized hooks run

(defun md/general-solarized-hook ()
  (setq evil-default-cursor t)
  (set-face-background 'cursor solarized-base1)
  (setq evil-emacs-state-cursor `(,solarized-violet box))
  (setq evil-normal-state-cursor `(,solarized-base1 box))
  (setq evil-visual-state-cursor `(,solarized-base1 box))
  (setq evil-insert-state-cursor `(,solarized-base1 bar))
  (setq evil-replace-state-cursor `(,solarized-red bar))
  (setq evil-operator-state-cursor `(,solarized-base1 hollow))

  ;; I think this is the easiest way to fontify all my buffers
  ;; after the changes. I always have font-lock-mode on anyway.
  (global-font-lock-mode 0)
  (global-font-lock-mode 1))
(add-hook 'solarized-theme-hook 'md/general-solarized-hook t)

;; Solarized Emacs custom theme setup
(require 'solarized-definitions)
(solarized-load-theme 'dark)
