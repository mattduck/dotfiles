
(setq gc-cons-threshold 100000000)

(defun md/get-dotfiles-path ()
  (or
    (getenv "DOTFILES")
    (concat (expand-file-name "~") "/dotfiles")))

(defun md/refresh-packages ()
  (interactive)
  (when (not package-archive-contents)
    (package-refresh-contents)))

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(setq load-prefer-newer t)  ; new in v24.4

(setq custom-file (concat (md/get-dotfiles-path) "/emacs.d.symlink/custom.el"))
(load custom-file 'noerror)

(unless (package-installed-p 'use-package)
    (package-install 'use-package))
(eval-when-compile
    (require 'use-package))
(require 'bind-key)  ; Required for :bind in use-package
(setq use-package-always-ensure nil
      use-package-verbose t)

(use-package
 exec-path-from-shell
 :demand t
 :config
 (progn
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))))

(setq inhibit-splash-screen t)

(setq-default fill-column 80)

(use-package
 fill-column-indicator
 :config
 (progn
   ;; Width of the fill column rule
   (setq fci-rule-width 5)))

(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(add-hook 'after-save-hook 'font-lock-fontify-buffer)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq linum-format
      (lambda (line-number)
        (let ((w (length (number-to-string
                          (count-lines (point-min) (point-max))))))
          (propertize
           (format
            (concat "%" (number-to-string w) "d ")
            line-number) 'face 'linum))))

(use-package
  linum-mode
  :bind (:map md/leader-map
         ("ln" . linum-mode)))

(global-hl-line-mode 1)

(defun md/set-default-font ()
  (interactive)
  (if (string= system-name "mattmbp.local")
      (set-frame-font "Monaco-12:antialias=subpixel")
    (set-frame-font "Monaco-13:antialias=subpixel")))
(md/set-default-font)

(md/set-default-font)

(add-hook 'focus-in-hook 'md/set-default-font)

(setq

  ;; TODO - do I need scroll-conservatively here? I used to use it, now not sure why.

  ;; Start scrolling when the cursor is one line away from the top/bottom. Default
  scroll-margin 1

  ;; If at the bottom of the file, don't scroll beyond that and show a lot of
  ;; empty space - st scroll
  scroll-conservatively 999

  ;; Only scroll one row at a time. Default behaviour is to centre the row.
  scroll-step 1)

;; Remove scrollbars (GUI only) to get extra screen space
(require 'scroll-bar)
(scroll-bar-mode -1)

(blink-cursor-mode 0)

(defun md/fringe-mode ()
  (interactive)
  ;; Create fringe at left side of screen.
  ;; I think 8 is the minimum width needed to display flycheck markers properly.
  (fringe-mode '(8 . 0)))

;; I just want to use this for flycheck mode.
(add-hook 'prog-mode-hook 'md/fringe-mode)

(setq-default

 ;; Use spaces instead of tabs
 indent-tabs-mode nil

 ;; Display tab as 4 chars wide
 tab-width 4)

;; Emable on-the-fly indenting. TODO - read docs for this
(electric-indent-mode 1)

(setq visible-bell nil
      ring-bell-function 'ignore)

;; Backup everything to the same directory, rather than dropping
;; files all over the place
(setq backup-directory-alist
      `(("." . ,(concat (md/get-dotfiles-path) "/emacs.d.symlink/.backups"))))

(if (eq system-type 'darwin)
    (setq

     ;; Set alt/option to use its default behaviour in OS X , so I can do
     ;; eg. alt+3 to insert #. By default in Emacs this is Meta, but I find Meta more
     ;; accessible on the left cmd key.
     ns-option-modifier nil

     ;; This is the default, and seems to handle the standard cmd key
     ;; bindings, so apple cmd+c runs super+c in emacs, etc. I don't use them
     ;; much, but they might be useful sometimes.
     ns-right-command-modifier 'super

     ;; Instead of the cmd bindings (that I don't use much), use the left
     ;; cmd key for Meta bindings. This is easier to reach than the default Meta
     ;; key (which is alt).
     ns-command-modifier 'meta))

(defun md/strip-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(defun md/fontify-buffer ()
  (interactive)
  (font-lock-fontify-buffer)
  (message "Fontified buffer"))

(defvar md/leader-map (make-sparse-keymap))

(bind-key "x" 'describe-face help-map)

(use-package
 evil
 :config
 (progn
   (defun md/normal-state-and-save ()
     (interactive)
     (evil-normal-state)
     (save-buffer))

   (defun md/insert-blank-line-before ()
     (interactive)
     (save-excursion
       (end-of-line)
       (open-line 1)
       (save-buffer)))

   (defun md/insert-blank-line-after ()
     (interactive)
     (save-excursion
       (evil-previous-visual-line)
       (end-of-line)
       (open-line 1)
       (save-buffer)))

   ;; Can't work out how to properly define map bindings using ":bind"
   (bind-key "<SPC>" md/leader-map evil-normal-state-map)
   (bind-key "<SPC>" md/leader-map evil-visual-state-map)

   (bind-key "h" help-map md/leader-map)  ; I prefer <leader>h to C-h

   (evil-mode 1))

 :bind (;; Like my vimrc, remap  ; to : and , to ;
        :map evil-motion-state-map
        (";" . evil-ex)
        ("," . evil-repeat-find-char)

        ;; Use H/L instead of ^/$
        :map evil-normal-state-map
        ("H" . move-beginning-of-line)
        ("L" . move-end-of-line)
        :map evil-visual-state-map
        ("H" . move-beginning-of-line)
        ("L" . move-end-of-line)

        ;; The equivalent of gj/gk
        :map evil-normal-state-map
        ("j" . evil-next-visual-line)
        ("k" . evil-previous-visual-line)

        ;; Leader bindings
        :map md/leader-map
        ("w" . save-buffer)
        ("W" . md/strip-whitespace-and-save)

        ;; TODO behave like vim - ie. comment the line or the selection
        ("cc" . comment-or-uncomment-region)
        ("k" . kill-buffer)

        ("ef" . eval-defun)
        ("ee" . eval-last-sexp)  ; Bound to e because I'm used to C-x e
        ("eb" . eval-buffer)
        ("er" . eval-region)
        ("ex" . md/fontify-buffer)  ; It's sort-of an eval

        ("lw" . toggle-truncate-lines)

        ;; Same as vim - insert and save
        ("o" . md/insert-blank-line-before)
        ("O" . md/insert-blank-line-after)))

(use-package
 evil-surround
 :config
 (progn
   (global-evil-surround-mode 1)))

(use-package
 ace-jump-mode

 :config
 (progn
   (setq
    ace-jump-mode-move-keys '(?f ?j ?d ?k ?s ?l ?a ?\; ?g ?h ?r ?u ?e ?i ?w ?o ?t ?y ?b ?v ?n ?c ?m ?x)
    ace-jump-mode-scope 'window  ; If scope is wider than window performance drops a lot
    ace-jump-word-mode-use-query-char))

 :bind (:map evil-normal-state-map
             ("f" . nil)
             ("f j" . evil-ace-jump-line-mode)
             ("f k" . evil-ace-jump-line-mode)
             ("f w" . evil-ace-jump-word-mode)
             ("f b" . evil-ace-jump-word-mode)
             ("f f" . evil-ace-jump-char-mode)
             ("f F" . evil-ace-jump-char-mode)
             ("f t" . evil-ace-jump-char-mode)
             ("f T" . evil-ace-jump-char-mode)))

(use-package
  key-chord
  :config
  (progn
    (setq key-chord-two-keys-delay 0.4)

    (key-chord-define evil-insert-state-map "jj" 'md/normal-state-and-save)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-mode 1)))

(use-package
 fic-mode
 :config
 (progn
   ;; NOTE: fic-mode doesn't seem to fontify the buffer, so words don't appear
   ;; highlighted unless either something else fontifies the buffer, or we do it
   ;; manually. Would like to improve this.
   ;;
   ;; FIX: fic-mode doesn't seem to identify words on the same line as my cursor
   ;; when I change theme and then fontify the buffer. All other lines seem fine.

   (add-hook 'prog-mode-hook 'fic-mode)

   (setq fic-highlighted-words
         '("TODO" "FIX" "FIXME" "BUG" "WARN" "WARNING" "HACK" "NOTE" "ERROR" "MATT"))

   ;; By default this includes font-lock-string-face, but I don't want strings to
   ;; have these words formatted.
   (setq fic-activated-faces '(font-lock-doc-face font-lock-comment-face))))

(use-package
 helm
 :config
 (progn
   (helm-mode 1)
   (helm-autoresize-mode 0))
 :bind (([remap find-file] . helm-find-files)
        ([remap occur] . helm-occur)
        ([remap dabbrev-expand] . helm-dabbrev)
        ([remap list-buffers] . helm-buffers-list)
        ("M-x" . helm-M-x)
        ("C-x b" . helm-mini)

        :map helm-map
        ("<tab>" . helm-execute-persistent-action)
        ("C-z" . helm-select-action)

        :map lisp-interaction-mode-map
        ([remap completion-at-point] . helm-lisp-completion)

        :map emacs-lisp-mode-map
        ([remap completion-at-point] . helm-lisp-completion)

        :map md/leader-map
        ("b" . helm-buffers-list)
        ("f" . helm-find-files)
        ("x" . helm-M-x)
        ("p" . helm-mini)

        :map help-map
        ("X" . helm-colors)))

;; TODO - why did I need this?
(use-package helm-config)

(use-package
 paren
 :config
 (progn
   (setq show-paren-style 'parenthesis
         blink-matching-paren nil
         blink-matching-paren-on-screen nil)))

(use-package
 elscreen
 :config
 (progn
   (defun md/elscreen-hide-tabs ()
     (interactive)
     ;; This is how elscreen hides tabs in (elscreen-toggle-display-tab)
     (setq elscreen-display-tab nil)
     (elscreen-notify-screen-modification 'force))
   (md/elscreen-hide-tabs)))

(setq md/splitscreen-path (concat (md/get-dotfiles-path) "/splitscreen/"))

(use-package
 splitscreen
 :load-path md/splitscreen-path
 :config
 (progn
   (splitscreen-mode)))

(use-package
 org
 :config
 (progn

   (defun md/org-timestamp-time-inactive-no-confirm ()
     (interactive)
     (org-insert-time-stamp (current-time) t t))

   (defun md/org-timestamp-date-inactive-no-confirm ()
     (interactive)
     (org-insert-time-stamp (current-time) nil t))

   (defun md/org-hook ()
     ;; Change tab widths to fit headline indents
     (setq tab-width 2
           evil-shift-width 2)

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
     (setq-local global-hl-line-mode nil))

   (define-minor-mode md/evil-org-mode
     "Buffer local minor mode for evil-org"
     :init-value nil
     :lighter " EvilOrg"
     :keymap (make-sparse-keymap) ; defines md/evil-org-mode-map
     :group 'md/evil-org)

   ;; NOTE - I don't think the use-package ":bind" arg allows defining evil keys
   ;; for a specific evil-mode/map combination.

   ;; Normal state shortcuts
   (evil-define-key 'normal md/evil-org-mode-map
     "gk" 'outline-previous-visible-heading
     "gj" 'outline-next-visible-heading
     "H" 'org-beginning-of-line
     "L" 'org-end-of-line
     "$" 'org-end-of-line
     "^" 'org-beginning-of-line
     "-" 'org-cycle-list-bullet
     (kbd "RET") 'org-cycle
     (kbd "TAB") 'org-cycle)

   ;; Normal & insert state shortcuts.
   (mapc (lambda (state)
           (evil-define-key state md/evil-org-mode-map
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
   ;; navigate using evil bindings (and eg. accidentally hit "j" and bring up
   ;; the calendar etc).
   ;;
   ;; Instead, open org-agenda in evil-normal-mode, with a couple of the useful
   ;; bindings copied directly from emacs-mode.
   (define-minor-mode md/evil-org-agenda-mode
     "Buffer local minor mode for evil-org-agenda"
     :init-value nil
     :lighter " EvilOrgAgenda"
     :keymap (make-sparse-keymap) ; defines evil-org-agenda-mode-map
     :group 'evil-org-agenda

     (evil-set-initial-state 'md/evil-org-agenda-mode 'normal))

   (evil-define-key 'normal md/evil-org-agenda-mode-map
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

   (add-hook 'org-mode-hook 'md/org-hook)
   (add-hook 'org-mode-hook 'md/evil-org-mode)
   (add-hook 'org-mode-agenda-hook 'md/evil-org-agenda-mode)

   (setq org-agenda-restore-windows-after-quit t

         ;; Add timestamp when set task as closed
         org-log-done 'time

         ;; Fontify inline code
         org-src-fontify-natively t

         ;; Colour the whole headline
         org-level-color-stars-only nil

         ;; Try to prevent accidentally editing invisible lines
         org-catch-invisible-edits 'show-and-error

         org-adapt-indentation nil

         org-clock-out-remove-zero-time-clocks t

         ;; If press M-RET I want a new line, not to split the line
         org-M-RET-may-split-line nil

         ;; Default to using my CSS theme for html exports
         org-html-head-extra "
        <link id='generic-css-dark' rel='stylesheet' type='text/css'
              href='https://mattduck.github.io/generic-css/css/generic-dark.css'>
        <link id='generic-css-light' rel='stylesheet' type='text/css'
              href='https://mattduck.github.io/generic-css/css/generic-light.css'>
        <script type='text/javascript'src='https://mattduck.github.io/generic-css/js/generic-css.js'></script>"

         org-export-headline-levels 6
         org-export-with-section-numbers 4))
 :bind (:map global-map
       ("C-c a" . org-agenda)

       :map org-mode-map
       ("C-c d" . md/org-timestamp-date-inactive-no-confirm)
       ("C-c t" . md/org-timestamp-time-inactive-no-confirm)))

(line-number-mode 1)
(column-number-mode 1)

(use-package
 powerline
 :config
 (progn
   (defun md/powerline-setup ()
     (interactive)
     (require 'flycheck)
     (setq-default mode-line-format
                   '("%e"
                     (:eval
                      (let* ((active (powerline-selected-window-active))
                             (mode-line (if active 'mode-line 'mode-line-inactive))
                             (face1 (if active 'powerline-active1 'powerline-inactive1))
                             (face2 (if active 'powerline-active2 'powerline-inactive2))
                             (separator-left (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (car powerline-default-separator-dir))))
                             (separator-right (intern (format "powerline-%s-%s"
                                                              (powerline-current-separator)
                                                              (cdr powerline-default-separator-dir))))

                             (lhs (list (powerline-raw evil-mode-line-tag face2 'l)
                                        (funcall separator-left face2 face1)
                                        (powerline-raw (format "*%s* " (powerline-major-mode)) face1 'l)
                                        (funcall separator-left face1 mode-line)
                                        (if (and (boundp 'projectile-mode) projectile-mode)
                                            (powerline-raw (concat (projectile-project-name) "::%b") 'l)
                                          (powerline-raw "%b" mode-line 'l))

                                        (when (buffer-modified-p)
                                          (powerline-raw "+" mode-line 'l))
                                        (when buffer-read-only
                                          (powerline-raw "[RO]" mode-line 'l))
                                        (when (buffer-narrowed-p)
                                          (powerline-raw "  Narrow" mode-line 'l))
                                        (when (and active (fboundp 'org-clocking-p) (org-clocking-p))
                                          (powerline-raw
                                           (propertize
                                            (format "  %s "
                                                    (if (> (length org-mode-line-string) 50)
                                                        (format "%s..." (string-trim (substring org-mode-line-string 0 50)))
                                                      org-mode-line-string))
                                            'face nil)
                                           mode-line 'l))))

                             (rhs (list (funcall separator-right mode-line face1)
                                        (powerline-vc face1 'r)
                                        (when (or line-number-mode column-number-mode)
                                          (cond ((and line-number-mode
                                                      column-number-mode)
                                                 (powerline-raw "%5l:%2c" face2 'r))
                                                (line-number-mode
                                                 (powerline-raw "%5l" face2 'r))
                                                (column-number-mode
                                                 (powerline-raw ":%2c" face2 'r))))


                                        ;; TODO: change colour when err/warn, and
                                        ;; list line of first error
                                        ;; (when flycheck-mode
                                        ;;   (powerline-raw (format "%6s" (flycheck-mode-line-status-text)) 'error 'r))
                                        (when (and active flycheck-mode (flycheck-has-current-errors-p))
                                          (powerline-raw
                                           (format " [line:%s (%s)] "
                                                   ;; Line of first err
                                                   (save-excursion
                                                     (flycheck-first-error)
                                                     (+ 1 (count-lines (point-min) (point))))
                                                   ;; Total lines
                                                   (length flycheck-current-errors))

                                           ;; Face
                                           (cond ((flycheck-has-current-errors-p 'error)
                                                  'md/modeline-flycheck-error)
                                                 ((flycheck-has-current-errors-p 'warning)
                                                  'md/modeline-flycheck-warning))
                                           'r))



                                        ))
                             )
                        (concat (powerline-render lhs)
                                (powerline-fill mode-line (powerline-width rhs))
                                (powerline-render rhs)))))))

   (defun md/powerline-reset ()
     (interactive)
     (setq mode-line-format (md/powerline-setup))
     (solarized-load-theme))

   (md/powerline-setup)))

(use-package
 flycheck
 :config
 (progn
   (defface md/modeline-flycheck-error '((t (:inherit 'error))) "")
   (defface md/modeline-flycheck-warning '((t (:inherit 'warning))) "")

   (setq flycheck-flake8rc ".config/flake8"
         flycheck-highlighting-mode 'symbols

         ;; defaults to 0.9, which is too slow
         flycheck-display-errors-delay 0.1

         ;; There's a short delay when flycheck runs, which causes the modeline to change
         ;; its format (or in my custom powerline stuff, to disappear briefly). It's
         ;; super annoying if this happens at random points during editing, so change it
         ;; to only happen on save (and when enabling the mode). This is quite similar to how
         ;; I had it setup in vim.
         flycheck-check-syntax-automatically '(save mode-enabled)

         flycheck-mode-line-prefix nil)

   (add-hook 'prog-mode-hook 'flycheck-mode))
 :bind (:map md/leader-map
        ;; S prefix, ie. "syntax"
        ("s <RET>" . flycheck-mode)
        ("sl" . flycheck-list-errors)
        ("sn" . flycheck-next-error)
        ("sj" . flycheck-next-error)
        ("sp" . flycheck-previous-error)
        ("sk" . flycheck-previous-error)))

(use-package
 projectile
 :config
 (progn
   (setq projectile-file-exists-local-cache-expire (* 10 60)
         projectile-enable-caching t)
   (projectile-global-mode))
 :bind (:map md/leader-map
       ("jk" . projectile-kill-buffers)))

(use-package
 helm-projectile
 :bind (:map md/leader-map
       ("jj" . helm-projectile-switch-project)
       ("jag" . helm-projectile-ag)
       ("jb" . helm-projectile-switch-to-buffer)

       ;; TODO - proper binding for invalidating cache
       ("jf" . helm-projectile-find-file)))

(use-package
 git-gutter
 :init
 (progn
   (add-hook 'prog-mode-hook 'git-gutter-mode))
 :config
 (progn
   (setq git-gutter:ask-p nil  ; Don't ask for confirmation of gadd
         git-gutter:modified-sign "~"
         git-gutter:added-sign "+"
         git-gutter:deleted-sign "-"

         ;; Without this, there's no space between the git-gutter column and the code.
         git-gutter:separator-sign " "))
 :bind (:map md/leader-map
       ("g <RET>" . git-gutter-mode)
       ("gk" . git-gutter:previous-hunk)
       ("gp" . git-gutter:previous-hunk)
       ("gj" . git-gutter:next-hunk)
       ("gn" . git-gutter:next-hunk)
       ("gadd" . git-gutter:stage-hunk)
       ("grev" . git-gutter:revert-hunk)))

(use-package
 magit
 :config
 (progn
   (delete 'magit-blame-mode evil-emacs-state-modes)
   (delete 'magit-revision-mode evil-emacs-state-modes)

   ;; I don't know why, but by default I can't get magit-blame to adhere to my
   ;; normal-mode map below, even though Evil says I'm in normal mode. Explicitly
   ;; calling evil-normal-state fixes it.
   (add-hook 'magit-blame-mode-hook 'evil-normal-state)
   (add-hook 'magit-revision-mode-hook 'evil-normal-state)

   (evil-define-key 'normal magit-blame-mode-map
     (kbd "<RET>") 'magit-show-commit
     "q" 'magit-blame-quit
     "gj" 'magit-blame-next-chunk
     "gn" 'magit-blame-next-chunk
     "gk" 'magit-blame-previous-chunk
     "gp" 'magit-blame-previous-chunk))
 :bind (:map md/leader-map
       ("gblame" . magit-blame)
       ("gdiff" . magit-ediff-popup)))

(use-package
 ediff
 :config
 (progn
   ;; TODO - I want ediff to have evil-like bindings
   (setq ediff-split-window-function 'split-window-horizontally)))

(use-package
 color-theme-solarized
 :ensure nil
 :load-path "non-elpa/color-theme-solarized"
 :config
 (progn
   (add-to-list 'custom-theme-load-path "non-elpa/color-theme-solarized")

   ;; Necessary on v24.4 to display accurate Solarized colors, due to Emacs bug
   ;; #8402. v24.3 didn't set ns-use-sgrb-colorspace.
   (setq ns-use-srgb-colorspace nil
         solarized-broken-srgb t)

   (load-theme 'solarized t)  ; Defaults to light
   (solarized-enable-theme 'dark))

 :bind (:map md/leader-map
        ("sol" . solarized-toggle-theme-mode)
        ("chl" . solarized-toggle-comment-visibility)))

(require 'evil)
(require 'powerline)
(require 'color-theme-solarized)

(setq gc-cons-threshold 800000)
