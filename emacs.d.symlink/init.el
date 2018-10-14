(defun md/dotfiles-get-root ()
  (or
    (getenv "DOTFILES")
    (concat (expand-file-name "~") "/dotfiles")))

(defun md/dotfiles-get-path (path)
  (concat (md/dotfiles-get-root) "/" path))

(defun md/dotfiles-compile ()
  (interactive)
  (find-file (md/dotfiles-get-path "emacs.d.symlink/init.org"))
  (setq-local org-confirm-babel-evaluate nil)
  (org-babel-tangle nil "init.el")
  (byte-compile-file (md/dotfiles-get-path "emacs.d.symlink/init.el")))

(defconst md/emacs-init-start (current-time))

(let ((file-handler-name-alist nil))

(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/"))) ; no TLS for org?

(setq load-prefer-newer t)  ; new in v24.4

(setq custom-file (md/dotfiles-get-path "emacs.d.symlink/custom.el"))
(load custom-file 'noerror)

;; Kill custom buffers on q
(setq custom-buffer-done-kill t)

(defgroup md/custom nil "Placeholder group, mostly just here to silence warnings" :group 'md/custom)

(setq use-package-always-ensure nil
      use-package-verbose t
      use-package-minimum-reported-time 0.01)

(eval-when-compile
    (require 'use-package))

(require 'bind-key)  ; Required for :bind in use-package

(when (file-exists-p "/usr/local/share/emacs/site-lisp")
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

(when (file-exists-p (md/dotfiles-get-path "emacs.d.symlink/non-elpa"))
  (let ((default-directory (md/dotfiles-get-path "emacs.d.symlink/non-elpa")) )
    (normal-top-level-add-subdirs-to-load-path)))

(use-package exec-path-from-shell
 :if (memq window-system '(mac ns))
 :demand t
 :config
 (progn (exec-path-from-shell-initialize)))

(defvar md/leader-map (make-sparse-keymap))

(defvar md/python-mode-leader-map (make-sparse-keymap))
(set-keymap-parent md/python-mode-leader-map md/leader-map)

(defvar md/go-mode-leader-map (make-sparse-keymap))
(set-keymap-parent md/go-mode-leader-map md/leader-map)

(defvar md/scheme-mode-leader-map (make-sparse-keymap))
(set-keymap-parent md/scheme-mode-leader-map md/leader-map)

(defvar md/org-mode-leader-map (make-sparse-keymap))
(set-keymap-parent md/org-mode-leader-map md/leader-map)

(setq inhibit-splash-screen t)

(setq-default fill-column 80)

(use-package fill-column-indicator
 :defer 1
 :config
 (progn
   ;; Width of the fill column rule
   (setq fci-rule-width 5)))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(add-to-list 'initial-frame-alist '(fullscreen . fullscreen))
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(defun md/fontify-if-font-lock-mode ()
  (when font-lock-mode
    (font-lock-ensure)))

(add-hook 'after-save-hook 'md/fontify-if-font-lock-mode)

(bind-key "tx" 'font-lock-mode md/leader-map)

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

;; TODO - I thought use-package would defer the loading of this until I do "ln",
;; but "ln" doesn't work.
(use-package linum
  :defer 1
  :bind (:map md/leader-map
         ("tn" . linum-mode)))

;; Disable this for a minute
;;(global-hl-line-mode 1)

(defvar md/font-size nil)

(defun md/font-size-incr ()
  (interactive)
  (when md/font-size
    (setq md/font-size (+ md/font-size 1)))
  (md/set-default-font))

(defun md/font-size-decr ()
  (interactive)
  (when md/font-size
    (setq md/font-size (- md/font-size 1)))
  (md/set-default-font))

(defun md/set-default-font ()
  (interactive)
  (cond ((s-starts-with-p "mattmbp" (system-name))
         (when (not md/font-size)
           (setq md/font-size 13))
         (set-frame-font (format "Menlo-%s" md/font-size) t t))

        ((s-starts-with-p "omattria" (system-name))
         (when (not md/font-size)
           (setq md/font-size 15))
         (set-frame-font
          (format "Inconsolata for Powerline-%s:antialias=subpixel" md/font-size) t t))

        (t
         (when (not md/font-size)
           (setq md/font-size 15))
         (set-frame-font (format "Roboto Mono Light for Powerline-%s:antialias=subpixel" md/font-size) t t))))

;; TODO add bindings for buffer-only, copying C-x C-+
(bind-key "+" 'md/font-size-incr md/leader-map)
(bind-key "-" 'md/font-size-decr md/leader-map)

(use-package s :demand t)
(add-hook 'focus-in-hook 'md/set-default-font)
(md/set-default-font)

(setq

  ;; Start scrolling when the cursor is one line away from the top/bottom.
  scroll-margin 1

  ;; If at the bottom of the file, don't allow scroll beyond that (because
  ;; there's no use in having half a screen of empty space
  scroll-conservatively 999

  ;; Only scroll one row at a time. Default behaviour is to centre the row.
  scroll-step 1)

;; Remove scrollbars (GUI only) to get extra screen space
(use-package scroll-bar
  :if (display-graphic-p)
  :demand t
  :config (scroll-bar-mode -1))

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

(setq tab-always-indent nil)  ;; Don't do magic indenting when I press tab

;; Emable on-the-fly indenting. TODO - read docs for this
(electric-indent-mode 1)

(setq visible-bell nil
      ring-bell-function 'ignore)

(when (not (display-graphic-p))
  (mapc
   (lambda (face)
     ;; Same issue with underline too.
     (set-face-underline face nil (selected-frame))

     (set-face-bold face nil (selected-frame)))
   (face-list)))

(use-package xclip
  :defer 1
  :config
  (progn
    (xclip-mode 1)))

(setq message-log-max 10000)

(defun md/log (content)
  (message
   (with-temp-buffer
     (cl-prettyprint content)
     (buffer-string))))

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

;; Backup everything to the same directory, rather than dropping
;; files all over the place
(setq backup-directory-alist
      `(("." . ,(md/dotfiles-get-path "emacs.d.symlink/.backups"))))

(defun md/pbpaste ()
  (interactive)
  (shell-command "pbpaste" t))

  (if (eq window-system 'ns)
    (global-set-key (kbd "M-v") 'md/pbpaste))

(setq gc-cons-threshold 100000000
      garbage-collection-messages t)

(defun md/strip-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(defun md/fontify-buffer ()
  "Fontify the buffer and tell me it happened."
  (interactive)
  (call-interactively 'font-lock-fontify-buffer)
  (message "Fontified buffer"))

(defun md/file-info ()
  (interactive)
  (message
   "%s | %s lines | line %d:%3d%% | %s"
           (buffer-file-name)
           (count-lines (point-min) (point-max))
           (count-lines (point-min) (point))
           (/ (window-end) 0.01 (point-max))
           major-mode))

(defun md/mode-info ()
  (interactive)
  (message
   (format
    "%s"
    (with-temp-buffer
      (let (mm result)
        (dolist (mm (sort minor-mode-list 'string<) result)
          (insert (format "%s\n" mm))
          (setq result (buffer-substring (point-min) (point-max))))
        result)))))

(defun md/ljust (len str)
  (if (< (length str) len)
      ;; 32 is ASCII space
      (concat str (make-string (- len (length str)) 32))
    str))

(defun md/remove-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun md/rename-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun md/expand-newlines ()
  (interactive)
  (funcall-interactively 'replace-string "\\n" "
" nil (region-beginning) (region-end)))

(defun md/unfill-paragraph ()
  "Because I can't always wrap to 80 characters :("
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun md/unfill-region (start end)
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(bind-key "x" 'describe-face help-map)
(bind-key "C-k" 'describe-personal-keybindings help-map)

(defun md/noop () (interactive))
(defun md/make-keymap-noop (kmap)
  "Overwrite bindings on a given keymap to perform a noop function."
  (mapc (lambda (key)
          (bind-key key 'md/noop kmap)
          (bind-key (concat "C-" key) 'md/noop kmap)
          (bind-key (concat "M-" key) 'md/noop kmap)
          (bind-key (concat "C-M-" key) 'md/noop kmap)
          (bind-key (capitalize key) 'md/noop kmap)
          (bind-key (concat "C-" (capitalize key)) 'md/noop kmap)
          (bind-key (concat "M-" (capitalize key)) 'md/noop kmap)
          (bind-key (concat "C-M-" (capitalize key)) 'md/noop kmap))
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
          "s" "t" "u" "v" "w" "x" "y" "z"
          "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
  (mapc (lambda (key)
          (bind-key key 'md/noop kmap))
        '("SPC" "TAB")))

(setq delete-by-moving-to-trash t)
(setq recentf-max-saved-items 100)

(use-package evil
 :demand t
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

   (defun md/evil-fill (&optional start end)
     (interactive
      (if (use-region-p)
          (list (region-beginning) (region-end))
        (list nil nil)))
     (if (string= evil-state "visual")
         (fill-region start end)
       (fill-paragraph)))

   (defun md/evil-unfill (&optional start end)
     (interactive
      (if (use-region-p)
          (list (region-beginning) (region-end))
        (list nil nil)))
     (if (string= evil-state "visual")
         (md/unfill-region start end)
       (md/unfill-paragraph)))

   (defun md/move-line-up ()
     (interactive)
     (let ((col (current-column)))
       (transpose-lines 1)
       (forward-line -2)
       (evil-goto-column col)))

   (defun md/move-line-down ()
     (interactive)
     (let ((col (current-column)))
       (forward-line 1)
       (transpose-lines 1)
       (forward-line -1)
       (evil-goto-column col)))

   ;; NOTE - temp commenting this, is it cause of performance issues?
   ;; By default the evil jump commands don't set markers as often
   ;; as I would like. But it installs a pre-command-hook to call
   ;; evil-set-jump for all commands that have the evil property :jump,
   ;; so we can configure the jump markers to be saved more often.
   ;; (defvar md/evil-jump-trigger-commands)
   ;; (setq md/evil-jump-trigger-commands
   ;;   '(evil-scroll-page-down
   ;;     evil-scroll-page-up
   ;;     evil-scroll-down
   ;;     evil-scroll-up
   ;;     switch-to-buffer
   ;;     next-buffer
   ;;     previous-buffer
   ;;     git-gutter:next-hunk
   ;;     git-gutter:previous-hunk
   ;;     quit-window
   ;;     bookmark-jump
   ;;     dired
   ;;     dired-jump
   ;;     ))
   ;; (dolist (command md/evil-jump-trigger-commands)
   ;;   (evil-add-command-properties command :jump t))

   ;; I keep accidentally quiting with :q. Just deleting the window is enough
   (evil-ex-define-cmd "q[uit]" 'evil-window-delete)

   (setq evil-jumps-max-length 20)  ; Lower than the default, but I rarely want more

   ;; This uses C-i by default (as in vim), but C-i is interpeted as TAB, which
   ;; is an important binding in org-mode. Use C-l instead, which is bound to
   ;; recenter-top-bottom by default.
   (bind-key "C-l" 'evil-jump-forward evil-normal-state-map)
   (bind-key "C-l" 'evil-jump-forward evil-visual-state-map)

   ;; Org-like binding everywhere
   (bind-key "M-j" 'md/move-line-down evil-normal-state-map)
   (bind-key "M-k" 'md/move-line-up evil-normal-state-map)
   (bind-key "M-h" 'evil-shift-left-line evil-normal-state-map)
   (bind-key "M-l" 'evil-shift-right-line evil-normal-state-map)


   ;; evil-paste-pop is handy, but I don't like the C-n/C-p default bindings,
   ;; because those are common bindings everywhere else in Emacs. Use C-S
   ;; instead.
   (unbind-key "C-n" evil-normal-state-map)
   (unbind-key "C-p" evil-normal-state-map)
   (bind-key "C-S-n" 'evil-paste-pop-next)
   (bind-key "C-S-p" 'evil-paste-pop)

   ;; Can't work out how to properly define map bindings using ":bind"
   (bind-key "<SPC>" md/leader-map evil-normal-state-map)
   (bind-key "<SPC>" md/leader-map evil-visual-state-map)

   ;; Make leader also accessible with C-c
   (bind-key "C-c <SPC>" md/leader-map evil-visual-state-map)
   (bind-key "C-c <SPC>" md/leader-map evil-normal-state-map)
   (bind-key "C-c <SPC>" md/leader-map evil-insert-state-map)
   (bind-key "C-c <SPC>" md/leader-map evil-motion-state-map)

   (bind-key "h" help-map md/leader-map)  ; I prefer <leader>h to C-h

   (setq evil-echo-state nil)

   (evil-mode 1))

   ;; Enable evil in the minibuffer. Adapted from
   ;; https://gist.github.com/ccdunder/5816865.
   ;; Not sure why this isn't provided by default.
   ;; (mapc (lambda (keymap)
   ;;           (evil-define-key 'insert (eval keymap) [escape] 'evil-normal-state))
   ;;         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/
   ;;         ;; Text-from-Minibuffer.html#Definition of minibuffer-local-map
   ;;         '(minibuffer-local-map
   ;;           minibuffer-local-ns-map
   ;;           minibuffer-local-completion-map
   ;;           minibuffer-local-must-match-map
   ;;           minibuffer-local-isearch-map))
   ;; (defun md/evil-minibuffer-setup ()
   ;;   ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
   ;;   ;; way to do this, but the minibuffer doesn't have a mode.
   ;;   (evil-insert 1))
   ;; )
  ;; (add-hook 'minibuffer-setup-hook 'md/evil-minibuffer-setup))

 :bind (;; Like my vimrc, remap  ; to : and , to ;
        :map evil-motion-state-map
        (";" . evil-ex)
        ("," . evil-repeat-find-char)

        ;; Like in the terminal. Mainly useful in minibuffer
        :map evil-insert-state-map
        ("C-a" . move-beginning-of-line)
        ("C-e" . move-end-of-line)

        ;; Use H/L instead of ^/$
        :map evil-normal-state-map
        ("H" . move-beginning-of-line)
        ("L" . move-end-of-line)
        :map evil-visual-state-map
        ("H" . move-beginning-of-line)
        ("L" . move-end-of-line)

        ;; Paren movement
        :map evil-normal-state-map
        ("(" . evil-previous-open-paren)
        (")" . evil-next-close-paren)
        :map evil-visual-state-map
        ("(" . evil-previous-open-paren)
        (")" . evil-next-close-paren)

        ;; The equivalent of gj/gk
        :map evil-normal-state-map
        ("j" . evil-next-visual-line)
        ("k" . evil-previous-visual-line)

        ;; zz - This is similar but more flexible
        :map evil-normal-state-map
        ("zz" . recenter-top-bottom)

        ;; Leader bindings
        :map md/leader-map
        ("w" . save-buffer)
        ("W" . md/strip-whitespace-and-save)

        ("q" . md/evil-fill)
        ("Q" . md/evil-unfill)

        ;; TODO behave like vim - ie. comment the line or the selection
        ("cc" . comment-or-uncomment-region)

        ;; Buffers
        ("bh" . previous-buffer)
        ("bl" . next-buffer)
        ("k" . kill-buffer)
        ("bK" . md/remove-file-and-buffer)
        ("bR" . md/rename-file-and-buffer)
        ("bk" . kill-buffer)
        ("bi" . md/file-info)
        ("bw" . save-buffer)
        ("bW" . md/strip-whitespace-and-save)
        ("br" . read-only-mode)

        ;; Eval
        ("ef" . eval-defun)
        ("ee" . eval-last-sexp)  ; Bound to e because I'm used to C-x e
        ("eE" . eval-expression)  ; Interactive
        ("eb" . eval-buffer)
        ("er" . eval-region)
        ("ex" . md/fontify-buffer)  ; It's sort-of an eval

        ;; Emacs
        ("Ek" . kill-emacs)
        ("Es" . server-start)
        ("Ep" . list-processes)

        ;; Packages
        ("Pi" . package-install)
        ("Pl" . package-list-packages)
        ("Pr" . package-refresh-contents)

        ;; Format
        ("Fj" . json-pretty-print)
        ("Fs" . sort-lines)
        ("Fn" . md/expand-newlines)

        ; Toggle misc
        ("tw" . toggle-truncate-lines)
        ("t <tab>" . whitespace-mode)

        ;; This could be useful
        ("U" . undo-tree-visualize)

        ;; Same as vim - insert and save
        ("o" . md/insert-blank-line-before)
        ("O" . md/insert-blank-line-after)))

(use-package evil-surround
 :config
 (progn
   (global-evil-surround-mode 1)))

(use-package ace-jump-mode

 :config
 (progn
   (setq
    ace-jump-mode-move-keys '(?f ?j ?d ?k ?s ?l ?a ?\; ?g ?h ?r ?u ?e ?i ?w ?o ?t ?y ?b ?v ?n ?c ?m ?x)
    ace-jump-mode-scope 'window  ; If scope is wider than window performance drops a lot
    ace-jump-word-mode-use-query-char nil))

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

(use-package key-chord
  :config
  (progn
    (setq key-chord-two-keys-delay 0.4)

    (key-chord-define evil-insert-state-map "jj" 'md/normal-state-and-save)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-replace-state-map "jj" 'md/normal-state-and-save)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
    (key-chord-mode 1)))

(use-package helm
  :defer 5
  :config
  (progn
    ;; Putting these bindings here to avoid byte-compiled issue where helm-map isn't defined.
    (helm-mode 1)
    (helm-autoresize-mode 0)
    (helm-descbinds-mode 1)

    ;; No need to display the header - it takes up room and doesn't add much.
    (setq helm-display-header-line t)

    ;; I don't need to know about some files
    (setq helm-ff-skip-boring-files t)
    (push "\\.$" helm-boring-file-regexp-list)
    (push "\\.\\.$" helm-boring-file-regexp-list)

    ;; This lets me quickly ag/grep for "todo" comments using the same
    ;; ag/grep functions that I usually do.
    (bind-key "C-c C-t" 'md/insert-todo-regexp helm-map)

    ;; Put C-j / C-l the sane way around.
    (bind-key "C-j" 'helm-find-files-up-one-level helm-map)
    (bind-key "C-l" 'helm-execute-persistent-action helm-map)
    (bind-key "C-l" 'helm-execute-persistent-action helm-read-file-map)
    (bind-key "C-l" 'helm-execute-persistent-action helm-find-files-map))

  :bind (([remap find-file] . helm-find-files)  ; Remember - this also opens URLs!
         ([remap occur] . helm-occur)
         ([remap dabbrev-expand] . helm-dabbrev)
         ([remap list-buffers] . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("C-x p" . helm-mini)

         :map lisp-interaction-mode-map
         ([remap completion-at-point] . helm-lisp-completion)

         :map emacs-lisp-mode-map
         ([remap completion-at-point] . helm-lisp-completion)

         :map md/leader-map
         ("bb" . helm-buffers-list)
         ("f" . helm-find-files)
         ("x" . helm-M-x)
         ("X" . helm-resume)
         ("p" . helm-mini)

         :map help-map
         ("X" . helm-colors)))

(use-package help-fns+ :defer 1)

(evil-set-initial-state 'help-mode 'normal)
(evil-define-key 'normal help-mode-map
  "q" 'quit-window
  (kbd "C-i") 'help-go-forward
  (kbd "C-o") 'help-go-back
  (kbd "<RET>") 'help-follow-symbol)

(defun md/quit-and-kill-window ()
  (interactive)
  (quit-window t))

(use-package helpful
  :defer 1
  :config
  (progn
    (unbind-key "h" help-map)  ;; view-hello-file by default
    (bind-key "f" 'helpful-function help-map)
    (bind-key "c" 'helpful-command help-map)
    (bind-key "v" 'helpful-variable help-map)
    (bind-key "kk" 'helpful-key help-map)
    (bind-key "h" 'helpful-at-point help-map)
    (evil-define-key 'normal helpful-mode-map
      "q" 'md/quit-and-kill-window)))

(defun md/which-key-patch ()
  "Override some which-key functions"
  (interactive)

;; TODO - this is failing to handle kdb values with periods? Eg. "C-a .. C-z"?
(fmakunbound 'which-key--show-keymap)
(defun which-key--show-keymap (keymap-name keymap &optional prior-args)
  "This is identical to the version shipped with which-key, but it returns the
function captured by user input."
  (setq which-key--current-prefix nil
        which-key--current-show-keymap-name keymap-name
        which-key--using-show-keymap t)
  (when prior-args (push prior-args which-key--prior-show-keymap-args))
  (when (keymapp keymap)
    (let ((formatted-keys (which-key--get-formatted-key-bindings
                           (which-key--get-keymap-bindings keymap))))
      (cond ((= (length formatted-keys) 0)
             (message "which-key: Keymap empty"))
            ((listp which-key-side-window-location)
             (setq which-key--last-try-2-loc
                   (apply #'which-key--try-2-side-windows
                          formatted-keys 0 which-key-side-window-location)))
            (t (setq which-key--pages-plist
                     (which-key--create-pages formatted-keys))
               (which-key--show-page 0)))))
  (let* ((key (key-description (list (read-key))))
         (next-def (lookup-key keymap (kbd key))))
    (cond ((and which-key-use-C-h-commands (string= "C-h" key))
           (which-key-C-h-dispatch))
          ((keymapp next-def)
           (which-key--hide-popup-ignore-command)
           (setq next-def (which-key--show-keymap (concat keymap-name " " key) next-def
                                   (cons keymap-name keymap))))
          (t (which-key--hide-popup)))
    next-def))

) ; Close md/which-key-patch

;; TODO There are some bindings that do not show up.
;; Eg. the C-x prefix displayed does not exactly match the real C-x prefix map
;; (eg. M-: repeat-complex-command is missing).
(defun md/get-all-active-bindings-as-keymap ()
  "Return keymap consisting of bindings in all active keymaps. This should
represent all current available bindings accurately as a single keymap."
  (let ((full-active-keymap (make-sparse-keymap)))
    (mapc (lambda (keymap)
            ;; Ignore empty keymaps
            (when (not (equal keymap (make-sparse-keymap)))
              (map-keymap (lambda (event definition)
                            (when (md/include-event-in-active-map event definition)
                              (define-key full-active-keymap
                                (vector event) definition)))
                          keymap)))
          ;; Reverse so that the keymaps with highest precendence
          ;; are written last, thus overriding the more global maps.
          (reverse (current-active-maps t)))
    full-active-keymap))

(defun md/include-event-in-active-map (event definition)
  "Placeholder"
  (and
   (not (equal definition 'digit-argument))))

(use-package which-key
  :defer 2
  :config (progn
            ;; Patch with my functions
            (md/which-key-patch)

            (setq which-key-idle-delay 0.1
                  which-key-max-description-length 30
                  which-key-allow-evil-operators nil
                  which-key-inhibit-regexps '("C-w")
                  which-key-show-operator-state-maps nil
                  which-key-sort-order 'which-key-key-order-alpha
                  which-key-highlighted-command-list '("md/"))

            ;; Use ESC/C-g to quit which-key. Not sure why the default key is 'a'.
            (bind-key "ESC" 'which-key-abort which-key-C-h-map)
            (bind-key "C-g" 'which-key-abort which-key-C-h-map)

            ;; This is the default for description-replacement-alist:
            (setq which-key-replacement-alist
                  '(((nil . "Prefix Command") nil . "prefix")
                    ((nil . "\\`\\?\\?\\'") nil . "lambda")
                    ((nil . "which-key-show-next-page-no-cycle") nil . "wk next pg")
                    (("<\\([[:alnum:]-]+\\)>") "\\1")
                    (("left") "←")
                    (("right") "→")))

            ;; Add scratch bindings:
            (dolist (mode '("elisp" "python" "restclient" "markdown" "gfm" "org"))
              (add-to-list 'which-key-replacement-alist
                           `((nil . ,(format "md/scratch-open-file-%s" mode)) nil . ,mode)))

            (which-key-add-key-based-replacements
              "SPC SPC" "major-mode"
              "SPC SPC e" "major-mode-eval"
              "SPC a" "org"
              "SPC A" "ag"
              "SPC b" "buffers"
              "SPC c" "comments"
              "SPC C" "compile"
              "SPC e" "eval"
              "SPC E" "Emacs"
              "SPC F" "Format"
              "SPC g" "git"
              "SPC h" "help"
              "SPC h k" "keys"
              "C-h k" "keys"
              "SPC h h" "helpful"
              "C-h h" "helpful"
              "SPC j" "project"
              "SPC j ;" "project-popwin"
              "SPC j a" "project-ag"
              "SPC l" "bookmarks"
              "SPC n" "narrow"
              "SPC P" "Packages"
              "SPC s" "flycheck"
              "SPC S" "flyspell"
              "SPC t" "display-options"
              "SPC v" "dotfiles"
              "SPC ;" "popwin"
              "SPC '" "scratch")
            (which-key-mode)

            (defun md/which-key ()
              "Use the which-key interface to list all active bindings and execute the
    current one. One prefix arg will pre-select the current evil-state in which-key,
    and two prefix args will let you choose an evil state to pre-select."
              (interactive)
              (catch 'no-evil-state-map
                (let* ((md-evil-state (cond ((equal current-prefix-arg '(4))
                                             (md/which-key--evil-state-current))
                                            ((equal current-prefix-arg '(16))
                                             (md/which-key--evil-state-select))))
                       (evil-keymap nil)
                       (base-keymap (md/get-all-active-bindings-as-keymap))
                       (final-keymap
                        (if md-evil-state
                            (progn
                              (message "evil state!")
                              (setq evil-keymap (lookup-key base-keymap md-evil-state))
                              (if (keymapp evil-keymap)
                                  evil-keymap
                                (throw 'no-evil-state-map
                                       (format "No available bindings for evil state %s" md-evil-state))))
                          base-keymap))
                       (chosen-func (which-key--show-keymap "All active bindings" final-keymap)))
                  (when (commandp chosen-func)
                    (message (format "calling interactively: %s" chosen-func))
                    (call-interactively chosen-func)))))


            (defconst md/which-key--evil-states '(normal-state
                                                  insert-state
                                                  visual-state
                                                  motion-state
                                                  replace-state
                                                  emacs-state))

            (defun md/which-key--evil-state-select ()
              "Return (kbd-for-state . local-keymap) for chosen Evil state"
              (kbd (format "<%s>" (completing-read "Evil state: " md/which-key--evil-states nil
                                                   t))))

            (defun md/which-key--evil-state-current ()
              "Return (kbd-for-state . local-keymap) for current Evil state"
              (kbd (format "<%s-state>" evil-state)))

            ))

(use-package free-keys
  :defer 10
  :config
    (progn
      (bind-key "@" 'free-keys help-map)))

(defvar md/keys-help-map (make-sparse-keymap))

(bind-key "k" md/keys-help-map help-map)

(bind-key "K" 'describe-keymap md/keys-help-map)
(bind-key "p" 'describe-personal-keybindings md/keys-help-map)
(bind-key "@" 'free-keys md/keys-help-map)
(bind-key "SPC" 'md/which-key md/keys-help-map)

(global-set-key (kbd "C-SPC") 'md/which-key)

;; Setting this mode on replaces describe-bindings, and
;; loads helm-descbinds.el, which I might want to use elsewhere.
(add-hook 'helm-descbinds-mode-hook
          (lambda () (bind-key "b" 'helm-descbinds md/keys-help-map)))

(use-package ag
  :config
  (progn
    ;; evil-integration basically breaks all sane bindings, so undo it.
    (-remove-item 'ag-mode evil-motion-state-modes)
    (add-hook 'ag-mode-hook 'evil-emacs-state)

    (defun md/ag-quit ()
      (interactive)
      (quit-window nil)
      (eyebrowse-close-window-config))

    ;; Not sure if there is a builtin way to achieve this.
    (defun md/compile-preview ()
      (interactive)
      (let ((current (get-buffer-window)))
        (compile-goto-error)
        (select-window current)))

    (defun md/ag-resume ()
      (interactive)
      (display-buffer "*ag search*"))

    ;; Make no-op as we only care about a few bindings
    (md/make-keymap-noop ag-mode-map)
    (bind-key "SPC" md/leader-map ag-mode-map)
    (bind-key "C-w" splitscreen/mode-map ag-mode-map)
    (bind-key "q" 'md/ag-quit ag-mode-map)
    (bind-key "RET" 'compile-goto-error ag-mode-map)
    (bind-key "TAB" 'md/compile-preview ag-mode-map)
    (bind-key "F" 'next-error-follow-minor-mode ag-mode-map) ;; Follow!
    (bind-key "n" 'compilation-next-error ag-mode-map)
    (bind-key "j" 'compilation-next-error ag-mode-map)
    (bind-key "p" 'compilation-previous-error ag-mode-map)
    (bind-key "k" 'compilation-previous-error ag-mode-map)
    (bind-key "C-n" 'compilation-next-file ag-mode-map)
    (bind-key "C-j" 'compilation-next-file ag-mode-map)
    (bind-key "C-p" 'compilation-previous-file ag-mode-map)
    (bind-key "C-k" 'compilation-previous-file ag-mode-map)
    (bind-key "C-f" 'evil-scroll-page-down ag-mode-map)
    (bind-key "C-b" 'evil-scroll-page-up ag-mode-map)
    (bind-key "C-d" 'evil-scroll-down ag-mode-map)
    (bind-key "G" 'evil-goto-line ag-mode-map)
    (bind-key "g" 'evil-goto-first-line ag-mode-map)

    ;; TODO - fix issue where ag will re-use an existing buffer if the buffer
    ;; that has a match is already open. Can wrap it in a temp shackle rule.

    (setq ag-context-lines nil
          ag-highlight-search t
          ag-reuse-buffers t  ; Only one buffer for ag searches
          ag-reuse-window nil))  ; Open files in new window, don't hide search window

  :bind (:map md/leader-map
              ("Ad" . ag-dired)
              ("Af" . ag-files)
              ("Ag" . ag)
              ("Aa" . md/ag-resume)
              ("/" . occur)))

(use-package company
  :defer 2
  :config
  (progn
    ;; Bind here rather than in ":bind" to avoid complaints about
    ;; company-mode-map not existing.
    (bind-key "C-n" 'company-select-next company-active-map)
    (bind-key "C-p" 'company-select-previous company-active-map)

    ;; By default this performs company-complete-common, but I don't
    ;; think I'll want to use that
    (bind-key "TAB" 'company-complete-selection company-active-map)

    (bind-key "C-n" 'company-complete evil-insert-state-map)

    (global-company-mode)))

(use-package flycheck
  :init
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode))
  :config
  (progn
    (defface md/modeline-flycheck-error '((t (:inherit 'error))) "")
    (defface md/modeline-flycheck-warning '((t (:inherit 'warning))) "")

    (setq-default flycheck-disabled-checkers

          ;; Most of these elisp warnings assume that I'm writing a proper package
          ;; with full documentation. This is usually not the case, so just
          ;; disable them.
          '(emacs-lisp-checkdoc))

    (setq flycheck-flake8rc ".config/flake8"
          flycheck-highlighting-mode 'symbols
          flycheck-display-errors-delay 0.1

          ;; Disabling this at is annoys me to have errors appearing
          ;; and disappearing quickly and messing with the size of the
          ;; window. I will just check the error list and the fringe.
          flycheck-display-errors-function nil

          flycheck-check-syntax-automatically '(mode-enabled)
          flycheck-idle-change-delay nil
          flycheck-mode-line-prefix nil)

    ;; For some reason in the flycheck mode list map it just uses all vi
    ;; keys. Mostly this is fine but I need an easy way to quit.
    (evil-define-key 'normal flycheck-error-list-mode-map "q" 'quit-window))
  :bind (:map md/leader-map
              ;; S prefix, ie. "syntax"
              ("s <RET>" . flycheck-mode)
              ("ss" . flycheck-buffer)
              ("sc" . flycheck-compile)
              ("sl" . flycheck-list-errors)
              ("sn" . flycheck-next-error)
              ("sj" . flycheck-next-error)
              ("sp" . flycheck-previous-error)
              ("sk" . flycheck-previous-error)
              ("S <RET>" . flyspell-mode)
              ("SS" . flyspell-correct-word-before-point)))

(setq compilation-mode-map (make-sparse-keymap))
(evil-set-initial-state 'compilation-mode 'normal)
(add-hook 'compliation-mode-hook 'evil-normal-state)
(evil-define-key 'normal compilation-mode-map "q" 'quit-window)

(use-package projectile
  :config
  (progn
    (setq projectile-file-exists-local-cache-expire 30
          projectile-enable-caching t
          projectile-globally-ignored-file-suffixes
          '("pyc"
            "png"
            "jpg"
            "gif"
            "zip"
            "Trash"
            "swp"
            "swo"
            "DS_Store"
            "swn"
            "ico"
            "o"
            "elc"
            "a"
            "so"
            "exe"
            "egg-info"
            "egg"
            "dmg")
          projectile-globally-ignored-directories
          '(".tmp"
            ".coverage"
            ".git"
            ".hg"
            ".idea"
            ".flsckout"
            ".bzr"
            "_darcs"
            ".tox"
            ".svn"
            ".egg"
            ".egg-info"
            ".sass-cache"
            "__pycache__"
            ".webassets-cache"
            "node_modules"
            "venv"
            "elpa"
            ".stack-work"))
    (projectile-mode 1))
  :bind (:map md/leader-map
              ("j!"  . projectile-invalidate-cache)
              ("jk"  . projectile-kill-buffers)
              ("jt" . projectile-run-term)
              ("jd" . projectile-dired)
              ("js" . projectile-run-shell)
              ("je" . projectile-run-eshell)))

(use-package helm-projectile :demand t
  :init (progn
          ;; This has to be set before loading helm-projectile
          (setq helm-projectile-fuzzy-match nil))
  :bind (:map md/leader-map
              ("jj" . md/projectile-switch-project)
              ("jag" . projectile-ag)
              ("jaf" . ag-project-files)
              ("jad" . ag-project-dired)
              ("jb" . helm-projectile)
              ("jp" . helm-projectile)
              ("jf" . helm-projectile-find-file)
              ("jF" . md/projectile-find-file-invalidate-cache)))

(defun md/projectile-switch-project ()
  (interactive)
  (let ((fn (which-key--show-keymap "switch project" (lookup-key md/leader-map "j")))
        (projectile-switch-project-action
         (lambda ()
           (let ((default-directory (projectile-project-root)))
             (call-interactively fn)))))
    (when fn
      (helm-projectile-switch-project))))

(defun md/projectile-find-file-invalidate-cache ()
  (interactive)
  (helm-projectile-find-file t))

(use-package dumb-jump
  :config
  (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm
        dumb-jump-force-searcher 'ag)
  (bind-key "gd" 'dumb-jump-go evil-normal-state-map))

(use-package ediff
 :defer 1
 :config
 (progn

   (defun md/ediff-scroll-left ()
     (interactive)
     (let ((last-command-event ?>))
       (ediff-scroll-horizontally 1)))

   (defun md/ediff-scroll-right ()
     (interactive)
     (let ((last-command-event ?<))
       (ediff-scroll-horizontally 1)))

   (defun md/ediff-scroll-up ()
     (interactive)
     (let ((last-command-event ?V))
       (ediff-scroll-vertically 1)))

   (defun md/ediff-scroll-down ()
     (interactive)
     (let ((last-command-event ?v))
       (ediff-scroll-vertically 1)))

   (setq
    ;; Horizontal instead of vertical splits.
    ediff-split-window-function 'split-window-horizontally

    ;; Make sure the ediff control window is NOT opened in a new frame.
    ediff-window-setup-function 'ediff-setup-windows-plain)

   (defvar md/ediff-help-changed nil)
   (defun md/ediff-adjust-help ()
     "Adjust long help messages to reflect evil-ediff bindings."
     (unless md/ediff-help-changed
       (dolist (msg '(ediff-long-help-message-compare2
                      ediff-long-help-message-compare3
                      ediff-long-help-message-narrow2
                      ediff-long-help-message-word-mode
                      ediff-long-help-message-merge
                      ediff-long-help-message-head
                      ediff-long-help-message-tail))
         (dolist (chng '(("p,DEL -previous diff " . " gk,p -previous diff ")
                         ("n,SPC -next diff     " . " gj,n -next diff     ")
                         ("    h -highlighting  " . "    H -highlighting  ")
                         ("    j -jump to diff  " . "    d -jump to diff  ")
                         ("  </> -scroll lt/rt  " . "  h/l -scroll lt/rt  ")
                         ("  v/V -scroll up/dn  " . "  k/j -scroll up/dn  ")
                         ("  z/q -suspend/quit  " . "  q/z -quit/suspend  ")))
           (setf (symbol-value msg)
                 (replace-regexp-in-string (car chng) (cdr chng) (symbol-value msg))))))
     (setq md/ediff-help-changed t))

   (defvar md/ediff-bindings
     '(("h" . md/ediff-scroll-left)
       ("j" . md/ediff-scroll-down)
       ("k" . md/ediff-scroll-up)
       ("l" . md/ediff-scroll-right)
       ("gj" . ediff-next-difference)
       ("gk" . ediff-previous-difference)
       ("d" . ediff-jump-to-difference)
       ("H" . ediff-toggle-hilit)
       ("q" . ediff-quit)))

   (defun md/ediff-startup-hook ()
     (evil-make-overriding-map ediff-mode-map 'normal)
     (dolist (entry md/ediff-bindings)
       (define-key ediff-mode-map (car entry) (cdr entry)))
     (evil-normalize-keymaps))

   ;; Override bindings
  (evil-set-initial-state 'ediff-mode 'normal)
  (add-hook 'ediff-startup-hook 'md/ediff-startup-hook)
  (md/ediff-adjust-help)

  ;; Ensure that outline buffers are expanded when in ediff mode, because
  ;; it doesn't automatically expand them, even if the diffs are inside a
  ;; hidden headline.
  (add-hook 'ediff-prepare-buffer-hook 'outline-show-all))

 :bind (:map md/leader-map
             ("D" . ediff)))

(use-package fic-mode
 :defer 1
 :init
 (progn
   (add-hook 'prog-mode-hook 'fic-mode))
 :config
 (progn
   ;; NOTE: fic-mode doesn't seem to fontify the buffer, so words don't appear
   ;; highlighted unless either something else fontifies the buffer, or we do it
   ;; manually. Would like to improve this.
   ;;
   ;; FIX: fic-mode doesn't seem to identify words on the same line as my cursor
   ;; when I change theme and then fontify the buffer. All other lines seem fine.

   (setq fic-highlighted-words
         '("TODO" "FIX" "FIXME" "BUG" "WARN" "WARNING" "HACK" "NOTE" "ERROR" "MATT" "DEPRECATED"))

   ;; By default this includes font-lock-string-face, but I don't want strings to
   ;; have these words formatted.
   (setq fic-activated-faces '(font-lock-doc-face font-lock-comment-face))))

(defun md/insert-todo-regexp ()
  (interactive)
  (insert "TODO|FIX|FIXME|BUG|WARN|HACK|ERROR"))

(bind-key "th" 'highlight-phrase md/leader-map)
(bind-key "tu" 'unhighlight-regexp md/leader-map)

(use-package paren
 :defer 1
 :init (progn
        (add-hook 'prog-mode-hook 'show-paren-mode))
 :config
 (progn
   (setq show-paren-style 'parenthesis
         blink-matching-paren nil
         blink-matching-paren-on-screen nil)))

(use-package rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (defun md/toggle-rainbow-delimiters ()
    (interactive)
    (if rainbow-delimiters-mode
        (rainbow-delimiters-mode -1)
      (rainbow-delimiters-mode 1)))
  :bind (:map md/leader-map
              ("t(" . md/toggle-rainbow-delimiters)))

(use-package rainbow-mode
  :defer 1
  :config
  (progn
     (add-hook 'css-mode-hook 'rainbow-mode)
     (add-hook 'help-mode-hook 'rainbow-mode)
     (add-hook 'html-mode-hook 'rainbow-mode))
  :bind (:map md/leader-map
              ("tr" . rainbow-mode)))

(use-package eldoc ;; builtin
  :config
  (setq eldoc-echo-area-use-multiline-p 'always
        eldoc-idle-delay 0.25
        ;; Makes much more usable imo
        eldoc-print-after-edit t))

(use-package hideshow
  :demand t
  :config (progn
            (add-hook 'hs-minor-mode-hook 'hs-hide-all)
            (add-hook 'prog-mode-hook 'hs-minor-mode)

            ;; Open all folds when searching
            (setq hs-isearch-open t)

            ;; Use same display for folds as org folds.
            (defun md/hideshow-overlay (ov)
              (overlay-put ov 'display (propertize (format "…") 'face 'org-ellipsis)))
            (setq hs-set-up-overlay 'md/hideshow-overlay)

            ;; As recommended in hideshow.el docs.
            (add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)))


(use-package hideshow-orgmode
  :demand t
  :config (progn
            (defun md/hideshow-add-bindings (keymap)
              (evil-define-key 'normal keymap
                (kbd "<backtab>") 'hs-cycle-all
                (kbd "<tab>") 'hs-cycle))
            (mapc 'md/hideshow-add-bindings
                  (list prog-mode-map
                        emacs-lisp-mode-map))))

(add-hook 'ansi-term-mode-hook 'evil-emacs-state)
(add-hook 'term-mode-hook 'evil-emacs-state)
(evil-set-initial-state 'ansi-term-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)

(defun md/emacs-lisp-hook ()
    (setq fill-column 100))
(add-hook 'emacs-lisp-mode-hook 'md/emacs-lisp-hook)

;; Jump to definition
(evil-define-key 'normal emacs-lisp-mode-map "gd" 'xref-find-definitions)

(use-package dash :demand t)
(use-package f :demand t)
(use-package s :demand t)

(require 'debug)
(add-hook 'debugger-mode-hook 'evil-emacs-state)
(md/make-keymap-noop debugger-mode-map)
(bind-key "j" 'evil-next-visual-line debugger-mode-map)
(bind-key "k" 'evil-previous-visual-line debugger-mode-map)
(bind-key "q" 'top-level debugger-mode-map)
(bind-key "n" 'debugger-step-through debugger-mode-map)
(bind-key "c" 'debugger-continue debugger-mode-map)
(bind-key "b" 'debugger-frame debugger-mode-map)  ;; breakpoint
(bind-key "u" 'debugger-frame-clear debugger-mode-map)
(bind-key "!" 'debugger-eval-expression debugger-mode-map)
(bind-key "v" 'debugger-toggle-locals debugger-mode-map)
;;(bind-key "C-w" splitscreen/mode-map debugger-mode-map)
(bind-key "SPC" md/leader-map debugger-mode-map)

;; This can be useful when debugging.
(setq edebug-trace t)

;; https://github.com/ScottyB/edebug-x
;; https://lists.gnu.org/archive/html/emacs-devel/2013-03/msg00304.html
;;
;; Provides some enhancements to edebug mode. Doesn't look like it is
;; maintained, but it's useful even if just for the syntax highlighting.
(use-package edebug-x)

(defun md/toggle-debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message (format "debug-on-error %s" debug-on-error)))

(defun md/edebug-quit ()
  (interactive)
  (top-level)
  (shackle--eyebrowse-close-slot-by-tag "debug"))

(bind-key "Ef" 'edebug-defun md/leader-map)
(bind-key "Ed" 'md/toggle-debug-on-error md/leader-map)
(bind-key "Em" 'view-echo-area-messages md/leader-map)

(add-hook 'edebug-mode-hook 'evil-normal-state)
(md/make-keymap-noop edebug-mode-map)

(bind-key "q" 'md/edebug-quit edebug-mode-map)
(bind-key "S" 'edebug-stop edebug-mode-map)
(bind-key "n" 'edebug-step-mode edebug-mode-map)
(bind-key "g" 'edebug-go-mode edebug-mode-map)
(bind-key "G" 'edebug-Go-nonstop-mode edebug-mode-map)
(bind-key "E" 'edebug-visit-eval-list edebug-mode-map)
(bind-key "I" 'edebug-instrument-callee edebug-mode-map)
(bind-key "i" 'edebug-step-in edebug-mode-map)
(bind-key "o" 'edebug-step-out edebug-mode-map)
(bind-key "b" 'edebug-set-breakpoint edebug-mode-map)
(bind-key "u" 'edebug-unset-breakpoint edebug-mode-map)
(bind-key "d" 'edebug-backtrace edebug-mode-map)
(bind-key "r" 'edebug-previous-result edebug-mode-map)
(bind-key "w" 'edebug-where edebug-mode-map)
(bind-key "SPC" md/leader-map edebug-mode-map)

(defun md/python-pudb-toggle-breakpoint ()
  (interactive)
  (let ((trace "import pudb; pu.db")
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (python-indent-line)
        (insert "\n")
        (python-indent-line)))))

(defun md/python-mode-hook ()
  (md/hideshow-add-bindings python-mode-map)
  (setq-local fill-column 90))
(add-hook 'python-mode-hook 'md/python-mode-hook)

(use-package find-file) ;; builtin, provides ff-basename
(defun md/refresh-python-path ()
  (interactive)
  (when (f-directory? "/server/apps")
    (-each (directory-files "/server/apps" t)
      (lambda (f)
        (when (and (f-directory? f)
                   (not (-contains? '("." "..") (ff-basename f))))
          (add-to-list 'python-shell-extra-pythonpaths f))))))

(use-package python ;; builtin
  :config
  (progn
    (evil-define-key 'normal python-mode-map
      (kbd "SPC") md/python-mode-leader-map
      "gk" 'python-nav-backward-defun
      "gj" 'python-nav-forward-defun)
  (md/refresh-python-path))
  :bind (:map md/python-mode-leader-map
              ("SPC B" . md/python-pudb-toggle-breakpoint)))

;; Provide autocomplete, jump to definition and eldoc integration.
(use-package anaconda-mode
  :defer 1
  :config
  (progn
    (defun md/anaconda-set-company-backend ()
      (interactive)
      (set (make-local-variable 'company-backends) '(company-anaconda)))
    (add-hook 'anaconda-mode-hook 'md/anaconda-set-company-backend)
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

    ;; TODO make sure this jumps to the current buffer
    (evil-define-key 'normal python-mode-map
      "gd" 'anaconda-mode-find-assignments
      "gD" 'anaconda-mode-find-definitions)

    (defun md/anaconda-quit ()
      (interactive)
      (quit-window)
      (shackle--eyebrowse-close-slot-by-tag "anaconda"))

    ;; TODO ideally this would open in a separate eyebrowse slot, and if you
    ;; press enter would jump to the buffer in your original window, but keep
    ;; the slot open.
    ;; q should return to the original position.
    ;; This behaviour should be consistent with eg. ag and other grep-type buffers.
    (evil-define-key 'normal anaconda-mode-view-mode-map
      "q" 'md/anaconda-quit
      (kbd "C-j") 'next-error-no-select
      (kbd "C-n") 'next-error-no-select
      (kbd "C-k") 'previous-error-no-select
      (kbd "C-p") 'previous-error-no-select))
  :bind (:map md/python-mode-leader-map
              ("SPC r" . anaconda-mode-find-references)
              ("SPC d" . anaconda-mode-show-doc)))

;; TODO pyvenv auto? Might work better to just have one emacs virtualenv.
;;   https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/python/packages.el#L205

(use-package py-isort
  :bind (:map md/python-mode-leader-map
              ("SPC I" . py-isort-buffer)))

;; Provide company completion for anaconda
(use-package company-anaconda)

;; Syntax and completion for pip requirements files.
(use-package pip-requirements)

(use-package pytest
  :commands (pytest-all pytest-one pytest-failed pytest-pdb-one pytest-pdb-all)
  :bind (:map md/python-mode-leader-map
              ("SPC T T" . pytest-all)
              ("SPC T t" . pytest-one)
              ("SPC T p" . pytest-pdb-one)
              ("SPC T P" . pytest-pdb-all)
              ("SPC T f" . pytest-failed)))

(defun md/blacken-buffer ()
  (interactive)
  (font-lock-mode -1)
  (blacken-buffer)
  (font-lock-mode 1))

(use-package blacken
  :bind (:map md/python-mode-leader-map
              ("SPC F" . md/blacken-buffer)))

;; Fallback to use if pycheckers doesn't find the right virtualenv.
(use-package pyvenv :demand t :config (pyvenv-workon "emacs"))

;; This is useful:
;; - Multiple concurrent python checkers
;; - Auto-guesses the virtualenv to use for the checks
(use-package flycheck-pycheckers
  :demand t
  :config (progn
            (add-hook 'flycheck-mode-hook 'flycheck-pycheckers-setup)
            (setq flycheck-pycheckers-checkers '(flake8)
                  flycheck-pycheckers-ignore-codes nil

                  ;; Seems this is required - would prefer to leave it to
                  ;; pylint/flake8 config though.
                  flycheck-pycheckers-max-line-length 120)))

(use-package git-commit
  :config
  (progn
    (defun md/git-commit-set-fill-column ()
      (interactive)
      (setq fill-column 70))
    (add-hook 'git-commit-setup-hook 'md/git-commit-set-fill-column)
    (global-git-commit-mode t)))

(use-package git-gutter
 :init
 (progn
   (defun md/set-sensible-column ()
     "Unless file is too big, either git-gutter mode (when in git dir)"
     (interactive)
     (when (and (< (count-lines (point-min) (point-max)) 1500)
                (not (and (boundp 'writeroom-mode) writeroom-mode))
                (not (eq major-mode 'org-mode)))
       (if (string= "git" (downcase (format "%s" (vc-backend
                                                  (buffer-file-name
                                                   (current-buffer))))))
           (git-gutter-mode 1))))
   (add-hook 'find-file-hook 'md/set-sensible-column))


 :config
 (progn
   (setq git-gutter:ask-p nil  ; Don't ask for confirmation of gadd
         git-gutter:modified-sign "~"
         git-gutter:added-sign "+"
         git-gutter:deleted-sign "-"

         ;; This ensures the separator is always displayed
         git-gutter:unchanged-sign " "
         git-gutter:always-show-separator t

         ;; Without this, there's no space between the git-gutter column and the code.
         git-gutter:separator-sign " "))
 :bind (:map md/leader-map
       ("g <RET>" . git-gutter-mode)
       ("gk" . git-gutter:previous-hunk)
       ("gp" . git-gutter:previous-hunk)
       ("gj" . git-gutter:next-hunk)
       ("gn" . git-gutter:next-hunk)
       ("g+" . git-gutter:stage-hunk)
       ("g-" . git-gutter:revert-hunk)))

(use-package magit
 :config
 (progn
   (evil-set-initial-state 'magit-blame-mode 'normal)
   (evil-set-initial-state 'magit-revision-mode 'normal)
   (evil-set-initial-state 'magit-diff-mode 'normal)
   (evil-set-initial-state 'magit-status-mode 'normal)

   (add-hook 'magit-diff-mode 'evil-normal-state)
   (add-hook 'magit-status-mode 'evil-normal-state)

   (defun md/magit-quit ()
     (interactive)
     (magit-mode-bury-buffer)
     (shackle--eyebrowse-close-slot-by-tag "git"))

   (evil-define-key 'normal magit-mode-map
     (kbd "TAB") 'magit-section-toggle
     (kbd "<RET>") 'magit-visit-thing
     "q" 'md/magit-quit
     "r" 'magit-refresh
     "n" 'magit-section-forward
     "p" 'magit-section-backward
     "+" 'magit-stage-file
     "-" 'magit-unstage-file
     "[" 'magit-diff-less-context
     "]" 'magit-diff-more-context
     )

   (evil-define-key 'emacs magit-log-mode-map
     "q" 'md/magit-quit)

   ;;(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
   (setq magit-display-buffer-function 'display-buffer)

   ;; I don't know why, but by default I can't get magit-blame to adhere to my
   ;; normal-mode map below, even though Evil says I'm in normal mode. Explicitly
   ;; calling evil-normal-state fixes it.
   (add-hook 'magit-blame-mode-hook 'evil-normal-state)
   (evil-define-key 'normal magit-blame-mode-map
     (kbd "<RET>") 'magit-show-commit
     "q" 'magit-blame-quit
     "gj" 'magit-blame-next-chunk
     "gn" 'magit-blame-next-chunk
     "gk" 'magit-blame-previous-chunk
     "gp" 'magit-blame-previous-chunk)

   (add-hook 'magit-revision-mode-hook 'evil-normal-state)
   (evil-define-key 'normal magit-revision-mode-map
     (kbd "<RET>") 'magit-diff-visit-file
     "q" 'magit-mode-bury-buffer))  ;; This quits

 :bind (:map md/leader-map
       ("gg" . magit-status)
       ("gm" . magit-dispatch-popup)
       ("gb" . magit-blame)
       ("gl" . magit-log-head)

       ;; Diff gives the full git diff output. Ediff shows ediff for a single
       ;; file.
       ("gd" . magit-diff-buffer-file)
       ("gD" . magit-diff-dwim)
       ("ge" . magit-ediff-popup)

       ;; NOTE - this doesn't play nicely with mode-line:
       ;; - https://github.com/magit/magit/blob/master/Documentation/magit.org#the-mode-line-information-isnt-always-up-to-date
       ;; - https://github.com/syl20bnr/spacemacs/issues/2172
       ("gC" . magit-commit-popup)
       ("gc" . magit-checkout)))

(use-package github-browse-file
  :config
  (progn
    (setq github-browse-file-show-line-at-point t))
  :bind (:map md/leader-map
        ("go" . github-browse-file)))

;; I don't need to confirm this via prompt.
(setq vc-follow-symlinks t)

(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

(use-package php-mode
	:config (progn
			(defun md/ometria-php-mode-hook ()
				(when (s-starts-with-p "omattria" (system-name))
				(setq-local indent-tabs-mode t)
				(setq-local tab-width 4))
				;;(whitespace-mode)
				;; Don't auto indent as php indentation doesn't match existing conventions
				;; on om.console
				(electric-indent-mode -1))

			(add-hook 'php-mode-hook 'md/ometria-php-mode-hook)))

(when (not (getenv "GOPATH"))
 (setenv "GOPATH" "/Users/matt/golang")
 (setenv "GO15VENDOREXPERIMENT" "1"))

(use-package go-mode
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)

    ;; Make sure SPC uses the go-mode leader map rather than my default leader
    ;; map
    (evil-define-key 'normal go-mode-map
      (kbd "SPC") md/go-mode-leader-map))

  :bind (:map md/go-mode-leader-map
              ("SPC =" . gofmt)))

(use-package company-go
  :init
  (progn
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))))))

(use-package scheme
  :config
  (progn
    ;; For SICP
    (setq scheme-program-name "/usr/local/bin/mit-scheme")

    ;; Setup leader map for this major mode
    (evil-define-key 'normal scheme-mode-map
      (kbd "SPC") md/scheme-mode-leader-map)
    (evil-define-key 'visual scheme-mode-map
      (kbd "SPC") md/scheme-mode-leader-map)

    ;; When I run the "send-to" functions I want to see the results
    ;; in the popwin window
    (defun md/scheme--eval (fn)
      (save-window-excursion
        (call-interactively 'run-scheme))
        (call-interactively fn)
      (popwin:display-buffer (get-buffer "*scheme*")))

    (defun md/scheme-send-last-sexp ()
      (interactive)
      (md/scheme--eval 'scheme-send-last-sexp))

    (defun md/scheme-send-region ()
      (interactive)
      (md/scheme--eval 'scheme-send-region))

    (defun md/scheme-send-defun ()
      (interactive)
      (md/scheme--eval 'scheme-send-definition)))

  :bind (:map md/scheme-mode-leader-map
              ("SPC ee" . md/scheme-send-last-sexp)
              ("SPC ef" . md/scheme-send-defun)
              ("SPC er" . md/scheme-send-region)))

(use-package yaml-mode)

(use-package lua-mode)

(use-package terraform-mode)

(use-package web-mode
  :defer 1)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.gfm\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.apib\\'" . markdown-mode)  ; Apiary
         ("\\.markdown\\'" . markdown-mode))
  :config (progn
            ;; Markdown-cycle behaves like org-cycle, but by default is only
            ;; enabled in insert mode. gfm-mode-map inherits from
            ;; markdown-mode-map, so this will enable it in both.
            (evil-define-key 'normal markdown-mode-map
              (kbd "TAB") 'markdown-cycle
              "gk" 'markdown-previous-visible-heading
              "gj" 'markdown-next-visible-heading)))

(use-package conf-mode
  :mode (("\\.conf\\'" . conf-mode)
         ("\\.cfg\\'" . conf-mode)
         ("\\.*rc\\'" . conf-mode)
         ("\\.ssh/config\\'" . conf-mode)
         ("\\.ini\\'" . conf-mode)))

(use-package coffee-mode)

(use-package dockerfile-mode)

(use-package csv-mode)

(use-package feature-mode
  :config (progn
            (setq feature-indent-offset 4
                  feature-indent-level 4)))

(use-package org
  :pin org
  :defer 5
  :config
  (progn

(defun md/org-hook ()
  ;; Change tab widths to fit headline indents
  (setq tab-width 2
        evil-shift-width 2)

  ;; Disable in-buffer line numbers and the colour column, as both decrease
  ;; org-mode / outline performance significantly on large files.
  (linum-mode 0)
  (fci-mode 0)

  ;; Also disable the row and column numbers in the modeline. Seems you have to set
  ;; these manually to make them buffer-local.
  (setq-local line-number-mode nil)
  (setq-local column-number-mode nil)

  ;; Also don't highlight the current line. For some reason this requires making
  ;; global-hl-line-mode buffer-local.
  (make-variable-buffer-local 'global-hl-line-mode)
  (setq-local global-hl-line-mode nil))
(add-hook 'org-mode-hook 'md/org-hook)

(setq
      ;; Whether to let org-agenda permanently mess with window layout
      org-agenda-restore-windows-after-quit nil

      ;; I'm trying this to decrease wait times for the agenda windows
      org-agenda-sticky t

      ;; I find this more intuitive
      org-indirect-buffer-display 'current-window

      ;; Add timestamp when set task as closed
      org-log-done 'time

      ;; Colour the whole headline
      org-level-color-stars-only nil

      ;; Colour done headlines to make them less prominent
      org-fontify-done-headline t

      ;; Try to prevent accidentally editing invisible lines
      org-catch-invisible-edits 'show-and-error

      ;; Don't indent things for nested headings (eg. properties)
      org-adapt-indentation nil

      org-clock-out-remove-zero-time-clocks t

      ;; Use UTF-8 ellipsis character
      org-ellipsis "…"

      ;; If press M-RET I want a new line, not to split the line
      org-M-RET-may-split-line nil)

;; Goto and refile
(setq
      ;; For org-goto, use helm rather than the weird default interface
      ;; where you search through the file
      org-goto-interface 'outline-path-completion

      ;; For org-goto, search all nodes rather than forcing me to start with
      ;; the top level heading and then search the children etc.
      org-outline-path-complete-in-steps nil

      ;; For org-goto and org-refile, show the outline path during Helm
      ;; completion rather than just the headline.
      org-refile-use-outline-path t

      ;; Include nested items in org-refile.
      org-refile-targets '((nil :maxlevel . 9)))


;; Only two priorities - default and flagged
(setq org-highest-priority 65)
(setq org-lowest-priority 66)
(setq org-default-priority 66)

;; I find these useful enough to want them in all insert maps.
(bind-key "C-c d" 'md/org-timestamp-date-inactive-no-confirm org-mode-map)
(bind-key "C-c d" 'md/org-timestamp-date-inactive-no-confirm evil-insert-state-map)
(bind-key "C-c t" 'md/org-timestamp-time-inactive-no-confirm org-mode-map)
(bind-key "C-c t" 'md/org-timestamp-time-inactive-no-confirm evil-insert-state-map)

(bind-key "C-c l" 'md/org-insert-link-from-paste org-mode-map)

(evil-define-key 'normal org-mode-map (kbd "SPC") md/org-mode-leader-map)

;; TODO - make closer to the C-c map / confirm my bindings
;; TODO - for all relevant evil modes
;; TODO - do i need to do this for org-agenda map too??
;; TODO - make macro to run sth with given evil modes??
(bind-key "SPC j" 'md/helm-org md/org-mode-leader-map)
(bind-key "SPC J" 'org-goto md/org-mode-leader-map)
(bind-key "SPC R" 'org-refile md/org-mode-leader-map)
(bind-key "SPC r" 'org-review md/org-mode-leader-map)
(bind-key "SPC A" 'org-archive-to-archive-sibling md/org-mode-leader-map)
(bind-key "SPC i" 'org-clock-in md/org-mode-leader-map)
(bind-key "SPC o" 'org-clock-out md/org-mode-leader-map)
(bind-key "SPC E" 'org-export-dispatch md/org-mode-leader-map)
(bind-key "SPC I" 'org-tree-to-indirect-buffer md/org-mode-leader-map)
(bind-key "SPC t" 'org-todo md/org-mode-leader-map)
(bind-key "SPC c" 'org-ctrl-c-ctrl-c md/org-mode-leader-map)
(bind-key "SPC l" 'md/org-insert-link-from-paste md/org-mode-leader-map)
(bind-key "SPC O" 'org-open-at-point md/org-mode-leader-map)

;; Global org leader bindings
(bind-key "a a" 'org-agenda md/leader-map)
(bind-key "a c" 'org-capture md/leader-map)

(defvar md/org-inherit-dates-p t
  "Used by a couple of my util functions, eg. md/org-skip-if-deadline-in-days.")

(defun md/org-timestamp-time-inactive-no-confirm ()
  "Insert inactive time timestamp without prompting the user"
  (interactive)
  (org-insert-time-stamp (current-time) t t))

(defun md/org-timestamp-date-inactive-no-confirm ()
  "Insert inactive date timestamp without prompting the user"
  (interactive)
  (org-insert-time-stamp (current-time) nil t))

(defun md/org-insert-link-from-paste ()
  "Perform org-insert-link with the current contents of the clipboard"
  (interactive)
  (org-insert-link nil
                   (with-temp-buffer
                     (evil-paste-after nil)
                     (delete-trailing-whitespace)
                     (buffer-string))))


(defun md/org-back-to-top-level-heading ()
  "Go back to the current top level heading."
  (interactive)
  (or (re-search-backward "^\* " nil t)
      (goto-char (point-min))))

(defun md/org-get-priority (inherit)
  "Get priority for the current line. If inherit is t, retrieve
priority from the closest parent headline"
  (if (not inherit)
      (org-get-priority (thing-at-point 'line))
    (save-excursion
      (let ((keep-searching t))
        (while keep-searching
          (if (string-match org-priority-regexp (thing-at-point 'line))
              (setq keep-searching nil)
            (setq keep-searching (org-up-heading-safe))))
        (org-get-priority (thing-at-point 'line))))))

(defun md/org-agenda-skip-rtn-point ()
  "If you customise org-agenda-skip-function, your skip function has to return
the point at which you want the agenda to continue processing the file. In my
  case I always want this to be the end of the subtree."
  (org-end-of-subtree t)
  (point))

(defun md/org-skip-if-deadline-in-days (user-fn)
  "A utility function that can be used for org-agenda-skip-function. It calls
  `(user-fn number-of-days-until-deadline)`. If `user-fn` returns a `t` value,
  the agenda item will be skipped.

  You could use this to eg. skip all items that have a deadline
  in more than 60 days by running as part of your config:

  '(org-agenda-skip-function
      '(md/org-skip-if-deadline-in-days (lambda (d) (if (eq d nil) t (> d 60)))))
  "
  (save-excursion
    (let* ((deadline-time (org-get-deadline-time (point)
                                                    md/org-inherit-dates-p))
           (days (if deadline-time
                     (time-to-number-of-days
                      (time-subtract deadline-time (org-current-effective-time))))))
        (if (funcall user-fn days)  ; Returns t if we should skip
            (md/org-agenda-skip-rtn-point))
        )))

(defun md/org-skip-if-scheduled-in-days (user-fn)
  "This function is just like org-skip-if-deadline-in-days, but
uses the scheduled property rather than the deadline."
  (save-excursion
    (let* ((scheduled-time (org-get-scheduled-time (point)
                                                    md/org-inherit-dates-p))
           (days (if scheduled-time
                     (time-to-number-of-days
                      (time-subtract scheduled-time (org-current-effective-time))))))
        (if (funcall user-fn days)  ; Returns t if we should skip
            (md/org-agenda-skip-rtn-point))
        )))

(defun md/org-skip-if-priority-level (user-fn)
  "Utility function that can be used for org-agenda-skip-function. It calls
  `(user-fn priority-level`). If `user-fn` returns a `t` value, the agenda item
  will be skipped. If the item doesn't have a priority assigned, the level used
  is 0.

  You could use this to eg. skip all items that don't have a priority:

  '(org-agenda-skip-function
       '(md/org-skip-if-priority-level (lambda (p) (<= p 0))))
  "
  (save-excursion
    (if (funcall user-fn (md/org-get-priority t))
        (md/org-agenda-skip-rtn-point))))

(defun md/org-skip-if-not-match-parent-kwd (keyword)
  "Utility function that can be used for org-agenda-skip-function. It searches
  parents of the current node for a matching keyword. If the match isn't found,
  it returns the skip point expected by org-agenda-skip-function.
  "
  (save-excursion
    (let (top bottom)
        (setq bottom (save-excursion (org-end-of-subtree t) (point)))
        (setq top (save-excursion (md/org-back-to-top-level-heading) (point)))
        (if
            (re-search-backward (format org-heading-keyword-regexp-format keyword) top t)
            nil
        (md/org-agenda-skip-rtn-point))
        )))

(define-minor-mode md/evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines md/evil-org-mode-map
  :group 'md/evil-org)

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

;; Normal and insert state shortcuts.
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

(add-hook 'org-mode-hook 'md/evil-org-mode)

(defun md/org-gcal-fetch-and-agenda-redo ()
  (interactive)
  (ignore-errors
    (md/org-gcal-fetch))
  (org-agenda-redo))

(define-minor-mode md/evil-org-agenda-mode
  "Buffer local minor mode for evil-org-agenda"
  :init-value nil
  :lighter " EvilOrgAgenda"
  :keymap (make-sparse-keymap) ; defines md/evil-org-agenda-mode-map
  :group 'md/evil-org-agenda)

(defun md/org-agenda-quit ()
  (interactive)
  (org-agenda-quit)
  (shackle--eyebrowse-close-slot-by-tag "agenda"))

(evil-set-initial-state 'org-agenda-mode 'normal)

(evil-define-key 'normal md/evil-org-agenda-mode-map
  ;; j / k
  (kbd "j") 'org-agenda-next-line
  (kbd "n") 'org-agenda-next-line
  (kbd "C-n") 'org-agenda-next-line
  (kbd "k") 'org-agenda-previous-line
  (kbd "p") 'org-agenda-previous-line
  (kbd "c") 'org-agenda-capture
  (kbd "C-p") 'org-agenda-previous-line

  (kbd "RET") 'org-agenda-goto

  (kbd "]") 'org-agenda-later
  (kbd "[") 'org-agenda-earlier

  (kbd "q") 'md/org-agenda-quit
  (kbd "r") 'md/org-gcal-fetch-and-agenda-redo  ; Recalculate the agenda
  (kbd "v") 'org-agenda-view-mode-dispatch  ; Alter the view - toggle archived, logs, clocks etc.
  (kbd "|") 'org-agenda-filter-remove-all  ; Remove existing filters
  (kbd "/") 'org-agenda-filter-by-regexp  ; Search
  (kbd "C-/") 'org-agenda-filter-by-tag  ; Tag filter
  (kbd "^") 'org-agenda-filter-by-top-headline  ; Show other items with same
                                    ; headline as current
  (kbd "A") 'org-agenda-append-agenda)  ; Add another agenda

(add-hook 'org-agenda-mode-hook 'md/evil-org-agenda-mode)

(require 'helm-org)  ; this is part of the helm source but not loaded by default

(setq helm-org-format-outline-path nil)
(setq helm-org-headings-fontify t)

(defun md/helm-org ()
  "Open org headlines in helm."
  (interactive)
  (let* ((src (helm-build-sync-source "Org headings"
                :candidates (helm-org-get-candidates (file-expand-wildcards buffer-file-name))
                :action '(("Go to heading" . helm-org-goto-marker)
                          ("Open in indirect buffer" . helm-org--open-heading-in-indirect-buffer)
                          ("Refile to this heading" . helm-org-heading-refile)
                          ("Insert link to this heading" . helm-org-insert-link-to-heading-at-marker))))
         (cmd (helm :sources '(src))))))

;; Load some language support
  (require 'ob-restclient)
  (require 'ob-python)
  (require 'ob-C)
  (require 'ob-go)

(setq
 ;; Fontify inline code
 org-src-fontify-natively t

 ;; When editing code, I don't want to open another window. This
 ;; just makes the screen tidier.
 org-src-window-setup 'current-window

 ;; tab / indentation is the main reason I would use C-' so prevent it if possible
 org-src-tab-acts-natively t)

(setq org-export-headline-levels 6
      org-export-with-section-numbers 4)

(use-package ox-reveal)

(defun md/org-gcal-fetch ()
  "Always refresh gcal token before fetching, as it expires every hour"
  (interactive)
  (ignore-errors ;; Error is thrown if token doesn't already exist in the gcal file
    (org-gcal-refresh-token))
  (sleep-for 1)
  (org-gcal-fetch))

(use-package org-gcal
  :config
  (progn
    (setq

     ;; Don't archive entries to a separate file
     org-gcal-auto-archive nil

     org-gcal-down-days 120
     org-gcal-up-days 120)))

(defconst md/org-review-property "LAST_REVIEWED"
  "I use this in a few places to keep track of when I lasted reviewed particular
headlines")

(defun org-review ()
  "Set the LAST_REVIEWED property to the current date/time"
  (interactive)
  (org-set-property md/org-review-property ; currently this is LAST_REVIEWED
                    (with-temp-buffer
                      (org-insert-time-stamp (current-time) nil t)))) ; Inactive stamp

(define-key org-mode-map (kbd "C-c C-r") 'org-review)

(bind-key "C-c a" 'org-agenda global-map)
(bind-key "C-c c" 'org-capture global-map)

))

(use-package dired
  :demand t
  :init
  (progn
     ;; Use human size
     (setq dired-listing-switches "-alh")

    ;; evil-integrations.el (https://github.com/emacsmirror/evil/blob/cd005aa50ab056492752c319b5105c38c79c2fd0/evil-integration.el#L111)
    ;; makes dired-mode-map an overriding keymap, which means that the default
    ;; dired-mode bindings take precendence over the normal-state bindings.
    ;;
    ;; There's no obvious way to undo that code, so I'm just replacing
    ;; dired-mode-map with a new keymap that has /not/ been made 'overriding'.
    ;;
    ;; NOTE: this broke (require 'dired-x) because dired-x expects [menu-bar] to
    ;; be bound in dired-mode-map, so we rebind it.
    (let ((menu (lookup-key dired-mode-map [menu-bar])))
      (setq dired-mode-map (make-sparse-keymap))
      (bind-key [menu-bar] menu dired-mode-map))

    (evil-define-key 'normal dired-mode-map
      "W" 'wdired-change-to-wdired-mode  ; This is v useful
      "q" 'md/quit-and-kill-window
      "d" 'dired-flag-file-deletion
      "u" 'dired-unmark
      "D" 'dired-do-delete
      (kbd "RET") 'dired-find-alternate-file
      "J" 'dired-jump
      "o" 'dired-find-file-other-window
      (kbd "TAB") 'dired-find-file-other-window
      "R" 'dired-do-rename
      "C" 'dired-do-copy
      "i" 'dired-maybe-insert-subdir
      "+" 'dired-create-directory))
  :bind (:map md/leader-map
                  ("d" . dired)))

(use-package neotree
  :demand t
  :config
  (progn
    (evil-set-initial-state 'neotree-mode 'normal)
    (setq neo-theme 'nerd neo-smart-open t neo-show-hidden-files
          t)

    (bind-key "N" 'neotree-toggle md/leader-map)
    (evil-define-key 'normal neotree-mode-map (kbd "J") 'neotree-dir)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "'") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-select-up-node)
    (evil-define-key 'normal neotree-mode-map (kbd "L") 'neotree-change-root)
    (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "C-c C-c")
      'neotree-change-root))
  :bind (:map md/leader-map
              (";d" . neotree-toggle)))

(use-package restclient
  :defer 1
  :mode (("\\.http\\'" . restclient-mode)))

(use-package restclient-helm :defer 5)

(use-package company-restclient
  :config
  (progn
      (add-to-list 'company-backends 'company-restclient)))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-extension-type-list '(ghost-text)
        atomic-chrome-buffer-open-style 'frame
        atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-url-major-mode-alist '(("github\\.com" . gfm-mode))))

(use-package mu4e
  :config
  (progn

   (bind-key "M" 'mu4e md/leader-map)

    (setq
     ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
     ;; automatically.
     mu4e-sent-messages-behavior 'delete

     ;; We're using mbsync to fetch mail
     ;;mu4e-get-mail-command "mbsync -a"
    mu4e-get-mail-command "true"

     ;; Use Helm (defaults to ido)
     mu4e-completing-read-function 'completing-read

     ;; UTF-8 instead of letters
     mu4e-use-fancy-chars t

     mu4e-confirm-quit nil
     mu4e-compose-dont-reply-to-self t

     ;; This is supposed to fix issues with duplicate UIDs when using mbsync.
     mu4e-change-filenames-when-moving t

     mu4e-view-date-format "%Y-%m-%d %H:%M"
     mu4e-view-show-addresses t
     mu4e-view-prefer-html nil
     mu4e-view-auto-mark-as-read nil

     ;; TODO - does this have any bad effects??
     mu4e-headers-skip-duplicates t

     mu4e-headers-date-format "%Y/%m/%d %H:%M"
     mu4e-headers-date-format-long "%Y/%m/%d %H:%M"
     mu4e-headers-time-format "%H:%M"
     mu4e-headers-include-related nil
     mu4e-headers-fields
     '((:flags . 6) (:from . 22) (:thread-subject . 60)
       (:human-date . 19) (:to . 22) (:maildir))

     mu4e-headers-visible-flags
     '(draft flagged replied trashed attach encrypted signed)

     mu4e-headers-new-mark       '("N" . "N")
     mu4e-headers-unread-mark    '("u" . "u")
     mu4e-headers-seen-mark      '("S" . "S")
     mu4e-headers-attach-mark    '("a" . "a")
     mu4e-headers-draft-mark     '("D" . "⚒")
     mu4e-headers-flagged-mark   `("F" . "★★ ")
     mu4e-headers-encrypted-mark '("x" . "x")
     mu4e-headers-trashed-mark   '("T" . "⏚")
     mu4e-headers-signed-mark    '("s" . "☡")
     mu4e-headers-passed-mark    '("P" . "❯")  ;; ie. forwarded
     mu4e-headers-replied-mark   '("R" . "❮"))

     ;; Custom view actions
     (add-to-list 'mu4e-view-actions
                  '("ViewInBrowser" . mu4e-action-view-in-browser) t)))

(use-package mu4e-alert
  :disabled
  :config
  (mu4e-alert-set-default-style 'notifier)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (setq
   mu4e-alert-notify-repeated-mails nil
   mu4e-alert-email-notification-types '(count)))

(use-package mu4e-maildirs-extension
  :after mu4e
  :config
  (progn
    (mu4e-maildirs-extension)))

(use-package evil-mu4e
  :after mu4e
  :config
  (evil-mu4e-init))

(defun md/send-mail-validator ()
    (unless (yes-or-no-p "Are you want to send this mail? ")
      (signal 'quit nil)))

(use-package message
  :config
  (add-hook 'message-send-hook 'md/send-mail-validator))

(use-package smtpmail
  :config
  (setq mail-user-agent 'message-mail-user-agent
        message-send-mail-function 'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        sendmail-program "/usr/local/bin/msmtp"))

(use-package edit-indirect
  :demand t
  :config
  (progn
    (defun md/edit-indirect-guess-mode (parent-buffer beg end)
      (let ((major (with-current-buffer parent-buffer major-mode)))
        (cond ((eq major 'python-mode)
               (sql-mode))
              ((eq major 'php-mode)
               (sql-mode))
              ((eq major 'web-mode)
               (sql-mode))
              (t (funcall major)))))
    (setq edit-indirect-guess-mode-function 'md/edit-indirect-guess-mode)))

;; Don't ask for confirmation on narrow-to-region
(put 'narrow-to-region 'disabled nil)

(bind-key "n" narrow-map md/leader-map)
(bind-key "i" 'org-tree-to-indirect-buffer narrow-map)
(bind-key "v" 'md/narrow-to-region-indirect narrow-map)
(bind-key "f" 'md/narrow-dwim narrow-map)
(bind-key "r" 'narrow-to-region narrow-map)  ; Duplicate this, I think "r" works
                                        ; better than "n" for narrow-to-region

(defun md/narrow-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (edit-indirect-region (region-beginning)
                               (region-end)
                               t))
        (edit-indirect--overlay
         (edit-indirect-commit))
        (org-src-mode
         (org-edit-src-exit))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        ((derived-mode-p 'restclient-mode)
         (restclient-narrow-to-current))
        (t (narrow-to-defun))))

(defun md/narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

(setq md/splitscreen-path (md/dotfiles-get-path "splitscreen/"))

;; NOTE - for some reason this doesn't seem to load with "defer"
(use-package splitscreen
 :load-path md/splitscreen-path
 :demand t
 :ensure nil
 :config
 (progn
   (splitscreen-mode)
   (bind-key "C-w" splitscreen/mode-map edebug-mode-map)))

(use-package winner
  :demand t
  :config (winner-mode 1)
  :bind (:map splitscreen/prefix
              ("u" . winner-undo)
              ("U" . winner-redo)))

(use-package eyebrowse
  :demand t
  :config
  (progn
    (setq eyebrowse-wrap-around t
          eyebrowse-mode-line-separator " "
          eyebrowse-mode-line-left-delimiter ""
          eyebrowse-mode-line-right-delimiter ""
          eyebrowse-mode-line-style t
          eyebrowse-new-workspace t)
    (eyebrowse-mode 1))
  :bind (:map splitscreen/prefix
              ("!" . eyebrowse-switch-to-window-config)))

(defun md/use-display-buffer-alist (fn &rest args)
  "Wrap a function that displays a buffer. Save window excursion, and
  re-display the new buffer using `display-buffer`, which allows Shackle to
  detect and process it. "
  (let ((buffer-to-display nil)
        (res nil))
    (save-window-excursion
      (setq res (apply fn args))
      (setq buffer-to-display (current-buffer)))
    (display-buffer buffer-to-display)
    res))

(use-package shackle
  :load-path "non-elpa/shackle"  ; fork
  :demand t
  :config
  (progn

    (defun md/shackle-down ()
      (interactive)
      (delete-window shackle-last-window))

    (defun md/shackle-up ()
      (interactive)
      (if shackle-last-buffer
          (display-buffer shackle-last-buffer)
        (message "No previous shackle buffer found")))

    ;; TODO - check len is > 1
    (defun md/shackle-toggle ()
      (interactive)
      (if (and (window-live-p shackle-last-window) (> (length (window-list)) 1))
          (md/shackle-down)
        (md/shackle-up)))

    (defmacro md/shackle-advise (fn)
      "Add advise to given function to wrap with md/shackle-wrapper."
      `(advice-add ,fn :around 'md/use-display-buffer-alist
                   '((name . "md/shackle"))))

    (defmacro md/shackle-unadvise (fn)
      `(advice-remove ,fn 'md/use-display-buffer-alist))

    ;; Add advice for functions that display a new buffer but usually escape
    ;; Shackle (eg. due to not calling display-buffer).
    (md/shackle-advise 'helpful-function)
    (md/shackle-advise 'helpful-command)
    (md/shackle-advise 'helpful-macro)
    (md/shackle-advise 'helpful-variable)
    (md/shackle-advise 'helpful-callable)
    (md/shackle-advise 'helpful-command)
    (md/shackle-advise 'helpful-at-point)
    (md/shackle-advise 'ansi-term)
    (md/shackle-advise 'term)
    (md/shackle-advise 'eshell)
    (md/shackle-advise 'shell)
    (md/shackle-advise 'dired)
    (md/shackle-advise 'dired-jump)
    (md/shackle-advise 'dired-single-buffer)
    (md/shackle-advise 'projectile-run-term)
    (md/shackle-advise 'undo-tree-visualize)
    (md/shackle-advise 'run-scheme)
    (md/shackle-advise 'mu4e~main-view)
    (md/shackle-advise 'mu4e-compose)
    (md/shackle-advise 'mu4e-headers-search)
    (md/shackle-advise 'magit-dispatch-popup)
    (md/shackle-advise 'magit-display-buffer)
    ;;(md/shackle-advise 'edebug-pop-to-buffer)
    ;; (md/shackle-unadvise 'edebug-debugger)
    ;; (md/shackle-unadvise 'edebug)
    ;; (md/shackle-unadvise 'edebug-enter)

    (defun md/mu4e-eyebrowse-quit (fn &rest args)
      (apply fn args)
      (shackle--eyebrowse-close-slot-by-tag "mail"))
    (advice-add 'mu4e-quit :around 'md/mu4e-eyebrowse-quit '((name . "md/eyebrowse")))

    (defun md/is-edebug? (buffer)
      (interactive)
      (message (format "%s" edebug-mode)))

    ;; TODO ediff
    (setq shackle-rules
          `(("\\`\\*helm.*?\\*\\'" :regexp t :align t :close-on-realign t :size 15 :select t)
            ("\\`\\*help.*?\\*\\'" :regexp t :align t :close-on-realign t :size 0.33 :select t)
            ('helpful-mode :align t :close-on-realign t :size 0.33 :select t)
            ("\\`\\*Flycheck.*?\\*\\'" :regexp t :align t :close-on-realign t :size 12 :select nil)
            ("\\`\\*Shell Command Output.*?\\*\\'" :regexp t :align t :close-on-realign t :size 12 :select nil)
            ("\\`\\*Async Shell Command.*?\\*\\'" :regexp t :align t :close-on-realign t :size 12 :select nil)
            ('undo-tree-visualizer-mode :align right :close-on-realign t :size 30 :select t)
            ("\\`\\*Directory.*?\\*\\'" :regexp t :align t :close-on-realign t :size 12 :select t)
            ("\\`\\*vc-change-log.*?\\*\\'" :regexp t :align t :close-on-realign t :size 0.33 :select nil)

            ;; TODO
            ;;('(:custom md/is-edebug?) :eyebrowse "debug" :align t :close-on-realign nil :only t :select t)
            ;;("\\`\\*edebug-trace.*?\\*\\'" :eyebrowse "debug" :regexp t :align right :size 0.5 :select nil)

            ("\\`\\*HTTP Response.*?\\*\\'" :regexp t :align t :close-on-realign t :size 20 :select nil)
            ("*Anaconda*" :eyebrowse "anaconda" :align left :close-on-realign t :size 0.5 :select t)
            ("\\*Agenda Commands\\*" :regexp t :eyebrowse "agenda" :align t :close-on-realign t :size 20 :select t)

            ('ansi-term-mode :align t :close-on-realign t :size 0.4 :select t)
            ('occur-mode :align t :close-on-realign t :size 0.4 :select nil)
            ('grep-mode :eyebrowse "grep" :align left :close-on-realign t :size 0.5 :select t)
            ('ag-mode :eyebrowse "grep" :align left :close-on-realign t :size 0.5 :select t)
            ('term-mode :align t :close-on-realign t :size 0.4 :select t)
            ('shell-mode :align t :close-on-realign t :size 0.4 :select t)
            ('eshell-mode :align t :close-on-realign t :size 0.4 :select t)

            ('magit-status-mode :eyebrowse "git" :align t :select t :size 0.33 :only t)
            ('magit-popup-mode :align t :select t :size 0.33 :close-on-realign t)
            ('magit-diff-mode :eyebrowse "git" :select t :align left :size 0.5 :only t)
            ('magit-log-mode :eyebrowse "git" :select t :align t :size 0.4 :only t)
            ('magit-revision-mode :eyebrowse "git" :select t :align t :size 0.5 :close-on-realign t)

            ("\\`\\*edit-indirect .*?\\*\\'" :regexp t :select t :same t)
            ('completion-list-mode :align t :close-on-realign t :size 0.33 :select t)
            ('compilation-mode :align t :close-on-realign t :size 0.33 :select t)
            ('inferior-scheme-mode :align t :close-on-realign t :size 0.33 :select t)
            ("*Warnings*" :align t :close-on-realign t :size 0.33 :select nil)
            ("*Messages*" :align t :close-on-realign t :size 0.33 :select nil)
            (".*emacs-scratch.*" :regexp t :align t :close-on-realign t :size 30 :select t) ;; TODO regex
            (".*init.org" :regexp t :same t :select t)
            (,neo-buffer-name :align left :close-on-realign t :size 25 :select t)
            (,mu4e~main-buffer-name :eyebrowse "mail" :size 40 :select t :align left :close-on-realign t)
            (,mu4e~headers-buffer-name :eyebrowse "mail" :select t :other t)
            ('mu4e-compose-mode :eyebrowse "mail" :select t :other t)
            (dired-mode :align nil :select t)))

    (defmacro shackle-with-temp (rules body)
      "Execute body with temporary shackle rules"
      `(let ((shackle-rules (append ,rules shackle-rules)))
         ,body))

    ;; This is deprecated but works for now - need to figure out how to implement it
    ;; with display-buffer-alist. It prevents us from opening a new frame every
    ;; time I use mu4e.
    (setq-default display-buffer-reuse-frames t)

    ;; Ensure Helm doesn't interfere with Shackle buffers too much.
    ;; - Use Shackle to control and position Helm buffers.
    ;; - But ensure that shackle-last-buffer is not set to any Helm buffer.
    ;;
    ;; TODO - ideally Helm would cleanup buffers immediately to avoid
    ;; interfering with other buffer display commands.
    (setq helm-display-function 'pop-to-buffer) ; make sure helm popups are detected.
    (defvar md/shackle-buffer-store nil)
    (defvar md/shackle-window-store nil)
    (defun md/helm-shackle-setup ()
      (setq md/shackle-buffer-store shackle-last-buffer
            md/shackle-window-store shackle-last-window))
    (defun md/helm-shackle-teardown ()
      (when md/shackle-buffer-store
        (setq shackle-last-buffer md/shackle-buffer-store
              shackle-last-window md/shackle-window-store)))
    (add-hook 'helm-before-initialize-hook 'md/helm-shackle-setup)
    (add-hook 'helm-cleanup-hook 'md/helm-shackle-teardown)

    ;; NOTE: In order to get Shackle working with some org-mode commands, we
    ;; have to override an internal function. Org is quite opinionated about
    ;; window display, and usually this function will unset various buffer
    ;; display variables before calling switch-to-buffer-other-window.
    ;; I'd rather just control those buffers with Shackle.
    (require 'org)
    (fmakunbound 'org-switch-to-buffer-other-window)
    (defun org-switch-to-buffer-other-window (&rest args)
      (apply 'switch-to-buffer-other-window args))
    (setq org-agenda-window-setup 'only-window)

    (defun md/shackle-match ()
      (interactive)
      (message (format "%s" (shackle-match (current-buffer)))))

    (shackle-mode 1))

  :bind (:map md/leader-map
              ("; ;" . display-buffer)  ;; Uses display-buffer-alist, so Shackle rules will apply.
              (";a" . md/shackle-toggle)))

;; Copyright (C) 2000 Eric Crampton <eric@atdesk.com>
;; https://github.com/emacsorphanage/dedicated
;; GPL v2.

(defvar dedicated-mode nil
  "Mode variable for dedicated minor mode.")
(make-variable-buffer-local 'dedicated-mode)

(defun dedicated-mode (&optional arg)
  "Dedicated minor mode."
  (interactive "P")
  (setq dedicated-mode (not dedicated-mode))
  (set-window-dedicated-p (selected-window) dedicated-mode)
  (if (not (assq 'dedicated-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(dedicated-mode " D")
                  minor-mode-alist))))

(bind-key "bd" 'dedicated-mode md/leader-map)
(bind-key "tD" 'dedicated-mode md/leader-map)

(defun md/bookmark-names-matching-tags (tags)
  "Return bmkp bookmark names that match the given list of tags."
  (-map (lambda (bookmark)
          (bookmark-name-from-record bookmark))
        (bmkp-some-tags-alist-only tags)))

(defun md/bookmark-set (name)
  "Set a bookmark and provide some auto tags"
  (interactive)
  (bookmark-set name)
  (message "Added bookmark: %s" name)
  (when (projectile-project-p)
    (bmkp-add-tags (bookmark-get-bookmark name)
                   (list
                    (format "project-%s" (projectile-project-name))))))

(defvar md/bookmark-last-temp nil)
(defun md/bookmark-temp-set ()
  "Set bookmark with automatic name and tag it as temporary"
  (interactive)
  (let ((bmk-name (s-truncate 50
                              (s-trim
                               (substring-no-properties (thing-at-point 'line))))))
    (when (string= "" (s-trim bmk-name))
      (setq bmk-name "[blank-line]"))
    (md/bookmark-set bmk-name)
    (setq md/bookmark-last-temp (bookmark-get-bookmark bmk-name))
    (bmkp-add-tags (bookmark-get-bookmark bmk-name) '("tmp"))))

(defun md/bookmark-jump-to-last-temp ()
  (interactive)
  (if md/bookmark-last-temp
      (bookmark-jump md/bookmark-last-temp)
    (message "No temp bookmark to jump to")))

(defun md/bookmark-temp-delete-all ()
  "Delete all temp-tagged bookmarks"
  (interactive)
  (-each (bmkp-some-tags-alist-only '("tmp"))
    (lambda (bookmark)
      (bookmark-delete bookmark)))
  (message "Deleted temp bookmarks"))

(defun md/helm-bookmark-action-delete (candidate)
  "Delete the helm candidate."
  (bookmark-delete (bookmark-get-bookmark candidate))
  (message "Deleted bookmark: %s" candidate))

(defun md/helm-bookmark-action-tag (candidate)
  "Prompt to tag the helm candidate."
  (let ((bookmark (bookmark-get-bookmark candidate)))
    (bmkp-add-tags bookmark (list (completing-read "Tag: " (bmkp-tags-list t nil))))))

(defun md/helm-bookmark-action-edit (candidate)
  "Edit the full file for the helm candidate."
  (bmkp-edit-bookmark-record (bookmark-get-bookmark candidate)))

(defface md/helm-bookmarks-bookmark '((t (:inherit default))) "")
(defface md/helm-bookmarks-file '((t (:inherit default))) "")
(defface md/helm-bookmarks-tags '((t (:inherit org-tag))) "")
(defface md/helm-bookmarks-separator '((t (:inherit org-tag))) "")
(defun md/helm-bookmark-transform (candidates)
  "Transform Helm source to provide display values with filename and tags.
Adapted from helm-bookmark-transformer."
  (-map (lambda (real)
          (let* ((bookmark (bookmark-get-bookmark real))
                 (loc (bookmark-location bookmark))
                 (trunc-loc (s-pad-right 35 " "
                                            (s-truncate 30
                                                        (if (listp loc)
                                                            (car loc)
                                                          loc))))

                 (trunc-name (s-pad-right 45 " "
                                          (s-truncate 40 real)))

                 ;; (trunc (if (> len bookmark-bmenu-file-column)
                 ;;            (helm-substring real bookmark-bmenu-file-column)
                 ;;          real))
                 ;; (sep (make-string (- (+ bookmark-bmenu-file-column 2)
                 ;;                      (length trunc))
                 ;;                   ? ))
                 (tags (s-join " "
                               (-map (lambda (text)
                                       (propertize text 'face
                                                   'md/helm-bookmarks-tags))
                                     (bmkp-get-tags bookmark))))
                 (display
                  (concat (propertize trunc-name 'face 'md/helm-bookmarks-bookmark)
                          (propertize trunc-loc 'face 'md/helm-bookmarks-file)
                          tags)))
            (cons display real)))
        candidates))

(defmacro md/helm-source-build-bookmarks (title bookmark-fetcher)
  "Return a helm bookmark source that fetches candidates using the provided body"
 `(helm-build-sync-source ,title
   :candidates (md/helm-bookmark-transform ,bookmark-fetcher)
   :action '(("Jump" . helm-bookmark-jump)
             ("Tag" . md/helm-bookmark-action-tag)
             ("Edit" . md/helm-bookmark-action-edit)
             ("Delete" . md/helm-bookmark-action-delete))))


(defmacro md/helm-source-build-bookmark-set (mark-as-temp-p)
  "Return a helm bookmarks source that is like helm-source-bookmark set, but
uses md/bookmark-set and optionally marks the bookmark as temporary."
  `(helm-build-dummy-source "Set Bookmark"
    :filtered-candidate-transformer
    (lambda (_candidates _source)
      (list (or (and (not (string= helm-pattern ""))
                     helm-pattern)
                "Enter a bookmark name to record")))
    :action '(("Set bookmark" . (lambda (candidate)
                                  (if (string= helm-pattern "")
                                      (message "No bookmark name given for record")
                                    (md/bookmark-set candidate)
                                    ,(when mark-as-temp-p
                                       '(bmkp-add-tags (bookmark-get-bookmark
                                                        candidate)
                                                       '("tmp")))))))))

(defun md/helm-bookmarks ()
  "Main Helm bookmarks command."
  (interactive)
  (helm :sources
        (list (md/helm-source-build-bookmarks
               "Bookmarks"
               (-difference
                (bookmark-all-names)
                (md/bookmark-names-matching-tags '("tmp"))))
              (md/helm-source-build-bookmarks
               "Temp bookmarks" (md/bookmark-names-matching-tags '("tmp")))
              (md/helm-source-build-bookmark-set nil))
        :buffer "*helm md/helm-bookmarks*"))

(defun md/helm-bookmarks-project ()
  "Like md/helm-bookmarks, but for the current projectile project."
  (interactive)
  (helm :sources
        (list (md/helm-source-build-bookmarks
               (format "Bookmarks for project: %s" (projectile-project-name))
               (md/bookmark-names-matching-tags
                (list (format "project-%s" (projectile-project-name)))))
              (md/helm-source-build-bookmark-set nil))
        :buffer "*helm md/helm-bookmarks-project*"))

(defun md/helm-bookmarks-temp ()
  "Like md/helm-bookmarks, but for the temp-tagged bookmarks."
  (interactive)
  (helm :sources
        (list (md/helm-source-build-bookmarks "Temp bookmarks"
               (md/bookmark-names-matching-tags '("tmp")))
              (md/helm-source-build-bookmark-set t))
        :buffer "*helm md/helm-bookmarks-temp*"))

(use-package bookmark
  :config
  (setq helm-bookmark-show-location t
        bookmark-bmenu-file-column 60
        bookmark-automatically-show-annotations nil
        bookmark-completion-ignore-case t))

(use-package bookmark+
    :demand t
    :bind (:map md/leader-map
                ("ll" . md/helm-bookmarks)
                ("jl" . md/helm-bookmarks-project)
                ("lf" . md/helm-bookmarks-temp)
                ("ls" . md/bookmark-temp-set)
                ("lj" . md/bookmark-jump-to-last-temp)
                ("ld" . md/bookmark-temp-delete-all)))

(defvar md/anchored-buffer nil "Current anchored buffer")
(defvar md/anchored-return-buffer nil "Current return buffer")
(defun md/anchor-toggle ()
  (interactive)
  (if (eq (current-buffer) md/anchored-buffer)
      (if md/anchored-return-buffer
          (switch-to-buffer md/anchored-return-buffer)
        (message "no return buffer!"))
    (if md/anchored-buffer
        (progn
          (setq md/anchored-return-buffer (current-buffer))
          (switch-to-buffer md/anchored-buffer))
      (message "no anchor buffer!"))))
(defun md/anchor-here ()
  (interactive)
  (setq md/anchored-buffer (current-buffer))
  (message (format "Anchored to %s" (current-buffer))))
(bind-key "la" 'md/anchor-toggle md/leader-map)
(bind-key "lA" 'md/anchor-here md/leader-map)

(defconst md/scratch-file-elisp "~/.emacs-scratch.el")
(defun md/scratch-open-file-elisp ()
  (interactive)
  (find-file md/scratch-file-elisp))

(defconst md/scratch-file-python "~/.emacs-scratch.py")
(defun md/scratch-open-file-python ()
  (interactive)
  (find-file md/scratch-file-python))

(defconst md/scratch-file-restclient "~/.emacs-scratch.http")
(defun md/scratch-open-file-restclient ()
  (interactive)
  (find-file md/scratch-file-restclient))

(defconst md/scratch-file-markdown "~/.emacs-scratch.md")
(defun md/scratch-open-file-markdown ()
  (interactive)
  (find-file md/scratch-file-markdown))

(defconst md/scratch-file-gfm "~/.emacs-scratch.gfm")
(defun md/scratch-open-file-gfm ()
  (interactive)
  (find-file md/scratch-file-gfm))

(defconst md/scratch-file-org "~/.emacs-scratch.org")
(defun md/scratch-open-file-org ()
  (interactive)
  (find-file md/scratch-file-org))

;; Open this scratch buffer on startup
(setq initial-buffer-choice md/scratch-file-org)

(bind-key "'s" 'md/scratch-open-file-elisp md/leader-map)
(bind-key "'e" 'md/scratch-open-file-elisp md/leader-map)
(bind-key "'p" 'md/scratch-open-file-python md/leader-map)
(bind-key "'r" 'md/scratch-open-file-restclient md/leader-map)
(bind-key "'m" 'md/scratch-open-file-markdown md/leader-map)
(bind-key "'g" 'md/scratch-open-file-gfm md/leader-map)
(bind-key "'o" 'md/scratch-open-file-org md/leader-map)

(line-number-mode 1)
(column-number-mode 1)

(use-package powerline
 :defer 1
 :config

 (progn
   (defadvice vc-mode-line (after strip-backend () activate)
     "Strip the Git- prefix from the modeline to just display branch name"
     (when (stringp vc-mode)
       (let ((gitlogo (replace-regexp-in-string "Git." "" vc-mode)))
         (setq vc-mode gitlogo))))

   (defface md/powerline-inactive '((t (:inherit 'modeline))) "")
   (defface md/powerline-normal '((t (:inherit 'modeline))) "")
   (defface md/powerline-insert '((t (:inherit 'modeline))) "")
   (defface md/powerline-visual '((t (:inherit 'modeline))) "")
   (defface md/powerline-replace '((t (:inherit 'modeline))) "")
   (defface md/powerline-emacs '((t (:inherit 'modeline))) "")
   (defun md/powerline-setup ()
     (interactive)
     (require 'flycheck)
     (setq mode-line-format
                   '("%e"
                     (:eval
                      (let* ((active (powerline-selected-window-active))
                             (mode-line (if active 'mode-line 'mode-line-inactive))
                             (face1 (if active 'powerline-active1 'powerline-inactive1))
                             (face2 (if active 'powerline-active2 'powerline-inactive2))

                             ;; Set face3 depending on Evil state
                             (face3 (if active
                                        (cond ((evil-normal-state-p) 'md/powerline-normal)
                                              ((evil-insert-state-p) 'md/powerline-insert)
                                              ((evil-visual-state-p) 'md/powerline-visual)
                                              ((evil-replace-state-p) 'md/powerline-replace)
                                              ((evil-emacs-state-p) 'md/powerline-emacs)
                                              (t 'md/powerline-normal))
                                      'md/powerline-inactive))
                             (separator-left (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (car powerline-default-separator-dir))))
                             (separator-right (intern (format "powerline-%s-%s"
                                                              (powerline-current-separator)
                                                              (cdr powerline-default-separator-dir))))

                             (lhs (list
                                   (when eyebrowse-mode
                                     (powerline-raw
                                      (let* ((window-configs (eyebrowse--get 'window-configs))
                                             (current-config (assoc (eyebrowse--get 'current-slot) window-configs))
                                             (current-index (car current-config))
                                             (current-tag (nth 2 current-config)))
                                        (format "%s:%s" current-index current-tag)) face3 'l ))

                                   ;; Line / column numbers
                                   (when (or line-number-mode column-number-mode)
                                     (cond ((and line-number-mode
                                                 column-number-mode)
                                            (powerline-raw "%4l:%2c " face3 'l))
                                           (line-number-mode
                                            (powerline-raw "%4l" face3 'l))
                                           (column-number-mode
                                            (powerline-raw ":%2c " face3 'l))))

                                   ;; Dedicated mode indicator
                                   (when dedicated-mode
                                     (powerline-raw (format "Ded.") face3 'l))

                                   ;; Evil status
                                   (powerline-raw evil-mode-line-tag face3 'l)

                                   ;; Major mode
                                   (funcall separator-left face3 face1)
                                   (powerline-raw (format "%s " (powerline-major-mode)) face1 'l)

                                   ;; Modeline
                                   (funcall separator-left face1 mode-line)
                                   (powerline-raw "%b" mode-line 'l)

                                   ;; Projectile
                                   ;; (when (and (boundp 'projectile-mode) projectile-mode)
                                   ;;   (powerline-raw (format "%s" (projectile-project-name)) face2 'l))

                                   ;; File state
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
                                      face1 'l))))

                             (rhs (list
                                   ;; Git
                                   (funcall separator-right mode-line mode-line)
                                   (or (powerline-vc mode-line 'r)
                                       (powerline-raw "-" mode-line 'r))

                                   ;; Projectile
                                   (funcall separator-right mode-line face1)
                                   (if (and (boundp 'projectile-mode) projectile-mode)
                                     (powerline-raw (format " %s " (projectile-project-name)) face1 'r)
                                     (powerline-raw "-" face1 'r))

                                   ;; Flycheck
                                   (cond ((and active flycheck-mode (flycheck-has-current-errors-p
                                                                     'error))
                                          (funcall separator-right face1 'md/modeline-flycheck-error)
                                          (powerline-raw " E " 'md/modeline-flycheck-error 'r))
                                         ((and active flycheck-mode (flycheck-has-current-errors-p
                                                                     'warning))
                                          (funcall separator-right face1 'md/modeline-flycheck-warning)
                                          (powerline-raw " W " 'md/modeline-flycheck-warning 'r))
                                         (t
                                          (funcall separator-right face1 face3)
                                          (powerline-raw " - " 'face3 'r)))
                                   )))
                        (concat (powerline-render lhs)
                                (powerline-fill mode-line (powerline-width rhs))
                                (powerline-render rhs))))))
                   (setq-default mode-line-format mode-line-format))

   (defun md/powerline-reset ()
     (interactive)
     (save-window-excursion
       (mapc (lambda (buffer)
               (switch-to-buffer buffer)
               (md/powerline-setup)
               (powerline-reset))
             (buffer-list))))

   (md/powerline-reset)))

(require 'artist)
(require 'org)
(require 'ob)

(defun diagram/draw-rectangle-small ()
  (interactive)
  (artist-draw-rect (current-column) (- (line-number-at-pos) 1)
                    (+ 45 (current-column)) (- (+ 7 (line-number-at-pos)) 1)))

(defun diagram/mouse-draw-rectangle-small ()
  (interactive)
  ;; TODO: move cursor to the clicked location.
  (diagram/draw-rectangle-small))

(defun diagram/draw-space-grid ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (move-end-of-line nil)
      (while (< (current-column) 200)
        (insert " "))
      (forward-line 1))))

(defvar diagram-mode-font-lock-keywords
  `((,(regexp-opt '("TODO" "FIX" "??")) . 'org-warning)
    (,(regexp-opt '("NOT" "BUT" "EXCEPT") 'words) . 'font-lock-builtin-face)
    (,(regexp-opt '(" [ ] ")) . 'org-checkbox)
    (,(regexp-opt '(" [X] ")) . 'font-lock-comment-face)
    (,(regexp-opt '(" * ")) . 'outline-1)
    (,(regexp-opt '(" ** ")) . 'outline-2)
    (,(regexp-opt '(" *** ")) . 'outline-3)
    (,(regexp-opt '(" **** ")) . 'outline-4)
    (,(regexp-opt '(" ***** ")) . 'outline-5)
    (,(regexp-opt '(" ***** ")) . 'outline-6)
    (org-do-emphasis-faces)
    (org-activate-dates
     (0 'org-date t))
    ))
(font-lock-add-keywords 'diagram-mode diagram-mode-font-lock-keywords t)

(defvar diagram-mode-map (make-sparse-keymap))
(set-keymap-parent diagram-mode-map artist-mode-map)

;; This does something else in artist-mode.
(evil-define-key 'emacs diagram-mode-map (kbd "<RET>") 'evil-ret)
(evil-define-key 'normal diagram-mode-map (kbd "<RET>") 'evil-ret)
(evil-define-key 'insert diagram-mode-map (kbd "<RET>") 'evil-ret)
(evil-define-key 'visual diagram-mode-map (kbd "<RET>") 'evil-ret)

(evil-define-key 'insert diagram-mode-map ">" 'self-insert-command)
(evil-define-key 'insert diagram-mode-map "<" 'self-insert-command)

(evil-define-key 'emacs diagram-mode-map (kbd "<down-mouse-1>") 'artist-down-mouse-1)
(evil-define-key 'emacs diagram-mode-map (kbd "<S-down-mouse-1>") 'diagram/mouse-draw-rectangle-small)
(evil-define-key 'emacs diagram-mode-map (kbd "<down-mouse-3>") 'artist-mouse-choose-operation)
(evil-define-key 'emacs diagram-mode-map (kbd "<S-down-mouse-3>") 'artist-mouse-choose-operation)
(evil-define-key 'emacs diagram-mode-map "u" 'undo-tree-undo)
(evil-define-key 'emacs diagram-mode-map "l" 'artist-select-op-line)
(evil-define-key 'emacs diagram-mode-map "o" 'artist-select-op-circle)
(evil-define-key 'emacs diagram-mode-map "r" 'artist-select-op-rectangle)
(evil-define-key 'emacs diagram-mode-map "R" 'diagram/draw-rectangle-small)
(evil-define-key 'emacs diagram-mode-map "S" 'diagram/draw-space-grid)
(evil-define-key 'emacs diagram-mode-map "e" 'artist-select-op-erase-rectangle)
(evil-define-key 'emacs diagram-mode-map "p" 'artist-select-op-pen-line)

(define-derived-mode diagram-mode artist-mode "Diagram" diagram-mode-map
  (evil-normal-state)
  (toggle-truncate-lines 1)
  (artist-select-op-line)
  (font-lock-fontify-buffer))

(add-to-list 'auto-mode-alist '("\\.diagram\\'" . diagram-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((diagram . t)))

(bind-key (kbd "\\") 'diagram-mode md/leader-map)

(use-package color-theme-solarized
 :demand t
 :ensure nil
 :load-path "non-elpa/color-theme-solarized"
 :config
 (progn
   (add-to-list 'custom-theme-load-path "non-elpa/color-theme-solarized")

   ;; Necessary on v24.4 to display accurate Solarized colors, due to Emacs bug
   ;; #8402. v24.3 didn't set ns-use-sgrb-colorspace.
   (setq ns-use-srgb-colorspace nil
         solarized-broken-srgb t)

   ;; See heading on terminal colour fixes near top of file
   (when (not (display-graphic-p))
     (setq solarized-bold nil)))

 :bind (:map md/leader-map
        ("ts" . solarized-toggle-theme-mode)
        ("cs" . solarized-toggle-comment-visibility)))

(use-package gruvbox
  :demand t
  :load-path "non-elpa/emacs-theme-gruvbox"
  :config
  (progn
    (add-to-list 'custom-theme-load-path (md/dotfiles-get-path "emacs.d.symlink/non-elpa/emacs-theme-gruvbox"))))

(defun md/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun md/load-theme ()
  (interactive)
  (md/disable-all-themes)
  (setq org-todo-keyword-faces nil)
  (call-interactively 'load-theme)
  (face-spec-set 'git-gutter:unchanged
                 `((t :inherit 'default
                      :background ,(face-attribute 'default :background))))
  (face-spec-set 'git-gutter:separator
                 `((t :inherit 'default
                      :background ,(face-attribute 'default :background))))
  (md/powerline-reset))

;; Initial setup
(md/disable-all-themes)
(if (display-graphic-p)
    (load-theme 'gruvbox-dark-medium t)
  (load-theme 'solarized t)
  (solarized-enable-theme 'dark))

(bind-key "tt" 'md/load-theme md/leader-map)

(use-package writeroom-mode
 :defer 1
 :demand t
 :config
 (progn
   (defun md/writeroom-mode ()
     "Handle clash with git-gutter mode as both use fringes"
     ;; TODO: why doesn't this work using writeroom-mode-hook?
     (interactive)
     (message (format "%s" writeroom-mode))
     (if (not writeroom-mode)
         (progn
           (git-gutter-mode 0)
           (redraw-frame)
           (writeroom-mode 1)
           (redraw-frame))
       (writeroom-mode 0)))

   (setq writeroom-width 90
         writeroom-mode-line t  ; Keep modeline
         writeroom-maximize-window nil  ; Don't delete other windows
         writeroom-fullscreen-effect 'fullboth
         writeroom-fringes-outside-margins nil
         writeroom-major-modes nil
         writeroom-restore-window-config nil))
 :bind (:map md/leader-map
        ("tW" . md/writeroom-mode)))

(defun md/dotfiles-edit-init ()
  (interactive)
  (find-file (md/dotfiles-get-path "emacs.d.symlink/init.org")))

(bind-key "ve" 'md/dotfiles-edit-init md/leader-map)
(bind-key "vc" 'md/dotfiles-compile md/leader-map)

(defconst md/dotfiles-init-local-path "~/.local.el")

(when (file-exists-p md/dotfiles-init-local-path)
      (load-file md/dotfiles-init-local-path))

(defun md/dotfiles-edit-init-local ()
  (interactive)
  (find-file md/dotfiles-init-local-path))

(bind-key "vl" 'md/dotfiles-edit-init-local md/leader-map)

(defun md/set-ssh-agent-from-mac-keychain ()
  "[2018-08-18] For El Capitan setup: use keychain to setup the SSH agent,
This is the same as the keychain setup used for new shell logins."
  (interactive)
  (let* ((keychain-eval-output
          (s-trim
           (shell-command-to-string
            "if [ $(command -v keychain) ]; then keychain --quiet --eval --agents 'ssh' --inherit 'local-once'; fi")))
         (sock-val (nth 1 (s-match "SSH_AUTH_SOCK=\\(?1:.*\\); " keychain-eval-output)))
         (pid-val (nth 1 (s-match "SSH_AGENT_PID=\\(?1:.*\\); " keychain-eval-output))))
    (setenv "SSH_AUTH_SOCK" sock-val)
    (setenv "SSH_AGENT_PID" pid-val)))

(md/set-ssh-agent-from-mac-keychain)

(use-package esup
  :defer 5)

(require 'server)
(when (not (server-running-p))
   (server-start))

(defconst md/emacs-init-end (current-time))

(defconst md/emacs-boot-time (float-time (time-subtract md/emacs-init-end md/emacs-init-start)))
(message (format "md/emacs-boot-time: %s" md/emacs-boot-time))

)
