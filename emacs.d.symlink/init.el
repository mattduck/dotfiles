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

(let ((file-handler-name-alist nil)
       (gc-cons-threshold 100000000))

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

(setq use-package-always-ensure nil
      use-package-verbose t
      use-package-minimum-reported-time 0.01)

(eval-when-compile
    (require 'use-package))

(require 'bind-key)  ; Required for :bind in use-package

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

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun md/fontify-if-font-lock-mode ()
  (when font-lock-mode
    (font-lock-fontify-buffer)))

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

(defun md/set-default-font ()
  (interactive)
  (if (string= (system-name) "mattmbp.local")
      (set-frame-font "Monaco-12:antialias=subpixel")
    (set-frame-font "Monaco-13:antialias=subpixel")))

(md/set-default-font)

(add-hook 'focus-in-hook 'md/set-default-font)

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
  :if (not (display-graphic-p))
  :defer 1
  :config
  (progn
    (turn-on-xclip)))

(setq message-log-max 10000)

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

(defun md/strip-whitespace-and-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(defun md/fontify-buffer ()
  "Fontify the buffer and tell me it happened."
  (interactive)
  (font-lock-fontify-buffer)
  (message "Fontified buffer"))

(defun md/file-info ()
  (interactive)
  (message
   "%s | %s lines | %3d%% | %s"
           (buffer-file-name)
           (count-lines (point-min) (point-max))
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

(setq debug-on-error nil)
(setq delete-by-moving-to-trash t)

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

   (setq evil-jumps-max-length 20)  ; Lower than the default, but I rarely want more

   ;; This uses C-i by default (as in vim), but C-i is interpeted as TAB, which
   ;; is an important binding in org-mode. Use C-l instead, which is bound to
   ;; recenter-top-bottom by default.
   (bind-key "C-l" 'evil-jump-forward evil-normal-state-map)
   (bind-key "C-l" 'evil-jump-forward evil-visual-state-map)

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

   (bind-key "h" help-map md/leader-map)  ; I prefer <leader>h to C-h
   (bind-key "n" (lookup-key global-map (kbd "C-x n")) md/leader-map)

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

        ;; The equivalent of gj/gk
        :map evil-normal-state-map
        ("j" . evil-next-visual-line)
        ("k" . evil-previous-visual-line)

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
        ("bk" . kill-buffer)
        ("bi" . md/file-info)
        ("bw" . save-buffer)
        ("bW" . md/strip-whitespace-and-save)
        ("br" . read-only-mode)

        ;; Eval
        ("ef" . eval-defun)
        ("ee" . eval-last-sexp)  ; Bound to e because I'm used to C-x e
        ("eb" . eval-buffer)
        ("er" . eval-region)
        ("ex" . md/fontify-buffer)  ; It's sort-of an eval

        ;; Emacs
        ("Ek" . kill-emacs)
        ("Es" . server-start)
        ("Ep" . list-processes)

        ;; Packages
        ("Pr" . package-refresh-contents)
        ("Pi" . package-install)
        ("Pl" . package-list-packages)

        ; Toggle misc
        ("tw" . toggle-truncate-lines)

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
    (setq helm-display-header-line nil)

    ;; I don't need to know about some files
    ;; TODO get this to workj
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

(defun md/which-key-patch ()
  "Override some which-key functions"
  (interactive)

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

            (setq which-key-idle-delay 0.6
                  which-key-max-description-length 30
                  which-key-allow-evil-operators nil
                  which-key-show-operator-state-maps nil
                  which-key-sort-order 'which-key-key-order-alpha
                  which-key-highlighted-command-list '("md/"))

            ;; Use ESC/C-g to quit which-key. Not sure why the default key is 'a'.
            (bind-key "ESC" 'which-key-abort which-key-C-h-map)
            (bind-key "C-g" 'which-key-abort which-key-C-h-map)

            ;; This is the default for description-replacement-alist:
            (setq which-key-description-replacement-alist
                  '(("Prefix Command" . "prefix")
                    ("which-key-show-next-page" . "wk next pg")
                    ("\\`\\?\\?\\'" . "lambda")))

            ;; Add scratch bindings:
            (dolist (mode '("elisp" "python" "restclient" "markdown" "gfm" "org"))
              (add-to-list 'which-key-description-replacement-alist
                           (cons (format "md/scratch-open-file-%s" mode) mode)))

            (which-key-declare-prefixes
              "SPC SPC" "major-mode"
              "SPC SPC e" "major-mode-eval"
              "SPC a" "ag"
              "SPC b" "buffers"
              "SPC B" "bookmarks"
              "SPC c" "comments"
              "SPC C" "compile"
              "SPC e" "eval"
              "SPC E" "Emacs"
              "SPC g" "git"
              "SPC h" "help"
              "SPC h k" "keys"
              "SPC j" "project"
              "SPC j ;" "project-popwin"
              "SPC j a" "project-ag"
              "SPC n" "narrow"
              "SPC P" "Packages"
              "SPC s" "flycheck"
              "SPC S" "flyspell"
              "SPC t" "toggle-misc"
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

(bind-key "k" 'describe-key md/keys-help-map)
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
    (evil-add-hjkl-bindings 'ag-mode-map
      (kbd "SPC") 'md/leader-map)

    (setq ag-context-lines nil
          ag-highlight-search t
          ag-reuse-buffers t  ; Only one buffer for ag searches§
          ag-reuse-window nil))  ; Open files in new window, don't hide search window

  :bind (:map md/leader-map
              ("ad" . ag-dired)
              ("af" . ag-files)
              ("ag" . ag)
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

    (setq flycheck-flake8rc ".config/flake8"
          flycheck-highlighting-mode 'symbols

          ;; defaults to 0.9, which is too slow
          flycheck-display-errors-delay 0.1

          ;; Disabling this at is annoys me to have errors appearing
          ;; and disappearing quickly and messing with the size of the
          ;; window. I will just check the error list and the fringe.
          flycheck-display-errors-function nil

          ;; There's a short delay when flycheck runs, which causes the modeline to change
          ;; its format (or in my custom powerline stuff, to disappear briefly). It's
          ;; super annoying if this happens at random points during editing, so change it
          ;; to only happen on save (and when enabling the mode). This is quite similar to how
          ;; I had it setup in vim.
          flycheck-check-syntax-automatically '(save mode-enabled)

          flycheck-mode-line-prefix nil)

    ;; For some reason in the flycheck mode list map it just uses all vi
    ;; keys. Mostly this is fine but I need an easy way to quit.
    (evil-define-key 'normal flycheck-error-list-mode-map "q" 'quit-window))
  :bind (:map md/leader-map
              ;; S prefix, ie. "syntax"
              ("s <RET>" . flycheck-mode)
              ("sl" . flycheck-list-errors)
              ("sn" . flycheck-next-error)
              ("sj" . flycheck-next-error)
              ("sp" . flycheck-previous-error)
              ("sk" . flycheck-previous-error)
              ("S <RET>" . flyspell-mode)
              ("SS" . flyspell-correct-word-before-point)))

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
   (projectile-global-mode))
 :bind (:map md/leader-map
       ("j!" . projectile-invalidate-cache)
       ("jk" . projectile-kill-buffers)))

(use-package helm-projectile
  :init (progn
          ;; This has to be set before loading helm-projectile
          (setq helm-projectile-fuzzy-match nil))
  :bind (:map md/leader-map
              ("jj" . helm-projectile-switch-project)
              ("jag" . projectile-ag)
              ("jaf" . ag-project-files)
              ("jad" . ag-project-dired)
              ("jb" . helm-projectile-switch-to-buffer)
              ("jp" . helm-projectile-switch-to-buffer)
              ("jf" . helm-projectile-find-file)))

(defun md/projectile-popwin-ansi-term ()
  "Open project-dedicated ansi-term buffer in popwin. Renames the term buffer to
match the project."
  (interactive)
  (when popwin:focus-window (popwin:close-popup-window))
  (popwin:display-buffer
   (or (get-buffer (format "*ansi-term-(%s)*" (projectile-project-name)))
        (save-window-excursion
          (with-current-buffer
            (call-interactively 'projectile-run-term)
            (rename-buffer
             (format "*ansi-term-(%s)*" (projectile-project-name))))))))

(defun md/projectile-popwin-shell ()
  "Open project-dedicated shell buffer in popwin. Renames the term buffer to
match the project."
  (interactive)
  (when popwin:focus-window (popwin:close-popup-window))
  (popwin:display-buffer
   (or (get-buffer (format "*shell-(%s)*" (projectile-project-name)))
        (save-window-excursion
          (with-current-buffer
            (call-interactively 'projectile-run-shell)
            (rename-buffer
             (format "*shell-(%s)*" (projectile-project-name))))))))

(defun md/projectile-popwin-eshell ()
  "Open project-dedicated eshell buffer in popwin. Renames the term buffer to
match the project."
  (interactive)
  (when popwin:focus-window (popwin:close-popup-window))
  (popwin:display-buffer
   (or (get-buffer (format "*eshell-(%s)*" (projectile-project-name)))
        (save-window-excursion
          (with-current-buffer
            (call-interactively 'projectile-run-eshell)
            (rename-buffer
             (format "*eshell-(%s)*" (projectile-project-name))))))))

(bind-key "j;t" 'md/projectile-popwin-ansi-term md/leader-map)
(bind-key "j;s" 'md/projectile-popwin-shell md/leader-map)
(bind-key "j;e" 'md/projectile-popwin-eshell md/leader-map)

(use-package git-commit
  :config
  (global-git-commit-mode t))

(use-package git-gutter
 :init
 (progn
   (defun md/set-sensible-column ()
     "Unless file is too big, either use git-gutter mode (when in
git dir) or linum mode"
     (interactive)
     (when (and (< (count-lines (point-min) (point-max)) 1500)
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
       ("gl" . magit-log)

       ;; Diff gives the full git diff output. Ediff shows ediff for a single
       ;; file.
       ("gd" . magit-diff-popup)
       ("gD" . magit-ediff-popup)

       ;; NOTE - this doesn't play nicely with mode-line:
       ;; - https://github.com/magit/magit/blob/master/Documentation/magit.org#the-mode-line-information-isnt-always-up-to-date
       ;; - https://github.com/syl20bnr/spacemacs/issues/2172
       ("gc" . magit-checkout)))

(use-package github-browse-file
  :config
  (progn
    (setq github-browse-file-show-line-at-point t))
  :bind (:map md/leader-map
        ("go" . github-browse-file)))

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
             ("d" . "ediff")))

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
         '("TODO" "FIX" "FIXME" "BUG" "WARN" "WARNING" "HACK" "NOTE" "ERROR" "MATT"))

   ;; By default this includes font-lock-string-face, but I don't want strings to
   ;; have these words formatted.
   (setq fic-activated-faces '(font-lock-doc-face font-lock-comment-face))))

(defun md/insert-todo-regexp ()
  (interactive)
  (insert "TODO|FIX|FIXME|BUG|WARN|HACK|ERROR"))

(use-package highlight-thing
  :defer 5
  :config
  (progn
    (setq highlight-thing-delay-seconds 0.2
          highlight-thing-case-sensitive-p t)
    (add-hook 'prog-mode-hook 'highlight-thing-mode))
  :bind (:map md/leader-map
              ("tt" . highlight-thing-mode)))

(use-package paren
 :defer 1
 :init (progn
        (add-hook 'prog-mode-hook 'show-paren-mode))
 :config
 (progn
   (setq show-paren-style 'parenthesis
         blink-matching-paren nil
         blink-matching-paren-on-screen nil)))

(use-package rainbow-mode
  :defer 1
  :config
  (progn
     (add-hook 'css-mode-hook 'rainbow-mode)
     (add-hook 'help-mode-hook 'rainbow-mode)
     (add-hook 'html-mode-hook 'rainbow-mode))
  :bind (:map md/leader-map
              ("tr" . rainbow-mode)))

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
    (setq dired-mode-map (make-sparse-keymap))

    (defun md/dired-single-buffer ()
      "If in popwin buffer, open dired in popwin. Otherwise as usual."
      (interactive)
      (if popwin:focus-window
          (progn
            (save-window-excursion (call-interactively 'dired-single-buffer))
            (popwin:close-popup-window)
            (popwin:display-buffer (get-buffer dired-single-magic-buffer-name)))
        (dired-single-buffer)))

    (evil-define-key 'normal dired-mode-map
      "W" 'wdired-change-to-wdired-mode  ; This is v useful
      "q" 'quit-window
      "d" 'dired-flag-file-deletion
      "u" 'dired-unmark
      "D" 'dired-do-delete
      (kbd "RET") 'md/dired-single-buffer
      "J" 'dired-jump
      "o" 'dired-find-file-other-window
      "R" 'dired-do-rename
      "C" 'dired-do-copy
      "i" 'dired-maybe-insert-subdir
      "+" 'dired-create-directory)))

(use-package dired-single
  :demand t)

(use-package restclient
  :defer 1
  :mode (("\\.http\\'" . restclient-mode)))

(use-package restclient-helm :defer 5)

(use-package company-restclient
  :config
  (progn
      (add-to-list 'company-backends 'company-restclient)))

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

(use-package coffee-mode)

(use-package elscreen
 :defer 1
 :config
 (progn
   (defun md/elscreen-display-tabs ()
     (interactive)
     (setq elscreen-display-tab t
           elscreen-tab-display-kill-screen nil
           elscreen-tab-display-control nil)

     ;; This is how elscreen redraws
     (elscreen-notify-screen-modification 'force))

   (md/elscreen-display-tabs)))

(setq md/splitscreen-path (md/dotfiles-get-path "splitscreen/"))

;; NOTE - for some reason this doesn't seem to load with "defer"
(use-package splitscreen
 :load-path md/splitscreen-path
 :demand t
 :config
 (progn
   (splitscreen-mode)))

(use-package popwin
  :demand t
  :config
  (progn
    (defun md/popwin-toggle ()
      "Either close popwin or open it in its last buffer"
      (interactive)
      (if popwin:focus-window
          (popwin:close-popup-window)
        (popwin:display-last-buffer)))

    (defun md/popwin-org ()
      (interactive)
      (when popwin:focus-window (popwin:close-popup-window))
      (if (get-buffer "index.org")
          (popwin:display-buffer "index.org")
        (message "No buffer: index.org")))

    (defun md/popwin-scratch ()
      (interactive)
      (when popwin:focus-window (popwin:close-popup-window))
      (popwin:display-buffer "*scratch*"))

    (defun md/popwin-messages ()
      (interactive)
      (when popwin:focus-window (popwin:close-popup-window))
      (popwin:display-buffer "*Messages*"))

    (defun md/popwin-ansi-term ()
      "Copied from
https://github.com/m2ym/popwin-el/blob/master/misc/popwin-term.el. For some
reason this is necessary to open term in a popwin window. Shell and eshell work
out of the box."
      (interactive)
      (when popwin:focus-window (popwin:close-popup-window))
      (popwin:display-buffer
       (or (get-buffer "*ansi-term*")
           (save-window-excursion
             (call-interactively 'ansi-term)))))

    (defun md/popwin-eshell ()
      (interactive)
      (when popwin:focus-window (popwin:close-popup-window))
      (popwin:display-buffer
       (or (get-buffer "*eshell*")
           (save-window-excursion
             (call-interactively 'eshell)))))

    (defun md/popwin-dired-single-magic-buffer ()
      (interactive)
      (when popwin:focus-window (popwin:close-popup-window))
      (popwin:display-buffer
       (or (get-buffer dired-single-magic-buffer-name)
           (save-window-excursion
             (call-interactively 'dired-single-magic-buffer)))))

    ;; Disable popwin-mode in an active Helm session, to prevent it from conflicting
    ;; with Helm windows. Also ensure that popwin-last-config doesn't return
    ;; helm buffers.
    (defvar md/popwin-last-config nil)
    (add-hook 'helm-before-initialize-hook
              (lambda ()
                (when popwin:focus-window (progn (popwin:close-popup-window)))))
    (add-hook 'helm-after-initialize-hook
              (lambda ()
                (setq md/popwin-last-config-store popwin:popup-last-config)
                (popwin:display-buffer helm-buffer t)
                (popwin-mode -1)
                ))
    (add-hook 'helm-cleanup-hook (lambda ()
                                   (popwin-mode 1)
                                   (setq popwin:popup-last-config md/popwin-last-config-store)))
    (push '("^\*helm.+\*$" :regexp t :dedicated nil :height 15) popwin:special-display-config)

    (setq popwin:popup-window-height 10)

    ;; TODO why isn't dired working? Judging by the examples it should, but
    ;; dired buffers just appear in their own windows. Tried on 24.5 and 25.1.
    (push '(dired-mode :dedicated t :height 20 :stick t) popwin:special-display-config)

    ;; NOTE: `:dedicated t` means matching buffers will reuse the same window.
    ;; Generally I only ever want one popwin window open.
    (push '("*Messages*" :tail t :dedicated t) popwin:special-display-config)
    (push '("index.org" :height 20 :dedicated t :stick t) popwin:special-display-config)
    (push '(help-mode :dedicated t :stick t :height 25) popwin:special-display-config)
    (push '("^\\*scratch\\*$" :regexp t :dedicated t :stick t) popwin:special-display-config)
    (push '("^\\*Flycheck.+\\*$" :regexp t :dedicated t :stick t :noselect t) popwin:special-display-config)
    (push '("*Messages*" :tail t :dedicated t) popwin:special-display-config)
    (push '("*Warnings*" :tail t :dedicated t) popwin:special-display-config)
    (push '("*Backtrace*" :dedicated t) popwin:special-display-config)
    (push '(completion-list-mode :noselect t :dedicated t) popwin:special-display-config)
    (push '(compilation-mode :noselect t :stick t :dedicated t :tail t) popwin:special-display-config)
    (push '(grep-mode :noselect t :dedicated t) popwin:special-display-config)
    (push '(ag-mode :noselect t :dedicated t :stick t :tail nil :height 15) popwin:special-display-config)
    (push '(occur-mode :noselect t :dedicated t :stick t) popwin:special-display-config)
    (push '("*vc-change-log*" :dedicated t) popwin:special-display-config)
    (push '("*undo-tree*" :width 60 :position right :dedicated t) popwin:special-display-config)
    (push '("*HTTP Response*" :height 20 :dedicated t :stick t :noselect t) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :tail t) popwin:special-display-config)
    (push '("*Async Shell Command*" :dedicated t :tail t) popwin:special-display-config)
    (push '(shell-mode :regexp t :dedicated t :height 15 :stick t :tail t) popwin:special-display-config)
    (push '(eshell-mode :regexp t :dedicated t :height 15 :stick t :tail t) popwin:special-display-config)
    (push '(term-mode :dedicated t :height 15 :stick t :tail t)
          popwin:special-display-config)  ; only works with md/popwin-ansi-term
    (push '(inferior-scheme-mode :dedicated t :height 15 :stick t :tail t) popwin:special-display-config)

    (popwin-mode 1))
  :bind (:map md/leader-map
              ;; I can't get arbitrary buffers/files to play nicely, so
              ;; just have the dedicated buffers.
              (";a" . md/popwin-toggle)
              (";d" . md/popwin-dired-single-magic-buffer)
              (";i" . md/popwin-org)
              (";s" . md/popwin-scratch)
              (";t" . md/popwin-ansi-term)
              (";e" . md/popwin-eshell)
              (";m" . md/popwin-messages)))

(use-package org
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
      ;; Don't let org-agenda permanently mess with window layout
      org-agenda-restore-windows-after-quit t

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

(bind-key "C-c d" 'md/org-timestamp-date-inactive-no-confirm org-mode-map)
(bind-key "C-c t" 'md/org-timestamp-time-inactive-no-confirm org-mode-map)
(bind-key "C-c l" 'md/org-insert-link-from-paste org-mode-map)

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

(define-minor-mode md/evil-org-agenda-mode
  "Buffer local minor mode for evil-org-agenda"
  :init-value nil
  :lighter " EvilOrgAgenda"
  :keymap (make-sparse-keymap) ; defines md/evil-org-agenda-mode-map
  :group 'md/evil-org-agenda)

(evil-set-initial-state 'org-agenda-mode 'normal)

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
  (kbd "C-/") 'org-agenda-filter-by-tag  ; Tag filter
  (kbd "^") 'org-agenda-filter-by-top-headline  ; Show other items with same
                                    ; headline as current
  (kbd "A") 'org-agenda-append-agenda)  ; Add another agenda

(add-hook 'org-agenda-mode-hook 'md/evil-org-agenda-mode)

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
  (org-gcal-refresh-token)
  (sleep-for 4)
  (org-gcal-fetch))

(use-package org-gcal
  :config
  (progn
    (setq
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

(let ((path (md/dotfiles-get-path "emacs.d.symlink/non-elpa/md.org/init.el")))
  (when (file-exists-p path)
    (load-file path)))

))

(bind-key "Bj" 'bookmark-jump md/leader-map)
(bind-key "Bs" 'bookmark-set md/leader-map)
(bind-key "Bd" 'bookmark-delete md/leader-map)

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
   (defface md/powerline-inactive '((t (:inherit 'modeline))) "")
   (defface md/powerline-normal '((t (:inherit 'modeline))) "")
   (defface md/powerline-insert '((t (:inherit 'modeline))) "")
   (defface md/powerline-visual '((t (:inherit 'modeline))) "")
   (defface md/powerline-replace '((t (:inherit 'modeline))) "")
   (defface md/powerline-emacs '((t (:inherit 'modeline))) "")
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
                                   ;; Line / column numbers
                                   (when (or line-number-mode column-number-mode)
                                     (cond ((and line-number-mode
                                                 column-number-mode)
                                            (powerline-raw "%4l:%2c " face3 'l))
                                           (line-number-mode
                                            (powerline-raw "%4l" face3 'l))
                                           (column-number-mode
                                            (powerline-raw ":%2c " face3 'l))))

                                   ;; Evil status
                                   (powerline-raw evil-mode-line-tag face3 'l)
                                   (funcall separator-left face3 face1)

                                   ;; Major mode
                                   (powerline-raw (format "*%s* " (powerline-major-mode)) face1 'l)
                                   (funcall separator-left face1 mode-line)

                                   ;; Projectile project
                                   (if (and (boundp 'projectile-mode) projectile-mode)
                                       (powerline-raw (concat (projectile-project-name) "::%b") 'l)
                                     (powerline-raw "%b" mode-line 'l))

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
                                      mode-line 'l))))

                             (rhs (list

                                   ;; Git
                                   (funcall separator-right mode-line face1)
                                   (powerline-vc face1 'r)

                                   ;; Flycheck
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
                                   )))
                        (concat (powerline-render lhs)
                                (powerline-fill mode-line (powerline-width rhs))
                                (powerline-render rhs)))))))

   (defun md/powerline-reset ()
     (interactive)
     (setq mode-line-format (md/powerline-setup))
     (solarized-load-theme))

   (md/powerline-setup)
   (md/powerline-reset)))

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
     (setq solarized-bold nil))

   (load-theme 'solarized t)  ; Defaults to light
   (solarized-enable-theme 'dark))

 :bind (:map md/leader-map
        ("ts" . solarized-toggle-theme-mode)
        ("cs" . solarized-toggle-comment-visibility)))

(use-package writeroom-mode
 :defer 1
 :demand t
 :config
 (progn
   (setq writeroom-width 90)

   (defun md/handle-elscreen (arg)
     (cond
      ((= arg 1)
       (setq elscreen-display-tab nil)
       (elscreen-notify-screen-modification 'force))
      ((= arg -1)
       (setq elscreen-display-tab t)
       (elscreen-notify-screen-modification 'force))))

   (add-to-list 'writeroom-global-effects 'md/handle-elscreen))

 :bind (:map md/leader-map
        ("tW" . writeroom-mode)))

(defun md/dotfiles-edit-init ()
  (interactive)
  (find-file (md/dotfiles-get-path "emacs.d.symlink/init.org")))

(bind-key "ve" 'md/dotfiles-edit-init md/leader-map)
(bind-key "vc" 'md/dotfiles-compile md/leader-map)

(use-package esup
  :defer 5)

(require 'server)
(when (not (server-running-p))
   (server-start))

(when (file-exists-p "~/.secrets.el")
      (load-file "~/.secrets.el"))

(defconst md/emacs-init-end (current-time))

(defconst md/emacs-boot-time (float-time (time-subtract md/emacs-init-end md/emacs-init-start)))
(message (format "md/emacs-boot-time: %s" md/emacs-boot-time))

)
