(define-prefix-command 'md/leader-map)

(defun md/maybe-native-compile-and-load (path loadp)
  "If a file exists and Emacs has the native compilation feature enabled,
use it to compile the file"
  (if (and path (file-exists-p path))
      (progn
        (when loadp
          (load-file path))
        (when (fboundp 'native-compile-async)
          (native-compile-async path t)))
    (message "Cannot load-file, doesn't exist: %s" path)))

(defun md/dotfiles-edit-init ()
  "Jump to my my init.org"
  (interactive)
  (find-file (md/emacs-get-path "init.org")))

(require 'bind-key)
(bind-key "ve" 'md/dotfiles-edit-init md/leader-map)
(bind-key "vc" 'md/dotfiles-compile-all md/leader-map)  ;; Defined in early-init file

(defun md/exwm-display-one ()
  "If monitor is connected, only use that. Otherwise, only use the main display."
  (interactive)
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--mode"
         (completing-read "Resolution: " '("2048x1080" "1920x1080" "2560x1440"))
         "--primary"
         "--output" default-output "--off")
        (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))

(defun md/exwm-display-mirror ()
  "Mirror display"
  (interactive)
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" default-output "--auto" "--primary"
         "--output" (match-string 1) "--auto" "--same-as" default-output)
        (setq exwm-randr-workspace-output-plist (list 0 default-output))))))

(defun compat-string-width (STRING &optional FROM TO)
  (string-width STRING FROM TO))

(defvar straight-bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (straight-bootstrap-version 6))
  ;; (unless (file-exists-p bootstrap-file)
  ;;   (with-current-buffer
  ;;       (url-retrieve-synchronously
  ;;        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
  ;;        'silent 'inhibit-cookies)
  ;;     (goto-char (point-max))
  ;;     (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Equivalent of using :straight t with use-package
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-minimum-reported-time 0.001)

  :config
  (add-to-list 'use-package-keywords :md/bind t)

  (defun use-package-normalize/:md/bind (name keyword args)
    "Custom use-keyword :md/bind. I use this to provide something similar to ':bind',
but with additional two features that I miss from the default implementation:

1. Integration with 'evil-define-key', so I can extend the keymap declaration
   to specify one or more evil states that the binding should apply to.

2. The ability to detect keymaps that aren't defined as prefix commands. This
   allows me to define a binding to a keymap variable, eg. maybe I want '<leader>h'
   to trigger 'help-map'. This fails using the default ':bind', meaning that I
   have to fall back to calling 'bind-key' manually if I want to assign a
   prefix.

The expected form is slightly different to 'bind':

((:map (KEYMAP . STATE) (KEY . FUNC) (KEY . FUNC) ...)
 (:map (KEYMAP . STATE) (KEY . FUNC) (KEY . FUNC) ...) ...)

STATE is the evil state. It can be nil or omitted entirely. If given, it should be an
argument suitable for passing to 'evil-define-key' -- meaning a symbol like 'normal', or
a list like '(normal insert)'."
    (setq args (car args))
    (unless (listp args)
      (use-package-error ":md/bind expects ((:map (MAP . STATE) (KEY . FUNC) ..) ..)"))
    (dolist (def args args)
      (unless (and (eq (car def) :map)
                   (consp (cdr def))
                   (listp (cddr def)))
        (use-package-error ":md/bind expects ((:map (MAP . STATE) (KEY . FUNC) ..) ..)"))))

  (defun use-package-handler/:md/bind (name _keyword args rest state)
    "Handler for ':md/bind' use-package extension. See 'use-package-normalize/:md/bind' for full docs."
    (let ((body (use-package-process-keywords name rest
                  (use-package-plist-delete state :md/bind))))
      (use-package-concat
       `((with-eval-after-load ',name
           ,@(mapcan
              (lambda (entry)
                (let ((keymap (car (cadr entry)))
                      (state (cdr (cadr entry)))
                      (bindings (cddr entry)))
                  (mapcar
                   (lambda (binding)
                     (let ((key (car binding))
                           (val (if (and (boundp (cdr binding)) (keymapp (symbol-value (cdr binding))))
                                    ;; Keymaps need to be vars without quotes
                                    (cdr binding)
                                  ;; But functions need to be quoted symbols
                                  `(quote ,(cdr binding)))))
                       ;; When state is provided, use evil-define-key. Otherwise fall back to bind-key.
                       (if state
                           `(evil-define-key ',state ,keymap (kbd ,key) ,val)
                         `(bind-key ,key ,val ,keymap))))
                   bindings)))
              args)))
       body))))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand t
  :config (exec-path-from-shell-initialize))

(use-package s
  :straight (:host github :repo "magnars/s.el"))

(use-package f :demand t)

(use-package exwm
  :if (md/exwm-enabled)
  :after (evil)
  :demand t
  :init
  ;; Required for sane bindings
  (evil-set-initial-state 'exwm-mode 'emacs)

  ;; Hooks for class and title, adapted from docs.
  ;;
  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
  ;;  all windows are probably the same.  Using window titles for them makes
  ;;  more sense.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.

  ;; TODO use :hooks to set these instead?
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  :custom
  (exwm-show-all-buffers t)
  (exwm-layout-show-all-buffers t)
  (exwm-workspace-number 1 "Only use a single exwm workspace")
  (exwm-input-global-keys
   `(;; Bind "s-r" to exit char-mode and fullscreen mode.
     ([?\s-r] . exwm-reset)

     ;; Make leader easily accessible
     (,(kbd "C-<SPC>") . md/leader-map)
     (,(kbd "s-<SPC>") . md/consult-launch)

     (,(kbd "s-$") . md/screenshot-image-selection)
     (,(kbd "s-%") . md/screenshot-video-selection-start)
     (,(kbd "s-^") . md/screenshot-video-stop)

     (,(kbd "s-<tab>") . eyebrowse-next-window-config)

     ([?\C-w] . splitscreen/prefix)
     ([?\s-w] . splitscreen/prefix)
     ([?\s-f] . exwm-layout-toggle-fullscreen))
   "exwm bindings that are always available")
  (exwm-input-simulation-keys
   `((,(kbd "C-w C-w") . [?\C-w]) ; The first C-w is the "splitscreen" prefix
     ([?\C-b] . [left])
     ([?\C-f] . [right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end]))
   "Rebindings that get sent through to X programs, eg. C-n/C-p can be down/up")

  :md/bind ((:map (exwm-mode-map)
                  ("C-q" . exwm-input-send-next-key)))
  :config
  ;; Suspending frame accidentally is very annoying with exwm
  (global-set-key (kbd "C-x C-z") nil)
  (exwm-enable))

;; randr / multi-monitor support for exwm
(use-package exwm-randr
  :if (md/exwm-enabled)
  :after (exwm)
  :straight nil
  :config
  (exwm-randr-enable))

(use-package emacs
  :after (s)

  ;; ======================================================================
  :init

  (defmacro md/with-widened-buffer (buffer-or-name &rest body)
    "Widen the given BUFFER-OR-NAME, execute BODY in the context of your current buffer, and restore restrictions on the given buffer.

This allows the calling code to not have to worry about manually handling
narrowed vs widened state."
    (let ((orig-buffer (gensym "orig-buffer")))
      `(let ((,orig-buffer (current-buffer)))
         (with-current-buffer ,buffer-or-name
           (save-restriction
             (save-excursion
               (widen)
               (with-current-buffer ,orig-buffer
                 ,@body)))))))

  (defun md/find-file-buffer (path)
    "Get or create a buffer visiting PATH without affecting current windows.

This is useful in situations where you have functions that accept a buffer object but you
only have the file path."
    (save-window-excursion
      (find-file path)
      (current-buffer)))

  (defun md/save-if-not-remote ()
    "I usually save files often (eg. when exiting insert-mode in evil).
Usually this is helpful, but if I'm using tramp to edit a remote file,
it can result in extra latency."
    (interactive)
    (if (not (file-remote-p default-directory))
        (save-buffer)))

  (defun md/strip-whitespace-and-save ()
    "Helper to clean up whitespace and save, which I run often."
    (interactive)
    (delete-trailing-whitespace)
    (save-buffer))

  (defun md/file-info ()
    "Print info about the file in the minibuffer"
    (interactive)
    (message
     "%s | %s %d:%d | %s lines | [%s]"
     major-mode
     (buffer-file-name)
     (array-current-line) ; current line
     (current-column) ; column
     (count-lines (point-min) (point-max)) ; total lines
     (let ((p (and (fboundp 'project-current) (project-current))))
       (if p
           (project-name p)
         "no project"))))

(defun md/project-copy-visited-file-path ()
  "Copy the file path relative to the current project root to the clipboard."
  (interactive)
  (if (buffer-file-name)
      (let* ((project-root (if (fboundp 'project-current)
                               (car (last (project-current)))
                             nil))
             (relative-path (if project-root
                                (file-relative-name buffer-file-name project-root)
                              buffer-file-name)))
        (kill-new relative-path)
        (message "Copied relative file path '%s'." relative-path))
    (message "No file is currently being visited.")))

  (defun md/toggle-debug-on-error ()
    "When enabled, this feature causes a debug buffer to pop up when there's an
    error. Helpful for er, debugging."
    (interactive)
    (setq debug-on-error (not debug-on-error))
    (message (format "debug-on-error %s" debug-on-error)))

  (defun md/status-message ()
    "Runs a script that prints some system info (time, battery etc.) and
echos results in the minibuffer. Only works on linux."
    (interactive)
    (let* ((message-log-max nil) ; ensure not logged in message buffer
           (output (s-trim-right
                    (shell-command-to-string "/f/users/matt/.config/i3-status-bash-once.sh")))
           (output-as-list (car (read-from-string output)))
           (propertized-string (mapconcat
                                (lambda (item)
                                  (concat
                                   (propertize (nth 0 item)
                                               'face
                                               `(:foreground ,(nth 2 item) :family "Font Awesome 5 Free" :height 0.6))
                                   (propertize (nth 1 item)
                                               'face
                                               `(:foreground ,(nth 2 item) :family "Noto sans" :height 0.7))))
                                output-as-list "")))
      (message propertized-string)))

  (defun md/fontify-buffer ()
    "Wrapper around font-lock-fontify-buffer, that tells me if it's working or not."
    (interactive)
    (if font-lock-mode
        (progn (call-interactively 'font-lock-fontify-buffer)
               (message "Fontified buffer"))
      (message "Not in font-lock-mode")))

  (defun md/font-size-incr ()
    "Increase the size of the default font."
    (interactive)
    (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 5)))

  (defun md/font-size-decr ()
    "Decrease the size of the default font."
    (interactive)
    (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 6)))

  (defun md/remove-file-and-buffer ()
    "Kill the current buffer and delete the file it's visiting (calling `git rm` if appropriate)."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when
          (and filename
               (string= (read-string (format "Delete %s? (y/n) " filename)) "y"))
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer))))))

  (defun md/rename-file-and-buffer ()
    "Rename the current buffer and the file its visiting (calling `git mv` if appropriate)."
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
    "Replace \n characters with an actual newline. Useful for making traceback strings readable."
    (interactive)
    (funcall-interactively 'replace-string "\\n" "
 " nil (region-beginning) (region-end)))

  (defun md/make-keymap-noop (kmap)
    "Overwrite bindings on a given keymap to perform a noop function.  There
might be a better way to do this (eg. unset the binding) but I've had this code
for ages and it seems to work."
    (mapc (lambda (key)
            (bind-key key 'ignore kmap)
            (bind-key (concat "C-" key) 'ignore kmap)
            (bind-key (concat "M-" key) 'ignore kmap)
            (bind-key (concat "C-M-" key) 'ignore kmap)
            (bind-key (capitalize key) 'ignore kmap)
            (bind-key (concat "C-" (capitalize key)) 'ignore kmap)
            (bind-key (concat "M-" (capitalize key)) 'ignore kmap)
            (bind-key (concat "C-M-" (capitalize key)) 'ignore kmap))
          '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
            "s" "t" "u" "v" "w" "x" "y" "z"
            "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
    (mapc (lambda (key)
            (bind-key key 'ignore kmap))
          '("SPC" "TAB")))

  (defun md/insert-blank-line-before ()
    "Util that I map to <leader>o to insert a new line."
    (interactive)
    (save-excursion
      (end-of-line)
      (open-line 1)
      (md/save-if-not-remote)))

  (defun md/quit-and-kill-window ()
    "Wrapper around quit-window that always kills the buffer instead of just hiding it."
    (interactive)
    (quit-window t))

  (defun md/system-lock ()
    "Linux util to lock the screen."
    (interactive)
    (shell-command "xset dpms force off >/dev/null 2>&1 & disown" nil nil))

  (defun md/system-sleep ()
    "Linux util to sleep."
    (interactive)
    (shell-command "systemctl suspend -i >/dev/null 2>&1 & disown" nil nil))

  (defun md/system-restart ()
    "Linux util to restart."
    (interactive)
    (shell-command "systemctl reboot -i >/dev/null 2>&1 & disown" nil nil))

  (defun md/system-shutdown ()
    "Linux util to shutdown."
    (interactive)
    (shell-command "systemctl poweroff -i >/dev/null 2>&1 & disown" nil nil))

  (defun md/screenshot-image-selection ()
    "Linux util to create a screenshot with a selection."
    (interactive)
    (shell-command ",screenshot --image-selection"))

  (defun md/screenshot-video-selection-start ()
    "Linux util to create a video screenshot with a selection."
    (interactive)
    (shell-command ",screenshot --video-selection-start"))

  (defun md/screenshot-video-stop ()
    "Linux util to stop a started video screenshot."
    (interactive)
    (shell-command ",screenshot --video-stop"))

  (defconst md/scratch-file-elisp "~/.emacs-scratch.el")
  (defun md/scratch-open-file-elisp ()
    "Open a persistent emacs-lisp 'scratch' file in my home directory. "
    (interactive)
    (find-file md/scratch-file-elisp))

  (defconst md/scratch-file-org "~/.emacs-scratch.org")
  (defun md/scratch-open-file-org ()
    "Open a persistent org 'scratch' file in my home directory. "
    (interactive)
    (find-file md/scratch-file-org))

  (defun md/toggle-window-dedicated ()
    "Use set-window-dedicated-p to toggle between a dedicated and non-dedicated window.

    Dedicated windows are fixed to displaying the current buffer."
    (interactive)
    (set-window-dedicated-p (selected-window) (not (window-dedicated-p)))
    (if (window-dedicated-p)
        (message "Dedicated")
      (message "Removed dedicated mode")))

  (defun md/disable-all-themes ()
    "Disable all current themes."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes))

  (defun md/load-theme ()
    "Wrapper to make load-theme more useful.

We disable all enabled themes before new theme selection, and then
make sure that we properly reload by fontifying the buffer etc.

Uses consult-theme if available.
"
    (interactive)
    (md/disable-all-themes)
    (setq org-todo-keyword-faces nil)
    (if (fboundp 'consult-theme)
        ;; consult-theme previews themes on the fly as you select them
        (call-interactively 'consult-theme)
      (call-interactively 'load-theme))
    ;; I don't like when themes show the fringe or git gutter with a different
    ;; background colour in the margin, so ensure this always matches the
    ;; default background
    (face-spec-set 'fringe
                   `((t :inherit 'default
                        :background ,(face-attribute 'default :background))))
    (face-spec-set 'git-gutter:unchanged
                   `((t :inherit 'default
                        :background ,(face-attribute 'default :background))))
    (face-spec-set 'git-gutter:separator
                   `((t :inherit 'default
                        :background ,(face-attribute 'default :background))))
    ;; Other theme customising

    ;;(md/powerline-reset)
    (md/fontify-buffer)
    (set-window-buffer nil (current-buffer)))

  (defun md/consult-diff-hunks-git-command (git-command-string)
    "Use consult read to jump between all current diff hunks with preview. Disclaimer -- seems to work but written quickly with GPT."
    (let* ((vertico-sort-function nil)
           (git-root (vc-root-dir))
           (diff-output (shell-command-to-string git-command-string))
           (lines (split-string diff-output "\n" t))  ; `t` to omit null strings from results
           (current-file "")
           (hunks '()))
      ;; Parse diff output to extract file names and hunks
      (dolist (line lines)
        (cond ((string-match "^diff --git a/\\(.*\\) b/\\(.*\\)" line)
               (setq current-file (expand-file-name (match-string 2 line) git-root)))
              ((string-match "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@\\(.*\\)" line)
               (let* ((line-number (match-string 3 line))
                      (deleted (if (> (length (match-string 2 line)) 0)
                                   (string-to-number (match-string 2 line))
                                 (if (equal (match-string 2 line) "") 1 0)))
                      (added (if (> (length (match-string 4 line)) 0)
                                 (string-to-number (match-string 4 line))
                               (if (equal (match-string 4 line) "") 1 0)))
                      (change-type (cond ((and (> added 0) (> deleted 0)) 'diff-changed)
                                         ((> added 0) 'diff-added)
                                         ((> deleted 0) 'diff-removed)
                                         (t 'diff-changed)))  ; Default to 'diff-changed' for any other unforeseen cases
                      (rest-of-line (match-string 5 line))
                      (formatted-line (format "%s:%s%s%s" current-file line-number
                                              (if (> added 0) (format " +%d" added) "")
                                              (if (> deleted 0) (format " -%d" deleted) ""))))
                 (put-text-property 0 (length formatted-line) 'face change-type formatted-line)
                 (setq hunks (append hunks
                                     (list (cons (format "%s %s" formatted-line rest-of-line)
                                                 (cons current-file (string-to-number line-number))))))))))
      ;; Use consult to select and navigate to hunks
      (if hunks
          (consult--read hunks
                         :prompt "Select hunk: "
                         :lookup #'consult--lookup-cdr
                         :category 'file
                         ;; If transform if t then this must return the item. If nil,
                         ;; must return the group name. We naively just split on a colon
                         ;; character, assuming it won't be included in the file path.
                         :group (lambda (cand transform)
                                  (if transform
                                      (mapconcat 'identity (cdr (split-string cand ":" t)) ":")
                                    (car (split-string cand ":"))))
                         :state (lambda (action entry)
                                  (when entry
                                    (let ((file (car entry))
                                          (line-number (cdr entry)))
                                      (find-file file)
                                      (goto-char (point-min))
                                      (forward-line (1- line-number))
                                      (recenter-top-bottom 10)
                                      ;; TODO: buffer display alist, and bindings
                                      (when git-gutter-mode
                                        (condition-case nil
                                            (call-interactively 'git-gutter:popup-hunk)
                                          (error nil)))
                                      ;; Kill windows when done
                                      (when (eq action 'return)
                                        (quit-windows-on git-gutter:popup-buffer))
                                      ))))
        (message "No matches"))))

  (defun md/consult-diff-hunks ()
    (interactive)
    (md/consult-diff-hunks-git-command "git diff --no-color -U0"))

  (defun md/consult-diff-cached-hunks ()
    (interactive)
    (md/consult-diff-hunks-git-command "git diff --no-color -U0 --cached"))

  ;; ======================================================================
  :config
  ;; Start up in fullscreen mode
  (toggle-frame-fullscreen)

  ;; By default "?" in help-map shows some help thing, but I prefer the
  ;; global behaviour of showing the key bindings for the map
  (unbind-key "?" help-map)

  ;; If the custom system is going to write to a file, don't do it in init.el -
  ;; use this file instead.
  (setq custom-file (md/emacs-get-path "custom.el"))

  ;; Wrap to 80 characters by default
  (setq-default fill-column 80)

  ;; The toolbar appears in the GUI frame. I don't want it:
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  ;; The menu bar is intrusive in terminal Emacs, so disable it. On the macos
  ;; app it appears as any other app toolbar at the top of the screen, which is
  ;; fine.
  (when (or (not (display-graphic-p))
            (string= (system-name) "arch"))
    (menu-bar-mode -1))

  ;; The cursor blinks by default. Disable it.
  (blink-cursor-mode -1)

  ;; Don't show scrollbars.
  (scroll-bar-mode -1)

  ;; Enable recentf so I can visit recent files..
  (recentf-mode 1)

  ;; There are various minor modes that I don't use that I've seen enabled in
  ;; the past. Make sure they're disabled.
  (dolist (this-minor-mode
           '(csv-field-index-mode
             diff-auto-refine-mode
             file-name-shadow-mode
             global-magit-file-mode
             mouse-wheel-mode))
    (when (fboundp this-minor-mode)
      (funcall this-minor-mode 0)))

  ;; ======================================================================
  :custom
  (inhibit-splash-screen t "I don't want to see the splash screen every time")
  (ring-bell-function 'ignore "Don't make a ridiculous noise when pressing C-g etc.")

  (scroll-margin 1 "How close the cursor should be to edge of page before scrolling")
  (scroll-conservatively 999 "")
  (scroll-step 1 "Only scroll one row at a time. Default behaviour is to centre the row")

  (left-margin-width 4 "Whitespace to the left of the window")
  (indent-tabs-mode nil "Use spaces instead of tabs")
  (tab-width 4 "Use 4 spaces for tabs")
  (tab-always-indent nil "Don't do magic indenting when I press tab")
  (line-spacing 0.4 "Increase the default line spacing ")

  (message-log-max 5000 "Increase default size of message buffer")
  (backup-directory-alist `(("." . ,(md/emacs-get-path ".backups"))) "Put backup files in .backups instead of dropping them everywhere")
  (delete-by-moving-to-trash t "Use the system's trash feature.")
  (recentf-max-saved-items 200 "Increase the number of recentf files")

  (vc-follow-symlinks t "I don't want to confirm this every time")
  (epa-pinentry-mode 'loopback "When showing the GPG prompt, use the minibuffer instead of popping up a separate GUI dialog box")

  (initial-buffer-choice md/scratch-file-org "Open my org scratch file on startup")

  (display-buffer-alist
   `(("\\*shell"
      (display-buffer-reuse-window display-buffer-same-window))
     ("\\*edit-indirect"
      (display-buffer-same-window))
     ("*\\(help\\|Help\\|Messages\\|Warnings\\|Compile-\\|chatgpt\\)"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.33))
     ("*\\(git-gutter:diff\\)"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (window-width . 0.5))
     ("*\\(Agenda Commands\\|Org Select\\)\\*" ;; Annoying org popups - agenda and capture selection
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.33))
     ("CAPTURE-.+"  ;; Org capture buffer
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.33)))
   "Customise how buffers are displayed. I used to use Shackle but I'm trying to stick to this for simplicity")

  (switch-to-buffer-obey-display-actions t "Ensure display-buffer-alist gets used when running switch-to-buffer, which would otherwise bypass it")

  (safe-local-variable-values
   '((org-confirm-babel-evaluate . nil)
     (org-time-stamp-formats . ("%Y-%m-%d" . "%Y-%m-%d %H:%M")))
   "Stop prompting me to accept these local variables")

  ;; ======================================================================
  :md/bind ((:map (global-map)
                  ("C-<SPC>" . md/leader-map))
            (:map (md/leader-map)
                  ("+" . md/font-size-incr)
                  ("-" . md/font-size-decr)
                  ("tx" . font-lock-mode)
                  ("x" . execute-extended-command) ; M-x
                  ("f" . find-file)
                  ("U" . undo-tree-visualize)
                  ("o" . md/insert-blank-line-before)

                  ("'o" . md/scratch-open-file-org)
                  ("'e" . md/scratch-open-file-elisp)

                  ("jf" . project-find-file)
                  ("jj" . project-switch-project)

                  ;; buffers
                  ("w" . save-buffer)
                  ("W" . md/strip-whitespace-and-save)
                  ("k" . kill-buffer)
                  ("bi" . md/file-info)
                  ("bk" . kill-buffer)
                  ("br" . read-only-mode)

                  ;; eval
                  ("ef" . eval-defun)
                  ("ee" . eval-last-sexp)
                  ("eb" . eval-buffer)
                  ("eE" . eval-expression)   ; in minibuffer
                  ("ex" . md/fontify-buffer)

                  ;; Emacs
                  ("Ek" . kill-emacs)
                  ("Es" . server-start)
                  ("Ep" . list-processes)
                  ("Ed" . md/toggle-debug-on-error)

                  ;; Git
                  ("gd" . md/consult-diff-hunks)
                  ("gc" . md/consult-diff-cached-hunks)

                  ;; Packages
                  ("Pi" . straight-use-package)
                  ("Pu" . package-update)
                  ("Pl" . package-list-packages)
                  ("Pr" . package-refresh-contents)

                  ;; Formatting
                  ("Fj" . json-pretty-print)
                  ("Fs" . sort-lines)
                  ("Fn" . md/expand-newlines)

                  ;; Toggle misc
                  ("tw" . toggle-truncate-lines)
                  ("tt" . md/load-theme)
                  ("tD" . md/toggle-window-dedicated)
                  ("t <tab>" . whitespace-mode)

                  ;; Help
                  ("h" . help-map))

            (:map (help-map)
                  ("x" . describe-face)
                  ("K" . describe-personal-keybindings))))

(use-package emacs
  :if (eq system-type 'darwin)

  :config
  ;; If this isn't set then pasting via Alfred doesn't work
  (if (eq window-system 'ns)
      (global-set-key (kbd "M-v") 'evil-paste-after))

  :custom
  ;; Set alt/option to use its default behaviour in OS X , so I can do
  ;; eg. alt+3 to insert #. By default in Emacs this is Meta, but I find Meta more
  ;; accessible on the left cmd key.
  (ns-option-modifier nil)

  ;; This is the default, and seems to handle the standard cmd key
  ;; bindings, so apple cmd+c runs super+c in emacs, etc. I don't use them
  ;; much, but they might be useful sometimes.
  (ns-right-command-modifier 'super)

  ;; Instead of the cmd bindings (that I don't use much), use the left
  ;; cmd key for Meta bindings. This is easier to reach than the default Meta
  ;; key (which is alt).
  (ns-command-modifier 'meta))

(use-package undo-tree
  :demand t
  :custom
  (undo-tree-auto-save-history nil "Don't save undo tree history files everywhere")
  :config (global-undo-tree-mode 1))

(use-package key-chord
  :functions key-chord-mode
  :custom
  (key-chord-two-keys-delay 0.4 "Set key delay"))

(use-package use-package-chords
  :after (key-chord)
  :config (key-chord-mode 1)
  :demand t)

(use-package evil
  :demand t
  :after (undo-tree)

  :md/bind ( ;; Like my vimrc, remap  ; to : and , to ;
            (:map (evil-motion-state-map)
                  (";" . evil-ex)
                  ("," . evil-repeat-find-char))
            ;; Like in the terminal. Mainly useful in minibuffer
            (:map (evil-insert-state-map)
                  ;; Use this instead of the default evil-complete-next, as
                  ;; completion-at-point integrates with consult etc. by default.
                  ("C-n" . completion-at-point)
                  ;; Emacs movement
                  ("C-a" . move-beginning-of-line)
                  ("C-e" . move-end-of-line))
            (:map (evil-visual-state-map)
                  ("SPC" . md/leader-map)
                  ("H" . move-beginning-of-line)
                  ("L" . move-beginning-of-line))
            ;; Various common bindings I use
            (:map (evil-normal-state-map)
                  ("H" . move-beginning-of-line)
                  ("L" . move-beginning-of-line)
                  ("(" . evil-previous-open-paren)
                  (")" . evil-next-close-paren)
                  ("j" . evil-next-visual-line)  ; equivalent of mapping to gj/gk
                  ("k" . evil-previous-visual-line)
                  ("M-j" . md/move-line-down)
                  ("M-k" . md/move-line-up)
                  ("M-h" . evil-shift-left-line)
                  ("M-l" . evil-shift-right-line)
                  ("C-l" . evil-jump-forward) ;; See setting below - we use this instead of vim's default C-i
                  ("gD" . xref-find-references)  ;; Like the opposite to gd, which goes to definition
                  ("SPC" . md/leader-map))
            (:map (md/leader-map)
                  ("q" . md/evil-fill)
                  ("Q" . md/evil-unfill)
                  ("cc" . comment-or-uncomment-region)
                  ("y" . md/project-copy-visited-file-path))
            ;; The *Warnings* buffer loads in normal mode, and I want to be able to quit
            ;; it easily
            (:map (special-mode-map . normal)
                  ("q" . quit-window))
            ;; Help bindings
            (:map (help-mode-map . normal)
                  ("q" . quit-window)
                  ("C-i" . help-go-forward)
                  ("C-o" . help-go-back)
                  ("<RET>" . help-follow-symbol)))

  :chords
  (:map evil-insert-state-map
        ;; Easy way to leave normal state
        ("jj" . md/normal-state-and-save)
        ("jk" . evil-normal-state)
        :map evil-replace-state-map
        ("jj" . md/normal-state-and-save)
        ("jk" . evil-normal-state))

  :init
  (defun md/normal-state-and-save ()
    "I bind this to jj and use it to exit insert-state and save."
    (interactive)
    (evil-normal-state)
    (md/save-if-not-remote))

  (defun md/evil-fill (&optional start end)
    "A DWIM fill function for the highlighted region or current paragraph."
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list nil nil)))
    (if (string= evil-state "visual")
        (fill-region start end)
      (fill-paragraph)))

  (defun md/evil-unfill (&optional start end)
    "Opposite of md/evil-fill."
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list nil nil)))
    (if (string= evil-state "visual")
        (let ((fill-column most-positive-fixnum))
          (fill-region start end))
      (let ((fill-column most-positive-fixnum))
        (fill-paragraph))))

  (defun md/move-line-up ()
    "Move the current line up one row."
    (interactive)
    (let ((col (current-column))
          ;; For org-agenda I like that you can temporarily change order of items, so ignore readonly
          (inhibit-read-only (eq major-mode 'org-agenda-mode)))
      (transpose-lines 1)
      (forward-line -2)
      (evil-goto-column col)))

  (defun md/move-line-down ()
    "Move the current line down one row."
    (interactive)
    (let ((col (current-column))
          ;; For org-agenda I like that you can temporarily change order of items, so ignore readonly
          (inhibit-read-only (eq major-mode 'org-agenda-mode)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (evil-goto-column col)))

  ;; Don't jump forward using C-i - we want to insert a TAB instead. This also
  ;; fixes issue where enabling evil prevents TAB from cycling headings in org-mode.
  ;; Seems like this has to be set before loading evil.
  (setq evil-want-C-i-jump nil)

  :custom
  (evil-echo-state nil "Don't put insert/visual etc in minibuffer")
  (evil-goto-definition-functions
   '(evil-goto-definition-xref
     evil-goto-definition-imenu
     evil-goto-definition-semantic
     evil-goto-definition-search)
   "Prefer xref for jumping to definition, as this means we get the Eglot jump-to feature when enabled. By default this prefers imenu")

  :config
  ;; I keep accidentally quiting with :q. Just deleting the window is enough
  (evil-ex-define-cmd "q[uit]" 'evil-window-delete)

  ;; Evil's undo functionality should be undo-tree
  (evil-set-undo-system 'undo-tree)

  ;; Use vi keys to navigate help-mode
  (evil-set-initial-state 'help-mode 'normal)

  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package emacs
  :after (evil)
  :init
  (defvar splitscreen/mode-map (make-sparse-keymap))
  (define-prefix-command 'splitscreen/prefix)
  (define-key splitscreen/mode-map (kbd "C-w") 'splitscreen/prefix)

  (defun splitscreen/window-left () (interactive) (evil-window-left 1))
  (defun splitscreen/window-right () (interactive) (evil-window-right 1))
  (defun splitscreen/window-up () (interactive) (evil-window-up 1))
  (defun splitscreen/window-down () (interactive) (evil-window-down 1))

  (defun splitscreen/increase-width () (interactive) (evil-window-increase-width 10))
  (defun splitscreen/decrease-width () (interactive) (evil-window-decrease-width 10))
  (defun splitscreen/increase-height () (interactive) (evil-window-increase-height 10))
  (defun splitscreen/decrease-height () (interactive) (evil-window-decrease-height 10))

  ;; We override these. Just declare them as part of the splitscreen map, not
  ;; evil-window-map.
  (define-key evil-window-map (kbd "h") nil)
  (define-key evil-window-map (kbd "j") nil)
  (define-key evil-window-map (kbd "k") nil)
  (define-key evil-window-map (kbd "l") nil)
  (define-key evil-window-map (kbd "n") nil)
  (define-key evil-window-map (kbd "p") nil)
  (define-key evil-window-map (kbd "c") nil)
  (define-key evil-window-map (kbd "C-h") nil)
  (define-key evil-window-map (kbd "C-j") nil)
  (define-key evil-window-map (kbd "C-k") nil)
  (define-key evil-window-map (kbd "C-l") nil)
  (define-key evil-window-map (kbd "l") nil)
  (define-key evil-window-map (kbd "o") nil)
  (define-key evil-window-map (kbd "x") nil)

  (define-key splitscreen/prefix (kbd "h") 'splitscreen/window-left)
  (define-key splitscreen/prefix (kbd "j") 'splitscreen/window-down)
  (define-key splitscreen/prefix (kbd "k") 'splitscreen/window-up)
  (define-key splitscreen/prefix (kbd "l") 'splitscreen/window-right)

  (define-key splitscreen/prefix (kbd "C-h") 'splitscreen/decrease-width)
  (define-key splitscreen/prefix (kbd "C-j") 'splitscreen/decrease-height)
  (define-key splitscreen/prefix (kbd "C-k") 'splitscreen/increase-height)
  (define-key splitscreen/prefix (kbd "C-l") 'splitscreen/increase-width)
  (define-key splitscreen/prefix (kbd "s-h") 'splitscreen/decrease-width)
  (define-key splitscreen/prefix (kbd "s-j") 'splitscreen/decrease-height)
  (define-key splitscreen/prefix (kbd "s-k") 'splitscreen/increase-height)
  (define-key splitscreen/prefix (kbd "s-l") 'splitscreen/increase-width)

  (define-key splitscreen/prefix (kbd "%") 'split-window-right)
  (define-key splitscreen/prefix (kbd "\"") 'split-window-below)
  (define-key splitscreen/prefix (kbd "x") 'delete-window)
  (define-key splitscreen/prefix (kbd "SPC") 'balance-windows)

  (define-minor-mode splitscreen-mode
    "Provides tmux-like bindings for managing windows and buffers.
                 See https://github.com/mattduck/splitscreen"
    :init-value 1 ; enable by default
    :global 1
    :keymap splitscreen/mode-map))

(use-package winner
  ;; I hit some issue using this with exwm
  :if (not (md/exwm-enabled))
  :straight nil
  :config (winner-mode 1)
  :md/bind ((:map (splitscreen/prefix)
                  ("u" . winner-undo)
                  ("U" . winner-redo))))

(use-package org
  :init
  (defun md/org-timestamp-time-inactive-no-confirm ()
    "Insert inactive time timestamp without prompting the user"
    (interactive)
    (org-insert-time-stamp (current-time) t t))

  (defun md/org-timestamp-date-inactive-no-confirm ()
    "Insert inactive date timestamp without prompting the user"
    (interactive)
    (org-insert-time-stamp (current-time) nil t))

  (defun md/org-narrow-next ()
    "Show a narrowed view of the next org node in the buffer. Can be used to cycle through nodes one by one."
    (interactive)
    (when (org-buffer-narrowed-p)
      (widen))
    (call-interactively 'org-next-visible-heading)
    (org-narrow-to-subtree)
    (outline-hide-subtree)
    (org-show-entry)
    (org-show-children))

  (defun md/org-narrow-prev ()
    "Show a narrowed view of the previous org node in the buffer. Can be used to cycle through nodes one by one."
    (interactive)
    (when (org-buffer-narrowed-p)
      (widen))
    (call-interactively 'org-previous-visible-heading)
    (org-narrow-to-subtree)
    (outline-hide-subtree)
    (org-show-entry)
    (org-show-children))

  (defconst md/org-review-property "LAST_REVIEWED"
    "I use this in a few places to keep track of when I lasted reviewed particular headlines")

  (defun md/org-review ()
    "Set the LAST_REVIEWED property to the current date/time"
    (interactive)
    (org-set-property md/org-review-property ; currently this is LAST_REVIEWED
                      (with-temp-buffer
                        (org-insert-time-stamp (current-time) nil t)))) ; Inactive stamp

  :config

  (defun md/org-link-sync ()
    "Sync an org-link to show the target headline as the contents.

When the cursor is on an org-link that uses the ID type, lookup the current state of the linked
headline, and replace the link contents with the current headline value.

For example, an \"outdated\" link like this:

    [[id:3C5473CB-3DCF-4A9B-9387-750730DAEB7B][My link contents description]]

Might be replaced by an up-to-date link like this:

    [[id:3C5473CB-3DCF-4A9B-9387-750730DAEB7B][DONE [#A] The current description of the headline]]"
    (interactive)
    (let* ((link-context (org-element-context))
           (type (org-element-property :type link-context))
           (path (org-element-property :path link-context))
           (point-begin (org-element-property :contents-begin link-context))
           (point-end (org-element-property :contents-end link-context)))
      (when (and path (equal type "id"))
        (let ((new-link-text
               (md/with-widened-buffer (md/find-file-buffer (org-id-find-id-file path))
                                       (save-window-excursion
                                         (org-open-at-point)
                                         (org-get-heading t nil nil nil)))))
          (goto-char point-begin)
          (delete-region point-begin point-end)
          (insert (format "%s" new-link-text)))
        (goto-char point-begin))))

  (defun md/org-ctrl-c-ctrl-c ()
    "I use this to add custom handlers and behaviour to C-c C-c.

For example, C-c- C-c is often used to update the state of org elements, and so
it feels like a natural way for me to call md/org-link-sync, because that
function updates the state of a ID link to be in sync with the target heading."
    (condition-case nil
        (let* ((link-context (org-element-context))
               (type (org-element-property :type link-context)))
          (cond
           ((and (eq (car link-context) 'link) (equal type "id"))
            (md/org-link-sync)
            t)  ; Returning t tells org-ctrl-c-ctrl-c that we did something
           (t nil)))  ; Tell org-ctrl-c-ctrl-c there was no match
      (error nil)))  ; Catch any errors in case org-element-context failed
  (add-hook 'org-ctrl-c-ctrl-c-hook 'md/org-ctrl-c-ctrl-c)

  (defun md/org-toggle-link-display ()
    "Call org-toggle-link-display and ensure any links in current buffer are redrawn.

By default, org-toggle-link-display does not force any kind of redraw of the
links in the buffer. AFAICT this /does/ happen by accident if you call it via
M-x - I don't know if that's universal by it does happen for me, I assume because
of the focus switch away from the original buffer to the vertico/helm buffer and then back."
    (interactive)
    (org-toggle-link-display)
    (org-restart-font-lock))

  (defadvice org-list-struct-fix-box (around ignore last activate)
    "Turn org-list-struct-fix-box into a no-op.

[2023-08-23]: By default, if an org list item is checked using the square-bracket
syntax [X], then org will look for a parent checkbox, and if all child items are
checked, it will set [X] on the parent too. This isn't how I personally use
child items -- I'll often use child checkboxes as subtasks, but it's almost
never an exhaustive list of everything that has to be done to close out the
parent -- and so I'd prefer to just control the parent checkbox state manually, or ideally to opt-in to this
behaviour by using a syntax similar to [/], which is used to show the count
of completed children.

AFAICT org-mode doesn't provide a way to customise this behaviour, /but/ the
behaviour all seems to be implemented in 'org-list-struct-fix-box'. And so I'm
trying something out by turning it into a no-op. It seems to work nicely initially,
but I won't be surprised if it causes an issue at some point because it's very
hacky."
    nil)

  ;; Another small org list / checkbox change here: add basic support for customising the
  ;; face for done '[X]' checkbox items.
  (defface md/org-checkbox-done-face
    '((t :height 0.9 :strike-through t))
    "Face for the done checkboxes.

AFAICT Org doesn't provide a way to customise the face used for done [X] checkboxes.
I use this face with a very basic regex to match done checkbox items, and apply the
face to the checkbox itself, so I can visually distinguish between done vs open items
more easily.

I think some properties will get overridden by org's actual font lock keywords, and
I haven't looked into that, but basic things like assigning strikethrough works ok,
and that's all I need."
    :group 'md/faces)
  (font-lock-add-keywords 'org-mode '(("- \\(\\[X\\]\\)" 1 'md/org-checkbox-done-face)))

  :md/bind ((:map (org-mode-map)
                  ("C-c d" . md/org-timestamp-date-inactive-no-confirm)
                  ("C-c t" . md/org-timestamp-time-inactive-no-confirm)
                  ("C-c l" . md/org-insert-link-from-paste)
                  ("C-c L" . org-insert-last-stored-link)
                  ("C-c y" . org-store-link)
                  ("C-c C-y" . org-store-link)
                  ("C-c P" . org-priority-up)
                  ("C-c T" . org-todo)
                  ("C-c E" . org-set-effort)
                  ("C-c C-r" . md/org-review)
                  ("C-j" . md/org-narrow-next)
                  ("C-k" . md/org-narrow-prev))
            (:map (global-map)
                  ("C-c c" . org-capture))
            (:map (md/leader-map)
                  ("a c" . org-capture)
                  ("RET" . org-capture)
                  ("a j" . org-clock-goto)
                  ("a i" . org-clock-in)
                  ("a o" . org-clock-out)
                  ("t l" . md/org-toggle-link-display)))

  :hook (
         ;; TODO - is this required or does it work by default?
         (org-mode . turn-on-auto-fill))

  :custom
  (org-src-window-setup 'current-window "When editing a src block, just use the current window instead of rearranging the frame")
  (org-indirect-buffer-display 'current-window "Similar to org-src-window-setup - I find this more intuitive")
  (org-edit-src-content-indentation 0 "Don't indent code in a src block. This way it's easier to edit inline in the org buffer")
  (org-startup-folded t "Don't expand org buffers on open")
  (org-log-done 'time "Add timestamp when set task as closed")
  (org-id-link-to-org-use-id 'create-if-interactive "Use :ID: values when calling org-store-link, instead of it storing a text-search link that can break easily")
  (org-level-color-stars-only nil "Colour the whole heading")
  (org-fontify-done-headline t "Colour done headings tomake them less prominent")
  (org-fold-catch-invisible-edits 'show-and-error "Try to prevent accidentally editing hidden lines")
  (org-adapt-indentation nil " Don't indent things for nested headings (eg. properties)")
  (org-clock-out-remove-zero-time-clocks t "Don't keep zero clocks")
  (org-ellipsis " …" "Use utf-8 ellipsis character when an item has hidden content")
  (org-hide-emphasis-markers nil "Whether to show the markup characters for bold/underline/emphasis etc")
  (org-pretty-entities nil "I don't really use special characters and don't want them showing up accidentally")
  (org-fontify-quote-and-verse-blocks t)
  (org-image-actual-width 400 "Set width for images as their original size can be too big sometimes")
  (org-capture-bookmark nil "Don't create a bookmark to the last captured item")
  (org-M-RET-may-split-line nil "If I press M-RET I want a new line, not to split the line")
  (org-goto-interface 'outline-path-completion "For org-goto, use completion rather than the weird default interface where you search through the file")
  (org-outline-path-complete-in-steps nil "Search the whole path rather than having to select the top-level heading first then the children")
  (org-refile-use-outline-path t "When refiling, show the full path to the node rather than just the node name")
  (org-highest-priority 65 "Priority A")
  (org-lowest-priority 68 "Priority D")
  (org-default-priority 68 "Default to D")
  (org-log-into-drawer t "Put state transitions into the LOGBOOK drawer, instead of the main body of the item")
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("linktoc=all,colorlinks=true,linkcolor=black,urlcolor=blue" "hyperref" nil))
   "Very similar to the original value - I think I've just added the href customisation "))

(use-package org
  :after (evil)

  :init
  (define-minor-mode md/evil-org-mode
    "Buffer local minor mode for evil-org"
    :init-value nil
    :lighter " EvilOrg"
    :keymap (make-sparse-keymap) ; defines md/evil-org-mode-map
    :group 'md/evil-org)

  (defun md/org-hook ()
    "Some behaviour overrides for org-mode"

    ;; Change tab widths to fit headline indents
    ;;
    ;; [2024-03-29] NOTE: since a recent org release lots of commands error if
    ;; tab width isn't set to 8. I previously had it as 2. Unsure implications of this.
    (setq tab-width 8
          evil-shift-width 2))

  (defun md/org-insert-link-from-paste ()
    "Perform org-insert-link with the current contents of the clipboard.

If a region is highlighted, then turn that region into a link using the
clipboard contents. Otherwise, prompt for a description"
    (interactive)
    ;; Region version - turn selected region into link
    (if (use-region-p)
        (let ((region-text (buffer-substring (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (org-insert-link
           nil
           (with-temp-buffer
             (evil-paste-after nil)
             (delete-trailing-whitespace)
             (buffer-string))
           region-text))

      ;; New link - prompt for description
      (org-insert-link
       nil
       (with-temp-buffer
         (evil-paste-after nil)
         (delete-trailing-whitespace)
         (buffer-string))
       (read-string "Description: "))))

  :md/bind ((:map (evil-insert-state-map)
                  ("C-c d" . md/org-timestamp-date-inactive-no-confirm)
                  ("C-c t" . md/org-timestamp-time-inactive-no-confirm))
            (:map (md/evil-org-mode-map . normal)
                  ("gk" . outline-previous-visible-heading)
                  ("gj" . outline-next-visible-heading)
                  ("gK" . md/org-narrow-prev)
                  ("gJ" . md/org-narrow-next)
                  ("H" . org-beginning-of-line)
                  ("L" . org-end-of-line)
                  ("$" . org-end-of-line)
                  ("^" . org-beginning-of-line)
                  ("-" . org-cycle-list-bullet)
                  ("RET" . org-cycle)
                  ("TAB" . org-cycle))
            (:map (md/evil-org-mode-map . (normal insert))
                  ("M-l" . org-metaright)
                  ("M-h" . org-metaleft)
                  ("M-k" . org-metaup)
                  ("M-j" . org-metadown)
                  ("M-L" . org-shiftmetaright)
                  ("M-H" . org-shiftmetaleft)
                  ("M-K" . org-shiftmetaup)
                  ("M-J" . org-shiftmetadown)
                  ("C-c u" . org-priority-up)))

  :hook ((org-mode . md/org-hook)
         (org-mode . md/evil-org-mode)))

(use-package org
  :after (evil)

  :init
  (define-minor-mode md/evil-org-agenda-mode
    "Buffer local minor mode for evil-org-agenda"
    :init-value nil
    :lighter " EvilOrgAgenda"
    :keymap (make-sparse-keymap) ; defines md/evil-org-agenda-mode-map
    :group 'md/evil-org-agenda)

  (defun md/org-agenda-todo ()
    "Wrap org-agenda-todo but always use the prefix. Saves me pressing C-u."
    (interactive)
    (setq current-prefix-arg '(4))  ; C-u
    (call-interactively 'org-agenda-todo))

  ;; Not strictly an org util - if I need it anywhere else I'll move it out.
  (defun md/advice-suppress-delete-other-windows (fn &rest args)
    "Hacky advice to fix something I don't like about org-agenda and org-capture.

When calling org-agenda, before the '*Agenda Commands*' buffer is switched to,
there's a manual call to (delete-other-windows). These windows are restored once
you've selected an agenda command, but I find it jarring to have my windows
temporarily disappear, and as the '*Agenda Commands*' buffer is quite small, it
isn't necessary to delete everything else on my screen. Org-capture has a
similar issue.

Unfortunately there's no clean way to prevent this behaviour. Eg. for org-agenda
it happens inside (org-agenda-get-restriction-and-command) with an inlined call
to (delete-other-windows). This is a quite a long function, and isn't something
that I want to inline and redefine myself. So instead we have this hacky
dual-advice approach: this advice will create temporary advice to turn
delete-other-windows into a no-op, and then restore once the org function has exited."
    (advice-add 'delete-other-windows :override
                'ignore
                '((name . "ignore")))
    (let ((result (condition-case nil
                      (apply fn args)
                    (t nil))))
      (advice-remove 'delete-other-windows "ignore")
      result))
  (advice-add 'org-agenda-get-restriction-and-command :around 'md/advice-suppress-delete-other-windows '((name . "ignore")))
  (advice-add 'org-capture :around 'md/advice-suppress-delete-other-windows '((name . "ignore")))

  :config
  ;; When org-agenda loads I want to be able to use j/k etc for navigation like any buffer.
  (evil-set-initial-state 'org-agenda-mode 'normal)

  :md/bind ((:map (global-map)
                  ("C-c a" . org-agenda))
            (:map (md/leader-map)
                  ("a a" . org-agenda)
                  ("TAB" . org-agenda))
            (:map (md/evil-org-agenda-mode-map . normal)
                  ;; Next/previous line
                  ("j" . org-agenda-next-line)
                  ("n" . org-agenda-next-line)
                  ("C-n" . org-agenda-next-line)
                  ("k" . org-agenda-previous-line)
                  ("p" . org-agenda-previous-line)
                  ("C-p" . org-agenda-previous-line)

                  ("TAB" . org-agenda-goto)  ; Goto selected item in other window
                  ("RET" . org-agenda-switch-to)  ; Replace agenda with this item
                  ("t" . md/org-agenda-todo)  ; Cycle todo state
                  ("P" . org-agenda-priority-up)
                  ("E" . org-agenda-set-effort)
                  ("R" . org-agenda-refile)
                  ("T" . org-agenda-set-tags)
                  ("C" . org-agenda-columns)

                  ;; Copy ID link to the heading
                  ("Y" . org-store-link)
                  ("C-c y" . org-store-link)
                  ("C-c C-y" . org-store-link)

                  ("]" . org-agenda-later)
                  ("[" . org-agenda-earlier)

                  ("q" . org-agenda-quit)
                  ("r" . org-agenda-redo)  ; Recalculate the agenda
                  ("v" . org-agenda-view-mode-dispatch)  ; Alter the view - toggle archived, logs, clocks etc.
                  ("\\" . org-agenda-filter-remove-all)  ; Remove existing filters
                  ("/" . org-agenda-filter-by-regexp)  ; Search
                  ("@" . org-agenda-filter)  ; Tag filter
                  ("'" . org-agenda-filter-by-category)  ; Show other items with same category as current
                  ("e" . org-agenda-filter-by-effort)
                  ("A" . org-agenda-append-agenda)))  ; Add another agenda

  :custom
  (org-agenda-restore-windows-after-quit nil "Whether to let org-agenda permanently mess with window layout")
  (org-agenda-window-setup 'current-window "The default of 'reorganize-frame reorganising all my windows in an annoying way. Using current-window makes it predictable")
  (org-agenda-sticky t "Cache org agenda until manually refreshed, to decrease wait times")

  :hook ((org-agenda-mode . md/evil-org-agenda-mode)))

(use-package ox
  :straight nil
  :custom
  (org-export-use-babel nil "Don't evaluate code as part of export. Actually part of ob-exp, I might want to split this out")
  (org-export-headline-levels 6 "The last level that's still exported as a headline")
  (org-export-with-section-numbers 2 "How many levels to export with numbers")
  (org-export-with-sub-superscripts nil "Don't mess up things_with_underscores in html export"))

(use-package ox-html
  :straight nil
  :custom
  (org-html-validation-link nil "Don't link to the validation service in the HTML export")
  (org-html-postamble "<hr>")
  (org-html-head-include-default-style nil "Don't use org's default html export style")
  (org-html-head "
  <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.min.css\">
  <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.6/styles/github.min.css\">
  <script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js\"></script>
  <script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/python.min.js\"></script>
  <script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/javascript.min.js\"></script>
  <script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/shell.min.js\"></script>
  <script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/lisp.min.js\"></script>
  <script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/markdown.min.js\"></script>

  <script type=\"text/javascript\">
  // @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&amp;dn=gpl-3.0.txt GPL-v3-or-Later
  <!--/*--><![CDATA[/*><!--*/
       function CodeHighlightOn(elem, id)
       {
         var target = document.getElementById(id);
         if(null != target) {
           elem.classList.add(\"code-highlighted\");
           target.classList.add(\"code-highlighted\");
         }
       }
       function CodeHighlightOff(elem, id)
       {
         var target = document.getElementById(id);
         if(null != target) {
           elem.classList.remove(\"code-highlighted\");
           target.classList.remove(\"code-highlighted\");
         }
       }
      /*]]>*///-->
  // @license-end
  </script>

   <style type=\"text/css\">
   @charset \"UTF-8\";

   body {
       /* Mobile settings */
       font-size: 17px;
       margin-left: 10px;
       margin-right: 10px;

       /* On mobile firefox the browser chrome often takes up the top of the
       screen. I think this is a browser issue but it looks bad, so push the content down. */
       margin-top: 3em;

       /* General settings */
       max-width: 700px;
       line-height: 1.6;
       font-family: sans-serif;

       /* Font smoothing */
       -moz-osx-font-smoothing: grayscale;
       -webkit-font-smoothing: antialiased;
   }

   /* Override the mobile settings for a bigger screen size. */
   @media (min-width: 701px) {
       body {
           margin-left: auto;
           margin-right: auto;
           margin-top: 0;
       }
   }

   /* Heading styles. h1 has a smaller relative top margin */
   h1 {
       margin-bottom: 0;
       margin-top: 2em;
       line-height: 1.1;
       font-weight: normal;

       /* Special styling for h1 */
       border-bottom: 2px solid black;
       padding-bottom: 0.5em;
   }
   h2, h3, h4, h5, h6 {
       margin-bottom: 0;
       margin-top: 3em;
       line-height: 1.1;
       font-weight: normal;
   }

   /* The org-mode section numbers in headers are made less prominent */
   .section-number-1, .section-number-2, .section-number-3 {
       font-family: monospace;
       font-size: smaller;
   }

   #root span {
       font-size: 2em;
       display: inline-block;
       margin-top: 1em;
       color: black !important;
   }

   /* Add margin below the nav links */
   #header-sitemap {
       margin-bottom: 2em;
   }

   img, video {
       margin-top: 1em;
       margin-bottom: 1em;
       display: block;  /* So the top/bottom margins aren't double-counted */
       max-width: 90%;
   }

   /* By default there's no spacing between list items, which is less readable IMO */
   li {
       margin-top: 1em;
       margin-bottom: 1em;
   }

   /* Don't show the HOME / UP links that org-mode generates */
   #org-div-home-and-up { display: none; }

   /* Org tags */
   .red {background-color: #af7575;}
   .amber {background-color: #efd8a1;}
   .green {background-color: #bcd693;}
   .blue {background-color: #afd7db;}


   /* Make the timestamp smaller */
   .timestamp {
       font-family: monospace;
       font-size: smaller;
   }

   /* Basic table styling */
   td, th {
       padding: 0.5em;
       vertical-align: top;
       text-align: left;
       background-color: #f9f9f9;
       font-size: smaller;
   }

   /* For the sitemap we don't use the normal table styling */
   .sitemap td, .sitemap th {
       background-color: transparent;
       padding-bottom: 1em;
       padding-left: 0;
       font-size: inherit;
       line-height: 1.1;
   }

   /* Make sure there isn't any weird padding in the nav */
   #header-sitemap td {
       padding-top: 0;
       padding-bottom: 0;
   }

   /* Make sure there isn't any weird padding in the nav */
   #footer-sitemap td {
       padding-top: 0;
       padding-bottom: 1em;
   }

   hr {
       margin-bottom: 2em;
   }

   /* This is copied from the hljs code blocks - it makes the pre blocks consistent. */
   pre {
       padding: 0.5em;
       color: #333;
       background: #f8f8f8;
       overflow-x: auto;
       display: block;

       /* Lower line-height than main prose */
       line-height: 1.3;
   }

   code {
       /* Inline code uses the same red colour from hljs github theme */
       color: #d14;
       background-color: #f8f8f8;

       /* Make sure pre elements don't cause the page to extend on mobile */
       overflow-wrap: anywhere;

       /* Lower line-height than main prose */
       line-height: 1.3;
   }

   /* Use a smaller font size for code blocks so there's less horizontal scrolling */
   pre > code, pre {
       font-size: smaller;
   }

   /* Indent code blocks, tables */
   pre, .hljs, table, img, video {
       margin-left: 1em;
       margin-right: 1em;
   }

   /* For the sitemap, we don't indent */
   table.sitemap {
       margin-left: 0;
       margin-right: 0;
   }

   /* Links use the same blue colour from hljs github theme */
   a {
       color: #0086b3;
   }

   /* Add a left border to quotes */
   blockquote {
       border-left: 2px solid black;
       padding-left: 0.5em;
   }

   /* Definition list terms can be bold */
   dt {
       font-weight: bold;
   }
   </style>

  <script type=\"text/javascript\">
  const init = () => {
      hljs.initHighlighting();
  }
  window.addEventListener('load', init, false );
  </script>"))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode))
  :custom
  (org-bullets-bullet-list '("■"
                             "▣"
                             "▢"
                             "▷"
                             "▹"
                             "*"
                             "*"
                             "*"
                             )))

(use-package org-super-agenda)

(use-package org-fancy-priorities)

(use-package ox-rss
  :demand t
  :config
  (when (fboundp 'org-rss-final-function)
    (fmakunbound 'org-rss-final-function)
    (defun org-rss-final-function (contents backend info)
      "Prettify the RSS output. Copied from ox-rss, but doesn't call indent-region"
      (with-temp-buffer
        (xml-mode)
        (insert contents)
        ;;(indent-region (point-min) (point-max))
        (buffer-substring-no-properties (point-min) (point-max))))))

(use-package ox-org :demand t :straight nil)

(use-package org-mind-map
  :demand t
  ;; includes my fix https://github.com/the-humanities/org-mind-map/pull/52
  :straight (:host github :repo "mattduck/org-mind-map")
  :load-path "/f/dotfiles/../emacs.default/non-elpa/org-mind-map"

  :custom
  (org-mind-map-include-text nil)
  (org-mind-map-engine "dot")
  (org-mind-map-tag-colors 'nil)
  (org-mind-map-wrap-text-length 30)
  (org-mind-map-default-node-attribs '(("shape" . "plaintext")))
  (org-mind-map-default-edge-attribs '(("color" . "#cccccc")
                                       ("arrowhead" . "none")
                                       ("arrowtail" . "none")))
  (org-mind-map-default-graph-attribs '(("autosize" . "false")
                                        ("size" . "125,50")
                                        ("resolution" . "100")
                                        ("nodesep" . "0.4")
                                        ("margin" . "0.1")
                                        ("overlap" . "false")
                                        ("splines" . "ortho")
                                        ("rankdir" . "LR")))

  :config
  (defun md/org-mind-map-export ()
    "org-mind-map export with some tag/property replacement"
    (interactive)
    (let ((current-buffer-contents ;; either selected region or whole buffer
           (if (region-active-p)
               (buffer-substring (region-beginning) (region-end))
             (buffer-string)))
          (base-filename  ;; if can't detect filename, prompt for it
           (file-name-sans-extension
            (file-name-sans-extension
             (if buffer-file-name buffer-file-name
               (completing-read "filename: " (directory-files "."))))))
          (buffer-offset 0))
      (with-temp-buffer
        (insert current-buffer-contents)
        (goto-char (point-min))
        (org-mode)
        (org-align-all-tags)
        (org-element-map (org-element-parse-buffer 'object nil) 'headline
          (lambda (elem)
            (goto-char (+ (org-element-property :begin elem) buffer-offset))
            (let ((first-tag (car (org-get-tags nil t)))
                  (elem-buffer-size (buffer-size))
                  (elem-offset 0))
              (cond ((string= first-tag "red") (org-set-property "OMM-COLOR" "#AF7575"))
                    ((string= first-tag "amber") (org-set-property "OMM-COLOR" "#EFD8A1"))
                    ((string= first-tag "green") (org-set-property "OMM-COLOR" "#BCD693"))
                    ((string= first-tag "blue") (org-set-property "OMM-COLOR" "#AFD7DB"))
                    (t nil))
              (org-set-tags "")
              ;; Offset is used to account for the fact that we have added/removed characters,
              ;; so the old :begin value will be wrong.
              (setq elem-offset (- (buffer-size) elem-buffer-size))
              (setq buffer-offset (+ buffer-offset elem-offset)))))
        (org-mind-map-write-named (concat base-filename ".mind-map") (concat base-filename ".mind-map.dot") t)))))

(use-package edit-indirect)

(use-package emacs
  :demand t
  :after (org edit-indirect)
  :config
  ;; Don't ask for confirmation on narrow-to-region
  (put 'narrow-to-region 'disabled nil)

  (defvar md/narrow-dwim-enable-org-clock nil
    "When true, md/narrow-dwim will start/stop the clock for narrowed org subtrees")

  (defun md/narrow-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p))
           (progn
             (when
                 (and md/narrow-dwim-enable-org-clock
                      (string= major-mode "org-mode")
                      (org-clock-is-active))
               (org-clock-out nil t))
             (widen)))
          ((region-active-p)
           (edit-indirect-region (region-beginning) (region-end) t))
          (edit-indirect--overlay
           (edit-indirect-commit))
          (org-src-mode
           (org-edit-src-exit))
          ((derived-mode-p 'org-mode)
           (cond ((ignore-errors (org-edit-src-code) t))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (progn
                      (org-narrow-to-subtree)
                      (when (and md/narrow-dwim-enable-org-clock
                                 (not (org-clock-is-active)))
                        (org-clock-in))))))
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

  :md/bind ((:map (md/leader-map)
                  ("n" . narrow-map))
            (:map (narrow-map)
                  ("i" . org-tree-to-indirect-buffer)
                  ("F" . md/edit-indirect-jinja)
                  ("v" . md/narrow-to-region-indirect)
                  ("f" . md/narrow-dwim)
                  ("r" . narrow-to-region))))

(use-package vertico
  :demand t
  :after (evil)
  :custom
  (vertico-count 7 "Items displayed, defaults to 10")
  :config
  (vertico-mode 1)
  :md/bind ((:map (vertico-map)
              ;; Make C-l and C-j behave similar to what I'm used to from my Helm setup
              ("C-l" . vertico-insert)
              ("C-j" . evil-delete-backward-word)
              ;; If the output is grouped, I can use this to cycle the groups. This is easier
              ;; than using consult narrowing.
              ("C-k" . vertico-next-group))))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic) "Use orderless but fall back to Emacs' 'basic' completion if it doesn't work")
  (completion-category-overrides '((file (styles basic partial-completion))) "Orderless docs recommend this for handling Tramp properly."))

(use-package marginalia
  :config
  (marginalia-mode 1)
  :custom
  (marginalia-field-width 160 "Increase the width from 80 to see more info"))

(use-package consult
  :after (evil)
  :init
  (defun md/list-applications ()
    "List applications installed in /usr/share. Linux only."
    (-map
     (lambda (item)
       (s-chop-suffix ".desktop" item))
     (-filter (lambda (d) (not (or (string= d ".") (string= d ".."))))
              (directory-files "/usr/share/applications"))))

  (defun md/gtk-launch (program-name)
    "Use gtk-launch to run a program."
    (shell-command (concat "gtk-launch " program-name " >/dev/null 2>&1 & disown") nil nil))

  (defun md/consult-launch ()
    "Alfred-like task launcher - type the program name and run it from within
    Emacs. I use this to start programs with exwm."
    (interactive)
    (consult--read
     (md/list-applications)
     :category 'md/program
     :prompt "Program: "
     :state (lambda (action cand)
              (when (and cand (eq action 'return))
                (md/gtk-launch cand)))))

  ;; [2024-02-28] Use consult to improve C-o -- taken from
  ;; https://github.com/emacs-evil/evil-collection/blob/master/modes/consult/evil-collection-consult.el
  (defun evil-collection-consult-jump-list ()
    "Jump to a position in the evil jump list."
    (interactive)
    (consult-global-mark
     (delq nil (mapcar (lambda (jump)
                         (let ((mark (car jump)))
                           (when (markerp mark)
                             mark)))
                       (ring-elements (evil--jumps-get-window-jump-list))))))

  :config
  (consult-customize
   ;; Disable preview when switching buffers
   consult-buffer :preview-key nil)

  ;; Populate an initial value for consult-line, as this doesn't happen by default
  ;; consult-line :initial (thing-at-point 'symbol))
  :md/bind ((:map (md/leader-map)
                  ("p" . consult-buffer)
                  ("jp" . consult-project-buffer)  ; project-file-file just works by default, this is separate
                  ("/" . consult-line)
                  ("j/" . consult-ripgrep)
                  ("j." . xref-find-apropos)
                  ("." . consult-imenu)) ; See eglot section for consult-eglot-symbols, bound to j.
            (:map (global-map . normal)
                  ("C-o" . evil-collection-consult-jump-list))
            (:map (consult-narrow-map)
                  ;; This shows the support "narrow" keys for this completion
                  ("?" . consult-narrow-help)))

  :custom
  (completion-in-region-function #'consult-completion-in-region "Use consult for completion")
  (xref-show-xrefs-function #'consult-xref "Use consult to show xrefs, ie. when looking for references to a symbol")
  (xref-show-definitions-function #'consult-xref "Use consult to select xref definitions when there are more than one")
  (consult-narrow-key "<" "Used to narrow results to a particular type, eg. functions, files")
  (consult-async-min-input 1 "Run async commands like consult-eglot-symbols sooner than the default of 3 keys"))

(use-package xclip
  :config
  (xclip-mode 1))

(use-package helpful
  :md/bind ((:map (help-map)
                  ("v" . helpful-variable)
                  ("f" . helpful-function)
                  ("k" . helpful-key)
                  ("c" . helpful-command)
                  ("m" . helpful-macro)
                  ("M" . describe-mode)  ; This is "m" by default.
                  ("g" . helpful-at-point))
            (:map (helpful-mode-map . normal)
                  ("q" . md/quit-and-kill-window))))

(use-package free-keys
  :md/bind ((:map (help-map)
               ("@" . free-keys))))

(use-package eyebrowse
  :demand t

  :init
  (defvar splitscreen/zoomed-p nil)
  (defun splitscreen/toggle-zoom ()
    "Toggle buffer-maximising within this eyebrowse tab. Replicates the
   tmux zoom feature that expands a single pane."
    (interactive)
    (if (= 1 (length (window-list)))
        (when (and (get-register (eyebrowse--get 'current-slot))
                   splitscreen/zoomed-p)
          (progn
            (setq-local splitscreen/zoomed-p nil)
            (jump-to-register (eyebrowse--get 'current-slot))))
      (progn
        (window-configuration-to-register (eyebrowse--get 'current-slot))
        (setq-local splitscreen/zoomed-p t)
        (delete-other-windows))))

  (defun splitscreen/reset-zoom (fn &rest args)
    "Ensure when a slot is closed, we forget the zoom for that slot."
    (apply fn args)
    (set-register (eyebrowse--get 'current-slot) nil))

  (defun md/eyebrowse-status (fn &rest args)
    "Advice for eyebrowse functions, to message the current eyebrowse status
in the echo area. I prefer to this putting it in the mode-line, because eyebrowse is a
slot/window-level thing, not buffer-level."
    (apply fn args)
    (message (format "%s" (eyebrowse-mode-line-indicator))))

  :custom
  (eyebrowse-wrap-around t "Allow cycling forever")
  (eyebrowse-mode-line-separator " " "Use space instead of comma in the mode-line")
  (eyebrowse-mode-line-left-delimiter "" "Remove the square brackets in the mode-line")
  (eyebrowse-mode-line-right-delimiter "" "Remove the square brackets in the mode-line")
  (eyebrowse-mode-line-style t "Always show this in the mode line (Although, I modify the mode-line separately so this is redundant unless I disable that)")
  (eyebrowse-new-workspace t "The new workspace should show the scratch buffer initally")

  :md/bind ((:map (splitscreen/prefix)
                  ("c" . eyebrowse-create-window-config)
                  ("n" . eyebrowse-next-window-config)
                  ("p" . eyebrowse-prev-window-config)
                  ("X" . eyebrowse-close-window-config)
                  ("o" . splitscreen/toggle-zoom)))
  :config

  ;; Add advice to handle my zoom feature with eyebrose
  (advice-add 'eyebrowse-close-window-config :around 'splitscreen/reset-zoom '((name . "splitscreen")))

  ;; Add advice to show status in echo area when I change eyebrowse state
  (advice-add 'eyebrowse-next-window-config :around 'md/eyebrowse-status '((name . "md/eyebrowse-status")))
  (advice-add 'eyebrowse-prev-window-config :around 'md/eyebrowse-status '((name . "md/eyebrowse-status")))
  (advice-add 'eyebrowse-close-window-config :around 'md/eyebrowse-status '((name . "md/eyebrowse-status")))
  (advice-add 'eyebrowse-create-window-config :around 'md/eyebrowse-status '((name . "md/eyebrowse-status")))

  (eyebrowse-mode 1))

(use-package vterm
  :demand t
  :after (evil)
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  :hook ((vterm-mode . evil-emacs-state))
  :md/bind ((:map (vterm-mode-map)
                  ("C-<SPC>" . md/leader-map)
                  ("C-w" . splitscreen/prefix)
                  ("C-g" . vterm--self-insert))
            (:map (vterm-mode-map . normal)
                  ("gk" . vterm-previous-prompt)
                  ("gj" . vterm-next-prompt)))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm [%s]"))

(use-package elfeed
  :after (evil)

  :init
  (defun md/elfeed-search-toggle-unread ()
    (interactive)
    (elfeed-search-toggle-all 'unread))

  (defun md/elfeed-search-toggle-hide ()
    (interactive)
    (elfeed-search-toggle-all 'hidden))

  (defun md/elfeed-search-toggle-starred ()
    (interactive)
    (elfeed-search-toggle-all 'starred))

  :config
  ;; Automatically remove the "unread" flag from anything older than 1 month
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "1 month ago"
                                :remove 'unread))

  ;; Default to emacs state so I can override bindings
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (md/make-keymap-noop elfeed-search-mode-map)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (md/make-keymap-noop elfeed-show-mode-map)

  :custom
  (elfeed-search-title-max-width 110 "Truncate title width")
  (elfeed-search-filter "@2-weeks-ago -hidden !ojwtech" "Default search filter")

  :md/bind ((:map (md/leader-map)
                  ("R" . elfeed))

            (:map (elfeed-search-mode-map)
                  ("SPC" . md/leader-map)
                  ("C-h" . help-mode-map)
                  ("q" . elfeed-search-quit-window)
                  ("j" . evil-next-visual-line)
                  ("n" . evil-next-visual-line)
                  ("C-n" . evil-next-visual-line)
                  ("k" . evil-previous-visual-line)
                  ("p" . evil-previous-visual-line)
                  ("C-p" . evil-previous-visual-line)
                  ("C-f" . evil-scroll-page-down)
                  ("C-b" . evil-scroll-page-up)
                  ("C-d" . evil-scroll-down)
                  ("l" . elfeed-search-show-entry)
                  ("o" . elfeed-search-browse-url)
                  ("r" . elfeed-search-update--force)
                  ("R" . elfeed-search-fetch)
                  ("|" . elfeed-search-clear-filter)
                  ("/" . elfeed-search-set-filter)
                  ("g" . elfeed-search-first-entry)
                  ("G" . elfeed-search-last-entry)
                  ("u" . md/elfeed-search-toggle-unread)
                  ("H" . md/elfeed-search-toggle-hide)
                  ("s" . md/elfeed-search-toggle-starred))

            (:map (elfeed-show-mode-map)
                  ("h" . elfeed-kill-buffer)
                  ("q" . elfeed-kill-buffer)
                  ("SPC" . md/leader-map)
                  ("C-h" . help-mode-map)
                  ("n" . evil-next-visual-line)
                  ("C-n" . evil-next-visual-line)
                  ("C-p" . evil-previous-visual-line)
                  ("C-f" . evil-scroll-page-down)
                  ("C-b" . evil-scroll-page-up)
                  ("C-d" . evil-scroll-down)
                  ("o" . elfeed-show-visit)
                  ("l" . elfeed-show-next-link)
                  ("j" . evil-next-visual-line)
                  ("k" . evil-previous-visual-line)
                  ("w" . evil-forward-word-begin)
                  ("b" . evil-backward-word-begin))))

(use-package fic-mode
  :hook ((prog-mode . fic-mode))
  :custom
  (fic-highlighted-words '("TODO" "FIX" "FIXME" "BUG" "WARN" "NOTE" "WARNING" "HACK" "NOTE" "ERROR" "MATT" "DEPRECATED" "BREAKPOINT") "")
  (fic-activated-faces '(font-lock-doc-face font-lock-comment-face)))

(use-package gruvbox-theme
  :straight (gruvbox-theme :type git :host github :repo "greduan/emacs-theme-gruvbox"
                           :fork (:host github
                                        :repo "mattduck/emacs-theme-gruvbox"))
  :config
  (md/disable-all-themes)
  (load-theme 'gruvbox-dark-hard t))

(use-package go-mode
  :demand t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package graphql-mode)

(use-package git-commit
  :demand t
  :config
  (defun md/git-commit-setup ()
    "Basic setup for git-commit mode, eg. to set a fill column"
    (interactive)
    (setq fill-column 70)
    (display-fill-column-indicator-mode)
    (evil-normal-state))

  (add-hook 'git-commit-setup-hook 'md/git-commit-setup)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

  ;; Remove some default hooks that I don't use
  (remove-hook 'git-commit-setup-hook 'git-commit-save-message)
  (remove-hook 'git-commit-setup-hook 'git-commit-setup-changelog-support)
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  (remove-hook 'git-commit-setup-hook 'git-commit-propertize-diff)

  (global-git-commit-mode 1))

(use-package git-gutter
  :demand t
  :init
  (defun md/maybe-git-gutter-mode ()
    "Unless file is too big, enter git-gutter mode (when in git dir)"
    (interactive)
    (when (and (< (count-lines (point-min) (point-max)) 1500)
               (not (eq major-mode 'org-mode)))
      (if (string= "git" (downcase (format "%s" (vc-backend
                                                 (buffer-file-name
                                                  (current-buffer))))))
          (git-gutter-mode 1))))
  (add-hook 'find-file-hook 'md/maybe-git-gutter-mode)

  :custom
  (git-gutter:ask-p nil "Don't ask for confirmation of gadd")
  (git-gutter:modified-sign "~" "Use format I initially got used to in the vim version")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:unchanged-sign " ")
  (git-gutter:always-show-separator t)
  (git-gutter:separator-sign " " "Ensure there's some space between the gutter column and the code")

  :config

  (defun md/git-file-history ()
    "Show a consult list of the git log history for the current file.
On preview/action, open the file contents in a readonly buffer, and use
git-gutter to show which lines were added/changed/removed."
    (interactive)
    (when (and buffer-file-name (vc-root-dir))
      (let* ((file (buffer-file-name))
             (original-mode major-mode)
             (original-point (point))
             (git-root (vc-root-dir))
             (relative-file (file-relative-name file git-root))
             ;; Update log-cmd to include commit hash, date, relative date, author, and message
             (log-cmd (format "git log --pretty=format:'%%h %%ad (%%ar) | %%an | %%s' --date=iso -- %s"
                              (shell-quote-argument file)))
             (log-entries (split-string (shell-command-to-string log-cmd) "\n" t))
             ;; Construct list of (display-string . commit-hash) pairs
             (colored-entries (mapcar (lambda (entry)
                                        (let* ((parts (split-string entry "\\s-+|\\s-+"))
                                               (hash-and-date (nth 0 parts))
                                               (author (nth 1 parts))
                                               (message (string-trim (nth 2 parts)))
                                               ;; Further split the hash-and-date part
                                               (hash (car (split-string hash-and-date)))
                                               (date-part (string-join (cdr (split-string hash-and-date)) " ")))
                                          ;; Construct display string and store commit hash in pair
                                          (cons (concat (propertize hash 'face 'font-lock-builtin-face)
                                                        " "
                                                        (propertize date-part 'face 'font-lock-string-face)
                                                        " | "
                                                        (propertize author 'face 'font-lock-keyword-face)
                                                        " | "
                                                        (propertize message 'face 'font-lock-variable-name-face))
                                                hash)))
                                      log-entries)))

        (consult--read (mapcar #'car colored-entries)
                       :prompt "Select commit: "
                       :sort nil
                       :require-match t
                       ;; The :state function handles both preview and final selection
                       :state (lambda (action chosen-entry)
                                (when chosen-entry
                                  (let ((commit-hash (cdr (assoc chosen-entry colored-entries))))
                                    (md/do-git-file-history commit-hash relative-file original-mode action original-point))))))))

  (defun md/do-git-file-history (commit-hash relative-file original-mode action original-point)
    "Open or preview the file content for COMMIT-HASH using RELATIVE-FILE and ORIGINAL-MODE.
Restores the cursor as close as possible to the ORIGINAL-POINT."
    (let* ((buffer-name
            (if (eq action 'preview)
                "*md/git-timemachine-preview*"
              (format "*md/git-timemachine %s@%s*" relative-file commit-hash)))
           (git-root (locate-dominating-file relative-file ".git"))
           (original-line-content (thing-at-point 'line t)))
      (with-current-buffer (get-buffer-create buffer-name)
        (setq-local default-directory git-root)
        (setq buffer-read-only nil)
        (erase-buffer)
        ;; Fetch and insert the file content for the given commit
        (message default-directory)
        (insert (shell-command-to-string
                 (format "git --no-pager show %s:%s" commit-hash (shell-quote-argument relative-file))))
        ;; Apply the original major mode
        (funcall original-mode)
        (setq buffer-read-only t)
        ;; Restore the cursor position (or as close as possible)
        (pop-to-buffer (current-buffer))
        (goto-char (min original-point (point-max)))
        (recenter)
        ;; Naive attempt to look at the same place in the file: try to match the
        ;; current line. Won't work when there are duplicates.
        (if (search-forward original-line-content nil t)
            (recenter)
          (if (search-backward original-line-content nil t)
              (recenter)))

        ;; Git-gutter stuff
        (let ((inhibit-read-only t)
              (diff-results nil))
          (with-temp-buffer
            ;; Run git diff command and insert output into the temp buffer
            (setq-local default-directory git-root)
            (process-file "git" nil (current-buffer) nil
                          "--no-pager" "-c" "diff.autorefreshindex=0"
                          "diff" "--no-color" "--no-ext-diff" "-U0"
                          (format "%s^" commit-hash) commit-hash "--" relative-file)
            (setq diff-results (git-gutter:process-diff-output (current-buffer))))

          ;; Use the diff result to apply git-gutter
          (git-gutter:update-diffinfo diff-results)
          (git-gutter)))))

  (defvar md/git-gutter-auto-diff-last-line nil
    "Stores the last line number to detect line changes.")

  (defun md/git-gutter-popup-hunk-on-line-change ()
    "Check if the current line has changed, and if so, call `git-gutter:popup-hunk`."
    (let ((current-line (line-number-at-pos)))
      ;; Check diffinfos rather than git-gutter-mode, as this way it catches my history function
      (when (and (bound-and-true-p git-gutter:diffinfos)
                 (not (equal md/git-gutter-auto-diff-last-line current-line))) ;; Check if the line has changed
        (setq md/git-gutter-auto-diff-last-line current-line)  ;; Update the last line tracked
        (condition-case nil
            (git-gutter:popup-hunk)
          (error
           (quit-windows-on git-gutter:popup-buffer)
           ))
        )))

;;;###autoload
  (define-minor-mode md/git-gutter-auto-diff-mode
    "A minor mode to automatically show Git hunk diffs when moving the cursor."
    :global nil
    (if md/git-gutter-auto-diff-mode
        (progn
          (add-hook 'post-command-hook #'md/git-gutter-popup-hunk-on-line-change nil t)
          (md/git-gutter-popup-hunk-on-line-change))
      (progn
        (quit-windows-on git-gutter:popup-buffer)
        (remove-hook 'post-command-hook #'md/git-gutter-popup-hunk-on-line-change t))))


  :md/bind ((:map (md/leader-map)
                  ("g <RET>" . git-gutter-mode)
                  ("gh" . md/git-file-history)
                  ("ga" . md/git-gutter-auto-diff-mode)
                  ("gk" . git-gutter:previous-hunk)
                  ("gp" . git-gutter:previous-hunk)
                  ("gj" . git-gutter:next-hunk)
                  ("gn" . git-gutter:next-hunk)
                  ("g+" . git-gutter:stage-hunk)
                  ("g-" . git-gutter:revert-hunk))))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config
  (evil-set-initial-state 'chatgpt-shell-mode 'emacs)
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     ;; NOTE: if this isn't working remember that auth-source-do-cache controls auth-source caching
     (auth-source-pick-first-password :host "api.openai.com"))
   "API credentials")
  (chatgpt-shell-model-version "gpt-4")
  :md/bind ((:map (md/leader-map)
               ("G" . chatgpt-shell))
         (:map (chatgpt-shell-mode-map . normal)
               ("q" . quit-window))))

(use-package treemacs
  :demand t
  :init
  (defun md/treemacs-mode-hook ()
    ;; Treemacs seems to use this by default, and I can't see a way to disable it via variables.
    (hl-line-mode -1)
    ;; Don't follow current buffers
    (treemacs-follow-mode -1))

  :hook ((treemacs-mode . md/treemacs-mode-hook))

  :custom
  (treemacs-no-png-images t "Don't use icons, too noisy")
  (treemacs-indentation 1 "Tree can get deep so use minimum indent")
  (treemacs-space-between-root-nodes nil "Don't show newlines between projects")

  :config
  ;; Override the fallback icons. I ignore the :icon value here as I'm not using png icons.
  (treemacs-modify-theme "Default"
    :config
    (progn
      (treemacs-create-icon :icon "" :fallback "  " :extensions '(tag-leaf))
      (treemacs-create-icon :icon "" :fallback "  " :extensions '(tag-open))
      (treemacs-create-icon :icon "" :fallback "+ " :extensions '(tag-closed))))

  (defun md/treemacs ()
    "Setup some variables before running treemacs"
    (interactive)
    ;; Reduce the margin width to make better use of space.
    (let ((left-margin-width 0))
      (call-interactively 'treemacs)))

  (defun md/treemacs-current-project ()
    (interactive)
    (call-interactively #'treemacs-add-and-display-current-project)
    (call-interactively #'treemacs-collapse-other-projects))

  :md/bind ((:map (md/leader-map)
                  ("T" . md/treemacs)
                  ("jT" . md/treemacs-current-project))
            (:map (treemacs-mode-map . normal)
                  ("l" . treemacs-TAB-action)
                  ("h" . treemacs-TAB-action)
                  ("r" . treemacs-refresh)
                  ("M-k" . treemacs-move-project-up)
                  ("M-j" . treemacs-move-project-down))))

(use-package magit
  :config
  (add-hook 'magit-blame-mode-hook 'evil-normal-state)
  :md/bind ((:map (md/leader-map)
                  ("gb" . magit-blame))
            (:map (magit-blame-mode-map . normal)
                  ("RET" . magit-show-commit)
                  ("q" . magit-blame-quit)
                  ("gn" . magit-blame-next-chunk)
                  ("gk" . magit-blame-previous-chunk))))

(use-package eglot
  :straight nil ;; Use the builtin version, don't download
  :config
  ;; eldoc and flymake make too much noise -- disable by default.
  ;;
  ;; For imenu, the languages I'm initially using with eglot are using have imenu
  ;; functions provided by treesit, which use labels like *class definition* and
  ;; *function definition* to show the imenu results as a tree in the same order
  ;; defined in the file. Eglot seems to instead break it into symbol types,
  ;; which is much less useful. So we stick with the treesit implementation for now,
  ;; and just use eglot for project-wide search etc.
  ;;
  ;; For some reason this doesn't get picked up setting via :custom, so we do it
  ;; with setq.
  (setq eglot-stay-out-of '(eldoc flymake imenu))

  ;; Changes supposed to help with performance
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-size 0)

  :custom
  (eglot-report-progress nil "Eglot spams the minibuffer a lot on save -- this seems to keep it quieter")

  :md/bind ((:map (md/leader-map)
                  ("ll" . eglot)
                  ("lL" . eglot-shutdown))))

(use-package consult-eglot
  :after (consult eglot)
  :config

  (defun md/consult-eglot-xref-dwim ()
    "If eglot is enabled and managing xref, consult-eglot-symbols is
similar to xref-find-apropos, because they both call :workspace/symbol on the
language server. The main difference is that consult-eglot-symbols groups and
lets you filter by symbol type, which can be useful if there are lots of
matches. Xref-find-apropos groups by file and doesn't show the type of the
symbol. I don't want to have to think about using both functions, so if
consult-eglot-symbols works, we'll use that, otherwise we fall back to
xref-find-apropos.

This should catch cases both where eglot isn't running for a buffer, and also
where the language server doesn't support the workspace/symbol call, in which
case if another xref backend is available that will be used instead.

If the eglot xref backend gets updated to provide more symbol-type-aware
features, then I can get rid of this and just use xref."
    (interactive)
     (condition-case nil
         (call-interactively #'consult-eglot-symbols)
       (error
        (call-interactively #'xref-find-apropos))))

  :md/bind ((:map (md/leader-map)
              ("j ." . md/consult-eglot-xref-dwim))))

(use-package treesit
  :straight nil
  :config
  (defun md/treesit-install-all-languages ()
    "From https://www.masteringemacs.org/article/how-to-get-started-tree-sitter"
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

  :custom
  (treesit-language-source-alist
   '((python "https://github.com/tree-sitter/tree-sitter-python"))
   "The treesitter grammars to use")

  (major-mode-remap-alist
   '((python-mode . python-ts-mode))
   "Introduced in Emacs 29, presumably to support treesitter")

  (treesit-font-lock-level 4))

(use-package rainbow-mode
  :hook
  ((css-mode . rainbow-mode)
   (help-mode . rainbow-mode)
   (html-mode . rainbow-mode))
  :bind (:map md/leader-map
              ("tr" . rainbow-mode)))

(use-package terraform-mode)

(use-package python
  :demand t
  :config
  (defun md/python-imenu-format-item-label (type name)
    "Instead of the default format of eg. `my_function (def)`, use `def:
myfunction`. This makes it easier to read."
    (format "%s: %s" type name))
  :custom
  (python-imenu-format-item-label-function #'md/python-imenu-format-item-label)
  (python-imenu-format-parent-item-label-function #'md/python-imenu-format-item-label))

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode))
  (("\\.djhtml\\'" . web-mode))
  :custom
  (web-mode-enable-engine-detection t))

(use-package diff-mode
  :md/bind ((:map (diff-mode-map . normal)
                  ("q" . quit-window))))

(use-package server
  :config (when (not (server-running-p))
            (server-start)))

(use-package emacs
  :init
  (defconst md/dotfiles-init-local-path "~/.local.el")
  (md/maybe-native-compile-and-load md/dotfiles-init-local-path t)
  (defun md/dotfiles-edit-init-local ()
    (interactive)
    (find-file md/dotfiles-init-local-path))
  :md/bind ((:map (md/leader-map)
                  ("vl" . md/dotfiles-edit-init-local))))

(message "end of init.el")
