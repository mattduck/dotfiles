(message "start of early-init.el")

(defun md/dotfiles-get-path (path)
  "Lookup files that are in my dotfiles directory"
  (concat
   (or (getenv "DOTFILES")
   "/f/dotfiles")
   "/"
   path))

(defun md/emacs-get-path (path)
  "Lookup files that are in the emacs user directory"
  (concat user-emacs-directory path))

(defun md/dotfiles-compile ()
  "Use org-babel-tangle to create init.el and byte-compile it."
  (interactive)
  (find-file (md/emacs-get-path "init.org"))
  (setq-local org-confirm-babel-evaluate nil)
  (org-babel-tangle nil "init.el")
  (byte-compile-file (md/emacs-get-path "init.el")))
  ;; [2023-08-18] Disabling, have some issue with emacs not finding packages when init.elc exists
  ;; (when (fboundp 'native-compile-async)
  ;;   (native-compile-async (md/emacs-get-path "init.el"))))

  (defun md/dotfiles-compile-early ()
  "Use org-babel-tangle to create early-init.el and byte-compile it."
  (interactive)
  (find-file (md/emacs-get-path "early-init.org"))
  (setq-local org-confirm-babel-evaluate nil)
  (org-babel-tangle nil "early-init.el")
  (byte-compile-file (md/emacs-get-path "early-init.el"))
  (when (fboundp 'native-compile-async)
    (native-compile-async (md/emacs-get-path "early-init.el"))))

  (defun md/dotfiles-compile-all ()
    (interactive)
    (md/dotfiles-compile-early)
    (md/dotfiles-compile))

(setq package-enable-at-startup nil)

(defun md/exwm-file-enable ()
  "If this file exists, my .xinitrc will load exwm instead of i3."
  (interactive)
  (f-touch "~/.exwm-enabled")
  (message "exwm file touched"))

(defun md/exwm-file-disable ()
  "If this file exists, my .xinitrc will load exwm instead of i3.
Calling this will delete the file, causing i3 to load next time."
  (interactive)
  (delete-file "~/.exwm-enabled")
  (message "exwm file deleted"))

(defun md/exwm-enabled ()
  (file-exists-p "~/.exwm-enabled"))

(defun md/set-default-font ()
	(interactive)
	(set-face-attribute 'default nil
					:height 180
					:family "Inconsolata")
	(setq-default line-spacing 0.2)
	(run-hooks 'after-setting-font-hook 'after-setting-font-hooks))

(md/set-default-font)

;; Want this to apply to first buffer - a very basic modeline
(setq-default mode-line-format "    %b")

(message "end of early-init.el")
