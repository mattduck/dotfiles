(message "start of init.el")

(defun md/dotfiles-get-path (path)
  "Lookup files that are in my dotfiles directory"
  (concat
   (or (getenv "DOTFILES")
       (concat (expand-file-name "~") "/f/dotfiles"))
   "/"
   path))

(defun md/dotfiles-compile ()
  "Use org-babel-tangle to create init.el and byte-compile it."
  (interactive)
  (find-file (md/dotfiles-get-path "emacs.d.symlink/init.org"))
  (setq-local org-confirm-babel-evaluate nil)
  (org-babel-tangle nil "init.el")
  (byte-compile-file (md/dotfiles-get-path "emacs.d.symlink/init.el"))
  (when (fboundp 'native-compile-async)
    (native-compile-async (md/dotfiles-get-path "emacs.d.symlink/init.el"))))

(require 'package)
;; Load packages. It's necessary to call this early.
(package-initialize)

;; The archives that package-list-packages and package-install will use.
;; The default only includes elpa.gnu.org, but a lot of my installed packages
;; come from MELPA.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq load-prefer-newer nil)
(require 'jka-compr)
(setq load-prefer-newer t)

(when (boundp 'comp-deferred-compilation-deny-list)
  (setq comp-deferred-compilation-deny-list '("powerline")))

(defun md/maybe-native-compile-and-load (path loadp)
  (if (and path (file-exists-p path))
      (progn
        (when loadp
          (load-file path))
        (when (fboundp 'native-compile-async)
          (native-compile-async path t)))
    (message "Cannot load-file, doesn't exist: %s" path)))

;; This is the custom file I used to use. I'm keeping the same path.
(setq custom-file (md/dotfiles-get-path "emacs.d.symlink/custom.el"))

;; This just marks some themes as safe. At some point I copy/pasted it from the custom file.
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("a2f27a9cf29c4148a6ade6f780250a62f9b6b2e460ead2df30cf4916135fa514" "e6c57d23ebe550228ff6a0b640c045244312e87bb69a7941f7909625d105eae9" "f6e6ef708c51a7ba82be632c142126e92206ab0ef40b50817bb32ce297bc0983" "29242a0f9e6987aa972e108a80413878d3c431dd8e6c69b9d824273b88f0c63f" "dd94b107dd63cde4eadc558550379f8bfb9e773d780ca3ff305d7ccd17049034" "579b8afedbd64052f45d921ffe40a91c4f80c2c28a54864905a11e834d8f0f38" "089736e335b34fc7d1bb7f42c5f315b1bafd935abd0511c62edce549caab8ee1" "73438b705bb3d7583e9edb6fcf6dc6a86a7c6ce4b59500de0ee941547fc80d4c" "4c7121d2591363cabf74f57e656a698291d4cda52c7cd2da08a860364f761091" "72d07f0ef90edcc4a674f3ba6305c66feb2db8400122c9c0bc37b0097455e939" "7687ce37b6a86109edccc24989fbdb8e35dabefd718d6d70f3042daed5f90b1d" "f06afcb8fded3e57346ce01be80cde0cab09fd82ca11c8758612f5b4d996ea3d" "0e6a2147a71a363987a3071ed1b25c729e04b4355d80276ae8bb2438a4ad6250" "1e1a6ed4c050f07c088190483d7f6aa8277875fd49533ca747067348dc45b3c6" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "1f38fb71e55e5ec5f14a39d03ca7d7a416123d3f0847745c7bade053ca58f043" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "f8067b7d0dbffb29a79e0843797efabdf5e1cf326639874d8b407e9b034136a4" "f8fb7488faa7a70aee20b63560c36b3773bd0e4c56230a97297ad54ff8263930" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "304c39b190267e9b863c0cf9c989da76dcfbb0649cbcb89592e7c5c08348fce9" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "7559ac0083d1f08a46f65920303f970898a3d80f05905d01e81d49bb4c7f9e39" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "227fabfb7d3f2334d1352ef507b1494ab08e143b49237617e99bd6b44eef73d4" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "5057614f7e14de98bbc02200e2fe827ad897696bfd222d1bcab42ad8ff313e20" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "0daf22a3438a9c0998c777a771f23435c12a1d8844969a28f75820dd71ff64e1" "6d0a48fd812d94910f159c23880f0145c0d0324cc43c2cbf08a3907829711619" "c5a66e8d5c579bb8bc24bfde216c8eb8bdc8e42ec10286443a8369e5ea58dc6c" "10409f46959ade6f875dcbd42443460aab5f071fdd4865475e7002a550636e82" "107523dfa441684e91fcf479c9c4a6556bd6201cbe631cba8f7b5356fce5bfc5" "9728f07d59a9192baeeb4f2ac6ddb0b469dd6a4489818a98516c5d482fe9eba4" "28cd2fd9da1f5d1d023dc53008290070c5b89625de147ae7eb91987f30985937" "e08b999f30ebb71bdb447dcb53ad59769bc2a60a6728eef69f6399d064b1da4c" "d79c093e1ea2482c67acb2e03b49645010291259d623b5e34e4dc1b8efd6470d" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))))

(setq use-package-always-ensure nil
      use-package-verbose t
      use-package-minimum-reported-time 0.0001)


;; [2021-05-02] requiring this on load rather than compile due to https://github.com/jwiegley/use-package/issues/737
(eval-when-compile
  (require 'use-package))
(require 'use-package)

(require 'bind-key)  ; Required for :bind in use-package

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq frame-resize-pixelwise t)
(set-frame-parameter nil 'ns-transparent-titlebar t)

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
 (setq exec-path-from-shell-variables
       '("BROWSER"
         "CI"
         "DB_HOST"
         "DB_NAME"
         "DB_PASSWORD"
         "DB_PORT"
         "DB_USER"
         "DDT"
         "DJANGO_SETTINGS_MODULE"
         "GO15VENDOREXPERIMENT"
         "GOPATH"
         "LANG"
         "LAUNCH_DARKLY_SDK_KEY"
         "LOGFORMAT"
         "LOGLEVEL"
         "MANPATH"
         "PATH"
         "PGPASSWORD"
         "PYTHONPATH"
         "REDIS_URI"
         "SILK"
         "TMUX"))
 (exec-path-from-shell-initialize))

(define-prefix-command 'md/leader-map)

(defvar md/org-mode-leader-map (make-sparse-keymap))
(set-keymap-parent md/org-mode-leader-map md/leader-map)

(setq inhibit-splash-screen t)

(setq-default fill-column 80)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when (or (not (display-graphic-p))
          (string= (system-name) "arch"))
  (menu-bar-mode -1))

(defun contextual-menubar (&optional frame)
    (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(add-hook 'after-make-frame-functions 'contextual-menubar)
(add-hook 'after-init-hook 'contextual-menubar)

(defun md/fontify-if-font-lock-mode ()
  (when font-lock-mode
    (font-lock-ensure)))

(add-hook 'after-save-hook 'md/fontify-if-font-lock-mode)

(bind-key "tx" 'font-lock-mode md/leader-map)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defvar md/font-size 125)

(defun md/font-size-incr ()
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         5)))

(defun md/font-size-decr ()
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

(defun md/set-default-font ()
  (interactive)
  (set-face-attribute 'default nil
                      :height md/font-size
                      ;; :family "Inconsolata-dz for Powerline")
                      :family "Inconsolata")
  (setq-default line-spacing 0.2)
  (run-hooks 'after-setting-font-hook 'after-setting-font-hooks))


;;   (interactive)
;;   (cond
;;         ((s-starts-with-p "arch" (system-name))
;;          (set-frame-font
;;             (format "Inconsolata-%s:antialias=subpixel" md/font-size) t t))
;;         ((s-starts-with-p "OMETRIA" (system-name))
;;          (set-frame-font
;;           (format "Inconsolata for Powerline-%s:antialias=subpixel" md/font-size) t t))
;;         (t
;;          (set-frame-font (format "Roboto Mono Light for Powerline-%s:antialias=subpixel" md/font-size) t t))))

;; ;; TODO add bindings for buffer-only, copying C-x C-+
(bind-key "+" 'md/font-size-incr md/leader-map)
(bind-key "-" 'md/font-size-decr md/leader-map)

;; Fix for issue where italic fonts don't actually use the italic property.
(set-face-attribute 'italic nil :slant 'italic :underline nil)


;; Try to use Apple Color Emoji to display emojis
(when (memq window-system '(mac ns))
  (set-fontset-font "fontset-default" 'symbol "Apple Color Emoji" nil 'prepend))

(use-package s :demand t)
;; (add-hook 'focus-in-hook 'md/set-default-font)
(md/set-default-font)

(setq
  ;; Start scrolling when the cursor is one line away from the top/bottom.
  scroll-margin 1

  ;; If at the bottom of the file, don't allow scroll beyond that (because
  ;; there's no use in having half a screen of empty space
  scroll-conservatively 999

  ;; Only scroll one row at a time. Default behaviour is to centre the row.
  scroll-step 1

  scroll-preserve-screen-position 1
  )

;; Remove scrollbars (GUI only) to get extra screen space
(use-package scroll-bar
  :if (display-graphic-p)
  :demand t
  :config (scroll-bar-mode -1))

(defun md/left-margin ()
  (setq left-margin-width 4))

(add-hook 'prog-mode-hook 'md/left-margin)
(add-hook 'org-mode-hook 'md/left-margin)

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

(blink-cursor-mode 0)

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
  :demand t
  :config
  (progn
    (xclip-mode 1)))

(if (eq window-system 'ns)
  (global-set-key (kbd "M-v") 'evil-paste-after))

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

(setq gc-cons-threshold 100000000
      garbage-collection-messages t)

(defun md/save-if-not-remote ()
  (interactive)
  (if (not (file-remote-p default-directory))
      (save-buffer)))

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
   "%s | %s lines | line %d:%d %3d%% | %s | %s"
   (buffer-file-name)
   (count-lines (point-min) (point-max)) ; total
   (count-lines (point-min) (point)) ; current
   (current-column) ; column
   (/ (window-end) 0.01 (point-max)) ; line %
   (or projectile-project-name "[no project]")
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
    (when
        (and filename
             (string= (read-string (format "Delete %s? (y/n) " filename)) "y"))
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun md/kill-buffer-and-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))

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


(defun md/align-two-spaces ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)  " 1 1 t))

(defun md/unfill-paragraph ()
  "Because I can't always wrap to 80 characters :("
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun md/unfill-region (start end)
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(require 'ht)
(defvar md/variable-layers (ht-create)
  "A hashmap of (scope -> layer), where each layer is a hashmap of symbols
to arbitrary values. Stores the state for md/toggle-variable-layer")

(defun md/toggle-variable-layer (scope var-alist &optional enable-fn disable-fn)
  "Apply and restore a given list of variables.

VAR-ALIST is an alist of (symbol . value), where symbol is a
variable that exists in global scope.

When called the first time, apply the given VAR-ALIST (the
equivalent of calling setq on each list item). When called a
second time, restore the values for each symbol that were present
/before/ the first invocation.

If supplied, ENABLE-FN and DISABLE-FN are respectively called on enable/disable.

SCOPE is used to allow for multiple versions of
toggle-variable-layer to be used simultaneously.

If a variable is modified after a layer is applied, then the
restoration will ignore that symbol, and it will maintain its
modified value.

All scope layers are stored in md/variable-layers."
  (interactive)
  (if (ht-get md/variable-layers scope)
      ;; Layer is already applied: restore the old values
      (progn
        (let ((oldlayer (ht-get md/variable-layers scope)))
          (-map (lambda (item)
                  ;; If current val == the val we passed in, then assume that the symbol hasn't been
                  ;; modified, and is therefore save to restore to its original value.
                  (when (equal (symbol-value (car item)) (cdr item))
                    (set (car item) (ht-get oldlayer (car item)))))
                var-alist))
        (ht-remove md/variable-layers scope)  ; Remove this layer's state
        (when disable-fn (funcall disable-fn))
        (message "Variable layer disabled: %s" scope))
    ;; Layer is not applied: apply the given values and store the originals
    (let ((newlayer (ht-create)))  ;; nested dict
      (-map (lambda (item)
                 (ht-set newlayer (car item) (symbol-value (car item)))  ; Save the current value
                 (set (car item) (cdr item)))  ; Set the new value
                 var-alist)
      (ht-set md/variable-layers scope newlayer)  ; Store the layer in md/variable-layers
      (when enable-fn (funcall enable-fn))
      (message "Variable layer enabled: %s" scope))))

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
  (setq recentf-max-saved-items 200)
  (setq compilation-read-command nil) ; Don't prompt me to run make

  ;; This is handy - instead of popping up a separate GPG UI prompt,
  ;; using loopback mode will allow Emacs to prompt in the minibuffer.
  (setq epa-pinentry-mode 'loopback)

  (line-number-mode 0)
  (column-number-mode 0)

(when (memq window-system '(mac ns))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(use-package evil
 :demand t
 :config
 (progn
   (defun md/normal-state-and-save ()
     (interactive)
     (evil-normal-state)
     (md/save-if-not-remote))

   (defun md/insert-blank-line-before ()
     (interactive)
     (save-excursion
       (end-of-line)
       (open-line 1)
       (md/save-if-not-remote)))

   (defun md/insert-blank-line-after ()
     (interactive)
     (save-excursion
       (evil-previous-visual-line)
       (end-of-line)
       (open-line 1)
       (md/save-if-not-remote)))

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

        ;; This is useful in linux when no cmd+v
        :map evil-insert-state-map
        ("C-v" . evil-paste-after)

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
        ("k" . kill-buffer)
        ("K" . md/kill-buffer-and-frame)
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

(use-package key-chord
  :config
  (progn
    (setq key-chord-two-keys-delay 0.4)

    (key-chord-define evil-insert-state-map "jj" 'md/normal-state-and-save)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-replace-state-map "jj" 'md/normal-state-and-save)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
    (key-chord-mode 1)))

;;;; Below are configurations for EXWM.

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

(when (file-exists-p "~/.exwm-enabled")
  ;; Required for sane bindings
  (evil-set-initial-state 'exwm-mode 'emacs)

  ;; Load EXWM.
  (require 'exwm)

  ;; Fix problems with Ido (if you use it).
  ;; (require 'exwm-config)
  ;; (exwm-config-ido)

  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 1)

  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  ;; Hide modeline for exwm buffers
  (add-hook 'exwm-manage-finish-hook 'exwm-layout-hide-mode-line)

  (setq exwm-workspace-minibuffer-position nil)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
                                        ;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
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


  (defun md/exwm-cycle ()
    (interactive)
    (exwm-workspace-switch
     (if (= exwm-workspace-current-index (- (exwm-workspace--count) 1))
         0
       (+ exwm-workspace-current-index 1))))

  (defun md/exwm-input-toggle-keyboard ()
    (interactive)
    (call-interactively 'exwm-input-toggle-keyboard)
    (message "exwm: %s" exwm--input-mode))


  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (setq exwm-input-global-keys
        `(;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)

          ;; TODO: helm-mini and helm-m-x don't cause screen issues, which is interesting.
          ;; There are particular alfred sources that cause the frame problems.
          (,(kbd "s-<SPC>") . md/alfred-no-frame)
          (,(kbd "s-t") . md/alfred)
          (,(kbd "C-<SPC>") . md/leader-map)

          ([?\s-w] . splitscreen/prefix)
          (,(kbd "s-<tab>") . eyebrowse-next-window-config)
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-z] . md/exwm-input-toggle-keyboard)))

  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ;; ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ;; ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])))
          ;; ([?\M-v] . [prior])
          ;; ([?\C-v] . [next])
          ;; ([?\C-d] . [delete])
          ;; ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ;; ([?\C-w] . [?\C-x])
          ;; ([?\M-w] . [?\C-c])
          ;; ([?\C-y] . [?\C-v])
          ;; search
          ;; ([?\C-s] . [?\C-f])))


  ;; Do not forget to enable EXWM. It will start by itself when things are
  ;; ready.  You can put it _anywhere_ in your configuration.
  (exwm-enable)

  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line.
                                        ;(setq exwm-workspace-minibuffer-position 'bottom)
  (require 'exwm-randr)
  ;; (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;           (lambda ()
  ;;             (start-process-shell-command
  ;;              "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))


  (defun md/exwm-display-one ()
    "If monitor is connected, only use that. Otherwise, only use the main display."
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
           "--output" (match-string 1) "--primary" "--auto" "--same-as" default-output
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

  ;; ---------------

  (defun md/status-message ()
    (interactive)
    (let* ((message-log-max nil) ; ensure not logged in message buffer
           (output (s-trim-right
                    (shell-command-to-string "/f/users/matt/.config/i3-status-bash-once.sh")))
           (output-as-list (car (read-from-string output)))
           (propertized-string (mapconcat
                                (lambda (item)
                                  (concat
                                   ;; (propertize " " 'face `(:family "Noto sans" :height 0.8))
                                   (propertize (nth 0 item)
                                               'face
                                               `(:foreground ,(nth 2 item) :family "Font Awesome 5 Free" :height 0.6))
                                   (propertize (nth 1 item)
                                               'face
                                               `(:foreground ,(nth 2 item) :family "Noto sans" :height 0.7))))
                                output-as-list "")))

      (message propertized-string)))
  (bind-key "SPC" 'md/status-message md/leader-map)

  (exwm-randr-enable))

(evil-set-initial-state 'help-mode 'normal)
(evil-define-key 'normal help-mode-map
  "q" 'quit-window
  (kbd "C-i") 'help-go-forward
  (kbd "C-o") 'help-go-back
  (kbd "<RET>") 'help-follow-symbol)

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

(defun md/quit-and-kill-window ()
  (interactive)
  (quit-window t))

(use-package helpful
  :demand t
  ;;:defer 1
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

;; (defun which-key--show-keymap (keymap-name keymap &optional prior-args)
;;   "This is identical to the version shipped with which-key, but it returns the
;; function captured by user input."
;;   (setq which-key--current-prefix nil
;;         which-key--current-show-keymap-name keymap-name
;;         which-key--using-show-keymap t)
;;   (when prior-args (push prior-args which-key--prior-show-keymap-args))
;;   (when (keymapp keymap)
;;     (let ((formatted-keys (which-key--get-formatted-key-bindings
;;                            (which-key--get-keymap-bindings keymap))))
;;       (cond ((= (length formatted-keys) 0)
;;              (message "which-key: Keymap empty"))
;;             ((listp which-key-side-window-location)
;;              (setq which-key--last-try-2-loc
;;                    (apply #'which-key--try-2-side-windows
;;                           formatted-keys 0 which-key-side-window-location)))
;;             (t (setq which-key--pages-plist
;;                      (which-key--create-pages formatted-keys))
;;                (which-key--show-page 0)))))
;;   (let* ((key (key-description (list (read-key))))
;;          (next-def (lookup-key keymap (kbd key))))
;;     (cond ((and which-key-use-C-h-commands (string= "C-h" key))
;;            (which-key-C-h-dispatch))
;;           ((keymapp next-def)
;;            (which-key--hide-popup-ignore-command)
;;            (setq next-def (which-key--show-keymap (concat keymap-name " " key) next-def
;;                                    (cons keymap-name keymap))))
;;           (t (which-key--hide-popup)))
;;     next-def))


(defun which-key--show-keymap
    (keymap-name keymap &optional prior-args all no-paging filter)
  "This is identical to the version shipped with which-key, but it returns the
  function captured by user input."
  (when prior-args (push prior-args which-key--prior-show-keymap-args))
  (let ((bindings (which-key--get-bindings nil keymap filter all)))
    (if (= (length bindings) 0)
        (message "which-key: No bindings found in %s" keymap-name)
      (cond ((listp which-key-side-window-location)
             (setq which-key--last-try-2-loc
                   (apply #'which-key--try-2-side-windows
                          bindings nil keymap-name
                          which-key-side-window-location)))
            (t (setq which-key--pages-obj
                     (which-key--create-pages bindings nil keymap-name))
               (which-key--show-page)))
      (unless no-paging
        (let* ((key (read-key))
               (key-desc (key-description (list key)))
               (next-def (lookup-key keymap (vector key))))
          (cond ((and which-key-use-C-h-commands
                      (numberp key) (= key help-char))
                 (which-key-C-h-dispatch))
                ((keymapp next-def)
                 (which-key--hide-popup-ignore-command)
                 (which-key--show-keymap
                  (concat keymap-name " " key-desc)
                  next-def
                  (cons keymap-name keymap)))
                (t (which-key--hide-popup)))
          next-def)))))

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
    :demand t
    ;;:defer 2
    :config (progn
              ;; Patch with my functions
              (md/which-key-patch)

              (setq which-key-idle-delay 1
                    which-key-max-description-length 30
                    which-key-allow-evil-operators nil
                    which-key-inhibit-regexps '("C-w" "s-w")
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

              )
    :bind (:map md/leader-map
                ("t <SPC>" . which-key-mode)))

(which-key-mode 0)

(use-package free-keys
  :demand t
  ;;:defer 10
  :config
    (progn
      (bind-key "@" 'free-keys help-map)))

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
  :demand t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-begin-commands '(self-insert-command org-self-insert-command)
        company-transformers '(company-sort-by-backend-importance)
        company-backends '((company-capf company-files company-keywords company-dabbrev-code))
        company-tooltip-minimum-width 40
        company-tooltip-width-grow-only t
        company-tooltip-offset-display 'scroll)

  ;; Bind here rather than in ":bind" to avoid complaints about
  ;; company-mode-map not existing.
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map)

  (bind-key "C-n" 'company-complete evil-insert-state-map)
  :hook ((emacs-lisp-mode . company-mode)
         (python-mode . company-mode)
         (web-mode . company-mode)
         (typescript-mode . company-mode)
         (restclient-mode . company-mode)
         (js-mode . company-mode)))

(use-package company-box
  :config (setq company-box-enable-icon t
                company-box-icon-right-margin 1
                company-box-color-icon t
                company-box-doc-enable t
                company-box-doc-delay 0.1
                company-box-icons-alist 'company-box-icons-all-the-icons
                company-box-icons-all-the-icons
                (let ((all-the-icons-scale-factor 1)
                      (all-the-icons-default-adjust 0))
                  `((Unknown       . ,(all-the-icons-faicon "question" :face 'all-the-icons-purple)) ;;question-circle is also good
                    (Text          . ,(all-the-icons-faicon "file-text-o" :face 'all-the-icons-green))
                    (Method        . ,(all-the-icons-faicon "cube" :face 'all-the-icons-dcyan))
                    (Function      . ,(all-the-icons-faicon "cube" :face 'all-the-icons-dcyan))
                    (Constructor   . ,(all-the-icons-faicon "cube" :face 'all-the-icons-dcyan))
                    (Field         . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (Variable      . ,(all-the-icons-faicon "tag" :face 'all-the-icons-dpurple))
                    (Class         . ,(all-the-icons-faicon "cog" :face 'all-the-icons-red))
                    (Interface     . ,(all-the-icons-faicon "cogs" :face 'all-the-icons-red))
                    (Module        . ,(all-the-icons-alltheicon "less" :face 'all-the-icons-red))
                    (Property      . ,(all-the-icons-faicon "wrench" :face 'all-the-icons-red))
                    (Unit          . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (Value         . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (Enum          . ,(all-the-icons-faicon "file-text-o" :face 'all-the-icons-red))
                    (Keyword       . ,(all-the-icons-material "format_align_center" :face 'all-the-icons-red))
                    (Snippet       . ,(all-the-icons-material "content_paste" :face 'all-the-icons-red))
                    (Color         . ,(all-the-icons-material "palette" :face 'all-the-icons-red))
                    (File          . ,(all-the-icons-faicon "file" :face 'all-the-icons-red))
                    (Reference     . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (Folder        . ,(all-the-icons-faicon "folder" :face 'all-the-icons-red))
                    (EnumMember    . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (Constant      . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (Struct        . ,(all-the-icons-faicon "cog" :face 'all-the-icons-red))
                    (Event         . ,(all-the-icons-faicon "bolt" :face 'all-the-icons-red))
                    (Operator      . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
                    (TypeParameter . ,(all-the-icons-faicon "cog" :face 'all-the-icons-red))
                    (Template      . ,(all-the-icons-faicon "bookmark" :face 'all-the-icons-dgreen)))))
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :demand t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode))
  :config
  (progn
    (defun md/flyspell-mode ()
      "Flyspell the buffer immediately after enabling flyspell mode"
      (interactive)
      (flyspell-mode)
      (when flyspell-mode
        (flyspell-buffer)))

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
              ("S <RET>" . md/flyspell-mode)
              ("SS" . flyspell-correct-word-before-point)))

(defun md/maybe-enable-flyspell ()
  (interactive)
  (when (< (count-lines (point-min) (point-max)) 5000)
    (flyspell-mode 1)))

(add-hook 'org-mode-hook 'md/maybe-enable-flyspell)

(setq compilation-mode-map (make-sparse-keymap))
(evil-set-initial-state 'compilation-mode 'normal)
(add-hook 'compliation-mode-hook 'evil-normal-state)
(evil-define-key 'normal compilation-mode-map "q" 'quit-window)

(use-package projectile
  :demand t
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
  :demand t
  :config
  (dumb-jump-mode 1)
  (setq dumb-jump-selector 'helm
        dumb-jump-force-searcher 'ag)
  (bind-key "gd" 'dumb-jump-go evil-normal-state-map))

(use-package ediff
 ;;:defer 1
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
 :demand t
 ;;:defer 1
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
         '("TODO" "FIX" "FIXME" "BUG" "WARN" "WARNING" "HACK" "NOTE" "ERROR" "MATT" "DEPRECATED" "BREAKPOINT"))

   ;; By default this includes font-lock-string-face, but I don't want strings to
   ;; have these words formatted.
   (setq fic-activated-faces '(font-lock-doc-face font-lock-comment-face))))

(defun md/insert-todo-regexp ()
  (interactive)
  (insert "TODO|FIX|FIXME|BUG|WARN|HACK|ERROR"))

(bind-key "th" 'highlight-phrase md/leader-map)
(bind-key "tu" 'unhighlight-regexp md/leader-map)

(use-package paren
 ;;:defer 1
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
  ;;:defer 1
  :config
  (progn
     (add-hook 'css-mode-hook 'rainbow-mode)
     (add-hook 'help-mode-hook 'rainbow-mode)
     (add-hook 'html-mode-hook 'rainbow-mode))
  :bind (:map md/leader-map
              ("tr" . rainbow-mode)))

(use-package eldoc ;; builtin
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 0.5
        eldoc-print-after-edit nil))

(use-package hideshow
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

(use-package vterm
  :demand t
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'vterm-mode-hook 'evil-emacs-state))

(use-package neotree
  :demand t
  :config
  (progn
    (evil-set-initial-state 'neotree-mode 'normal)
    (setq neo-theme 'nerd neo-smart-open t neo-show-hidden-files
          t)

    (bind-key "Nn" 'neotree-toggle md/leader-map)
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

(use-package bug-reference
  :hook ((prog-mod . bug-reference-prog-mode)
         (git-commit-mode . bug-reference-mode)))

(defun md/ide ()
  (interactive)
  (helm :sources
        (list
         (helm-build-sync-source "System"
           :multimatch nil
           :requires-pattern nil
           :candidates '(("Show docs" . lsp-describe-thing-at-point)
                         ;;("Show buffer symbols" . lsp-treemacs-symbols)
                         ("Find references" . lsp-find-references)
                         ;;("Find project symbol" . helm-lsp-workspace-symbol)
                         ;;("Goto defintion" . lsp-ui-peek-find-definitions)
                         ("Apropos" . xref-find-apropos)
                         ("Goto defintion" . lsp-find-definition)
                         ("Format buffer" . lsp-format-buffer)
                         ("Rename symbol" . lsp-rename)
                         ("Run debugger" . dap-debug))
           :action '(("Execute" . (lambda (candidate)
                                    (call-interactively candidate))))))
        :prompt ""))

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
(use-package edebug-x :demand t)

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

;; [2020-05-17 Sun] Disabling to prevent (lsp--auto-configure) from calling (lsp-ui-mode)
(use-package lsp-ui
  :disabled)

(use-package lsp-mode
  :demand t
  :config
  (defun md/lsp-setup()
    ;; recommended by LSP docs for performance
    (setq read-process-output-max (* 1024 1024)) ;; 1mb

    (lsp-enable-imenu)
    (setq
          lsp-auto-configure t
          lsp-enable-dap-auto-configure nil ; Don't try to auto-enable dap: this creates a lot of binding clashes
          lsp-auto-guess-root t ; Uses projectile to guess the project root.
          lsp-before-save-edits t
          lsp-eldoc-enable-hover t
          lsp-eldoc-render-all nil
          lsp-completion-enable t
          lsp-completion-show-detail t
          lsp-completion-show-kind t
          lsp-enable-file-watchers t
          lsp-enable-folding t
          lsp-enable-imenu t
          lsp-enable-indentation t
          lsp-enable-links t
          lsp-clients-python-library-directories `("/usr/" ,(expand-file-name "~/.virtualenvs")) ; This seems appropriate
          lsp-enable-on-type-formatting nil
          lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
          lsp-enable-symbol-highlighting nil
          lsp-enable-text-document-color nil
          lsp-enable-xref t
          lsp-flycheck-live-reporting nil
          lsp-idle-delay 0.5
          lsp-imenu-show-container-name t
          lsp-imenu-sort-methods '(position kind name)
          lsp-pyls-plugins-flake8-enabled t
          lsp-signature-auto-activate t
          lsp-signature-render-documentation t
          lsp-signature-doc-lines 10)
    (lsp-register-custom-settings
     '(("pyls.plugins.pyls_mypy.enabled" t t)
       ("pyls.plugins.pyls_mypy.live_mode" nil t)
       ("pyls.plugins.pyls_black.enabled" t t)
       ("pyls.plugins.pyls_isort.enabled" t t)

       ;; Disable these as they're duplicated by flake8
       ("pyls.plugins.pycodestyle.enabled" nil t)
       ("pyls.plugins.mccabe.enabled" nil t)
       ("pyls.plugins.pyflakes.enabled" nil t))))
  :hook
   ;; NOTE: we don't have a python-mode hook - it gets handled by pyvenv-track-virtualenv
  (;;(js-mode . lsp)
   (web-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-before-initialize . md/lsp-setup))
  :bind (:map evil-normal-state-map
              ("gh" . lsp-describe-thing-at-point)
              ("gr" . lsp-find-references)
              ("gD" . xref-find-apropos)
              ("gd" . lsp-find-definition)
              :map md/leader-map
              ("Ni" . imenu)
              ("Ff" . lsp-format-buffer)
              ("FR" . lsp-rename)))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :config
;;   (lsp-ui-mode nil))


;; (use-package lsp-ui
;;   :after flycheck
;;   :config
;;   (defun md/lsp-ui-setup ()
;;     (setq lsp-ui-sideline-show-hover nil
;;           lsp-ui-sideline-enable nil
;;           lsp-ui-sideline-delay 0.5
;;           lsp-ui-sideline-ignore-duplicate t
;;           lsp-ui-doc-delay 0.2
;;           lsp-ui-doc-position 'bottom
;;           lsp-ui-doc-alignment 'frame
;;           lsp-ui-doc-header nil
;;           lsp-ui-doc-include-signature t
;;           lsp-ui-doc-use-childframe nil))
;;   :commands lsp-ui-mode
;;   :hook ((lsp-before-initialize . md/lsp-ui-setup))
;;   :bind (:map evil-normal-state-map
;;               ("gd" . lsp-ui-peek-find-definitions)))

;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   :config
;;   (setq treemacs-no-png-images t
;;         lsp-treemacs-symbols-position-params '((side . left)
;;                                                (slot . 1)
;;                                                (window-width . 30)))
;;   (add-to-list 'shackle-rules
;;               '("\\`\\*LSP Lookup.*?\\*\\'" :regexp t :align t :close-on-realign t :size 12 :select t))
;;   :bind (:map evil-normal-state-map
;;               ("gr" . lsp-treemacs-references)
;;          :map md/leader-map
;;               ("Ni" . lsp-treemacs-symbols)))

;; (use-package helm-lsp
;;   :after lsp-mode
;;   :config (setq helm-lsp-treemacs-icons nil)
;;   :bind (:map evil-normal-state-map
;;             ("gD" . helm-lsp-workspace-symbol)))

(use-package python ;; builtin
  :config
  (evil-define-key 'normal python-mode-map
    "gk" 'python-nav-backward-defun
    "gj" 'python-nav-forward-defun))

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-workon pyvenv-workon)

  (when (fboundp 'pyvenv-track-virtualenv)
    (fmakunbound 'pyvenv-track-virtualenv))

(defun pyvenv-track-virtualenv ()
  "Set a virtualenv as specified for the current buffer.

This is originally provided by pyvenv, but I've added a couple
of features. The most important one is that this invokes lsp
/after/ all the pyvenv activate logic has been done, which means
lsp can properly jump to definitions."
  (when (string= major-mode "python-mode")
    (cond
     (pyvenv-activate
      (when (and (not (equal (file-name-as-directory pyvenv-activate)
                             pyvenv-virtual-env))
                 (or (not pyvenv-tracking-ask-before-change)
                     (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                       pyvenv-activate pyvenv-virtual-env))))
        (pyvenv-activate pyvenv-activate)))
     (pyvenv-workon
      (when (and (not (equal pyvenv-workon pyvenv-virtual-env-name))
                 (or (not pyvenv-tracking-ask-before-change)
                     (y-or-n-p (format "Switch to virtualenv %s (currently %s)"
                                       pyvenv-workon pyvenv-virtual-env-name))))
        (message "pyvenv switching from %s to %s" pyvenv-virtual-env-name pyvenv-workon)
        (pyvenv-workon pyvenv-workon))
      ;; lsp needs to run after pyvenv-workon, so we make sure it's running here rather than
      ;; in the python-mode-hook.
      (when (not lsp-mode)
        (lsp))))))

  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

;; Syntax and completion for pip requirements files.
(use-package pip-requirements :demand t)

(use-package web-mode
:demand t
:mode (("\\.tsx\\'" . web-mode)
        ("\\.jsx\\'" . web-mode)))

(when (fboundp 'lsp-typescript-javascript-tsx-jsx-activate-p)
  (fmakunbound 'lsp-typescript-javascript-tsx-jsx-activate-p))

  (defun lsp-typescript-javascript-tsx-jsx-activate-p (filename &optional _)
    "Check if the javascript-typescript language server should be enabled based on FILENAME."
    (or (string-match-p (rx (one-or-more anything) "." (or "ts" "js") (opt "x") string-end) filename)
        (and (derived-mode-p 'js-mode 'js2-mode 'typescript-mode 'web-mode)
             (not (derived-mode-p 'json-mode)))))

(org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)))

;; Fix - the default one uses `required('sys')` which is not available in
;; modern node versions.
  (setq org-babel-js-function-wrapper
    "process.stdout.write(require('util').inspect(function(){%s}()));")

(use-package git-commit
  :demand t
  :config
  (progn
    (defun md/git-commit-setup ()
      (interactive)
      (setq fill-column 70)
      (when (not (display-graphic-p))
        (font-lock-mode -1))  ;; Terminal colours are often bad
      (display-fill-column-indicator-mode)
      (evil-normal-state))
    (add-hook 'git-commit-setup-hook 'md/git-commit-setup)
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

    ;; Remove some default hooks that I don't use
    (remove-hook 'git-commit-setup-hook 'git-commit-save-message)
    (remove-hook 'git-commit-setup-hook 'git-commit-setup-changelog-support)
    (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
    (remove-hook 'git-commit-setup-hook 'git-commit-propertize-diff)))

(use-package git-gutter
 :demand t
 :init
 (progn
   (defun md/set-sensible-column ()
     "Unless file is too big, either git-gutter mode (when in git dir)"
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
 :demand t
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
  :demand t
  :config
  (progn
    (setq github-browse-file-show-line-at-point t))
  :bind (:map md/leader-map
        ("go" . github-browse-file)))

;; I don't need to confirm this via prompt.
(setq vc-follow-symlinks t)

(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

(use-package php-mode
    :demand t
    :config (progn
            (defun md/ometria-php-mode-hook ()
                (when (s-starts-with-p "OMETRIA" (system-name))
                (setq-local indent-tabs-mode t)
                (setq-local tab-width 4))
                ;;(whitespace-mode)
                ;; Don't auto indent as php indentation doesn't match existing conventions
                ;; on om.console
                (electric-indent-mode -1))

            (add-hook 'php-mode-hook 'md/ometria-php-mode-hook)))

(when (not (getenv "GOPATH"))
  (if (string= (system-name) "arch")
      (setenv "GOPATH" "/f/users/matt/golang")
    (setenv "GOPATH" "/Users/matt/golang"))
  (setenv "GO15VENDOREXPERIMENT" "1"))

(use-package go-mode
  :demand t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)

    ;; Make sure SPC uses the go-mode leader map rather than my default leader
    ;; map
    (evil-define-key 'normal go-mode-map
      "gd" 'go-goto-function
      "gD" 'go-goto-function)))

(use-package company-go
  :demand t
  :init
  (progn
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))))))

(use-package yaml-mode :demand t)

(use-package lua-mode :demand t)

(use-package terraform-mode)

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

(message "calling use-package for org")
(use-package org
  ;;:pin org
  :demand t
  :config
  (progn

(message "running org general setup")
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
     ;; [2021-05-02] Don't expand org buffers on open.
     org-startup-folded t

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
      ;; org-ellipsis "…"
      org-ellipsis " …"

      ;; Don't display the bold/underline/emphasis markup
      org-hide-emphasis-markers t

      org-pretty-entities nil
      org-fontify-quote-and-verse-blocks t
      org-image-actual-width 400


      ;; I shouldn't need to set this manually but in my current emacs
      ;; version the "note" entry is in an unexpected format, causing
      ;; the template insert function to fail. I can remove this
      ;; when that issue is fixed.
      org-structure-template-alist
      '(("n" . "note")
        ("a" . "export ascii")
        ("c" . "center")
        ("C" . "comment")
        ("e" . "example")
        ("E" . "export")
        ("h" . "export html")
        ("l" . "export latex")
        ("q" . "quote")
        ("s" . "src")
        ("v" . "verse"))

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
(bind-key "C-c T" 'md/org-timestamp-time-clipboard org-mode-map)
(bind-key "C-c T" 'md/org-timestamp-time-clipboard evil-normal-state-map)
(bind-key "C-c T" 'md/org-timestamp-time-clipboard evil-insert-state-map)

(bind-key "C-c D" 'md/org-timestamp-date-clipboard org-mode-map)
(bind-key "C-c D" 'md/org-timestamp-date-clipboard evil-normal-state-map)
(bind-key "C-c D" 'md/org-timestamp-date-clipboard evil-insert-state-map)

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
(bind-key "SPC u" 'org-priority-up md/org-mode-leader-map)

(evil-add-command-properties 'org-clock-goto :jump t)

;; Global org leader bindings
(bind-key "a a" 'org-agenda md/leader-map)
(bind-key "a c" 'helm-org-capture-templates md/leader-map)
(bind-key "a j" 'org-clock-goto md/leader-map)
(bind-key "RET" 'helm-org-capture-templates md/leader-map)
(bind-key "TAB" 'org-agenda md/leader-map)

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

(defun md/org-timestamp-date-clipboard ()
  "Copy timestamp without time to the clipboard"
  (interactive)
  (with-temp-buffer
    (org-insert-time-stamp (current-time) nil t)
    (buffer-string)
    (clipboard-kill-region (point-min) (point-max))))

(defun md/org-timestamp-time-clipboard ()
  "Copy timestamp with time to the clipboard"
  (interactive)
  (with-temp-buffer
    (org-insert-time-stamp (current-time) t t)
    (buffer-string)
    (clipboard-kill-region (point-min) (point-max))))


(defun md/org-insert-link-from-paste ()
  "Perform org-insert-link with the current contents of the clipboard"
  (interactive)
  (org-insert-link nil
                   (with-temp-buffer
                     (evil-paste-after nil)
                     (delete-trailing-whitespace)
                     (buffer-string))
                   (read-string "Description: ")))

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
  "gK" 'md/org-narrow-prev
  "gJ" 'md/org-narrow-next
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
          (kbd "C-c u") 'org-priority-up
          ))
      '(normal insert))

(add-hook 'org-mode-hook 'md/evil-org-mode)

(defun md/org-gcal-fetch-and-agenda-redo ()
  (interactive)
  ;;(ignore-errors
  ;;  (md/org-gcal-fetch))
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

(defun md/org-agenda-todo ()
  "Wrap org-agenda-todo but always use the prefix. Saves me pressing C-u."
  (interactive)
  (setq current-prefix-arg '(4))  ; C-u
  (call-interactively 'org-agenda-todo))

(evil-set-initial-state 'org-agenda-mode 'normal)

(evil-define-key 'normal md/evil-org-agenda-mode-map
  ;; j / k
  (kbd "j") 'org-agenda-next-line
  (kbd "n") 'org-agenda-next-line
  (kbd "C-n") 'org-agenda-next-line
  (kbd "k") 'org-agenda-previous-line
  (kbd "p") 'org-agenda-previous-line
  (kbd "C-p") 'org-agenda-previous-line

  (kbd "RET") 'org-agenda-goto

  (kbd "T") 'md/org-agenda-todo
  (kbd "E") 'org-agenda-set-effort
  (kbd "R") 'org-agenda-refile
  (kbd "c") 'org-agenda-set-tags

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

(defun md/org-narrow-next ()
  (interactive)
  (when (org-buffer-narrowed-p)
    (widen))
  (call-interactively 'org-next-visible-heading)
  (org-narrow-to-subtree)
  (outline-hide-subtree)
  (org-show-entry)
  (org-show-children))

(defun md/org-narrow-prev ()
  (interactive)
  (when (org-buffer-narrowed-p)
    (widen))
  (call-interactively 'org-previous-visible-heading)
  (org-narrow-to-subtree)
  (outline-hide-subtree)
  (org-show-entry)
  (org-show-children))

(bind-key "C-j" 'md/org-narrow-next md/evil-org-mode-map)
(bind-key "C-k" 'md/org-narrow-prev md/evil-org-mode-map)

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
      org-export-with-section-numbers 2
      org-html-head-include-default-style nil
      org-export-with-section-numbers nil
      org-html-validation-link nil
      org-html-postamble nil
      org-export-with-sub-superscripts nil  ;; don't mess up things_with_underscores in html export
      org-html-htmlize-output-type nil
      org-export-babel-evaluate nil
      org-html-head "
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.min.css\">
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.6/styles/github.min.css\">
<script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js\"></script>
<script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/python.min.js\"></script>
<script charset=\"UTF-8\" src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/javascript.min.js\"></script>

   <style type=\"text/css\">
 b {color: #000}
 a {font-weight: normal; color: blue}
 a>code {font-weight: normal; color: blue}
 html {font-size: 19px; max-width: 100%; margin: 20px 10%; font-family: Freeserif, serif; line-height:1.2}
 pre {font-family: monospace; color: #000; font-size: 13px; line-height: 1.4; max-width: 720px}
 code {font-family: monospace; font-size: 13px; background-color: rgba(27,31,35,.05); padding: 2px 4px; max-width: 720px;}
 pre > code { background-color: #ffffff !important; }
 p, dl { color: #555}
 dt { color: #000; font-weight: bold; margin-top: 8px;}
 hr {margin-bottom: 40px; margin-left: 0; margin-right: 0}
 h1 {font-size: 24px}
 h1, h2, h3 {margin-bottom: 8px}
 h2, h3, h4 {font-family: sans-serif}
 p {margin-top: 8px}
 img {margin-top: 16px; margin-bottom: 16px; max-width: 100%;}
 hr {min-height: 4px; background-color: #000}
 hr.hrsmaller {min-height: 1px; background-color: #000}
 ul {padding-left: 20px}
 ul > li {list-style-type: circle}
 li {color: #555; margin-top: 8px; margin-bottom: 8px;}
 a:hover {color: blue}
 h1, h2, h3, h4, p, ol, li, hr, dl {width:540px; max-width: 100%}
 blockquote {font-style: italic; border-left: 2px solid #555; padding-left: 10px; }
#table-of-contents > h2 {display:none};
   </style>

<script type=\"text/javascript\">
const init = () => {
    document.querySelectorAll('.src-js').forEach(el => {
        newcode = document.createElement('code');
        newcode.classList.add('hljs', 'javascript');
        newcode.innerHTML = el.innerHTML;
        newpre = document.createElement('pre');
        newpre.appendChild(newcode);
        el.parentNode.replaceChild(newpre, el);
    });
    hljs.initHighlighting();

    document.querySelectorAll('h2').forEach(el => {
        hr = document.createElement('hr');
        el.parentNode.insertBefore(hr, el);
        br = document.createElement('br');
        el.parentNode.insertBefore(br, el);
    });
    document.querySelectorAll('h3').forEach(el => {
        br = document.createElement('br');
        el.parentNode.insertBefore(br, el);
    });
    document.querySelectorAll('h2')[1].scrollIntoView();

}
window.addEventListener('load', init, false );
</script>

<script charset=\"UTF-8\" src=\"org-info.js\"></script>
")

(use-package ox-reveal)

(setq org-latex-default-class "report"
      org-latex-default-packages-alist '(("AUTO" "inputenc" t
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
                                         ("linktoc=all,colorlinks=true,linkcolor=black,urlcolor=blue" "hyperref" nil)))

(defun md/org-gcal-fetch ()
  "Always refresh gcal token before fetching, as it expires every hour"
  (interactive)
  (ignore-errors ;; Error is thrown if token doesn't already exist in the gcal file
    ;; TODO could this only prompt once per hour?
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

(use-package org-bullets
  :config
  (progn
    (add-hook 'org-mode-hook 'org-bullets-mode)

    (setq   org-bullets-bullet-list '(
                                      ;;"❆"
                                      ;;"▶"
                                      "▷"
                                      "▷"
                                      "▷"
                                      "▹"
                                      "▹"
                                      "▹"
                                      "▹"
                                      "▹"
                                      "▹"
                                      "▹"
                                      "▹"

                                      ;; "✸"
                                      ;; "◎"
                                      ;; "▶"
                                      ;; "◉"
                                      ;; "◈"
                                      ;; "○"
                                      ;; "◇"
                                      ;; "◦"
                                    ))))

(defvar md/org-toggle-list-utf-val t)

(defun md/org-list-utf-enable ()
  (interactive)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq md/org-toggle-list-utf-val t)
  (when (string= major-mode "org-mode") (org-mode-restart))
  (message "List list-utf enabled"))

(defun md/org-list-utf-disable ()
  (interactive)
  (font-lock-remove-keywords 'org-mode
                             '(("^ *\\([-]\\) "
                                (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq md/org-toggle-list-utf-val nil)
  (when (string= major-mode "org-mode") (org-mode-restart))
  (message "List list-utf disabled"))

(defun md/org-toggle-list-utf ()
  (interactive)
  (if md/org-toggle-list-utf-val (md/org-list-utf-disable) (md/org-list-utf-enable)))

(defun md/org-md-import ()
  "Use pandoc to convert clipboard markdown contents into org,
and insert in current buffer."
  (interactive)
  (insert
   (with-temp-buffer
     (shell-command "xclip -selection clipboard -o | pandoc --from gfm --to org" (current-buffer) (current-buffer))
     (buffer-string))))

(defun md/org-md-export ()
  "Use pandoc to copy the region or buffer into the clipboard,
converting to markdown."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (point-min) (point-max)))
  ;; Use sed to remove backslashes that get appended around some special chars,
  ;; as this is less friendly when working with people who are editing markdown.
  ;; The awful backslahes here are because we are replacing a backslash, and then have to escape
  ;; both (1) in emacs and (2) in sed, because the backslash has signficance in both programs.
  (shell-command "xclip -selection clipboard -o | pandoc --from org --to gfm | sed -e 's/\\\\\\[/\\[/g' -e 's/\\\\\\]/\\]/g' -e 's/\\\\>/>/g' -e 's/\\\\</</g' | xclip -selection clipboard" nil nil))

(defun md/org-html-export ()
  "Use pandoc to copy the region or buffer into the clipboard,
converting to html. This is also opened in the browser so it can
be quickly copy/pasted into eg. gmail."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (point-min) (point-max)))
  (let ((path (format "%s.html" (s-trim-right (shell-command-to-string "mktemp"))))
        (from (if (string= major-mode "markdown-mode") "gfm" "org")))
    (async-shell-command (format "xclip -selection clipboard -o | pandoc --from %s --to html | tee %s | xclip -selection clipboard" from path) nil nil)
    (shell-command (format "firefox %s" path) nil nil)))

(bind-key "Tm" 'md/org-md-export md/leader-map)
(bind-key "TM" 'md/org-md-import md/leader-map)
(bind-key "Th" 'md/org-html-export md/leader-map)

(use-package ox-org :demand t)

(use-package org-mind-map
  :demand t
  :load-path "non-elpa/org-mind-map" ;; includes my fix https://github.com/the-humanities/org-mind-map/pull/52
  :config
  (setq org-mind-map-include-text nil
        org-mind-map-engine "dot"
        org-mind-map-tag-colors 'nil
        org-mind-map-wrap-text-length 30
        org-mind-map-default-node-attribs '(("shape" . "plaintext"))
        org-mind-map-default-edge-attribs '(("color" . "#cccccc")
                                            ("arrowhead" . "none")
                                            ("arrowtail" . "none"))
        org-mind-map-default-graph-attribs '(("autosize" . "false")
                                             ("size" . "125,50")
                                             ("resolution" . "100")
                                             ("nodesep" . "0.1")
                                             ("margin" . "0.1")
                                             ("overlap" . "false")
                                             ("splines" . "ortho")  ;; straight lines that wrap around
                                             ("rankdir" . "LR")))

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
         (org-mind-map-write-named (concat base-filename ".mind-map") nil t)))))

(message "use-package for org finished")
  ))

(use-package org-download :demand t
  :config (progn
            (org-download-enable)
            (bind-key "SPC L" 'org-download-yank md/org-mode-leader-map)
            (bind-key "C-c L" 'org-download-yank org-mode-map)))

(use-package helm
  :demand t
  ;;:defer 5
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

(defun md/strip-first-word (s)
  "Remove the first word from a string. This function just exists for readability."
  (string-remove-prefix (format "%s " (car (split-string s))) s))

(defun md/org-capture-popup-frame (template-shortcut)
  (make-frame '((name . "org-capture")
                ;;(window-system . x)
                (auto-raise . t)
                (height . 20)
                (width . 120)
                (left . 0.3)
                (top . 0.2)))
  (select-frame-by-name "org-capture")
  (org-capture nil template-shortcut)
  (delete-other-windows))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun md/org-capture-no-popup-frame (template-shortcut)
  "A version that doesn't open a new frame, for use with exwm"
  (org-capture nil template-shortcut))

(defun md/alfred-source-org-capture-templates ()
  "Run org-capture in a popup window. Copied from helm-org.el, but plugs in my
popup version of org-capture instead of using the usual org-capture."
  (helm-build-sync-source "Org Capture"
    :fuzzy-match nil
    :multimatch nil
    :candidates (cl-loop for template in org-capture-templates
                         collect (cons (nth 1 template) (nth 0 template)))
    :action '(("Capture" . (lambda (template-shortcut)
                             (if (md/exwm-enabled)
                                 (md/org-capture-no-popup-frame template-shortcut)
                               (md/org-capture-popup-frame template-shortcut)))))))

(defun md/alfred-source-org-light (my-helm-title my-helm-prefix my-org-keyword)
  "MY-HELM-TITLE is the title given to the Helm source.
MY-ORG-KEYWORD is the org keyword to search for.
MY-HELM-PREFIX is a prefix that should be typed before any candidates start matching."
  (helm-build-sync-source my-helm-title
    :requires-pattern t
    :multimatch nil
    :candidates
    ;; Loop over my agenda files. For each one, use a regex to match all the headlines starting with
    ;; the "NOW" keyword. Collect the headline, the buffer and the point.
    (let ((cands nil)
          (case-fold-search nil)) ; make search case-sensitive
      (cl-loop for f in (org-agenda-files)
               do (save-window-excursion
                    (with-current-buffer (find-file f)
                      (save-excursion
                        (save-restriction
                          (widen)
                          (goto-char (point-min))
                          (cl-loop until (not (search-forward-regexp (format "^*+ \\(%s\\) " my-org-keyword) nil t)) do
                                   (add-to-list 'cands
                                                (cons (substring-no-properties (org-get-heading))
                                                      (cons (current-buffer) (point))))))))))
      cands)
    ;; Similar pattern to elsewhere - only match these items if I search for "now $query".
    :match `((lambda (candidate)
               (and
                (s-starts-with? ,(format "%s " my-helm-prefix) helm-pattern)
                (string-match (s-chop-prefix ,(format "%s " my-helm-prefix) helm-pattern) candidate))))
    ;; Open the item in a new frame, using org-tree-to-indirect-buffer.
    :action '(("None" . (lambda (cand)
                          ;; Only make a separate frame when I'm not on exwm
                          (when (not (md/exwm-enabled))
                            (make-frame '((name . "org-popup")
                                          ;;(window-system . x)
                                          (auto-raise . t)
                                          ;; I'm actually ignoring the size params as it gets
                                          ;; tiled by i3.
                                          (height . 40)
                                          (width . 100)
                                          (left . 0.3)
                                          (top . 0.2)))
                            (select-frame-by-name "org-popup"))
                          (with-current-buffer (car cand)
                            (save-excursion
                              (goto-char (cdr cand))
                              (org-tree-to-indirect-buffer))))))))

(defun md/alfred-source-org-clock-out ()
  "Open predefined web bookmarks."
  (helm-build-sync-source "Org clock"
    :multimatch nil
    :requires-pattern nil
    :fuzzy-match t
    :candidates '(("Stop org clock" . nil))
    :action '(("Stop" . (lambda (cand) (org-clock-out nil t nil))))))

(defvar md/alfred-source-search-candidates
  '(("Google" . ("g" . "https://www.google.co.uk/search?q=%s"))
    ("DuckDuckGo" . ("d" . "https://www.duckduckgo.com/?q=%s"))
    ("Dictionary.com" . ("spell" . "https://www.dictionary.com/browse/%s"))))

(defun md/alfred-source-search ()
  "Perform web searches - eg. on DuckDuckGo. Similar to the 'g ' and 'd ' features on Alfred."
  (helm-build-sync-source "Search"
    :nohighlight t
    :nomark t
    :multimatch nil
    :requires-pattern t
    :candidates md/alfred-source-search-candidates
    :match '((lambda (candidate)
               (string= (car (cdr (assoc candidate md/alfred-source-search-candidates))) (car (split-string helm-pattern)))))
    :fuzzy-match nil
    :filtered-candidate-transformer (lambda (candidates source)
                                      (map 'list (lambda (c) (cons (format "%s: %s" (car c) (md/strip-first-word helm-pattern)) (cdr c))) candidates))
    :action '(("Search" . (lambda (candidate)
                            (browse-url (format (cdr candidate)  ;; the url
                                                (url-hexify-string
                                                 ;; This removes the "g " part from the string
                                                 (md/strip-first-word helm-pattern)))))))))

(defun md/alfred-source-web-bookmarks ()
  "Open predefined web bookmarks."
  (helm-build-sync-source "Web Bookmarks"
    :multimatch nil
    :requires-pattern nil
    :candidates '(("Localhost" . "http://localhost:8000")
                  ("Gmail" . "https://mail.google.com")
                  ("Calendar" . "https://calendar.google.com")
                  ("Slack: emacs.london" . "https://app.slack.com/client/TJ29PTDDX")
                  ("Emacs.london" . "https://emacs.london")
                  ("Github" . "https://github.com/"))
    :action '(("Open" . (lambda (candidate) (browse-url candidate))))))

(defun md/alfred-source-system ()
  "System commands - lock, sleep etc."
  (helm-build-sync-source "System"
    :multimatch nil
    :requires-pattern nil
    :candidates '(("Lock" . "xset dpms force off")  ;; turns screen off and triggers i3lock
                  ("Sleep" . "systemctl suspend -i")
                  ("Restart" . "systemctl reboot -i")
                  ("Shutdown" . "systemctl poweroff -i"))
    :action '(("Execute" . (lambda (candidate)
                             (shell-command (concat candidate " >/dev/null 2>&1 & disown") nil nil))))))

(defun md/alfred-source-apps ()
  "Use gtk-launch to open installed .desktop programs. This isn't perfect - eg. the program names
are ugly. It works fine though."
  (helm-build-sync-source "Launch"
    :multimatch nil
    :requires-pattern nil
    :candidates (lambda ()
                  (-map
                   (lambda (item)
                     (s-chop-suffix ".desktop" item))
                   (-filter (lambda (d) (not (or (string= d ".") (string= d ".."))))
                            (directory-files "/usr/share/applications"))))
    :action '(("Launch" . (lambda (candidate)
                            (shell-command (concat "gtk-launch " candidate " >/dev/null 2>&1 & disown") nil nil))))))

(defun md/alfred-source-webserver ()
  "Start a webserver process in a particular directory."
  (helm-build-sync-source "Webserver"
    :multimatch nil
    :requires-pattern t
    :candidates '(("mattduck.com" . "/f/www.mattduck.com/build")
                  ("shonamcgovern.com" . "/f/clients/shona-mcgovern/shonamcgovern.com/site")
                  ("emacs.london" . "/f/emacs.london/london-emacs-hacking.github.io")
                  ("pkb" . "/f/md.pkb"))
    :match '((lambda (candidate)
               (and
                (s-starts-with? "serve " helm-pattern)
                ;; this is what helm-default-match-function does
                (string-match (s-chop-prefix "serve " helm-pattern) candidate))))
    :action '(("Serve" . (lambda (candidate)
                           (shell-command (format "cd '%s'; ,serve >/tmp/helm-webserver 2>&1 & disown" candidate) nil nil))))))

(defun md/alfred-source-processes ()
  "Source which uses `ps` to list processes, and then offer options to interact with them."
  (helm-build-async-source "Processes"
    :nohighlight t  ;; Because grep is doing the matching, not helm
    :multimatch nil
    :requires-pattern t
    :candidates-process (lambda ()
                          (if (s-starts-with? "p " helm-pattern)
                              (let ((pat (s-chop-prefix "p " helm-pattern)))
                                (start-process-shell-command
                                 ;; The weird grep -v at the end is a silly quick way to strip the grep
                                 ;; itself from showing in the results.
                                 "helm-source-ps" nil (format "ps axh -o pid,group,args --cols 90 -q $(pgrep '%s' -d ',' --full) 2>/dev/null | grep -v 'pid,group,args --cols 90'" pat)))
                            (start-process "helm-source-noop" nil "true")))
    :action '(("SIGTERM" . (lambda (cand) (start-process "helm-source-sigterm" nil "kill" (car (split-string cand)))))
              ("SIGKILL" . (lambda (cand) (start-process "helm-source-sigkill" nil "kill" "-9" (car (split-string cand))))))))

(defun md/alfred-source-directories ()
  "Open a directory."
  (helm-build-async-source "Directories"
    :multimatch nil
    :requires-pattern t
    :candidates-process (lambda ()
                          (if (and (s-starts-with? "fd " helm-pattern) (> (length helm-pattern) 4))
                              (let ((pat (s-chop-prefix "fd " helm-pattern)))
                                (start-process-shell-command
                                 "helm-source-fd" nil (format "fd -t d -a -d 8 --full-path --color never '%s' /f 2>/dev/null" pat)))
                            (start-process "helm-source-noop" nil "true")))
    ;; Invoke terminal because ranger needs a terminal. If you copy/paste this you may not need a
    ;; terminal - xdg-open could open a GUI file manager.
    :action '(("Open" . (lambda (cand) (shell-command (format "gnome-terminal -- /bin/sh -c \"xdg-open '%s'\" & disown" cand) nil nil))))))

(defun md/alfred-source-files ()
  "Open a file with its default program."
  (helm-build-async-source "Files"
    :multimatch nil
    :requires-pattern t
    :candidates-process (lambda ()
                          (if (and (s-starts-with? "f " helm-pattern) (> (length helm-pattern) 4))
                              (let ((pat (s-chop-prefix "f " helm-pattern)))
                                (start-process-shell-command
                                 "helm-source-fd" nil (format "fd -t f -a -d 15 --full-path --color never '%s' /f 2>/dev/null" pat)))
                            (start-process "helm-source-noop" nil "true")))
    :action '(("Open" . (lambda (cand) (call-process "sh" nil nil nil "-c" (format "xdg-open '%s' & disown" cand)))))))

(defun md/alfred--helm (buffer)
  (helm :sources (list (md/alfred-source-system)
                       (md/alfred-source-search)
                       (md/alfred-source-webserver)
                       (md/alfred-source-org-capture-templates)
                       (md/alfred-source-org-light "Org - NOW" "now" "NOW")
                       (md/alfred-source-org-light "Org - BACK" "back" "BACK")
                       (md/alfred-source-org-clock-out)
                       (md/alfred-source-web-bookmarks)
                       (when (string= (system-name) "arch")
                         (md/alfred-source-apps))
                       (md/alfred-source-directories)
                       (md/alfred-source-files)
                       (md/alfred-source-processes))
        :prompt ""
        :buffer buffer))

(defun md/alfred-no-frame ()
  (interactive)
  (md/alfred--helm nil))

(defun md/alfred ()
  "Entry point to create the 'alfred' frame and run helm."
  (interactive)
  (with-current-buffer (get-buffer-create "*alfred*")
    (let ((frame (make-frame '((name . "alfred")
                               ;;(window-system . x)
                               (auto-raise . t)
                               (height . 10)
                               (internal-border-width . 20)
                               (left . 0.33)
                               (left-fringe . 0)
                               (line-spacing . 3)
                               (menu-bar-lines . 0)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (top . 48)
                               ;; enable this to remove frame border
                               (undecorated . t)
                               (unsplittable . t)
                               (vertical-scroll-bars . nil)
                               (width . 120))))
          (alert-hide-all-notifications t)
          (inhibit-message t)
          (mode-line-format nil)
          (helm-mode-line-string nil)
          (helm-full-frame t)
          (helm-display-header-line nil)
          (helm-use-undecorated-frame-option nil)
          ;; If we run an async shell, don't show us anything
          (async-shell-command-display-buffer nil))
      (md/alfred--helm "*alfred*")
      (delete-frame frame)
      ;; For some reason without killing the buffer it messes up future state.
      (kill-buffer "*alfred*")
      ;; I don't want this to cause the main frame to flash
      (x-urgency-hint (selected-frame) nil))))

(use-package annotate
  :demand t
  :init
  (progn
    ;; (add-hook 'prog-mode-hook 'annotate-mode)

    ;; TODO: annotate faces
    ;; TODO: jump to annotation? Search?
    (setq annotate-use-messages nil)
    (defun md/annotate ()
      (interactive)
      (when (not annotate-mode)
        (annotate-mode))
      (call-interactively 'annotate-annotate)
      (md/save-if-not-remote)))

  :bind (:map md/leader-map
             ("cj" . md/annotate)
             ("cn" . annotate-next-annotation)
             ("cp" . annotate-previous-annotation)
             ("cJ" . annotate-mode)))

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
      "+" 'dired-create-directory)))

(use-package restclient
  ;;:defer 1
  :mode (("\\.http\\'" . restclient-mode)))

(use-package restclient-helm
  ;;:defer 5
)

(use-package company-restclient
  :config
  (progn
      (add-to-list 'company-backends 'company-restclient)))

(use-package elfeed
  :demand t
  :config
  (progn
    (setq elfeed-search-title-max-width 110
          elfeed-search-filter "@2-weeks-ago -hidden !ojwtech")

    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "1 month ago"
                                  :remove 'unread))

    (defun md/elfeed-search-toggle-unread ()
      (interactive)
      (elfeed-search-toggle-all 'unread))

    (defun md/elfeed-search-toggle-hide ()
      (interactive)
      (elfeed-search-toggle-all 'hidden))

    (defun md/elfeed-search-toggle-starred ()
      (interactive)
      (elfeed-search-toggle-all 'starred))

    (evil-set-initial-state 'elfeed-search-mode 'emacs)
    (md/make-keymap-noop elfeed-search-mode-map)
    (bind-key "SPC" md/leader-map elfeed-search-mode-map)
    (bind-key "C-h" help-mode-map elfeed-search-mode-map)
    (bind-key "q" 'elfeed-search-quit-window elfeed-search-mode-map)
    (bind-key "j" 'evil-next-visual-line elfeed-search-mode-map)
    (bind-key "n" 'evil-next-visual-line elfeed-search-mode-map)
    (bind-key "C-n" 'evil-next-visual-line elfeed-search-mode-map)
    (bind-key "k" 'evil-previous-visual-line elfeed-search-mode-map)
    (bind-key "p" 'evil-previous-visual-line elfeed-search-mode-map)
    (bind-key "C-p" 'evil-previous-visual-line elfeed-search-mode-map)
    (bind-key "C-f" 'evil-scroll-page-down elfeed-search-mode-map)
    (bind-key "C-b" 'evil-scroll-page-up elfeed-search-mode-map)
    (bind-key "C-d" 'evil-scroll-down elfeed-search-mode-map)
    (bind-key "l" 'elfeed-search-show-entry elfeed-search-mode-map)
    (bind-key "o" 'elfeed-search-browse-url elfeed-search-mode-map)
    (bind-key "r" 'elfeed-search-update--force elfeed-search-mode-map)
    (bind-key "R" 'elfeed-search-fetch elfeed-search-mode-map)
    (bind-key "|" 'elfeed-search-clear-filter elfeed-search-mode-map)
    (bind-key "/" 'elfeed-search-set-filter elfeed-search-mode-map)
    (bind-key "g" 'elfeed-search-first-entry elfeed-search-mode-map)
    (bind-key "G" 'elfeed-search-last-entry elfeed-search-mode-map)
    (bind-key "u" 'md/elfeed-search-toggle-unread elfeed-search-mode-map)
    (bind-key "H" 'md/elfeed-search-toggle-hide elfeed-search-mode-map)
    (bind-key "s" 'md/elfeed-search-toggle-starred elfeed-search-mode-map)

    (evil-set-initial-state 'elfeed-show-mode 'emacs)
    (md/make-keymap-noop elfeed-show-mode-map)
    (bind-key "h" 'elfeed-kill-buffer elfeed-show-mode-map)
    (bind-key "q" 'elfeed-kill-buffer elfeed-show-mode-map)
    (bind-key "SPC" md/leader-map elfeed-show-mode-map)
    (bind-key "C-h" help-mode-map elfeed-show-mode-map)
    (bind-key "n" 'evil-next-visual-line elfeed-show-mode-map)
    (bind-key "C-n" 'evil-next-visual-line elfeed-show-mode-map)
    (bind-key "C-p" 'evil-previous-visual-line elfeed-show-mode-map)
    (bind-key "C-f" 'evil-scroll-page-down elfeed-show-mode-map)
    (bind-key "C-b" 'evil-scroll-page-up elfeed-show-mode-map)
    (bind-key "C-d" 'evil-scroll-down elfeed-show-mode-map)
    (bind-key "o" 'elfeed-show-visit elfeed-show-mode-map)
    (bind-key "j" 'elfeed-show-next-link elfeed-show-mode-map)
    (bind-key "l" 'elfeed-show-next-link elfeed-show-mode-map)

    (bind-key "R" 'elfeed md/leader-map)

    ;; elfeed-show-mode uses shr
    (setq shr-width 70
          shr-use-fonts nil)))

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
    :load-path "non-elpa/bookmark-plus"
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

(use-package powerline
  :demand t
  ;;:defer 1
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
                            ;; Eyebrowse index/tag
                            (when eyebrowse-mode
                              (powerline-raw
                               (let* ((window-configs (eyebrowse--get 'window-configs))
                                      (current-config (assoc (eyebrowse--get 'current-slot) window-configs))
                                      (current-index (car current-config))
                                      (current-tag (nth 2 current-config)))
                                 (format "%s %s " current-index current-tag)) face3 'l ))

                            ;; Line / column numbers
                            (when (or line-number-mode column-number-mode)
                              (cond ((and line-number-mode
                                          column-number-mode)
                                     (powerline-raw "%4l:%2c " face3 'l))
                                    (line-number-mode
                                     (powerline-raw "%4l" face3 'l))
                                    (column-number-mode
                                     (powerline-raw ":%2c " face3 'l))))

                            ;; Dedicated mode indicaitor
                            (when (and (boundp 'dedicated-mode) dedicated-mode)
                              (powerline-raw (format "Ded.") face3 'l))

                            ;; Modeline
                            (when (face-foreground 'md/powerline-normal)
                              (funcall separator-left face3 mode-line))

                            (powerline-raw "%b" mode-line 'l)

                            ;; File state
                            (when (buffer-modified-p)
                              (powerline-raw "+ " mode-line 'l))
                            (when buffer-read-only
                              (powerline-raw "[RO] " mode-line 'l))
                            (when (buffer-narrowed-p)
                              (powerline-raw "[Narrow] " mode-line 'l))
                            (when (string= major-mode "python-mode")
                              (powerline-raw (format "[venv:%s] " pyvenv-virtual-env-name) mode-line 'l))
                            (when (and active (fboundp 'org-clocking-p) (org-clocking-p))
                              (powerline-raw
                               (propertize
                                (format "%s "
                                        (if (> (length org-mode-line-string) 50)
                                            (format "%s..." (string-trim (substring org-mode-line-string 0 50)))
                                          org-mode-line-string))
                                'face nil)
                               face2 'l))))

                      (rhs
                       ;; Flycheck
                       (when (and active flycheck-mode flycheck-current-errors)
                         (let* ((errors (flycheck-count-errors flycheck-current-errors))
                                (warn-count (cdr (assoc 'warning errors)))
                                (err-count (cdr (assoc 'error errors)))
                                (msg (format " %s %s"
                                             (or err-count 0)
                                             (or warn-count 0)))
                                (face (if (flycheck-has-current-errors-p 'error)
                                          'md/modeline-flycheck-error
                                        'md/modeline-flycheck-warning)))
                           (list
                            (if err-count
                                (powerline-raw (format " %s" err-count) 'flycheck-error-list-error 'r))
                            (if warn-count
                                (powerline-raw (format " %s" warn-count) 'flycheck-error-list-warning 'r)))))
                       ))
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
 '((shell . t)))

(use-package color-theme-solarized
 :demand nil
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

(add-to-list 'custom-theme-load-path (md/dotfiles-get-path "emacs.d.symlink/non-elpa/emacs-theme-gruvbox"))

(add-to-list 'custom-theme-load-path (md/dotfiles-get-path "emacs.d.symlink/non-elpa/emacs-theme-md-wiki"))

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
  (md/powerline-reset)
  (md/fontify-buffer)
  (set-window-buffer nil (current-buffer))) ;; temp?

;; Initial setup
(md/disable-all-themes)
(load-theme 'gruvbox-dark-medium t)

(bind-key "tt" 'md/load-theme md/leader-map)
(md/set-default-font)

;; TODO a way to revert all these changes to their original setting.
;; maybe a macro where that idea can be reapplied - after something is disabled, all the original values get restored.

(defun md/toggle-org-pretty ()
  (interactive)
  (require 'org-indent)
  (make-local-variable 'org-hide-leading-stars)
  (make-local-variable 'org-indent-indentation-per-level)
  (make-local-variable 'org-cycle-separator-lines)
  (make-local-variable 'org-bullets-bullet-list)
  (md/toggle-variable-layer
   (format "org-pretty-%s" (current-buffer))
   '((line-spacing . 0.25)
     (left-margin-width . 10)
     (right-margin-width . 5)
     (header-line-format . " ")
     (mode-line-format . " ")
     (org-hide-leading-stars . t)
     (org-indent-indentation-per-level . 2)
     (org-cycle-separator-lines . 1)
     (org-bullets-bullet-list . ("▷"
                                 "▷"
                                 "▷"
                                 "▹"
                                 "▹"
                                 "▹"
                                 "▹"
                                 "▹"
                                 "▹"
                                 "▹")))
   (lambda () ;; Enable fn
     (set-window-buffer nil (current-buffer))
     (font-lock-fontify-buffer)
     (org-indent-mode 1)
     (md/org-list-utf-enable))
   (lambda () ;; Disable fn
     (set-window-buffer nil (current-buffer))
     (font-lock-fontify-buffer)
     (org-indent-mode 0)
     (md/org-list-utf-disable))))

(bind-key "tW" 'md/toggle-org-pretty md/leader-map)

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
    (eyebrowse-mode 1)))

(defvar splitscreen/zoomed-p nil)
(defun splitscreen/toggle-zoom ()
  "Toggle buffer maximising within this eyebrowse tab. Replicates the
   tmux zoom feature."
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
  (apply fn args)
  (set-register (eyebrowse--get 'current-slot) nil))

(advice-add 'eyebrowse-close-window-config :around 'splitscreen/reset-zoom '((name . "splitscreen")))

(defun splitscreen/window-left ()
  (interactive)
  (evil-window-left 1))

(defun splitscreen/window-right ()
  (interactive)
  (evil-window-right 1))

(defun splitscreen/window-up ()
  (interactive)
  (evil-window-up 1))

(defun splitscreen/window-down ()
  (interactive)
  (evil-window-down 1))

(defun splitscreen/increase-width () (interactive) (evil-window-increase-width 10))
(defun splitscreen/decrease-width () (interactive) (evil-window-decrease-width 10))
(defun splitscreen/increase-height () (interactive) (evil-window-increase-height 10))
(defun splitscreen/decrease-height () (interactive) (evil-window-decrease-height 10))

(defvar splitscreen/mode-map (make-sparse-keymap))
(define-prefix-command 'splitscreen/prefix)
(define-key splitscreen/mode-map (kbd "C-w") 'splitscreen/prefix)

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
(define-key evil-window-map (kbd "C-l") nil)
(define-key evil-window-map (kbd "o") nil)

(define-key splitscreen/prefix (kbd "h") 'splitscreen/window-left)
(define-key splitscreen/prefix (kbd "j") 'splitscreen/window-down)
(define-key splitscreen/prefix (kbd "k") 'splitscreen/window-up)
(define-key splitscreen/prefix (kbd "l") 'splitscreen/window-right)

(define-key splitscreen/prefix (kbd "c") 'eyebrowse-create-window-config)
(define-key splitscreen/prefix (kbd "n") 'eyebrowse-next-window-config)
(define-key splitscreen/prefix (kbd "p") 'eyebrowse-prev-window-config)
(define-key splitscreen/prefix (kbd "X") 'eyebrowse-close-window-config)
(define-key splitscreen/prefix (kbd "!") 'eyebrowse-switch-to-window-config)
(define-key splitscreen/prefix (kbd "%") 'split-window-right)
(define-key splitscreen/prefix (kbd "\"") 'split-window-below)
(define-key splitscreen/prefix (kbd "x") 'delete-window)
(define-key splitscreen/prefix (kbd "z") 'splitscreen/toggle-zoom)
(define-key splitscreen/prefix (kbd "o") 'splitscreen/toggle-zoom)  ;; This is easier to reach than z
(define-key splitscreen/prefix (kbd "C-h") 'splitscreen/decrease-width)
(define-key splitscreen/prefix (kbd "C-j") 'splitscreen/decrease-height)
(define-key splitscreen/prefix (kbd "C-k") 'splitscreen/increase-height)
(define-key splitscreen/prefix (kbd "C-l") 'splitscreen/increase-width)
(define-key splitscreen/prefix (kbd "SPC") 'balance-windows)
(define-key splitscreen/prefix (kbd "0") 'eyebrowse-switch-to-window-config-0)
(define-key splitscreen/prefix (kbd "1") 'eyebrowse-switch-to-window-config-1)
(define-key splitscreen/prefix (kbd "2") 'eyebrowse-switch-to-window-config-2)
(define-key splitscreen/prefix (kbd "3") 'eyebrowse-switch-to-window-config-3)
(define-key splitscreen/prefix (kbd "4") 'eyebrowse-switch-to-window-config-4)
(define-key splitscreen/prefix (kbd "5") 'eyebrowse-switch-to-window-config-5)
(define-key splitscreen/prefix (kbd "6") 'eyebrowse-switch-to-window-config-6)
(define-key splitscreen/prefix (kbd "7") 'eyebrowse-switch-to-window-config-7)
(define-key splitscreen/prefix (kbd "8") 'eyebrowse-switch-to-window-config-8)
(define-key splitscreen/prefix (kbd "9") 'eyebrowse-switch-to-window-config-9)

(define-minor-mode splitscreen-mode
    "Provides tmux-like bindings for managing windows and buffers.
     See https://github.com/mattduck/splitscreen"
    1 ; enable by default
    :lighter " Split"
    :global 1
    :keymap splitscreen/mode-map)

(if (md/exwm-enabled)
    (splitscreen-mode 0) ;; If exwm is on, this is provided by s-w
  (splitscreen-mode 1)
  (bind-key "C-w" splitscreen/mode-map edebug-mode-map))

(use-package edit-indirect
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

(use-package winner
  :demand t
  :config (winner-mode 1)
  :bind (:map splitscreen/prefix
              ("u" . winner-undo)
              ("U" . winner-redo)))

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

    ;;(defmacro md/shackle-advise (fn)
    ;;  "Add advise to given function to wrap with md/shackle-wrapper."
    ;;  `(advice-add ,fn :around 'md/use-display-buffer-alist
    ;;               '((name . "md/shackle"))))

    ;;(defmacro md/shackle-unadvise (fn)
    ;;  `(advice-remove ,fn 'md/use-display-buffer-alist))
    (defun md/shackle-advise (fn)
      (advice-add fn :around 'md/use-display-buffer-alist
                  '((name . "md/shackle"))))

    (defun md/shackle-unadvise (fn)
      (advice-remove fn 'md/use-display-buffer-alist))

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
    ;;(md/shackle-advise 'mu4e~main-view)
    ;;(md/shackle-advise 'mu4e-compose)
    ;;(md/shackle-advise 'mu4e-headers-search)
    (md/shackle-advise 'magit-dispatch-popup)
    (md/shackle-advise 'magit-display-buffer)
    ;;(md/shackle-advise 'edebug-pop-to-buffer)
    ;; (md/shackle-unadvise 'edebug-debugger)
    ;; (md/shackle-unadvise 'edebug)
    ;; (md/shackle-unadvise 'edebug-enter)

    ;;(defun md/mu4e-eyebrowse-quit (fn &rest args)
    ;;  (apply fn args)
    ;;  (shackle--eyebrowse-close-slot-by-tag "mail"))
    ;;(advice-add 'mu4e-quit :around 'md/mu4e-eyebrowse-quit '((name . "md/eyebrowse")))

    (defun md/is-edebug? (buffer)
      (interactive)
      (message (format "%s" edebug-mode)))

    ;; TODO ediff
    (dolist (rule
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

            ("\\`\\*xref.*?\\*\\'" :regexp t :align t :close-on-realign t :size 15 :select t)

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

            ;; lsp
            ("\\`\\*lsp-help.*?\\*\\'" :regexp t :align t :close-on-realign t :size 10 :select t)

            ("\\`\\*edit-indirect .*?\\*\\'" :regexp t :select t :same t)
            ('completion-list-mode :align t :close-on-realign t :size 0.33 :select t)
            ('compilation-mode :align t :close-on-realign t :size 0.33 :select t)
            ('inferior-scheme-mode :align t :close-on-realign t :size 0.33 :select t)
            ("*Warnings*" :align t :close-on-realign t :size 0.33 :select nil)
            ("*Messages*" :align t :close-on-realign t :size 0.33 :select nil)
            (".*emacs-scratch.*" :regexp t :align t :close-on-realign t :size 30 :select t) ;; TODO regex
            (".*init.org" :regexp t :same t :select t)
            (,neo-buffer-name :align left :close-on-realign t :size 25 :select t)
            ;;(,mu4e~main-buffer-name :eyebrowse "mail" :size 40 :select t :align left :close-on-realign t)
            ;;(,mu4e~headers-buffer-name :eyebrowse "mail" :select t :other t)
            ;;('mu4e-compose-mode :eyebrowse "mail" :select t :other t)
            (dired-mode :align nil :select t)))
          (add-to-list 'shackle-rules rule))

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

(dolist (this-minor-mode
         '(csv-field-index-mode
           diff-auto-refine-mode
           file-name-shadow-mode
           global-magit-file-mode
           mouse-wheel-mode
           treemacs-filewatch-mode
           treemacs-follow-mode
           treemacs-git-mode
           treemacs-fringe-indicator-mode))
  (when (fboundp this-minor-mode)
    (funcall this-minor-mode 0)))

(defun md/dotfiles-edit-init ()
  (interactive)
  (find-file (md/dotfiles-get-path "emacs.d.symlink/init.org")))

(bind-key "ve" 'md/dotfiles-edit-init md/leader-map)
(bind-key "vc" 'md/dotfiles-compile md/leader-map)

(defconst md/dotfiles-init-local-path "~/.local.el")

(md/maybe-native-compile-and-load md/dotfiles-init-local-path t)

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
    (when sock-val
      (setenv "SSH_AUTH_SOCK" sock-val))
    (when pid-val
      (setenv "SSH_AGENT_PID" pid-val))))

(md/set-ssh-agent-from-mac-keychain)

(defun md/org-clock-status ()
  "Return a string appropriate for my i3 status bar,
   showing the current clock time and the heading."
  (interactive)
  (if (not (org-clock-is-active))
      "-"
    (with-current-buffer (org-clock-is-active)  ; this returns the current clocking buffer
      (save-excursion
        (save-restriction
          (goto-char org-clock-marker)
          (org-back-to-heading t)
          (format "%s %s"
                  (format-seconds "%.2h:%.2m" (org-time-convert-to-integer (org-time-since org-clock-start-time)))
                  (s-truncate 60 (substring-no-properties (org-get-heading)))))))))

(use-package esup)
  ;;:defer 5)

(require 'server)
(when (not (server-running-p))
   (server-start))

(message "end of init.el")
