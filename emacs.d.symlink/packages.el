;; Taken from https://github.com/hans/dotfiles/blob/master/emacs.d/packages.el

(require 'package)

(setq package-archives '(
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")
    ))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(evil 
    ace-jump-mode 
    color-theme 
    fill-column-indicator
    undo-tree
    )
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Local
(load-file "../splitscreen/splitscreen.el")
