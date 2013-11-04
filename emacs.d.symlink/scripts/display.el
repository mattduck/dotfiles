(set-default-font "Inconsolata-15:antialias=subpixel")

;; Colour column. Not sure if this slows down org mode.
(require 'fill-column-indicator)
(setq fci-rule-width 5) ; 5 seems to be max width
(add-hook 'solarized-theme-hook '(lambda () (setq fci-rule-color sol-base02)))
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;; Line and Column number in ruler
(line-number-mode 1)
(column-number-mode 1)

;; Highlight cursor line
(global-hl-line-mode 1)

;; Set number
(require 'linum)
(global-linum-mode t)
