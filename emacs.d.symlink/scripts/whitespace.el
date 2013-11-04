;; Set tabs to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert tab)

;; Wrap text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
    '(lambda() (set-fill-column 80)))

