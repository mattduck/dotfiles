;; Change tab width to fit headline indents. Not working atm?
(add-hook 'org-mode-hook '(lambda () 
    (setq tab-width 2)
    (setq evil-shift-width 2)
    ))

;; Add timestamp when set task as done
(setq org-log-done t)

;; Remove line numbers, calculating hurts performance.
(defun nolinum ()
  (interactive)
  (message "Deactivated linum mode")
  (global-linum-mode 0)
  (linum-mode 0)
  (message "Deactivated line number mode")
  (line-number-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)

;; Think these are suggested shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Want the org faces to update whenever a solarized theme is
;; enabled. However, you can't set the faces outside of org mode without error.
;; So need to add an org-mode hook too, and suppress the errors
;; that are raised if you switch to solarized outside org mode.
;; There's probably a saner way.
(defun set-org-solarized-faces () 
  (with-demoted-errors 
    (set-face-attribute 'org-level-1 nil :foreground sol-blue)
    (set-face-attribute 'org-level-2 nil :foreground sol-yellow)
    (set-face-attribute 'org-level-3 nil :foreground sol-violet)
    (set-face-attribute 'org-level-4 nil :foreground sol-cyan)
    (set-face-attribute 'org-level-5 nil :foreground sol-blue)
    (set-face-attribute 'org-level-6 nil :foreground sol-yellow)
    (set-face-attribute 'org-level-7 nil :foreground sol-violet)
    (set-face-attribute 'org-level-8 nil :foreground sol-cyan)

    (set-face-attribute 'org-date nil :foreground sol-blue)
    (set-face-attribute 'org-upcoming-deadline nil :foreground sol-base03 :background sol-orange)
    (set-face-attribute 'org-scheduled nil :foreground sol-blue)
    (set-face-attribute 'org-scheduled-today nil :foreground sol-orange)
    (set-face-attribute 'org-scheduled-previously nil :foreground sol-blue)

    (set-face-attribute 'org-checkbox nil :foreground sol-yellow)
    (set-face-attribute 'org-tag nil :foreground sol-yellow)

    (set-face-attribute 'org-code nil :foreground sol-green)
    (set-face-attribute 'bold nil :foreground sol-orange)
    )
  )
(add-hook 'org-mode-hook 'set-org-solarized-faces)
(add-hook 'solarized-theme-hook 'set-org-solarized-faces)
