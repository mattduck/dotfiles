;; Set tabs to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert tab)

;; Wrap text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
    '(lambda() (set-fill-column 80)))

;; Auto indent code
(electric-indent-mode 1)

;; Python indentation is handled differently - taken from
;; www.emacswiki.org/emacs/AutoIndentation

     ;; Ignoring electric indentation
     (defun electric-indent-ignore-python (char)
       "Ignore electric indentation for python-mode"
       (if (equal major-mode 'python-mode)
         `no-indent'
         nil))
    (add-hook 'electric-indent-functions
              'electric-indent-ignore-python)

    ;; Enter key executes newline-and-indent
    (defun set-newline-and-indent ()
      "Map the return key with `newline-and-indent'"
      (local-set-key (kbd "RET") 'newline-and-indent))
    (add-hook 'python-mode-hook 'set-newline-and-indent)

    ;; This seems to indent org mode in odd way. Fix to disable it from
    ;; foldl.me/2012/disabling-electric-indent-mode/
    (add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'no-indent)))))

