(require 'evil)
(evil-mode 1) 

;; Use undo-tree so don't have to figure out the full undo/redo system atm.
;; Evil will detect this. 
(require 'undo-tree)

;; Remap ";" to ":" and "," to ";", like vimrc
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map "," 'evil-repeat-find-char)

;; Make <esc> cancel everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Closest thing to easymotion
(require 'ace-jump-mode)
(setq ace-jump-mode-scope 'window) ;; it's quicker this way
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-line-mode)
