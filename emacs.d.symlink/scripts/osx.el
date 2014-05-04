;; Map modifier keys so that all are accessible, but the left option key
;; is kept free so can use it for character modifications, eg. alt+3 = #. 
;;
;; CTRL = ctrl
;; LEFT ALT = none
;; COMMAND = meta
;; RIGHT ALT = super

(if (not (eq system-type 'darwin))
  (setq ns-option-modifier nil)
  (setq ns-command-modifier 'meta)
  (setq ns-right-option-modifier 'super)
  )
