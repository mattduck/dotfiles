(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(autothemer-deftheme md-wiki "My wiki theme"
  ((((class color) (min-colors #xFFFFFF)))  ;; 24 bit display support. I haven't added other
   ;; displays yet.

    ;; Specify the color palette for each of the classes above.
    (md/background   "#fefefe")
    (md/default   "#504945")
    ;; (md/default   "#555")
    ;; (md/hl "#1d2021")
    (md/hl "black")
    ;; (md/hide "#ebdbb2")
    (md/hide "#AAACCC")
    ;; (md/href   "#5f8787")
    (md/href   "#222FFF")
    ;; (md/href "#0487D9")
    ;; (md/href "#5B9199")

    (md/serif "EtBookOT")
    (md/sans "Inconsolata") ;; TODO: change to something variable width sans
    (md/fixed "Courier New")
    )

  (
   ;; basics
   (default (:foreground md/default :background md/background :family md/serif))
   (bold (:foreground md/hl :weight 'bold))
   (fringe                                    (:background md/background))
   (hl-line                                   (:background md/href))
   (region                                    (:background md/hl :foreground md/background)) ;;selection
   (secondary-selection                       (:background md/hl))
   (minibuffer-prompt                         (:foreground md/href :bold t))
   (vertical-border                           (:foreground md/hl))
   (window-divider                            (:foreground md/hl))
   (link                                      (:foreground md/href :underline t))
   (shadow                                    (:foreground md/hide))
   (error  (:foreground md/hl))
   (success                                           (:foreground md/hl :bold t))
   (warning                                           (:foreground md/hl :bold t))
   (trailing-whitespace                               (:background md/default))
   (escape-glyph                                      (:foreground md/hl))
   (header-line                                       (:foreground md/hl :box nil :inherit nil))
   (highlight                                         (:foreground md/hl))
   (homoglyph                                         (:foreground md/hl))
   (match                                             (:foreground md/background :background md/href))

   (font-lock-keyword-face                            (:foreground md/hl :bold t))
   (font-lock-builtin-face                            (:foreground md/hl :bold t))
   (font-lock-constant-face                           (:foreground md/href :bold t))
   (font-lock-type-face                               (:foreground md/href :bold t))
   (font-lock-string-face                             (:foreground md/href :family md/fixed :bold t))
   (font-lock-function-name-face                      (:foreground md/hl :italic t :bold nil))
   (font-lock-variable-name-face                      (:foreground md/hl :italic t :bold t))
   (font-lock-comment-face                            (:foreground md/hide))
   (font-lock-warning-face                            (:foreground md/hl :bold t :italic t))

   (fic-face (:foreground md/hl :background md/background))

   ;; outline-mode
   (outline-1                               (:foreground md/hl :height 1.5 :bold t))
   (outline-2                               (:foreground md/hl :height 1.4))
   (outline-3                               (:foreground md/hl :height 1.3 :family md/sans :bold t))
   (outline-4                               (:foreground md/hl :height 1.3 :family md/sans))
   (outline-5                               (:foreground md/hl :height 1.1 :family md/sans :bold t))
   (outline-6                               (:foreground md/hl))
   (outline-7                               (:foreground md/hl))
   (outline-8                               (:foreground md/hl))

   ;; org-mode
   (org-hide                                  (:foreground md/background :family md/fixed))  ;; Should be invisible
   (org-indent                               (:foreground md/background :family md/fixed))  ;; Should be invisible
   (org-level-1                               (:inherit 'outline-1))
   (org-level-2                               (:inherit 'outline-2))
   (org-level-3                               (:inherit 'outline-3))
   (org-level-4                               (:inherit 'outline-4))
   (org-level-5                               (:inherit 'outline-5 ))
   (org-level-6                               (:inherit 'outline-6))
   (org-level-7                               (:inherit 'outline-7))
   (org-level-8                               (:inherit 'outline-8))
   (org-special-keyword                       (:foreground md/hide :family md/fixed))
   (org-drawer                                (:foreground md/default))
   (org-block                                 (:foreground md/default))
   (org-block-begin-line                      (:foreground md/hide :family md/fixed :height 0.6))
   (org-block-end-line                        (:foreground md/hide :family md/fixed :height 0.6))
   (org-column                                (:background md/hl))
   (org-column-title                          (:background md/hl :underline t :weight 'bold))
   (org-warning                               (:foreground md/hl :weight 'bold :underline nil :bold t))
   (org-archived                              (:foreground md/hl :weight 'bold))
   (org-link                                  (:foreground md/href :underline t))
   (org-footnote                              (:foreground md/href :underline t))
   (org-ellipsis                              (:foreground md/default))
   (org-date                                  (:foreground md/href :underline t))
   (org-sexp-date                             (:foreground md/href :underline t))
   (org-code                                  (:foreground md/hide :family md/fixed :height 0.9))
   (org-tag                                   (:foreground md/href :bold t :weight 'bold))
   (org-list-dt                               (:foreground md/href :bold t :weight 'bold))
   (org-todo                                  (:foreground md/hl :weight 'bold :bold nil))
   (org-done                                  (:foreground md/hl :weight 'bold :bold nil))
   (org-agenda-done                           (:foreground md/hl))
   (org-headline-done                         (:foreground md/hl))
   (org-checkbox                              (:foreground md/hl :bold t))
   (org-table                                 (:foreground md/hl))
   (org-formula                               (:foreground md/hl))
   (org-document-title                        (:foreground md/href))
   (org-document-info                         (:foreground md/href))
   (org-scheduled                             (:foreground md/href))
   (org-scheduled-today                       (:foreground md/href))
   (org-scheduled-previously                  (:foreground md/hl))
   (org-upcoming-deadline                     (:foreground md/href))
   (org-deadline-announce                     (:foreground md/hl))
   (org-quote                     (:foreground md/hl :italic t :size 0.9))

   ;; mode-line
   (mode-line                                 (:background md/background
                                                           :foreground md/hl
                                                           :inverse-video nil
                                                           :box (:line-width 1 :color md/hl :style 'released-button)
                                                           :underline nil))
   (mode-line-inactive                        (:background md/background
                                                           :inverse-video nil
                                                           :foreground md/hl
                                                           :box (:line-width -1 :color md/hl :style nil)
                                                           :underline nil))
   (powerline-active0                         (:background md/background :foreground md/hl :inherit 'mode-line))
   (powerline-active1                         (:background md/background :foreground md/hl :italic nil :inherit 'mode-line))
   (powerline-active2                         (:background md/background :foreground md/hl :inherit 'mode-line))
   (powerline-inactive0                       (:inherit 'mode-line-inactive :foreground md/hl))
   (powerline-inactive1                       (:inherit 'mode-line-inactive :foreground md/hl))
   (powerline-inactive2                       (:inherit 'mode-line-inactive :foreground md/hl))
   (md/powerline-inactive                     (:inherit 'mode-line-inactive :foreground md/hl))
   (md/powerline-normal                       (:background md/background :foreground md/hl :inherit 'mode-line))
   (md/powerline-insert                       (:background md/background :foreground md/hl :inherit 'mode-line))
   (md/powerline-visual                       (:background md/background :foreground md/hl :inherit 'mode-line))
   (md/powerline-replace                      (:background md/background :foreground md/hl :inherit 'mode-line))
   (md/powerline-emacs                        (:background md/background :foreground md/hl :inherit 'mode-line))
   (md/modeline-flycheck-warning              (:background md/background :foreground md/hl :inherit 'mode-line))
   (md/modeline-flycheck-error                (:background md/background :foreground md/hl :inherit 'mode-line))

   ))

  ;; )
  ;; TODO this seems to be enacted even when the theme isn't enabled, and doesn't reset when I go to
  ;; other themes.
  ;; (custom-theme-set-variables
  ;;  'md-wiki
  ;;  ;; '(md/wiki-test "intheme")))
  ;;  '(line-spacing 0.5)
  ;;  '(left-margin-width 10)  ;; TODO: should be separate, buffer local.
  ;;  '(right-margin-width 2)  ;; TODO: should be separate, buffer local.
  ;;  '(header-line-format " "))  ;; TODO should be separate, buffer local.
  ;;  (set-window-buffer nil (current-buffer)))

;; Forms after the face specifications are evaluated.
;; (palette vars can be used, read below for details.)
;; (custom-theme-set-variables 'example-name
;;     `(ansi-color-names-vector [,example-red
;;                                ,example-green
;;                                ,example-blue
;;                                ,example-purple
;;                                ,example-yellow
;;                                ,example-orange
;;                                ,example-cyan])))
;;)

(provide-theme 'md-wiki)

;; TODO
;; - [ ] why does it apply the font as soon as this file is loaded? surely that shouldn't happen.
;; - [ ] why does the font stay even after the theme has been disabled? Perhaps because it's been
;; loaded outside the theme??
