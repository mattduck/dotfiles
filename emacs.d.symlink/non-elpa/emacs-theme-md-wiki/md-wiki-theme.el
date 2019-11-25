(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

;; TODO add dark + light support

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

                      ;; Attempt 1
                      ;; (md/bg   "#fefefe")
                      ;; (md/default   "#504945")
                      ;; (md/hl "#222222")
                      ;; (md/hide "#AAACCC")
                      ;; (md/blue   "#222FFF")
                      ;; (md/red   "#FF2222")

                      ;; Flat UI - Adobe Color
                      ;; I like the idea of something plain-ish with blue/red highlights.
                      (md/bg   "#ecf0f1")
                      (md/default   "#2c3e50")
                      (md/hl "#222222")
                      (md/hide "#bbbebf")
                      (md/red "#e74c3c")
                      (md/blue   "#3498db")

                      ;; Copy of Mag Book
                      ;; (md/bg   "#f0f4f5")
                      ;; (md/default   "#4b4c4d")
                      ;; (md/hl "#333333")
                      ;; (md/hide "#c8cbcc")
                      ;; (md/red "#df0413")
                      ;; (md/blue   "#80a2db")

                      (md/serif "ETBookOT")
                      (md/sans "MerriweatherSans") ;; TODO: change to something variable width sans
                      (md/fixed "Inconsolata")
                      )

                     (
                      ;; basics
                      (default (:foreground md/default :background md/bg :family md/serif :weight 'normal))
                      (bold (:foreground md/hl :weight 'bold))
                      (fringe                                    (:background md/bg))
                      (hl-line                                   (:background md/blue))
                      (region                                    (:background md/hl :foreground md/bg)) ;;selection
                      (secondary-selection                       (:background md/hl :foreground md/hide))
                      (minibuffer-prompt                         (:foreground md/blue :bold t))
                      (vertical-border                           (:foreground md/hl))
                      (window-divider                            (:foreground md/hl))
                      (link                                      (:foreground md/blue :underline t))
                      (shadow                                    (:foreground md/hide))
                      (error  (:foreground md/hl))
                      (success                                           (:foreground md/hl :bold t))
                      (warning                                           (:foreground md/hl :bold t))
                      (trailing-whitespace                               (:background md/default))
                      (escape-glyph                                      (:foreground md/hl))
                      (header-line                                       (:foreground md/hl :box nil :inherit nil))
                      (highlight                                         (:foreground md/hl))
                      (homoglyph                                         (:foreground md/hl))
                      (match                                             (:foreground md/bg :background md/red))

                      (font-lock-keyword-face                            (:foreground md/hl :bold t))
                      (font-lock-builtin-face                            (:foreground md/hl :bold t))
                      (font-lock-constant-face                           (:foreground md/blue :bold t))
                      (font-lock-type-face                               (:foreground md/blue :bold t))
                      (font-lock-string-face                             (:foreground md/red :bold t))
                      (font-lock-function-name-face                      (:foreground md/hl :italic t :bold nil))
                      (font-lock-variable-name-face                      (:foreground md/hl :italic t :bold t))
                      (font-lock-comment-face                            (:foreground md/hide))
                      (font-lock-warning-face                            (:foreground md/hl :bold t :italic t))

                      (fic-face (:foreground md/hl :background md/bg))

                      ;; outline-mode
                      (outline-1                               (:foreground md/hl :height 1.5 :family md/sans :weight 'normal))
                      (outline-2                               (:foreground md/hl :height 1.4 :family md/sans :weight 'normal))
                      (outline-3                               (:foreground md/hl :height 1.3 :family md/sans :weight 'normal))
                      (outline-4                               (:foreground md/hl :height 1.3 :family md/sans :weight 'normal))
                      (outline-5                               (:foreground md/hl :height 1.2 :family md/sans :weight 'normal))
                      (outline-6                               (:foreground md/hl :height 1.2 :family md/sans :weight 'normal))
                      (outline-7                               (:foreground md/hl :height 1.1 :family md/sans :weight 'normal))
                      (outline-8                               (:foreground md/hl :height 1.1 :family md/sans :weight 'normal))

                      ;; org-mode
                      (org-hide                                  (:foreground md/bg :family md/fixed))  ;; Should be invisible
                      (org-indent                               (:foreground md/bg :family md/fixed))  ;; Should be invisible
                      (org-level-1                               (:inherit 'outline-1))
                      (org-level-2                               (:inherit 'outline-2))
                      (org-level-3                               (:inherit 'outline-3))
                      (org-level-4                               (:inherit 'outline-4))
                      (org-level-5                               (:inherit 'outline-5 ))
                      (org-level-6                               (:inherit 'outline-6))
                      (org-level-7                               (:inherit 'outline-7))
                      (org-level-8                               (:inherit 'outline-8))
                      (org-special-keyword                       (:foreground md/hide :family md/fixed :height 0.6))
                      (org-property-value                                (:foreground md/hide :height 0.6))
                      (org-drawer                                (:foreground md/default))
                      (org-block                                 (:foreground md/default :family md/fixed))
                      (org-block-begin-line                      (:foreground md/hide :family md/fixed :height 0.6))
                      (org-block-end-line                        (:foreground md/hide :family md/fixed :height 0.6))
                      (org-column                                (:background md/hl))
                      (org-column-title                          (:background md/hl :underline t :weight 'bold))
                      (org-warning                               (:foreground md/hl :weight 'bold :underline nil :bold t))
                      (org-archived                              (:foreground md/hl :weight 'bold))
                      (org-link                                  (:foreground md/blue :underline t))
                      (org-footnote                              (:foreground md/blue :underline t))
                      (org-ellipsis                              (:foreground md/default))
                      (org-date                                  (:foreground md/blue :underline t :family md/fixed :height 0.8))
                      (org-sexp-date                             (:foreground md/blue :underline t))
                      (org-code                                  (:foreground md/red :family md/fixed :height 0.9))
                      (org-tag                                   (:family md/fixed :foreground md/blue :bold t :weight 'normal :height 0.8))
                      (org-list-dt                               (:foreground md/blue :bold t :weight 'bold))
                      (org-todo                                  (:foreground md/hl :weight 'bold :bold nil))
                      (org-done                                  (:foreground md/hl :weight 'bold :bold nil))
                      (org-agenda-done                           (:foreground md/hl))
                      (org-headline-done                         (:foreground md/hl))
                      (org-checkbox                              (:foreground md/hl :bold t))
                      (org-table                                 (:foreground md/hl))
                      (org-formula                               (:foreground md/hl))
                      (org-document-title                        (:foreground md/blue))
                      (org-document-info                         (:foreground md/blue))
                      (org-scheduled                             (:foreground md/blue))
                      (org-scheduled-today                       (:foreground md/blue))
                      (org-scheduled-previously                  (:foreground md/hl))
                      (org-upcoming-deadline                     (:foreground md/blue))
                      (org-deadline-announce                     (:foreground md/hl))
                      (org-quote                     (:family md/fixed :foreground md/default :italic nil :size 0.7))

                      ;; mode-line
                      (mode-line                                 (:background md/bg
                                                                              :foreground md/hl
                                                                              :inverse-video nil
                                                                              :box (:line-width 1 :color md/hl :style 'released-button)
                                                                              :underline nil))
                      (mode-line-inactive                        (:background md/bg
                                                                              :inverse-video nil
                                                                              :foreground md/hl
                                                                              :box (:line-width -1 :color md/hl :style nil)
                                                                              :underline nil))
                      (powerline-active0                         (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (powerline-active1                         (:background md/bg :foreground md/hl :italic nil :inherit 'mode-line))
                      (powerline-active2                         (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (powerline-inactive0                       (:inherit 'mode-line-inactive :foreground md/hl))
                      (powerline-inactive1                       (:inherit 'mode-line-inactive :foreground md/hl))
                      (powerline-inactive2                       (:inherit 'mode-line-inactive :foreground md/hl))
                      (md/powerline-inactive                     (:inherit 'mode-line-inactive :foreground md/hl))
                      (md/powerline-normal                       (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (md/powerline-insert                       (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (md/powerline-visual                       (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (md/powerline-replace                      (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (md/powerline-emacs                        (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (md/modeline-flycheck-warning              (:background md/bg :foreground md/hl :inherit 'mode-line))
                      (md/modeline-flycheck-error                (:background md/bg :foreground md/hl :inherit 'mode-line))

                      ;;which-key
                      (which-key-key-face (:family md/fixed :foreground md/blue))
                      (which-key-separator-face (:family md/fixed))
                      (which-key-command-description-face (:family md/fixed))
                      (which-key-local-map-description-face (:family md/fixed))
                      (which-key-highlighted-command-face (:family md/fixed))
                      (which-key-group-description-face (:family md/fixed :foreground md/hl :weight 'bold))
                      (which-key-special-key-face (:family md/fixed))
                      (which-key-docstring-face (:family md/fixed))

                      ;; elfeed
                      (elfeed-search-title-face (:family md/fixed :foreground md/hide))
                      (elfeed-search-unread-title-face (:family md/fixed :foreground md/hl :weight 'bold))
                      (elfeed-search-feed-face (:family md/fixed :foreground md/blue))
                      (elfeed-search-tag-face (:family md/fixed :foreground md/hl))
                      (elfeed-log-date-face (:family md/fixed :foreground md/default))

                      ;; helm
                      (helm-source-header (:inherit 'default :family md/fixed :background md/hide))
                      (helm-header (:inherit 'default :family md/fixed :background md/hide))
                      (helm-visible-mark (:inherit 'default :family md/fixed))
                      (helm-candidate-number (:inherit 'default :family md/fixed))
                      (helm-candidate-number-suspended (:inherit 'default :family md/fixed))
                      (helm-selection (:inherit 'default :family md/fixed :background md/hl :foreground md/bg))
                      (helm-selection-line (:inherit 'default :family md/fixed))
                      (helm-buffer-file (:inherit 'default :family md/fixed))
                      (helm-non-file-buffer (:inherit 'default :family md/fixed))
                      (helm-separator (:inherit 'default :family md/fixed))
                      (helm-action (:inherit 'default :family md/fixed))
                      (helm-prefarg (:inherit 'default :family md/fixed))
                      (helm-match (:inherit 'default :family md/fixed :foreground md/red))
                      (helm-header-left-margin (:inherit 'default :family md/fixed))
                      (helm-ff-prefix (:inherit 'default :family md/fixed))
                      (helm-ff-executable (:inherit 'default :family md/fixed))
                      (helm-ff-directory (:inherit 'default :family md/fixed :foreground md/blue))
                      (helm-ff-dirs (:inherit 'helm-ff-directory))
                      (helm-ff-dotted-directory (:inherit 'default :family md/fixed :foreground md/hide))
                      (helm-ff-symlink (:inherit 'default :family md/fixed))
                      (helm-ff-invalid-symlink (:inherit 'default :family md/fixed :foreground md/hl :background md/red))
                      (helm-ff-file (:inherit 'default :family md/fixed))
                      (helm-M-x-key (:inherit 'default :family md/fixed :foreground md/red))
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
