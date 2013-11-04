(setq custom-file "./scripts/custom.el")
(load custom-file 'noerror)

(add-to-list 'load-path' ".")

;; Update path with ./non-elpa and its top-level sub directories
(let ((base "./non-elpa"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(load "packages")
(load-dir "./scripts")

;; Load solarized last, so hooks trigger on startup
(load-file "./scripts/solarized.el")
