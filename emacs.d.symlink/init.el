(setq custom-file (concat (file-name-directory load-file-name) "scripts/custom.el"))
(load custom-file 'noerror)

(add-to-list 'load-path (file-name-directory load-file-name))

;; Update path with ./non-elpa and its top-level sub directories
(let ((base (concat (file-name-directory load-file-name) "non-elpa")))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(load "packages")

;; Load all files in ./scripts
(let ((base (concat (file-name-directory load-file-name) "scripts")))
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f "solarized.el")))
        (load-file name)))))

;; Load solarized last, so hooks trigger on startup
(load-file (concat (file-name-directory load-file-name) "scripts/solarized.el"))
