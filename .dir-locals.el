;; Support for compiling in subdirectories from Emacs. Adapted from Coq source.
((nil
  . ((eval
      . (progn
          ;; root directory (ending with slash)
          (let ((clerical-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (clerical-project-find-file
                 (and (boundp 'clerical-project-find-file) clerical-project-find-file)))

            ;; clerical tags file
            (when clerical-root-directory
              (setq tags-file-name (concat clerical-root-directory "TAGS"))
              (add-to-list 'compilation-search-path clerical-root-directory)
              ;; Setting the compilation directory to clerical root. This is
              ;; mutually exclusive with the setting of default-directory
              ;; below.
              (if (not clerical-project-find-file)
                  (setq compile-command (concat "make -C " clerical-root-directory)))
              )
            (setq clerical-executable (concat clerical-root-directory "clerical.native")))))))
 (tuareg-mode
  (show-trailing-whitespace . t))
 (clerical-mode
  (show-trailing-whitespace . t)))
