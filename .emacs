;; check emacs version
(when (version< emacs-version "27")
  (message "emacs version too old; skipping config")
  (with-current-buffer " *load*"
    (goto-char (point-max))))

;;;; Packages
(require 'package)
(require 'cl-lib)

(add-to-list 'package-archives '("melpa"
	     . "https://melpa.org/packages/") t)

(package-initialize)

;; see tutorials here:
;; https://realpython.com/blog/python/emacs-the-best-python-editor/
;; batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly
(defvar prelude-packages
  '(better-defaults
    elpy
    ein ;; emacs ipython notebook
    rainbow-delimiters
    fill-column-indicator
    fireplace
    magit
    rust-mode
    flycheck
    flycheck-pycheckers
    ))


(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
	when (not (package-installed-p p)) do (cl-return nil)
	finally (cl-return t)))

(unless (prelude-packages-installed-p)
  ;; check for new package versions
  (message "%s" "refreshing package database...")
  (package-refresh-contents)
  (message "%s" "done.")
  ;; install missing packages
  (dolist (p prelude-packages)
	     (when (not (package-installed-p p))
	       (package-install p))))
(provide 'prelude-packages)

;; look for libraries in home
(add-to-list 'load-path "~/emacs_libs")

;; add libraries
(load-library "google-c-style")
(require 'google-c-style)
(load-library "etags-select")
(require 'etags-select)

(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))


;;;; General settings

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen nil)

(setq-default indent-tabs-mode nil)

;; Theme
;; (load-theme 'manoj-dark t)
(load-theme 'tango-dark t)

;; configuration for vertical line at 80 columns
(setq fci-rule-width 1)
(setq fci-rule-color "red")
(setq fci-rule-column 80)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'rust-mode-hook 'fci-mode)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; line numbers
(global-linum-mode nil)
(add-hook 'c-mode-common-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'sh-mode-hook 'linum-mode)
(set-face-foreground 'linum "light blue")
(setq linum-format "%4d \u2502")

;; Org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Calendar settings
; set calendar to start on Monday
(setq calendar-week-start-day 1)
; add week number: from https://stackoverflow.com/questions/21364948/how-to-align-the-calendar-with-week-number-as-the-intermonth-text
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 0.7)
(setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'font-lock-keyword-face))


;; rainbow braces
(when (require 'rainbow-delimiters nil 'noerror)
  ;; for all programming modes
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;; fireplace
(setq fireplace-smoke-on t)

;;;; C++

;; open headers in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; fix for emacs not understanding enum class
;; https://gist.github.com/nschum/2626303
(defun inside-class-enum-p (pos)
  "Check if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (or (looking-back "enum\\s-+class\\s-+")
	  (looking-back "enum\\s-+class\\s-+\\S-+\\s-*:\\s-*")))))
;;      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

;; configuration for C++ style
(add-hook 'c-mode-common-hook 'google-set-c-style)


;;;; Python
(elpy-enable)


;;;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))


;;;; ctags
;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(setq path-to-ctags "/usr/bin/ctags")
(setq tags-table-list '("~/geopm/TAGS"))

(defun build-ctags ()
  (interactive) ; can be invoked with M-x
  (message "building project tags")
  (let ((root "~/geopm/")) ; hardcoded project path because it won't change much. could also use eproject
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat "~/geopm/" "TAGS")))
    (visit-tags-table tags-file) ; reload tags from disk
    (message (concat "Loaded " tags-file))))

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat "~/geopm/" "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))

(global-set-key (kbd "M-.") 'my-find-tag)
