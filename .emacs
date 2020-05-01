;;;; Packages
(require 'package)
(require 'cl) ;; provides loop and return without cl- prefix

(add-to-list 'package-archives '("melpa"
	     . "http://melpa.org/packages/") t)

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
    ))

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

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

;;;; General settings

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen nil)

(setq-default indent-tabs-mode nil)

;; Theme
(load-theme 'manoj-dark t)

;; configuration for vertical line at 80 columns
(setq fci-rule-width 1)
(setq fci-rule-color "red")
(setq fci-rule-column 80)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)

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
  "Checks if POS is within the braces of a C++ \"enum class\"."
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

;; style checking - requires pyflakes and pep8 packages to be installed
(require 'flymake)
(load-library "flymake-cursor")

;; Script that flymake uses to check code. This script must be
;; present in the system path.
(setq pycodechecker "pychecker.sh")

(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(add-hook 'python-mode-hook 'flymake-mode)

(add-hook 'python-mode-hook 'auto-complete-mode)
(elpy-enable)

;; use ipython
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))


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
