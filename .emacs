; look for libraries in home
(add-to-list 'load-path "~/emacs_libs")

; add libraries
(load-library "fill-column-indicator")
(require 'fill-column-indicator)
(load-library "google-c-style")
(require 'google-c-style)
(load-library "etags-select")
(require 'etags-select)


;;;; General settings

; configuration for vertical line at 80 columns
(setq fci-rule-width 1)
(setq fci-rule-color "red")
(setq fci-rule-column 80)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)

;delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;; C++

; open headers in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; fix for emacs not understanding enum class
; https://gist.github.com/nschum/2626303
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (or (looking-back "enum\\s-+class\\s-+")
	  (looking-back "enum\\s-+class\\s-+\\S-+\\s-*:\\s-*")))))
;      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

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


; configuration for C++ style
(add-hook 'c-mode-common-hook 'google-set-c-style)


;;;; Python

; style checking - requires pyflakes package to be installed
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


;;;; ctags
; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
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
  (if (file-exists-p (concat "~/cnr/" "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))

(global-set-key (kbd "M-.") 'my-find-tag)

;;;; Theme

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (manoj-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
