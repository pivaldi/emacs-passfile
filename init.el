;;; init.el Store --- All your secrets in a simple file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Philippe IVALDI
;;
;; Author: Philippe IVALDI <emacs@MY-NAME.me>
;; Maintainer: Philippe IVALDI <emacs@MY-NAME.me>
;; Created: August 25, 2025
;; Version: 0.0.1
;; Keywords: emacs password manager
;; Homepage: https://github.com/pivaldi/emacs-passfile
;; Package-Requires: ((emacs "30") (password-generator "1.20") (hidepw "0.3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(setq
 ;; No disturbation
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-scratch-message nil

 ;; Security enhancement
 enable-local-variables nil
 enable-local-eval nil
 tramp-mode nil

 ;; Improve font lock behavior
 font-lock-maximum-decoration t
 jit-lock-contextually t
 jit-lock-context-time 0.1)

(setenv "GPG_AGENT_INFO" nil)

(require 'package)
(setq package-enable-at-startup t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar epf-packages-list
  '(password-generator
    hidepw))

(defvar epf-package-refresh-done nil)

(defun epf-package-maybe-install(pkg)
  "Check and potentially install `PKG'."
  (if (not (package-installed-p pkg))
      (if (not (require pkg nil t))
          (progn
            (when (not epf-package-refresh-done)
              (package-refresh-contents)
              (setq epf-package-refresh-done t))
            (package-install pkg))
        (message "%s is already installed OUT OF the Emacs package manager" pkg))
    (message "%s is already installed by the Emacs package manager but not checked for update..." pkg)))

(defun epf-packages-maybe-install()
  "Refresh package manifest to the defined set."
  (interactive)
  (mapc 'epf-package-maybe-install epf-packages-list))

(defun epf-font-lock-extend-region ()
  "Extend the search region to include hidden block.
Hidden blocks are between #+hidden and #-hidden."
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "#\\+hidden\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "#-hidden$" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

(eval-after-load 'hidepw
  (lambda nil
    (add-hook 'hidepw-mode (lambda nil (set (make-local-variable 'font-lock-multiline) t)))
    (setq hidepw-patterns
          '("#\\+hidden\n\\([^]*?\\)\n#-hidden$"
            " \\[\\(.+\\)\\] ?"
            "^\\[\\(.+\\)\\] ?"
            "[pP]wd[  ]?: \\([^ \n]+\\)"
            "[pP]assword[  ]?: \\(.+\\)$"
            "[uU]ser[  ]?: \\([^ \n]+\\)"
            "[uU]sername[  ]?: \\(.+\\)"))
    (add-hook 'font-lock-extend-region-functions
              'epf-font-lock-extend-region)))

;;;###autoload
(defun epf-password-generate (&optional len return)
  "Wrapper of `password-generator-paranoid'."
  (interactive)
  (password-generator-paranoid len return))

(epf-packages-maybe-install)

(add-to-list 'auto-mode-alist '("\\.gpg\\'" . hidepw-mode))

(setq custom-file (format "%s/custom.el" (directory-file-name user-emacs-directory)))
(defvar config-file (format "%s/configure.el" (directory-file-name user-emacs-directory)))

(dolist (f `(,custom-file ,config-file))
  (when (file-exists-p f)
    (load f)))

(provide 'init)
;;; init.el ends here
