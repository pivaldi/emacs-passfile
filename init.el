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
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-scratch-message nil
 enable-local-variables nil
 enable-local-eval nil
 tramp-mode nil)

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

;;;###autoload
(defun epf-password-generate (&optional len)
  "Generate a good password with `password-generator-paranoid'.

Overwrite the length of the password with an integer prefix arguments."
  (interactive "P")
  (password-generator-paranoid len))

(epf-packages-maybe-install)

(add-to-list 'auto-mode-alist '("\\.gpg\\'" . hidepw-mode))

(setq custom-file (format "%s/custom.el" (directory-file-name user-emacs-directory)))

(dolist (f `(,custom-file "configure.el"))
  (when (file-exists-p f)
    (load f)))

(provide 'init)
;;; init.el ends here
