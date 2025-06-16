;;; nix-nar.el --- Open nar files in virtual dired -*- lexical-binding: t -*-

;; Author: Daniel Nagy <danielnagy@posteo.de>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'dired-x)
(require 'pcase)
(require 'subr-x)

(require 'nix)

(defun nix-nar--process (&optional buffer)
  "Pipe BUFFER through \"nix nar ls -lR\".
Return the output of the process as a list of strings."
  (cl-check-type buffer buffer "Non-buffer nar input not implemented yet")
  (cl-assert (zerop (call-process-region nil nil nix-executable t buffer nil "nar" "ls" "--long" "--recursive" "/dev/stdin" "")))
  (split-string (buffer-string) "\n"))

(defvar nix-nar-keymap
  (let ((map (make-keymap)))
    (set-keymap-parent map dired-mode-map)
    (define-key map [remap dired-find-file] #'nix-nar-find-file)
    map)
  "Keymap to use when displaying a nix-nar file.")

;;;###autoload
(defun nix-nar-mode ()
  "Display the contents of a nar file via `virtual-dired'."
  (interactive)
  (read-only-mode 1)
  (with-silent-modifications
    (let ((split (mapcar #'split-string (nix-nar--process (current-buffer)))))
      (erase-buffer)
      (insert (format "  %s:" (buffer-file-name)) ?\n)
      (pcase-dolist (`(,mode ,size ,path) split)
	(when mode
	  (insert (format "  %s 1 root root   %11s Jan  1  1970 %s\n"
			  mode size (string-remove-prefix "./" path)))))
      (virtual-dired "/")
      (use-local-map nix-nar-keymap))))

(defun nix-nar-find-file ()
  "Open the nar entry in a new buffer."
  (interactive)
  (let ((nar (buffer-file-name))
	(path (string-remove-prefix (buffer-file-name) (car (dired-get-marked-files)))))
    (switch-to-buffer (generate-new-buffer (format "%s/%s" nar path)))
    (read-only-mode 1)
    (with-silent-modifications
      (call-process nix-executable nil t nil "nar" "cat" nar path)
      (goto-char (point-min))
      (set-auto-mode))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nar\\'" . nix-nar-mode))

;;###autoload
;; (add-to-list 'auto-coding-alist ....)
;; (add-to-list 'file-coding-alist ....)

(provide 'nix-nar)
;;; nix-nar.el ends here
