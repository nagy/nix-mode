;;; nix-search.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'nix-instantiate)
(require 'nix-shell)
(require 'json)
(eval-when-compile
  (require 'let-alist))

;;;###autoload
(defun nix-search--search (search file &optional no-cache use-flakes)
  (nix--process-json-nocheck "search" "--json"
			     (unless use-flakes "--file") file
			     (when no-cache "--no-cache")
			     (unless (string= "" search) search)))

(defface nix-search-pname
  '((t :height 1.5
       :weight bold))
  "Face used for package names."
  :group 'nix-mode)

(defface nix-search-version
  '((((class color) (background dark))
     :foreground "light blue")
    (((class color) (background light))
     :foreground "blue"))
  "Face used for package version."
  :group 'nix-mode)

(defface nix-search-description
  '((t))
  "Face used for package description."
  :group 'nix-mode)

(defvar nix-search-mode-menu (make-sparse-keymap "Nix")
  "Menu for Nix Search mode.")

(defvar nix-search-mode-map (make-sparse-keymap)
  "Local keymap used for Nix Search mode.")

(defvar-local nix-search--filter nil
  "Search filter used for current buffer")
(defvar-local nix-search---file nil
  "File/flake used for current buffer")

(defun nix-search--refresh (&rest _args)
  "Refresh Nix Search buffer"
  (interactive)
  (let ((results (nix-search--search nix-search--filter nix-search--file nil use-flakes)))
    (nix-search--display results (current-buffer) use-flakes nix-search--filter nix-search--file)))

(let ((m '("Nix Search"
          ["Refresh" nix-search--refresh t])))
  (easy-menu-define nix-search-mode-menu nix-search-mode-map "Menu keymap for Nix mode" m))

(define-derived-mode nix-search-mode view-mode "Nix Search"
  "Major mode for showing Nix search results.

\\{nix-search-mode-map}"
  :interactive nil
  :group 'nix-mode
  (setq-local revert-buffer-function #'nix-search--refresh))

;;;###autoload
(defun nix-search--display (results &optional display-buffer use-flakes search file)
  (unless display-buffer (setq display-buffer (generate-new-buffer "*nix search*")))
  (with-current-buffer display-buffer
    (setq-local nix-search--filter search)
    (setq-local nix-search--file file)
    (unless (derived-mode-p 'nix-search-mode)
      (nix-search-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (entry results)
	(let-alist (cdr entry)
	  (let ((pname (if use-flakes .pname .pkgName))
	       (version .version)
	       (description .description))
	    (put-text-property 0 (length pname) 'face 'nix-search-pname pname)
	    (put-text-property 0 (length version) 'face 'nix-search-version version)
	    (put-text-property 0 (length description) 'face 'nix-search-description description)
	    (insert (format "%s (%s)\n%s\n" pname version description))
	    (insert (make-separator-line))
	    )))
      (goto-char (point-min))))
  (display-buffer display-buffer))

;;;###autoload
(defun nix-search (search &optional file display-buffer)
  "Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in."
  (interactive "snix-search> \n")
  (setq use-flakes (nix-has-flakes))
  (setq file (or file (if use-flakes (nix-read-flake) (nix-read-file))))
  (let ((results (nix-search--search search file nil use-flakes)))
    (when (called-interactively-p 'any)
      (nix-search--display results display-buffer use-flakes search file))
    results))

(defun nix-search-read-attr (file)
  "Read from a list of attributes.
FILE the nix file to look in."
  (let ((collection
	 (sort (mapcar (lambda (x) (symbol-name (car x)))
		       (nix-search "" file))
	       'string<))
	(read (cond ((fboundp 'ivy-read) 'ivy-read)
		    (t 'completing-read))))
    (funcall read "Attribute: " collection)))

(provide 'nix-search)
;;; nix-search.el ends here
