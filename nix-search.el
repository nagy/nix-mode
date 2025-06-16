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

(defun nix-search--search (search file &optional no-cache use-flakes)
  (nix--process-json-nocheck "search" "--json"
			     (unless use-flakes "--file") file
			     (when no-cache "--no-cache")
			     (unless (string-empty-p search) search)))

(defface nix-search-pname
  '((t :weight bold))
  "Face used for package names."
  :group 'nix-mode)

(defface nix-search-version
  '((t))
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

(defun nix-search--refresh ()
  "Refresh Nix Search buffer"
  (interactive)
  (let ((results (nix-search--search nix-search--filter nix-search--file nil use-flakes)))
    (nix-search--display results (current-buffer) use-flakes nix-search--filter nix-search--file)))

(defun nix-search-create-keymap ()
  "Create the keymap associated with the Nix Search mode.")

(defun nix-search-create-menu ()
  "Create the Nix Search menu as shown in the menu bar."
  (let ((m '("Nix Search"
             ["Refresh" nix-search--refresh t])))
    (easy-menu-define nix-search-mode-menu nix-search-mode-map "Menu keymap for Nix mode" m)))

(nix-search-create-keymap)
(nix-search-create-menu)

(defvar-local nix-search--results nil)
(put 'nix-search--results 'permanent-local t)

(defun nix-search--display (results &optional display-buffer use-flakes search file)
  (unless display-buffer (setq display-buffer (generate-new-buffer "*nix search*")))
  (with-current-buffer display-buffer
    (setq nix-search--results
	  (cl-loop for x to (- (length results) 1)
		   for el = (cdr (elt results x))
		   collect
		   (list (number-to-string x)
			 (let-alist el
			   (vector
			    (propertize .pname 'face 'nix-search-pname)
			    (propertize .version 'face 'nix-search-version)
			    (propertize .description 'face 'nix-search-description))))))
    (nix-search-mode))
  (display-buffer display-buffer))

(define-derived-mode nix-search-mode tabulated-list-mode "Nix-Search"
  "Major mode for showing Nix search results."
  :interactive nil
  :group 'nix-mode
  (setq tabulated-list-format [("Name" 30 t)
			       ("Version" 30 t)
			       ("Description" 0 t)]) ;; last column takes what left
  (setq tabulated-list-entries nix-search--results)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (tabulated-list-print t))


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
