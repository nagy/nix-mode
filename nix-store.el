;;; nix-store.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'nix)
(require 'nix-log)
(require 'magit-section)
(eval-when-compile
  (require 'cl-lib))

(defgroup nix-store nil
  "Nix-store customizations."
  :group 'nix)

(defcustom nix-store-path-omit-self t
  "Do not list the current entry itself within sections of `nix-store-path-mode'."
  :package-version '(nix-mode . "1.6.0")
  :type 'boolean)

(defun nix-store-realise (path)
  "Realise a path asynchronously.
PATH the path within /nix/store to realise"
  (make-process
   :buffer nil
   :command (list nix-store-executable "--realise" path)
   :noquery t
   :name (format "*nix-store*<%s>" path)))

(defvar-local nix-buffer-store-path nil "Buffer-local object holding the variable `nix-store-path' object.")
(put 'nix-buffer-store-path 'permanent-local t)

(defclass nix-store-path (magit-section)
  ((path       :initarg :path        :accessor nix-store-path-path)
   (status     :initarg :status      :accessor nix-store-path-status)
   (hash       :initarg :hash        :accessor nix-store-path-hash)
   (size       :initarg :size        :accessor nix-store-path-size)
   (derivers   :initarg :derivers    :accessor nix-store-path-derivers)
   (outputs    :initarg :outputs     :accessor nix-store-path-outputs)
   (references :initarg :references  :accessor nix-store-path-references)
   (referrers  :initarg :referrers   :accessor nix-store-path-referrers)
   (requisites :initarg :requisites  :accessor nix-store-path-requisites))
  "Nix-Store-Path Class holds all information of the path that
is displayed")

(cl-defmethod nix-store-fill-data ((object nix-store-path))
  "Query the nix store via `nix-store-executable' and save that data into OBJECT."
  (oset object :size (nix-store--query 'size (nix-store-path-path object)))
  (oset object :hash (nix-store--query 'hash (nix-store-path-path object)))
  (oset object :derivers (nix-store--query 'deriver (nix-store-path-path object)))
  (oset object :outputs (nix-store--query 'outputs (nix-store-path-path object)))
  (oset object :referrers (nix-store--query 'referrers (nix-store-path-path object)))
  (oset object :requisites (nix-store--query 'requisites (nix-store-path-path object)))
  (oset object :references (nix-store--query 'references (nix-store-path-path object)))
  (oset object :status (file-exists-p (nix-store-path-path object)))
  object)

(cl-defun nix-store--query (argument &optional (path (nix-store-path-path nix-buffer-store-path)))
  "Query the nix-store for information.
ARGUMENT is given to the executable as an argument. See man page
`nix-store(1)' for possibilities. PATH is the store object that
is being queried. Runs `nix-store-executable' to get that information."
  (declare (side-effect-free t))
  (let ((nix-executable nix-store-executable))
    (cl-case argument
      (deriver
       ;; Special treatment for "derivers", we want to treat a single entry
       ;; with this string as an empty list
       (remove "unknown-deriver"
	       (nix--process-lines "--query" "--deriver" path)))
      (size (string-to-number (nix--process-string "--query" "--size" path)))
      (hash (nix--process-string "--query" "--hash" path))
      (requisites (nix--process-lines "--query" "--requisites" path))
      (references (nix--process-lines "--query" "--references" path))
      (referrers (nix--process-lines "--query" "--referrers" path))
      (outputs
       (ignore-errors
	 ;; This can fail for non-derivation paths
	 (nix--process-lines "--query" "--outputs" path )))
      (t (error "Unknown argument to nix-store --query: %s" argument)))))

(cl-defun nix-store-path-insert-path (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the path of STORE-PATH."
  (magit-insert-section (path (nix-store-path-path store-path))
    (magit-insert-heading (propertize (format "%-11s" "Path:") 'font-lock-face 'magit-section-heading)
      (propertize (oref store-path path)
		  'font-lock-face (if (file-exists-p (nix-store-path-path store-path))
				      'nix-store-path-realised-face
				    'nix-store-path-unrealised-face) ))))

(cl-defun nix-store-path-insert-size (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the size of STORE-PATH."
  (magit-insert-section (size (nix-store-path-size store-path))
    (magit-insert-heading (propertize (format "%-11s" "Size:") 'font-lock-face 'magit-section-heading)
      (format "%s" (file-size-human-readable (oref store-path size))))))

(cl-defun nix-store-path-insert-hash (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the hash of STORE-PATH."
  (magit-insert-section (hash (nix-store-path-hash store-path))
    (magit-insert-heading (propertize (format "%-11s" "Hash:") 'font-lock-face 'magit-section-heading)
      (format "%s" (oref store-path hash)))))

(cl-defun nix-store-path-insert-status (&optional (store-path nix-buffer-store-path))
  "Insert a section showing the status of STORE-PATH."
  (magit-insert-section (status (nix-store-path-status store-path))
    (magit-insert-heading (propertize (format "%-11s" "Status:") 'font-lock-face 'magit-section-heading)
      (if (nix-store-path-status store-path) "realised" "unrealised"))))

(defmacro nix-store--magit-insert-section-list (type value label)
  "Helper macro for inserting a list as a `magit-section'.
TYPE and VALUE will be used as the type and value of the section
respectively. The LABEL is the text displayed."
  `(let ((value (cl-remove
		 (and nix-store-path-omit-self (nix-store-path-path nix-buffer-store-path))
		 ,value :test #'equal)))
     (when (and (listp value) (> (length value) 0))
       (magit-insert-section (,type value)
	 (magit-insert-heading ,label)
	 (cl-loop for x in value
		  for exists = (file-exists-p x)
		  for face = (if exists 'nix-store-path-realised-face 'nix-store-path-unrealised-face)
		  do
		  (magit-insert-section (store-path x)
		    (insert (propertize (prog1 x
					  ;; (put-text-property
					  ;;  0 (+ 1 (length nix-store-dir))
					  ;;  'invisible 'nix-store-path x)
					  )
					'font-lock-face face) ?\n)))
	 (insert ?\n)
	 (magit-insert-child-count (magit-current-section))))))

(cl-defun nix-store-path-insert-derivers (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all derivers of STORE-PATH."
  (nix-store--magit-insert-section-list derivers (nix-store-path-derivers store-path) "Derivers:"))

(cl-defun nix-store-path-insert-outputs (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all outputs of STORE-PATH."
  (nix-store--magit-insert-section-list outputs (nix-store-path-outputs store-path) "Outputs:"))

(cl-defun nix-store-path-insert-references (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all references of STORE-PATH."
  (nix-store--magit-insert-section-list references (nix-store-path-references store-path) "References:"))

(cl-defun nix-store-path-insert-referrers (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all referrers of STORE-PATH."
  (nix-store--magit-insert-section-list referrers (nix-store-path-referrers store-path) "Referrers:"))

(cl-defun nix-store-path-insert-requisites (&optional (store-path nix-buffer-store-path))
  "Insert sections showing all requisites of STORE-PATH."
  (nix-store--magit-insert-section-list requisites (nix-store-path-requisites store-path) "Requisites:"))

(defcustom nix-store-path-headers-hook
  '(nix-store-path-insert-path
    nix-store-path-insert-status
    nix-store-path-insert-hash
    nix-store-path-insert-size)
  "Hook run to insert headers into the nix-store buffer.
A list of function that each take one argument, the store path object."
  :group 'nix-store
  :type 'hook
  :package-version '(nix-mode . "1.5.0")
  :options '(nix-store-path-insert-path
	     nix-store-path-insert-status
	     nix-store-path-insert-hash
	     nix-store-path-insert-size))

(defcustom nix-store-path-sections-hook
  '(nix-store-path-insert-derivers
    nix-store-path-insert-outputs
    nix-store-path-insert-references
    nix-store-path-insert-referrers
    nix-store-path-insert-requisites)
  "Hook run to insert sections into a nix-store buffer.
A list of function that each take one argument, the store path object."
  :group 'nix-store
  :type 'hook
  :package-version '(nix-mode . "1.5.0")
  :options '(nix-store-path-insert-derivers
	     nix-store-path-insert-outputs
	     nix-store-path-insert-references
	     nix-store-path-insert-referrers
	     nix-store-path-insert-requisites))

(defun nix-store-show-path (path)
  "Show a nix-store PATH.

If you want to change the order of the section lists (or even
implement your own ones) you can customize the variable
`nix-store-path-headers-hook' and
`nix-store-path-sections-hook'."
  (interactive "FNix-Store-Path: ")
  (setq path (expand-file-name (substring-no-properties path) default-directory))
  (switch-to-buffer (format "Nix Store Path: %s" (string-remove-prefix (concat nix-store-dir "/") path)))
  (unless (derived-mode-p 'nix-store-path-mode)
    (nix-store-path-mode)
    (setq nix-buffer-store-path (nix-store-fill-data (make-instance 'nix-store-path :path path))
	  list-buffers-directory path)
    (when (file-directory-p path)
      (setq default-directory path))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (store-path path)
	(magit-insert-headers 'nix-store-path-headers-hook)
	(magit-run-section-hook 'nix-store-path-sections-hook))
      (goto-char (point-min) )
      ;; (+nagy/hide-store-path)
      ;; (ov-set "/nix/store/" 'invisible t)
      ))
  (current-buffer))

(defun nix-store-path-at-point ()
  "Return the nix-store path at point."
  (or (magit-section-value-if 'store-path)
      (awhen (thing-at-point 'existing-filename)
	(substring-no-properties it))))

(defun nix-store-show-path-at-point ()
  "Opens the nix store path at point.

It uses \\[nix-store-show-path] to display the store path."
  (interactive)
  ;; the problem is, that this will not allow to show things like "./result"
  (aif (nix-store-path-at-point)
      (nix-store-show-path it)
    (user-error "No Nix-store-path at point")))

(defun nix-store-show-log ()
  "Opens the log file for the derivation of the nix-store path."
  (interactive)
  (if-let ((drv-path
	    (if (string-suffix-p ".drv" (nix-store-path-path nix-buffer-store-path))
		(nix-store-path-path nix-buffer-store-path)
	      (car (nix-store-path-derivers nix-buffer-store-path)))))
      (find-file (nix-log-path drv-path))
    (user-error "This store path has no associated derivation")))

(defvar nix-store-path-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nix-store-show-path-at-point)
    (define-key map (kbd "M-w") 'nix-store-path-kill)
    (define-key map (kbd "l") 'nix-store-show-log)
    map))

(defun nix-store--revert-buffer-function (&rest _ignore)
  "Helper function to be called by `revert-buffer'."
  (nix-store-show-path (nix-store-path-path nix-buffer-store-path)))

(define-derived-mode nix-store-path-mode magit-section-mode "Nix Store Path"
  :group 'nix-store
  (setq-local revert-buffer-function #'nix-store--revert-buffer-function)
  (read-only-mode 1))

(defun nix-store-path-kill ()
  "Kill store path."
  (interactive)
  (kill-new (nix-store-path-path nix-buffer-store-path))
  (message "%s" (nix-store-path-path nix-buffer-store-path)))

;;;###autoload
(defun nix-store-path-at-point ()
  (interactive)
  (let ((filename (thing-at-point-file-at-point)))
    (when (string-prefix-p nix-store-dir filename)
      filename)))

;;;###autoload
(with-eval-after-load 'thingatpt
  (push '(nix-store-path . nix-store-path-at-point) thing-at-point-provider-alist))

(defmacro with-output-to-nix-store (name &rest body)
  (declare (indent 0) (debug t))
  ;; TODO
  `(progn))

;; TODO BOOKMARK
;; TODO nix-store-path if buffer already exist, switch to it
;; TODO nix-store-path defcustom to enable trimming of the `nix-store-path`

;; TODO make nix-store-dir invisible using https://www.gnu.org/software/emacs/manual/html_node/elisp/Invisible-Text.html

(provide 'nix-store)
;;; nix-store.el ends here
