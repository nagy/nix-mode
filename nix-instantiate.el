;;; nix-instantiate.el --- Run nix commands -*- lexical-binding: t -*-

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix


;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'nix)
(require 'json)
(require 'dash)

(defun nix-instantiate--parsed (drv)
  "Get the parsed version of the .drv file.
DRV file to load from."
  (cdar
    (nix--process-json "show-derivation" drv)))

(defun nix-instantiate (nix-file &optional attribute parse args)
  "Run nix-instantiate on a Nix expression.
NIX-FILE the file to instantiate.
ATTRIBUTE an attribute of the Nix file to use.
PARSE whether to parse nix-instantiate output."
  (interactive (list (read-file-name "Nix file: ") nil nil))
  (let ((nix-executable nix-instantiate-executable)
	(argsargs (--mapcat (list "--arg"
				  (format "%s" (car it))
				  (format "%s" (cdr it)))
			    args)))
    (cl-multiple-value-bind (stdout stderr exitcode)
	(apply #'nix--process nix-file
	       `(,@(when attribute `("-A" ,attribute))
		 ,@argsargs))
      (setq stdout (string-remove-suffix "\n" stdout))
      (when (string-empty-p stdout)
	(error "nix-instantiate %s failed to produce any output: %s"
               nix-file stderr))
      (if parse
	  (nix-instantiate--parsed stdout)
	stdout))))

(defvar nix-instantiate--running-processes nil)

(defun nix-instantiate--sentinel (prop err proc event)
  "Make a nix-instantiate process.
PROP the prop name of nix-instantiate--running-processes.
ERR the error buffer.
PROC the process that has been run.
EVENT the event that was fired."
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer proc)
      (unless (eq (buffer-size) 0)
	(let ((drv (nix-instantiate--parsed
		    (substring (buffer-string) 0 (- (buffer-size) 1)))))
	  (dolist
	      (callback (lax-plist-get nix-instantiate--running-processes prop))
	    (funcall callback drv)))))
    (setq nix-instantiate--running-processes
	  (lax-plist-put nix-instantiate--running-processes prop nil)))
  (unless (process-live-p proc)
    (kill-buffer (process-buffer proc))
    (kill-buffer err)))

(defun nix-instantiate-async (callback nix-file &optional attribute)
  "Run nix-instantiate on a Nix expression, asynchronously.
CALLBACK the function to call when instantiate completes.
NIX-FILE the file to instantiate
ATTRIBUTE an attribute of the Nix file to use."
  (setq nix-file (expand-file-name nix-file))
  (let* ((prop (if attribute
		   (expand-file-name attribute nix-file) nix-file))
	 (data (lax-plist-get nix-instantiate--running-processes prop))
	 (stdout (generate-new-buffer "nix-instantiate"))
	 (stderr (generate-new-buffer "nix-instantiate error")))
    (setq nix-instantiate--running-processes
	  (lax-plist-put nix-instantiate--running-processes
			 prop (cons callback data)))
    (make-process
     :name "nix-instantiate"
     :buffer stdout
     :command (append (list nix-instantiate-executable nix-file)
		      (when attribute (list "-A" attribute)))
     :noquery t
     :sentinel (apply-partially #'nix-instantiate--sentinel prop stderr)
     :stderr stderr)))

(provide 'nix-instantiate)
;;; nix-instantiate.el ends here
