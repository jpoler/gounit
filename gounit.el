;;; go-unit.el --- Convenient wrapper for running go unit tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Jonathan Poler

;; Author: Jonathan Poler <jonathan.poler@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; go-unit.el provides a convenient interface for running go unit tests.
;;
;; This package provides helm autocompletions for all go packages
;; under GOPATH on the target host, and also helm completions for unit
;; tests within the specified package(s).

;;; Code:

(require 'cl)
(require 'compile)
(require 'f)
(require 'helm)

(cl-defun gounit-build-go-test-command (&key (host-name nil) (package nil) (test nil))
  (print (mapconcat 'identity (list host-name package test) "\n"))
  (let* ((args '())
		 (command-string '()))
	(when test
	  (setq command-string (append command-string '("-run %s$")))
	  (setq args (append args (list test)))
	  )
	
	(when package
	  (setq command-string (append '("go test -v ") command-string '(" %s ")))
	  (setq args (append args (list package))))
	(when host-name
	  (setq command-string (append '("ssh %s 'bash -c -l \"") command-string '("\"'") ))
	  (setq args (append (list host-name) args)))

	(print command-string)
	(apply 'format (cons (apply 'concat command-string) args))))

;; TODO exit when the fist helm call is exited from by user
;; TODO also do better error handling and figure out throw/catch or
;; best practice
(defun gounit-run-tests ()
  "Run unit tests.
Arguments for package and test name are collected via helm."
  (interactive)
  (let* ((host-name (getenv "GOUNIT_REMOTE_HOST"))
		 (package (gounit-get-package-to-test host-name))
		 (test (gounit-search-for-tests-in-go-package host-name package)))
	(message (format "package: %s, test %s\n" package test))
	(compile (gounit-build-go-test-command
			  :host-name host-name
			  :package package
			  :test test))))

(defun gounit-get-package-to-test (host-name)
  (defun find-go-packages ()
	(let* ((root (projectile-project-root))

		   ;; TODO we really need to fail hard here if we can't match
		   ;; the /src/ here becuase everything following depends on
		   ;; it. Another though is to figure out what the GOPATH is
		   ;; on HOST-NAME and chop that off the beginning. Still must
		   ;; fail though.
		   (package-root (and (string-match "\\/src\\/" root)
							  (substring root (match-end 0)))))
	  (print (concat "project root: " package-root))

	  (start-process
	   "list-go-packages"
	   nil
	   "ssh"
	   host-name
	   (format "bash -lc 'go list %s/...'" package-root))))
  (print (symbol-function 'find-go-packages))	
  (helm :sources (helm-build-async-source "Choose a package to test:"
				   :candidates-process 'find-go-packages
				   :filtered-candidate-transformer 'gounit-packages-candidates-transformer)
		:buffer "*helm choose go package*"))

(defun gounit-packages-candidates-transformer (candidates _source)
  (cl-reduce (lambda (cand rest)
			   (if (match-helm-pattern cand)
				   (cons cand rest)
				 rest))
			 candidates
			 :initial-value '()
			 :from-end t))


(defun gounit-keep-test-files (filepath)
  (string-match ".*_test\.go" filepath))

(defun gounit-search-for-tests-in-go-package (host-name package)
  "Search for tests under golang GOPATH in PACKAGE."
  (let*  ((package-path (concat (format "/ssh:%s:/go/src/" host-name) package))
		  (test-files (f-files package-path 'gounit-keep-test-files)))
	(defun grep-for-tests ()
	  (let ((command
			 (format "bash -lc 'grep -hI '[T]est' %s'"
					 (mapconcat 'request-untrampify-filename test-files " "))))
		(start-process
		 "grep-list-unit-tests"
		 nil
		 "ssh"
		 host-name
		 command)))	
	(helm :sources (helm-build-async-source "Choose a test to run:"
					 :candidates-process 'grep-for-tests
					 :filtered-candidate-transformer 'gounit-tests-candidate-transformer)
		  :buffer "*helm choose unit test*")))

(defun extract-test-name (cand)
  (when (stringp cand)
	(and (string-match "func \\([T]est[[:alnum:]_]*\\)(" cand)
		 (match-string 1 cand))))

(defun match-helm-pattern (cand)
  (and
   cand
   (or (when (< (length helm-pattern) 3) cand)
	   (and (string-match helm-pattern cand) cand))))

(defun gounit-tests-candidate-transformer (candidates _source)
  (cl-reduce (lambda (cand rest)
			   (let* ((test-name (extract-test-name cand))			   
					  (match (match-helm-pattern test-name)))
				 (if (and test-name match)
					 (cons test-name rest)
				   rest)))
			 candidates
			 :initial-value '()
			 :from-end t))

;; TODO skip tests and run whole package
;; TODO allow going back from package selection (could be hard)
;; TODO error out with no package

;; (gounit-run-tests)

;; (setq debug-on-error t)

(provide 'gounit)
;;; go-unit.el ends here
