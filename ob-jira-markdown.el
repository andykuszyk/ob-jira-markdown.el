;;; ob-jira-markdown.el --- Org-babel functions for jira-cli evaluation -*- lexical-binding:t; ispell-buffer-session-localwords: ("JIRA" "SRC" "EMX"); -*-

;; Copyright (C) 2024 Andy Kuszyk

;; Author: Andy Kuszyk <emacs@akuszyk.com>
;; URL: https://github.com/andykuszyk/ob-jira-markdown.el
;; Version: 0.1
;; Keywords: org-babel
;; Package-Requires: ((emacs "29.1") markdown-mode)

;; This file is not part of GNU Emacs.

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
;; Org-babel support for creating, viewing, and editing Jira issues via the Jira
;; CLI: https://github.com/ankitpokhrel/jira-cli

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'jira-markdown-mode)

(defgroup ob-jira-markdown
  nil
  "Variables for the ob-jira-markdown package."
  :group 'applications
  :prefix "ob-jira-markdown")

(defcustom ob-jira-markdown-host
  nil
  "The URL of the Jira host to use when formatting issue URLs."
  :type 'string)

(defvar org-babel-default-header-args:jira-markdown
  '((:results . "output"))
  "Default arguments for evaluating a Jira Markdown source block.")

(defun org-babel-execute:jira-markdown (body params)
  "Create or edit a Jira issue based on a BODY and PARAMS in a source block."
  (let (
	(issue (cdr (assoc :issue params)))
	(title (cdr (assoc :title params)))
	(type (cdr (assoc :type params)))
	(project (cdr (assoc :project params)))
	(parent (cdr (assoc :parent params)))
	(command (cdr (assoc :command params)))
	(execute (cdr (assoc :execute params))))
    (cond
     ((string= command "create")
      (if issue
	  (error "Issues can't be created if the :issue header argument is set"))
      (if (not title)
	  (error ":title is required when creating issues"))
      (let ((jira-cli-cmd
	     (format
	      "jira issue create --no-input %s%s-s '%s' -t '%s' -b '%s'"
	      (if parent (format "-P %s " parent) "")
	      (if project (format "-p '%s' " project) "")
	      title
	      (if type type "Story")
	      (ob-jira-markdown--clean-src-body body))))
	(message (format "creating issue with the command: %s" jira-cli-cmd))
	(cond
	 ((string= execute "cli")
	  (let ((new-jira-cli-issue
		 (ob-jira-markdown--parse-issue-reference
		  (shell-command-to-string jira-cli-cmd))))
	    (kill-new new-jira-cli-issue)
	    (message (format
		      "Created Jira issue and saved key to kill ring: %s"
		      new-jira-cli-issue))
	    new-jira-cli-issue))
	 ((string= execute "script")
	  (message "returning script")
	  jira-cli-cmd)
	 (t
	  (error ":execute must be \"cli\" or \"script\"")))))
     ((string= command "edit")
      (if (not issue)
	  (error ":issue must be set to edit an issue"))
      (let ((jira-cli-cmd (format
			   "jira issue edit %s %s%s--no-input -b '%s'"
			   issue
			   (if title (format "-s '%s' " title) "")
			   (if project (format "-p '%s' " project) "")
			   (ob-jira-markdown--clean-src-body body))))
	(cond
	 ((string= execute "cli")
	  (let ((updated-jira-cli-issue
		 (ob-jira-markdown--parse-issue-reference
		  (shell-command-to-string jira-cli-cmd))))
	    (message (format "Updated Jira issue: %s"
			     updated-jira-cli-issue))))
	 ((string= execute "script")
	  jira-cli-cmd)
	 (t
	  (error ":execute must be \"cli\" or \"script\"")))))
     (t
      (error ":command must be \"create\" or \"edit\"")))))

(defun ob-jira-markdown--parse-issue-reference (text)
  "Try to parse the Jira issue reference from the provided TEXT."
  (if (string-match "[A-Z]+\\-[0-9]+" text)
      (match-string 0 text)
    nil))

(defun ob-jira-markdown--clean-src-body (body)
  "Replace special characters in BODY ready for CLI execution."
  (string-replace "%" "%%" (string-replace "'" "'\"'\"'" body)))

(defun ob-jira-markdown-open-in-browser ()
  "Open the Jira issue associated with the current source block in a browser.

This function opens the Jira issue associated with the current \"org-mode\"
source block based on its :issue header argument.

For example, if this function was invoked on the source block below, issue
EMX-123 would be opened in the system`s default web browser:

  #+begin_src markdown :issue EMX-123
  Emacs rocks!
  #+end_src

The URL of the Jira issue is constructed using the `ob-jira-markdown-host`
  variable."
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (issue (cdr (assoc :issue (nth 2 src-block-info)))))
    (if (not issue)
	(error "Jira issue could not be found!")
      (let ((url (format "%s/browse/%s" ob-jira-markdown-host issue)))
	(message (format "opening issue in browser: %s" url))
	(browse-url url)))))

(defun ob-jira-markdown-kill-issue ()
  "Kill the issue key of the current source block to the kill ring.

When invoked on a source block with the :issue header argument, this function
will add the issue key to the kill ring.

For example, if called on the source block below, \"EMX-123\" would be added to
the kill ring:

  #+begin_src markdown :issue EMX-123
  Plain text for the win!
  #+end_src"
  (interactive)
  (let* ((src-block-info (org-babel-get-src-block-info))
	 (jira-cli-issue (cdr (assoc :issue (nth 2 src-block-info)))))
    (if (not jira-cli-issue)
	(error "Jira issue could not be found!")
      (message (format "killed reference: %s" jira-cli-issue))
      (kill-new jira-cli-issue))))

(provide 'ob-jira-markdown)
;;; ob-jira-markdown.el ends here
