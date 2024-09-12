;;; ob-jira-markdown.el --- org-babel functions for jira-cli evaluation

;;; Commentary:
;; org-babel support for creating, viewing, and editing Jira issues via the Jira
;; CLI.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'markdown-mode)

(define-derived-mode jira-markdown-mode markdown-mode "jira-markdown"
  "A major mode for editing Jira markup using Markdown.")

(defvar org-babel-default-header-args:jira-markdown
  '((:results . "output"))
  "Default arguments for evaluatiing a Jira Markdown source block.")

(defun org-babel-execute:jira-markdown (body params)
  "Execute a block of Jira-Cli code with org-babel with BODY based on PARAMS."
  (let (
	(issue (cdr (assoc :issue params)))
	(title (cdr (assoc :title params)))
	(type (cdr (assoc :type params)))
	(project (cdr (assoc :project params)))
	(parent (cdr (assoc :parent params)))
	(command (cdr (assoc :command params)))
	(execute (cdr (assoc :execute params)))
	)
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

(provide 'ob-jira-markdown)
;;; ob-jira-markdown.el ends here
