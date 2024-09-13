;;; ob-jira-markdown-tests.el --- tests for the ob-jira-markdown package -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <emacs@akuszyk.com>
;; Keywords: org-babel

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Tests for the ob-jira-markdown package.

;;; Code:
(require 'ert)
(add-to-list 'load-path "../")
(require 'ob-jira-markdown)

(ert-deftest org-babel-execute:jira-markdown-create-issue ()
  (let* ((body "hello world")
	 (params '((:command . "create")
		   (:execute . "script")
		   (:project . "foo")
		   (:title . "bar")))
	 (result (org-babel-execute:jira-markdown body params)))
    (should (string=
	     result
	     "jira issue create --no-input -p 'foo' -s 'bar' -t 'Story' -b 'hello world'"))))

(ert-deftest org-babel-execute:jira-markdown-edit-issue ()
  (let* ((body "hello world")
	 (params '((:command . "edit")
		   (:execute . "script")
		   (:project . "foo")
		   (:issue . "abc-123")
		   (:title . "bar")))
	 (result (org-babel-execute:jira-markdown body params)))
    (should (string=
	     result
	     "jira issue edit abc-123 -s 'bar' -p 'foo' --no-input -b 'hello world'"))))

(provide 'ob-jira-markdown-tests)
;;; ob-jira-markdown-tests.el ends here
