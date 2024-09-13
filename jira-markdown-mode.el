;;; jira-markdown-mode.el --- A major mode for Jira markdown -*- lexical-binding:t; ispell-buffer-session-localwords: (); -*-

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
;; A major mode for Jira markdown

;;; Code:
(require 'markdown-mode)

(define-derived-mode jira-markdown-mode markdown-mode "jira-markdown"
  "A major mode for editing Jira markup using Markdown.")

(provide 'jira-markdown-mode)
;;; jira-markdown-mode.el ends here
