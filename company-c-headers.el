;;; company-c-headers.el --- Company mode backend for C/C++ header files  -*- lexical-binding: t -*-

;; Copyright (C) 2014 Alastair Rankine

;; Author: Alastair Rankine <alastair@girtby.net>
;; Keywords: development company
;; Package-Requires: ((emacs "24.1") (company "0.8"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'company)
(require 'rx)
(require 'cl-lib)

(defvar c-header-prefix
  (rx
   line-start
   "#" (zero-or-more blank) (or "include" "import")
   (one-or-more blank)
   (submatch
    (in "<\"")
    (zero-or-more (not (in ">\""))))
   )
  "Prefix matching C/C++/ObjC include directives.")

(defvar company-c-headers-modes
  `(
    (c-mode     . ,(rx ".h" line-end))
    (c++-mode   . ,(rx (or (: line-start (one-or-more (in "A-Za-z0-9_")))
                           (or ".h" ".hpp" ".hxx"))
                       line-end))
    (objc-mode  . ,(rx ".h" line-end))
    )
  "Assoc list of supported major modes and associated header file names.")

(defvar-local company-c-include-path-system '("/usr/include")
  "List of paths to search for system (i.e. angle-bracket
delimited) header files.  Alternatively this symbol can be
bound to a function which returns the path list.")

(defvar-local company-c-include-path-user '(".")
  "List of paths to search for user (i.e. double-quote delimited)
header files.  Alternatively this symbol can be bound to a
function which returns the path list.  Note that paths in
`company-c-include-path-system' are implicitly appended.")

(defun call-if-function (path)
  "If PATH is bound to a function, return the result of calling it.
Otherwise just return the value."
  (if (functionp path)
      (funcall path)
    path))

(defun company-c-headers--candidates-for (prefix dir)
  "Return a list of candidates for PREFIX in directory DIR.
Filters on the appropriate regex for the current major mode."
  (let* ((prefixdir (file-name-directory prefix))
         (subdir (and prefixdir (concat (file-name-as-directory dir) prefixdir)))
         (hdrs (cdr (assoc major-mode company-c-headers-modes))))
    (if subdir

        ;; If a subdirectory exists, just recurse into it
        (when (file-directory-p subdir)
          ;; TODO mapconcat to prepend prefixdir
          (company-c-headers--candidates-for (file-name-nondirectory prefix) subdir))
      
      ;; Using a list of completions for this directory, remove those that a) don't match the
      ;; headers regexp, and b) are not directories (except for "." and ".." which ARE removed)
      (sort (cl-remove-if (lambda (F) (and (not (string-match-p hdrs F))
                                           (or (cl-member (directory-file-name F) '("." "..") :test 'equal)
                                               (not (file-directory-p (concat (file-name-as-directory dir) F))))))
                          (file-name-all-completions prefix dir))
            #'string<)
      )))

(defun company-c-headers--candidates (prefix)
  (let ((pfx (substring prefix 1))
        (userpaths (when (equal (aref prefix 0) ?\")
                     (call-if-function company-c-include-path-user)))
        (syspaths (call-if-function company-c-include-path-system))
        candidates)
     
    (dolist (P userpaths)
      (setq candidates (append candidates (company-c-headers--candidates-for pfx P))))
    (dolist (P syspaths)
      (setq candidates (append candidates (company-c-headers--candidates-for pfx P))))
    candidates
    ))

(defun company-c-headers-backend (command &optional arg &rest ignored)
  "Backend for C/C++ header files."
  (pcase command
    (`interactive (company-begin-backend 'company-c-headers-backend))
    (`prefix (when (and (assoc major-mode company-c-headers-modes)
                        (looking-back c-header-prefix (line-beginning-position)))
               (match-string 1)))
    (`candidates (company-c-headers--candidates arg))
    (`meta (format "This value is named %s" arg))))

(provide 'company-c-headers)

;;; company-c-headers.el ends here
