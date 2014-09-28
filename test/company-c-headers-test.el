;; company-c-headers-test.el --- company-c-headers unit tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Alastair Rankine

;; Author: Alastair Rankine <alastair@girtby.net>
;; Keywords: development company
;; URL: http://github.com/randomphrase/company-c-headers

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

(require 'ert)
(require 'f)

(ert-deftest parse-include-directives ()
  "Tests correct parsing of include directives"
  
  (should (string-match-p company-c-headers-include-declaration "#include <"))
  (should (string-match-p company-c-headers-include-declaration "#include <sys/"))
  (should (string-match-p company-c-headers-include-declaration "#include <blah.h"))
  (should (string-match-p company-c-headers-include-declaration "#include <sys/blah.h"))
  (should (string-match-p company-c-headers-include-declaration "#include \""))
  (should (string-match-p company-c-headers-include-declaration "#include \"blah\""))

  (should-not (string-match-p company-c-headers-include-declaration "// #include <blah>"))
  (should-not (string-match-p company-c-headers-include-declaration "/* #include \"blah.h\" */"))
  )

(ert-deftest match-header-files ()
  "Tests filtering of header files by regexp."

  (let ((c-headers (cdr (assoc 'c-mode company-c-headers-modes)))
        (c++-headers (cdr (assoc 'c++-mode company-c-headers-modes))))
    
    (should (string-match-p c-headers "foo.h"))
    (should-not (string-match-p c-headers "foo.hpp"))
    (should-not (string-match-p c-headers "vector"))

    (should (string-match-p c++-headers "foo.h"))
    (should (string-match-p c++-headers "foo.hpp"))
    (should (string-match-p c++-headers "vector"))
    (should (string-match-p c++-headers "unordered_map"))
    ))

(ert-deftest init-test ()
  "Should be able to call 'init method without error"
  (company-c-headers 'init)
  )

(ert-deftest check-major-mode-test ()
  "Checks the major mode before returning prefix"
  (with-temp-buffer
    (insert "#include \"")
    (should-not (company-c-headers 'prefix))
    (setq major-mode 'c-mode)
    (should (company-c-headers 'prefix))
   ))
   
(ert-deftest parse-prefix ()
  "Tests that we can parse the prefix for completion"
  (with-test-c-buffer
   (should (equal (company-c-headers 'prefix) nil))
   (insert "#include \"")
   (should (equal (company-c-headers 'prefix) "\""))
   (insert "foo")
   (should (equal (company-c-headers 'prefix) "\"foo"))
   (insert "/bar")
   (should (equal (company-c-headers 'prefix) "\"foo/bar"))
   ))

(ert-deftest user-include-candidates ()
  "Tests that we can build a list of candidates from user include directories."
  (with-test-headers
   tmpdir '("a.h" "b.h")
   
   (with-test-c-buffer
    (setq company-c-headers-path-system (list tmpdir))
    (should (equal (company-c-headers 'candidates "<") '("<a.h" "<b.h")))
    (should (equal (company-c-headers 'candidates "<a") '("<a.h")))
    )))

(ert-deftest system-include-candidates ()
  "User include paths should search system include paths"

  (with-test-headers
   tmpdir '("sys/sys.h" "user/user.h")

   (with-test-c-buffer
    (setq company-c-headers-path-system (list (f-join tmpdir "sys")))
    (setq company-c-headers-path-user (list (f-join tmpdir "user")))
    (should (equal (company-c-headers 'candidates "\"") '("\"user.h" "\"sys.h")))
    )))

(ert-deftest ignore-nonexistent-paths ()
  "Silently ignore nonexistent include paths."
  (with-test-c-buffer
   (setq company-c-headers-path-system (list "/nonexistent"))
   (should (equal (company-c-headers 'candidates "<") nil))
   ))

(ert-deftest subdir-candidates ()
  "Test that subdirectories are included in the list of candidates"

  (with-test-headers
   tmpdir '("a.h" "sub/sub.h")

   (with-test-c-buffer
    (setq company-c-headers-path-system (list tmpdir))
    
    (should (equal (company-c-headers 'candidates "<") '("<a.h" "<sub/")))
    (should (equal (company-c-headers 'candidates "<sub") '("<sub/")))
    (should (equal (company-c-headers 'candidates "<sub/") '("<sub/sub.h")))
    )))

(ert-deftest path-bound-to-function ()
  "Tests that include paths can be provided by a function"

  (with-test-headers
   tmpdir '("foo.h")

   (with-test-c-buffer
    (setq company-c-headers-path-system (lambda () (list tmpdir)))
     
    (should (equal (company-c-headers 'candidates "<") '("<foo.h")))
    )))

(ert-deftest metadata-returns-directory ()
  "Metadata for each candidate should be the directory for the corresponding header."
  (with-test-headers
   tmpdir '("foo.h")
  
   (with-test-c-buffer
    (setq company-c-headers-path-system (list tmpdir))

    (should (equal
             (company-c-headers 'meta (car (company-c-headers 'candidates "<")))
             tmpdir))
    )))

(ert-deftest candidate-location ()
  "Location data for each candidate should be supported."
  (with-test-headers
   tmpdir '("foo.h")

   (with-test-c-buffer
    (setq company-c-headers-path-system (list tmpdir))

    (should (equal
             (company-c-headers 'location (car (company-c-headers 'candidates "<")))
             (cons (concat tmpdir "foo.h") 1)))
    )))


(ert-deftest post-completion-user-include ()
  "After completion, add a terminating \" for user includes.
If terminating \" already exist, don't add the \"."
  (with-test-c-buffer
   (insert "#include \"foo.h")
   (company-c-headers 'post-completion)
   (should (looking-back "#include \"foo.h\"")))
  ;; testcase if pre-existing terminating delimiter already exist
  (with-test-c-buffer
   (insert "#include \"foo.h\"") (backward-char 1)
   (company-c-headers 'post-completion)
   (end-of-line)
   (should (looking-back "#include \"foo.h\""))
   ))

(ert-deftest post-completion-system-include ()
  "After completion, add a terminating > for system includes.
If terminating > already exist, don't add the >."
  (with-test-c-buffer
   (insert "#include <foo.h")
   (company-c-headers 'post-completion)
   (should (looking-back "#include <foo.h>")))
  ;; testcase if pre-existing terminating delimiter already exist
  (with-test-c-buffer
   (insert "#include <foo.h>") (backward-char 1)
   (company-c-headers 'post-completion)
   (end-of-line)
   (should (looking-back "#include <foo.h>"))
   ))

(ert-deftest post-completion-directory-name ()
  "Don't add a terminating delimiter if we've completed a directory name"
  (with-test-c-buffer
   (let ((expected (format "#include <%s" (file-name-as-directory "foo"))))
     (insert expected)
     (company-c-headers 'post-completion)
     (should (looking-back expected))
     )))

;;; company-c-headers-test.el ends here
