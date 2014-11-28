;; test-helper.el --- Helpers for company-c-headers unit tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014 Alastair Rankine

;; Author: Alastair Rankine <alastair@girtby.net>
;; Keywords: development company

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

;;(require 'cl-lib)

(require 'f)
(require 'undercover)

(defvar company-c-headers-test-dir (f-dirname (f-this-file)))
(defvar company-c-headers-dir (f-parent company-c-headers-test-dir))

(undercover "company-c-headers.el")

(require 'company-c-headers (f-expand "company-c-headers.el" company-c-headers-dir))

(defmacro with-test-c-buffer (&rest body)
  `(with-temp-buffer
     (setq major-mode 'c-mode)
     (company-mode 1)
     ,@body
     ))

(defmacro with-temp-directory (dir &rest body)
  "Create DIR as a temporary directory and invoke BODY.  The temporary directory will be deleted on exit/error."
  `(let ((,dir (f-slash (make-temp-file "test-" t))))
    (unwind-protect
        (progn ,@body)
      (f-delete ,dir t))))

(defmacro with-test-headers (dir files &rest body)
  "Create a temporary directory and populate it with FILES."
  `(with-temp-directory
    ,dir

    (dolist (F ,files)
      (f-mkdir (f-join ,dir (f-dirname F)))
      (f-touch (f-join ,dir F)))

    ,@body
    ))
