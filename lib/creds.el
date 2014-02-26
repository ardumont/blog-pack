;;; creds.el --- a simple creds lib

;; Copyright (C) 2013
;;   Free Software Foundation, Inc.

;; Author: Antoine R. Dumont <eniotna.t@gmail.com>
;; Keywords: credentials
;; Version: 0.2

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; A small library (non optimal) to deal with more entries than just credentials
;; (The search is linear so not optimal)

;; Here is an example of .authinfo
;; machine machine0 port http login nouser password nopass
;; machine machine1 login some-login password some-pwd port 993
;; machine machine2 login some-login port 587 password some-pwd
;; machine jabber         login some-login password some-pwd
;; machine description    name "my name is" blog some-blog mail some-mail

(defun creds/read-lines (filepath)
  "Return a list of lines from a file."
  (with-temp-buffer
    (insert-file-contents filepath)
    (mapcar (lambda (l) (split-string l "[ ]+")) (split-string (buffer-string) "\n" t))))

;; Here is the result
;; (creds/read-lines "~/.authinfo")
;; (("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")
;;  ("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993")
;;  ("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")
;;  ("machine" "jabber" "login" "some-login" "password" "some-pwd")
;;  ("machine" "description" "name" "\"my" "name" "is\"" "blog" "some-blog" "mail" "some-mail"))
;; (setq dat (creds/read-lines "~/.authinfo"))

(defun creds/get (data entry-name)
  "Return the data list for the line entry-name"
  (if data
      (let* ((d     (car data))
             (entry (cadr d)))
        (if (equal entry entry-name)
            d
          (creds/get (cdr data) entry-name)))))

;; (creds/get dat "machine0")
;; ("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")

;; (creds/get dat "nil")
;; nil

(defun creds/get-entry (data entry)
  "Given a data list, return the entry in that list"
  (if data
      (let* ((k (car data))
             (v (cadr data)))
        (if (equal k entry)
            v
          (creds/get-entry (cddr data) entry)))))

;; (setq machine (creds/get dat "machine0"))
;; (creds/get-entry machine "machine")
;; "machine0"
;; (creds/get-entry machine "port")
;; "http"
;; (creds/get-entry machine "login")
;; "nouser"
;; (creds/get-entry machine "password")
;; "nopass"

(provide 'creds)

;;; creds.el ends here
