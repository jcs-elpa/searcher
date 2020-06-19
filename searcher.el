;;; searcher.el --- Searcher in pure elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-19 20:12:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Searcher in pure elisp
;; Keyword: search searcher project file text string
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (dash "2.10") (f "0.20.0"))
;; URL: https://github.com/jcs-elpa/searcher

;; This file is NOT part of GNU Emacs.

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
;;
;; Searcher in pure elisp
;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)

(defgroup searcher nil
  "Searcher in pure elisp."
  :prefix "searcher-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/searcher"))

(defcustom searcher-ignore-dirs '(".log"
                                  "[Bb]in"
                                  "[Bb]uild"
                                  "node_modules"
                                  "res"
                                  ".vs"
                                  ".vscode")
  "List of path you want to ignore by the searcher."
  :type 'list
  :group 'searcher)

(defcustom searcher-ignore-files '("[.]gitignore"
                                   "[.]gitattributes"
                                   "[.]meta"
                                   "[.]img" "[.]png"
                                   "[.]iso"
                                   "[.]wav"
                                   "[.]exe")
  "List of files you want to ignore by the searcher."
  :type 'list
  :group 'searcher)

(defun searcher--is-contain-list-string-regexp (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p lb-sub-str in-str)) in-list))

(defun searcher--f-directories-ignore-directories (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (let ((dirs (f-directories path))
        (valid-dirs '())
        (final-dirs '())
        (ignore-lst (append searcher-ignore-dirs
                            grep-find-ignored-directories
                            (if (boundp 'projectile-globally-ignored-directories)
                                projectile-globally-ignored-directories
                              '()))))
    (dolist (dir dirs)
      (unless (searcher--is-contain-list-string-regexp ignore-lst dir)
        (push dir valid-dirs)))
    (when rec
      (dolist (dir valid-dirs)
        (push (searcher--f-directories-ignore-directories dir rec) final-dirs)))
    (setq valid-dirs (reverse valid-dirs))
    (setq final-dirs (reverse final-dirs))
    (-flatten (append valid-dirs final-dirs))))

(defun searcher--f-files-ignore-directories (path &optional fn rec)
  "Find all files in PATH by ignored common directories with FN and REC."
  (let ((dirs (append (list path) (searcher--f-directories-ignore-directories path rec)))
        (files '()))
    (dolist (dir dirs) (push (f-files dir fn) files))
    (-flatten (reverse files))))

;;;###autoload
(defun searcher-search-project (str-or-regex)
  "Search STR-OR-REGEX from the root of project directory."
  (let ((project-path (cdr (project-current))))
    (if project-path
        (searcher-search project-path str-or-regex)
      (user-error "[WARNING] No project root folder found from default path"))))

;;;###autoload
(defun searcher-search (path str-or-regex)
  "Search STR-OR-REGEX from PATH."
  (let* ((ignore-lst searcher-ignore-files)
         (files (searcher--f-files-ignore-directories
                 path
                 (lambda (file)  ; Filter it.
                   (not (searcher--is-contain-list-string-regexp ignore-lst file)))
                 t)))
    (message "files: %s" files)
    ))

(provide 'searcher)
;;; searcher.el ends here
