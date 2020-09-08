;;; searcher.el --- Searcher in pure elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-06-19 20:12:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Searcher in pure elisp
;; Keyword: search searcher project file text string
;; Version: 0.1.7
;; Package-Requires: ((emacs "25.1") (dash "2.10") (f "0.20.0"))
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
(require 'subr-x)

(defgroup searcher nil
  "Searcher in pure elisp."
  :prefix "searcher-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/searcher"))

(defcustom searcher-ignore-dirs
  '("/[.]log/"
    "/[.]vs/" "/[.]vscode/"
    "/[.]svn/" "/[.]git/" "/[.]hg/" "/[.]bzr/"
    "/[.]idea/"
    "/[.]tox/"
    "/[.]stack-work/"
    "/[.]ccls-cache/" "/[.]clangd/"
    "/[.]ensime_cache/" "/[.]eunit/" "/[.]fslckout/"
    "/[Bb]in/" "/[Bb]uild/" "/res/" "/[.]src/"
    "/SCCS/" "/RCS/" "/CVS/" "/MCVS/" "/_MTN/" "/_FOSSIL_/"
    "/_darcs/" "/{arch}/"
    "/node_modules/")
  "List of path you want to ignore by the searcher."
  :type 'list
  :group 'searcher)

(defcustom searcher-ignore-files
  '("[.]gitignore" "[.]gitattributes"
    "[.]meta" "[.]iso"
    "[.]img" "[.]png" "[.]jpg" "[.]jpng" "[.]gif"
    "[.]psd"
    "[.]obj" "[.]maya" "[.]fbx"
    "[.]mp3" "[.]wav"
    "[.]mp4"  "[.]avi" "[.]flv" "[.]mov" "[.]webm" "[.]mpg" "[.]mkv" "[.]wmv"
    "[.]exe" "[.]bin"
    "[.]elc" "[.]javac" "[.]pyc"
    "[.]lib" "[.]dll" "[.]o" "[.]a")
  "List of files you want to ignore by the searcher."
  :type 'list
  :group 'searcher)

(defcustom searcher-use-cache t
  "Use cache to speed up the search speed."
  :type 'boolean
  :group 'searcher)

(defvar searcher--cache-project-files nil
  "Cache for valid project files.
Do `searcher-clean-cache' if project tree strucutre has been changed.")

;;; Util

(defun searcher--is-contain-list-string-regexp (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some (lambda (lb-sub-str) (string-match-p lb-sub-str in-str)) in-list))

(defun searcher--f-directories-ignore-directories (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (let ((dirs (f-directories path)) (valid-dirs '()) (final-dirs '()))
    (dolist (dir dirs)
      (unless (searcher--is-contain-list-string-regexp searcher-ignore-dirs (f-slash dir))
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

;;; Core

(defun searcher--form-match (file ln-str pos ln col)
  "Form a match candidate; data are FILE, POS and LN-STR."
  (list :file file :string ln-str :position pos :line-number ln :column col))

(defun searcher-clean-cache ()
  "Clean up the cache files."
  (setq searcher--cache-project-files nil))

;;;###autoload
(defun searcher-search-in-project (str-or-regex)
  "Search STR-OR-REGEX from the root of project directory."
  (let ((project-path (cdr (project-current))))
    (if project-path
        (searcher-search-in-path project-path str-or-regex)
      (error "[ERROR] No project root folder found from default path"))))

;;;###autoload
(defun searcher-search-in-path (path str-or-regex)
  "Search STR-OR-REGEX from PATH."
  (let ((result '()))
    (when (or (not searcher--cache-project-files)
              (not searcher-use-cache))
      (setq searcher--cache-project-files
            (searcher--f-files-ignore-directories
             path
             (lambda (file)  ; Filter it.
               (not (searcher--is-contain-list-string-regexp searcher-ignore-files file)))
             t)))
    (dolist (file searcher--cache-project-files)
      (push (searcher-search-in-file file str-or-regex) result))
    (-flatten-n 1 result)))

;;;###autoload
(defun searcher-search-in-file (file str-or-regex)
  "Search STR-OR-REGEX in FILE."
  (let ((matchs '()) (match "") (ln-str "") (ln nil) (col nil)
        (buf-str "") (start 0))
    (unless (string-empty-p str-or-regex)
      (with-temp-buffer
        (if (file-exists-p file)
            (insert-file-contents file)
          (insert (with-current-buffer file (buffer-string))))
        (setq buf-str (buffer-string))
        (while start
          (setq start (string-match str-or-regex buf-str start))
          (when start
            (goto-char start)
            (setq ln-str (substring buf-str (1- (line-beginning-position)) (1- (line-end-position)))
                  ln (line-number-at-pos)
                  col (current-column))
            (when (and (not (string-empty-p ln-str))
                       ;; TODO: Not sure why `string-match-p' doesn't give the
                       ;; correct result when checking the end of the string.
                       (not (<= (length ln-str) col)))
              (setq match (searcher--form-match file ln-str start ln col))
              (push match matchs))
            (setq start (1+ start))))))
    matchs))

(provide 'searcher)
;;; searcher.el ends here
