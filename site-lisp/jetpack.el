;;; jetpack.el --- Jetpack elm compilation           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Charles Comstock

;; Author: Charles Comstock <dgtized@gmail.com>
;; Version: 0.5.0
;; Package-Requires: ((elm-mode "20190815") (emacs "24.4"))
;; Keywords: tools

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `jetpack-compile' and `jetpack-compile-file' as a means of finding
;; jetpack entrypoints and compiling, or running jetpack on a specific
;; entrypoint.

;; Re-uses `elm-compilation-mode' for parsing the compilation output from
;; jetpack as it wraps `elm-make'.

;;; Code:


(defvar jetpack-history nil
  "History of recent jetpack invocations.")

(defvar jetpack-last-compiled nil
  "Remember last file jetpack compiled for default re-run.")

;;;###autoload
(defun jetpack-compile-file (file)
  "Compile a file using Jetpack and show compilation results."
  (interactive "fJetpack: ")
  (let ((default-directory (locate-dominating-file file "jetpack.json")))
    (compilation-start
     (concat "/usr/bin/npx jetpack " (file-relative-name file))
     'elm-compilation-mode
     (lambda (_) "*jetpack*"))))

;;;###autoload
(defun jetpack-compile-buffer ()
  "Compile the current-buffer using Jetpack."
  (interactive)
  (jetpack-compile (buffer-file-name)))

(defun jetpack-preselect ()
  "Preselect filename of current buffer or re-use last compiled."
  (or (if (buffer-file-name)
          (let ((current-file (file-relative-name (buffer-file-name))))
            (if (string-match-p "\.js$" current-file) current-file))
        jetpack-last-compiled)))

(defun jetpack-action (file)
  "Helper function to compile file and save last-compiled."
  (setq jetpack-last-compiled file)
  (jetpack-compile-file file))

;;;###autoload
(defun jetpack-compile ()
  "Complete entrypoint and compile with Jetpack."
  (interactive)
  (let* ((current-file (or (buffer-file-name) default-directory))
         (root-dir (locate-dominating-file current-file "jetpack.json")))
    (if root-dir
        (let* ((default-directory root-dir)
               (json-object-type 'hash-table)
               (json (json-read-file (expand-file-name "jetpack.json")))
               (entry-point (gethash "entry_points" json)))
          (if entry-point
              (jetpack-action
               (completing-read "Jetpack: "
                                (directory-files-recursively entry-point ".*\\.js$")
                                nil t nil
                                'jetpack-history
                                (jetpack-preselect)))
            (message "No entry_points directory defined in jetpack.json")))
      (message "Error: unable to find jetpack.json at project root."))))

(provide 'jetpack)
;;; jetpack.el ends here
