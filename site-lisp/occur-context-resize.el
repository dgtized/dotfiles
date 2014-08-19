;;; occur-context-resize.el --- dynamically resize context in occur matches  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Charles L.G. Comstock

;; Author: Charles L.G. Comstock <dgtized@gmail.com>
;; Keywords: matching

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

;; 

;;; Code:

(defun occur-context-resize-larger ()
  (interactive)
  (setcar (cdr occur-revert-arguments)
          (1+ (or (cadr occur-revert-arguments) 0)))
  (revert-buffer))

(defun occur-context-resize-smaller ()
  (interactive)
  (setcar (cdr occur-revert-arguments)
          (max (1- (or (cadr occur-revert-arguments) 0)) 0))
  (revert-buffer))

(defun occur-context-resize-default ()
  (interactive)
  (setcar (cdr occur-revert-arguments) nil)
  (revert-buffer))

(let ((map occur-mode-map))
  (define-key map (kbd "+") 'occur-context-resize-larger)
  (define-key map (kbd "-") 'occur-context-resize-smaller)
  (define-key map (kbd "0") 'occur-context-resize-default))

(provide 'occur-context-resize)
;;; occur-context-resize.el ends here
