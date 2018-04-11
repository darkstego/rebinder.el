;;; rebinder.el --- Allow safe and dynamic rebinding of Emacs prefix keys -*- lexical-binding: t -*-

;; Author: Abdulla Bubshait
;; URL: https://github.com/darkstego/rebinder.el
;; Created: 9 April 2018
;; Keywords: prefix, leader, keybindings, keys
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; Emacs minor mode that provides a modern, efficient and easy to learn keybindings

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; Functions & Macros

(defun rebinder-dynamic-binding (key &optional toggle)
  "Act as prefix definition in the current context.
    This uses an extended menu item's capability of dynamically computing a
    definition. This idea came from general.el"
  `(menu-item
	 ,""
	 nil
	 :filter
	 (lambda (&optional _)
		,`(rebinder-key-binding ,key ,toggle))))


;; should probably use let instead of double call to (car x)
(defun rebinder-minor-mode-key-binding (key)
  "Gets list of minor mode keybindings while ignoring the override map"
  (let ((active-maps nil))
	 (mapc (lambda (x)
				(when (and (symbolp (car x)) (symbol-value (car x)))
				  (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
			 minor-mode-map-alist )
	 (make-composed-keymap active-maps)))


;; might need to do keymap inheretence to perserve priority
(defun rebinder-key-binding (key &optional toggle)
  "Gets the keymap of associated key. If toggle is non-nil then the Ctrl status of all bindings in the returned keymap
   will be changed."
  (let ((map (make-composed-keymap (list (rebinder-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key))))))
	 (if toggle
		  (mapcar 'rebinder-toggle-ctrl map)
		map)))

(defun rebinder-toggle-ctrl (item)
  "Returns a keymap with all Ctrl status of binding toggled"
  (cond
	((and (listp item)
			(not (listp (cdr item))))
	 (cons (rebinder-toggle-ctrl (car item)) (cdr item)))
	((listp item)
	 (mapcar 'rebinder-toggle-ctrl item))
	((event-basic-type item)
	 (let ((mods (event-modifiers item))
			 (key (event-basic-type item)))
		(if (member 'control mods)
			 (event-convert-list (append (remove 'control mods) (list (event-basic-type item))))
		  (event-convert-list (append (append mods '(control)) (list (event-basic-type item)))))))
	(t item)))


(defvar rebinder-mode-map (make-sparse-keymap))
(defvar rebinder-linked-mode nil)

(defun rebinder-override ()
  "Add modemap to override C-c into minor-mode-overriding-map-alist"
  (interactive)
  (add-to-list 'minor-mode-overriding-map-alist (cons 'rebinder-mode rebinder-mode-map)))
(add-hook 'after-change-major-mode-hook 'rebinder-override)


;; Remove overrides on mode exit
(defun rebinder-update-override ()
  "Enables and disables rebinder override keymap depending on status of
  associated mode"
  (setq rebinder-mode (symbol-value rebinder-link-mode)))
  

(defun rebinder-hook-to-mode (mode modehook)
  "Link rebinder override map to associated mode"
  (setq rebinder-link-mode mode)
  (add-hook modehook 'rebinder-update-override))


(provide 'rebinder)
