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
