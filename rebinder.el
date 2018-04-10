(defun rebinder-dynamic-binding (key)
  "Act as prefix definition in the current context.
    This uses an extended menu item's capability of dynamically computing a
    definition. This idea came from general.el"
  `(menu-item
	 ,""
	 nil
	 :filter
	 (lambda (&optional _)
		,`(rebinder-key-binding ,key))))


;; should probably use let instead of double call to (car x)
(defun rebinder-minor-mode-key-binding (key)
  (let ((active-maps nil))
	 (mapc (lambda (x)
				(when (and (symbolp (car x)) (symbol-value (car x)))
				  (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
			 minor-mode-map-alist )
	 (make-composed-keymap active-maps)))


;; might need to do keymap inheretence to perserve priority
(defun rebinder-key-binding (key)
  (make-composed-keymap (list (rebinder-minor-mode-key-binding key) (local-key-binding (kbd key)) (global-key-binding (kbd key)))))


(defvar rebinder-mode-map (make-sparse-keymap))
(defvar rebinder-linked-mode nil)

(defun rebinder-override ()
  "Add modemap to override C-c into minor-mode-overriding-map-alist"
  (interactive)
  (add-to-list 'minor-mode-overriding-map-alist (cons 'rebinder-mode rebinder-mode-map)))
(add-hook 'after-change-major-mode-hook 'rebinder-override)


;; Remove overrides on mode exit
(defun rebinder-update-override ()
  (setq rebinder-mode (symbol-value rebinder-link-mode)))
  

(defun rebinder-hook-to-mode (mode modehook)
(setq rebinder-link-mode mode)
(add-hook modehook 'rebinder-update-override))
  
(provide 'rebinder)  
