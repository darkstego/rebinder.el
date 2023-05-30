# rebinder.el
Allow rebinding of Emacs prefix keys


## Installation

Place file in load path and add

```
(require 'rebinder)
```
to your code

## Usage


Let's say you have a mode *my-mode* with a keymap *my-mode-map* and a hook *my-mode-hook*

This is how to rebind the prefix key C-c to C-d and make C-c a pure copy

```
(define-key my-mode-map (kbd "C-h") (rebinder-dynamic-binding "C-c"))
(define-key rebinder-mode-map (kbd "C-c") 'kill-ring-save)

(rebinder-hook-to-mode 'my-mode 'my-mode-hook)
```

This will work for any keymap and is the proper way to rebind other keys like C-x

If you want to the prefix keymap to be returned with all the Ctrl modifiers toggled, then pass
a non-nil second argument to *rebinder-dynamic-binding*


While the recommended method of changing bindings is creating your own mode map and placing all the keys there,
as shown above, if you would like to edit the global map directly and set a permanent binding the above example
will look like
```
(define-key global-map (kbd "C-h") (rebinder-dynamic-binding "C-c"))
(define-key rebinder-mode-map (kbd "C-c") 'kill-ring-save)

(rebinder-hook-to-mode 't 'after-change-major-mode-hook)
```
