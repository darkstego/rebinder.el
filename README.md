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
(define-key my-mode-map (kbd "C-d") (rebinder-dynamic-binding "C-c"))
(define-key rebinder-mode-map (kbd "C-c") 'kill-ring-save)

(rebinder-hook-to-mode 'my-mode 'my-mode-hook)
```

This will work for any keymap is is the proper way to rebind other keys like C-x
