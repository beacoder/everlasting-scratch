# everlasting-scratch
The scratch buffer that lasts forever.


# Summary
This package provides a global minor mode `everlasting-scratch-mode'
that causes the scratch buffer to respawn after it's killed and with
its content restored.

@note: borrowed lots of stuff from immortal-scratch, thanks mate.

# Usage


```lisp
    (add-hook 'after-init-hook 'everlasting-scratch-mode)
```
