# everlasting-scratch

[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](COPYING.md)
[![MELPA](https://melpa.org/packages/everlasting-scratch-badge.svg)](https://melpa.org/#/everlasting-scratch)
[[996.icu] [file:https://img.shields.io/badge/link-996.icu-red.svg]]

The scratch buffer that lasts forever.

# Summary
This package provides a global minor mode `everlasting-scratch-mode'
that causes the *scratch* to respawn after it's killed and with content restored,
the *scratch* could survive manual kill and emacs restart (with help of desktop.el)

@note: borrowed lots of stuff from immortal-scratch, thanks mate.

# Usage


```lisp
    (add-hook 'after-init-hook 'everlasting-scratch-mode)
```

# License

[LICENSE](LICENSE). Copyright (c) 2022 Huming Chen <chenhuming@gmail.com>
