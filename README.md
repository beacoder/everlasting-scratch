# everlasting-scratch

[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](COPYING.md)
[![MELPA](https://melpa.org/packages/everlasting-scratch-badge.svg)](https://melpa.org/#/everlasting-scratch)
[![MELPA Stable](https://stable.melpa.org/packages/everlasting-scratch-badge.svg)](https://stable.melpa.org/#/everlasting-scratch)
[![996.icu](https://img.shields.io/badge/link-996.icu-red.svg)](https://996.icu)

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

# Features

- [x] Automatically respawn *scratch*.
- [x] Content will be restored together with *scratch*.
- [x] Content could survive emacs restart.

# License

[LICENSE](LICENSE). Copyright (c) 2022-2023 Huming Chen <chenhuming@gmail.com>
