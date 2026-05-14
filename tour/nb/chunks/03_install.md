<!-- chunk: 03_install -->
<!-- prev: 02_peek  next: 10_lec1_open -->

## Install

You need Lua 5.4 and `awk`. Both ship with macOS and most Linux
distributions. Then:

```
git clone https://github.com/USER/REPO.git tour
cd tour/nb
lua nb.lua --num
```

If that prints a line that starts with `NUM{`, you are ready. If
not, check `lua -v` — anything 5.3 or later should work.

To rebuild the tour itself into one assembled markdown file:

```
make
```

`make` produces `build/build.md`. GitHub Pages renders it as the
chapter you are reading.
