

## Installation

Install gawk

```sh
wget -O gawk.tar.gz https://ftp.gnu.org/gnu/gawk/gawk-5.1.0.tar.gz
tar xzf gawk.tar.gz
cd gawk-5.1.0
./configure
sudo make
sudo make install                                │~
```

Then run the set ups inside `ape`.

```
sh ape
```

Then run the test scripts.

```
sh ape test/allok.md
```
