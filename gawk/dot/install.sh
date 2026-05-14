#!/usr/bin/env sh
# install.sh -- fetch dot preprocessor + libs into current dir
# usage:  curl -sL https://raw.githubusercontent.com/timm/misc/master/gawk/dot/install.sh | sh
#         curl -sL ...install.sh | sh -s -- --dest ~/.local/bin --extras

set -e

BASE="https://raw.githubusercontent.com/timm/misc/master/gawk/dot"
DEST="."
EXTRAS=0

while [ $# -gt 0 ]; do
  case $1 in
    --dest)    DEST=$2; shift 2 ;;
    --extras)  EXTRAS=1; shift ;;
    -h|--help) sed -n '2,5p' "$0"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 1 ;;
  esac
done

mkdir -p "$DEST"

CORE="dot prep.awk dot.awk numsym.awk"
EXTRA="data.awk tree.awk bayes.awk metrics.awk shuf.awk"

FILES="$CORE"
[ $EXTRAS -eq 1 ] && FILES="$CORE $EXTRA"

for f in $FILES; do
  echo "fetch $f -> $DEST/$f"
  curl -fsSL "$BASE/$f" -o "$DEST/$f"
done

chmod +x "$DEST/dot"

echo
echo "done. add to PATH if needed:  export PATH=\"$DEST:\$PATH\""
echo "test:  echo '{print .x}' | $DEST/dot /dev/stdin"
