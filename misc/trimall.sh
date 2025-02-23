#!/bin/bash

# sudo port install py312-fonttools
# --retain-gids preserves variable font axis
# 3000-30ff: hiragana, etc, and chinese

# d7b0-d7ff, 1100-11ff, a960-a97f, ac00–d7af: hangul, I decided to
# skip hangul and save 1 MB in font size.

# You have to add the --flavor="woff2" flag, even if the source file
# is woff2, otherwise the file size doesn’t decrease (it even
# increases).

pyftsubset SourceHanSerif-VF.otf.woff2 --text-file=中日韩简繁常用字及符号.txt --unicodes=3000-30ff --unicodes=a960-a97f  --layout-features="*" --retain-gids --flavor="woff2"
