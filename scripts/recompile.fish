#!/usr/bin/fish

yad --form --field=compile-info:TXT --field-text "$(xmonad --recompile &| cat)"