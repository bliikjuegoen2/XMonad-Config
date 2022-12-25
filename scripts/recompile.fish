#!/usr/bin/fish

yad --info --text "$(xmonad --recompile &| cat)"