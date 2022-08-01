#!/usr/bin/python3

import subprocess as sp 

out = "[no message]"

try:
    out = sp.check_output("xmonad --recompile".split(), stderr=sp.STDOUT, text = True)
except sp.CalledProcessError as error:
    out = error.output

sp.run("zenity --info --text".split() + [out])