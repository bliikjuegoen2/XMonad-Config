#!/usr/bin/python3

import subprocess as sp 

out = "[no message]"

try:
    out = sp.check_output("xmonad --recompile".split(), stderr=sp.STDOUT, text = True)
except sp.CalledProcessError as error:
    out = error.output

if out == "":
    out = "There has probably been an error. Recheck your .xmonad file"
# must be zenity cant be yad
sp.run("yad --info --text".split() + [out])