#!/usr/bin/python3

import subprocess as sp 

out = sp.check_output("xmonad --recompile".split(), stderr=sp.STDOUT, text = True)

sp.run("zenity --info --text".split() + [out])