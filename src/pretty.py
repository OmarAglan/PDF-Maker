#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
correctly indent all Haskell source files
"""
import os
if __name__ == '__main__':
    for i in os.listdir("."):
        if i.split(".")[-1] == "hs":
            print(i)
            haskellsourcefile = open(i)
            text = haskellsourcefile.read()
            if text[0:6] == "module":
                text = "\n" + text
            text = text.split("\nmodule")
            haskellsourcecode = "\nmodule".join([""] + text[1:]).replace(
                  "{-DHUN", "{-# WARNING\nx \"").replace("DHUN-}", "\"\n #-}")
            haskellsourcefile.close()
            haskellsourcefile = open(i, "w")
            haskellsourcefile.write(haskellsourcecode)
            haskellsourcefile.close()
            os.system("./pretty " + i + " > temp.temp")
            os.system("cp temp.temp " + i)
            haskellsourcefile = open(i)
            haskellsourcecode = haskellsourcefile.read().replace(
                  "{-# WARNING\nx \"", "{-DHUN").replace("\"\n #-}", "DHUN-}")
            haskellsourcefile.close()
            haskellsourcefile = open(i, "w")
            haskellsourcefile.write(text[0] + "\n" + haskellsourcecode)
            haskellsourcefile.close()

