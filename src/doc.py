#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
generate source code documentation of the Haskell and python files in the ./doc/
directory
"""
import os
if __name__ == '__main__':
    def whereindex(sourcecode):
        for linenumber, line in enumerate(sourcecode.split("\n")):
            if line.find("where") > -1:
                return linenumber
    os.chdir("doc")
    os.system("rm *")
    os.chdir("..")
    for i in os.listdir("."):
        if i.split(".")[-1] == "hs":
            print (i)
            f = open(i)
            S = f.read().replace("{-DHUN", "{-").replace("DHUN-}", "-}")
            f.close()
            f = open("../" + i, "w")
            f.write("\n".join(S.split("\n")[0:whereindex(S)])+"\n" +
                    " ".join(S.split("\n")[whereindex(S)].split(" ")[0:2]) + 
                    " where" + "\n" +
                    "\n".join(S.split("\n")[whereindex(S) + 1:]))
            f.close()
    os.chdir("..")
    os.system("haddock -h mediawiki2latex.hs")
    os.system("haddock --latex mediawiki2latex.hs")
    os.system("pdflatex --interaction=nonstopmode main.tex")
    os.system("pdflatex --interaction=nonstopmode main.tex")
    os.system("pdflatex --interaction=nonstopmode main.tex")
    os.chdir("src")

