#!/usr/bin/env python
# encoding: utf-8
"""
__init__.py

Created by Andrew Cain on 2011-04-06.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import sys
import os

#------------------

def main():
    '''Load all of the files in this directory into attributes of this module.'''
    (path, script_file) = os.path.split(sys.modules[__name__].__file__) 
    dirList=os.listdir(path)
    
    for f in dirList:
        if '.py' in f or f[0] == '.' : continue
        if f == '__pycache__': continue
        
        (dirName, fileName) = os.path.split(f)
        key = fileName.replace('.', '_')
        #print key
        
        fin = open(path + '/' + f)
        data = fin.read()
        fin.close()
        
        setattr(sys.modules[__name__], key, data)

main()