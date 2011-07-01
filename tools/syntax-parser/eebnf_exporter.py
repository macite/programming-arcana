#!/usr/bin/env python
# encoding: utf-8
"""
EEBNF_exporter.py

Created by Andrew Cain on 2011-04-29.
Copyright (c) 2011 Andrew Cain. All rights reserved.
"""

from eebnf_to_tikzpicture import EEBNFToTikzPicture

import sys
import os
import logging
import errno
logger = logging.getLogger("EEBNFExporter")

class EEBNFExporter():
    
    def _mkdir_p(self, path):
        try:
            os.makedirs(path)
        except OSError as exc: # Python >2.5
            if exc.errno == errno.EEXIST:
                pass
            else: raise
    
    def export(self, code_root, output_root):
        '''
        Export all of the ebnf files from the directories within the code_root, and save
        tex files into matching directories in the output_root
        '''
        
        converter = EEBNFToTikzPicture()
        
        logger.info('Exporting code from %s to %s' % (code_root, output_root) )
        
        for root, dirs, files in os.walk(code_root):
            for file in [f for f in files if f.endswith(".ebnf")]:
                full_path = os.path.join(root, file)
                logger.info('  - Exporting %s' % full_path )
                output = converter.convert_file(full_path)
                
                out_dir = root.replace(code_root, output_root)
                self._mkdir_p(out_dir)
                
                out_path = full_path.replace(code_root, output_root).replace('ebnf', 'tex')
                logger.info('  - Exporting %s' % out_path )
                out_file = open(out_path, 'w')
                out_file.write(output)
                out_file.close()
        
    
#----------------------------------------------------------------------------

def test_basic():
    p = EEBNFExporter()
    p.export('../../syntax', '../../syntax-out',)

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    test_basic()
