#!/usr/bin/env python
# encoding: utf-8
"""
EEBNF_exporter.py

Created by Andrew Cain on 2011-04-29.
Copyright (c) 2011 Andrew Cain. All rights reserved.
"""

from eebnf_to_tikzpicture import EEBNFToTikzPicture

import tikz_picture

import sys
import os
import platform
import subprocess
import logging
import errno
logger = logging.getLogger("EEBNFExporter")

def get_os_name():
    """ Returns the name of the Operating System."""
    osName = platform.system()
    if osName == "Darwin":
        return "Mac OS X"
    elif osName == "Linux":
        return "Linux"
    else:
        return "Windows"


def run_bash(script_name, opts):
    if get_os_name() == "Windows":
        exec_list = ["bash", script_name]
    else:
        exec_list = [script_name]

    if opts:
        if isinstance(opts, list):
            exec_list.extend(opts)
        else:
            exec_list.append(opts)

    # output_line('Running bash script: ' + str(exec_list))

    proc = subprocess.Popen(exec_list, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()

    if proc.returncode != 0:
        print("Error running script: ", script_name, " ", opts)
        print(out, err)

        if (get_os_name() == "Windows"):
          print("Make sure you have msys/bin in your environment PATH variable")

        quit()


class EEBNFExporter():

    def _mkdir_p(self, path):
        try:
            os.makedirs(path)
        except OSError as exc: # Python >2.5
            if exc.errno == errno.EEXIST:
                pass
            else: raise

    def export(self, code_root, output_root, as_image):
        '''
        Export all of the ebnf files from the directories within the code_root, and save
        tex files into matching directories in the output_root
        '''

        converter = EEBNFToTikzPicture()

        logger.info('Exporting code from %s to %s' % (code_root, output_root) )

        for root, dirs, files in os.walk(code_root):
            for file in [f for f in files if f.endswith(".ebnf")]:
                full_path = os.path.join(root, file)
                img_path = full_path.replace(code_root, output_root).replace('ebnf', 'png')
                tex_path = full_path.replace(code_root, output_root).replace('ebnf', 'tex')

                out_dir = root.replace(code_root, output_root)

                if (as_image and ((not os.path.isfile(img_path) or os.stat(full_path).st_mtime > os.stat(img_path).st_mtime))) or (not as_image and ((not os.path.isfile(tex_path) or os.stat(full_path).st_mtime > os.stat(tex_path).st_mtime))) :
                    logger.info('  - Exporting %s' % full_path )
                    output = converter.convert_file(full_path)


                    self._mkdir_p(out_dir)

                    out_path = full_path.replace(code_root, output_root).replace('ebnf', 'tex')
                    logger.info('  - Exporting %s' % out_path )
                    out_file = open(out_path, 'w')
                    if as_image:
                        out_file.write(tikz_picture.file_header_tex + "\n")
                    out_file.write(output)
                    if as_image:
                        out_file.write(tikz_picture.file_footer_tex + "\n")
                    out_file.close()

                    logger.info('  - Compiling %s' % out_path )
                    if as_image:
                        run_bash('./build-file.sh', [out_dir, file.replace('ebnf', 'tex')])

#----------------------------------------------------------------------------

def test_basic():
    p = EEBNFExporter()

    src_path = None
    dest_path = None
    as_image = False

    if len(sys.argv) == 4:
        src_path = sys.argv[1]
        dest_path = sys.argv[2]
        as_image = sys.argv[3].lower().startswith('y')

    if src_path == None:
        src_path = os.getenv("SYNTAX_PATH_IN", None)
    if dest_path == None:
        dest_path = os.getenv("SYNTAX_PATH_OUT", None)

    if src_path == None:
        print("Enter path to syntax: ",)
        src_path = input()
    
    if dest_path == None:
        print("Enter output path: ",)
        dest_path = input()

    p.export(src_path, dest_path, as_image)

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    test_basic()
