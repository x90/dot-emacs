#!/usr/bin/python

import os
import subprocess
import re

# OS = re.sub('/$','',sys.argv[-1])

# oslist = [
#     'OSXLion',
#     'LinuxUbuntu'
#     ]

environ = {
    'PWD':os.getcwd(),
    'HOME':os.environ['HOME']
    }

config = [
    '.emacs',
    '.emacs.d'
    ]

# if OS not in oslist:
#     exit('No OS profile')

for f in config:
    cmd = 'ln -svf %(PWD)s/%(file)s %(HOME)s/%(file)s' % \
          dict(environ.items()+[('file',f)])
    subprocess.call(cmd,shell=True)

