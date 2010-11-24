import re
from operator import add
p = re.compile('[ (]([a-z-]*\\-?buffer\\-?[a-z-]*)[ )]')
buffervars = []
on = 0
for line in open('org-export-latex.el','r'):
    if 'defun org-export-as-latex' in line: on = 1
    if ';;; Parsing functions:' in line: on = 0
    if on == 1:
        x = p.search(line)
        if x: buffervars.append(reduce(add,x.groups()))

uniq = sorted(set(buffervars))
print ', '.join(uniq)
