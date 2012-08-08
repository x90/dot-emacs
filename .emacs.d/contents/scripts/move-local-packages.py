import os
packages = [x.replace('\n','') for x in open('../local-packages.txt','r')]
here = os.getcwd()
os.chdir('..')
for elem in packages:
    # print elem,os.path.join('local-packages',elem)
    os.rename(elem,os.path.join('local-packages',elem))
os.chdir(here)

