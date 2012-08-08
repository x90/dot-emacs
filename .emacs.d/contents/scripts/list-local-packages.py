import os
with open('../local-packages.txt','w') as f:
    for elem in os.listdir('../local-packages'):
        f.write(elem+'\n')
