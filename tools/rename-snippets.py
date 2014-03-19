import os
import re
import stat
import sys

key_re = re.compile('^# key: (.*)')
d = sys.argv[1]


def get_available_path(dirname, name):
    pn = os.path.join(dirname, name + '.yasnippet')
    if not os.path.isfile(pn):
        return pn
    i = 1
    while True:
        pn = os.path.join(dirname, name + '.%i.yasnippet' % i)
        if not os.path.isfile(pn):
            return pn
        i += 1


for fn in os.listdir(d):
    pn = os.path.join(d, fn)
    if not stat.S_ISREG(os.stat(pn).st_mode):
        continue
    with open(pn) as f:
        matches = [key_re.match(line) for line in f if line.startswith('# key')]
        if matches:
            key = matches[0].group(1)
            npn = get_available_path(d, key)
    os.rename(pn, npn)
        


