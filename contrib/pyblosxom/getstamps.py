"""
Run this file 'python getstamps.py' from your pyblosxom data-dir.

You may need to make some modification for your situation. This
assumes your blog entries use a .txt extension.

Hacked on by Michael Olson <http://www.mwolson.org/>.
"""
__author__ = 'Nathan Kent Bullock'
__homepage__ = 'http://bullock.moo.com/nathan/'
__email__ = 'nathan_kent_bullock -at- yahoo.ca'
__version__ = '1.0'

import re, sys, os, types

OutFile=None

DateRegexp = re.compile (r'^#date\s+(.+)$')

def getdate(f):
    for line in f:
        matched = DateRegexp.search(line)
        if matched:
            return matched.group(1)

def recurse(so_far):
    global OutFile

    for filename in os.listdir(so_far):
        filepath = so_far + "/" + filename

        # just makes output prettier.
        if filename == ".svn": continue

        if os.path.isdir(filepath):
            print "dir %s" % (filepath,)
            recurse(filepath)

        # You may need to modify the extension test
        if os.path.isfile(filepath) and filepath != "timestamps":
            thisfile = open(filepath,'r')
            thisdate = getdate (thisfile)
            if thisdate:
                OutFile.write("%s %s\n" % (thisdate, filepath[2:]))
                continue

if __name__ == "__main__":
    OutFile = open("timestamps", "w+")
    recurse(".")
