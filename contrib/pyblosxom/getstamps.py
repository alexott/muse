"""
Run 'python getstamps.py' from the directory that contains your
unpublished blog entries.

You may need to make some modification for your situation. This
assumes your blog entries use a .txt extension and that you generate
them from "source" files in a different directory (with an optional
.muse extension).

History:

1.1

* Michael Olson <http://www.mwolson.org/> adapted this for Emacs Muse
  and added a few more exluded patterns for other version control
  systems.

1.0

* Original version
"""
__author__ = 'Nathan Kent Bullock'
__homepage__ = 'http://bullock.moo.com/nathan/'
__email__ = 'nathan_kent_bullock -at- yahoo.ca'
__version__ = '1.1'

import re, sys, os, types

OutFile=None

# The format of the date line in each blog entry
DateRegexp = re.compile (r'^#date\s+(.+)$')

# The part of the filename of the blog entry to write to the
# timestamps file.  Only the first grouping will be used.
FileNameRegexp = re.compile (r'^(.+?)(\.muse)?$')

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
        if filename == ".arch-ids": continue
        if filename == "{arch}": continue
        if filename == ".bzr": continue
        if filename == "_darcs": continue

        if os.path.isdir(filepath):
            print "dir %s" % (filepath,)
            recurse(filepath)

        # You may need to modify the extension test
        if os.path.isfile(filepath) and filepath != "timestamps":
            thisfile = open(filepath,'r')
            thisdate = getdate (thisfile)
            matched = FileNameRegexp.search(filepath[2:])
            if thisdate and matched:
                thisname = matched.group(1) + ".txt"
                OutFile.write("%s %s\n" % (thisdate, thisname))
                continue

if __name__ == "__main__":
    OutFile = open("timestamps", "w+")
    recurse(".")
