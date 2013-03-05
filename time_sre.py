import sys
from timeit import Timer

if len(sys.argv) > 1 and sys.argv[1] == "-s":
    sys.path.insert(0, "src")
    import _sre_support
    _sre_support.import_sre_py()

import re

patterns = [
    ("Pure literals", re.compile("bar"), "bar", "bazbarfoo"),
    ("Classes and stuff", re.compile(r"\d+.\d+\s\w{,2}"), r"\d+.\d+\s\w{,2}", "Price 144,50 USD"),
    ("Branching and grouping", re.compile(r"<(strong|b|em)>.+?</\1>"), r"<(strong|b|em)>.+?</\1>", "Bla <em>bla</em>"),
]

if __name__ == "__main__":
    for i in range(len(patterns)):
        t = Timer("patterns[%s][1].search(%s)" % (i, repr(patterns[i][3])),
                                                "from __main__ import patterns")
        print "%s: re.search(r'%s', '%s')" \
                % (patterns[i][0], patterns[i][2], patterns[i][3])
        timed = t.timeit(100)
        print "100 passes took %.6f, %.6f per pass" % (timed, timed / 100)
