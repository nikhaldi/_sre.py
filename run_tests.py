#!/usr/bin/env python
"""Runs all test cases contained in modules in the 'test' directory.
Pass the -c option to use the _sre C extension module instead of _sre.py."""
__revision__ = "$Id: run_tests.py,v 1.8 2005/02/26 13:10:09 Nik Exp $"

import imp, os.path, sys, unittest

if len(sys.argv) > 1 and sys.argv[1] == "-c":
    sys.argv.pop(1)
else:
    sys.path.append("./src")
    import _sre_support
    _sre_support.import_sre_py()    

import test


# Load all test modules from test
TEST_PREFIX = "test_"
if len(sys.argv) > 1:
    test_mods = [test.__dict__[TEST_PREFIX + sys.argv[1]]]
else:
    test_mods = test.version_specific[sys.version_info[:2]]

# Load all test cases from each module as a suite of its own
loader = unittest.TestLoader()
suites = [loader.loadTestsFromModule(mod) for mod in test_mods]
mod_names = [mod.__name__ for mod in test_mods]

# Run all test suites
runner = unittest.TextTestRunner(verbosity=1)
for (suite, name) in zip(suites, mod_names):
    print "Running tests in %s " % name
    import time
    t = time.time()
    runner.run(suite)
    sys.stdout.flush() # ant won't pick up the print statement otherwise
    print time.time() - t
