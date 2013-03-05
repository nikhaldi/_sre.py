# Makes the test directory a package
import test_sre_py
import test_re_235
import test_re_241

version_specific = {
    (2, 3): [test_sre_py, test_re_235],
    (2, 4): [test_sre_py, test_re_241]
}
