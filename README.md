_sre.py 2.4c
============

_sre.py is a reimplementation of CPython's core regular expression engine, the
_sre extension module. It's written in pure Python to allow for easier
experimentation and to benefit alternative Python implementations that can't use
C modules like the standard _sre. This was originally written for use by PyPy.

This version is compatible with _sre from CPython 2.3 and 2.4. Yes, this is by
now quite outdated. Things will probably not work in expected ways for any
version of CPython 2.5 or higher.


Testing
-------

To check for general compliance with your version of Python run the unit tests
with

    $ python run_tests.py

on your shell.


Installing and Using
--------------------

To install _sre.py as a library on your system execute:

    $ python setup.py install

This does not override the standard _sre module of your Python installation. If
you want to use _sre.py instead of the standard module in a Python program, you
have to explicitly load it by inserting these lines in your Python code before
any imports of the re module:

    import _sre_support
    _sre_support.import_sre_py()


History
-------

Release 2.4a, 7/25/2005:
  - initial fully compliant alpha release

Release 2.4b, 8/9/2005:
  - implemented some substantial optimizations

Release 2.4c, 8/30/2005:
  - fixed bug related to the definition of a unicode word character
  - added timing script
  - postscript 3/5/2012: republished code and binary on GitHub


License
-------

Copyright (c) 2005 by Nik Haldimann. It comes with a MIT-style license. See the
LICENSE.txt file for details.
