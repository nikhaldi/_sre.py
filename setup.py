"""
_sre.py distutils setup. To install _sre.py as a library on your system use:

    python setup.py install

"""
from distutils.core import setup

version = "2.4c"
long_description = """
_sre.py is a reimplementation of CPython's core regular expression engine, the
_sre extension module. It's written in pure Python to allow for easier
experimentation and to benefit alternative Python implementations that can't use
C modules like the standard _sre. This version is compatible with _sre from
CPython 2.3 and 2.4.
"""

def main():
    setup(
         name="_sre.py",
         version=version,
         author="Nik Haldimann",
         author_email="nhaldimann@gmail.com",
         url="https://github.com/nikhaldi/_sre.py",
         description="a reimplementation of _sre in pure Python",
         long_description=long_description,
         download_url="http://nikhaldi.github.com/_sre.py/downloads/_sre.py-%s.tar.gz" % version,
         classifiers=[
            "Development Status :: 4 - Beta",
            "Intended Audience :: Developers",
            "License :: OSI Approved :: MIT License",
            "Operating System :: OS Independent",
            "Programming Language :: Python",
            "Topic :: Text Processing"
        ],
         package_dir={"": "src"},
         py_modules=["_sre", "_sre_support"]
    )


if __name__ == "__main__":
    main()
