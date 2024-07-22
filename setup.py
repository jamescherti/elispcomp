#!/usr/bin/env python
#
# Copyright (c) 2024 James Cherti
# URL: https://github.com/jamescherti/elispcomp
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.
#

from pathlib import Path

from setuptools import find_packages, setup

setup(
    name="elispcomp",
    version="1.0.7",
    packages=find_packages(),
    description="Compiling Emacs Lisp code from the command-line",
    long_description=((Path(__file__).parent.resolve().joinpath("README.md"))
                      .read_text(encoding="utf-8")),
    long_description_content_type="text/markdown",
    url="https://github.com/jamescherti/elispcomp",
    author="James Cherti",
    python_requires=">=3.6, <4",
    install_requires=['importlib-resources; python_version<="3.8"'],
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "License :: OSI Approved :: GNU General Public License (GPL)",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Environment :: Console",
        "Environment :: Plugins",
        "Intended Audience :: Developers",
        "Intended Audience :: Information Technology",
        "Intended Audience :: System Administrators",
        "Operating System :: POSIX :: Linux",
        "Operating System :: POSIX :: Other",
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python",
        "Topic :: Software Development :: Build Tools",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: System :: Systems Administration",
        "Topic :: Text Editors",
        "Topic :: Utilities",
    ],
    include_package_data=True,
    package_data={
        'elispcomp': ['*.el'],
    },
    entry_points={
        "console_scripts": [
            "elispcomp=elispcomp.__init__:command_line_interface",
        ],
    },
)
