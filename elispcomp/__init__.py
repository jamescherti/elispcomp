#!/usr/bin/env python
#
# Copyright (c) James Cherti
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
"""Recursively byte compilation and native compilation .el files."""

import argparse
import os
import shutil
import subprocess
import sys

from .lisp_code import LISP_CODE

# Global variables
EMACS_NATIVE_COMP_ENABLED = True


class ElispCompileCli:
    """Recursively byte compilation and native compilation .el files."""

    def __init__(self):
        """Run the 'emacscomp' command-line interface."""
        self.args = None
        self._parse_args()

        self._flight_checks()

        if not self.args.directories:
            print("Nothing to do.")
            sys.exit(1)

        for directory in self.args.directories:
            self._compile(directory)

    def _parse_args(self):
        """Parse command-line arguments."""
        usage = "%(prog)s [--option] [N]"
        parser = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                         usage=usage)
        parser.add_argument(
            "directories", metavar="N", type=str, nargs="*",
            help=("The directories to be scanned recursively by Emacs "
                  "to locate the '.el' files for compilation."),
        )

        parser.add_argument(
            "-c",
            "--eln-cache-dir",
            default=None,
            required=False,
            help=("The eln-cache directory where Emacs stores the "
                  "compiled native compiled code. Defaults to "
                  "the default Emacs eln-cache directory."),
        )

        parser.add_argument(
            "-e",
            "--emacs-bin",
            default="emacs",
            required=False,
            help="Path to the Emacs binary. Defaults: emacs",
        )

        half_cpus = os.cpu_count() // 2
        parser.add_argument(
            "-j",
            "--jobs",
            type=int,
            default=half_cpus,
            required=False,
            help=("Specify the number of parallel jobs for compilation. "
                  "Default: Half the number of available CPUs"),
        )

        parser.add_argument(
            "-b",
            "--disable-byte-comp",
            default=False,
            action="store_true",
            required=False,
            help="Disable byte-compile. Default: enabled",
        )

        parser.add_argument(
            "-n",
            "--disable-native-comp",
            default=False,
            action="store_true",
            required=False,
            help="Disable native compilation. Default: enabled",
        )

        self.args = parser.parse_args()
        if self.args.disable_native_comp and self.args.disable_byte_comp:
            print("Error: Nothing to do. Both byte compilation and native "
                  "compilation are disabled.", file=sys.stderr)
            sys.exit(1)

    def _flight_checks(self):
        if not shutil.which(self.args.emacs_bin):
            print("Error: Command not found or not executable: "
                  f"{self.args.emacs_bin}", file=sys.stderr)
            sys.exit(1)

        if self.args.eln_cache_dir and \
                not os.path.exists(self.args.eln_cache_dir):
            try:
                os.mkdir(self.args.eln_cache_dir)
            except OSError as err:
                print(f"Error: {err}")
                sys.exit(1)
            else:
                self.args.eln_cache_dir = \
                    os.path.abspath(self.args.eln_cache_dir)

        for directory in self.args.directories:
            if not os.path.exists(directory):
                print(f"Error: The directory does not exist at the "
                      f"specified path: {directory}")
                sys.exit(1)

            if not os.path.isdir(directory):
                print("Error: The specified path is not a directory: "
                      f"{directory}")
                sys.exit(1)

    def _compile(self, directory: str):
        env = os.environ.copy()
        env["EMACS_BYTE_COMP_DIR"] = os.path.abspath(directory)
        env["EMACS_NATIVE_COMP_ASYNC_JOBS_NUMBER"] = \
            str(self.args.jobs) if self.args.jobs else ""
        env["EMACS_NATIVE_COMP_ENABLED"] = \
            '0' if self.args.disable_native_comp else '1'
        env["EMACS_BYTE_COMP_ENABLED"] = \
            '0' if self.args.disable_byte_comp else '1'
        env["EMACS_ELN_CACHE_DIR"] = \
            self.args.eln_cache_dir if self.args.eln_cache_dir else ""

        emacs_bin = shutil.which(self.args.emacs_bin)
        print("[INFO] Emacs binary:", emacs_bin)
        emacs_cmd = [emacs_bin, "--batch", "--eval", LISP_CODE]
        try:
            subprocess.check_call(emacs_cmd, env=env,
                                  stderr=subprocess.STDOUT)
            print("Success.")
        except subprocess.CalledProcessError as err:
            print(f"Error: {err}")
            sys.exit(1)


def command_line_interface():
    """Run the command-line interface."""
    ElispCompileCli()
