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
"Recursively byte-compile and native-compile .el files."

import argparse
import os
import shutil
import subprocess
import sys

from .lisp_code import LISP_CODE

# Global variables
EMACS_BYTE_COMP_ENABLED = True
EMACS_NATIVE_COMP_ENABLED = True

EMACS_D = os.path.expanduser("~/.emacs.d")


class EmacsCompile:
    def __init__(self):
        self.args = None
        self._parse_args()

        self._flight_checks()
        self.update_os_environment()

        if not self.args.directories:
            print("Nothing to do.")
            sys.exit(1)

        for directory in self.args.directories:
            print(f"[INFO] Recursively compile the directory: {directory}")
            self.compile(directory)

    def _parse_args(self):
        """Parse command-line arguments."""
        usage = "%(prog)s [--option] [args]"
        parser = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                         usage=usage)
        parser.add_argument(
            "directories", metavar="N", type=str, nargs="*",
            help=("The directories to be scanned recursively by Emacs "
                  "to locate the '.el' files for compilation."),
        )

        default_eln_cache_dir = os.path.join(EMACS_D, "eln-cache")
        parser.add_argument(
            "-c",
            "--eln-cache",
            default=default_eln_cache_dir,
            required=False,
            help=("The eln-cache directory where Emacs stores the "
                  "compiled native compiled code. Defaults to "
                  f"'{default_eln_cache_dir}'"),
        )

        parser.add_argument(
            "-e",
            "--emacs-bin",
            default="emacs",
            required=False,
            help="Path to the Emacs binary. Defaults to 'emacs'.",
        )

        half_cpus = os.cpu_count() // 2
        parser.add_argument(
            "-j",
            "--jobs",
            type=int,
            default=half_cpus,
            required=False,
            help=("Specify the number of parallel jobs for compilation. "
                  "Defaults to half the number of available CPUs if "
                  "not provided"),
        )

        parser.add_argument(
            "-b",
            "--byte-compile",
            default=False,
            action="store_true",
            required=False,
            help="Enable byte-compile. Disabled by default.",
        )

        parser.add_argument(
            "-n",
            "--native-compile",
            default=False,
            action="store_true",
            required=False,
            help="Enable native-compile. Disabled by default.",
        )

        self.args = parser.parse_args()

    def update_os_environment(self):
        os.environ["EMACS_D"] = EMACS_D
        os.environ["EMACS_NATIVE_COMP_ASYNC_JOBS_NUMBER"] = \
            str(self.args.jobs) if self.args.jobs else ""
        os.environ["EMACS_NATIVE_COMP_ENABLED"] = \
            '1' if self.args.native_compile else '0'
        os.environ["EMACS_BYTE_COMP_ENABLED"] = \
            '1' if self.args.byte_compile else '0'
        os.environ["EMACS_ELN_CACHE_DIR"] = self.args.eln_cache

    def _flight_checks(self):
        if not EMACS_BYTE_COMP_ENABLED and not EMACS_NATIVE_COMP_ENABLED:
            print("Error: Neither byte compilation nor native compilation "
                  "is enabled.", file=sys.stderr)
            sys.exit(1)

        if not shutil.which(self.args.emacs_bin):
            print("Error: Command not found or not executable: "
                  f"{self.args.emacs_bin}", file=sys.stderr)
            sys.exit(1)

        if not os.path.isdir(self.args.eln_cache):
            try:
                os.mkdir(self.args.eln_cache)
            except OSError as err:
                print(f"Error: {err}")
                sys.exit(1)

        if not os.path.isdir(EMACS_D):
            print(f"Error: The directory does not exist: {EMACS_D}")
            sys.exit(1)

        for directory in self.args.directories:
            if not os.path.exists(directory):
                print(f"Error: The directory does not exist at the "
                      f"specified path: {directory}")
                sys.exit(1)

            if not os.path.isdir(directory):
                print("Error: The specified path is not a directory: "
                      f"{directory}")
                sys.exit(1)

    def compile(self, directory: os.PathLike):
        env = os.environ.copy()
        env["EMACS_BYTE_COMP_DIR"] = directory

        print("[INFO] Emacs binary:", self.args.emacs_bin)
        emacs_cmd = [self.args.emacs_bin, "--batch", "--eval", LISP_CODE]
        try:
            subprocess.check_call(emacs_cmd, env=env,
                                  stderr=subprocess.STDOUT)
            print("Success.")
        except subprocess.CalledProcessError as err:
            print(f"Error: {err}")
            sys.exit(1)


def command_line_interface():
    """The command-line interface."""
    EmacsCompile()
