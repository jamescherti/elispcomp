#!/usr/bin/env python
#
# Copyright (c) 2024-2025 James Cherti
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
"""Recursively byte and native compile .el files."""

import argparse
import os
import shutil
import subprocess
import sys
import tempfile

from .misc import unique_directories

if sys.version_info >= (3, 9):
    from importlib.resources import files
else:
    from importlib_resources import files


class ElispcompCli:
    """Recursively byte and native compile .el files."""

    def __init__(self):
        """Run the 'emacscomp' command-line interface."""
        self.args = None
        self._parse_args()

        self._flight_checks()

        if not self.args.dirs_or_files:
            print("Nothing to do.")
            sys.exit(1)

        for directory in self.args.dirs_or_files:
            self._compile(directory)

    def _parse_args(self):
        """Parse command-line arguments."""
        usage = "%(prog)s [--option] [N]"
        parser = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                         usage=usage)
        parser.add_argument(
            "dirs_or_files", metavar="N", type=str, nargs="*",
            help=("The .el files or directories containing .el files "
                  "to compile."),
        )

        parser.add_argument(
            "-c",
            "--eln-cache",
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
            "--disable-byte-compile",
            "--disable-byte-comp",
            default=False,
            action="store_true",
            required=False,
            help="Disable byte compile. Default: enabled",
        )

        parser.add_argument(
            "-n",
            "--disable-native-compile",
            "--disable-native-comp",
            default=False,
            action="store_true",
            required=False,
            help="Disable native compilation. Default: enabled",
        )

        parser.add_argument(
            "-i",
            "--load-path",
            default=[],
            action="append",
            required=False,
            help=("Add the specified directory to the Emacs `load-path`. "
                  "This option can be used multiple times to include several "
                  "directories."),
        )

        parser.add_argument(
            "-a",
            "--ensure-native-compile-available",
            "--ensure-native-comp-available",
            default=False,
            action="store_true",
            required=False,
            help=("Fail when native compilation is not available. "
                  "Default: disabled"),
        )

        parser.add_argument(
            "-l",
            "--compile-initial-load-paths",
            default=False,
            action="store_true",
            required=False,
            help=("Also compile the initial paths in `load-path` "
                  "(generally Emacs Lisp code) and site run file. "
                  "Default: disabled."),
        )

        self.args = parser.parse_args()

    def _flight_checks(self):
        if not shutil.which(self.args.emacs_bin):
            print("Error: Command not found or not executable: "
                  f"{self.args.emacs_bin}", file=sys.stderr)
            sys.exit(1)

        if self.args.eln_cache and \
                not os.path.exists(self.args.eln_cache):
            try:
                os.mkdir(self.args.eln_cache)
            except OSError as err:
                print(f"Error: {err}")
                sys.exit(1)
            else:
                self.args.eln_cache = \
                    os.path.abspath(self.args.eln_cache)

        for directory in self.args.dirs_or_files + self.args.load_path:
            if not os.path.exists(directory):
                print(f"Error: The directory does not exist at the "
                      f"specified path: {directory}")
                sys.exit(1)

            if not os.path.isdir(directory) and not os.path.isfile(directory):
                print("Error: The specified path is not a file or a "
                      f"directory: {directory}")
                sys.exit(1)

        if self.args.disable_native_compile and self.args.disable_byte_compile:
            print("Error: Nothing to do. Both byte compilation and native "
                  "compilation are disabled.", file=sys.stderr)
            sys.exit(1)

    def _compile(self, dir_or_file: str):
        env = os.environ.copy()
        dir_or_file = os.path.abspath(dir_or_file)
        byte_comp_dir = dir_or_file \
            if os.path.isdir(dir_or_file) \
            else os.path.dirname(dir_or_file)
        env["EMACS_BYTE_COMP_DIR"] = byte_comp_dir
        env["EMACS_DIR_OR_FILE"] = dir_or_file
        env["EMACS_NATIVE_COMP_ASYNC_JOBS_NUMBER"] = \
            str(self.args.jobs) if self.args.jobs else ""
        env["EMACS_NATIVE_COMP_ENABLED"] = \
            '0' if self.args.disable_native_compile else '1'
        env["EMACS_BYTE_COMP_ENABLED"] = \
            '0' if self.args.disable_byte_compile else '1'
        env["EMACS_ENSURE_NATIVE_COMP_AVAILABLE"] = \
            '1' if self.args.ensure_native_compile_available else '0'
        env["EMACS_ELN_CACHE_DIR"] = \
            self.args.eln_cache if self.args.eln_cache else ""
        env["EMACS_COMPILE_INITIAL_LOAD_PATHS"] = \
            '1' if self.args.compile_initial_load_paths else "0"

        env["EMACS_LOAD_PATH_LIST"] = \
            "\n".join(unique_directories([byte_comp_dir] +
                                         self.args.load_path))

        emacs_bin = shutil.which(self.args.emacs_bin)
        print("[INFO] Emacs binary:", emacs_bin)

        try:
            temp_file_path = None
            try:
                with tempfile.NamedTemporaryFile(prefix="elispcomp-",
                                                 suffix=".el",
                                                 delete=False) \
                        as temp_file:
                    lisp_code = \
                        files(__package__) \
                        .joinpath('elispcomp.el') \
                        .read_bytes()
                    temp_file.write(lisp_code)
                    temp_file_path = temp_file.name

                subprocess.check_call([emacs_bin, "--script",
                                       temp_file_path],
                                      env=env, stderr=subprocess.STDOUT)
            except KeyboardInterrupt:
                sys.exit(1)
            finally:
                if temp_file_path:
                    os.unlink(temp_file_path)

            print("Success.")
        except subprocess.CalledProcessError as err:
            print(f"Error: {err}")
            sys.exit(1)
