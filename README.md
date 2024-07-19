# elispcomp - Compiling Emacs Lisp code from the command-line

The `elispcomp` command line tool allows compiling Emacs Lisp (Elisp) code directly from the terminal or from a shell script. It facilitates the generation of optimized `.elc` (byte-compiled) and `.eln` (native-compiled) files, which can significantly improve the performance of Emacs.

The command line tool executes a headless instance of Emacs that recursively scans the specified directories, byte compiling and native compiling all `.el` files it encounters. It supports various configuration options, allowing you to adapt the compilation process to suit your needs.

When configured appropriately, Emacs can compile to both `.elc` and `.eln` files. However, for those who wish to automate the background compilation of `.el` files using a script, the `elispcomp` command-line tool can be beneficial in ensuring that their Emacs setup remains up-to-date without manual intervention and without starting an Emacs instance.

## Installation

To get started with `elispcomp`, you can install it using `pip`, Python’s package installer:
```
pip install --user elispcomp
```

This command installs `elispcomp` and places the executable in your `~/.local/bin/` directory, making it easily accessible from your command line.

## Usage

The `elispcomp` command line tool is straightforward to use.

First example: To compile all `.el` files located in the `~/.emacs.d/lisp` directory:
```
elispcomp ~/.emacs.d/lisp
```

Second example: To compile all `.el` files located in the `~/.emacs.d/lisp` directory, and store the native-compiled files in the `~/.emacs.d/eln-cache` directory:
```
elispcomp --eln-cache ~/.emacs.d/eln-cache ~/.emacs.d/lisp
```

## Command line options

```
usage: elispcomp [--option] [N]

Recursively byte and native compile .el files.

positional arguments:
  N                     The directories to be scanned recursively by Emacs to locate the '.el' files for compilation.

options:
  -h, --help            show this help message and exit
  -c ELN_CACHE, --eln-cache ELN_CACHE
                        The eln-cache directory where Emacs stores the compiled native compiled code. Defaults to the default Emacs eln-cache directory.
  -e EMACS_BIN, --emacs-bin EMACS_BIN
                        Path to the Emacs binary. Defaults: emacs
  -j JOBS, --jobs JOBS  Specify the number of parallel jobs for compilation. Default: Half the number of available CPUs
  -b, --disable-byte-comp
                        Disable byte-compile. Default: enabled
  -n, --disable-native-comp
                        Disable native compilation. Default: enabled
  -a LOAD_PATH, --load-path LOAD_PATH
                        Recursively adds the subdirectories of the specified directory to the Emacs `load-path`. This option can be used multiple times to
                        include several directories.
```

## License

Copyright (c) [James Cherti](https://www.jamescherti.com).

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## Links

- [elispcomp @GitHub](https://github.com/jamescherti/elispcomp)
- [Article: Introducing elispcomp: Compiling Elisp code directly from the command line](https://www.jamescherti.com/elispcomp-elisp-compilation-from-command-line/)
