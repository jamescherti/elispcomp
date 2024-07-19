# elispcomp - Compiling Emacs Lisp code from the command-line

The `elispcomp` command-line tool allows compiling Elisp (Emacs Lisp) code efficiently, providing optimized .elc and .eln files for improved performance in Emacs.

This tool simplifies the compilation of Emacs Lisp (Elisp) code by generating optimized .elc and .eln files directly from the command-line. Utilizing Emacs, it recursively compiles all Elisp files within a specified directory. The tool supports various compilation configurations, enabling users to adapt the process to their specific needs.

With an emphasis on simplicity and efficiency, it offers a powerful solution for managing and optimizing Emacs Lisp codebases from the command-line.

## Installation

Here is how to install `elispcomp` using pip:
```
pip install --user elispcomp
```

The pip command above will install the `elispcomp` executable in the directory `~/.local/bin/`.

## Usage

```
usage: elispcomp [--option] [args]

Recursively byte-compile and native-compile .el files.

positional arguments:
  N                     The directories to be scanned recursively by Emacs to locate the '.el' files for compilation.

options:
  -h, --help            show this help message and exit
  -c ELN_CACHE, --eln-cache ELN_CACHE
                        The eln-cache directory where Emacs stores the compiled native compiled code. Default: ~/.emacs.d/eln-cache
```

## License

Copyright (c) [James Cherti](https://www.jamescherti.com).

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## Links

- [The 'elispcomp' Git repository](https://github.com/jamescherti/elispcomp)
