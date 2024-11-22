# elispcomp - Compile Emacs Lisp code from the command-line

(The author also recommends trying: **[compile-angel.el](https://github.com/jamescherti/compile-angel.el)**. The compile-angel package automatically byte-compiles and native-compiles Emacs Lisp libraries, transparently, without any user intervention.)

## Introduction

The `elispcomp` command line tool allows compiling Emacs Lisp (Elisp) code directly from the terminal or from a shell script. It facilitates the generation of optimized `.elc` (byte-compiled) and `.eln` (native-compiled) files, which can significantly improve the performance of Emacs.

The command line tool executes a headless instance of Emacs and Elisp that recursively scans the specified directories, byte compiling and native compiling all the `.el` files that haven't been compiled yet. It supports various configuration options, allowing you to adapt the compilation process to suit your needs.

When configured appropriately, Emacs can compile to both `.elc` and `.eln` files. However, for those who wish to automate the background compilation of `.el` files using a script, the `elispcomp` command-line tool can be beneficial in ensuring that their Emacs setup remains up-to-date without manual intervention and without starting an Emacs instance.

## Installation

To get started with `elispcomp`, you can install it using `pip`:
```
pip install --user elispcomp
```

This command installs `elispcomp` and places the executable in your `~/.local/bin/` directory, making it easily accessible from your command line.

## Requirements

- Python 3
- Emacs >= 28

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
  N                     The directories to be scanned recursively by Emacs to locate the '.el'
                        files for compilation.

options:
  -h, --help            show this help message and exit
  -c ELN_CACHE, --eln-cache ELN_CACHE
                        The eln-cache directory where Emacs stores the compiled native compiled
                        code. Defaults to the default Emacs eln-cache directory.
  -e EMACS_BIN, --emacs-bin EMACS_BIN
                        Path to the Emacs binary. Defaults: emacs
  -j JOBS, --jobs JOBS  Specify the number of parallel jobs for compilation. Default: Half the
                        number of available CPUs
  -b, --disable-byte-compile, --disable-byte-comp
                        Disable byte compile. Default: enabled
  -n, --disable-native-compile, --disable-native-comp
                        Disable native compilation. Default: enabled
  -i LOAD_PATH, --load-path LOAD_PATH
                        Recursively adds the subdirectories of the specified directory to the
                        Emacs `load-path`. This option can be used multiple times to include
                        several directories.
  -a, --ensure-native-compile-available, --ensure-native-comp-available
                        Fail when native compilation is not available.
```

## Frequently asked questions

### Can't I achieve the same result using Emacs?

Indeed, when configured appropriately, Emacs can compile to both `.elc` and `.eln` files.

The `elispcomp` command-line tool is an Emacs wrapper that makes it easy to compile Emacs Lisp (Elisp) code directly from the terminal or from a script. It provides options and ensures that the byte compilation and native compilation processes are truly finished before quitting.

The `elispcomp` command-line tool can also be used in conjunction with other tools like `parallel` to speed up the compilation process.

### How does the author utilize elispcomp?

The author utilizes the `elispcomp` tool to compile multiple Emacs Lisp files across various machines and Emacs versions. With a diverse range of machines and Emacs versions in his workflow, `elispcomp` ensures consistent compilation results and compatibility.

Additionally, the author employs the `parallel` command-line tool to enhance the efficiency of the byte-compilation process. This tool allows the distribution of the compilation workload across multiple processors, significantly accelerating the process.

### Is elispcomp written in Python or Elisp?

Most of what `elispcomp` does is written in Elisp (Emacs Lisp), including scanning directories, which is handled by the Emacs functions `(byte-recompile-directory)` and `(native-compile-async)`. Python handles the command-line interface (arguments), and Emacs and the Elisp code does the rest (byte compilation, native compilation, and waiting until all the .el files are byte compiled and native compiled).

## License

The `elispcomp` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (c) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## Links

- [elispcomp @GitHub](https://github.com/jamescherti/elispcomp)
- [elispcomp @PyPI](https://pypi.org/project/elispcomp/)
- [Article: Introducing elispcomp: Compiling Elisp code directly from the command line](https://www.jamescherti.com/elispcomp-elisp-compilation-from-command-line/)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and any other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): An Emacs package that automatically compiles Emacs loaded or saved Elisp libraries. This lightweight alternative to auto-compile not only compiles Elisp files loaded with load and require, but also handles deferred and autoloaded files.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
