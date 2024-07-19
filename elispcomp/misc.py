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
"""Miscellaneous functions."""

from pathlib import Path
from typing import List


def unique_directories(list_dirs: List[Path]) -> List[str]:
    """Return directories ensuring no directory is contained within another.

    :param list_dirs: A list of directory paths.
    :return: A list of unique directories with no directory being a
    subdirectory of another.
    """
    # Resolve and sort the paths
    paths = (Path(dir).resolve() for dir in sorted(list_dirs))
    result = []
    for path in paths:
        if not any(path.is_relative_to(cur_result) for cur_result in result):
            result.append(path)
    return [str(cur_path) for cur_path in result]
