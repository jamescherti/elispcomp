#!/usr/bin/env bash
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.
#
cd "$(dirname "${BASH_SOURCE[0]}")"
export PYTHONPATH="$(pwd)"
exec pytest -v -v --cov=elispcomp --cov=tests \
  --cov-report=term \
  --cov-report=html:htmlcov \
  tests/test_*.py
