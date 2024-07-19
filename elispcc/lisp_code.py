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
"Lisp code that recursively byte-compile and native-compile .el files."

LISP_CODE = """
(progn
  (setq byte-compile-warnings t)
  (setq byte-compile-verbose t)
  (let* ((user-emacs-directory (getenv "EMACS_D"))
         (default-directory (getenv "EMACS_BYTE_COMP_DIR"))
         (eln-path (getenv "EMACS_ELN_CACHE_DIR"))
         (jobs (string-to-number
                (getenv "EMACS_NATIVE_COMP_ASYNC_JOBS_NUMBER")))
         (byte-comp-enabled
          (/= 0 (string-to-number (getenv "EMACS_BYTE_COMP_ENABLED"))))
         (native-comp-enabled
          (and (/= 0 (string-to-number (getenv "EMACS_NATIVE_COMP_ENABLED")))
               (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))))
    ;; SET ELN-CACHE DIR
    (when native-comp-enabled
      (cond ((fboundp 'startup-redirect-eln-cache) ; Emacs >= 28
             (startup-redirect-eln-cache eln-path))
            ((boundp 'native-comp-eln-load-path)
             (setcar native-comp-eln-load-path
                     (expand-file-name (convert-standard-filename eln-path)
                                       user-emacs-directory)))
            (t (error "Cannot change the eln-path directory"))))

    ;; LOAD DIRECTORIES
    (normal-top-level-add-subdirs-to-load-path)

    ;; SHOW MESSAGES
    (message "[INFO] Byte comp enabled: %s" native-comp-enabled)
    (message "[INFO] Native comp enabled: %s" byte-comp-enabled)
    (message "[INFO] Emacs user directory: %s\n" user-emacs-directory)
    (message "[INFO] Jobs: %s\n" jobs)

    ;; BYTE-COMP
    (when byte-comp-enabled
      (message "[TASK] Byte compile")
      (byte-recompile-directory default-directory 0))

    ;; NATIVE-COMP
    (when native-comp-enabled
      (message "\\n[TASK] Native compile")
      (setq native-comp-async-report-warnings-errors t)
      (setq native-comp-warning-on-missing-source t)
      (when (and jobs (not (string= jobs "")))
        (setq native-comp-async-jobs-number jobs))
      (setq native-comp-deferred-compilation nil)
      (setq package-native-compile nil)
      (native-compile-async default-directory 'recursively)
      (while comp-files-queue
        (sleep-for 0.1)))))
"""
