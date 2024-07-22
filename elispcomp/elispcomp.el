;;; elispcomp.el --- Compile Elisp code from the command-line -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1
;; URL: https://github.com/jamescherti/elispcomp
;; Keywords: development, tools, convenience, byte-compilation, native-compilation, build, productivity, automation
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Lisp code that recursively byte-compile and native-compile .el files.

;;; Code:

(defun elispcomp-add-load-paths (paths-string)
  "Add directories specified in PATHS-STRING to `load-path`.
PATHS-STRING should be a string containing directories separated by newline
characters."
  (let ((paths (split-string paths-string "\n" t)))
    (dolist (path paths)
      (let ((default-directory (expand-file-name path)))
        (normal-top-level-add-subdirs-to-load-path)))))

(defvar elispcomp-native-comp-finished nil
  "Description.")

(defun elispcomp-native-compilation-finish-hook ()
  "Hook function to set local compilation status to t."
  (setq elispcomp-native-comp-finished t))

(let* ((default-directory (getenv "EMACS_BYTE_COMP_DIR"))
       (load-path-list (getenv "EMACS_LOAD_PATH_LIST"))
       (eln-cache-dir (getenv "EMACS_ELN_CACHE_DIR"))
       (jobs (getenv "EMACS_NATIVE_COMP_ASYNC_JOBS_NUMBER"))
       (ensure-native-comp-available
        (/= 0 (string-to-number (getenv "EMACS_ENSURE_NATIVE_COMP_AVAILABLE"))))
       (byte-comp-enabled
        (/= 0 (string-to-number (getenv "EMACS_BYTE_COMP_ENABLED"))))
       (native-comp-enabled
        (/= 0 (string-to-number (getenv "EMACS_NATIVE_COMP_ENABLED"))))
       (native-comp-available (and (featurep 'native-compile)
                                   (fboundp 'native-comp-available-p)
                                   (native-comp-available-p))))
  (when (and native-comp-enabled
             ensure-native-comp-available
             (not native-comp-available))
    (error "Native comp is not available"))

  ;; SET ELN-CACHE DIR
  (if (and native-comp-enabled
           native-comp-available
           (not (string= eln-cache-dir "")))
      (cond ((fboundp 'startup-redirect-eln-cache) ; Emacs >= 28
             (startup-redirect-eln-cache eln-cache-dir))
            ((boundp 'native-comp-eln-load-path)
             (setcar native-comp-eln-load-path
                     (expand-file-name
                      (convert-standard-filename eln-cache-dir)
                      user-emacs-directory)))
            (t (error "Cannot change the eln-cache-dir directory"))))
  (when byte-comp-enabled
    (setq no-byte-compile nil)
    (setq byte-compile-verbose t)
    (setq byte-compile-warnings t))

  (when (and native-comp-enabled native-comp-available)
    (when (and jobs (not (string= jobs "")))
      (setq native-comp-async-jobs-number (string-to-number jobs)))
    (setq native-comp-async-report-warnings-errors t)
    (setq native-comp-warning-on-missing-source t)
    (setq native-comp-verbose 0)
    (setq native-comp-deferred-compilation t)
    (setq native-comp-jit-compilation t)
    (setq package-native-compile nil)
    (add-hook 'native-comp-async-all-done-hook
              #'elispcomp-native-compilation-finish-hook))

  ;; SHOW MESSAGES
  (message "[INFO] Recursively compile the directory: %s"
           default-directory)
  (message "[INFO] Byte comp enabled: %s" byte-comp-enabled)
  (message "[INFO] Native comp enabled: %s" native-comp-enabled)
  (message "[INFO] Ensure native comp available: %s"
           ensure-native-comp-available)
  (message "[INFO] Jobs: %s" jobs)
  (message "[INFO] Emacs user directory: %s" user-emacs-directory)
  (message "[INFO] eln-cache directory: %s\n" eln-cache-dir)
  (message "[INFO] Load path directories:\n%s\n" load-path-list)

  ;; LOAD DIRECTORIES
  (elispcomp-add-load-paths load-path-list)

  ;; BYTE-COMP
  (when byte-comp-enabled
    (message "[TASK] Byte compile")
    (byte-recompile-directory default-directory 0))

  ;; NATIVE-COMP
  (when (and native-comp-enabled native-comp-available)
    (message "[TASK] Native compile")
    (native-compile-async default-directory 'recursively)
    (while (not elispcomp-native-comp-finished)
      (sleep-for 0.25))))

(provide 'elispcomp)
;;; elispcomp.el ends here
